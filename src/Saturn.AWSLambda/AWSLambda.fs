namespace Saturn

open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.ResponseCompression
open Giraffe
open Microsoft.AspNetCore
open System
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open System.Threading.Tasks

open Amazon.Lambda.Core
open Amazon.Lambda.APIGatewayEvents
open Amazon.Lambda.RuntimeSupport
open Amazon.Lambda.Serialization.Json

[<AutoOpen>]
module AWSLambda =

  type LambdaApplicationBuilder internal () =
    member __.Yield(_) =
      let errorHandler (ex : Exception) (logger : ILogger) =
        logger.LogError(EventId(), ex, "An unhandled exception has occurred while executing the request.")
        clearResponse >=> Giraffe.HttpStatusCodeHandlers.ServerErrors.INTERNAL_ERROR ex.Message
      {Router = None; ErrorHandler = Some errorHandler; Pipelines = []; Urls = []; MimeTypes = []; AppConfigs = []; HostConfigs = []; ServicesConfig = []; CliArguments = None; CookiesAlreadyAdded = false; NoRouter = false; Channels = [] }

    member __.Run(state: ApplicationState) : IWebHostBuilder =
      /// to build the app we have to separate our configurations and our pipelines.
      /// we can only call `Configure` once, so we have to apply our pipelines in the end
      let router =
        match state.Router with
        | None ->
          if not state.NoRouter then printfn "Router needs to be defined in Saturn application. If you're building channels-only application, or gRPC application you may disable this message with `no_router` flag in your `application` block"
          None
        | Some router ->
          Some ((succeed |> List.foldBack (fun e acc -> acc >=> e) state.Pipelines) >=> router)

      // as we want to add middleware to our pipeline, we can add it here and we'll fold across it in the end
      let useParts = ResizeArray<IApplicationBuilder -> IApplicationBuilder>()

      let wbhst =
        // Explicit null removes unnecessary handlers.
        WebHost.CreateDefaultBuilder(Option.toObj state.CliArguments)
        |> List.foldBack (fun e acc -> e acc ) state.HostConfigs

      wbhst.ConfigureServices(fun svcs ->
        let services = svcs.AddGiraffe()
        state.ServicesConfig |> List.rev |> List.iter (fun fn -> fn services |> ignore) |> ignore)
      |> ignore // need giraffe (with user customizations) in place so that I can get an IJsonSerializer for the channels

      /// error handler first so that errors are caught
      match state.ErrorHandler with
      | Some err -> useParts.Add (fun app -> app.UseGiraffeErrorHandler(err))
      | None -> ()

      /// user-provided middleware
      state.AppConfigs |> List.iter (useParts.Add)

      /// finally Giraffe itself
      match router with
      | None -> ()
      | Some router -> useParts.Add (fun app -> app.UseGiraffe router; app)

      let wbhst =
        if not (state.Urls |> List.isEmpty) then
          wbhst.UseUrls(state.Urls |> List.toArray)
        else wbhst

      wbhst.Configure(fun ab ->
        (ab, useParts)
        ||> Seq.fold (fun ab part -> part ab)
        |> ignore
      )

    ///Defines top-level router used for the application
    ///This construct is obsolete, use `use_router` instead
    [<CustomOperation("router")>]
    [<ObsoleteAttribute("This construct is obsolete, use use_router instead")>]
    member __.RouterOld(state, handler) =
      {state with Router = Some handler}

    ///Defines top-level router used for the application
    [<CustomOperation("use_router")>]
    member __.Router(state, handler) =
      {state with Router = Some handler}

    ///Disable warning message about lack of `router` definition. Should be used for channels-only or gRPC applications.
    [<CustomOperation("no_router")>]
    member __.NoRouter(state) =
      {state with NoRouter = true}

    ///Adds pipeline to the list of pipelines that will be used for every request
    [<CustomOperation("pipe_through")>]
    member __.PipeThrough(state : ApplicationState, pipe) =
      {state with Pipelines = pipe::state.Pipelines}

    ///Adds error/not-found handler for current scope
    [<CustomOperation("error_handler")>]
    member __.ErrorHandler(state : ApplicationState, handler) =
      {state with ErrorHandler = Some handler}

    ///Adds custom application configuration step.
    [<CustomOperation("app_config")>]
    member __.AppConfig(state, config) =
      {state with AppConfigs = config::state.AppConfigs}

    ///Adds custom host configuration step.
    [<CustomOperation("host_config")>]
    member __.HostConfig(state, config) =
      {state with HostConfigs = config::state.HostConfigs}

    ///Adds custom service configuration step.
    [<CustomOperation("service_config")>]
    member __.ServiceConfig(state, config) =
      {state with ServicesConfig = config::state.ServicesConfig}

    ///Adds MIME types definitions as a list of (extension, mime)
    [<CustomOperation("use_mime_types")>]
    member __.AddMimeTypes(state, mimeList) =
      {state with MimeTypes = mimeList}


    ///Adds logging configuration.
    [<CustomOperation("logging")>]
    member __.Logging(state, (config : ILoggingBuilder -> unit)) =
      {state with HostConfigs = (fun (app : IWebHostBuilder)-> app.ConfigureLogging(config))::state.HostConfigs}

    ///Enables in-memory session cache
    [<CustomOperation("memory_cache")>]
    member __.MemoryCache(state) =
      let service (s : IServiceCollection) = s.AddDistributedMemoryCache()
      let serviceSet (s : IServiceCollection) = s.AddSession()

      { state with
          ServicesConfig = serviceSet::(service::state.ServicesConfig)
          AppConfigs = (fun (app : IApplicationBuilder)-> app.UseSession())::state.AppConfigs
      }

    ///Enables gzip compression
    [<CustomOperation("use_gzip")>]
    member __.UseGZip(state : ApplicationState) =
      let service (s : IServiceCollection) =
        s.Configure<GzipCompressionProviderOptions>(fun (opts : GzipCompressionProviderOptions) -> opts.Level <- System.IO.Compression.CompressionLevel.Optimal)
         .AddResponseCompression()
      let middleware (app : IApplicationBuilder) = app.UseResponseCompression()

      { state with
          ServicesConfig = service::state.ServicesConfig
          AppConfigs = middleware::state.AppConfigs
      }

  ///Computation expression used to configure AWS Lambda application
  let lambdaApplication = LambdaApplicationBuilder()

  type LambdaEntryPoint() =
    inherit Amazon.Lambda.AspNetCoreServer.APIGatewayProxyFunction()

    override _.Init(_ : IWebHostBuilder) = ()

  ///Runs AWS Lambda function/app
  let runLambdaApp (app: IWebHostBuilder) =
    //if SiteMap.isDebug then SiteMap.generate ()

    let lambdaEntry = new LambdaEntryPoint()
    let functionHandler = Func<APIGatewayProxyRequest, ILambdaContext, Task<APIGatewayProxyResponse>>(fun request context -> lambdaEntry.FunctionHandlerAsync(request, context))
    use handlerWrapper = HandlerWrapper.GetHandlerWrapper<APIGatewayProxyRequest, Task<APIGatewayProxyResponse>>(functionHandler, new JsonSerializer())
    use bootstrap = new LambdaBootstrap(handlerWrapper)

    bootstrap.RunAsync() |> Async.AwaitTask |> ignore
