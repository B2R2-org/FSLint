using Microsoft.VisualStudio.LanguageServer.Client;
using Microsoft.VisualStudio.LanguageServer.Protocol;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Threading;
using Microsoft.VisualStudio.Utilities;
using Newtonsoft.Json.Linq;
using StreamJsonRpc;
using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.IO;
using System.Threading;
using System.Threading.Tasks;
using Task = System.Threading.Tasks.Task;

namespace FSLint.VisualStudio
{
    [ContentType("F#")]
    [Export(typeof(ILanguageClient))]
    public class LanguageClient : ILanguageClient
    {
        private Process serverProcess;
        private static bool hasScannedWorkspace = false;
        private static readonly object scanLock = new object();
        private static string lastWorkspaceRoot = null;
        private static LanguageClient instance = null;

        // Package.InitializeAsync에서 설정, RestartServerAsync에서 갱신
        public static bool StrictMode { get; set; } = false;

        public LanguageClient()
        {
            instance = this;
        }

        [Import]
        internal SVsServiceProvider ServiceProvider { get; set; }

        public string Name => "FSLint Language Server";

        public IEnumerable<string> ConfigurationSections => null;

        public object InitializationOptions => new { strict = StrictMode };

        public IEnumerable<string> FilesToWatch
        {
            get { yield return "**/*.fs"; }
        }

        public bool ShowNotificationOnInitializeFailed => true;

        public event AsyncEventHandler<EventArgs> StartAsync;
        public event AsyncEventHandler<EventArgs> StopAsync;

        public static async Task RestartServerAsync(bool isStrict)
        {
            StrictMode = isStrict;  // 먼저 저장
            OutputWindowHelper.WriteLine($"[FSLint] Restarting server (strict: {isStrict})");

            if (instance == null)
            {
                OutputWindowHelper.WriteLine("[FSLint] No instance to restart");
                return;
            }

            if (instance.StopAsync != null)
                await instance.StopAsync.InvokeAsync(instance, EventArgs.Empty);

            await Task.Delay(500);

            if (instance.StartAsync != null)
                await instance.StartAsync.InvokeAsync(instance, EventArgs.Empty);
        }

        private string GetWorkspaceRootPath()
        {
            try
            {
                ThreadHelper.ThrowIfNotOnUIThread();

                var dte = ServiceProvider.GetService(typeof(EnvDTE.DTE)) as EnvDTE.DTE;

                if (dte == null)
                    return Environment.GetFolderPath(Environment.SpecialFolder.UserProfile);

                if (dte.Solution != null && !string.IsNullOrEmpty(dte.Solution.FullName))
                    return Path.GetDirectoryName(dte.Solution.FullName);

                if (dte.Solution?.Projects != null)
                {
                    foreach (EnvDTE.Project project in dte.Solution.Projects)
                    {
                        if (project != null && !string.IsNullOrEmpty(project.FullName))
                            return Path.GetDirectoryName(project.FullName);
                    }
                }
            }
            catch (Exception ex)
            {
                OutputWindowHelper.WriteLine($"[FSLint] Failed to get workspace path: {ex.Message}");
            }

            return Environment.GetFolderPath(Environment.SpecialFolder.UserProfile);
        }

        public async Task<Connection> ActivateAsync(CancellationToken token)
        {
            await Task.Yield();
            try
            {
                string serverPath = Configuration.GetServerPath();

                if (string.IsNullOrEmpty(serverPath))
                {
                    string error = "FSLint Language Server executable not found. " +
                                   "Please set FSLINT_SERVER_PATH environment variable or install the server.";
                    throw new FileNotFoundException(error);
                }

                if (!File.Exists(serverPath))
                {
                    string error = $"FSLint Language Server not found at: {serverPath}";
                    throw new FileNotFoundException(error);
                }

                string workspaceRoot = null;
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync(token);
                workspaceRoot = GetWorkspaceRootPath();

                if (lastWorkspaceRoot != workspaceRoot)
                {
                    lock (scanLock)
                    {
                        hasScannedWorkspace = false;
                    }
                    lastWorkspaceRoot = workspaceRoot;
                    DiagnosticStore.Clear();
                }

                string strictArg = StrictMode ? " --strict" : "";
                OutputWindowHelper.WriteLine(
                    $"[FSLint] Starting server for workspace: {workspaceRoot} (strict: {StrictMode})");

                ProcessStartInfo info = new ProcessStartInfo
                {
                    FileName = serverPath,
                    Arguments = $"\"{workspaceRoot}\"{strictArg}",
                    RedirectStandardInput = true,
                    RedirectStandardOutput = true,
                    RedirectStandardError = true,
                    UseShellExecute = false,
                    CreateNoWindow = true,
                    WorkingDirectory = Path.GetDirectoryName(serverPath)
                };

                serverProcess = new Process { StartInfo = info };

                serverProcess.ErrorDataReceived += (sender, e) =>
                {
                    if (!string.IsNullOrEmpty(e.Data))
                        OutputWindowHelper.WriteLine(e.Data);
                };

                if (!serverProcess.Start())
                {
                    string error = "Failed to start FSLint Language Server process";
                    OutputWindowHelper.WriteLine(error);
                    throw new InvalidOperationException(error);
                }

                serverProcess.BeginErrorReadLine();

                _ = Task.Run(() =>
                {
                    try
                    {
                        serverProcess.WaitForExit();
                        OutputWindowHelper.WriteLine(
                            $"[FSLint] Server process exited with code: {serverProcess.ExitCode}");

                        if (serverProcess.ExitCode != 0 && StopAsync != null)
                            _ = StopAsync.InvokeAsync(this, EventArgs.Empty);
                    }
                    catch (Exception ex)
                    {
                        OutputWindowHelper.WriteLine(
                            "[FSLint] Error monitoring process exit: " + ex.ToString());
                    }
                });

                var outputStream = new LoggingStream(
                    serverProcess.StandardOutput.BaseStream,
                    "LSP Server -> Client");
                var inputStream = new LoggingStream(
                    serverProcess.StandardInput.BaseStream,
                    "LSP Client -> Server");

                return new Connection(outputStream, inputStream);
            }
            catch (Exception ex)
            {
                OutputWindowHelper.WriteLine("[FSLint] ActivateAsync FAILED: " + ex.ToString());

                if (StopAsync != null)
                    await StopAsync.InvokeAsync(this, EventArgs.Empty);

                throw;
            }
        }

        public async Task OnLoadedAsync()
        {
            try
            {
                OutputWindowHelper.WriteLine("[FSLint] Language client loaded");

                if (StartAsync != null)
                    await StartAsync.InvokeAsync(this, EventArgs.Empty);
                else
                    OutputWindowHelper.WriteLine("[FSLint] WARNING: StartAsync event is null");
            }
            catch (Exception ex)
            {
                OutputWindowHelper.WriteLine("[FSLint] OnLoadedAsync FAILED: " + ex.ToString());
                throw;
            }
        }

        public Task OnServerInitializedAsync()
        {
            lock (scanLock)
            {
                if (!hasScannedWorkspace)
                {
                    hasScannedWorkspace = true;
                    OutputWindowHelper.WriteLine(
                        $"[FSLint] Language server initialized (strict: {StrictMode})");
                }
            }
            return Task.CompletedTask;
        }

        public Task<InitializationFailureContext> OnServerInitializeFailedAsync(
            ILanguageClientInitializationInfo initializationState)
        {
            string statusMessage = initializationState?.StatusMessage ?? "Unknown error";

            if (initializationState?.InitializationException != null)
                OutputWindowHelper.WriteLine("[FSLint] Initialization Exception: " +
                    initializationState.InitializationException.ToString());

            var failureContext = new InitializationFailureContext
            {
                FailureMessage = $"FSLint Language Server failed to initialize.\n\n" +
                                 $"Status: {statusMessage}\n\n" +
                                 $"Please ensure:\n" +
                                 $"1. The FSLint Language Server is properly installed\n" +
                                 $"2. You have an F# solution or project open\n" +
                                 $"3. Check the Output window (View -> Output -> FSLint) for details"
            };

            if (StopAsync != null)
                _ = StopAsync.InvokeAsync(this, EventArgs.Empty);

            return Task.FromResult(failureContext);
        }
    }
}