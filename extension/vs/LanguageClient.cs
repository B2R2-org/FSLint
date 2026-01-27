using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.IO;
using System.Threading;
using System.Threading.Tasks;
using Task = System.Threading.Tasks.Task;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Threading;
using Microsoft.VisualStudio.Utilities;
using Microsoft.VisualStudio.LanguageServer.Client;

namespace FSLint.VisualStudio
{
    /// <summary>
    /// FSLint Language Server Client for Visual Studio
    /// Implements ILanguageClient to provide LSP-based F# linting
    /// </summary>
    [ContentType("F#")]
    [Export(typeof(ILanguageClient))]
    public class LanguageClient : ILanguageClient
    {

        private Process serverProcess;

        [Import]
        internal SVsServiceProvider ServiceProvider { get; set; }

        /// <summary>
        /// Gets the name of the language client (displayed to the user)
        /// </summary>
        public string Name
        {
            get
            {
                return "FSLint Language Server";
            }
        }

        /// <summary>
        /// Gets the configuration section names.
        /// Null means no custom configuration.
        /// </summary>
        public IEnumerable<string> ConfigurationSections
        {
            get
            {
                return null;
            }
        }

        /// <summary>
        /// Gets the initialization options to send when 'initialize' message
        /// is sent.
        /// Can be used to pass custom settings to the server.
        /// </summary>
        public object InitializationOptions
        {
            get
            {
                return null;
            }
        }

        /// <summary>
        /// Gets the list of file names to watch for changes.
        /// Null means the server will handle file watching.
        /// </summary>
        public IEnumerable<string> FilesToWatch
        {
            get
            {
                yield return "**/*.fs";
            }
        }

        /// <summary>
        /// Gets whether to show a notification when initialization fails
        /// </summary>
        public bool ShowNotificationOnInitializeFailed
        {
            get
            {
                return true;
            }
        }

        /// <summary>
        /// Raised when the language server is ready to receive messages
        /// </summary>
        public event AsyncEventHandler<EventArgs> StartAsync;

        /// <summary>
        /// Raised when the language server should be stopped
        /// </summary>
        public event AsyncEventHandler<EventArgs> StopAsync;

        /// <summary>
        /// Gets the workspace root path from the current solution
        /// </summary>
        private string GetWorkspaceRootPath()
        {
            try
            {
                ThreadHelper.ThrowIfNotOnUIThread();

                var dte = ServiceProvider.GetService(typeof(EnvDTE.DTE)) as EnvDTE.DTE;
                if (dte?.Solution != null && !string.IsNullOrEmpty(dte.Solution.FullName))
                {
                    return Path.GetDirectoryName(dte.Solution.FullName);
                }

                if (dte?.Solution?.Projects != null)
                {
                    foreach (EnvDTE.Project project in dte.Solution.Projects)
                    {
                        if (!string.IsNullOrEmpty(project.FullName))
                        {
                            return Path.GetDirectoryName(project.FullName);
                        }
                    }
                }
            }
            catch (Exception ex)
            {
                OutputWindowHelper.WriteLine($"[FSLint] Failed to get workspace path: {ex.Message}");
            }

            return Environment.GetFolderPath(Environment.SpecialFolder.UserProfile);
        }

        /// <summary>
        /// Called to activate the language server
        /// This is where we start the server process and establish connection
        /// </summary>
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

                OutputWindowHelper.WriteLine($"[FSLint] Starting server for workspace: {workspaceRoot}");

                ProcessStartInfo info = new ProcessStartInfo
                {
                    FileName = serverPath,
                    Arguments = $"\"{workspaceRoot}\"",
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
                    {
                        OutputWindowHelper.WriteLine(e.Data);
                    }
                };

                if (!serverProcess.Start())
                {
                    string error = "Failed to start FSLint Language Server process";
                    OutputWindowHelper.WriteLine(error);
                    throw new InvalidOperationException(error);
                }

                // Begin reading stderr asynchronously
                serverProcess.BeginErrorReadLine();

                // Monitor process exit
                _ = Task.Run(() =>
                {
                    try
                    {
                        serverProcess.WaitForExit();
                        OutputWindowHelper.WriteLine($"[FSLint] Server process exited with code: {serverProcess.ExitCode}");

                        if (serverProcess.ExitCode != 0 && StopAsync != null)
                        {
                            _ = StopAsync.InvokeAsync(this, EventArgs.Empty);
                        }
                    }
                    catch (Exception ex)
                    {
                        OutputWindowHelper.WriteLine("[FSLint] Error monitoring process exit: " + ex.ToString());
                    }
                });

                var outputStream = new LoggingStream(
                    serverProcess.StandardOutput.BaseStream,
                    "LSP Server -> Client");
                var inputStream = new LoggingStream(
                    serverProcess.StandardInput.BaseStream,
                    "LSP Client -> Server");

                Connection connection = new Connection(
                    outputStream,
                    inputStream
                );

                return connection;
            }
            catch (Exception ex)
            {
                OutputWindowHelper.WriteLine("[FSLint] ActivateAsync FAILED: " + ex.ToString());

                if (StopAsync != null)
                {
                    await StopAsync.InvokeAsync(this, EventArgs.Empty);
                }

                throw;
            }
        }

        /// <summary>
        /// Called when the extension is loaded
        /// </summary>
        public async Task OnLoadedAsync()
        {
            try
            {
                OutputWindowHelper.WriteLine("[FSLint] Language client loaded");

                if (StartAsync != null)
                {
                    await StartAsync.InvokeAsync(this, EventArgs.Empty);
                }
                else
                {
                    OutputWindowHelper.WriteLine("[FSLint] WARNING: StartAsync event is null");
                }
            }
            catch (Exception ex)
            {
                OutputWindowHelper.WriteLine("[FSLint] OnLoadedAsync FAILED: " + ex.ToString());
                throw;
            }
        }

        /// <summary>
        /// Called when the server has been successfully initialized
        /// </summary>
        public Task OnServerInitializedAsync()
        {
            OutputWindowHelper.WriteLine("[FSLint] Language server initialized successfully");
            return Task.CompletedTask;
        }

        /// <summary>
        /// Called when server initialization has failed
        /// </summary>
        public Task<InitializationFailureContext> OnServerInitializeFailedAsync(
            ILanguageClientInitializationInfo initializationState)
        {
            string statusMessage = initializationState?.StatusMessage ?? "Unknown error";

            if (initializationState?.InitializationException != null)
            {
                OutputWindowHelper.WriteLine("[FSLint] Initialization Exception: " +
                                  initializationState.InitializationException.ToString());
            }

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
            {
                _ = StopAsync.InvokeAsync(this, EventArgs.Empty);
            }

            return Task.FromResult(failureContext);
        }
    }
}