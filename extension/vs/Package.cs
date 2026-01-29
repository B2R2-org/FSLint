using System;
using System.Runtime.InteropServices;
using System.Threading;
using Task = System.Threading.Tasks.Task;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;

namespace FSLint.VisualStudio
{
    [PackageRegistration(UseManagedResourcesOnly = true, AllowsBackgroundLoading = true)]
    [Guid(PackageGuidString)]
    [ProvideAutoLoad(UIContextGuids.SolutionExists, PackageAutoLoadFlags.BackgroundLoad)]
    [ProvideAutoLoad(UIContextGuids.NoSolution, PackageAutoLoadFlags.BackgroundLoad)]
    public sealed class Package : AsyncPackage
    {
        public const string PackageGuidString = "9cbe68c1-132a-49cb-80fd-399060bd283c";
        private static string currentWorkspace = null;

        protected override async Task InitializeAsync(CancellationToken cancellationToken, IProgress<ServiceProgressData> progress)
        {
            await this.JoinableTaskFactory.SwitchToMainThreadAsync(cancellationToken);

            OutputWindowHelper.Initialize(this);

            var dte = await GetServiceAsync(typeof(EnvDTE.DTE)) as EnvDTE.DTE;
            string newWorkspace = GetWorkspacePath(dte);

            if (currentWorkspace != newWorkspace)
            {
                OutputWindowHelper.WriteLine("==========================================");
                OutputWindowHelper.WriteLine("FSLint Extension Loaded");
                OutputWindowHelper.WriteLine($"Time: {DateTime.Now:yyyy-MM-dd HH:mm:ss}");

                if (!string.IsNullOrEmpty(currentWorkspace))
                {
                    OutputWindowHelper.WriteLine($"Workspace changed from: {currentWorkspace}");
                    OutputWindowHelper.WriteLine($"                    to: {newWorkspace}");
                    OutputWindowHelper.WriteLine("Clearing previous diagnostics...");
                    DiagnosticStore.Clear();
                    OutputWindowHelper.WriteLine("Language Server will restart for new workspace");
                }

                currentWorkspace = newWorkspace;

                if (!string.IsNullOrEmpty(newWorkspace))
                {
                    OutputWindowHelper.WriteLine($"Workspace: {newWorkspace}");
                }
                else
                {
                    OutputWindowHelper.WriteLine("No workspace currently loaded");
                }

                OutputWindowHelper.WriteLine("Waiting for F# files to open...");
                OutputWindowHelper.WriteLine("==========================================");
            }
        }

        private string GetWorkspacePath(EnvDTE.DTE dte)
        {
            ThreadHelper.ThrowIfNotOnUIThread();

            if (dte?.Solution != null)
            {
                if (!string.IsNullOrEmpty(dte.Solution.FullName))
                {
                    return System.IO.Path.GetDirectoryName(dte.Solution.FullName);
                }

                if (dte.Solution.Projects != null)
                {
                    foreach (EnvDTE.Project project in dte.Solution.Projects)
                    {
                        if (!string.IsNullOrEmpty(project.FullName))
                        {
                            return System.IO.Path.GetDirectoryName(project.FullName);
                        }
                    }
                }
            }

            return null;
        }
    }
}