using System;
using System.Diagnostics;
using System.Runtime.InteropServices;
using System.Threading;
using Task = System.Threading.Tasks.Task;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;

namespace FSLint.VisualStudio
{
    /// <summary>
    /// FSLint Visual Studio Extension Package
    /// This package provides LSP-based F# linting capabilities
    /// </summary>
    [PackageRegistration(UseManagedResourcesOnly = true, AllowsBackgroundLoading = true)]
    [Guid(PackageGuidString)]
    [ProvideAutoLoad(UIContextGuids.SolutionHasMultipleProjects, PackageAutoLoadFlags.BackgroundLoad)]
    [ProvideAutoLoad(UIContextGuids.SolutionHasSingleProject, PackageAutoLoadFlags.BackgroundLoad)]
    public sealed class Package : AsyncPackage
    {
        /// <summary>
        /// FSLintVisualStudioPackage GUID string.
        /// </summary>
        public const string PackageGuidString = "9cbe68c1-132a-49cb-80fd-399060bd283c";

        /// <summary>
        /// Initialization of the package; this method is called right after the package is sited.
        /// </summary>
        /// <param name="cancellationToken">A cancellation token to monitor for initialization cancellation</param>
        /// <param name="progress">A provider for progress updates</param>
        /// <returns>A task representing the async work of package initialization</returns>
        protected override async Task InitializeAsync(CancellationToken cancellationToken, IProgress<ServiceProgressData> progress)
        {
            await this.JoinableTaskFactory.SwitchToMainThreadAsync(cancellationToken);
        }
    }
}