using System;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Threading;

namespace FSLint.VisualStudio
{
    internal static class OutputWindowHelper
    {
        private static IVsOutputWindowPane _outputPane;
        private static readonly Guid PaneGuid = new Guid("A8B5E7F2-9C3D-4E1F-8A6B-2D4C5F7E9A1B");
        private static JoinableTaskFactory _joinableTaskFactory;

        public static void Initialize(IServiceProvider serviceProvider)
        {
            ThreadHelper.ThrowIfNotOnUIThread();

            try
            {
                _joinableTaskFactory = ThreadHelper.JoinableTaskFactory;

                if (!(serviceProvider.GetService(typeof(SVsOutputWindow)) is IVsOutputWindow outputWindow)) return;

                Guid guid = PaneGuid;
                outputWindow.CreatePane(ref guid, "FSLint", 1, 1);
                outputWindow.GetPane(ref guid, out _outputPane);

                if (_outputPane != null)
                {
                    _outputPane.Activate();
                    WriteLineInternal("FSLint Output Window initialized");
                }
            }
            catch (Exception ex)
            {
                System.Diagnostics.Debug.WriteLine($"Failed to create output pane: {ex}");
            }
        }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Usage", "VSTHRD010:Invoke single-threaded types on Main thread", Justification = "Method internally switches to main thread")]
        public static void WriteLine(string message)
        {
            if (_joinableTaskFactory == null || _outputPane == null)
            {
                System.Diagnostics.Debug.WriteLine($"[FSLint] {message}");
                return;
            }

            _ = System.Threading.Tasks.Task.Run(async () =>
            {
                try
                {
                    await _joinableTaskFactory.SwitchToMainThreadAsync();
                    WriteLineInternal(message);
                }
                catch (Exception ex)
                {
                    System.Diagnostics.Debug.WriteLine($"[OutputWindowHelper] Error: {ex.Message}");
                }
            });
        }

        private static void WriteLineInternal(string message)
        {
            try
            {
#pragma warning disable VSTHRD010
                if (_outputPane != null)
                {
                    string timestamp = DateTime.Now.ToString("HH:mm:ss.fff");
                    _outputPane.OutputStringThreadSafe($"[{timestamp}] {message}\r\n");
                }
#pragma warning restore VSTHRD010
            }
            catch (Exception ex)
            {
                System.Diagnostics.Debug.WriteLine($"[OutputWindowHelper] WriteLineInternal error: {ex.Message}");
            }
        }
    }
}