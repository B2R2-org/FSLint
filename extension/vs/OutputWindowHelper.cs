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

                var outputWindow = serviceProvider.GetService(typeof(SVsOutputWindow)) as IVsOutputWindow;
                if (outputWindow == null) return;

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

        public static void WriteLine(string message)
        {
            if (_joinableTaskFactory == null || _outputPane == null)
            {
                Console.WriteLine($"[FSLint] {message}");
                return;
            }

            if (!ThreadHelper.CheckAccess())
            {
                _joinableTaskFactory.Run(async () =>
                {
                    await _joinableTaskFactory.SwitchToMainThreadAsync();
                    WriteLineInternal(message);
                });
            }
            else
            {
                WriteLineInternal(message);
            }
        }

        private static void WriteLineInternal(string message)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            try
            {
                if (_outputPane != null)
                {
                    string timestamp = DateTime.Now.ToString("HH:mm:ss.fff");
                    _outputPane.OutputStringThreadSafe($"[{timestamp}] {message}\r\n");
                }
            }
            catch
            {
            }
            Console.WriteLine($"[FSLint] {message}");
        }
    }
}