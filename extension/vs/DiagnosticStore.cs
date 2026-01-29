using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Linq;
using Newtonsoft.Json.Linq;

namespace FSLint.VisualStudio
{
    /// <summary>
    /// Stores diagnostics received from LSP server for use by the classifier
    /// </summary>
    internal static class DiagnosticStore
    {
        private static readonly object lockObject = new object();
        private static readonly Dictionary<string, List<DiagnosticInfo>> diagnosticsByUri
            = new Dictionary<string, List<DiagnosticInfo>>(StringComparer.OrdinalIgnoreCase);

        public static event EventHandler<string> DiagnosticsChanged;

        public static void UpdateDiagnostics(string uri, JArray diagnostics)
        {
            lock (lockObject)
            {
                var diagList = new List<DiagnosticInfo>();

                if (diagnostics != null)
                {
                    foreach (var diag in diagnostics)
                    {
                        try
                        {
                            var range = diag["range"];
                            var start = range["start"];
                            var end = range["end"];

                            diagList.Add(new DiagnosticInfo
                            {
                                StartLine = start["line"].Value<int>(),
                                StartCharacter = start["character"].Value<int>(),
                                EndLine = end["line"].Value<int>(),
                                EndCharacter = end["character"].Value<int>(),
                                Message = diag["message"].Value<string>(),
                                Severity = diag["severity"].Value<int>()
                            });
                        }
                        catch (Exception ex)
                        {
                            OutputWindowHelper.WriteLine($"[DiagnosticStore] Parse error: {ex.Message}");
                        }
                    }
                }

                diagnosticsByUri[uri] = diagList;
            }

            DiagnosticsChanged?.Invoke(null, uri);
        }

        public static List<DiagnosticInfo> GetDiagnostics(string uri)
        {
            lock (lockObject)
            {
                if (diagnosticsByUri.TryGetValue(uri, out var diags))
                {
                    return diags.ToList();
                }
                return new List<DiagnosticInfo>();
            }
        }

        public static void Clear()
        {
            lock (lockObject)
            {
                diagnosticsByUri.Clear();
            }

            DiagnosticsChanged?.Invoke(null, string.Empty);
        }
    }

    internal class DiagnosticInfo
    {
        public int StartLine { get; set; }
        public int StartCharacter { get; set; }
        public int EndLine { get; set; }
        public int EndCharacter { get; set; }
        public string Message { get; set; }
        public int Severity { get; set; }
    }
}