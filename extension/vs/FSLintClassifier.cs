using System;
using System.Collections.Generic;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Classification;

namespace FSLint.VisualStudio
{
    internal class FSLintClassifier : IClassifier
    {
        private readonly IClassificationType errorType;
        private readonly ITextBuffer buffer;
        private string documentUri;
        private static readonly IList<ClassificationSpan> empty = Array.Empty<ClassificationSpan>();

        public FSLintClassifier(IClassificationTypeRegistryService registry, ITextBuffer buffer)
        {
            this.errorType = registry.GetClassificationType(FSLintClassificationTypes.Error);
            this.buffer = buffer;

            if (buffer.Properties.TryGetProperty(typeof(ITextDocument), out ITextDocument document))
            {
                documentUri = new Uri(document.FilePath).AbsoluteUri;
            }

            DiagnosticStore.DiagnosticsChanged += OnDiagnosticsChanged;
        }

        private void OnDiagnosticsChanged(object sender, string uri)
        {
            if (uri == documentUri)
            {
                ClassificationChanged?.Invoke(this,
                    new ClassificationChangedEventArgs(
                        new SnapshotSpan(buffer.CurrentSnapshot, 0, buffer.CurrentSnapshot.Length)));
            }
        }

        public IList<ClassificationSpan> GetClassificationSpans(SnapshotSpan span)
        {
            if (span.IsEmpty || string.IsNullOrEmpty(documentUri))
            {
                return empty;
            }

            var diagnostics = DiagnosticStore.GetDiagnostics(documentUri);
            if (diagnostics.Count == 0)
            {
                return empty;
            }

            var result = new List<ClassificationSpan>();

            foreach (var diag in diagnostics)
            {
                try
                {
                    var snapshot = span.Snapshot;

                    if (diag.StartLine >= snapshot.LineCount || diag.EndLine >= snapshot.LineCount)
                        continue;

                    var startLine = snapshot.GetLineFromLineNumber(diag.StartLine);
                    var endLine = snapshot.GetLineFromLineNumber(diag.EndLine);

                    int startPos = startLine.Start.Position + diag.StartCharacter;
                    int endPos = endLine.Start.Position + diag.EndCharacter;

                    if (startPos < 0 || endPos > snapshot.Length || startPos >= endPos)
                        continue;

                    var diagSpan = new SnapshotSpan(snapshot, startPos, endPos - startPos);

                    if (diagSpan.IntersectsWith(span))
                    {
                        result.Add(new ClassificationSpan(diagSpan, errorType));
                    }
                }
                catch (Exception ex)
                {
                    OutputWindowHelper.WriteLine($"[Classifier] Error creating span: {ex.Message}");
                }
            }

            return result;
        }

        public event EventHandler<ClassificationChangedEventArgs> ClassificationChanged;
    }
}