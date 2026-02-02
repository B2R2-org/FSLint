using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Utilities;

namespace FSLint.VisualStudio
{
    [Export(typeof(IClassifierProvider))]
    [ContentType("F#")]
    internal class FSLintClassifierProvider : IClassifierProvider
    {
        [Import]
        internal IClassificationTypeRegistryService RegistryService { get; set; }

        public IClassifier GetClassifier(ITextBuffer textBuffer)
        {
            return textBuffer.Properties.GetOrCreateSingletonProperty(
                () => new FSLintClassifier(RegistryService, textBuffer));
        }
    }
}