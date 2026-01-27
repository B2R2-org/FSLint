using System.ComponentModel.Composition;
using System.Windows.Media;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Utilities;

namespace FSLint.VisualStudio
{
    internal static class FSLintClassificationTypes
    {
        public const string Error = "FSLintError";

        [Export, Name(FSLintClassificationTypes.Error)]
        internal static ClassificationTypeDefinition FSLintErrorType { get; set; }
    }

    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = FSLintClassificationTypes.Error)]
    [Name(FSLintClassificationTypes.Error)]
    [Order(After = Priority.High)]
    [UserVisible(true)]
    internal sealed class FSLintErrorFormatDefinition : ClassificationFormatDefinition
    {
        public FSLintErrorFormatDefinition()
        {
            BackgroundColor = Color.FromRgb(255, 145, 145);
            DisplayName = "FSLint Error";
        }
    }
}