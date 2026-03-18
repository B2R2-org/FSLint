using Microsoft.VisualStudio.Shell;
using System.ComponentModel;
using System.Runtime.InteropServices;
using System.Threading.Tasks;
using System;

namespace FSLint.VisualStudio
{
    [ComVisible(true)]
    public class FSLintOptions : DialogPage
    {
        [Category("Analysis")]
        [DisplayName("Strict Mode")]
        [Description("Enable strict linting rules.")]
        [DefaultValue(false)]
        public bool IsStrict { get; set; } = false;

        protected override void OnApply(PageApplyEventArgs e)
        {
            base.OnApply(e);
            if (e.ApplyBehavior != ApplyKind.Apply) return;

            bool value = IsStrict;
            OutputWindowHelper.WriteLine($"[FSLint] Options applied: strict = {value}");

            _ = System.Threading.Tasks.Task.Run(async () =>
            {
                try
                {
                    await LanguageClient.RestartServerAsync(value);
                }
                catch (Exception ex)
                {
                    OutputWindowHelper.WriteLine($"[FSLint] Restart failed: {ex.Message}");
                }
            });
        }
    }
}