using System;
using System.IO;

namespace FSLint.VisualStudio
{
  /// <summary>
  /// Configuration helper for FSLint Visual Studio extension
  /// </summary>
  internal static class Configuration
  {
    private const string ServerExecutableName = "B2R2.FSLint.LanguageServer.exe";
    /// <summary>
    /// Gets the path to the FSLint Language Server executable
    /// </summary>
    public static string GetServerPath()
    {
      string extensionDir = Path.GetDirectoryName(typeof(LanguageClient).Assembly.Location);
      string serverInExtension = Path.Combine(extensionDir, "B2R2.FSLint.LanguageServer.exe");
      if (File.Exists(serverInExtension)) { return serverInExtension; }
      string envPath = Environment.GetEnvironmentVariable("FSLINT_SERVER_PATH");
      if (!string.IsNullOrEmpty(envPath))
      {
        if (File.Exists(envPath)) { return envPath; }
      }
      string[] possiblePaths = new[]
      {
        Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ProgramFiles), "FSLint", ServerExecutableName),
        Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ProgramFilesX86), "FSLint", ServerExecutableName),
        Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData), "FSLint", ServerExecutableName),
      };
      foreach (var path in possiblePaths)
      { if (File.Exists(path)) { return path; } }
      string pathEnv = Environment.GetEnvironmentVariable("PATH");
      if (!string.IsNullOrEmpty(pathEnv))
      {
        foreach (var path in pathEnv.Split(Path.PathSeparator))
        {
          try
          {
            string fullPath = Path.Combine(path, ServerExecutableName);
            if (File.Exists(fullPath)) { return fullPath; }
          }
          catch
          {
          }
        }
      }
      return null;
    }
  }
}