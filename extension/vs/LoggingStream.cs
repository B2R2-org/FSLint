using System;
using System.IO;
using System.Text;

namespace FSLint.VisualStudio
{
    /// <summary>
    /// Stream wrapper that logs all data passing through for debugging LSP communication
    /// </summary>
    internal class LoggingStream : Stream
    {
        private readonly Stream innerStream;
        private readonly string direction;
        private readonly StringBuilder buffer = new StringBuilder();

        public LoggingStream(Stream innerStream, string direction)
        {
            this.innerStream = innerStream;
            this.direction = direction;
        }

        public override int Read(byte[] buffer, int offset, int count)
        {
            int bytesRead = innerStream.Read(buffer, offset, count);
            if (bytesRead > 0)
            {
                LogData(buffer, offset, bytesRead, "READ");
            }
            return bytesRead;
        }

        public override void Write(byte[] buffer, int offset, int count)
        {
            LogData(buffer, offset, count, "WRITE");
            innerStream.Write(buffer, offset, count);
        }

        private void LogData(byte[] data, int offset, int count, string operation)
        {
            try
            {
                string text = Encoding.UTF8.GetString(data, offset, count);
                buffer.Append(text);

                string bufferedText = buffer.ToString();
                int headerEnd = bufferedText.IndexOf("\r\n\r\n");

                while (headerEnd >= 0)
                {
                    string headers = bufferedText.Substring(0, headerEnd);

                    int contentLength = 0;
                    foreach (string line in headers.Split(new[] { "\r\n" }, StringSplitOptions.RemoveEmptyEntries))
                    {
                        if (line.StartsWith("Content-Length: "))
                        {
                            int.TryParse(line.Substring(16), out contentLength);
                            break;
                        }
                    }

                    int messageStart = headerEnd + 4;
                    if (bufferedText.Length >= messageStart + contentLength)
                    {
                        string message = bufferedText.Substring(messageStart, contentLength);

                        OutputWindowHelper.WriteLine($"[{direction}] {operation}:");
                        OutputWindowHelper.WriteLine($"Headers: {headers.Replace("\r\n", " | ")}");

                        OutputWindowHelper.WriteLine($"Content: {message}");
                        OutputWindowHelper.WriteLine("---");

                        bufferedText = bufferedText.Substring(messageStart + contentLength);
                        buffer.Clear();
                        buffer.Append(bufferedText);

                        headerEnd = bufferedText.IndexOf("\r\n\r\n");
                    }
                    else
                    {
                        break;
                    }
                }
            }
            catch (Exception ex)
            {
                OutputWindowHelper.WriteLine($"[{direction}] Logging error: {ex.Message}");
            }
        }

        public override bool CanRead => innerStream.CanRead;
        public override bool CanSeek => innerStream.CanSeek;
        public override bool CanWrite => innerStream.CanWrite;
        public override long Length => innerStream.Length;
        public override long Position
        {
            get => innerStream.Position;
            set => innerStream.Position = value;
        }

        public override void Flush() => innerStream.Flush();
        public override long Seek(long offset, SeekOrigin origin) => innerStream.Seek(offset, origin);
        public override void SetLength(long value) => innerStream.SetLength(value);

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                innerStream?.Dispose();
            }
            base.Dispose(disposing);
        }
    }
}