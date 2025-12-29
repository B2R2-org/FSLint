import * as path from 'path';
import * as vscode from 'vscode';
import {
  LanguageClient,
  TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient | undefined;

class FSLintInlayHintsProvider implements vscode.InlayHintsProvider {
  private _onDidChangeInlayHints = new vscode.EventEmitter<void>();
  public readonly onDidChangeInlayHints = this._onDidChangeInlayHints.event;
  public refresh(): void { this._onDidChangeInlayHints.fire(); }
  provideInlayHints(
    document: vscode.TextDocument,
    range: vscode.Range,
    token: vscode.CancellationToken
  ): vscode.InlayHint[] {
    if (token.isCancellationRequested) {
      return [];
    }
    const diagnostics = vscode.languages.getDiagnostics(document.uri);
    const hints: vscode.InlayHint[] = [];
    const diagnosticsByLine = new Map<number, vscode.Diagnostic[]>();

    for (const diag of diagnostics) {
      if (diag.source === 'FSLint' && range.contains(diag.range)) {
        const line = diag.range.start.line;
        if (!diagnosticsByLine.has(line)) {
          diagnosticsByLine.set(line, []);
        }
        diagnosticsByLine.get(line)!.push(diag);
      }
    }

    for (const [errorLine, diags] of diagnosticsByLine) {
      diags.sort((a, b) => a.range.start.character - b.range.start.character);
      let lineOffset = 0;
      for (const diag of diags) {
        const startCol = diag.range.start.character;
        const endCol = diag.range.end.character;
        const carets = '^'.repeat(Math.max(1, endCol - startCol));
        const spacing = ' '.repeat(startCol);
        const nextLine = errorLine + 1 + lineOffset;

        if (nextLine < document.lineCount) {
          const nextLineText = document.lineAt(nextLine).text;
          if (nextLineText.trim().length === 0) {
            const hint = new vscode.InlayHint(
              new vscode.Position(nextLine, 0),
              `${spacing}${carets} ${diag.message}`,
              vscode.InlayHintKind.Type
            );
            hints.push(hint);
            lineOffset++;
          }
        }
      }
    }

    return hints;
  }
}

const underlineDecorationType = vscode.window.createTextEditorDecorationType({
  textDecoration: 'underline wavy',
  borderColor: new vscode.ThemeColor('editorWarning.foreground'),
  borderWidth: '0 0 2px 0',
  borderStyle: 'none none wavy none'
});

function updateDecorations(editor: vscode.TextEditor) {
  const diagnostics = vscode.languages.getDiagnostics(editor.document.uri);
  const underlines: vscode.DecorationOptions[] = [];
  
  for (const diag of diagnostics) {
    if (diag.source === 'FSLint') {
      underlines.push({
        range: diag.range,
        hoverMessage: diag.message
      });
    }
  }
  
  editor.setDecorations(underlineDecorationType, underlines);
}

export function activate(context: vscode.ExtensionContext) {
  const workspaceFolder = vscode.workspace.workspaceFolders?.[0];
  if (!workspaceFolder) {
    vscode.window.showErrorMessage('FSLint requires a workspace folder');
    return;
  }

  const serverPath = context.asAbsolutePath(
    path.join('bin', process.platform === 'win32' ? 'FSLint.LanguageServer.exe' : 'FSLint.LanguageServer')
  );

  const fs = require('fs');
  if (!fs.existsSync(serverPath)) {
    vscode.window.showErrorMessage(`FSLint server not found: ${serverPath}`);
    return;
  }

  const inlayHintsProvider = new FSLintInlayHintsProvider();
  context.subscriptions.push(
    vscode.languages.registerInlayHintsProvider(
      { scheme: 'file', language: 'fsharp' },
      inlayHintsProvider
    )
  );

  client = new LanguageClient(
    'fslint',
    'FSLint',
    { command: serverPath,
      args: [workspaceFolder.uri.fsPath],
      transport: TransportKind.stdio },
    { documentSelector: [{ scheme: 'file', language: 'fsharp' }],
      outputChannel: vscode.window.createOutputChannel('FSLint'),
      workspaceFolder: workspaceFolder }
  );

  client.start().then(() => {
    const updateEditor = (editor: vscode.TextEditor | undefined) => {
      if (editor && editor.document.languageId === 'fsharp') {
        updateDecorations(editor);
        inlayHintsProvider.refresh();
      }
    };
    
    context.subscriptions.push(
      vscode.languages.onDidChangeDiagnostics(e => {
        for (const uri of e.uris) {
          const editor = vscode.window.visibleTextEditors.find(
            ed => ed.document.uri.toString() === uri.toString()
          );
          if (editor) updateEditor(editor);
        }
      })
    );
    
    context.subscriptions.push(
      vscode.window.onDidChangeActiveTextEditor(updateEditor)
    );
    
    context.subscriptions.push(
      vscode.workspace.onDidChangeTextDocument(e => {
        const editor = vscode.window.activeTextEditor;
        if (editor && editor.document === e.document) {
          setTimeout(() => updateEditor(editor), 100);
        }
      })
    );
    
    if (vscode.window.activeTextEditor) {
      setTimeout(() => updateEditor(vscode.window.activeTextEditor), 500);
    }
  });
  
  context.subscriptions.push(
    vscode.commands.registerCommand('fslint.restart', async () => {
      if (client) {
        await client.stop();
        await client.start();
      }
    })
  );
}

export function deactivate(): Thenable<void> | undefined {
  underlineDecorationType.dispose();
  return client?.stop();
}