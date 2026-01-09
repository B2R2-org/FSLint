import * as path from 'path';
import * as vscode from 'vscode';
import {
  LanguageClient,
  TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient | undefined;
let diagnosticCollection: vscode.DiagnosticCollection;

const warningDecoration = vscode.window.createTextEditorDecorationType({
  backgroundColor: new vscode.ThemeColor('editorWarning.background'),
  borderRadius: '2px',
  light: {
    backgroundColor: 'rgba(255, 100, 100, 0.15)',
  },
  dark: {
    backgroundColor: 'rgba(255, 100, 100, 0.2)',
  }
});

const errorDecoration = vscode.window.createTextEditorDecorationType({
  backgroundColor: new vscode.ThemeColor('editorError.background'),
  borderRadius: '2px',
  light: {
    backgroundColor: 'rgba(255, 100, 100, 0.15)',
  },
  dark: {
    backgroundColor: 'rgba(255, 100, 100, 0.2)',
  }
});

class FSLintCodeLensProvider implements vscode.CodeLensProvider {
  private _onDidChangeCodeLenses = new vscode.EventEmitter<void>();
  public readonly onDidChangeCodeLenses = this._onDidChangeCodeLenses.event;  
  public refresh(): void { this._onDidChangeCodeLenses.fire(); }

  provideCodeLenses(document: vscode.TextDocument): vscode.CodeLens[] {
    const currentPath = document.uri.fsPath.toLowerCase().replace(/\\/g, '/');
    const allDiagnostics = vscode.languages.getDiagnostics();    
    let matchedDiagnostics: vscode.Diagnostic[] = [];

    for (const [uri, diags] of allDiagnostics) {
      const diagPath = uri.fsPath.toLowerCase().replace(/\\/g, '/');
      if (diagPath === currentPath) {
        matchedDiagnostics = diags.filter(d => d.source === 'FSLint');
        break;
      }
    }

    if (matchedDiagnostics.length === 0) { return []; }

    const codeLenses: vscode.CodeLens[] = [];

    for (const diag of matchedDiagnostics) {
      const lineText = document.lineAt(diag.range.start.line).text;
      const leadingWhitespace = lineText.match(/^(\s*)/)?.[1].length || 0;
      const spacing = '⠀'.repeat(Math.max(0, diag.range.start.character - leadingWhitespace));
      const codeLensRange = new vscode.Range(
        diag.range.start.line,
        0,
        diag.range.start.line,
        0
      );
      if (diag.message == 'Use single blank line') {
        codeLenses.push(
          new vscode.CodeLens(codeLensRange, {
            title: `${diag.message}`,
            command: ''
          })
        );
      } else {
        codeLenses.push(
          new vscode.CodeLens(codeLensRange, {
            title: `${spacing}${diag.message}`,
            command: ''
          })
        );
      }
    }
    return codeLenses;
  }
}

function updateDecorations(editor: vscode.TextEditor) {
  if (!editor || editor.document.languageId !== 'fsharp') {
    return;
  }

  const diagnostics = vscode.languages.getDiagnostics(editor.document.uri);
  const fslintDiagnostics = diagnostics.filter(d => d.source === 'FSLint');

  const warnings: vscode.DecorationOptions[] = [];
  const errors: vscode.DecorationOptions[] = [];

  for (const diag of fslintDiagnostics) {
    const decoration: vscode.DecorationOptions = {
      range: diag.range,
      hoverMessage: `FSLint: ${diag.message}`
    };

    if (diag.severity === vscode.DiagnosticSeverity.Error) {
      errors.push(decoration);
    } else {
      warnings.push(decoration);
    }
  }

  editor.setDecorations(warningDecoration, warnings);
  editor.setDecorations(errorDecoration, errors);
}

export function activate(context: vscode.ExtensionContext) {
  const workspaceFolders = vscode.workspace.workspaceFolders;
  const serverPath = context.asAbsolutePath(
    path.join('bin', process.platform === 'win32' ? 'B2R2.FSLint.LanguageServer.exe' : 'B2R2.FSLint.LanguageServer')
  );

  const fs = require('fs');
  if (!fs.existsSync(serverPath)) {
    vscode.window.showErrorMessage(`FSLint server not found: ${serverPath}`);
    return;
  }

  const codeLensProvider = new FSLintCodeLensProvider();

  context.subscriptions.push(
    vscode.languages.registerCodeLensProvider(
      { scheme: 'file', language: 'fsharp' },
      codeLensProvider
    )
  );

  const rootPath = workspaceFolders && workspaceFolders.length > 0 
    ? workspaceFolders[0].uri.fsPath 
    : process.env.HOME || process.env.USERPROFILE || '/';

  client = new LanguageClient(
    'fslint',
    'FSLint',
    { command: serverPath,
      args: [rootPath],
      transport: TransportKind.stdio },
    { documentSelector: [{ scheme: 'file', language: 'fsharp' }],
      outputChannel: vscode.window.createOutputChannel('FSLint') }
  );

  client.start().then(() => {    
    const updateEditor = (editor: vscode.TextEditor | undefined) => {
      if (editor && editor.document.languageId === 'fsharp') {
        codeLensProvider.refresh();
        updateDecorations(editor);
      }
    };

    context.subscriptions.push(
      vscode.languages.onDidChangeDiagnostics(e => {
        const activeEditor = vscode.window.activeTextEditor;
        if (!activeEditor || activeEditor.document.languageId !== 'fsharp') {
          return;
        }
        const activePath = activeEditor.document.uri.fsPath.toLowerCase().replace(/\\/g, '/');

        for (const uri of e.uris) {
          const diagPath = uri.fsPath.toLowerCase().replace(/\\/g, '/');
          
          if (diagPath === activePath) {
            updateEditor(activeEditor);
            break;
          }
        }
      })
    );

    context.subscriptions.push(
      vscode.window.onDidChangeActiveTextEditor(editor => {
        updateEditor(editor);
      })
    );

    context.subscriptions.push(
      vscode.workspace.onDidChangeTextDocument(e => {
        const editor = vscode.window.activeTextEditor;
        if (editor && editor.document === e.document && editor.document.languageId === 'fsharp') {
          setTimeout(() => updateEditor(editor), 300);
        }
      })
    );

    context.subscriptions.push(
      vscode.workspace.onDidOpenTextDocument(doc => {
        if (doc.languageId === 'fsharp') {
          const editor = vscode.window.visibleTextEditors.find(e => e.document === doc);
          if (editor) {
            setTimeout(() => updateEditor(editor), 500);
          }
        }
      })
    );

    if (vscode.window.activeTextEditor) {
      setTimeout(() => updateEditor(vscode.window.activeTextEditor), 1000);
    }
  }).catch(err => {
    console.error('[FSLINT] Failed to start LSP client:', err);
  });

  context.subscriptions.push(
    vscode.commands.registerCommand('fslint.restart', async () => {
      if (client) {
        await client.stop();
        await client.start();
      }
    })
  );

  console.log('[FSLINT] Extension activated');
}

export function deactivate(): Thenable<void> | undefined {
  console.log('[FSLINT] Deactivating extension...');
  return client?.stop();
}