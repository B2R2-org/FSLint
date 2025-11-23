import * as path from 'path';
import * as vscode from 'vscode';
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient | undefined;

/**
 * Find the FSLint language server executable
 */
function findServerExecutable(context: vscode.ExtensionContext): string {
  // Check custom path from settings
  const config = vscode.workspace.getConfiguration('fslint');
  const customPath = config.get<string>('serverPath');
  
  if (customPath && customPath.trim() !== '') {
    return customPath;
  }
  
  // Default: look for server in extension directory
  const platform = process.platform;
  const serverName = platform === 'win32' 
    ? 'FSLint.LanguageServer.exe' 
    : 'FSLint.LanguageServer';
  
  // Try extension's bin directory
  const serverPath = context.asAbsolutePath(path.join('bin', serverName));
  
  return serverPath;
}

/**
 * Activate the extension
 */
export function activate(context: vscode.ExtensionContext) {
  console.log('FSLint extension is now active');
  
  // Check if FSLint is enabled
  const config = vscode.workspace.getConfiguration('fslint');
  if (!config.get<boolean>('enable')) {
    console.log('FSLint is disabled');
    return;
  }
  
  // Find server executable
  const serverPath = findServerExecutable(context);
  console.log(`FSLint server path: ${serverPath}`);
  
  // Check if server exists
  const fs = require('fs');
  if (!fs.existsSync(serverPath)) {
    vscode.window.showErrorMessage(
      `FSLint Language Server not found at: ${serverPath}\n` +
      `Please build the server or set a custom path in settings.`
    );
    return;
  }
  
  // Server options
  const serverOptions: ServerOptions = {
    command: serverPath,
    args: [],
    transport: TransportKind.stdio,
    options: {
      env: process.env
    }
  };
  
  // Client options
  const clientOptions: LanguageClientOptions = {
    // Register for F# documents
    documentSelector: [
      { scheme: 'file', language: 'fsharp' }
    ],
    
    // Create output channel
    outputChannel: vscode.window.createOutputChannel('FSLint Language Server'),
    
    // Trace communication (from settings)
    traceOutputChannel: vscode.window.createOutputChannel('FSLint Language Server Trace'),
    
    // Synchronize configuration
    synchronize: {
      configurationSection: 'fslint',
      fileEvents: vscode.workspace.createFileSystemWatcher('**/*.fs')
    }
  };
  
  // Create language client
  client = new LanguageClient(
    'fslint',
    'FSLint Language Server',
    serverOptions,
    clientOptions
  );
  
  // Start the client (and server)
  client.start().then(() => {
    console.log('FSLint Language Server started successfully');
    vscode.window.showInformationMessage('FSLint is now active');
  }).catch(error => {
    console.error('Failed to start FSLint Language Server:', error);
    vscode.window.showErrorMessage(
      `Failed to start FSLint Language Server: ${error.message}`
    );
  });
  
  // Register commands
  const restartCommand = vscode.commands.registerCommand(
    'fslint.restart',
    async () => {
      if (client) {
        await client.stop();
        await client.start();
        vscode.window.showInformationMessage('FSLint Language Server restarted');
      }
    }
  );
  
  context.subscriptions.push(restartCommand);
}

/**
 * Deactivate the extension
 */
export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  
  console.log('Stopping FSLint Language Server');
  return client.stop();
}