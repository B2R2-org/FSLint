#!/bin/bash
set -e

echo "Building FSLint Language Server..."
cd ../src/FSLint.LanguageServer
dotnet publish -c Release -o ../../vscode-extension/bin

echo "Building VS Code Extension..."
cd ../../vscode-extension
npm install
npm run compile

echo "Creating VSIX package..."
npm run package

echo "Done! Install with: code --install-extension fslint-1.0.0.vsix"