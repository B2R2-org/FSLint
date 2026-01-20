#!/bin/bash
set -e

echo "Building FSLint Language Server..."
dotnet publish -c Release \
  ../../src/FSLint.LanguageServer/B2R2.FSLint.LanguageServer.fsproj \
  -o bin

echo ""
echo "Installing npm dependencies..."
npm install --legacy-peer-deps

echo ""
echo "Running npm audit fix..."
npm audit fix --legacy-peer-deps 2>/dev/null || true

echo ""
echo "Compiling TypeScript..."
npm run compile

echo ""
echo "Packaging VSIX..."
npx @vscode/vsce@latest package

VSIX_FILE=$(ls -t *.vsix | head -1)

# Check if VS Code is running
if tasklist 2>/dev/null | grep -q "Code.exe"; then
    echo ""
    echo "ERROR: VS Code is currently running!"
    echo "       Please close VS Code before installing the extension."
    echo ""
    read -p "Press Enter after closing VS Code..."
    
    if tasklist 2>/dev/null | grep -q "Code.exe"; then
        echo "ERROR: VS Code is still running. Please close it and try again."
        exit 1
    fi
fi

echo ""
echo "Install: code --install-extension $VSIX_FILE"
echo ""
read -p "Proceed with installation? (y/n) " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
    code --install-extension $VSIX_FILE
    echo "Installation complete! Please restart VS Code."
fi