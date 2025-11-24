B2R2.FSLint
===

Linting tool for B2R2 projects written in F#.

## Motivation

We need a lightweight linter for our F# projects. The goal is to have a tool
that can be used in the CI pipeline to ensure that the code adheres to our style
guidelines. Unfortunately, existing tools like
[FSharpLint](https://github.com/fsprojects/FSharpLint) does not meet our needs
and is not actively maintained. So we decided to create our own linter.

## VS Code Extension Development

### Building the Extension
```bash
# 1. Build F# Language Server
cd src/FSLint.LanguageServer
dotnet publish -c Release -o ../../vscode-extension/bin

# 2. Build VS Code Extension
cd ../../vscode-extension
npm install
npm run compile

# 3. Package Extension (Optional)
npm run package  # Creates fslint-1.0.0.vsix
```

### Running in Development

1. Open `vscode-extension/` folder in VS Code
2. Press F5 to launch Extension Development Host
3. Open any `.fs` file to see FSLint diagnostics

### Installing the Extension
```bash
code --install-extension fslint-1.0.0.vsix
```