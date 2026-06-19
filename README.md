B2R2.FSLint
===

![](https://img.shields.io/github/license/B2R2-org/B2R2.svg?style=flat)
[![](https://img.shields.io/nuget/v/B2R2.RearEnd.Launcher)](https://www.nuget.org/packages/B2R2.RearEnd.Launcher/)

Linting tool for B2R2 projects written in F#.

## Motivation

We need a lightweight linter for our F# projects. The goal is to have a tool
that can be used in the CI pipeline to ensure that the code adheres to our style
guidelines. Unfortunately, existing tools like
[FSharpLint](https://github.com/fsprojects/FSharpLint) does not meet our needs
and is not actively maintained. So we decided to create our own linter.

## Installation

### From Marketplace

**VS Code:**
- Install from [Visual Studio Code Marketplace](https://marketplace.visualstudio.com/items?itemName=B2R2.fslint)
- Or search for "FSLint" in VS Code Extensions view

**Visual Studio:**
- Install from [Visual Studio Marketplace](https://marketplace.visualstudio.com/items?itemName=B2R2.B2R2)
- Or use Extensions → Manage Extensions in Visual Studio

### Manual Installation

**VS Code:**
```bash
cd extension/vscode
./build.sh
code --install-extension fslint-*.vsix
```

**Visual Studio:**
```powershell
cd extension/vs
.\build.ps1
# Then install the generated .vsix file through Visual Studio
```
