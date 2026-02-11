# FSLint

[![VS Code Marketplace](https://img.shields.io/visual-studio-marketplace/v/B2R2.fslint?style=flat-square&label=VS%20Code&logo=visual-studio-code)](https://marketplace.visualstudio.com/items?itemName=B2R2.fslint) [![VS Marketplace](https://img.shields.io/visual-studio-marketplace/v/B2R2.B2R2?style=flat-square&label=Visual%20Studio&logo=visual-studio)](https://marketplace.visualstudio.com/items?itemName=B2R2.B2R2) [![NuGet](https://img.shields.io/nuget/v/B2R2.FSLint?style=flat-square&logo=nuget)](https://www.nuget.org/packages/B2R2.FSLint) [![License](https://img.shields.io/github/license/B2R2-org/B2R2?style=flat-square)](https://github.com/B2R2-org/B2R2/blob/main/LICENSE.md)


High-performance F# linter based on syntactic analysis, providing real-time feedback in IDEs and seamless CI/CD integration.

## The Problem

Your F# codebase grows. Style inconsistencies creep in. Code reviews catch formatting issues instead of logic bugs. CI pipelines slow down waiting for linters that need full semantic analysis.

## The Solution

FSLint gives you **real-time style enforcement** without the wait. No dependency resolution. No type checking overhead. Just instant feedback on what matters.

## What You Get

**Zero Setup** — Install and start linting. No config files required.

**Instant Feedback** — See violations as you type, not after you push.

**CI/CD Native** — File-level analysis works in any pipeline, any environment.

**Precise Rules** — Based on [F# official guidelines](https://learn.microsoft.com/en-us/dotnet/fsharp/style-guide/) and [B2R2 F# conventions](https://github.com/B2R2-org/B2R2/blob/main/CONTRIBUTING.md).

## Quick Start

**VS Code**
```bash
ext install B2R2.fslint
```

**Visual Studio**
```
Extensions → Manage Extensions → Search "FSLint"
```

**Command Line (CI/CD)**
```bash
dotnet tool install --global B2R2.FSLint
fslint check ./src
```

Open any `.fs` file. Violations appear instantly with visual indicators.

## Why Syntactic Analysis?

FSLint analyzes your code **structure**, not its **semantics**. This means:
- No waiting for type inference
- No dependency graph resolution
- Works on incomplete code
- Scales to massive codebases

Perfect for catching style violations the moment you write them.

## Built by Security Researchers

Developed at KAIST SoftSec Lab for the [B2R2 binary analysis platform](https://github.com/B2R2-org/B2R2) — a project with 700,000+ lines of F# that demands both speed and precision.

## Requirements

- .NET SDK 10.0+
- F# source files (`.fs`)

## Extension Settings

- `fslint.enable` — Toggle linting (default: `true`)
- `fslint.trace.server` — Debug LSP communication

## Configuration

FSLint respects `.editorconfig` settings in your workspace:
```ini
[*.fs]
max_line_length = 120  # Default: 80
```

## Release Notes

### 1.0.3

- Relax Declaration and ArrayOrList rules
- Use ParsedInput instead of source-aware algorithm

### 1.0.2

- Linting is no longer performed in real time and is now executed on file save.

## Contributing

Found an issue? Have a suggestion?
→ [GitHub Issues](https://github.com/B2R2-org/FSLint/issues)

## License

MIT — Free for commercial and open source use.

---

*Stop debating style in code reviews. Enforce it automatically.*