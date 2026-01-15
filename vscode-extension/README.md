# FSLint - Fast F# Linter for CI/CD

High-performance F# linter based on syntactic analysis, designed for 
seamless CI/CD integration.

## Features

- **Real-time Feedback**: Instant lint warnings as you type
- **CI/CD Ready**: File-level analysis without dependency resolution
- **Zero Configuration**: Works out of the box
- **Workspace Scanning**: Analyzes all F# files on startup

## Quick Start

1. Install the extension
2. Open any F# file or workspace
3. Lint warnings appear automatically with visual indicators

## Coding Conventions

FSLint enforces F# coding conventions based on the 
[B2R2 Contributing Guidelines](https://github.com/B2R2-org/B2R2/blob/main/CONTRIBUTING.md):

- Line length limits (80 columns)
- Naming conventions (PascalCase/camelCase)
- Indentation consistency
- Pattern matching completeness
- Type annotation usage
- Function definition formatting
- List operation patterns

## Requirements

- .NET SDK 10.0 or higher
- F# source files (`.fs`)

## Extension Settings

This extension contributes the following settings:

- `fslint.enable`: Enable/disable FSLint (default: `true`)
- `fslint.trace.server`: Trace LSP communication for debugging

## Known Issues

- Workspace scanning triggers on first file open or initialized event
- Large workspaces may take time for initial scanning

## Release Notes

### 1.0.0

Initial release:
- Real-time linting with LSP integration
- Workspace-wide scanning
- File-level syntactic analysis

## Contributing

Issues and pull requests welcome at 
[GitHub repository](https://github.com/B2R2-org/FSLint)

## License

MIT License - see LICENSE file for details

---

**Note**: FSLint uses syntactic analysis only, providing fast feedback 
for style and convention violations without semantic type checking.