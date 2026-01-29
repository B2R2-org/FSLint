# FSLint - Fast F# Linter for CI/CD

High-performance F# linter based on syntactic analysis, designed for seamless CI/CD integration.

## Features

- **Real-time Feedback**: Instant lint warnings as you type with inline diagnostic messages
- **CI/CD Ready**: File-level analysis without dependency resolution
 
## Quick Start

1. Install the extension from Visual Studio Marketplace
2. Open any F# solution or file
3. Lint warnings appear automatically with visual indicators

## Visual Features

- **Error List Integration**: All diagnostics also appear in Visual Studio's Error List
- **Squiggly Underlines**: Traditional Visual Studio error highlighting

## Coding Conventions

FSLint enforces F# coding conventions based on the [B2R2 Contributing Guidelines](https://github.com/B2R2-org/B2R2/blob/main/CONTRIBUTING.md):

- Line length limits (80 columns)
- Naming conventions (PascalCase/camelCase)
- Indentation consistency
- Pattern matching completeness
- Type annotation usage
- Function definition formatting
- List operation patterns

## Requirements

- Visual Studio 2022 (17.0 or higher)
- .NET SDK 10.0 or higher
- F# language support installed

## Extension Settings

View the FSLint output window:
- View → Output → Select "FSLint" from dropdown

## Release Notes

### 1.0.0

Initial release:
- Real-time linting with LSP integration
- Workspace-wide scanning
- File-level syntactic analysis
## Contributing

Issues and pull requests welcome at [GitHub repository](https://github.com/B2R2-org/FSLint)

## License

MIT License - see LICENSE.txt file for details