Write-Host "Building FSLint Language Server..." -ForegroundColor Green
Set-Location ..\src\FSLint.LanguageServer
dotnet publish -c Release -o ..\..\vscode-extension\bin

Write-Host "Building VS Code Extension..." -ForegroundColor Green
Set-Location ..\..\vscode-extension
npm install
npm run compile

Write-Host "Creating VSIX package..." -ForegroundColor Green
npm run package

Write-Host "Done! Install with: code --install-extension fslint-1.0.0.vsix" -ForegroundColor Green
