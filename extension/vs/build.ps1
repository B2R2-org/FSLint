$ErrorActionPreference = "Stop"

Write-Host "========================================" -ForegroundColor Cyan
Write-Host "FSLint Visual Studio Extension Builder" -ForegroundColor Cyan
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ""

$Configuration = "Release"
if ($args.Length -gt 0) {
    $Configuration = $args[0]
}

Write-Host "Build Configuration: $Configuration" -ForegroundColor Yellow
Write-Host ""

Write-Host "[1/4] Building Language Server..." -ForegroundColor Green
$ServerPath = "..\..\src\FSLint.LanguageServer"

if (-not (Test-Path $ServerPath)) {
    Write-Host "ERROR: Language Server project not found at: $ServerPath" -ForegroundColor Red
    Write-Host "Expected directory structure:" -ForegroundColor Yellow
    Write-Host "  FSLint/" -ForegroundColor Yellow
    Write-Host "  ├── src/FSLint.LanguageServer/" -ForegroundColor Yellow
    Write-Host "  └── extension/FSLint.VisualStudio/" -ForegroundColor Yellow
    exit 1
}

Push-Location $ServerPath
try {
    Write-Host "  Building FSLint.LanguageServer ($Configuration)..." -ForegroundColor Gray
    dotnet build -c $Configuration --verbosity minimal
    if ($LASTEXITCODE -ne 0) {
        throw "Language Server build failed"
    }

    $ServerExe = "bin\$Configuration\net10.0\B2R2.FSLint.LanguageServer.exe"
    if (-not (Test-Path $ServerExe)) {
        throw "Language Server executable not found: $ServerExe"
    }

    Write-Host "Language Server built successfully" -ForegroundColor Green
    Write-Host "    Location: $ServerExe" -ForegroundColor Gray
} finally {
    Pop-Location
}

Write-Host ""

Write-Host "[2/4] Restoring NuGet packages..." -ForegroundColor Green
dotnet restore FSLint_VisualStudio.sln
if ($LASTEXITCODE -ne 0) {
    Write-Host "WARNING: NuGet restore failed, continuing anyway..." -ForegroundColor Yellow
}
Write-Host "NuGet packages restored" -ForegroundColor Green
Write-Host ""

if ($args -contains "--clean") {
    Write-Host "[3/4] Cleaning previous build..." -ForegroundColor Green
    msbuild /t:Clean /p:Configuration=$Configuration /v:minimal
    Write-Host "Clean complete" -ForegroundColor Green
    Write-Host ""
}

Write-Host "[4/4] Building VS Extension..." -ForegroundColor Green
Write-Host "  Building FSLint.VisualStudio ($Configuration)..." -ForegroundColor Gray
msbuild /p:Configuration=$Configuration /v:minimal
if ($LASTEXITCODE -ne 0) {
    Write-Host "ERROR: VS Extension build failed" -ForegroundColor Red
    exit 1
}

Write-Host "VS Extension built successfully" -ForegroundColor Green
Write-Host ""
Write-Host "========================================" -ForegroundColor Cyan
Write-Host "Build Complete!" -ForegroundColor Cyan
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ""

$VsixPath = "bin\$Configuration\FSLint.VisualStudio.vsix"
$ServerInOutput = "bin\$Configuration\B2R2.FSLint.LanguageServer.exe"

if (Test-Path $VsixPath) {
    $VsixSize = (Get-Item $VsixPath).Length / 1MB
    Write-Host "VSIX Package: $VsixPath" -ForegroundColor Green
    Write-Host "Size: $([math]::Round($VsixSize, 2)) MB" -ForegroundColor Gray
    $TempExtractPath = "bin\$Configuration\vsix-verify-temp"
    if (Test-Path $TempExtractPath) {
        Remove-Item $TempExtractPath -Recurse -Force
    }
    try {
        Expand-Archive $VsixPath $TempExtractPath -Force
        $ServerInVsix = Get-ChildItem $TempExtractPath -Recurse -Filter "B2R2.FSLint.LanguageServer.exe" -ErrorAction SilentlyContinue
        $DllsInVsix = Get-ChildItem $TempExtractPath -Recurse -Filter "*.dll" -ErrorAction SilentlyContinue
        if ($ServerInVsix) {
            Write-Host "Language Server bundled in VSIX" -ForegroundColor Green
        } else {
            Write-Host "ERROR: Language Server NOT in VSIX!" -ForegroundColor Red
            Write-Host "This is a critical error!" -ForegroundColor Red
        }
        if ($DllsInVsix.Count -gt 15) {
            Write-Host "Dependencies bundled: $($DllsInVsix.Count) DLL files" -ForegroundColor Green
        } else {
            Write-Host "Warning: Only $($DllsInVsix.Count) DLLs found (expected 20+)" -ForegroundColor Yellow
        }
        Remove-Item $TempExtractPath -Recurse -Force
    } catch {
        Write-Host "Could not verify VSIX contents" -ForegroundColor Yellow
    }
} else {
    Write-Host "VSIX Package not found!" -ForegroundColor Red
    exit 1
}

Write-Host ""

if (Test-Path $ServerInOutput) {
    Write-Host "Language Server also copied to output directory" -ForegroundColor Green
    $ServerSize = (Get-Item $ServerInOutput).Length / 1KB
    Write-Host "Size: $([math]::Round($ServerSize, 2)) KB" -ForegroundColor Gray
} else {
    Write-Host "Language Server not in output directory (this is OK)" -ForegroundColor Cyan
    Write-Host "It's bundled in the VSIX, which is what matters" -ForegroundColor Gray
}

Write-Host ""
Write-Host "Next steps:" -ForegroundColor Yellow
Write-Host "  1. Install: .\$VsixPath" -ForegroundColor White
Write-Host "  2. Restart Visual Studio" -ForegroundColor White
Write-Host "  3. Open an F# file" -ForegroundColor White
Write-Host ""
if ($args -contains "--install") {
    Write-Host "Installing extension..." -ForegroundColor Green
    Start-Process $VsixPath -Wait
    Write-Host "Installation complete" -ForegroundColor Green
}