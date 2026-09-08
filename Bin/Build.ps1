#Requires -Version 7

$ProjectDir = Split-Path $PSScriptRoot -Parent
$ResourcesDir = Join-Path $ProjectDir Resources
$ReleasesDir = Join-Path $PSScriptRoot Releases
mkdir $ReleasesDir -Force | Out-Null

$AppInc = Get-Content (Join-Path $ResourcesDir App.inc) -Raw
$App = Invoke-Expression "@{ $AppInc }"
$AppName = $App.Name
$Version = $App.Version
$AppNameCLI = 'dx'

# Build via RAD Studio CLI: https://github.com/Delphier/radstudio
function BuildProject($ProjectName, $Platform, $ProductName, $FileDescription) {
    radstudio build (Join-Path $ProjectDir "$ProjectName.dproj") `
        --config=Release `
        --platform=$Platform `
        --FileVersion=$Version `
        --ProductVersion=${Version}_$Platform `
        --ProductName=$ProductName `
        --FileDescription=$FileDescription `
        --CompanyName=https://github.com/Delphier/DxAutoInstaller `
        --LegalCopyright="Copyright © 2014-2026 faceker@gmail.com"
}

function Build($Platform, $FileSuffix) {
    Write-Host "============================================" -ForegroundColor Green
    Write-Host "Building $AppName v$Version for $Platform..." -ForegroundColor Blue
    Write-Host "============================================" -ForegroundColor Green
    BuildProject $AppName $Platform "$AppName GUI" "DevExpress VCL Components Automatic Installer"
    BuildProject $AppNameCLI $Platform "$AppName CLI" "$AppName Command Line Interface"
    
    tar -caf `
        (Join-Path $ReleasesDir "$AppName-$Version$FileSuffix.zip") `
        -C $PSScriptRoot `
        "$AppName.exe" `
        "$AppNameCLI.exe"
}

Build Win32 '-x86'
Build Win64 '-x64'
