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
$VersionInfoRaw = @"
CompanyName=https://github.com/Delphier/DxAutoInstaller;
FileDescription={0};
FileVersion=$Version;
LegalCopyright=Copyright © 2014-2026 faceker@gmail.com;
ProductName=$AppName {1};
ProductVersion=${Version}_
"@ -replace "\r|\n", ''
$VersionInfoGUI = $VersionInfoRaw -f "DevExpress VCL Components Automatic Installer", "GUI"
$VersionInfoCLI = $VersionInfoRaw -f "$AppName Command Line Interface", "CLI"

function BuildProject($ProjectName, $Platform, $VersionInfo) {
    $ProjectFileName = Join-Path $ProjectDir "$ProjectName.dproj"
    cmd /c MSBuild $ProjectFileName `
        /t:Build `
        /p:config=Release `
        /p:platform=$Platform `
        /p:VerInfo_IncludeVerInfo=true `
        /p:VerInfo_Keys=`"$VersionInfo$Platform`"
}

function Build($Platform, $FileSuffix) {
    Write-Host "============================================" -ForegroundColor Green
    Write-Host "Building $AppName v$Version for $Platform..." -ForegroundColor Blue
    Write-Host "============================================" -ForegroundColor Green
    BuildProject $AppName $Platform $VersionInfoGUI
    #BuildProject $AppNameCLI $Platform $VersionInfoCLI
    
    tar -caf `
        (Join-Path $ReleasesDir "$AppName-$Version$FileSuffix.zip") `
        -C $PSScriptRoot `
        "$AppName.exe" `
        #"$AppNameCLI.exe"
}

Build Win32 '-x86'
Build Win64 '-x64'
