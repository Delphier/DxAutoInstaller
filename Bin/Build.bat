@echo off
chcp 65001
cls

set AppName=DxAutoInstaller
set /p Version=<..\Resources\Version.txt
set Version=%Version:'=%
set VersionInfo="CompanyName=https://github.com/Delphier/DxAutoInstaller;FileDescription=DevExpress VCL Components Automatic Installer;FileVersion=%Version%;LegalCopyright=Copyright © 2014-2026 faceker@gmail.com;ProductName=%AppName%;ProductVersion=%Version%"

call "C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\rsvars.bat"
call :Build Win32 -x86
call :Build Win64 -x64

pause
exit /b

:Build
	MSBuild "..\%AppName%.dproj" /t:Build /p:config=Release /p:platform=%1 /p:VerInfo_IncludeVerInfo=true /p:VerInfo_Keys=%VersionInfo%
	tar -caf %AppName%-%Version%%~2.zip %AppName%.exe
