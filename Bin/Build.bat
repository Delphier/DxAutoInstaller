@echo off
cmd.exe /c " "C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\rsvars.bat" && pwsh.exe %~dp0Build.ps1 "
pause
