@echo off
setlocal EnableExtensions

rem Optional override: set FFMPEG_DEV_PATH to your Delphi-FFMPEG clone (see InitDelphiFFMPEG.local.cmd.example)
if exist "%~dp0InitDelphiFFMPEG.local.cmd" call "%~dp0InitDelphiFFMPEG.local.cmd"
if not defined FFMPEG_DEV_PATH set "FFMPEG_DEV_PATH=%~dp0..\..\Delphi-FFMPEG"

echo === JEDI submodule ===
git submodule update --init jedi
if errorlevel 1 goto :error
pushd jedi
git checkout master 2>nul
popd

echo.
echo === Delphi-FFMPEG ===
if exist "%~dp0Delphi-FFMPEG\source\ffmpeg.inc" (
  echo Delphi-FFMPEG already available at Delphi-FFMPEG\source
  goto :done
)

if exist "%FFMPEG_DEV_PATH%\source\ffmpeg.inc" (
  echo Linking Delphi-FFMPEG junction -^> "%FFMPEG_DEV_PATH%"
  if exist "%~dp0Delphi-FFMPEG" (
    rem empty gitlink dir or stale folder
    rmdir "%~dp0Delphi-FFMPEG" 2>nul
    if exist "%~dp0Delphi-FFMPEG" (
      echo ERROR: remove or rename existing Delphi-FFMPEG folder, then re-run.
      goto :error
    )
  )
  mklink /J "%~dp0Delphi-FFMPEG" "%FFMPEG_DEV_PATH%"
  if errorlevel 1 (
    echo ERROR: mklink failed. Try an elevated command prompt or enable Developer Mode.
    goto :error
  )
  goto :done
)

echo Initializing Delphi-FFMPEG submodule...
git submodule update --init Delphi-FFMPEG
if errorlevel 1 goto :error

:done
echo.
echo Done.
goto :eof

:error
echo Setup failed.
exit /b 1
