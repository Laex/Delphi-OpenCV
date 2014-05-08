@echo off
if not exist "!Install!" mkdir "!Install!"
if exist "!Install!\Source" del /s /q "!Install!\Source"
xcopy /s /EXCLUDE:.\xcopyignore ".\bin\*.*" "!Install!\Source\bin\"
xcopy /s /EXCLUDE:.\xcopyignore ".\samples\*.*" "!Install!\Source\samples\"
xcopy /s /EXCLUDE:.\xcopyignore ".\include\*.*" "!Install!\Source\include\"
xcopy /s /EXCLUDE:.\xcopyignore ".\component\*.*" "!Install!\Source\component\"

