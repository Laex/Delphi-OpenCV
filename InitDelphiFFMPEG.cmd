@echo off
rem Get the submodule initially
@git submodule update --init
@git submodule update

rem Time passes, submodule upstream is updated
rem and you now want to update

echo Update Delphi-FFMPEG
rem Change to the submodule directory
cd Delphi-FFMPEG
rem Checkout desired branch
git checkout master

rem Get back to your project root
cd ..

echo JEDI
rem Change to the submodule directory
cd jedi
rem Checkout desired branch
git checkout master

rem Update
git pull

rem Get back to your project root
cd ..