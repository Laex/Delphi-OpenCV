@echo off

del *.~*
del *.dcu
del descript.ion
del *.ddp
del *.map
del *.identcache 
del *.local
del *.drc
del /S /Q Win32\Release\*.dcu
del /S /Q Win64\Release\*.dcu
del /S /Q Win32\Release\*.map
del /S /Q Win64\Release\*.map
del /S /Q Win32\Release\*.drc
del /S /Q Win64\Release\*.drc
del /S /Q Win32\Release\*.bak
del /S /Q Win64\Release\*.bak
del /S /Q Win32\Release\*.rsm
del /S /Q Win64\Release\*.rsm
rd /S /Q __history
