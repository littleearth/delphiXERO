@echo off
cls
echo.
echo This will remove all build files
echo and Delphi history and temporary files.
echo Press CTRL+C now to abort, otherwise,
pause


echo Removing History Files...
SET EXT=~*
del *.%EXT% /S /Q /F >nul 2>&1

echo Removing dcu Files...
SET EXT=dcu
del *.%EXT% /S /Q /F >nul 2>&1

echo Removing bak Files...
SET EXT=bak
del *.%EXT% /S /Q /F >nul 2>&1

echo Removing identcache Files...
SET EXT=dentcache
del *.%EXT% /S /Q /F >nul 2>&1

echo Removing ddp Files...
SET EXT=ddp
del *.%EXT% /S /Q /F >nul 2>&1

echo Removing dsk Files...
SET EXT=dsk
del *.%EXT% /S /Q /F >nul 2>&1

echo Removing todo Files...
SET EXT=todo
del *.%EXT% /S /Q /F >nul 2>&1

echo Removing tvsconfig Files...
SET EXT=tvsconfig
del *.%EXT% /S /Q /F >nul 2>&1

echo Removing tvsconfig Files...
SET EXT=tmp
del *.%EXT% /S /Q /F >nul 2>&1

echo Removing local Files...
SET EXT=local
del *.%EXT% /S /Q /F >nul 2>&1

echo Removing log Files...
SET EXT=log
del *.%EXT% /S /Q /F >nul 2>&1

echo Removing err Files...
SET EXT=err
del *.%EXT% /S /Q /F >nul 2>&1

echo Removing identcache Files...
SET EXT=identcache
del *.%EXT% /S /Q /F >nul 2>&1

pause