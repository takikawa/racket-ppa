@echo off
setlocal

set M=%1
set WORKAREA=%M%
set LINKAS=dll
set RUNTIMEAS=dll
set SRCDIR=%~dp0
set MAKETARGET=all-dlls

if "%WORKAREA%"=="" goto needargument

:argloop
shift
set ARG=%1
if defined ARG (
  if "%ARG%"=="/dll" set LINKAS=dll && goto argloop
  if "%ARG%"=="/exe" set LINKAS=exe && goto argloop
  if "%ARG%"=="/MD" set RUNTIMEAS=dll && goto argloop
  if "%ARG%"=="/MT" set RUNTIMEAS=static && goto argloop
  if "%ARG%"=="/only" set MAKETARGET=build && goto argloop
  if "%ARG%"=="/kernel" set MAKETARGET=kernel && goto argloop
  if "%ARG%"=="/none" set MAKETARGET=none && goto argloop
  echo Unrecognized argument %ARG%
  exit /B 1
)

if "%M%"=="pb" set VSCONFIG=x86
if "%M%"=="i3nt" set VSCONFIG=x86
if "%M%"=="ti3nt" set VSCONFIG=x86
if "%M%"=="a6nt" set VSCONFIG=x86_amd64
if "%M%"=="ta6nt" set VSCONFIG=x86_amd64
if "%M%"=="arm64nt" set VSCONFIG=x64_arm64
if "%M%"=="tarm64nt" set VSCONFIG=x64_arm64
if "%VSCONFIG%"=="" (
  echo Unrecognized machine type %M%
  exit /B 1
)

if not exist %WORKAREA% mkdir %WORKAREA%

echo srcdir=%SRCDIR% > %WORKAREA%\Mf-config
echo m=%M% >> %WORKAREA%\Mf-config
echo linkAs=%LINKAS% >> %WORKAREA%\Mf-config
echo runtimeAs=%RUNTIMEAS% >> %WORKAREA%\Mf-config

echo workarea=%WORKAREA% > Makefile
echo !include %WORKAREA%\Mf-config >> Makefile
type "%SRCDIR%\makefiles\Makefile.nt" >> Makefile

copy /y "%SRCDIR%\makefiles\buildmain.zuo" main.zuo > NUL
copy /y "%SRCDIR%\makefiles\workmain.zuo" %WORKAREA%\main.zuo > NUL

echo Configured for %M%

if %MAKETARGET%==none goto donebuilding

for %%X in (cl.exe) do (set FOUND=%%~$PATH:X)
if not defined FOUND (
  echo Configuring VS for %VSCONFIG%
  call "%SRCDIR%/c/vs.bat" %VSCONFIG%
 )

nmake /nologo %MAKETARGET%

goto donebuilding

:needargument

echo Please supply the machine name as an argument
exit /B 1

:donebuilding
