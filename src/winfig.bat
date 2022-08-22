@echo off
setlocal

set SRCDIR=%~dp0
set BUILDMODE=cs
set USE_SUFFIX=

:argloop
shift
set ARG=%0
if defined ARG (
  if "%ARG%"=="/both" set BUILDMODE=both && goto argloop
  if "%ARG%"=="/csonly" set BUILDMODE=cs && goto argloop
  if "%ARG%"=="/bconly" set BUILDMODE=bc && goto argloop
  if "%ARG%"=="/suffix" set USE_SUFFIX=%1 && shift && goto argloop
  echo Unrecognized argument %ARG%
  exit /B 1
)

set use_cs=yes
set use_bc=yes
if %BUILDMODE%==cs set use_bc=no
if %BUILDMODE%==bc set use_cs=no

set default_vm=%BUILDMODE%
if %BUILDMODE%==both set default_vm=cs

echo srcdir=%SRCDIR% > Makefile
echo use_cs=%use_cs% >> Makefile
echo use_bc=%use_bc% >> Makefile
echo default_vm=%default_vm% >> Makefile

if %BUILDMODE%==bc echo MMM_CAP_INSTALLED=%USE_SUFFIX% >> Makefile
if %BUILDMODE%==cs echo CS_CAP_INSTALLED=%USE_SUFFIX% >> Makefile

type "%SRCDIR%\Makefile.nt" >> Makefile

copy /y "%SRCDIR%\buildmain.zuo" main.zuo > NUL

goto donebuilding

:needargument

echo Please supply the machine name as an argument
exit /B 1

:donebuilding
