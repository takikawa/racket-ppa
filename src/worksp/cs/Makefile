
INCS = /I.. /I..\..\rktio /I..\librktio /I..\..\worksp\cs /I$(SCHEME_DIR)\$(MACHINE)\boot\$(MACHINE)

RKTIO_LIB = ..\..\build\librktio.lib
BASE_WIN32_LIBS = WS2_32.lib Shell32.lib User32.lib Winmm.lib
WIN32_LIBS = $(BASE_WIN32_LIBS) RpCrt4.lib Ole32.lib Advapi32.lib
SCHEME_LIB_FULL = $(SCHEME_DIR)\$(MACHINE)\boot\$(MACHINE)\$(SCHEME_LIB)

LIBS = $(RKTIO_LIB) \
       $(SCHEME_LIB_FULL) \
       $(WIN32_LIBS)

LZ4_DLL = ..\liblz4mt.dll
LZ4_LIB = $(SCHEME_DIR)\$(MACHINE)\lz4\lib\liblz4mt.lib

DEST_DLL = ..\..\build\libracketcsxxx_raw.dll
DEST = ..\..\build\raw_racketcs.exe
GDEST = ..\..\build\raw_gracketcs.exe
CSDIR = ..\..\cs\c

MAIN_DEPS = ..\..\start\config.inc \
            ..\..\start\parse_cmdl.inc \
            ..\..\start\cmdl_to_argv.inc \
            ..\..\start\delayed.inc \
            ..\..\start\win_single.inc \
            ..\..\start\embedded_dll.inc

MEMMOD = ..\..\start\MemoryModule.c

FLAGS = /DWIN32 /Ox /MT /GF
COMP_SUBDIR = /DCS_COMPILED_SUBDIR=1

all: $(DEST) $(GDEST) $(LZ4_DLL)

$(DEST_DLL): $(CSDIR)\boot.c libracket.res $(RKTIO_LIB) $(SCHEME_LIB_FULL)
	cl /LD /DLL /Fe$(DEST_DLL) $(FLAGS) $(INCS) $(CSDIR)\boot.c libracket.res $(LIBS)

libracket.res: libracket.rc
	rc /l 0x409 /folibracket.res libracket.rc

$(DEST): $(CSDIR)\main.c $(MAIN_DEPS) $(MEMMOD) $(DEST_DLL) racket.res
	cl /Fe$(DEST) $(COMP_SUBDIR) $(FLAGS) $(INCS) $(CSDIR)\main.c $(MEMMOD) racket.res $(WIN32_LIBS)

racket.res: ../racket/racket.rc ../racket/racket.ico
	rc /l 0x409 /foracket.res ../racket/racket.rc

$(GDEST): $(CSDIR)\grmain.c $(CSDIR)\main.c $(MAIN_DEPS) $(DEST_DLL) gracket.res
	cl /Fe$(GDEST) $(COMP_SUBDIR) $(FLAGS) $(INCS) $(CSDIR)\grmain.c $(MEMMOD) gracket.res $(WIN32_LIBS)

gracket.res: ../gracket/gracket.rc ../gracket/gracket.ico
	rc /l 0x409 /fogracket.res ../gracket/gracket.rc

# Useful for adjusting boot file compression:

$(LZ4_DLL): lz4.c lz4.def $(LZ4_LIB)
	cl /LD /DLL /Fe$(LZ4_DLL) $(FLAGS) lz4.c $(LZ4_LIB) $(WIN32_LIBS) /link /DEF:lz4.def 

# Useful for debugging:

RKTIO_DLL = ..\..\lib\librktio.dll

rktio-dll:
	cl /Fe$(RKTIO_DLL) /LD $(RKTIO_LIB) $(BASE_WIN32_LIBS) ..\..\rktio\rktio.def /link /machine:x64
