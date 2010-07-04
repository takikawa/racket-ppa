/*
 * File:	imagif.h
 * Purpose:	Declaration of the Platform Independent GIF Image Class
 * Author:	Alejandro Aguilar Sierra
 * Created:	1995
 * Copyright:	(c) 2004-2010 PLT Scheme Inc.
 * Copyright:	(c) 1995, Alejandro Aguilar Sierra <asierra@servidor.unam.mx>
 */
#if !defined(__wximgfil_h)
#define __wximgfil_h
#include "wx_gdi.h"
#include <stdio.h>


/* Various error codes used by decoder
 */
#define OUT_OF_MEMORY 0
#define BAD_CODE_SIZE 0
#define READ_ERROR -1
#define WRITE_ERROR -2
#define OPEN_ERROR -3
#define CREATE_ERROR -4

#define MAX_CODES   4095

#ifndef uchar
typedef unsigned char uchar;
#endif

#ifndef ushort
typedef unsigned short ushort;
#endif

#ifndef ulong
typedef unsigned long ulong;
#endif

#ifndef NULL
#define NULL 0L
#endif

typedef unsigned char byte;
    
#ifndef BOOL
typedef int BOOL;
#endif

#ifndef FALSE
#define FALSE 0
#endif

#ifndef TRUE
#define TRUE 1
#endif

//#define GIFBUFTAM 16384
#define GIFBUFTAM 4096

struct rgb { byte  r,g,b; };
struct rgbq { byte  r,g,b,rv; };

class wxGIF
#ifndef MZ_PRECISE_GC
: public gc
#endif
{
private:
  void InitInterlaceRow(int);
public:
  wxGIF();
  wxGIF(char * path);
  ~wxGIF();
  
  void Create(ushort width, ushort height, ushort deep);

  FILE *fp;
  BOOL flag_subir;
  ushort ibf;
  uchar buf[GIFBUFTAM];
  ushort bad_code_count;

  int transparent_index;

  char * lpbi;  		//  Image data from msw
  Ptr GetRawImage();
  char * RawImage;  		//  Image data
  ushort ItCount;
  long IterImage;  		//  Image data
  ushort Deep;	 				// (bits x pixel)
  ushort Width, Height;    //  Dimensions
  long EfeWidth;	 // Efective Width
  wxColourMap* ColourMap;
// to be moved to wx_gdi.h later
  ushort numcmapentries;
  unsigned short red[256];
  unsigned short green[256];
  unsigned short blue[256];
  
  void Set_VDir(BOOL dir) { flag_subir = dir; }
  BOOL ReadHeader(FILE *);
  BOOL Extrae_imagen();
  wxColourMap *getColorMap();

  ushort GetWidth( void ) { return Width; };
  ushort GetHeight( void ) { return Height; };
  ushort GetDeep( void ) { return Deep; };
// ColorMap settings
  BOOL SetColourMap(wxColourMap* colourmap);
  BOOL SetColourMap(ushort n, byte *r, byte *g, byte *b);
  BOOL SetColourMap(ushort n, ushort array_width, byte **array);
  wxColourMap* GetColourMap() { return ColourMap; }


// Iterators
  Bool ItOK () { return (ItCount < Height); }
  void reset ();
  void upset ();
  Bool ItNext ();
  Bool ItPrev ();
  void SetRow(ushort n, byte *buf);
  void GetRow(ushort n, byte *buf);

protected:
  ushort get_byte();

ushort curr_size;                     /* The current code size */
ushort clear;                         /* Value for a clear code */
ushort ending;                        /* Value for a ending code */
ushort newcodes;                      /* First available code */
ushort top_slot;                      /* Highest code for current size */
ushort slot;                          /* Last read code */

/* The following static variables are used
 * for seperating out codes
 */
ushort navail_bytes;              /* # bytes left in block */
ushort nbits_left;                /* # bits left in current byte */
uchar b1;                           /* Current byte */
uchar byte_buff[257];               /* Current block */
long pbytes;                      /* Pointer to next byte in block */

long code_mask[13];

 uchar stack[MAX_CODES + 1];            /* Stack for storing pixels */
 uchar suffix[MAX_CODES + 1];           /* Suffix table */
 ushort prefix[MAX_CODES + 1];           /* Prefix linked list */

ushort init_exp(ushort);
ushort get_next_code();
ushort decoder(ushort linewidth);
};

Bool wxLoadGifIntoBitmap(char *, wxBitmap *, wxColourMap ** = NULL, int withMask = 0);
Bool wxLoadPICTIntoBitmap(char *, wxBitmap *, wxColourMap ** = NULL);
Bool wxLoadXPMIntoBitmap(char *, wxBitmap *, wxColourMap ** = NULL);
Bool wxLoadXBMIntoBitmap(char *, wxBitmap *, wxColourMap ** = NULL);
Bool wxLoadBMPIntoBitmap(char *, wxBitmap *, wxColourMap ** = NULL);

Bool wxSaveXBMFromBitmap(char *, wxBitmap *, wxColourMap ** = NULL);

/* taken from wxJPEG.cxx */
int read_JPEG_file(char * filename, wxBitmap *bm);

void *XpmMalloc(size_t size);
void *XpmMallocA(size_t size);
void *XpmRealloc(void *p, size_t size);
void *XpmReallocA(void *p, size_t size);
void XpmFree(void *);
void *XpmCallocA(size_t nelem, size_t elsize);

#endif

