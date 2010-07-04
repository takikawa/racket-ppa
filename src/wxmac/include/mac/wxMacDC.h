///////////////////////////////////////////////////////////////////////////////
// File:	wxMacDC.h
// Purpose:	MacDC (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004-2009 PLT Scheme Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#ifndef wxMacDCh
#define wxMacDCh

class wxMacDC
#ifndef MZ_PRECISE_GC
: public gc
#endif
{
  protected:
	CGrafPtr	cMacGrafPort;
	wxObject*	cCurrentUser;
	CGContextRef    cgcref;

  public:
	wxMacDC(CGrafPtr port);		// constructor
	~wxMacDC(void);				// destructor

	CGrafPtr macGrafPort(void);
	wxObject* currentUser(void);
	void setCurrentUser(wxObject* user);

	Bool isCurrentPort(void);

	CGContextRef GetCG(Bool only_if_already = FALSE);
	void EndCG();
};

#endif // wxMacDCh
