#include "racket/sconfig.h"
/* Starting in Racket v7.5, SCHEME_PLATFORM_LIBRARY_SUBPATH may have spaces
 * in the string, so use a substitution to remove it */
RACKET_ARCH := $(subst " ",,SCHEME_PLATFORM_LIBRARY_SUBPATH)
