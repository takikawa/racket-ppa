#lang racket
(require "intset.rkt")
(provide (all-defined-out))

;; Reference:
;; - https://tools.ietf.org/html/rfc3454

;; ----------------------------------------
;; Unassigned Code Points Table

;; A.1 Unassigned code points in Unicode 3.2
(define unassigned-in-unicode-3.2
  (intset
   #x0221
   [#x0234 #x024F]
   [#x02AE #x02AF]
   [#x02EF #x02FF]
   [#x0350 #x035F]
   [#x0370 #x0373]
   [#x0376 #x0379]
   [#x037B #x037D]
   [#x037F #x0383]
   #x038B
   #x038D
   #x03A2
   #x03CF
   [#x03F7 #x03FF]
   #x0487
   #x04CF
   [#x04F6 #x04F7]
   [#x04FA #x04FF]
   [#x0510 #x0530]
   [#x0557 #x0558]
   #x0560
   #x0588
   [#x058B #x0590]
   #x05A2
   #x05BA
   [#x05C5 #x05CF]
   [#x05EB #x05EF]
   [#x05F5 #x060B]
   [#x060D #x061A]
   [#x061C #x061E]
   #x0620
   [#x063B #x063F]
   [#x0656 #x065F]
   [#x06EE #x06EF]
   #x06FF
   #x070E
   [#x072D #x072F]
   [#x074B #x077F]
   [#x07B2 #x0900]
   #x0904
   [#x093A #x093B]
   [#x094E #x094F]
   [#x0955 #x0957]
   [#x0971 #x0980]
   #x0984
   [#x098D #x098E]
   [#x0991 #x0992]
   #x09A9
   #x09B1
   [#x09B3 #x09B5]
   [#x09BA #x09BB]
   #x09BD
   [#x09C5 #x09C6]
   [#x09C9 #x09CA]
   [#x09CE #x09D6]
   [#x09D8 #x09DB]
   #x09DE
   [#x09E4 #x09E5]
   [#x09FB #x0A01]
   [#x0A03 #x0A04]
   [#x0A0B #x0A0E]
   [#x0A11 #x0A12]
   #x0A29
   #x0A31
   #x0A34
   #x0A37
   [#x0A3A #x0A3B]
   #x0A3D
   [#x0A43 #x0A46]
   [#x0A49 #x0A4A]
   [#x0A4E #x0A58]
   #x0A5D
   [#x0A5F #x0A65]
   [#x0A75 #x0A80]
   #x0A84
   #x0A8C
   #x0A8E
   #x0A92
   #x0AA9
   #x0AB1
   #x0AB4
   [#x0ABA #x0ABB]
   #x0AC6
   #x0ACA
   [#x0ACE #x0ACF]
   [#x0AD1 #x0ADF]
   [#x0AE1 #x0AE5]
   [#x0AF0 #x0B00]
   #x0B04
   [#x0B0D #x0B0E]
   [#x0B11 #x0B12]
   #x0B29
   #x0B31
   [#x0B34 #x0B35]
   [#x0B3A #x0B3B]
   [#x0B44 #x0B46]
   [#x0B49 #x0B4A]
   [#x0B4E #x0B55]
   [#x0B58 #x0B5B]
   #x0B5E
   [#x0B62 #x0B65]
   [#x0B71 #x0B81]
   #x0B84
   [#x0B8B #x0B8D]
   #x0B91
   [#x0B96 #x0B98]
   #x0B9B
   #x0B9D
   [#x0BA0 #x0BA2]
   [#x0BA5 #x0BA7]
   [#x0BAB #x0BAD]
   #x0BB6
   [#x0BBA #x0BBD]
   [#x0BC3 #x0BC5]
   #x0BC9
   [#x0BCE #x0BD6]
   [#x0BD8 #x0BE6]
   [#x0BF3 #x0C00]
   #x0C04
   #x0C0D
   #x0C11
   #x0C29
   #x0C34
   [#x0C3A #x0C3D]
   #x0C45
   #x0C49
   [#x0C4E #x0C54]
   [#x0C57 #x0C5F]
   [#x0C62 #x0C65]
   [#x0C70 #x0C81]
   #x0C84
   #x0C8D
   #x0C91
   #x0CA9
   #x0CB4
   [#x0CBA #x0CBD]
   #x0CC5
   #x0CC9
   [#x0CCE #x0CD4]
   #x0CD7 #x0CDD
   #x0CDF
   [#x0CE2 #x0CE5]
   [#x0CF0 #x0D01]
   #x0D04
   #x0D0D
   #x0D11
   #x0D29
   [#x0D3A #x0D3D]
   [#x0D44 #x0D45]
   #x0D49
   [#x0D4E #x0D56]
   [#x0D58 #x0D5F]
   [#x0D62 #x0D65]
   [#x0D70 #x0D81]
   #x0D84
   [#x0D97 #x0D99]
   #x0DB2
   #x0DBC
   [#x0DBE #x0DBF]
   [#x0DC7 #x0DC9]
   [#x0DCB #x0DCE]
   #x0DD5
   #x0DD7
   [#x0DE0 #x0DF1]
   [#x0DF5 #x0E00]
   [#x0E3B #x0E3E]
   [#x0E5C #x0E80]
   #x0E83
   [#x0E85 #x0E86]
   #x0E89
   [#x0E8B #x0E8C]
   [#x0E8E #x0E93]
   #x0E98
   #x0EA0
   #x0EA4
   #x0EA6
   [#x0EA8 #x0EA9]
   #x0EAC
   #x0EBA
   [#x0EBE #x0EBF]
   #x0EC5
   #x0EC7
   [#x0ECE #x0ECF]
   [#x0EDA #x0EDB]
   [#x0EDE #x0EFF]
   #x0F48
   [#x0F6B #x0F70]
   [#x0F8C #x0F8F]
   #x0F98
   #x0FBD
   [#x0FCD #x0FCE]
   [#x0FD0 #x0FFF]
   #x1022
   #x1028
   #x102B
   [#x1033 #x1035]
   [#x103A #x103F]
   [#x105A #x109F]
   [#x10C6 #x10CF]
   [#x10F9 #x10FA]
   [#x10FC #x10FF]
   [#x115A #x115E]
   [#x11A3 #x11A7]
   [#x11FA #x11FF]
   #x1207
   #x1247
   #x1249
   [#x124E #x124F]
   #x1257
   #x1259
   [#x125E #x125F]
   #x1287
   #x1289
   [#x128E #x128F]
   #x12AF
   #x12B1
   [#x12B6 #x12B7]
   #x12BF
   #x12C1
   [#x12C6 #x12C7]
   #x12CF
   #x12D7
   #x12EF
   #x130F
   #x1311
   [#x1316 #x1317]
   #x131F
   #x1347
   [#x135B #x1360]
   [#x137D #x139F]
   [#x13F5 #x1400]
   [#x1677 #x167F]
   [#x169D #x169F]
   [#x16F1 #x16FF]
   #x170D
   [#x1715 #x171F]
   [#x1737 #x173F]
   [#x1754 #x175F]
   #x176D
   #x1771
   [#x1774 #x177F]
   [#x17DD #x17DF]
   [#x17EA #x17FF]
   #x180F
   [#x181A #x181F]
   [#x1878 #x187F]
   [#x18AA #x1DFF]
   [#x1E9C #x1E9F]
   [#x1EFA #x1EFF]
   [#x1F16 #x1F17]
   [#x1F1E #x1F1F]
   [#x1F46 #x1F47]
   [#x1F4E #x1F4F]
   #x1F58
   #x1F5A
   #x1F5C
   #x1F5E
   [#x1F7E #x1F7F]
   #x1FB5
   #x1FC5
   [#x1FD4 #x1FD5]
   #x1FDC
   [#x1FF0 #x1FF1]
   #x1FF5
   #x1FFF
   [#x2053 #x2056]
   [#x2058 #x205E]
   [#x2064 #x2069]
   [#x2072 #x2073]
   [#x208F #x209F]
   [#x20B2 #x20CF]
   [#x20EB #x20FF]
   [#x213B #x213C]
   [#x214C #x2152]
   [#x2184 #x218F]
   [#x23CF #x23FF]
   [#x2427 #x243F]
   [#x244B #x245F]
   #x24FF
   [#x2614 #x2615]
   #x2618
   [#x267E #x267F]
   [#x268A #x2700]
   #x2705
   [#x270A #x270B]
   #x2728
   #x274C
   #x274E
   [#x2753 #x2755]
   #x2757
   [#x275F #x2760]
   [#x2795 #x2797]
   #x27B0
   [#x27BF #x27CF]
   [#x27EC #x27EF]
   [#x2B00 #x2E7F]
   #x2E9A
   [#x2EF4 #x2EFF]
   [#x2FD6 #x2FEF]
   [#x2FFC #x2FFF]
   #x3040
   [#x3097 #x3098]
   [#x3100 #x3104]
   [#x312D #x3130]
   #x318F
   [#x31B8 #x31EF]
   [#x321D #x321F]
   [#x3244 #x3250]
   [#x327C #x327E]
   [#x32CC #x32CF]
   #x32FF
   [#x3377 #x337A]
   [#x33DE #x33DF]
   #x33FF
   [#x4DB6 #x4DFF]
   [#x9FA6 #x9FFF]
   [#xA48D #xA48F]
   [#xA4C7 #xABFF]
   [#xD7A4 #xD7FF]
   [#xFA2E #xFA2F]
   [#xFA6B #xFAFF]
   [#xFB07 #xFB12]
   [#xFB18 #xFB1C]
   #xFB37
   #xFB3D
   #xFB3F
   #xFB42
   #xFB45
   [#xFBB2 #xFBD2]
   [#xFD40 #xFD4F]
   [#xFD90 #xFD91]
   [#xFDC8 #xFDCF]
   [#xFDFD #xFDFF]
   [#xFE10 #xFE1F]
   [#xFE24 #xFE2F]
   [#xFE47 #xFE48]
   #xFE53
   #xFE67
   [#xFE6C #xFE6F]
   #xFE75
   [#xFEFD #xFEFE]
   #xFF00
   [#xFFBF #xFFC1]
   [#xFFC8 #xFFC9]
   [#xFFD0 #xFFD1]
   [#xFFD8 #xFFD9]
   [#xFFDD #xFFDF]
   #xFFE7
   [#xFFEF #xFFF8]
   [#x10000 #x102FF]
   #x1031F
   [#x10324 #x1032F]
   [#x1034B #x103FF]
   [#x10426 #x10427]
   [#x1044E #x1CFFF]
   [#x1D0F6 #x1D0FF]
   [#x1D127 #x1D129]
   [#x1D1DE #x1D3FF]
   #x1D455
   #x1D49D
   [#x1D4A0 #x1D4A1]
   [#x1D4A3 #x1D4A4]
   [#x1D4A7 #x1D4A8]
   #x1D4AD
   #x1D4BA
   #x1D4BC
   #x1D4C1
   #x1D4C4
   #x1D506
   [#x1D50B #x1D50C]
   #x1D515
   #x1D51D
   #x1D53A
   #x1D53F
   #x1D545
   [#x1D547 #x1D549]
   #x1D551
   [#x1D6A4 #x1D6A7]
   [#x1D7CA #x1D7CD]
   [#x1D800 #x1FFFD]
   [#x2A6D7 #x2F7FF]
   [#x2FA1E #x2FFFD]
   [#x30000 #x3FFFD]
   [#x40000 #x4FFFD]
   [#x50000 #x5FFFD]
   [#x60000 #x6FFFD]
   [#x70000 #x7FFFD]
   [#x80000 #x8FFFD]
   [#x90000 #x9FFFD]
   [#xA0000 #xAFFFD]
   [#xB0000 #xBFFFD]
   [#xC0000 #xCFFFD]
   [#xD0000 #xDFFFD]
   #xE0000
   [#xE0002 #xE001F]
   [#xE0080 #xEFFFD]
   ))

;; ----------------------------------------
;; Mapping Tables

;; B.1 Commonly mapped to nothing
(define commonly-mapped-to-nothing
  (intset #x00AD
          #x034F
          #x1806
          [#x180B #x180D]
          [#x200B #x200D]
          #x2060
          [#xFE00 #xFE0F]
          #xFEFF))

;; ----------------------------------------
;; Prohibition Tables

;; C.1 Space characters

;; C.1.1 ASCII space characters
(define ascii-space-characters
  (intset 
   #x0020; SPACE
   ))

;; C.1.2 Non-ASCII space characters
(define non-ascii-space-characters
  (intset
   #x00A0; NO-BREAK SPACE
   #x1680; OGHAM SPACE MARK
   #x2000; EN QUAD
   #x2001; EM QUAD
   #x2002; EN SPACE
   #x2003; EM SPACE
   #x2004; THREE-PER-EM SPACE
   #x2005; FOUR-PER-EM SPACE
   #x2006; SIX-PER-EM SPACE
   #x2007; FIGURE SPACE
   #x2008; PUNCTUATION SPACE
   #x2009; THIN SPACE
   #x200A; HAIR SPACE
   #x200B; ZERO WIDTH SPACE
   #x202F; NARROW NO-BREAK SPACE
   #x205F; MEDIUM MATHEMATICAL SPACE
   #x3000; IDEOGRAPHIC SPACE
   ))

;; C.2 Control characters

;; C.2.1 ASCII control characters
(define ascii-control-characters
  (intset
   [#x0000 #x001F]; [CONTROL CHARACTERS]
   #x007F; DELETE
   ))

;; C.2.2 Non-ASCII control characters
(define non-ascii-control-characters
  (intset
   [#x0080 #x009F]; [CONTROL CHARACTERS]
   #x06DD; ARABIC END OF AYAH
   #x070F; SYRIAC ABBREVIATION MARK
   #x180E; MONGOLIAN VOWEL SEPARATOR
   #x200C; ZERO WIDTH NON-JOINER
   #x200D; ZERO WIDTH JOINER
   #x2028; LINE SEPARATOR
   #x2029; PARAGRAPH SEPARATOR
   #x2060; WORD JOINER
   #x2061; FUNCTION APPLICATION
   #x2062; INVISIBLE TIMES
   #x2063; INVISIBLE SEPARATOR
   [#x206A #x206F]; [CONTROL CHARACTERS]
   #xFEFF; ZERO WIDTH NO-BREAK SPACE
   [#xFFF9 #xFFFC]; [CONTROL CHARACTERS]
   [#x1D173 #x1D17A]; [MUSICAL CONTROL CHARACTERS]
   ))

;; C.3 Private use
(define private-use
  (intset
   [#xE000 #xF8FF]; [PRIVATE USE, PLANE 0]
   [#xF0000 #xFFFFD]; [PRIVATE USE, PLANE 15]
   [#x100000 #x10FFFD]; [PRIVATE USE, PLANE 16]
   ))

;; C.4 Non-character code points
(define non-character-code-points
  (intset
   [#xFDD0 #xFDEF]; [NONCHARACTER CODE POINTS]
   [#xFFFE #xFFFF]; [NONCHARACTER CODE POINTS]
   [#x1FFFE #x1FFFF]; [NONCHARACTER CODE POINTS]
   [#x2FFFE #x2FFFF]; [NONCHARACTER CODE POINTS]
   [#x3FFFE #x3FFFF]; [NONCHARACTER CODE POINTS]
   [#x4FFFE #x4FFFF]; [NONCHARACTER CODE POINTS]
   [#x5FFFE #x5FFFF]; [NONCHARACTER CODE POINTS]
   [#x6FFFE #x6FFFF]; [NONCHARACTER CODE POINTS]
   [#x7FFFE #x7FFFF]; [NONCHARACTER CODE POINTS]
   [#x8FFFE #x8FFFF]; [NONCHARACTER CODE POINTS]
   [#x9FFFE #x9FFFF]; [NONCHARACTER CODE POINTS]
   [#xAFFFE #xAFFFF]; [NONCHARACTER CODE POINTS]
   [#xBFFFE #xBFFFF]; [NONCHARACTER CODE POINTS]
   [#xCFFFE #xCFFFF]; [NONCHARACTER CODE POINTS]
   [#xDFFFE #xDFFFF]; [NONCHARACTER CODE POINTS]
   [#xEFFFE #xEFFFF]; [NONCHARACTER CODE POINTS]
   [#xFFFFE #xFFFFF]; [NONCHARACTER CODE POINTS]
   [#x10FFFE #x10FFFF]; [NONCHARACTER CODE POINTS]
   ))

;; C.5 Surrogate codes
(define surrogate-codes
  (intset
   [#xD800 #xDFFF]; [SURROGATE CODES]
   ))

;; C.6 Inappropriate for plain text
(define inappropriate-for-plain-text
  (intset
   #xFFF9; INTERLINEAR ANNOTATION ANCHOR
   #xFFFA; INTERLINEAR ANNOTATION SEPARATOR
   #xFFFB; INTERLINEAR ANNOTATION TERMINATOR
   #xFFFC; OBJECT REPLACEMENT CHARACTER
   #xFFFD; REPLACEMENT CHARACTER
   ))

;; C.7 Inappropriate for canonical representation
(define inappropriate-for-canonical-representation
  (intset
   [#x2FF0 #x2FFB]; [IDEOGRAPHIC DESCRIPTION CHARACTERS]
   ))

;; C.8 Change display properties or are deprecated
(define change-display-properties-or-deprecated
  (intset
   #x0340; COMBINING GRAVE TONE MARK
   #x0341; COMBINING ACUTE TONE MARK
   #x200E; LEFT-TO-RIGHT MARK
   #x200F; RIGHT-TO-LEFT MARK
   #x202A; LEFT-TO-RIGHT EMBEDDING
   #x202B; RIGHT-TO-LEFT EMBEDDING
   #x202C; POP DIRECTIONAL FORMATTING
   #x202D; LEFT-TO-RIGHT OVERRIDE
   #x202E; RIGHT-TO-LEFT OVERRIDE
   #x206A; INHIBIT SYMMETRIC SWAPPING
   #x206B; ACTIVATE SYMMETRIC SWAPPING
   #x206C; INHIBIT ARABIC FORM SHAPING
   #x206D; ACTIVATE ARABIC FORM SHAPING
   #x206E; NATIONAL DIGIT SHAPES
   #x206F; NOMINAL DIGIT SHAPES
   ))

;; C.9 Tagging characters
(define tagging-characters
  (intset
   #xE0001; LANGUAGE TAG
   [#xE0020 #xE007F]; [TAGGING CHARACTERS]
   ))

;; ----------------------------------------
;; Bidi Tables

;; D. Bidirectional tables

;; D.1 Characters with bidirectional property "R" or "AL"
(define RandALCat-characters
  (intset
   #x05BE
   #x05C0
   #x05C3
   [#x05D0 #x05EA]
   [#x05F0 #x05F4]
   #x061B
   #x061F
   [#x0621 #x063A]
   [#x0640 #x064A]
   [#x066D #x066F]
   [#x0671 #x06D5]
   #x06DD
   [#x06E5 #x06E6]
   [#x06FA #x06FE]
   [#x0700 #x070D]
   #x0710
   [#x0712 #x072C]
   [#x0780 #x07A5]
   #x07B1
   #x200F
   #xFB1D
   [#xFB1F #xFB28]
   [#xFB2A #xFB36]
   [#xFB38 #xFB3C]
   #xFB3E
   [#xFB40 #xFB41]
   [#xFB43 #xFB44]
   [#xFB46 #xFBB1]
   [#xFBD3 #xFD3D]
   [#xFD50 #xFD8F]
   [#xFD92 #xFDC7]
   [#xFDF0 #xFDFC]
   [#xFE70 #xFE74]
   [#xFE76 #xFEFC]
   ))

;; D.2 Characters with bidirectional property "L"
(define LCat-characters
  (intset
   [#x0041 #x005A]
   [#x0061 #x007A]
   #x00AA
   #x00B5
   #x00BA
   [#x00C0 #x00D6]
   [#x00D8 #x00F6]
   [#x00F8 #x0220]
   [#x0222 #x0233]
   [#x0250 #x02AD]
   [#x02B0 #x02B8]
   [#x02BB #x02C1]
   [#x02D0 #x02D1]
   [#x02E0 #x02E4]
   #x02EE
   #x037A
   #x0386
   [#x0388 #x038A]
   #x038C
   [#x038E #x03A1]
   [#x03A3 #x03CE]
   [#x03D0 #x03F5]
   [#x0400 #x0482]
   [#x048A #x04CE]
   [#x04D0 #x04F5]
   [#x04F8 #x04F9]
   [#x0500 #x050F]
   [#x0531 #x0556]
   [#x0559 #x055F]
   [#x0561 #x0587]
   #x0589
   #x0903
   [#x0905 #x0939]
   [#x093D #x0940]
   [#x0949 #x094C]
   #x0950
   [#x0958 #x0961]
   [#x0964 #x0970]
   [#x0982 #x0983]
   [#x0985 #x098C]
   [#x098F #x0990]
   [#x0993 #x09A8]
   [#x09AA #x09B0]
   #x09B2
   [#x09B6 #x09B9]
   [#x09BE #x09C0]
   [#x09C7 #x09C8]
   [#x09CB #x09CC]
   #x09D7
   [#x09DC #x09DD]
   [#x09DF #x09E1]
   [#x09E6 #x09F1]
   [#x09F4 #x09FA]
   [#x0A05 #x0A0A]
   [#x0A0F #x0A10]
   [#x0A13 #x0A28]
   [#x0A2A #x0A30]
   [#x0A32 #x0A33]
   [#x0A35 #x0A36]
   [#x0A38 #x0A39]
   [#x0A3E #x0A40]
   [#x0A59 #x0A5C]
   #x0A5E
   [#x0A66 #x0A6F]
   [#x0A72 #x0A74]
   #x0A83
   [#x0A85 #x0A8B]
   #x0A8D
   [#x0A8F #x0A91]
   [#x0A93 #x0AA8]
   [#x0AAA #x0AB0]
   [#x0AB2 #x0AB3]
   [#x0AB5 #x0AB9]
   [#x0ABD #x0AC0]
   #x0AC9
   [#x0ACB #x0ACC]
   #x0AD0
   #x0AE0
   [#x0AE6 #x0AEF]
   [#x0B02 #x0B03]
   [#x0B05 #x0B0C]
   [#x0B0F #x0B10]
   [#x0B13 #x0B28]
   [#x0B2A #x0B30]
   [#x0B32 #x0B33]
   [#x0B36 #x0B39]
   [#x0B3D #x0B3E]
   #x0B40
   [#x0B47 #x0B48]
   [#x0B4B #x0B4C]
   #x0B57
   [#x0B5C #x0B5D]
   [#x0B5F #x0B61]
   [#x0B66 #x0B70]
   #x0B83
   [#x0B85 #x0B8A]
   [#x0B8E #x0B90]
   [#x0B92 #x0B95]
   [#x0B99 #x0B9A]
   #x0B9C
   [#x0B9E #x0B9F]
   [#x0BA3 #x0BA4]
   [#x0BA8 #x0BAA]
   [#x0BAE #x0BB5]
   [#x0BB7 #x0BB9]
   [#x0BBE #x0BBF]
   [#x0BC1 #x0BC2]
   [#x0BC6 #x0BC8]
   [#x0BCA #x0BCC]
   #x0BD7
   [#x0BE7 #x0BF2]
   [#x0C01 #x0C03]
   [#x0C05 #x0C0C]
   [#x0C0E #x0C10]
   [#x0C12 #x0C28]
   [#x0C2A #x0C33]
   [#x0C35 #x0C39]
   [#x0C41 #x0C44]
   [#x0C60 #x0C61]
   [#x0C66 #x0C6F]
   [#x0C82 #x0C83]
   [#x0C85 #x0C8C]
   [#x0C8E #x0C90]
   [#x0C92 #x0CA8]
   [#x0CAA #x0CB3]
   [#x0CB5 #x0CB9]
   #x0CBE
   [#x0CC0 #x0CC4]
   [#x0CC7 #x0CC8]
   [#x0CCA #x0CCB]
   [#x0CD5 #x0CD6]
   #x0CDE
   [#x0CE0 #x0CE1]
   [#x0CE6 #x0CEF]
   [#x0D02 #x0D03]
   [#x0D05 #x0D0C]
   [#x0D0E #x0D10]
   [#x0D12 #x0D28]
   [#x0D2A #x0D39]
   [#x0D3E #x0D40]
   [#x0D46 #x0D48]
   [#x0D4A #x0D4C]
   #x0D57
   [#x0D60 #x0D61]
   [#x0D66 #x0D6F]
   [#x0D82 #x0D83]
   [#x0D85 #x0D96]
   [#x0D9A #x0DB1]
   [#x0DB3 #x0DBB]
   #x0DBD
   [#x0DC0 #x0DC6]
   [#x0DCF #x0DD1]
   [#x0DD8 #x0DDF]
   [#x0DF2 #x0DF4]
   [#x0E01 #x0E30]
   [#x0E32 #x0E33]
   [#x0E40 #x0E46]
   [#x0E4F #x0E5B]
   [#x0E81 #x0E82]
   #x0E84
   [#x0E87 #x0E88]
   #x0E8A
   #x0E8D
   [#x0E94 #x0E97]
   [#x0E99 #x0E9F]
   [#x0EA1 #x0EA3]
   #x0EA5
   #x0EA7
   [#x0EAA #x0EAB]
   [#x0EAD #x0EB0]
   [#x0EB2 #x0EB3]
   #x0EBD
   [#x0EC0 #x0EC4]
   #x0EC6
   [#x0ED0 #x0ED9]
   [#x0EDC #x0EDD]
   [#x0F00 #x0F17]
   [#x0F1A #x0F34]
   #x0F36
   #x0F38
   [#x0F3E #x0F47]
   [#x0F49 #x0F6A]
   #x0F7F
   #x0F85
   [#x0F88 #x0F8B]
   [#x0FBE #x0FC5]
   [#x0FC7 #x0FCC]
   #x0FCF
   [#x1000 #x1021]
   [#x1023 #x1027]
   [#x1029 #x102A]
   #x102C
   #x1031
   #x1038
   [#x1040 #x1057]
   [#x10A0 #x10C5]
   [#x10D0 #x10F8]
   #x10FB
   [#x1100 #x1159]
   [#x115F #x11A2]
   [#x11A8 #x11F9]
   [#x1200 #x1206]
   [#x1208 #x1246]
   #x1248
   [#x124A #x124D]
   [#x1250 #x1256]
   #x1258
   [#x125A #x125D]
   [#x1260 #x1286]
   #x1288
   [#x128A #x128D]
   [#x1290 #x12AE]
   #x12B0
   [#x12B2 #x12B5]
   [#x12B8 #x12BE]
   #x12C0
   [#x12C2 #x12C5]
   [#x12C8 #x12CE]
   [#x12D0 #x12D6]
   [#x12D8 #x12EE]
   [#x12F0 #x130E]
   #x1310
   [#x1312 #x1315]
   [#x1318 #x131E]
   [#x1320 #x1346]
   [#x1348 #x135A]
   [#x1361 #x137C]
   [#x13A0 #x13F4]
   [#x1401 #x1676]
   [#x1681 #x169A]
   [#x16A0 #x16F0]
   [#x1700 #x170C]
   [#x170E #x1711]
   [#x1720 #x1731]
   [#x1735 #x1736]
   [#x1740 #x1751]
   [#x1760 #x176C]
   [#x176E #x1770]
   [#x1780 #x17B6]
   [#x17BE #x17C5]
   [#x17C7 #x17C8]
   [#x17D4 #x17DA]
   #x17DC
   [#x17E0 #x17E9]
   [#x1810 #x1819]
   [#x1820 #x1877]
   [#x1880 #x18A8]
   [#x1E00 #x1E9B]
   [#x1EA0 #x1EF9]
   [#x1F00 #x1F15]
   [#x1F18 #x1F1D]
   [#x1F20 #x1F45]
   [#x1F48 #x1F4D]
   [#x1F50 #x1F57]
   #x1F59
   #x1F5B
   #x1F5D
   [#x1F5F #x1F7D]
   [#x1F80 #x1FB4]
   [#x1FB6 #x1FBC]
   #x1FBE
   [#x1FC2 #x1FC4]
   [#x1FC6 #x1FCC]
   [#x1FD0 #x1FD3]
   [#x1FD6 #x1FDB]
   [#x1FE0 #x1FEC]
   [#x1FF2 #x1FF4]
   [#x1FF6 #x1FFC]
   #x200E
   #x2071
   #x207F
   #x2102
   #x2107
   [#x210A #x2113]
   #x2115
   [#x2119 #x211D]
   #x2124
   #x2126
   #x2128
   [#x212A #x212D]
   [#x212F #x2131]
   [#x2133 #x2139]
   [#x213D #x213F]
   [#x2145 #x2149]
   [#x2160 #x2183]
   [#x2336 #x237A]
   #x2395
   [#x249C #x24E9]
   [#x3005 #x3007]
   [#x3021 #x3029]
   [#x3031 #x3035]
   [#x3038 #x303C]
   [#x3041 #x3096]
   [#x309D #x309F]
   [#x30A1 #x30FA]
   [#x30FC #x30FF]
   [#x3105 #x312C]
   [#x3131 #x318E]
   [#x3190 #x31B7]
   [#x31F0 #x321C]
   [#x3220 #x3243]
   [#x3260 #x327B]
   [#x327F #x32B0]
   [#x32C0 #x32CB]
   [#x32D0 #x32FE]
   [#x3300 #x3376]
   [#x337B #x33DD]
   [#x33E0 #x33FE]
   [#x3400 #x4DB5]
   [#x4E00 #x9FA5]
   [#xA000 #xA48C]
   [#xAC00 #xD7A3]
   [#xD800 #xFA2D]
   [#xFA30 #xFA6A]
   [#xFB00 #xFB06]
   [#xFB13 #xFB17]
   [#xFF21 #xFF3A]
   [#xFF41 #xFF5A]
   [#xFF66 #xFFBE]
   [#xFFC2 #xFFC7]
   [#xFFCA #xFFCF]
   [#xFFD2 #xFFD7]
   [#xFFDA #xFFDC]
   [#x10300 #x1031E]
   [#x10320 #x10323]
   [#x10330 #x1034A]
   [#x10400 #x10425]
   [#x10428 #x1044D]
   [#x1D000 #x1D0F5]
   [#x1D100 #x1D126]
   [#x1D12A #x1D166]
   [#x1D16A #x1D172]
   [#x1D183 #x1D184]
   [#x1D18C #x1D1A9]
   [#x1D1AE #x1D1DD]
   [#x1D400 #x1D454]
   [#x1D456 #x1D49C]
   [#x1D49E #x1D49F]
   #x1D4A2
   [#x1D4A5 #x1D4A6]
   [#x1D4A9 #x1D4AC]
   [#x1D4AE #x1D4B9]
   #x1D4BB
   [#x1D4BD #x1D4C0]
   [#x1D4C2 #x1D4C3]
   [#x1D4C5 #x1D505]
   [#x1D507 #x1D50A]
   [#x1D50D #x1D514]
   [#x1D516 #x1D51C]
   [#x1D51E #x1D539]
   [#x1D53B #x1D53E]
   [#x1D540 #x1D544]
   #x1D546
   [#x1D54A #x1D550]
   [#x1D552 #x1D6A3]
   [#x1D6A8 #x1D7C9]
   [#x20000 #x2A6D6]
   [#x2F800 #x2FA1D]
   [#xF0000 #xFFFFD]
   [#x100000 #x10FFFD]
   ))
