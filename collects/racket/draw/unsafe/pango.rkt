#lang scheme/base
(require ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/alloc
         ffi/unsafe/atomic
         setup/dirs
         "glib.rkt"
         "cairo.rkt"
         "../private/utils.rkt"
         "../private/libs.rkt")

(define-runtime-lib pango-lib 
  [(unix) (ffi-lib "libpango-1.0" '("0" ""))]
  [(macosx) 
   (ffi-lib "libintl.8.dylib")
   (ffi-lib "libpango-1.0.0.dylib")]
  [(windows) 
   (ffi-lib "libpango-1.0-0.dll")])

(define-runtime-lib pangowin32-lib 
  [(unix) #f]
  [(macosx)]
  [(windows) 
   (ffi-lib "libpangowin32-1.0-0.dll")])

(define-runtime-lib pangocairo-lib 
  [(unix) (ffi-lib "libpangocairo-1.0" '("0" ""))]
  [(macosx)
   (ffi-lib "libpangocairo-1.0.0.dylib")]
  [(win32) 
   (ffi-lib "libpangowin32-1.0-0.dll")
   (ffi-lib "libexpat-1.dll")
   (ffi-lib "freetype6.dll")
   (ffi-lib "libfontconfig-1.dll")
   (ffi-lib "libpangoft2-1.0-0.dll")
   (ffi-lib "libpangocairo-1.0-0.dll")]
  [(win64) 
   (ffi-lib "libintl-8.dll")
   (ffi-lib "libpangowin32-1.0-0.dll")
   (ffi-lib "libexpat-1.dll")
   (ffi-lib "libfreetype-6.dll")
   (ffi-lib "libfontconfig-1.dll")
   (ffi-lib "libpangoft2-1.0-0.dll")
   (ffi-lib "libpangocairo-1.0-0.dll")])

(define-ffi-definer define-pango pango-lib
  #:provide provide)
(define-ffi-definer define-pangocairo pangocairo-lib
  #:provide provide)
(define-ffi-definer define-pangowin32 pangowin32-lib
  #:provide provide)

(define PangoContext (_cpointer 'PangoContext))
(define PangoLayout (_cpointer 'PangoLayout))
(define PangoFontDescription (_cpointer 'PangoFontDescription))
(define PangoFontFamily (_cpointer 'PangoFontFamily))
(define PangoFont (_cpointer 'PangoFont))
(define PangoFontMap (_cpointer 'PangoFontMap))
(define PangoFontMetrics (_cpointer 'PangoFontMetrics))
(define PangoAttrList (_cpointer 'PangoAttrList))
(define PangoAttribute (_cpointer 'PangoAttribute))
(define PangoLanguage (_cpointer 'PangoLanguage))
(define PangoCoverage (_cpointer 'PangoCoverage))
(define PangoLayoutIter (_cpointer 'PangoLayoutIter))
(define PangoLayoutLine (_cpointer 'PangoLayoutLine))

(define-cstruct _PangoRectangle ([x _int]
                                 [y _int]
                                 [width _int]
                                 [height _int]))
(provide make-PangoRectangle
         PangoRectangle-x
         PangoRectangle-y
         PangoRectangle-width
         PangoRectangle-height)

(define-cstruct _PangoItem
  ([offset _int]
   [length _int]
   [num_chars _int]
   ;; Inline PangoAnalysis:
   [shape_engine _pointer]
   [lang_engine _pointer]
   [font PangoFont]
   [level _uint8]
   [gravity _uint8]
   [flags _uint8]
   [script _uint8]
   [language _pointer]
   [extra_attrs _pointer]))

(provide (struct-out PangoItem)
         _PangoItem _PangoItem-pointer)

(define-cstruct _PangoGlyphInfo
  ([glyph _uint32]
   [width _uint32]
   [dx _uint32]
   [dy _uint32]
   [is_cluster_start _uint]))

(provide (struct-out PangoGlyphInfo)
         _PangoGlyphInfo _PangoGlyphInfo-pointer)

(define-cstruct _PangoGlyphString
  ([num_glyphs _int]
   [glyphs _pointer]
   [log_clusters _pointer]))

(provide (struct-out PangoGlyphString)
         _PangoGlyphString)

(define-cstruct _PangoGlyphItem ([item _PangoItem-pointer]
                                 [glyphs _PangoGlyphString-pointer]))
(provide (struct-out PangoGlyphItem))

(provide g_object_unref g_free)
(define-gobj g_object_unref (_fun _pointer -> _void)
  #:wrap (deallocator))
(define-glib g_free (_fun _pointer -> _void)
  #:wrap (deallocator))

(define-pangocairo pango_cairo_font_map_get_default (_fun -> PangoFontMap)) ;; not an allocator
(define-pangocairo pango_cairo_font_map_new (_fun -> PangoFontMap)
  #:wrap (allocator g_object_unref))

(define-pango pango_context_new (_fun -> PangoContext)
  #:wrap (allocator g_object_unref))
;; pango_font_map_create_context() is in 1.22 and later
(provide pango_font_map_create_context)
(define (pango_font_map_create_context fm)
  (let ([c (pango_context_new)])
    (pango_context_set_font_map c fm)
    c))
(define-pangocairo pango_cairo_update_context (_fun _cairo_t PangoContext -> _void))

;; The convenince function pango_cairo_create_context() is in 1.22 and later
(provide pango_cairo_create_context)
(define (pango_cairo_create_context cr)
  (let ([ctx (pango_font_map_create_context
              (pango_cairo_font_map_get_default))])
    (pango_cairo_update_context cr ctx)
    ctx))

(define-pangocairo pango_cairo_create_layout (_fun _cairo_t -> PangoLayout)
  #:wrap (allocator g_object_unref))
(define-pangocairo pango_cairo_update_layout (_fun _cairo_t PangoLayout -> _void))
(define-pango pango_layout_set_text (_fun PangoLayout [s : _string] [_int = -1] -> _void))
(define-pangocairo pango_cairo_show_layout (_fun _cairo_t PangoLayout -> _void))
(define-pangocairo pango_cairo_show_layout_line (_fun _cairo_t PangoLayoutLine -> _void))
(define-pangocairo pango_cairo_show_glyph_string (_fun _cairo_t PangoFont _PangoGlyphString-pointer -> _void))

(define-pango pango_layout_iter_free (_fun PangoLayoutIter -> _void)
  #:wrap (deallocator))
(define-pango pango_layout_get_iter (_fun PangoLayout -> PangoLayoutIter)
  #:wrap (allocator pango_layout_iter_free))
(define-pango pango_layout_iter_get_baseline (_fun PangoLayoutIter -> _int))
(define-pango pango_layout_iter_next_run (_fun PangoLayoutIter -> _bool))
(define-pango pango_layout_iter_get_run_readonly (_fun PangoLayoutIter -> (_or-null _PangoGlyphItem-pointer)))

(define-pango pango_layout_get_line (_fun PangoLayout _int -> PangoLayoutLine))
(define-pango pango_layout_get_line_readonly (_fun PangoLayout _int -> PangoLayoutLine)
  #:fail (lambda () pango_layout_get_line))

(define-pango pango_layout_get_context (_fun PangoLayout -> PangoContext)) ;; not an allocator
(define-pango pango_layout_get_extents (_fun PangoLayout  _pointer _PangoRectangle-pointer -> _void))
(define-pango pango_layout_get_baseline (_fun PangoLayout -> _int)
  ;; The convenince function pango_layout_get_baseline() is in 1.22 and later
  #:fail (lambda ()
           (lambda (layout)
             (let ([iter (pango_layout_get_iter layout)])
               (begin0
                (pango_layout_iter_get_baseline iter)
                (pango_layout_iter_free iter))))))
(define-pango pango_layout_get_spacing (_fun PangoLayout -> _int))

(define-pango pango_layout_new (_fun PangoContext -> PangoLayout)
  #:wrap (allocator g_object_unref))

(define-pangocairo pango_cairo_context_get_font_options (_fun PangoContext -> _cairo_font_options_t)) ;; not an allocator
(define-pangocairo pango_cairo_context_set_font_options (_fun PangoContext _cairo_font_options_t -> _void)) ;; makes a copy

(define-pango pango_layout_set_font_description (_fun PangoLayout PangoFontDescription -> _void)) ;; makes a copy
(define-pango pango_context_get_font_map (_fun PangoContext -> PangoFontMap)) ;; not an allocator
(define-pango pango_context_set_font_map (_fun PangoContext PangoFontMap -> _void))
(define-pango pango_font_family_get_name (_fun PangoFontFamily -> _string)) ;; not an allocator
(define-pango pango_font_family_is_monospace (_fun PangoFontFamily -> _bool))

(define-pango pango_language_get_default (_fun -> PangoLanguage))
(define-pango pango_font_map_load_font (_fun PangoFontMap PangoContext PangoFontDescription -> (_or-null PangoFont)))
(define-pango pango_coverage_unref (_fun PangoCoverage -> _void)
  #:wrap (deallocator))
(define-pango pango_font_get_coverage (_fun PangoFont PangoLanguage -> PangoCoverage)
  #:wrap (allocator pango_coverage_unref))
(define-pango pango_coverage_get (_fun PangoCoverage _int -> _int))

(define-pango pango_font_metrics_unref (_fun PangoFontMetrics -> _void)
  #:wrap (deallocator))
(define-pango pango_font_get_metrics (_fun PangoFont PangoLanguage -> PangoFontMetrics)
  #:wrap (allocator pango_font_metrics_unref))
(define-pango pango_font_metrics_get_approximate_char_width (_fun PangoFontMetrics -> _int))
(define-pango pango_font_metrics_get_ascent (_fun PangoFontMetrics -> _int))
(define-pango pango_font_metrics_get_descent (_fun PangoFontMetrics -> _int))

(define-pango pango_layout_get_unknown_glyphs_count (_fun PangoLayout -> _int))

(define-pango pango_attr_list_unref (_fun PangoAttrList -> _void)
  #:wrap (deallocator))
(define-pango pango_attr_list_new (_fun -> PangoAttrList)
  #:wrap (allocator pango_attr_list_unref))
(define-pango pango_attr_list_insert (_fun PangoAttrList PangoAttribute -> _void)
  ;; takes ownership of the attribute
  #:wrap (deallocator cadr))

(define-pango pango_attribute_destroy (_fun PangoAttribute -> _void)
  #:wrap (deallocator))
(define-pango pango_attr_underline_new (_fun _int -> PangoAttribute)
  #:wrap (allocator pango_attribute_destroy))
(define-pango pango_attr_fallback_new (_fun _bool -> PangoAttribute)
  #:wrap (allocator pango_attribute_destroy))

(define-pango pango_layout_set_attributes (_fun PangoLayout PangoAttrList -> _void))

(define-pango pango_font_map_list_families (_fun PangoFontMap
                                                 (fams : (_ptr o _pointer))
                                                 (len : (_ptr o _int))
                                                 -> _void
                                                 -> (begin0
                                                      (for/list ([i (in-range len)])
                                                        (ptr-ref fams PangoFontFamily i))
                                                      (g_free fams))))

(define-pango pango_font_description_free (_fun PangoFontDescription -> _void) 
  #:wrap (deallocator))
(define-pango pango_font_description_new (_fun -> PangoFontDescription) 
  #:wrap (allocator pango_font_description_free))
(define-pango pango_font_description_from_string (_fun _string -> PangoFontDescription)
  #:wrap (allocator pango_font_description_free))
(define-pango pango_font_description_set_family (_fun PangoFontDescription _string -> _void))
(define-pango pango_font_description_set_style (_fun PangoFontDescription _int -> _void))
(define-pango pango_font_description_set_weight (_fun PangoFontDescription _int -> _void))
(define-pango pango_font_description_set_size (_fun PangoFontDescription _int -> _void))
(define-pango pango_font_description_set_absolute_size (_fun PangoFontDescription _double* -> _void))

(define _PangoWin32FontCache (_cpointer 'PangoWin32FontCache))
(define _HFONT (_cpointer 'HFONT))
(define _LOGFONT-pointer _pointer)
(define-pangowin32 pango_win32_font_map_for_display (_fun -> PangoFontMap)
  #:make-fail make-not-available)
(define-pangowin32 pango_win32_font_logfont (_fun PangoFont -> _LOGFONT-pointer)
  #:make-fail make-not-available
  #:wrap (allocator g_free))
(define-pangowin32 pango_win32_font_cache_unload (_fun _PangoWin32FontCache _HFONT -> _void)
  #:make-fail make-not-available)
(define-pangowin32 pango_win32_font_cache_load (_fun _PangoWin32FontCache _LOGFONT-pointer -> _HFONT)
  #:make-fail make-not-available)
(define-pangowin32 pango_win32_font_cache_new (_fun -> _PangoWin32FontCache)
  #:make-fail make-not-available)

(define-enum
  0
  PANGO_STYLE_NORMAL
  PANGO_STYLE_OBLIQUE
  PANGO_STYLE_ITALIC)

(define-enum
  0
  PANGO_UNDERLINE_NONE
  PANGO_UNDERLINE_SINGLE
  PANGO_UNDERLINE_DOUBLE
  PANGO_UNDERLINE_LOW
  PANGO_UNDERLINE_ERROR)

(define/provide PANGO_WEIGHT_LIGHT 300)
(define/provide PANGO_WEIGHT_MEDIUM 500)
(define/provide PANGO_WEIGHT_BOLD 700)

(define/provide PANGO_SCALE 1024)
