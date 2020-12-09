static ptr copy(vfasl_info *vfi, ptr p, seginfo *si)
{
  ptr new_p;
  {
    ITYPE t = TYPEBITS(p);
    if (t == type_typed_object)
    {
      ptr tf = TYPEFIELD(p);
      if (TYPEP(tf, mask_record, type_record))
      {
        {
          ptr rtd = RECORDINSTTYPE(p);
          int p_vspc = ((is_rtd(rtd, vfi))
                        ? vspace_rtd
                        : (((RECORDDESCMPM(rtd)) == (FIX(0)))
                           ? vspace_pure_typed
                           : vspace_impure_record));
          if (is_rtd(rtd, vfi))
          {
            if (p != S_G.base_rtd)
            {
              (void)(vfasl_relocate_help(vfi, rtd));
            }
            vfasl_relocate_parents(vfi, RECORDDESCPARENT(p));
          }
          {
            uptr len = UNFIX((RECORDDESCSIZE(rtd)));
            {
              uptr p_sz = size_record_inst(len);
              FIND_ROOM(vfi, p_vspc, type_typed_object, p_sz, new_p);
              RECORDINSTTYPE(new_p) = rtd;
              memcpy_aligned(&RECORDINSTIT(new_p, 0), &RECORDINSTIT(p, 0), len - ptr_bytes);
              if (p == S_G.base_rtd)
              {
                vfi -> base_rtd = new_p;
              }
              {
                uptr ua_size = unaligned_size_record_inst(len);
                if (p_sz != ua_size)
                {
                  *(((ptr*)(TO_VOIDP((((uptr)(UNTYPE(new_p, type_typed_object))) + ua_size))))) = FIX(0);
                }
              }
            }
          }
        }
      }
      else if (TYPEP(tf, mask_vector, type_vector))
      {
        int p_vspc = vspace_impure;
        {
          uptr len = Svector_length(p);
          {
            uptr p_sz = size_vector(len);
            FIND_ROOM(vfi, p_vspc, type_typed_object, p_sz, new_p);
            VECTTYPE(new_p) = (uptr)tf;
            memcpy_aligned(&INITVECTIT(new_p, 0), &INITVECTIT(p, 0), ptr_bytes * len);
            if ((len & 1) == 0)
            {
              INITVECTIT(new_p, len) = FIX(0);
            }
          }
        }
      }
      else if (TYPEP(tf, mask_stencil_vector, type_stencil_vector))
      {
        int p_vspc = vspace_impure;
        {
          uptr len = Sstencil_vector_length(p);
          {
            uptr p_sz = size_stencil_vector(len);
            FIND_ROOM(vfi, p_vspc, type_typed_object, p_sz, new_p);
            STENVECTTYPE(new_p) = (uptr)tf;
            memcpy_aligned(&INITSTENVECTIT(new_p, 0), &INITSTENVECTIT(p, 0), ptr_bytes * len);
            if ((len & 1) == 0)
            {
              INITSTENVECTIT(new_p, len) = FIX(0);
            }
          }
        }
      }
      else if (TYPEP(tf, mask_string, type_string))
      {
        int p_vspc = vspace_data;
        {
          uptr sz = size_string((Sstring_length(p)));
          {
            uptr p_sz = sz;
            FIND_ROOM(vfi, p_vspc, type_typed_object, p_sz, new_p);
            memcpy_aligned(&STRTYPE(new_p), &STRTYPE(p), sz);
          }
        }
      }
      else if (TYPEP(tf, mask_fxvector, type_fxvector))
      {
        int p_vspc = vspace_data;
        {
          uptr sz = size_fxvector((Sfxvector_length(p)));
          {
            uptr p_sz = sz;
            FIND_ROOM(vfi, p_vspc, type_typed_object, p_sz, new_p);
            memcpy_aligned(&FXVECTOR_TYPE(new_p), &FXVECTOR_TYPE(p), sz);
          }
        }
      }
      else if (TYPEP(tf, mask_bytevector, type_bytevector))
      {
        int p_vspc = vspace_data;
        {
          uptr sz = size_bytevector((Sbytevector_length(p)));
          {
            uptr p_sz = sz;
            FIND_ROOM(vfi, p_vspc, type_typed_object, p_sz, new_p);
            memcpy_aligned(&BYTEVECTOR_TYPE(new_p), &BYTEVECTOR_TYPE(p), sz);
          }
        }
      }
      else if ((iptr)tf == type_tlc)
      {
        vfasl_fail(vfi, "tlc");
        return(((ptr)0));
      }
      else if (TYPEP(tf, mask_box, type_box))
      {
        int p_vspc = vspace_impure;
        {
          uptr p_sz = size_box;
          FIND_ROOM(vfi, p_vspc, type_typed_object, p_sz, new_p);
          BOXTYPE(new_p) = (uptr)tf;
          INITBOXREF(new_p) = INITBOXREF(p);
        }
      }
      else if ((iptr)tf == type_ratnum)
      {
        int p_vspc = vspace_impure;
        {
          uptr p_sz = size_ratnum;
          FIND_ROOM(vfi, p_vspc, type_typed_object, p_sz, new_p);
          RATTYPE(new_p) = type_ratnum;
          RATNUM(new_p) = RATNUM(p);
          RATDEN(new_p) = RATDEN(p);
          ((ptr*)(TO_VOIDP((UNTYPE(new_p, type_typed_object)))))[3] = 0;
        }
      }
      else if ((iptr)tf == type_exactnum)
      {
        int p_vspc = vspace_impure;
        {
          uptr p_sz = size_exactnum;
          FIND_ROOM(vfi, p_vspc, type_typed_object, p_sz, new_p);
          EXACTNUM_TYPE(new_p) = type_exactnum;
          EXACTNUM_REAL_PART(new_p) = EXACTNUM_REAL_PART(p);
          EXACTNUM_IMAG_PART(new_p) = EXACTNUM_IMAG_PART(p);
          ((ptr*)(TO_VOIDP((UNTYPE(new_p, type_typed_object)))))[3] = 0;
        }
      }
      else if ((iptr)tf == type_inexactnum)
      {
        int p_vspc = vspace_data;
        {
          uptr p_sz = size_inexactnum;
          FIND_ROOM(vfi, p_vspc, type_typed_object, p_sz, new_p);
          INEXACTNUM_TYPE(new_p) = type_inexactnum;
          INEXACTNUM_REAL_PART(new_p) = INEXACTNUM_REAL_PART(p);
          INEXACTNUM_IMAG_PART(new_p) = INEXACTNUM_IMAG_PART(p);
        }
      }
      else if (TYPEP(tf, mask_bignum, type_bignum))
      {
        int p_vspc = vspace_data;
        {
          uptr sz = size_bignum((BIGLEN(p)));
          {
            uptr p_sz = sz;
            FIND_ROOM(vfi, p_vspc, type_typed_object, p_sz, new_p);
            memcpy_aligned(&BIGTYPE(new_p), &BIGTYPE(p), sz);
          }
        }
      }
      else if (TYPEP(tf, mask_port, type_port))
      {
        vfasl_fail(vfi, "port");
        return(((ptr)0));
      }
      else if (TYPEP(tf, mask_code, type_code))
      {
        int p_vspc = vspace_code;
        {
          uptr len = CODELEN(p);
          {
            uptr p_sz = size_code(len);
            FIND_ROOM(vfi, p_vspc, type_typed_object, p_sz, new_p);
            CODETYPE(new_p) = (uptr)tf;
            CODELEN(new_p) = CODELEN(p);
            CODERELOC(new_p) = CODERELOC(p);
            CODENAME(new_p) = CODENAME(p);
            CODEARITYMASK(new_p) = CODEARITYMASK(p);
            CODEFREE(new_p) = CODEFREE(p);
            CODEINFO(new_p) = CODEINFO(p);
            CODEPINFOS(new_p) = CODEPINFOS(p);
            memcpy_aligned(&CODEIT(new_p, 0), &CODEIT(p, 0), len);
          }
        }
      }
      else if ((iptr)tf == type_thread)
      {
        vfasl_fail(vfi, "thread");
        return(((ptr)0));
      }
      else if ((iptr)tf == type_rtd_counts)
      {
        return(Sfalse);
      }
      else if ((iptr)tf == type_phantom)
      {
        vfasl_fail(vfi, "phantom");
        return(((ptr)0));
      }
      else
      {
        S_error_abort("vfasl-copy: illegal typed object type");
      }
    }
    else if (t == type_pair)
    {
      {
        ISPC p_at_spc = si->space;
        if (p_at_spc == space_ephemeron)
        {
          vfasl_fail(vfi, "ephemeron");
          return(((ptr)0));
        }
        else if (p_at_spc == space_weakpair)
        {
          vfasl_fail(vfi, "weakpair");
          return(((ptr)0));
        }
        else
        {
          int p_vspc = vspace_impure;
          {
            uptr p_sz = size_pair;
            FIND_ROOM(vfi, p_vspc, type_pair, p_sz, new_p);
            INITCAR(new_p) = INITCAR(p);
            INITCDR(new_p) = INITCDR(p);
          }
        }
      }
    }
    else if (t == type_closure)
    {
      ptr code = CLOSCODE(p);
      if ((CODETYPE(code)) & (code_flag_continuation << code_flags_offset))
      {
        vfasl_fail(vfi, "closure");
        return(((ptr)0));
      }
      else
      {
        int p_vspc = vspace_closure;
        if ((CODETYPE(code)) & (code_flag_mutable_closure << code_flags_offset))
        {
          vfasl_fail(vfi, "mutable closure");
          return(((ptr)0));
        }
        {
          uptr len = CODEFREE(code);
          {
            uptr p_sz = size_closure(len);
            FIND_ROOM(vfi, p_vspc, type_closure, p_sz, new_p);
            SETCLOSCODE(new_p, code);
            memcpy_aligned(&CLOSIT(new_p, 0), &CLOSIT(p, 0), ptr_bytes * len);
            if ((len & 1) == 0)
            {
              CLOSIT(new_p, len) = FIX(0);
            }
          }
        }
      }
    }
    else if (t == type_symbol)
    {
      int p_vspc = vspace_symbol;
      {
        uptr p_sz = size_symbol;
        FIND_ROOM(vfi, p_vspc, type_symbol, p_sz, new_p);
        INITSYMVAL(new_p) = FIX((vfasl_symbol_to_index(vfi, p)));
        INITSYMPVAL(new_p) = Snil;
        INITSYMPLIST(new_p) = Snil;
        INITSYMNAME(new_p) = INITSYMNAME(p);
        INITSYMSPLIST(new_p) = Snil;
        INITSYMHASH(new_p) = INITSYMHASH(p);
      }
    }
    else if (t == type_flonum)
    {
      int p_vspc = vspace_data;
      {
        uptr p_sz = size_flonum;
        FIND_ROOM(vfi, p_vspc, type_flonum, p_sz, new_p);
        FLODAT(new_p) = FLODAT(p);
      }
    }
    else
    {
      S_error_abort("vfasl-copy: illegal type");
    }
  }
  vfasl_register_forward(vfi, p, new_p);
  return new_p;
}

static uptr sweep(vfasl_info *vfi, ptr p)
{
  uptr result_sz;
  {
    ITYPE t = TYPEBITS(p);
    if (t == type_typed_object)
    {
      ptr tf = TYPEFIELD(p);
      if (TYPEP(tf, mask_record, type_record))
      {
        {
          ptr rtd = RECORDINSTTYPE(p);
          if (p == (vfi -> base_rtd))
          {
            {
              ptr* pp = &(RECORDINSTIT(p, 0));
              ptr* ppend = ((ptr*)(TO_VOIDP((((uptr)(TO_PTR(pp))) + (UNFIX((RECORDDESCSIZE(rtd)))))))) - 1;
              while (pp < ppend)
              {
                *(pp) = Snil;
                pp += 1;
              }
              return((size_record_inst((UNFIX((RECORDDESCSIZE(rtd)))))));
            }
          }
          vfasl_relocate(vfi, &(RECORDINSTTYPE(p)));
          {
            uptr len = UNFIX((RECORDDESCSIZE(rtd)));
            {
              uptr p_sz = size_record_inst(len);
              result_sz = p_sz;
              {
                ptr num = RECORDDESCPM(rtd);
                ptr* pp = &(RECORDINSTIT(p, 0));
                if (Sfixnump(num))
                {
                  {
                    uptr mask = ((uptr)(UNFIX(num))) >> 1;
                    if (mask == (((uptr)-1) >> 1))
                    {
                      {
                        ptr* ppend = ((ptr*)(TO_VOIDP((((uptr)(TO_PTR(pp))) + len)))) - 1;
                        while (pp < ppend)
                        {
                          vfasl_relocate(vfi, &(*(pp)));
                          pp += 1;
                        }
                      }
                    }
                    else
                    {
                      while (mask != 0)
                      {
                        if (mask & 1)
                        {
                          vfasl_relocate(vfi, &(*(pp)));
                        }
                        mask >>= 1;
                        pp += 1;
                      }
                    }
                  }
                }
                else
                {
                  {
                    iptr index = (BIGLEN(num)) - 1;
                    bigit mask = (BIGIT(num, index)) >> 1;
                    INT bits = bigit_bits - 1;
                    while (1)
                    {
                      do
                      {
                        if (mask & 1)
                        {
                          vfasl_relocate(vfi, &(*(pp)));
                        }
                        mask >>= 1;
                        pp += 1;
                        bits -= 1;
                      }
                      while (bits > 0);
                      if (index == 0)
                      {
                        break;
                      }
                      index -= 1;
                      mask = BIGIT(num, index);
                      bits = bigit_bits;
                    }
                  }
                }
              }
            }
          }
        }
      }
      else if (TYPEP(tf, mask_vector, type_vector))
      {
        uptr len = Svector_length(p);
        {
          uptr p_sz = size_vector(len);
          result_sz = p_sz;
          {
            uptr idx, p_len = len;
            ptr *p_p = &INITVECTIT(p, 0);
            for (idx = 0; idx < p_len; idx++)
            {
              vfasl_relocate(vfi, &(p_p[idx]));
            }
          }
        }
      }
      else if (TYPEP(tf, mask_stencil_vector, type_stencil_vector))
      {
        uptr len = Sstencil_vector_length(p);
        {
          uptr p_sz = size_stencil_vector(len);
          result_sz = p_sz;
          {
            uptr idx, p_len = len;
            ptr *p_p = &INITSTENVECTIT(p, 0);
            for (idx = 0; idx < p_len; idx++)
            {
              vfasl_relocate(vfi, &(p_p[idx]));
            }
          }
        }
      }
      else if (TYPEP(tf, mask_string, type_string))
      {
        uptr sz = size_string((Sstring_length(p)));
        {
          uptr p_sz = sz;
          result_sz = p_sz;
        }
      }
      else if (TYPEP(tf, mask_fxvector, type_fxvector))
      {
        uptr sz = size_fxvector((Sfxvector_length(p)));
        {
          uptr p_sz = sz;
          result_sz = p_sz;
        }
      }
      else if (TYPEP(tf, mask_bytevector, type_bytevector))
      {
        uptr sz = size_bytevector((Sbytevector_length(p)));
        {
          uptr p_sz = sz;
          result_sz = p_sz;
        }
      }
      else if ((iptr)tf == type_tlc)
      {
        vfasl_fail(vfi, "tlc");
        return(0);
      }
      else if (TYPEP(tf, mask_box, type_box))
      {
        uptr p_sz = size_box;
        result_sz = p_sz;
        vfasl_relocate(vfi, &INITBOXREF(p));
      }
      else if ((iptr)tf == type_ratnum)
      {
        uptr p_sz = size_ratnum;
        result_sz = p_sz;
        vfasl_relocate(vfi, &RATNUM(p));
        vfasl_relocate(vfi, &RATDEN(p));
      }
      else if ((iptr)tf == type_exactnum)
      {
        uptr p_sz = size_exactnum;
        result_sz = p_sz;
        vfasl_relocate(vfi, &EXACTNUM_REAL_PART(p));
        vfasl_relocate(vfi, &EXACTNUM_IMAG_PART(p));
      }
      else if ((iptr)tf == type_inexactnum)
      {
        uptr p_sz = size_inexactnum;
        result_sz = p_sz;
      }
      else if (TYPEP(tf, mask_bignum, type_bignum))
      {
        uptr sz = size_bignum((BIGLEN(p)));
        {
          uptr p_sz = sz;
          result_sz = p_sz;
        }
      }
      else if (TYPEP(tf, mask_port, type_port))
      {
        vfasl_fail(vfi, "port");
        return(0);
      }
      else if (TYPEP(tf, mask_code, type_code))
      {
        uptr len = CODELEN(p);
        {
          uptr p_sz = size_code(len);
          result_sz = p_sz;
          vfasl_relocate(vfi, &CODENAME(p));
          vfasl_relocate(vfi, &CODEARITYMASK(p));
          vfasl_relocate(vfi, &CODEINFO(p));
          vfasl_relocate(vfi, &CODEPINFOS(p));
          {
            ptr t = CODERELOC(p);
            {
              iptr m = (t
                        ? (RELOCSIZE(t))
                        : 0);
              {
                ptr oldco = (t
                             ? (RELOCCODE(t))
                             : 0);
                {
                  uptr r_sz = size_reloc_table(m);
                  ptr new_t = vfasl_find_room(vfi, vspace_reloc, typemod, r_sz);
                  memcpy_aligned(TO_VOIDP(new_t), TO_VOIDP(t), r_sz);
                  t = new_t;
                }
                {
                  iptr a = 0;
                  {
                    iptr n = 0;
                    while (n < m)
                    {
                      {
                        uptr entry = RELOCIT(t, n);
                        uptr item_off = 0;
                        uptr code_off = 0;
                        n = n + 1;
                        if (RELOC_EXTENDED_FORMAT(entry))
                        {
                          item_off = RELOCIT(t, n);
                          n = n + 1;
                          code_off = RELOCIT(t, n);
                          n = n + 1;
                        }
                        else
                        {
                          item_off = RELOC_ITEM_OFFSET(entry);
                          code_off = RELOC_CODE_OFFSET(entry);
                        }
                        a = a + code_off;
                        {
                          ptr obj = S_get_code_obj(RELOC_TYPE(entry), oldco, a, item_off);
                          obj = vfasl_encode_relocation(vfi, obj);
                          S_set_code_obj("vfasl", abs_reloc_variant((RELOC_TYPE(entry))), p, a, obj, item_off);
                        }
                      }
                    }
                    RELOCCODE(t) = (ptr)(ptr_diff(p, vfi -> base_addr));
                    CODERELOC(p) = (ptr)(ptr_diff(t, vfi -> base_addr));
                  }
                }
              }
            }
          }
        }
      }
      else if ((iptr)tf == type_thread)
      {
        vfasl_fail(vfi, "thread");
        return(0);
      }
      else if ((iptr)tf == type_rtd_counts)
      {
        vfasl_fail(vfi, "rtd-counts");
        return(0);
      }
      else if ((iptr)tf == type_phantom)
      {
        vfasl_fail(vfi, "phantom");
        return(0);
      }
      else
      {
        S_error_abort("vfasl-sweep: illegal typed object type");
      }
    }
    else if (t == type_pair)
    {
      {
        ISPC p_at_spc = SPACE(p);
        if (p_at_spc == space_ephemeron)
        {
          vfasl_fail(vfi, "ephemeron");
          return(0);
        }
        else if (p_at_spc == space_weakpair)
        {
          vfasl_fail(vfi, "weakpair");
          return(0);
        }
        else
        {
          uptr p_sz = size_pair;
          result_sz = p_sz;
          vfasl_relocate(vfi, &INITCAR(p));
          vfasl_relocate(vfi, &INITCDR(p));
        }
      }
    }
    else if (t == type_closure)
    {
      ptr code = CLOSCODE(p);
      code = vfasl_relocate_code(vfi, code);
      if ((CODETYPE(code)) & (code_flag_continuation << code_flags_offset))
      {
        vfasl_fail(vfi, "closure");
        return(0);
      }
      else
      {
        if ((CODETYPE(code)) & (code_flag_mutable_closure << code_flags_offset))
        {
          vfasl_fail(vfi, "mutable closure");
          return(0);
        }
        {
          uptr len = CODEFREE(code);
          {
            uptr p_sz = size_closure(len);
            result_sz = p_sz;
            {
              ptr rel_code = (ptr)(ptr_diff(code, vfi -> base_addr));
              SETCLOSCODE(p, rel_code);
              {
                uptr idx, p_len = len;
                ptr *p_p = &CLOSIT(p, 0);
                for (idx = 0; idx < p_len; idx++)
                {
                  vfasl_relocate(vfi, &(p_p[idx]));
                }
              }
            }
          }
        }
      }
    }
    else if (t == type_symbol)
    {
      uptr p_sz = size_symbol;
      result_sz = p_sz;
      vfasl_relocate(vfi, &INITSYMPLIST(p));
      vfasl_relocate(vfi, &INITSYMNAME(p));
      vfasl_relocate(vfi, &INITSYMSPLIST(p));
      vfasl_relocate(vfi, &INITSYMHASH(p));
    }
    else if (t == type_flonum)
    {
      uptr p_sz = size_flonum;
      result_sz = p_sz;
    }
    else
    {
      S_error_abort("vfasl-sweep: illegal type");
    }
  }
  return result_sz;
}

