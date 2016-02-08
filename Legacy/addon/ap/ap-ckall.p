/***************************************************************************\
*****************************************************************************
**  Program: ap\ap-ckall.p  All package Laser format
** Descript: Copied from ap-chks.p and one line changed
**
*****************************************************************************
\***************************************************************************/

{sys/inc/var.i shared}

{ap/ap-chk.i}

def workfile wrk-chk
        field inv-no like ap-sel.inv-no
        field line-amt as dec format "->>>,>>9.99"
        field disc-amt like ap-sel.disc-amt.


form
  skip(8)
  ap-chk.check-no to 6 ap-chk.check-date FORMAT "99/99/99" to 15
  vend.vend-no to 24 ctot to 77 format "*,***,**9.99"
  skip(2)
  dol at 10
  skip(3)
  vend.remit at 10
  add1       at 10
  add2       at 10
  csz        at 10
  skip(4)
/* gdm - 10190903
  skip(2)
  vend.remit at 10
  add1       at 10
  add2       at 10
  csz        at 10
  skip(1)
  "Memo:"    to 8
  vend.check-memo
  skip(3)
 gdm - 10190903 end */
  with frame b1 width 85 no-box no-labels NO-ATTR-SPACE STREAM-IO.
  
form
  skip(8)
  ap-chk.check-no to 6 ap-chk.check-date FORMAT "99/99/99" to 15
  vend.vend-no to 24 ctot to 77 format "*,***,**9.99"
  skip(2)
  dollar[1]  at 10
  dollar[2]  at 12
  skip(1)
  vend.remit at 10
  add1       at 10
  add2       at 10
  csz        at 10
  skip(1)
  "Memo:"    to 8
  vend.check-memo 
  skip(3)
  with frame b2 width 85 no-box no-labels NO-ATTR-SPACE STREAM-IO.
  
form
  ap-chk.check-date FORMAT "99/99/99" at 7
  "** Check Totals **" at 31
  ctot to 79
  with frame b3 no-box no-labels no-attr-space.
  
form
  skip(8)
  ap-chk.check-no to 6 ap-chk.check-date FORMAT "99/99/99" to 15
  vend.vend-no to 24 ctot to 77 format "*,***,**9.99"
  skip(2)
  dol at 10
  skip(3)
  vend.remit at 10
  add1       at 10
  add2       at 10
  csz        at 10
  skip(3)
/* gdm - 10190903
  skip(3)
  vend.remit at 10
  add1       at 10
  add2       at 10
  csz        at 10
  skip(1)
  "Memo:"    to 8
  vend.check-memo
  skip(2)
 gdm - 10190903 end */
  with frame b4 width 85 no-box no-labels NO-ATTR-SPACE STREAM-IO.


find first sys-ctrl where sys-ctrl.company eq cocode and
                          sys-ctrl.name eq "CHKFMT" no-lock no-error.

  for each ap-chk
      where ap-chk.company   eq cocode
        and ap-chk.vend-no   ge wvend-no
        and ap-chk.vend-no   le evend-no
        and ap-chk.man-check eq no,
        
      first vend
      where vend.company eq cocode
        and vend.vend-no eq ap-chk.vend-no,
        
      each ap-sel
      where ap-sel.company   eq cocode
        and ap-sel.vend-no   eq ap-chk.vend-no
        and ap-sel.man-check eq no

      break by (if v-sort-name then vend.name else "")
            by ap-chk.vend-no
            by ap-sel.vend-no
            by ap-sel.inv-no:

    IF FIRST-OF(ap-chk.vend-no) THEN DO:
      ASSIGN
       add1   = vend.r-add1
       add2   = vend.r-add2
       csz    = vend.r-city + ", " + vend.r-state
       lv-zip = vend.r-zip.

      IF add1 EQ "" AND add2 EQ "" THEN    /*if no remit-to address*/
        ASSIGN
         add1   = vend.add1
         add2   = vend.add2
         csz    = vend.city + ", " + vend.state
         lv-zip = vend.zip.

      IF lv-zip BEGINS "00000" THEN lv-zip = "".

      csz = TRIM(csz) + "  " + SUBSTR(lv-zip,1,5) +
            (IF LENGTH(lv-zip) GT 5 THEN ("-" + SUBSTR(lv-zip,6,4)) ELSE "").

      IF TRIM(csz) EQ "," THEN csz = "".

      ASSIGN
       ll                = 0
       ap-chk.check-date = wdate
       ap-chk.check-no   = stnum
       ap-chk.bank-code  = x-bank
       v-vend-no         = vend.vend-no
       v-vend-name       = vend.name.

     IF add1 EQ "" THEN
       ASSIGN
        add1 = add2
        add2 = "".

     IF add2 EQ "" THEN
       ASSIGN
        add2 = csz
        csz  = "".
    END.
  
    /************* print check stub at top of form ***************/
    find first ap-inv use-index inv-no
      where ap-inv.company eq cocode and
            ap-inv.inv-no  eq ap-sel.inv-no.
     
    assign ap-inv.pay-date = wdate.
    if ll eq 0 then do:
      if laser-chk then do:
        page.
        if sys-ctrl.char-fld eq "Raritan" then do:
          display         v-vend-no to 8 format "x(8)"
                          v-vend-name at 11 skip(2)
                          with no-labels no-box frame xyz-raritan STREAM-IO.
        end.
        else
          display skip(2) v-vend-no to 8 format "x(8)"
                          v-vend-name at 11 skip(2)
                          with no-labels no-box frame xyz-laser STREAM-IO.
      end.
      else
        display skip(3) v-vend-no to 8 format "x(8)"
                        v-vend-name at 11 skip(2)
                        with no-labels no-box frame xyz STREAM-IO.

      assign v-left = yes.
    end.

    assign  ctot   = ctot + ap-sel.amt-paid
      cdis   = cdis + ap-sel.disc-amt
      ap-sel.check-date = wdate
      cgrossl = cgrossl + (ap-sel.amt-paid + ap-sel.disc-amt)
      cgross = cgross + cgrossl.

    if laser-chk then do:
      create wrk-chk.
      assign wrk-chk.inv-no = ap-sel.inv-no
              wrk-chk.line-amt = cgrossl
              wrk-chk.disc-amt = ap-sel.disc-amt.
    end.

    if v-left then do:
      put ap-sel.inv-no to 12
        cgrossl to 24 format "->>>,>>9.99"
        ap-sel.disc-amt to 34.
      assign v-left = no
        ll = ll + 1.
    end.
    else do:
      put ap-sel.inv-no to  59
        cgrossl to 70 format "->>>,>>9.99"
        ap-sel.disc-amt to 80 skip.
      assign v-left = yes.
    end.
    cgrossl = 0.
    if ll = max-per-chk / 2 and v-left and not last-of(ap-sel.vend-no) then do:
      assign  stnum = stnum + 1
        ap-chk.check-no = stnum
        ll = 0.
      display skip(12)
        "V   V      OOO       III      DDDD"   at 10 skip
        "V   V     O   O       I       D   D"  at 10 skip
        "V   V     O   O       I       D   D"  at 10 skip
        " V V      O   O       I       D   D"  at 10 skip
        " V V      O   O       I       D   D"  at 10 skip
        "  V        OOO       III      DDDD "  at 10 skip(8)
        with frame u no-box no-labels no-attr-space.

      if laser-chk then do:
        if sys-ctrl.char-fld eq "Raritan" then do:
          display         v-vend-no to 8 format "x(8)"
                          v-vend-name at 11 skip(2)
                          with no-labels no-box frame xyz-raritan STREAM-IO.
        end.
        else
          display skip(2) v-vend-no to 8 format "x(8)"
                          v-vend-name at 11  skip(2)
                          with no-labels no-box frame xyz-laser1 STREAM-IO.
        assign v-left = yes.

        ll = 0.
        for each wrk-chk:
          if v-left then
          do:
            put wrk-chk.inv-no to 12
                wrk-chk.line-amt to 24 format "->>>,>>9.99"
                wrk-chk.disc-amt to 34.
            assign v-left = no
                    ll = ll + 1.
          end.
          else
          do:
            put wrk-chk.inv-no to 59
                wrk-chk.line-amt to 70 format "->>>,>>9.99"
                wrk-chk.disc-amt to 80 skip.
            assign v-left = yes.
          end.
          delete wrk-chk.
        end.
        put skip((max-per-chk / 2) + 2 - ll).
        put stnum to 6 wdate to 15 v-vend-no to 24 format "x(8)"
            cgross to 43 cdis to 59 ctot to 80.
        PUT SKIP(1).       
        page.
        ll = 0.
      end.
    end.

    /********************** print check ****************************/
    if last-of(ap-sel.vend-no) then do:
      checks-avail = yes.
      put skip((max-per-chk / 2) + 2 - ll).
      put stnum to 6 wdate to 15 v-vend-no to 24 format "x(8)"
        cgross to 43 cdis to 59 ctot to 80 skip.
      run ap/apchks.p (input ctot, input 70, output dol).

      /** display ap-chk.check-date ctot with frame b3.**/
      if sys-ctrl.char-fld eq "TriadLas" then
        display ap-chk.check-no ap-chk.check-date ctot
                vend.vend-no
                caps(dol)         @ dol
                caps(vend.remit)  @ vend.remit
                caps(add1)        @ add1
                caps(add2)        @ add2
                caps(csz)         @ csz
/* gdm - 10190903  vend.check-memo */
                with frame b4.
      else
        display ap-chk.check-no ap-chk.check-date ctot
                vend.vend-no
                caps(dol)         @ dol
                caps(vend.remit)  @ vend.remit
                caps(add1)        @ add1
                caps(add2)        @ add2
                caps(csz)         @ csz
/* gdm - 10190903  vend.check-memo */
                with frame b1.

      if laser-chk then do:
        if sys-ctrl.char-fld eq "Raritan" then do:
          display         v-vend-no to 8 format "x(8)"
                          v-vend-name at 11 skip(2)
                          with no-labels no-box frame xyz-raritan STREAM-IO.
        end.
        else
          display skip(3) v-vend-no to 8 format "x(8)"
                          v-vend-name at 11 skip(2)
                          with no-labels no-box frame xyz-laser2 STREAM-IO.
        assign v-left = yes.

        ll = 0.
        for each wrk-chk:
          if v-left then do:
            put wrk-chk.inv-no to 12
                wrk-chk.line-amt to 24 format "->>>,>>9.99"
                wrk-chk.disc-amt to 34.
            assign v-left = no
                    ll = ll + 1.
          end.
          else do:
            put wrk-chk.inv-no to 59
                wrk-chk.line-amt to 70 format "->>>,>>9.99"
                wrk-chk.disc-amt to 80 skip.
            assign v-left = yes.
          end.
          cgrossl = 0.
          if ll eq max-per-chk / 2 and v-left and
             not last-of(ap-sel.vend-no) then do:
            /* Need a second stub */
            assign  stnum = stnum + 1
                    ap-chk.check-no = stnum
                    ll = 0.
            display skip(12)
                  "V   V      OOO       III      DDDD"   at 10 skip
                  "V   V     O   O       I       D   D"  at 10 skip
                  "V   V     O   O       I       D   D"  at 10 skip
                  " V V      O   O       I       D   D"  at 10 skip
                  " V V      O   O       I       D   D"  at 10 skip
                  "  V        OOO       III      DDDD "  at 10 skip(8)
                  with frame m no-box no-labels no-attr-space.

          end.
        end.  /* end for each wrk-chk  */

        put skip((max-per-chk / 2) + 2 - ll).
        put stnum to 6 wdate to 15 v-vend-no to 24 format "x(8)"
            cgross to 43 cdis to 59 ctot to 80.
        PUT SKIP(1).

        for each wrk-chk:
          delete wrk-chk.
        end.
      end.  /* end if laser */

      assign stnum  = stnum + 1
        ctot   = 0
        cdis   = 0
        cgross = 0
        dol    = ""
        ll = 0.
    end.
  end.
  if not(checks-avail) then put "No checks found ready to print!".

