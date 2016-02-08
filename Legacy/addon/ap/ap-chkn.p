/***************************************************************************\
*****************************************************************************
**  Program: /u2/fold/all/test/asi/ap/ap-chkn.p
**       By: Chris Heins
** Descript: custom format check (n in System Control Parameters [CHKFMT]).
**
*****************************************************************************
\***************************************************************************/

{sys/inc/var.i shared}
{sys/form/s-top.f}

{ap/ap-chk.i}

form
  ap-chk.check-date at 7
  "** Check Totals **" at 31
  ctot to 79
  with frame b3 no-box no-labels stream-io no-attr-space.

form
  skip (10)
  wdate to 35 ap-chk.check-no to 53 ctot to 70 format "*,***,**9.99"
  skip(2)
  dollar[1] at 10
  dollar[2] at 12 format "x(68)"
  skip(1)
  vend.name at 16
  vend.add1 at 16
  add2 at 16
  csz at 16
  skip(3)
  "Memo:"    to 8
  vend.check-memo 
  skip(2)
  with frame b2 no-box no-labels stream-io no-attr-space.

form
  skip (10)
  wdate to 35 ap-chk.check-no to 53 ctot to 70 format "*,***,**9.99"
  skip(2)
  dol at 10
  skip(1)
  vend.name at 16
  add1 at 16
  add2 at 16
  csz at 16
  skip(4)
  "Memo:"    to 8
  vend.check-memo 
  skip(2)
  with frame b1 no-box no-labels stream-io no-attr-space.

if v-print-mode ne "ALIGN" then do:  /* production mode */
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
    if ll ge 0 and
       ll lt max-per-chk then
    do:
      if ll eq 0 then display skip(2)  with no-labels stream-io no-box frame xyz.
      display
        ap-inv.inv-date at  7
        ap-sel.inv-no   at  16
        ap-sel.amt-paid to  79
        with no-box no-labels stream-io no-attr-space no-underline. /* frame aa ? */
      /*  assign v-left = yes. 9508 CAH */
      ll = ll + 1.
    end.

    assign  ctot   = ctot + ap-sel.amt-paid
      cdis   = cdis + ap-sel.disc-amt
      ap-sel.check-date = wdate
      cgrossl = cgrossl + (ap-sel.amt-paid + ap-sel.disc-amt)
      cgross = cgross + cgrossl.

      /* Taken out by CAH 9508 - not sure what all this does,
        but looks like lousy code:
    if v-left then
    do:
      put ap-sel.inv-no to 16
        cgrossl to 28 format "->>>,>>9.99"
        ap-sel.disc-amt to 38.
      assign v-left = no
        ll = ll + 1.
    end.
    else
    do:
      put ap-sel.inv-no to  62
        cgrossl to 73 format "->>>,>>9.99"
        ap-sel.disc-amt to 84 skip.
      assign v-left = yes.
    end.
        */

    cgrossl = 0.
    if ll eq max-per-chk and not last-of(ap-sel.vend-no) then
    do:
      assign  stnum = stnum + 1
        ap-chk.check-no = stnum
        ll = 0.
      display skip(12)
        "V   V      OOO       III      DDDD"   at 10 skip
        "V   V     O   O       I       D   D"  at 10 skip
        "V   V     O   O       I       D   D"  at 10 skip
        " V V      O   O       I       D   D"  at 10 skip
        " V V      O   O       I       D   D"  at 10 skip
        "  V        OOO       III      DDDD "  at 10 skip(9) /* CAH */
        with frame m no-box no-labels stream-io no-attr-space.
    end.

    /********************** print check ****************************/
    if last-of(ap-sel.vend-no) then
    do:
      checks-avail = yes.
      put skip(max-per-chk - ll).
      run ap/apchks.p (input ctot, input 70, output dol).
      display ap-chk.check-date ctot with frame b3.

      display ap-chk.check-no
              wdate
              ctot
              caps(dol)         @ dol
              caps(vend.name)   @ vend.name
              caps(vend.add1)   @ vend.add1
              caps(add2)        @ add2
              caps(csz)         @ csz
              vend.check-memo
          with frame b1.

      assign stnum  = stnum + 1
        ctot   = 0
        cdis   = 0
        cgross = 0
        dol    = ""
        ll = 0.
    end.
  end.
  if not(checks-avail) then put "No checks found ready to print!".
end.
