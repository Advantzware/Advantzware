/* ------------------------------------------------- ap/ap-ckrud.p 12/99 FWK */
/* Check Format for S-8-CHKFMT = Rudd                                             */
/* ------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/s-top.f}

{ap/ap-chk.i}

form
  skip(4)
  ap-chk.check-no to 7 ap-chk.check-date  to 16 FORM "99/99/99"
  vend.vend-no to 27 skip(5)
  dol at 1 skip
  ctot to 75 format "*,***,**9.99" skip(2)
  vend.remit at 10
  add1       at 10
  add2       at 10
  csz        at 10
  skip(2)
  "Memo:"    to 8
  vend.check-memo 
  skip(2)
  with frame b1 width 85 no-box no-labels stream-io no-attr-space.

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
      ap-inv.inv-no  eq ap-sel.inv-no and
      ap-inv.vend-no eq ap-sel.vend-no.
    if avail ap-inv then
      assign ap-inv.pay-date = wdate.

    assign v-left = yes.
    if ll eq 0 then
      put skip(1) ap-inv.vend-no at 8 skip(2).
/*
      put skip(4).
*/

    assign  ctot   = ctot + ap-sel.amt-paid
      cdis   = cdis + ap-sel.disc-amt
      ap-sel.check-date = wdate
      cgrossl = cgrossl + (ap-sel.amt-paid + ap-sel.disc-amt)
      cgross = cgross + cgrossl.


    put trim(string(ap-inv.po-no)) at 1 format "x(8)"
        trim(ap-sel.inv-no) at 9 format "x(11)"
        ap-inv.inv-date to 25 FORM "99/99/99"
        ap-inv.due to 40
        cgrossl to 54 format "->>>,>>9.99"
        ap-sel.disc-amt to 66
        ap-sel.amt-paid to 80.
    assign      ll = ll + 1.

    cgrossl = 0.
    if ll eq max-per-chk and v-left and not last-of(ap-sel.vend-no) then
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
        "  V        OOO       III      DDDD "  at 10 skip(8) /* DAR */
        with frame m no-box no-labels stream-io no-attr-space.
    end.

    /********************** print check ****************************/
    if last-of(ap-sel.vend-no) then
    do:
      checks-avail = yes.
      put skip(max-per-chk + 1 - ll).
      put /* stnum to 6 wdate to 15 v-vend-no to 24 format "x(8)"
        cgross to 52 cdis to 65 */ ctot to 80 skip.
      /* {ap/apchks.i} */
      run ap/apchks.p (input ctot, input 70, output dol).
      display
        ap-chk.check-no ap-chk.check-date ctot
        vend.vend-no
        caps(dol)         @ dol
        caps(vend.remit)  @ vend.remit
        caps(add1)        @ add1
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
