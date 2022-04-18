/***************************************************************************\
*****************************************************************************
**  Program: /u2/fold/all/test/asi/ap/ap
**       By: Chris Heins
** Descript: Check format for P&P Packaging.
**
*****************************************************************************
\***************************************************************************/

{sys/inc/var.i shared}
{sys/form/s-top.f}

{ap/ap-chk.i}

form
  skip(3)
  ap-pay.check-no to 6 ap-pay.check-date FORM "99/99/99" to 15
  vend.vend-no to 24 skip(5)
  dol at 1 skip
  ctot to 75 format "*,***,**9.99" skip(2)
  vend.remit at 10
  add1       at 10
  add2       at 10
  csz        at 10
  skip(4)
  "Memo:"    to 8
  vend.check-memo 
  skip(2)  
  with frame b1 width 85 no-box no-labels stream-io no-attr-space.

if v-print-mode ne "ALIGN" then do:  /* production mode */
  for each ap-pay NO-LOCK
      where ap-pay.company   eq cocode
        and ap-pay.vend-no   ge wvend-no
        and ap-pay.vend-no   le evend-no
        and ap-pay.check-no  GE iBeginCheckNo
        and ap-pay.check-no  LE iEndCheckNo
        AND ap-pay.check-date EQ wdate
        AND ap-pay.bank-code EQ x-bank
        and ap-pay.man-check eq no,
        
      first vend NO-LOCK
      where vend.company eq cocode
        and vend.vend-no eq ap-pay.vend-no,
        
      each ap-payl NO-LOCK
      where ap-payl.check-no  eq ap-pay.check-no
        and ap-payl.vend-no   eq ap-pay.vend-no
        and ap-payl.man-check eq no

      break by (if v-sort-name then vend.name else "")
            by ap-pay.vend-no
            by ap-payl.vend-no
            by ap-payl.inv-no:

    IF FIRST-OF(ap-pay.vend-no) THEN DO:
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
      ap-inv.inv-no  eq ap-payl.inv-no and
      ap-inv.vend-no eq ap-payl.vend-no NO-LOCK.
    
    assign v-left = yes.
    if ll eq 0 then
      put skip(3).

    assign  ctot   = ctot + ap-payl.amt-paid
      cdis   = cdis + ap-payl.amt-disc      
      cgrossl = cgrossl + (ap-payl.amt-paid + ap-payl.amt-disc)
      cgross = cgross + cgrossl.


    put ap-payl.inv-no at 1
        ap-inv.inv-date FORM "99/99/99" to 24
        ap-inv.due to 38
        cgrossl to 52 format "->>>,>>9.99"
        ap-payl.amt-disc to 65.
    assign      ll = ll + 1.

    cgrossl = 0.
    if ll eq max-per-chk and v-left and not last-of(ap-payl.vend-no) then
    do:
      assign  stnum = stnum + 1
        ap-pay.check-no = stnum
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
    if last-of(ap-payl.vend-no) then
    do:
      checks-avail = yes.
      put skip(max-per-chk + 1 - ll).
      put stnum to 6 wdate to 15 v-vend-no to 24 format "x(8)"
        cgross to 52 cdis to 65 ctot to 80 skip.
      /* {ap/apchks.i} */
      run ap/apchks.p (input ctot, input 70, output dol).
      display
        ap-pay.check-no ap-pay.check-date ctot
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
