
{sys/inc/var.i shared}

{ap/ap-chk.i}
DEF VAR lv-line-cnt AS INT NO-UNDO.
DEF VAR ll-void AS LOG NO-UNDO.

def workfile wrk-chk
  field inv-no      like ap-sel.inv-no
  field po-no       like ap-inv.po-no
  field inv-date    like ap-inv.inv-date
  field inv-amt     like ap-inv.net
  field amt-paid    like ap-sel.amt-paid
  field disc-amt    like ap-sel.disc-amt
  field line-amt    as   dec format "->>>,>>9.99".

form 
     "-----------"      to 79    skip
     "Check Date: " AT 5 ap-chk.check-date
     "Net Check Amount" to 67
     ctot               to 79
    with frame b3 no-box no-labels stream-io no-attr-space.

form skip(9)
     ap-chk.check-date  to 60
     ctot               to 79 format "**,***,**9.99"
     skip(1)
     dol                at 10
     skip(1)
     vend.remit         at 10
     add1               at 10
     add2               at 10
     csz                at 10
     skip(1)
     "Memo:"            to 8
     vend.check-memo 
     skip(2)                              

    with frame b1 width 85 no-box no-labels stream-io no-attr-space.

if v-print-mode ne "ALIGN" then do:         /* production mode */
  for each ap-chk
      where ap-chk.company   eq cocode
        and ap-chk.vend-no   ge wvend-no
        and ap-chk.vend-no   le evend-no
        and ap-chk.man-check eq no
        and can-find(first ap-sel
                     where ap-sel.company   eq cocode
                       and ap-sel.vend-no   eq ap-chk.vend-no
                       and ap-sel.man-check eq no),
        
      first vend
      where vend.company eq cocode
        and vend.vend-no eq ap-chk.vend-no
        
      break by (if v-sort-name then vend.name else "")
            by ap-chk.vend-no:
      
   ll = 0.   
   for each ap-sel
       where ap-sel.company   eq cocode
         and ap-sel.vend-no   eq ap-chk.vend-no
         and ap-sel.man-check eq no
       no-lock
       
       break by ap-sel.inv-no:
       
     ll = ll + 1.
      
     if ll eq max-per-chk and not last(ap-sel.inv-no) then
       assign
        stnum = stnum + 1
        ll    = 0.
   end.
   
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
    
   ll = 0.
   
   for each ap-sel
       where ap-sel.company   eq cocode
         and ap-sel.vend-no   eq ap-chk.vend-no
         and ap-sel.man-check eq no
         
       break by ap-sel.inv-no:  
    
    /************* print check stub at top of form ***************/
    find first ap-inv
        where ap-inv.company eq cocode
          and ap-inv.vend-no eq ap-sel.vend-no
          and ap-inv.inv-no  eq ap-sel.inv-no
        use-index inv-no.

    ap-inv.pay-date = wdate.

    if ll eq 0 then do:
      page.
      
      display "Vendor ID: "
              v-vend-no
              space(8)
              "Vendor Name: "
              v-vend-name
              skip(1)
   
          with no-labels stream-io no-box frame xyz.

      put "Invoice No."     at 2
          "Reference"       at 16
          "Date"            at 29
          "Inv Amt"         at 37
          "Amt Paid"        at 48
          "Disc Taken"      at 58
          "Net Amt"         at 71
          "============"    at 1
          "============"    at 14
          "========"        at 27
          "=========="      at 36
          "=========="      at 47
          "=========="      at 58
          "==========="     at 69.
    end.

    assign
     ctot              = ctot + ap-sel.amt-paid
     cdis              = cdis + ap-sel.disc-amt
     ap-sel.check-date = wdate
     cgrossl           = cgrossl + ap-sel.amt-paid /*+ ap-sel.disc-amt) */
     cgross            = cgross + cgrossl.

    create wrk-chk.
    assign
     wrk-chk.inv-no   = ap-sel.inv-no
     wrk-chk.po-no    = ap-inv.po-no
     wrk-chk.inv-date = ap-inv.inv-date
     wrk-chk.inv-amt  = if ap-inv.gross gt 0 then ap-inv.gross else ap-inv.net
     wrk-chk.amt-paid = ap-sel.amt-paid
     wrk-chk.disc-amt = ap-sel.disc-amt
     wrk-chk.line-amt = cgrossl.

    put wrk-chk.inv-no                to 12 format "x(12)"
        trim(string(wrk-chk.po-no,">>>>>>"))
                                      to 25 format "x(12)"
        wrk-chk.inv-date              to 34 format "99/99/99"
        wrk-chk.inv-amt               to 45 format "->>,>>9.99"
        wrk-chk.amt-paid              to 56 format "->>,>>9.99"
        wrk-chk.disc-amt              to 67 format "->>,>>9.99"
        wrk-chk.line-amt              to 79 format "->>>,>>9.99".

    assign
     ll      = ll + 1
     cgrossl = 0
     ll-void = NO   
     .
    
    if ll eq max-per-chk or last(ap-sel.inv-no) then do:
      if last(ap-sel.inv-no) then do:
        checks-avail = yes.

        display ap-chk.check-date ctot with frame b3.
        
        put skip(max-per-chk - ll - 1).

        run ap/apchks.p (input ctot, input 70, output dol).

        dol = trim(dol) + fill("*",70) .                                    
    
        display caps(dol)         @ dol
                ap-chk.check-date
                ctot
                caps(vend.remit)  @ vend.remit
                caps(add1)        @ add1
                caps(add2)        @ add2
                caps(csz)         @ csz
                vend.check-memo
            with frame b1.
        PUT SKIP(1).
      end.
     
      else DO:
           display skip(9)
                "V   V      OOO       III      DDDD"   at 10 skip
                "V   V     O   O       I       D   D"  at 10 skip
                "V   V     O   O       I       D   D"  at 10 skip
                " V V      O   O       I       D   D"  at 10 skip
                " V V      O   O       I       D   D"  at 10 skip
                "  V        OOO       III      DDDD "  at 10
                skip(8)
                with frame u no-box no-labels stream-io no-attr-space.
            ll-void = YES.
      END.
      display skip(2)
              "Vendor ID: "
              v-vend-no
              space(8)
              "Vendor Name: "
              v-vend-name
              skip(1)
          with no-labels stream-io no-box frame abc1.

      put "Invoice No."     at 2
          "Reference"       at 16
          "Date"            at 29
          "Inv Amt"         at 37
          "Amt Paid"        at 48
          "Disc Taken"      at 58
          "Net Amt"         at 71
          "============"    at 1
          "============"    at 14
          "========"        at 27
          "=========="      at 36
          "=========="      at 47
          "=========="      at 58
          "==========="     at 69.
      
      lv-line-cnt = 0.
      for each wrk-chk:
        put wrk-chk.inv-no                to 12 format "x(12)"
            trim(string(wrk-chk.po-no,">>>>>>"))
                                          to 25 format "x(12)"
            wrk-chk.inv-date              to 34 format "99/99/99"
            wrk-chk.inv-amt               to 45 format "->>,>>9.99"
            wrk-chk.amt-paid              to 56 format "->>,>>9.99"
            wrk-chk.disc-amt              to 67 format "->>,>>9.99"
            wrk-chk.line-amt              to 79 format "->>>,>>9.99".

        delete wrk-chk.
        lv-line-cnt = lv-line-cnt + 1.
      end.

      if last(ap-sel.inv-no) then do: 
        display ap-chk.check-date ctot with frame b3.
               /*max-per-chk = 12 */
        put skip(13 - lv-line-cnt).

        assign
         stnum  = stnum + 1
         ctot   = 0
         cdis   = 0
         cgross = 0
         dol    = "".
      end. 
      ELSE IF ll-void THEN PUT SKIP(15 - lv-line-cnt) /* no total frame b3 */.

      ll = 0.
    end.
  end.
 end.

 if not checks-avail then put "No checks found ready to print!".
end.

/*
{sys/inc/var.i shared}
{sys/form/s-top.f}

{ap/ap-chk.i}

form
  skip(3)
  ap-chk.check-no to 10
  ap-chk.check-date FORMAT "99/99/99" to 19
  vend.vend-no to 29
  skip(6)
  dol at 4 format "x(60)"
  ctot to 79 format "***,***,**9.99"
  skip(1)
  vend.remit at 11
  add1       at 11
  add2       at 11
  csz        at 11
  skip(1)
  "Memo:"    to 8
  vend.check-memo 
  skip(4)
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

  if r-add1 ne " " then do:  /*if a remit-to address exists  GEH */
    if first-of(ap-chk.vend-no) then do:
       if length(vend.r-zip) > 5 then
         assign csz = vend.r-city + ", " + vend.r-state + " " +
                substr(vend.r-zip,1,5) + "-" + substr(vend.r-zip,6,4).
       else
         assign csz = vend.r-city + ", " + vend.r-state + " " + vend.r-zip.

       assign
              add1              = vend.r-add1
              add2              = vend.r-add2
              ap-chk.check-date = wdate
              ap-chk.check-no   = stnum
              ap-chk.bank-code  = x-bank
              v-vend-no         = vend.vend-no
              v-vend-name       = vend.name.
      if add2 eq " " then
            assign add2 = csz
                   csz = "".
      ll = 0.
    end.
  end.

  if r-add1 eq " " then do:   /*if no remit-to address*/
    if first-of(ap-chk.vend-no) then do:
       if length(vend.r-zip) > 5 then
         assign csz = vend.city + ", " + vend.state + " " +
                substr(vend.zip,1,5) + "-" + substr(vend.zip,6,4).
       else
         assign csz = vend.city + ", " + vend.state + " " + vend.zip.

       assign
        add1              = vend.add1
        add2              = vend.add2
        ap-chk.check-date = wdate
        ap-chk.check-no   = stnum
        ap-chk.bank-code  = x-bank
        v-vend-no         = vend.vend-no
        v-vend-name       = vend.name.
      if add2 eq "" then
        assign add2 = csz
               csz = "".
      ll = 0.
    end.  /* check form */
  end.

    /************* print check stub at top of form ***************/
    find first ap-inv use-index inv-no
      where ap-inv.company eq cocode and
      ap-inv.inv-no  eq ap-sel.inv-no and
      ap-inv.vend-no eq ap-sel.vend-no.
    if avail ap-inv then
      assign ap-inv.pay-date = wdate.

    assign v-left = yes.
    if ll eq 0 then
      display skip(1)
              v-vend-no to 17 format "x(8)"
              skip(2)
          with no-labels no-box frame xyz STREAM-IO.

    assign  ctot   = ctot + ap-sel.amt-paid
      cdis   = cdis + ap-sel.disc-amt
      ap-sel.check-date = wdate
      cgrossl = cgrossl + (ap-sel.amt-paid + ap-sel.disc-amt)
      cgross = cgross + cgrossl.

    put ap-sel.inv-no TO 18
        ap-inv.inv-date to 28
        ap-inv.due to 42
        cgrossl to 55 format "->>>,>>9.99"
        ap-sel.disc-amt to 68
        ap-sel.amt-paid to 83.
    assign      ll = ll + 1.

    cgrossl = 0.
    if ll eq max-per-chk and v-left and not last-of(ap-sel.vend-no) then
    do:
      assign  stnum = stnum + 1
        ap-chk.check-no = stnum
        ll = 0.
      display skip(12)
        "V   V      OOO       III      DDDD"   at 11 skip
        "V   V     O   O       I       D   D"  at 11 skip
        "V   V     O   O       I       D   D"  at 11 skip
        " V V      O   O       I       D   D"  at 11 skip
        " V V      O   O       I       D   D"  at 11 skip
        "  V        OOO       III      DDDD "  at 11 skip(6) /* DAR */
        with frame m no-box no-labels stream-io no-attr-space.
    end.

    /********************** print check ****************************/
    if last-of(ap-sel.vend-no) then
    do:
      checks-avail = yes.
      put skip(max-per-chk + 1 - ll).
      put stnum to 24 wdate to 33 v-vend-no to 42 format "x(8)"
        cgross to 55 cdis to 68 ctot to 83 skip.
      /* {ap/apchks.i} */
      run ap/apchks.p (input ctot, input 60, output dol).
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
*/
