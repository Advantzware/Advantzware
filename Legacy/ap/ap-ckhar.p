
{sys/inc/var.i shared}
{sys/form/s-top.f}

{ap/ap-chk.i}

def workfile wrk-chk
  field inv-no      like ap-sel.inv-no
  field po-no       like ap-inv.po-no
  field inv-date    like ap-inv.inv-date
  field inv-amt     like ap-inv.net
  field amt-paid    like ap-sel.amt-paid
  field disc-amt    like ap-sel.disc-amt
  field line-amt    as   dec format "->>>,>>9.99".

form "-----------"      to 79    skip
     "Net Check Amount" to 67
     ctot               to 79

    with frame b3 no-box no-labels stream-io no-attr-space.

form 
     skip(7)
     ap-chk.check-date  to 60
     ctot               to 79 format "**,***,**9.99"
     skip                      
 
     dol                at 10          
     skip(3)
     vend.remit         at 10
     add1               at 10
     add2               at 10
     csz                at 10
     skip(1)
     "Memo:"            to 8
     vend.check-memo 
     skip(1)                              

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
      
      display skip(2)
              "Vendor ID: "
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
     cgrossl           = cgrossl + (ap-sel.amt-paid + ap-sel.disc-amt)
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
     cgrossl = 0.

    if ll eq max-per-chk or last(ap-sel.inv-no) then do:
      if last(ap-sel.inv-no) then do:
        checks-avail = yes.

        display ctot with frame b3.
        put skip(max-per-chk - ll).

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
      end.
      
      else
        display skip(9)
                "V   V      OOO       III      DDDD"   at 10 skip
                "V   V     O   O       I       D   D"  at 10 skip
                "V   V     O   O       I       D   D"  at 10 skip
                " V V      O   O       I       D   D"  at 10 skip
                " V V      O   O       I       D   D"  at 10 skip
                "  V        OOO       III      DDDD "  at 10
                skip(6)

            with frame u no-box no-labels stream-io no-attr-space.

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
      end.
      
      if last(ap-sel.inv-no) then do:
        display ctot with frame b3.

        assign
         stnum  = stnum + 1
         ctot   = 0
         cdis   = 0
         cgross = 0
         dol    = "".
      end.
      
      ll = 0.
    end.
  end.
 end.

 if not checks-avail then put "No checks found ready to print!".
end.

