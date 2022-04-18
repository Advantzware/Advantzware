/* ap-ckind.p  Check print for Indiana Laser */
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
     "Check Date: " AT 1 ap-pay.check-date
     "Check No: " AT 25 ap-pay.check-no FORMAT ">>>>>>>9"
     "Net Check Amount" to 67
     ctot               to 79
    with frame b3 no-box no-labels stream-io no-attr-space.

form skip(8)
     dol                at 10
     SKIP(2)
     ap-pay.check-date  to 60
     ctot               to 79 format "**,***,**9.99"
     skip(1)
     vend.remit         at 10
     add1               at 10
     add2               at 10
     csz                at 10
     SKIP(1)
    /* "Memo:"            to 8
     vend.check-memo  */
     skip(3)  /*1*/                             
    with frame b1 width 85 no-box no-labels stream-io no-attr-space.
        
if v-print-mode ne "ALIGN" then do:         /* production mode */
  for each ap-pay  NO-LOCK
      where ap-pay.company   eq cocode
        and ap-pay.vend-no   ge wvend-no
        and ap-pay.vend-no   le evend-no
        and ap-pay.check-no  GE iBeginCheckNo
        and ap-pay.check-no  LE iEndCheckNo
        AND ap-pay.check-date EQ wdate
        AND ap-pay.bank-code EQ x-bank
        and ap-pay.man-check eq no
         ,           
      first vend
      where vend.company eq cocode
        and vend.vend-no eq ap-pay.vend-no
        
      break by (if v-sort-name then vend.name else "")
            by ap-pay.vend-no:
      
   ll = 0.   
   
     if length(vend.r-zip) gt 5 then
       csz = vend.r-city + ", " + vend.r-state + " " +
             substr(vend.r-zip,1,5) + "-" + substr(vend.r-zip,6,4).
     ELSE IF vend.r-state NE ""  OR vend.r-zip NE "" THEN
       csz = vend.r-city + ", " + vend.r-state + " " + vend.r-zip.

     assign
      add1 = vend.r-add1
      add2 = vend.r-add2.
      
   assign     
    v-vend-no         = vend.vend-no
    v-vend-name       = vend.name.

   if add2 eq " " then
     assign
      add2 = csz
      csz  = "".
    
   ll = 0.
   
   for each ap-payl NO-LOCK
       where ap-payl.check-no   eq ap-pay.check-no   
         and ap-payl.vend-no   eq ap-pay.vend-no
         and ap-payl.man-check eq no
         
       break by ap-payl.inv-no:  
    
    /************* print check stub at top of form ***************/
    find first ap-inv NO-LOCK
        where ap-inv.company eq cocode
          and ap-inv.vend-no eq ap-payl.vend-no
          and ap-inv.inv-no  eq ap-payl.inv-no
        use-index inv-no.
      
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
     ctot              = ctot + ap-payl.amt-paid
     cdis              = cdis + ap-payl.amt-disc      
     cgrossl           = cgrossl + ap-payl.amt-paid /*+ ap-sel.disc-amt) */
     cgross            = cgross + cgrossl.

    create wrk-chk.
    assign
     wrk-chk.inv-no   = ap-payl.inv-no
     wrk-chk.po-no    = ap-inv.po-no
     wrk-chk.inv-date = ap-inv.inv-date
     wrk-chk.inv-amt  = if ap-inv.gross gt 0 then ap-inv.gross else ap-inv.net
     wrk-chk.amt-paid = ap-payl.amt-paid
     wrk-chk.disc-amt = ap-payl.amt-disc
     wrk-chk.line-amt = cgrossl.

    put wrk-chk.inv-no                to 12 format "x(12)"
        trim(string(wrk-chk.po-no,">>>>>>>>"))
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
    
    if ll eq max-per-chk or last(ap-payl.inv-no) then do:
      if last(ap-payl.inv-no) then do:
        checks-avail = yes.

        display ap-pay.check-date ap-pay.check-no ctot with frame b3.
        
        PUT skip(max-per-chk - ll ).

        run ap/apchks.p (input ctot, input 70, output dol).

        dol = trim(dol) + fill("*",70) .                                    
    
        display caps(dol)         @ dol
                ap-pay.check-date
                ctot
                caps(vend.remit)  @ vend.remit
                caps(add1)        @ add1
                caps(add2)        @ add2
                caps(csz)         @ csz
                /*vend.check-memo*/
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
                skip(9)
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
            trim(string(wrk-chk.po-no,">>>>>>>>"))
                                          to 25 format "x(12)"
            wrk-chk.inv-date              to 34 format "99/99/99"
            wrk-chk.inv-amt               to 45 format "->>,>>9.99"
            wrk-chk.amt-paid              to 56 format "->>,>>9.99"
            wrk-chk.disc-amt              to 67 format "->>,>>9.99"
            wrk-chk.line-amt              to 79 format "->>>,>>9.99".

        delete wrk-chk.
        lv-line-cnt = lv-line-cnt + 1.
      end.

      if last(ap-payl.inv-no) then do: 
        display ap-pay.check-date ap-pay.check-no ctot with frame b3.
               /*max-per-chk = 12 */
        put skip(12 - lv-line-cnt ).
        
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

