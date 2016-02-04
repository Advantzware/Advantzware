/* Action Laser check format ap\chkaction.p*/

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

form skip(8)
     ap-chk.check-date  to 60
     ctot               to 79 format "**,***,**9.99"
     "MM/DD/YYYY"       TO 60
     dol                at 10
     SKIP(1)
     "Memo:"            to 59
     vend.check-memo 
     vend.remit         at 10
     add1               at 10
     add2               at 10
     csz                at 10
     skip(2)
    with frame b1 width 85 no-box no-labels stream-io no-attr-space.

if v-print-mode ne "ALIGN" then do: /* production mode */
   for each ap-chk EXCLUSIVE-LOCK
       where ap-chk.company   eq cocode
         and ap-chk.vend-no   ge wvend-no
         and ap-chk.vend-no   le evend-no
         and ap-chk.man-check eq no
         and can-find(first ap-sel
                      where ap-sel.company   eq cocode
                        and ap-sel.vend-no   eq ap-chk.vend-no
                        and ap-sel.man-check eq no),
         
       first vend NO-LOCK
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
       
       if r-add1 eq " " then do:   /*if no remit-to address*/
          if length(vend.r-zip) gt 5 then
            csz = vend.city + ", " + vend.state + " " +
                  substr(vend.zip,1,5) + "-" + substr(vend.zip,6,4).
          else
            csz = vend.city + ", " + vend.state + " " + vend.zip.
         
          assign
           add1 = vend.add1
           add2 = vend.add2.
       end.
       
       else do: /*if a remit-to address exists*/
          if length(vend.r-zip) gt 5 then
             csz = vend.r-city + ", " + vend.r-state + " " +
                   substr(vend.r-zip,1,5) + "-" + substr(vend.r-zip,6,4).
          else
             csz = vend.r-city + ", " + vend.r-state + " " + vend.r-zip.
         
          assign
             add1 = vend.r-add1
             add2 = vend.r-add2.
       end.
       
       assign
          ap-chk.check-date = wdate
          ap-chk.check-no   = stnum
          ap-chk.bank-code  = x-bank
          v-vend-no         = vend.vend-no
          v-vend-name       = vend.name.
      
       if add2 eq " " then
          assign
             add2 = csz
             csz  = "".
        
       ll = 0.
       
       for each ap-sel EXCLUSIVE-LOCK
           where ap-sel.company   eq cocode
             and ap-sel.vend-no   eq ap-chk.vend-no
             and ap-sel.man-check eq no
             
           break by ap-sel.inv-no:  
        
           /************* print check stub at top of form ***************/
           find first ap-inv EXCLUSIVE-LOCK
                where ap-inv.company eq cocode
                  and ap-inv.vend-no eq ap-sel.vend-no
                  and ap-inv.inv-no  eq ap-sel.inv-no
                use-index inv-no.
          
           ap-inv.pay-date = wdate.
          
           FIND CURRENT ap-inv NO-LOCK.
          
           if ll eq 0 then do:
              page.
              
              display "Vendor ID: "
                      v-vend-no
                      space(8)
                      "Vendor Name: "
                      v-vend-name
                      skip
                      "Check No.: "
                      trim(string(ap-chk.check-no,">>>>>>>>>>")) format "x(10)"
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
           end. /* ll eq 0 */
          
           create wrk-chk.
           assign
             ctot              = ctot + ap-sel.amt-paid
             cdis              = cdis + ap-sel.disc-amt
             ap-sel.check-date = wdate
             cgrossl           = cgrossl + ap-sel.amt-paid
             cgross            = cgross + cgrossl
             wrk-chk.inv-no   = ap-sel.inv-no
             wrk-chk.po-no    = ap-inv.po-no
             wrk-chk.inv-date = ap-inv.inv-date
             wrk-chk.inv-amt  = if ap-inv.gross gt 0 then ap-inv.gross
                                else ap-inv.net
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
             ll-void = NO.
           
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
                         vend.check-memo
                         caps(vend.remit)  @ vend.remit
                         caps(add1)        @ add1
                         caps(add2)        @ add2
                         caps(csz)         @ csz
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
              display skip(4)
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
                        
                 put skip(13 - lv-line-cnt).
                
                 assign
                  stnum  = stnum + 1
                  ctot   = 0
                  cdis   = 0
                  cgross = 0
                  dol    = "".
              end. 
              ELSE IF ll-void THEN
                 PUT SKIP(15 - lv-line-cnt).
             
              ll = 0.
           end. /*ll eq max-per-chk or last(ap-sel.inv-no)*/
       end. /* each ap-sel*/
 end. /*each ap-chk*/

 if not checks-avail then put "No checks found ready to print!".
end.

