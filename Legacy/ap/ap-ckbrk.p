/***************************************************************************\
*****************************************************************************
**  Program: ap\ap-ckbrk.p
**       By: Ed Wilson
** Descript: Brick format check.
**
*****************************************************************************
\***************************************************************************/

{sys/inc/var.i shared}
{sys/form/s-top.f}

{ap/ap-chk.i}

def workfile wrk-chk
        field inv-no like ap-sel.inv-no
        field line-amt as dec format "->>>,>>9.99"
        field disc-amt like ap-sel.disc-amt.

form
  ap-chk.check-date at 7
  "** Check Totals **" at 31
  ctot to 79
  with frame b3 no-box no-labels stream-io no-attr-space.

form                                                                                 
  skip(8)
  ap-chk.check-no to 6 ap-chk.check-date to 15
  vend.vend-no to 24 ctot to 77 format "*,***,**9.99" skip(2)
  dollar[1]  at 10
  dollar[2]  at 12 skip(1)
  vend.remit at 10
  add1       at 10
  add2       at 10
  csz        at 10
  skip(2)
  "Memo:"    to 8
  vend.check-memo 
  skip(2)
  with frame b2 width 85 no-box no-labels stream-io no-attr-space.

form
  skip(12)
  ap-chk.check-no to 53  ap-chk.check-date to 62
  ctot to 77 format "*,***,**9.99" skip(1)
  dol at 10 skip
  vend.remit at 10
  add1       at 10
  add2       at 10
  csz        at 10
  skip(1)
  "Memo:"    to 8
  vend.check-memo 
  skip(2)
  with frame b1 width 80 no-box no-labels stream-io no-attr-space.

form 
   skip(1)
   "Invoice Num."  at 1
   "Gross Amount"  at 14
   "Discount"      at 28
   "Invoice Num."  at 46
   "Gross Amount"  at 59
   "Discount"      at 73
   with frame stub-hd width 80 no-box no-labels stream-io no-attr-space.
 
find first sys-ctrl where sys-ctrl.company eq cocode and
                          sys-ctrl.name eq "CHKFMT" no-lock no-error.

if v-print-mode ne "ALIGN" then do: /* production mode */
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
        if laser-chk then do:
          page.
          
          if sys-ctrl.char-fld eq "Raritan" then do:
            display v-vend-no to 8 format "x(8)"
                    v-vend-name at 11 skip(2)
                with no-labels width 85 stream-io no-box frame xyz-raritan.
          end.
        
          else
            display skip(2)
                    v-vend-no to 8 format "x(8)"
                    v-vend-name at 11 skip(2)
                with no-labels width 85 stream-io no-box frame xyz-laser.
        end.
      
        else do:     
          display skip(2)
                  "VENDOR NUMBER: " at 1  v-vend-no   at 16 format "x(8)"
                  "NAME:"           at 25 v-vend-name at 31 format "x(29)"
                  "CHECK DATE:"     at 60 ap-chk.check-date at 72 skip
              with no-labels width 85 stream-io no-box frame xyz.
             
          display space(1) with no-labels stream-io no-box frame stub-hd.
        end.
      end.

      assign
       ctot   = ctot + ap-sel.amt-paid
       cdis   = cdis + ap-sel.disc-amt
       ap-sel.check-date = wdate
       cgrossl = cgrossl + (ap-sel.amt-paid + ap-sel.disc-amt)
       cgross = cgross + cgrossl.

      if laser-chk then do:
        create wrk-chk.
        assign
         wrk-chk.inv-no = ap-sel.inv-no
         wrk-chk.line-amt = cgrossl
         wrk-chk.disc-amt = ap-sel.disc-amt.
      end.

      if ll modulo 2 eq 0 then
        put ap-sel.inv-no to 12
            cgrossl to 25 format "->>>,>>9.99"
            ap-sel.disc-amt to 35.
      
      else
        put ap-sel.inv-no to 59
            cgrossl to 70 format "->>>,>>9.99"
            ap-sel.disc-amt to 80 skip.
      
      assign
       ll      = ll + 1
       cgrossl = 0.
       
      if ll eq max-per-chk and not last(ap-sel.inv-no) then do:
        display skip(12)
                "V   V      OOO       III      DDDD"   at 10 skip
                "V   V     O   O       I       D   D"  at 10 skip
                "V   V     O   O       I       D   D"  at 10 skip
                " V V      O   O       I       D   D"  at 10 skip
                " V V      O   O       I       D   D"  at 10 skip
                "  V        OOO       III      DDDD "  at 10 skip(8)
            with frame u no-box no-labels stream-io no-attr-space.

        if laser-chk then do:
          display skip(3)
                  v-vend-no to 8 format "x(8)"
                  v-vend-name at 11 skip(2)
              with no-labels stream-io no-box frame abc1.

          ll = 0.
          
          for each wrk-chk:
            if ll modulo 2 eq 0 then
              put wrk-chk.inv-no to 12
                  wrk-chk.line-amt to 24 format "->>>,>>9.99"
                  wrk-chk.disc-amt to 34.
            else
              put wrk-chk.inv-no to 59
                  wrk-chk.line-amt to 70 format "->>>,>>9.99"
                  wrk-chk.disc-amt to 80 skip.
                  
            ll = ll + 1.
          
            delete wrk-chk.
          end.
        end.
        
        page.
        
        ll = 0.
      end.

      else
      /********************** print check ****************************/
      if last(ap-sel.inv-no) then do:
        checks-avail = yes.
        
        put skip((max-per-chk / 2 + 2) - 
                 (trunc(ll / 2,0) + int(ll modulo 2 gt 0))).
        
        put stnum to 6 wdate to 15
            v-vend-no to 24 format "x(8)"
            cgross to 43
            cdis to 59
            ctot to 80
            skip.
            
        run ap/apchks.p (input ctot, input 70, output dol).

        display skip(1)
                ap-chk.check-no
                ap-chk.check-date
                ctot
                caps(dol)        @ dol
                caps(vend.remit) @ vend.remit
                caps(add1)       @ add1
                caps(add2)       @ add2
                caps(csz)        @ csz
                vend.check-memo
            with frame b1.

        if laser-chk then do:
          display skip(3)
                  v-vend-no to 8 format "x(8)"
                  v-vend-name at 11 skip(2)
              with no-labels stream-io no-box frame abc.

          ll = 0.
          
          for each wrk-chk:
            if ll modulo 2 eq 0 then
              put wrk-chk.inv-no to 12
                  wrk-chk.line-amt to 24 format "->>>,>>9.99"
                  wrk-chk.disc-amt to 34.
            else
              put wrk-chk.inv-no to 59
                  wrk-chk.line-amt to 70 format "->>>,>>9.99"
                  wrk-chk.disc-amt to 80 skip.
            
            ll = ll + 1.
            
            delete wrk-chk.
          end.  /* end for each wrk-chk  */

          put skip((max-per-chk / 2 + 2) -
                   (trunc(ll / 2,0) + int(ll modulo 2 gt 0))).
          
          put stnum to 6
              wdate to 15
              v-vend-no to 24 format "x(8)"
              cgross to 43
              cdis to 59
              ctot to 80.
        end.  /* end if laser */

        assign
         stnum  = stnum + 1
         ctot   = 0
         cdis   = 0
         cgross = 0
         dol    = ""
         ll     = 0.
      end.
    end.
  end.
  
  if not(checks-avail) then put "No checks found ready to print!".
end.

