
{sys/inc/var.i shared}
{sys/form/s-top.f}

{ap/ap-chk.i}
DEF VAR v-check-memo AS cha NO-UNDO.

form /*company.name       at 6*/
     cgross             to 55
     cdis               to 67 format "->>,>>9.99"
     ctot               to 80
     skip(1)     
    with frame b3 width 85 no-box no-labels stream-io no-attr-space.

form skip(11)
     ap-chk.check-date  to 60
     ctot               to 79 format "**,***,**9.99"
     dol                at 10          
     skip(1)
     vend.remit         at 10
     add1               at 10
     add2               at 10
     csz                at 10
     skip(5)
    with frame b1 width 85 no-box no-labels stream-io no-attr-space.
    

find first company where company.company eq cocode no-lock.    

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
      
      display v-vend-name                               at 6
              ap-chk.check-date                         at 44
              /*"Check #:"                                at 63
              trim(string(ap-chk.check-no,">>>>>>"))    format "x(6)"*/
              skip(1)

          with no-labels stream-io no-box frame xyz.

      put "Date"            to 8
          "Invoice #"       to 24
          "Memo"            to 36
          "Amount"          to 57
          "Discount"        to 67
          "Net"             to 80.
      v-check-memo   = vend.check-memo.
    end.

    assign
     v-inv-no          = fill(" ",12 - length(trim(ap-sel.inv-no))) +
                         trim(ap-sel.inv-no)
     ctot              = ctot + ap-sel.amt-paid
     cdis              = cdis + ap-sel.disc-amt
     ap-sel.check-date = wdate
     cgrossl           = cgrossl + (ap-sel.amt-paid + ap-sel.disc-amt)
     cgross            = cgross + cgrossl
     .

    put ap-inv.inv-date to 10 format "99/99/99"
        v-inv-no        to 23 format "x(12)"
        v-check-memo TO 44 FORM "x(20)"
        cgrossl         to 57 format "->,>>>,>>9.99"
        ap-sel.disc-amt to 67 format "->>,>>9.99"
        ap-sel.amt-paid to 80 format "->,>>>,>>9.99"
        /*skip
        "Desc: "        at 6
        ap-sel.dscr*/
        skip.

    v-check-memo = "". /* print only one time */

    assign
     ll      = ll + 1
     cgrossl = 0.

    if ll eq max-per-chk and not last(ap-sel.inv-no) then do:
      display skip(14)
              "V   V      OOO       III      DDDD"   at 10 skip
              "V   V     O   O       I       D   D"  at 10 skip
              "V   V     O   O       I       D   D"  at 10 skip
              " V V      O   O       I       D   D"  at 10 skip
              " V V      O   O       I       D   D"  at 10 skip
              "  V        OOO       III      DDDD "  at 10
              skip(6)

          with frame u no-box no-labels stream-io no-attr-space.

      ll = 0.
    end.

    else
    /********************** print check ****************************/
    if last(ap-sel.inv-no) then do:
      checks-avail = yes.

      put skip(max-per-chk - ll).
      
      display /*company.name*/ cgross cdis ctot with frame b3.
      
      run ap/apchks.p (input ctot, input 70, output dol).

      dol = trim(dol) + fill("*",70) .                                    
    
      display caps(dol)         @ dol
              ap-chk.check-date
              ctot
              caps(vend.remit)  @ vend.remit
              caps(add1)        @ add1
              caps(add2)        @ add2
              caps(csz)         @ csz

          with frame b1.

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

 if not checks-avail then put "No checks found ready to print!".
end.

