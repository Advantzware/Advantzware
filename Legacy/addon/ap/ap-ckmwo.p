{sys/inc/var.i shared}
{sys/form/s-top.f}

{ap/ap-chk.i}
DEF VAR v-check-memo AS cha NO-UNDO.

FORM cgross             TO 55
     cdis               TO 67 FORMAT "->>,>>9.99"
     ctot               TO 80   
    SKIP(1)
    WITH FRAME b3 WIDTH 85 NO-BOX NO-LABELS STREAM-IO NO-ATTR-SPACE.

FORM SKIP(10)
     ap-chk.check-date  TO 60
     ctot               TO 79 FORMAT "**,***,**9.99"
     dol                AT 10          
     SKIP(1)
     vend.remit         AT 10
     add1               AT 10
     add2               AT 10
     csz                AT 10
     SKIP(4)
    WITH FRAME b1 WIDTH 85 NO-BOX NO-LABELS STREAM-IO NO-ATTR-SPACE.
    

FIND FIRST company WHERE company.company EQ cocode NO-LOCK.    

IF v-print-mode NE "ALIGN" THEN DO:         /* production mode */

  FOR EACH ap-chk
     WHERE ap-chk.company   EQ cocode
       AND ap-chk.vend-no   GE wvend-no
       AND ap-chk.vend-no   LE evend-no
       AND ap-chk.man-check EQ NO 
       AND can-find(FIRST ap-sel 
                          WHERE ap-sel.company  EQ cocode
                           AND ap-sel.vend-no   EQ ap-chk.vend-no
                           AND ap-sel.man-check EQ NO),
    FIRST vend 
     WHERE vend.company EQ cocode
       AND vend.vend-no EQ ap-chk.vend-no
    BREAK BY (IF v-sort-name THEN vend.name ELSE "")
          BY ap-chk.vend-no:

    ASSIGN ll = 0.   

    FOR EACH ap-sel no-lock
      WHERE ap-sel.company   EQ cocode
        and ap-sel.vend-no   EQ ap-chk.vend-no
        AND ap-sel.man-check eq NO 
      BREAK BY ap-sel.inv-no:

      ASSIGN ll = ll + 1.

      IF ll EQ max-per-chk AND 
         NOT LAST(ap-sel.inv-no) 
        THEN ASSIGN stnum = stnum + 1
                    ll    = 0.
    END.

    ASSIGN add1   = vend.r-add1
           add2   = vend.r-add2
           csz    = vend.r-city + ", " + vend.r-state
           lv-zip = vend.r-zip.

    IF add1 EQ "" AND 
       add2 EQ "" 
      THEN ASSIGN add1   = vend.add1
                  add2   = vend.add2
                  csz    = vend.city + ", " + vend.state
                  lv-zip = vend.zip.

    IF lv-zip BEGINS "00000" THEN lv-zip = "".

    ASSIGN csz = TRIM(csz) + "  " +
                 SUBSTR(lv-zip,1,5) +
                 (IF LENGTH(lv-zip) GT 5 THEN 
                    ("-" + SUBSTR(lv-zip,6,4)) ELSE "").

    IF TRIM(csz) EQ "," THEN csz = "".

    ASSIGN ap-chk.check-date = wdate
           ap-chk.check-no   = stnum
           ap-chk.bank-code  = x-bank
           v-vend-no         = vend.vend-no
           v-vend-name       = vend.name.

   IF add1 EQ "" THEN ASSIGN add1 = add2
                             add2 = "".

   IF add2 EQ "" THEN ASSIGN add2 = csz
                             csz  = "".
    
   ASSIGN ll = 0.

   FOR EACH ap-sel
      WHERE ap-sel.company  EQ cocode
       AND ap-sel.vend-no   EQ ap-chk.vend-no
       AND ap-sel.man-check EQ NO 
      BREAK BY ap-sel.inv-no:  


     /************* print check stub at top of form ***************/
     FIND FIRST ap-inv
        WHERE ap-inv.company eq cocode
          AND ap-inv.vend-no eq ap-sel.vend-no
          AND ap-inv.inv-no  eq ap-sel.inv-no USE-INDEX inv-no.

     ASSIGN ap-inv.pay-date = wdate.

     IF ll eq 0 THEN do:

       PAGE.
      
      DISPLAY v-vend-name       AT 6
              ap-chk.check-date AT 44 
             SKIP (1)
           WITH NO-LABELS STREAM-IO NO-BOX FRAME xyz.

      PUT "Date"            TO 8
          "Invoice #"       TO 24
          "Memo"            TO 36
          "Amount"          TO 57
          "Discount"        TO 67
          "Net"             TO 80.

      ASSIGN v-check-memo   = vend.check-memo.

     END.
     
     ASSIGN v-inv-no          = FILL(" ",12 - LENGTH(TRIM(ap-sel.inv-no))) +
                                TRIM(ap-sel.inv-no)
            ctot              = ctot + ap-sel.amt-paid
            cdis              = cdis + ap-sel.disc-amt
            ap-sel.check-date = wdate
            cgrossl           = cgrossl + (ap-sel.amt-paid + ap-sel.disc-amt)
            cgross            = cgross + cgrossl.

     PUT ap-inv.inv-date TO 10 FORMAT "99/99/99"
         v-inv-no        TO 23 FORMAT "x(12)"
         v-check-memo    TO 44 FORMAT "x(20)"
         cgrossl         TO 57 FORMAT "->,>>>,>>9.99"
         ap-sel.disc-amt TO 67 FORMAT "->>,>>9.99"
         ap-sel.amt-paid TO 80 FORMAT "->,>>>,>>9.99"
        SKIP.

     ASSIGN v-check-memo = "". /* print only one time */

     ASSIGN ll      = ll + 1
            cgrossl = 0.

     IF ll EQ max-per-chk AND 
        not LAST(ap-sel.inv-no) THEN DO:

       DISPLAY SKIP(11)               
              "V   V      OOO       III      DDDD"   AT 10 SKIP 
              "V   V     O   O       I       D   D"  AT 10 SKIP 
              "V   V     O   O       I       D   D"  AT 10 SKIP 
              " V V      O   O       I       D   D"  AT 10 SKIP 
              " V V      O   O       I       D   D"  AT 10 SKIP 
              "  V        OOO       III      DDDD "  AT 10
            SKIP(7)
         WITH FRAME u NO-BOX NO-LABELS STREAM-IO NO-ATTR-SPACE.

       ASSIGN ll = 0.
     END.
     ELSE /********************** print check ****************************/
     IF last(ap-sel.inv-no) THEN DO:

       ASSIGN checks-avail = YES.

       PUT SKIP(max-per-chk - ll).

       DISPLAY cgross cdis ctot WITH FRAME b3.

       RUN ap/apchks.p (INPUT ctot, INPUT 70, OUTPUT dol).

       ASSIGN dol = TRIM(dol) + FILL("*",70).

       DISPLAY CAPS(dol)         @ dol
               ap-chk.check-date
               ctot
               CAPS(vend.remit)  @ vend.remit
               CAPS(add1)        @ add1
               CAPS(add2)        @ add2
               CAPS(csz)         @ csz
           WITH FRAME b1.

       ASSIGN stnum  = stnum + 1
              ctot   = 0
              cdis   = 0
              cgross = 0
              dol    = ""
              ll     = 0.
     END.
   END.
  END.
  
  IF NOT checks-avail THEN PUT "No checks found ready to print!".

END.

