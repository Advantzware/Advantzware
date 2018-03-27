/* ap-ckadv.p  Check print for Advantzware - Julie New printer */
{sys/inc/var.i SHARED}

{ap/ap-chk.i}
DEF VAR lv-line-cnt AS INT NO-UNDO.
DEF VAR ll-void AS LOG NO-UNDO.
DEF VAR lv-date AS CHAR FORMAT "x(9)" NO-UNDO.

DEF WORKFILE wrk-chk
  FIELD inv-no      LIKE ap-sel.inv-no
  FIELD po-no       LIKE ap-inv.po-no
  FIELD inv-date    AS   CHAR FORMAT "x(9)"
  FIELD inv-amt     LIKE ap-inv.net
  FIELD amt-paid    LIKE ap-sel.amt-paid
  FIELD disc-amt    LIKE ap-sel.disc-amt
  FIELD line-amt    AS   DEC FORMAT "->>>,>>9.99".

FORM 
     "-----------"      TO 79    SKIP
     "Check Date: "     AT 5
     lv-date
     "Net Check Amount" TO 67
     ctot               TO 79    
    WITH FRAME b3 NO-BOX NO-LABELS STREAM-IO NO-ATTR-SPACE.

FORM SKIP(5)
     lv-date            TO 67         
     SKIP(1)              
     ctot               TO 69    FORMAT "$->>>,>>9.99"
     SKIP(2)
     dol                AT 6
     SKIP(1)
     vend.remit         AT 6
     add1               AT 6
     add2               AT 6
     csz                AT 6    
     SKIP(4)
    WITH FRAME b1 WIDTH 85 NO-BOX NO-LABELS STREAM-IO NO-ATTR-SPACE.

IF v-print-mode NE "ALIGN" THEN DO:         /* production mode */
  FOR EACH ap-chk
      WHERE ap-chk.company   EQ cocode
        AND ap-chk.vend-no   GE wvend-no
        AND ap-chk.vend-no   LE evend-no
        AND ap-chk.man-check EQ NO
        AND CAN-FIND(FIRST ap-sel
                     WHERE ap-sel.company   EQ cocode
                       AND ap-sel.vend-no   EQ ap-chk.vend-no
                       AND ap-sel.man-check EQ NO),
        
      FIRST vend
      WHERE vend.company eq cocode
        AND vend.vend-no eq ap-chk.vend-no
        
      BREAK BY (IF v-sort-name THEN vend.name ELSE "")
            by ap-chk.vend-no:
      
   ll = 0.   
   FOR EACH ap-sel
       WHERE ap-sel.company   EQ cocode
         AND ap-sel.vend-no   EQ ap-chk.vend-no
         AND ap-sel.man-check EQ NO
       NO-LOCK
       
       BREAK by ap-sel.inv-no:
       
     ll = ll + 1.
      
     IF ll EQ max-per-chk AND NOT LAST(ap-sel.inv-no) THEN
       ASSIGN
        stnum = stnum + 1
        ll    = 0.
   END.
   
   IF r-add1 EQ " " THEN DO:   /*if no remit-to address*/
     IF LENGTH(vend.r-zip) GT 5 THEN
       csz = vend.city + ", " + vend.state + " " +
             SUBSTR(vend.zip,1,5) + "-" + SUBSTR(vend.zip,6,4).
     ELSE
       csz = vend.city + ", " + vend.state + " " + vend.zip.
        
     ASSIGN
      add1 = vend.add1
      add2 = vend.add2.
   END.
   
   ELSE DO: /*if a remit-to address exists  GEH */
     IF LENGTH(vend.r-zip) GT 5 THEN
       csz = vend.r-city + ", " + vend.r-state + " " +
             SUBSTR(vend.r-zip,1,5) + "-" + SUBSTR(vend.r-zip,6,4).
     ELSE
       csz = vend.r-city + ", " + vend.r-state + " " + vend.r-zip.

     ASSIGN
      add1 = vend.r-add1
      add2 = vend.r-add2.
   END.
   
   ASSIGN
    ap-chk.check-date = wdate
    ap-chk.check-no   = stnum
    ap-chk.bank-code  = x-bank
    v-vend-no         = vend.vend-no
    v-vend-name       = vend.name.

   RUN custom/eurodate.p (ap-chk.check-date, OUTPUT lv-date).

   IF add2 EQ " " THEN
     ASSIGN
      add2 = csz
      csz  = "".
    
   ll = 0.
   
   FOR EACH ap-sel
       WHERE ap-sel.company   EQ cocode
         AND ap-sel.vend-no   EQ ap-chk.vend-no
         AND ap-sel.man-check EQ NO
         
       BREAK BY ap-sel.inv-no:  
    
    /************* print check stub at top of form ***************/
    FIND FIRST ap-inv
        WHERE ap-inv.company EQ cocode
          AND ap-inv.vend-no EQ ap-sel.vend-no
          AND ap-inv.inv-no  EQ ap-sel.inv-no
        USE-INDEX inv-no.

    ap-inv.pay-date = wdate.

    RUN custom/eurodate.p (wdate, OUTPUT lv-date).

    IF ll EQ 0 THEN DO:
      PAGE.

      DISPLAY v-vend-name   AT 27
              lv-date       AT 65
              SKIP(1)
   
          WITH NO-LABELS STREAM-IO NO-BOX FRAME xyz.

      /*DISPLAY "Vendor ID: "
              v-vend-no
              SPACE(8)
              "Vendor Name: "
              v-vend-name
              SKIP(1)
   
          WITH NO-LABELS STREAM-IO NO-BOX FRAME xyz.

      PUT "Invoice No."     AT 2
          "Reference"       AT 15
          "Date"            AT 28
          "Inv Amt"         AT 37
          "Amt Paid"        AT 48
          "Disc Taken"      AT 58
          "Net Amt"         AT 71
          "============"    AT 1
          "==========="     AT 14
          "========="       AT 26
          "=========="      AT 36
          "=========="      AT 47
          "=========="      AT 58
          "==========="     AT 69.*/
    END.

    ASSIGN
     ctot              = ctot + ap-sel.amt-paid
     cdis              = cdis + ap-sel.disc-amt
     ap-sel.check-date = wdate
     cgrossl           = cgrossl + ap-sel.amt-paid
     cgross            = cgross + cgrossl.

    CREATE wrk-chk.
    ASSIGN
     wrk-chk.inv-no   = ap-sel.inv-no
     wrk-chk.po-no    = ap-inv.po-no
     wrk-chk.inv-amt  = IF ap-inv.gross GT 0 THEN ap-inv.gross ELSE ap-inv.net
     wrk-chk.amt-paid = ap-sel.amt-paid
     wrk-chk.disc-amt = ap-sel.disc-amt
     wrk-chk.line-amt = cgrossl.

    RUN custom/eurodate.p (ap-inv.inv-date, OUTPUT wrk-chk.inv-date).

    /*PUT wrk-chk.inv-no                TO 12 FORMAT "x(12)"
        TRIM(STRING(wrk-chk.po-no,">>>>>>>>"))
                                      TO 25 FORMAT "x(11)"
        wrk-chk.inv-date              TO 34 FORMAT "x(9)"
        wrk-chk.inv-amt               TO 45 FORMAT "->>,>>9.99"
        wrk-chk.amt-paid              TO 56 FORMAT "->>,>>9.99"
        wrk-chk.disc-amt              TO 67 FORMAT "->>,>>9.99"
        wrk-chk.line-amt              TO 79 FORMAT "->>>,>>9.99".*/

    PUT wrk-chk.inv-no                TO 13 FORMAT "x(12)"
        wrk-chk.inv-date              TO 23 FORMAT "x(9)"
        wrk-chk.amt-paid +
          wrk-chk.disc-amt            TO 38 FORMAT "->>>,>>9.99"
        wrk-chk.disc-amt              TO 54 FORMAT "->>>,>>9.99"
        wrk-chk.amt-paid              TO 70 FORMAT "->>>,>>9.99".

    ASSIGN
     ll      = ll + 1
     cgrossl = 0
     ll-void = NO.

    IF ll EQ max-per-chk OR LAST(ap-sel.inv-no) THEN DO:
      PUT SKIP(max-per-chk - ll).

      IF LAST(ap-sel.inv-no) THEN DO:
        checks-avail = YES.

        /*DISPLAY lv-date ctot WITH FRAME b3.*/
        
        PUT SKIP(1)
            ctot + cdis TO 38 FORMAT "->>>,>>9.99"
            cdis        TO 54 FORMAT "->>>,>>9.99"
            ctot        TO 70 FORMAT "->>>,>>9.99".

        RUN ap/apchks.p (INPUT ctot, INPUT 70, OUTPUT dol).

        dol = TRIM(dol) + fill("*",70) .                                    
    
        DISPLAY lv-date
                ctot
                CAPS(dol)         @ dol
                CAPS(vend.remit)  @ vend.remit
                CAPS(add1)        @ add1
                CAPS(add2)        @ add2
                CAPS(csz)         @ csz
            WITH FRAME b1.
      END.
     
      ELSE DO:
           DISPLAY SKIP(12)
                "V   V      OOO       III      DDDD"   AT 8 SKIP
                "V   V     O   O       I       D   D"  AT 8 SKIP
                "V   V     O   O       I       D   D"  AT 8 SKIP
                " V V      O   O       I       D   D"  AT 8 SKIP
                " V V      O   O       I       D   D"  AT 8 SKIP
                "  V        OOO       III      DDDD "  AT 8
                SKIP(23)
                WITH frame u NO-BOX NO-LABELS STREAM-IO NO-ATTR-SPACE.
            ll-void = YES.
      END.

      /*DISPLAY SKIP(2)
              "Vendor ID: "
              v-vend-no
              SPACE(8)
              "Vendor Name: "
              v-vend-name
              SKIP(1)
          WITH NO-LABELS STREAM-IO NO-BOX FRAME abc1.

      PUT "Invoice No."     AT 2
          "Reference"       AT 15
          "Date"            AT 28
          "Inv Amt"         AT 37
          "Amt Paid"        AT 48
          "Disc Taken"      AT 58
          "Net Amt"         AT 71
          "============"    AT 1
          "==========="     AT 14
          "========="       AT 26
          "=========="      AT 36
          "=========="      AT 47
          "=========="      AT 58
          "==========="     AT 69.*/
      
      lv-line-cnt = 0.
      FOR EACH wrk-chk:
        /*PUT wrk-chk.inv-no                TO 12 FORMAT "x(12)"
            TRIM(STRING(wrk-chk.po-no,">>>>>>>>"))
                                          TO 24 FORMAT "x(11)"
            wrk-chk.inv-date              TO 34 FORMAT "x(9)"
            wrk-chk.inv-amt               TO 45 FORMAT "->>,>>9.99"
            wrk-chk.amt-paid              TO 56 FORMAT "->>,>>9.99"
            wrk-chk.disc-amt              TO 67 FORMAT "->>,>>9.99"
            wrk-chk.line-amt              TO 79 FORMAT "->>>,>>9.99".*/

        DELETE wrk-chk.
        lv-line-cnt = lv-line-cnt + 1.
      END.

      IF LAST(ap-sel.inv-no) THEN DO:
        PUT SKIP(4)
            SPACE(5)
            "Account Name: "
            v-vend-name
            SPACE(3)
            "Cheque Date: "
            lv-date
            SKIP(1)
            "Reference #"   AT 3 SKIP
            ap-chk.check-no AT 3
            SKIP(12).

        /*DISPLAY lv-date ctot WITH FRAME b3.

        PUT SKIP(12 - lv-line-cnt ).*/

        ASSIGN
         stnum  = stnum + 1
         ctot   = 0
         cdis   = 0
         cgross = 0
         dol    = "".
      END.

      ll = 0.
    END.
  END.
 END.

 IF NOT checks-avail THEN PUT "No checks found ready to print!".
END.

