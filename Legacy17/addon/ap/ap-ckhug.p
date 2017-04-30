/* ap/ap-ckhug.p  for Hughes Check, one stub */
{sys/inc/var.i SHARED}

{ap/ap-chk.i}

DEF WORKFILE wrk-chk
  FIELD inv-no      LIKE ap-sel.inv-no
  FIELD po-no       LIKE ap-inv.po-no
  FIELD inv-date    LIKE ap-inv.inv-date
  FIELD inv-amt     LIKE ap-inv.net
  FIELD amt-paid    LIKE ap-sel.amt-paid
  FIELD disc-amt    LIKE ap-sel.disc-amt
  FIELD line-amt    AS   DEC FORMAT "->>>,>>9.99".
    
DEF VAR camt LIKE ctot NO-UNDO.
DEF VAR lv-line-cnt AS INT NO-UNDO.
DEF VAR v-remit LIKE vend.remit NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR lv-date AS CHAR FORMAT "x(15)" EXTENT 2 NO-UNDO.
DEF VAR ll-last AS LOG NO-UNDO.

FORM "----------"       TO 57
     "----------"       TO 68
     "-----------"      TO 80
     ap-chk.check-date  TO 8    FORMAT "99/99/99"
     ap-chk.check-no    TO 20   FORMAT ">>>>999999"
     vend.remit         AT 22   FORMAT "x(25)"
     camt               TO 57   FORMAT "->>,>>9.99"
     cdis               TO 68   FORMAT "->>,>>9.99"
     ctot               TO 80
    WITH FRAME b3 WIDTH 85 NO-BOX NO-LABELS STREAM-IO NO-ATTR-SPACE.

FORM "  " SKIP 
     vend.vend-no       TO 78
     SKIP(1)
     lv-date[2]         AT 64
     SKIP(2)
     dol                AT 7  FORMAT "x(50)"
     ctot               TO 79 FORMAT "**,***,**9.99"
     SKIP(2)
     vend.remit         AT 10
     add1               AT 10
     add2               AT 10
     csz                AT 10
     SKIP(1)
     vend.check-memo    AT 10
     SKIP(7)                              
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
      WHERE vend.company EQ cocode
        AND vend.vend-no EQ ap-chk.vend-no        
      BREAK BY (IF v-sort-name THEN vend.name ELSE "")
            BY ap-chk.vend-no:
   
   ASSIGN
    add1   = vend.r-add1
    add2   = vend.r-add2
    csz    = vend.r-city + ", " + vend.r-state
    lv-zip = IF vend.r-postal NE "" THEN vend.r-postal ELSE vend.r-zip.

   IF add1 EQ "" AND add2 EQ "" THEN    /*if no remit-to address*/
     ASSIGN
      add1   = vend.add1
      add2   = vend.add2
      csz    = vend.city + ", " + vend.state
      lv-zip = IF vend.postal NE "" THEN vend.postal ELSE vend.zip.

   IF lv-zip BEGINS "00000" THEN lv-zip = "".

   csz = TRIM(csz) + "  " + SUBSTR(lv-zip,1,5) +
         (IF LENGTH(lv-zip) EQ 9 THEN "-" ELSE "") +
         SUBSTR(lv-zip,6,4).

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

    ASSIGN
     ctot              = ctot + ap-sel.amt-paid
     cdis              = cdis + ap-sel.disc-amt
     ap-sel.check-date = wdate.

    CREATE wrk-chk.
    ASSIGN
     wrk-chk.inv-no   = ap-sel.inv-no
     wrk-chk.po-no    = ap-inv.po-no
     wrk-chk.inv-date = ap-inv.inv-date
     wrk-chk.inv-amt  = IF ap-inv.gross GT 0 THEN ap-inv.gross ELSE ap-inv.net
     wrk-chk.amt-paid = ap-sel.amt-paid + ap-sel.disc-amt
     wrk-chk.disc-amt = ap-sel.disc-amt
     wrk-chk.line-amt = ap-sel.amt-paid.
    
    IF LAST(ap-sel.inv-no) THEN DO:
      checks-avail = YES.

      RUN ap/apchks.p (INPUT ctot, INPUT 50, OUTPUT dol).

      ASSIGN
       dol        = TRIM(dol) + FILL("*",50)
       lv-date[1] = STRING(YEAR(ap-chk.check-date),"9999") +
                    STRING(MONTH(ap-chk.check-date),"99")  +
                    STRING(DAY(ap-chk.check-date),"99")
       lv-date[2] = "".

      DO li = 1 TO 8:
        lv-date[2] = lv-date[2] + SUBSTR(lv-date[1],li,1) + " ".
      END.

      DISPLAY vend.vend-no
              lv-date[2]
              CAPS(dol)         @ dol
              ctot
              CAPS(vend.remit)  @ vend.remit
              CAPS(add1)        @ add1
              CAPS(add2)        @ add2
              CAPS(csz)         @ csz
              vend.check-memo
            WITH FRAME b1.     
      
      DO WHILE CAN-FIND(FIRST wrk-chk):
        DO li = 1 TO 2:
          lv-line-cnt = 0.

          RELEASE wrk-chk.

          FOR EACH wrk-chk BREAK BY wrk-chk.inv-no:
            PUT wrk-chk.inv-date              TO 8  FORMAT "99/99/99"
                wrk-chk.inv-no                TO 21 FORMAT "x(12)"
                wrk-chk.amt-paid              TO 57 FORMAT "->>,>>9.99"
                wrk-chk.disc-amt              TO 68 FORMAT "->>,>>9.99"
                wrk-chk.line-amt              TO 80 FORMAT "->>>,>>9.99".
      
            ASSIGN
             lv-line-cnt = lv-line-cnt + 1
             ll-last     = LAST(wrk-chk.inv-no).

            IF li EQ 2 THEN DELETE wrk-chk.

            IF ll-last THEN DO:
              camt = ctot + cdis.

              DISPLAY ap-chk.check-date
                      ap-chk.check-no
                      vend.remit
                      camt
                      cdis
                      ctot
                  WITH FRAME b3.

              RUN end-stub(INPUT li EQ 2).
            END.

            ELSE
            IF lv-line-cnt GE max-per-chk THEN DO:
              PUT SKIP(1).

              RUN end-stub (INPUT li EQ 2).

              IF li EQ 2 THEN DO:
                stnum  = stnum + 1.

                DISPLAY SKIP(6)
                        "V   V      OOO       III      DDDD"   AT 10 SKIP
                        "V   V     O   O       I       D   D"  AT 10 SKIP
                        "V   V     O   O       I       D   D"  AT 10 SKIP
                        " V V      O   O       I       D   D"  AT 10 SKIP
                        " V V      O   O       I       D   D"  AT 10 SKIP
                        "  V        OOO       III      DDDD "  AT 10
                        SKIP(10)
                    WITH FRAME u NO-BOX NO-LABELS STREAM-IO NO-ATTR-SPACE.
              END.

              LEAVE.
            END.
          END.
        END.
      END.

      ASSIGN
       stnum  = stnum + 1
       ctot   = 0
       cdis   = 0
       dol    = "".

     
    END. /*each ap-sel*/
  END.  /*each ap-chk*/
 END.

 IF NOT checks-avail THEN PUT "No checks found ready to print!".
END.

RETURN.

PROCEDURE end-stub.
    DEFINE INPUT PARAMETER iplPage AS LOGICAL NO-UNDO.

   
    IF iplPage THEN
        PAGE.
    ELSE
    DO:
         PUT SKIP(max-per-chk - lv-line-cnt).
         PUT SKIP.
    END.
    

END PROCEDURE.
