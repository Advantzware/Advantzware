/* ap/AchBadger.p Remittance Form for Badger */

{sys/inc/var.i shared}

{ap/ap-chk.i}
DEFINE VARIABLE iLineCount AS INTEGER NO-UNDO.
DEFINE VARIABLE lVoid AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lPage AS LOGICAL     NO-UNDO.
DEF VARIABLE cAddress AS CHARACTER EXTENT 4 FORMAT "x(50)" NO-UNDO.

DEFINE TEMP-TABLE tt-chk
    FIELD inv-no    LIKE ap-sel.inv-no
    FIELD po-no     LIKE ap-inv.po-no
    FIELD inv-date  LIKE ap-inv.inv-date
    FIELD inv-amt   LIKE ap-inv.net
    FIELD amt-paid  LIKE ap-sel.amt-paid
    FIELD disc-amt  LIKE ap-sel.disc-amt
    FIELD line-amt  AS DECIMAL FORMAT "->>>,>>9.99"
    .

FIND FIRST cust
    WHERE cust.company EQ cocode
      AND cust.ACTIVE EQ "X"
    NO-LOCK NO-ERROR.
IF AVAIL cust THEN DO:
    ASSIGN 
        cAddress[1]  = cust.name
        cAddress[2]  = cust.addr[1]
        cAddress[3]  = cust.addr[2]
        cAddress[4]  = cust.city + ", " + cust.state + " " + cust.zip
        .
    IF cAddress[3] EQ '' THEN
        ASSIGN
            cAddress[3] = cAddress[4]
            cAddress[4] = ''
            .
END.

IF v-print-mode NE "ALIGN" THEN DO:         
    lPage = NO.
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
        ll = 0.                                                                    
        ASSIGN
            ap-chk.check-date = wdate
            ap-chk.check-no   = stnum
            ap-chk.bank-code  = x-bank
            v-vend-no         = vend.vend-no
            v-vend-name       = vend.name.

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
            IF ll EQ 0 THEN DO:
                IF lPage THEN PUT CHR(12).
                ELSE lPage = YES.
                PUT "<FCalibri><R1><C33><P15><B>ACH REMITTANCE</B><P12>" SKIP(3)
                    "<C4>" cAddress[1] SKIP
                    "<C4>" cAddress[2] SKIP
                    "<C4>" cAddress[3] SKIP
                    "<C4>" cAddress[4]
                    SKIP(3)
                    "Vendor ID: "
                    v-vend-no
                    SPACE(8)
                    "Vendor Name: "
                    v-vend-name
                    SKIP(1)
                    .
                PUT "<C2>Invoice No."
                    "<C16>Reference"       
                    "<C29>Date"
                    "<C38>Inv Amt"
                    "<C48>Amt Paid"        
                    "<C58>Disc Taken"
                    "<C71>Net Amt"
                    SKIP
                    "<C1>============"
                    "<C14>============"
                    "<C27>========"
                    "<C36>==========" 
                    "<C47>=========="
                    "<C58>=========="
                    "<C69>==========="
                    SKIP
                    .
            END.
            ASSIGN
                ctot              = ctot + ap-sel.amt-paid
                cdis              = cdis + ap-sel.disc-amt
                ap-sel.check-date = wdate
                cgrossl           = cgrossl + ap-sel.amt-paid /*+ ap-sel.disc-amt) */
                cgross            = cgross + cgrossl
                .
            CREATE tt-chk.
            ASSIGN
                tt-chk.inv-no   = ap-sel.inv-no
                tt-chk.po-no    = ap-inv.po-no
                tt-chk.inv-date = ap-inv.inv-date
                tt-chk.inv-amt  = if ap-inv.gross gt 0 then ap-inv.gross else ap-inv.net
                tt-chk.amt-paid = ap-sel.amt-paid
                tt-chk.disc-amt = ap-sel.disc-amt
                tt-chk.line-amt = cgrossl
                .
            PUT "<C1>" tt-chk.inv-no    FORMAT "x(12)"
                "<C14>" TRIM(STRING(tt-chk.po-no,">>>>>>"))
                                        FORMAT "x(12)"
                "<C27>" tt-chk.inv-date FORMAT "99/99/99"
                "<C36>" tt-chk.inv-amt  FORMAT "->,>>>,>>9.99"
                "<C47>" tt-chk.amt-paid FORMAT "->,>>>,>>9.99"
                "<C58>" tt-chk.disc-amt FORMAT "->>,>>9.99"
                "<C69>" tt-chk.line-amt FORMAT "->,>>>,>>9.99"
                SKIP
                .
            ASSIGN
                ll      = ll + 1
                cgrossl = 0
                lVoid = NO   
                .
            IF ll EQ max-per-chk OR LAST(ap-sel.inv-no) THEN DO:
                IF LAST(ap-sel.inv-no) THEN DO:
                    checks-avail = YES.
                    PUT SKIP(1)
                        "<C35>============" SKIP
                        "<C1>Check Date: " ap-chk.check-date 
                        "<C33>Check No: " ap-chk.check-no
                        "<C55>Net Check Amount " ctot FORMAT "->,>>>,>>9.99".
/*                     PUT SKIP(max-per-chk - ll - 1).    */
/*                     RUN ap/apchks.p (INPUT ctot,       */
/*                                      INPUT 70,         */
/*                                      OUTPUT dol).      */
/*                     dol = TRIM(dol) + FILL("*",70) .   */
/*                     DISPLAY CAPS(dol)         @ dol    */
/*                         ap-chk.check-date              */
/*                         ctot                           */
/*                         caps(vend.remit)  @ vend.remit */
/*                         caps(add1)        @ add1       */
/*                         caps(add2)        @ add2       */
/*                         caps(csz)         @ csz        */
/*                         vend.check-memo                */
/*                         WITH FRAME b1.                 */
/*                     PUT SKIP(1).                       */
/*                 END. */
/*                 ELSE DO:                                                              */
/*                     DISPLAY SKIP(10)                                                  */
/*                         "V   V      OOO       III      DDDD"   AT 10 SKIP             */
/*                         "V   V     O   O       I       D   D"  AT 10 SKIP             */
/*                         "V   V     O   O       I       D   D"  AT 10 SKIP             */
/*                         " V V      O   O       I       D   D"  AT 10 SKIP             */
/*                         " V V      O   O       I       D   D"  AT 10 SKIP             */
/*                         "  V        OOO       III      DDDD "  AT 10                  */
/*                         SKIP(8)                                                       */
/*                         WITH FRAME u NO-BOX NO-LABELS STREAM-IO NO-ATTR-SPACE.        */
/*                     lVoid = YES.                                                      */
/*                 END.                                                                  */
/*                 DISPLAY SKIP(2)                                                       */
/*                     "Vendor ID: "                                                     */
/*                     v-vend-no                                                         */
/*                     SPACE(8)                                                          */
/*                     "Vendor Name: "                                                   */
/*                     v-vend-name                                                       */
/*                     SKIP(1)                                                           */
/*                     WITH NO-LABELS STREAM-IO NO-BOX FRAME abc1.                       */
/*                 PUT "Invoice No."     AT 2                                            */
/*                     "Reference"       AT 16                                           */
/*                     "Date"            AT 29                                           */
/*                     "Inv Amt"         AT 37                                           */
/*                     "Amt Paid"        AT 48                                           */
/*                     "Disc Taken"      AT 58                                           */
/*                     "Net Amt"         AT 71                                           */
/*                     "============"    AT 1                                            */
/*                     "============"    AT 14                                           */
/*                     "========"        AT 27                                           */
/*                     "=========="      AT 36                                           */
/*                     "=========="      AT 47                                           */
/*                     "=========="      AT 58                                           */
/*                     "==========="     AT 69                                           */
/*                     .                                                                 */
/*                 iLineCount = 0.                                                       */
/*                 FOR EACH tt-chk:                                                      */
/*                     PUT tt-chk.inv-no                TO 12 FORMAT "x(12)"             */
/*                         TRIM(STRING(tt-chk.po-no,">>>>>>"))                           */
/*                                                      TO 25 FORMAT "x(12)"             */
/*                         tt-chk.inv-date              TO 34 FORMAT "99/99/99"          */
/*                         tt-chk.inv-amt               TO 45 FORMAT "->>,>>9.99"        */
/*                         tt-chk.amt-paid              TO 56 FORMAT "->>,>>9.99"        */
/*                         tt-chk.disc-amt              TO 67 FORMAT "->>,>>9.99"        */
/*                         tt-chk.line-amt              TO 79 FORMAT "->>>,>>9.99".      */
/*                     DELETE tt-chk.                                                    */
/*                     iLineCount = iLineCount + 1.                                      */
/*                 END.                                                                  */
/*                 IF LAST(ap-sel.inv-no) THEN DO:                                       */
/*                     DISPLAY ap-chk.check-date ctot WITH FRAME b3.                     */
/*                     /*max-per-chk = 12 */                                             */
/*                     PUT SKIP(12 - iLineCount).                                        */
                    ASSIGN
                        stnum  = stnum + 1
                        ctot   = 0
                        cdis   = 0
                        cgross = 0
                        dol    = ""
                        .
                END.
                ll = 0.
            END.
        END.
    END.
    IF NOT checks-avail THEN PUT "No checks found ready to print!".
END.

