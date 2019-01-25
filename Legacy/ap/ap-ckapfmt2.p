/* ap/ap-ckprf.p   copied from ap/ap-ckasi.p  for Preferred box */

{sys/inc/var.i shared}

{ap/ap-chk.i}
DEFINE VARIABLE lv-line-cnt AS INTEGER   NO-UNDO.
DEFINE VARIABLE ll-void     AS LOG       NO-UNDO.
DEFINE VARIABLE cInvAmt     AS DECIMAL   NO-UNDO.

DEFINE VARIABLE v-comp-add1 AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add2 AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add3 AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE cBankAdd1   AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE cBankAdd2   AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE cBankAdd3   AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE cBankCode AS CHARACTER FORMAT "x(50)" NO-UNDO.

DEFINE WORKFILE wrk-chk
    FIELD inv-no      LIKE ap-sel.inv-no
    FIELD po-no       LIKE ap-inv.po-no
    FIELD cDscr       LIKE ap-sel.dscr
    FIELD inv-date    LIKE ap-inv.inv-date
    FIELD inv-amt     LIKE ap-inv.net
    FIELD amt-paid    LIKE ap-sel.amt-paid
    FIELD disc-amt    LIKE ap-sel.disc-amt
    FIELD line-amt    AS   DECIMAL FORMAT "->>>,>>9.99".


FORM 
   /* "----------"       AT 46
    "----------"        AT 57
    "----------"        AT 71   */ SKIP
    "<c37>" "TOTALS:"            
    "<c44.5>" cInvAmt            FORMAT "$->>,>>9.99" 
    "<c56.5>" cdis               FORMAT "$->>,>>9.99" 
    "<c71>" ctot               FORMAT "$->>,>>9.99"
    WITH FRAME b3 NO-BOX NO-LABELS STREAM-IO NO-ATTR-SPACE.

FORM SKIP(3)
    "<b><c1>" company.NAME  "<c38>" bank.bank-name 
    "</b><c60>"  bank.SwiftBIC FORMAT "x(11)"
    "<c72><b>" ap-chk.check-no "</b>"  SKIP
    "<c1>" v-comp-add1 "<c38>" cBankAdd1 SKIP
    "<c1>" v-comp-add2 "<c38>" cBankAdd2  SKIP
    "<c1>" v-comp-add3 "<c38>" cBankAdd3  SKIP
    "<b><c57>DATE"
    "<c69>AMOUNT</b>"  SKIP(1)
    "<c57>" ap-chk.check-date  FORMAT "99/99/9999"
    "<c69>" ctot               FORMAT "**,***,**9.99" SKIP(1)
    "<b>Pay</b>" AT 1
    dol  "<R-0.5>         ____________________________________________________________________________"
    SKIP(1)
    "to the Order of:<b>"  AT 10
    vend.remit         AT 20
    add1               AT 20
    add2               AT 20  " _________________________________" AT 100
    csz                AT 20   "</b>"
    SKIP(1)                              

    WITH FRAME b1 WIDTH 150 NO-BOX NO-LABELS STREAM-IO NO-ATTR-SPACE.

FIND FIRST company WHERE company.company EQ cocode NO-LOCK. 
FIND FIRST bank WHERE bank.company = cocode AND bank.bank-code = x-bank NO-ERROR.

IF v-print-mode NE "ALIGN" THEN 
DO:       /* production mode */   
    DISPLAY SKIP.
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
        FOR EACH ap-sel
            WHERE ap-sel.company   EQ cocode
            AND ap-sel.vend-no   EQ ap-chk.vend-no
            AND ap-sel.man-check EQ NO
            NO-LOCK
       
            BREAK BY ap-sel.inv-no:
       
            ll = ll + 1.
      
            IF ll EQ max-per-chk AND NOT last(ap-sel.inv-no) THEN
                ASSIGN
                    stnum = stnum + 1
                    ll    = 0.
        END.
   
        ASSIGN
            add1        = vend.r-add1
            add2        = vend.r-add2
            csz         = vend.r-city + ", " + vend.r-state
            lv-zip      = vend.r-zip
            v-comp-add1 = company.addr[1]
            v-comp-add2 = company.addr[2]
            v-comp-add3 = company.city + ", " +
                   company.state + "  " +
                   company.zip
            cBankAdd1   = bank.addr[1]
            cBankAdd2   = bank.addr[2]
            cBankAdd3   = bank.city + ", " +
                   bank.state + "  " +
                   bank.zip
            .

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

        IF v-comp-add1 EQ "" THEN
            ASSIGN
                v-comp-add1 = v-comp-add2
                v-comp-add2 = "".

        IF v-comp-add2 EQ "" THEN
            ASSIGN
                v-comp-add2 = v-comp-add3
                v-comp-add3 = "".
        IF cBankAdd1 EQ "" THEN
            ASSIGN
                cBankAdd1 = cBankAdd2
                cBankAdd2 = "".

        IF v-comp-add2 EQ "" THEN
            ASSIGN
                cBankAdd2 = cBankAdd3
                cBankAdd3 = "".
    
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

            IF ll EQ 0 THEN 
            DO:  
                PUT "</Progress><FArial>".

                DISPLAY /*SKIP*/
                    "<P10><R2><b>"company.NAME
                    "<c55>Check Number   <c70></b>" 
                    TRIM(STRING(ap-chk.check-no,">>>>>>"))  FORMAT "x(6)"   SKIP(1)
                    "To:" AT 1
                    v-vend-no
                    "<c55><b>Date   </b><c70>" 
                    ap-chk.check-date 
                    SKIP(1)
                    WITH WIDTH 85 NO-LABELS STREAM-IO NO-BOX FRAME xyz.

                PUT "<b><c1>Invoice Number"     
                    "<c15>Date"            
                    "<c25>Description"    
                    "<c49>Amount"        
                    "<c60>Discount"      
                    "<c72>Paid Amount</b>"  SKIP       
                    .
            END.

            ASSIGN
                ctot              = ctot + ap-sel.amt-paid
                cdis              = cdis + ap-sel.disc-amt
                ap-sel.check-date = wdate
                cgrossl           = cgrossl + ap-sel.amt-paid /*+ ap-sel.disc-amt) */
                cgross            = cgross + cgrossl
                cInvAmt           = cInvAmt + IF ap-inv.gross GT 0 THEN ap-inv.gross ELSE ap-inv.net.
                cBankCode = "C" + STRING(ap-chk.check-no,">>>>>>") + "C" + "   A" + STRING(bank.RTN,">>>>>>>>>") + "A   " + STRING(bank.bk-act + "C","x(15)") .
            CREATE wrk-chk.
            ASSIGN
                wrk-chk.inv-no   = ap-sel.inv-no
                wrk-chk.inv-date = ap-inv.inv-date
                wrk-chk.inv-amt  = IF ap-inv.gross GT 0 THEN ap-inv.gross ELSE ap-inv.net
                wrk-chk.amt-paid = ap-sel.amt-paid
                wrk-chk.disc-amt = ap-sel.disc-amt
                wrk-chk.line-amt = cgrossl
                wrk-chk.cDscr    = IF wrk-chk.inv-amt LT 0 THEN "Credit" ELSE "".

           PUT "<FCourier New><c1>"  wrk-chk.inv-no             FORMAT "x(12)"
                       "<c15>" wrk-chk.inv-date           FORMAT "99/99/9999"
                       "<c25>" wrk-chk.cDscr              FORMAT "x(15)"
                       "<c45>" wrk-chk.inv-amt            FORMAT "$->>,>>9.99"
                       "<c57>" wrk-chk.disc-amt           FORMAT "$->>,>>9.99"
                       "<c71.5>" wrk-chk.amt-paid           FORMAT "$->>,>>9.99"   SKIP.

            ASSIGN
                ll      = ll + 1
                cgrossl = 0
                ll-void = NO   
                .
    
            IF ll EQ max-per-chk OR last(ap-sel.inv-no) THEN 
            DO:
                IF LAST(ap-sel.inv-no) THEN 
                DO:
                    checks-avail = YES.

                    PUT SKIP "<C45>___________"       
                        "<C57>___________"     
                        "<C71.5>___________"   SKIP.

                    DISPLAY cInvAmt
                        cdis   
                        ctot  WITH FRAME b3 NO-LABELS STREAM-IO NO-BOX .  
        
                    PUT "<FArial>" SKIP(max-per-chk - ll ).

                    RUN ap/apchks.p (INPUT ctot, INPUT 70, OUTPUT dol).

                    dol = TRIM(dol) + fill("*",70) .  

                    DISPLAY CAPS(dol)         @ dol
                        TRIM(bank.SwiftBIC)    @ bank.SwiftBIC
                        TRIM(STRING(ap-chk.check-no,">>>>>>")) FORMAT "x(6)" @ ap-chk.check-no
                        ap-chk.check-date
                        ctot 
                        (COMPANY.NAME) @ COMPANY.NAME
                        (bank.bank-name) @ bank.bank-name
                        (v-comp-add1) @ v-comp-add1
                        (cBankAdd1) @ cBankAdd1 
                        (v-comp-add2) @ v-comp-add2
                        (cBankAdd2) @ cBankAdd2 
                        (v-comp-add3) @ v-comp-add3
                        (cBankAdd3) @ cBankAdd3 
                        CAPS(vend.remit)  @ vend.remit
                        CAPS(add1)        @ add1
                        CAPS(add2)        @ add2
                        CAPS(csz)         @ csz
                        /*vend.check-memo*/
                        WITH NO-LABELS STREAM-IO NO-BOX FRAME b1.
                    
                    PUT "<FMICR Encoding><P20>"
                        "<c15>" cBankCode FORMAT "x(50)"
                        "<FCourier New><FArial><P10>".
                END.
     
                ELSE 
                DO:
                    DISPLAY SKIP(7)
                        "V   V      OOO       III      DDDD"   AT 10 SKIP
                        "V   V     O   O       I       D   D"  AT 10 SKIP
                        "V   V     O   O       I       D   D"  AT 10 SKIP
                        " V V      O   O       I       D   D"  AT 10 SKIP
                        " V V      O   O       I       D   D"  AT 10 SKIP
                        "  V        OOO       III      DDDD "  AT 10
                        SKIP(10)
                        WITH FRAME u NO-BOX NO-LABELS STREAM-IO NO-ATTR-SPACE.
                    ll-void = YES.
                END.

              
                DISPLAY SKIP(3)
                    "<b>" company.NAME
                    "<c55>Check Number   <c70></b>" 
                    TRIM(STRING(ap-chk.check-no,">>>>>>"))  FORMAT "x(6)"   SKIP(1)
                    "To:" AT 1
                    v-vend-no
                    "<c55><b>Date  </b><c70>" 
                    ap-chk.check-date 
                    SKIP(1)
                    WITH WIDTH 85   NO-LABELS STREAM-IO NO-BOX FRAME abc1.

                PUT "<b><c1>Invoice Number"     
                    "<c15>Date"            
                    "<c25>Description"    
                    "<c49>Amount"        
                    "<c60>Discount"      
                    "<c72>Paid Amount</b>" SKIP .
      
                lv-line-cnt = 0.
                FOR EACH wrk-chk:
                   PUT "<FCourier New><c1>"  wrk-chk.inv-no             FORMAT "x(12)"
                       "<c15>" wrk-chk.inv-date           FORMAT "99/99/9999"
                       "<c25>" wrk-chk.cDscr              FORMAT "x(15)"
                       "<c45>" wrk-chk.inv-amt            FORMAT "$->>,>>9.99"
                       "<c57>" wrk-chk.disc-amt           FORMAT "$->>,>>9.99"
                       "<c71.5>" wrk-chk.amt-paid           FORMAT "$->>,>>9.99" SKIP.


                    DELETE wrk-chk.
                    lv-line-cnt = lv-line-cnt + 1.
                END.

                IF LAST(ap-sel.inv-no) THEN 
                DO: 
                     PUT SKIP "<C45>___________"       
                        "<C57>___________"     
                        "<C71.5>___________"   SKIP.

                    DISPLAY cInvAmt
                        cdis   
                        ctot  WITH FRAME b3 NO-LABELS STREAM-IO NO-BOX .
                    /*max-per-chk = 12 */
                    PUT "<FArial>" SKIP(10 - lv-line-cnt).

                    ASSIGN
                        stnum   = stnum + 1
                        ctot    = 0
                        cdis    = 0
                        cgross  = 0
                        dol     = ""
                        cInvAmt = 0.
                END. 
                ELSE IF ll-void THEN PUT SKIP(12 - lv-line-cnt) /* no total frame b3 */.
                PUT UNFORMATTED CHR(12). 
                ll = 0.
            END.
        END.
    END.

    IF NOT checks-avail THEN PUT "No checks found ready to print!".
END.


