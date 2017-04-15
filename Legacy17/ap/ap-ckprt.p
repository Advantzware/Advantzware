/* ap/ap-ckprt.p  for Protagon BPV - 11/2012*/

/*globals*/
{sys/inc/var.i shared}
{ap/ap-chk.i}

/*local variables*/
DEF VAR lv-line-cnt AS INT NO-UNDO.
DEF VAR ll-void AS LOG NO-UNDO.
DEF VAR lc-booktemp AS CHAR FORMAT "X(20)" NO-UNDO.

/*font control*/
DEF VAR lc-font AS CHAR FORMAT "X(10)" INIT "<FArial>" NO-UNDO.
DEF VAR lc-font-size AS CHAR FORMAT "X(5)" INIT "<P8>" NO-UNDO.

/*column markers*/
DEF VAR li-col-inv AS INT INIT 0 FORMAT ">9" NO-UNDO.
DEF VAR li-col-date AS INT INIT 10 FORMAT ">9" NO-UNDO.
DEF VAR li-col-po AS INT INIT 20 FORMAT ">9" NO-UNDO.
DEF VAR li-col-amount AS INT INIT 30 FORMAT ">9" NO-UNDO.
DEF VAR li-col-discount AS INT INIT 40 FORMAT ">9" NO-UNDO.
DEF VAR li-col-net AS INT INIT 50 FORMAT ">9" NO-UNDO.

/*Function forwards*/
FUNCTION formatDate RETURNS CHAR
  ( INPUT ip-date AS DATE)  FORWARD.

/*Primary temp table for records*/
DEF TEMP-TABLE wrk-chk
    FIELD inv-no      LIKE ap-sel.inv-no
    FIELD po-no       LIKE ap-inv.po-no
    FIELD inv-date    LIKE ap-inv.inv-date
    FIELD inv-amt     LIKE ap-inv.net
    FIELD amt-paid    LIKE ap-sel.amt-paid
    FIELD disc-amt    LIKE ap-sel.disc-amt
    FIELD line-amt    AS DEC FORMAT "->>>,>>9.99"
    FIELD vend-no     LIKE ap-inv.vend-no.

PUT lc-font lc-font-size.
IF v-print-mode NE "ALIGN" THEN DO:         /* production mode */
    FOR EACH ap-chk EXCLUSIVE-LOCK 
        WHERE ap-chk.company EQ cocode
            AND ap-chk.vend-no GE wvend-no
            AND ap-chk.vend-no LE evend-no
            AND ap-chk.man-check EQ NO
            AND CAN-FIND(FIRST ap-sel
                            WHERE ap-sel.company   EQ cocode
                                AND ap-sel.vend-no   EQ ap-chk.vend-no
                                AND ap-sel.man-check EQ NO),
        FIRST vend NO-LOCK
            WHERE vend.company EQ cocode
                AND vend.vend-no EQ ap-chk.vend-no
        BREAK 
            BY (IF v-sort-name THEN vend.name ELSE "")
            BY ap-chk.vend-no:

        ll = 0.
        FOR EACH ap-sel
            WHERE ap-sel.company EQ cocode
                AND ap-sel.vend-no EQ ap-chk.vend-no
                AND ap-sel.man-check EQ NO
            NO-LOCK
            BREAK BY ap-sel.inv-no:

            ll = ll + 1.
            IF ll EQ max-per-chk AND NOT LAST(ap-sel.inv-no) THEN
                ASSIGN
                    stnum = stnum + 1
                    ll = 0.
        END. /*each ap-sel*/
        ASSIGN
            add1   = vend.r-add1
            add2   = vend.r-add2
            csz    = vend.r-city + ", " + vend.r-state
            lv-zip = vend.r-zip
            lv-postal = vend.r-postal.

        IF add1 EQ "" AND add2 EQ "" THEN    /*if no remit-to address*/
            ASSIGN
                add1   = vend.add1
                add2   = vend.add2
                csz    = vend.city + ", " + vend.state
                lv-zip = vend.zip
                lv-postal = vend.postal.

        IF lv-zip BEGINS "00000" THEN lv-zip = "".
        csz = TRIM(csz) + "  " + SUBSTR(lv-zip,1,5) +
            (IF LENGTH(lv-zip) GT 5 THEN ("-" + SUBSTR(lv-zip,6,4)) ELSE "").
        IF lv-postal NE "" THEN csz = TRIM(csz) + "  " + TRIM(lv-postal).
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
        FOR EACH ap-sel
            WHERE ap-sel.company    EQ cocode
            AND ap-sel.vend-no      EQ ap-chk.vend-no
            AND ap-sel.man-check    EQ NO
            EXCLUSIVE-LOCK
            BREAK BY ap-sel.inv-no:
            FIND FIRST ap-inv
                WHERE ap-inv.company EQ cocode
                AND ap-inv.vend-no EQ ap-sel.vend-no
                AND ap-inv.inv-no  EQ ap-sel.inv-no
                EXCLUSIVE-LOCK
                USE-INDEX inv-no.
            FIND FIRST ap-invl
                WHERE ap-invl.company = ap-inv.company
                AND ap-invl.i-no = ap-inv.i-no
                USE-INDEX i-no NO-LOCK NO-ERROR.
            
            CREATE wrk-chk.
            ASSIGN
                ap-inv.pay-date   = wdate
                ctot              = ctot + ap-sel.amt-paid
                cdis              = cdis + ap-sel.disc-amt
                ap-sel.check-date = wdate
                cgrossl           = cgrossl + ap-sel.amt-paid /*+ ap-sel.disc-amt) */
                cgross            = cgross + cgrossl
                wrk-chk.inv-no    = ap-sel.inv-no
                wrk-chk.po-no     = IF AVAIL ap-invl THEN ap-invl.po-no ELSE ap-inv.po-no
                wrk-chk.inv-date  = ap-inv.inv-date
                wrk-chk.inv-amt   = IF ap-inv.gross GT 0 THEN ap-inv.gross ELSE ap-inv.net
                wrk-chk.amt-paid  = ap-sel.amt-paid
                wrk-chk.disc-amt  = ap-sel.disc-amt
                wrk-chk.line-amt  = cgrossl
                wrk-chk.vend-no   = ap-inv.vend-no
                ll      = ll + 1
                cgrossl = 0
                ll-void = NO.

            IF LAST(ap-sel.inv-no) THEN DO:
                REPEAT WHILE ll > 15 AND CAN-FIND(FIRST wrk-chk WHERE wrk-chk.vend-no EQ v-vend-no):
                    RUN writeVoidedCheck(vend.remit,stnum).
                    ASSIGN
                        ll-void = YES
                        ll = ll - 15.
                    RUN writeRegisterSummary(YES,
                                             ?,
                                             0,
                                             vend.remit).
                    RUN writeRegisterHeader(YES,
                                            OUTPUT lc-booktemp).
                    lv-line-cnt = 0.
                    FOR EACH wrk-chk WHERE wrk-chk.vend-no EQ v-vend-no:
                        lv-line-cnt = lv-line-cnt + 1.
                        RUN writeRegisterLine(lc-booktemp,
                                              lv-line-cnt,
                                              wrk-chk.inv-no,
                                              wrk-chk.po-no,
                                              wrk-chk.inv-date,
                                              wrk-chk.inv-amt,
                                              wrk-chk.disc-amt, 
                                              wrk-chk.line-amt).
                        IF lv-line-cnt = 15 THEN LEAVE.
                    END. /*each wrk-chk (top)*/
                    RUN writeRegisterSummary(NO,
                                             ?,
                                             0,
                                             vend.remit).
                    RUN writeRegisterHeader(NO,
                                            OUTPUT lc-booktemp).
                    lv-line-cnt = 0.
                    FOR each wrk-chk WHERE wrk-chk.vend-no EQ v-vend-no:
                        lv-line-cnt = lv-line-cnt + 1.
                        RUN writeRegisterLine(lc-booktemp,
                                              lv-line-cnt,
                                              wrk-chk.inv-no,
                                              wrk-chk.po-no,
                                              wrk-chk.inv-date,
                                              wrk-chk.inv-amt,
                                              wrk-chk.disc-amt, 
                                              wrk-chk.line-amt).
                        DELETE wrk-chk.
                        IF lv-line-cnt = 15 THEN LEAVE.
                    END. /*each wrk-check (bottom)*/
                    PAGE.
                END. /*repeat*/
    
                IF LAST(ap-sel.inv-no) THEN DO:
                    checks-avail = YES.
                    RUN ap/apchks.p (INPUT ctot, 
                                     INPUT 70, 
                                     OUTPUT dol).
        
                    dol = TRIM(dol) + FILL("*",70).
                    RUN writeCheck (CAPS(dol),
                                    ctot,
                                    ap-chk.check-date,
                                    CAPS(vend.remit),
                                    add1,
                                    add2,
                                    csz,
/*                                     lv-postal, */
                                    stnum).
                END. /* if last(ap-sel.inv-no)*/
                RUN writeRegisterSummary(YES,
                                         ap-chk.check-date,
                                         ctot,
                                         CAPS(vend.remit)).
                RUN writeRegisterHeader(YES,
                                        OUTPUT lc-booktemp).
                lv-line-cnt = 0.
                FOR EACH wrk-chk WHERE wrk-chk.vend-no EQ v-vend-no:
                    lv-line-cnt = lv-line-cnt + 1.
                    RUN writeRegisterLine(lc-booktemp,
                                          lv-line-cnt,
                                          wrk-chk.inv-no,
                                          wrk-chk.po-no,
                                          wrk-chk.inv-date,
                                          wrk-chk.inv-amt,
                                          wrk-chk.disc-amt, 
                                          wrk-chk.line-amt).
                    IF lv-line-cnt = 15 THEN LEAVE.
                END. /*each wrk-chk (top)*/
                RUN writeRegisterSummary(NO,ap-chk.check-date,ctot,CAPS(vend.remit)).
                RUN writeRegisterHeader(NO,OUTPUT lc-booktemp).
                lv-line-cnt = 0.
                FOR EACH wrk-chk WHERE wrk-chk.vend-no EQ v-vend-no:
                    lv-line-cnt = lv-line-cnt + 1.
                    RUN writeRegisterLine(lc-booktemp,
                                          lv-line-cnt,
                                          wrk-chk.inv-no,
                                          wrk-chk.po-no,
                                          wrk-chk.inv-date,
                                          wrk-chk.inv-amt,
                                          wrk-chk.disc-amt, 
                                          wrk-chk.line-amt).
                    DELETE wrk-chk.
                    IF lv-line-cnt EQ 15 THEN LEAVE.
                END. /*each wrk-chk (bottom)*/
                IF lv-line-cnt <= 15 THEN /*PUT skip(14 - lv-line-cnt).*/ PAGE.               
                ASSIGN
                    stnum  = stnum + 1
                    ctot   = 0
                    cdis   = 0
                    cgross = 0
                    dol    = ""
                    ll = 0.
            END. /*IF LAST(ap-sel.inv-no)*/
        END. /*each ap-sel*/
    END.  /*each ap-chk*/
    IF NOT checks-avail THEN PUT "No checks found ready to print!".
END. /*if v-print-mode NE "ALIGN"*/

PROCEDURE writeCheck:
    DEF INPUT PARAMETER ipc-amount-desc AS CHAR FORMAT "X(80)" NO-UNDO.
    DEF INPUT PARAMETER ipd-amount AS DECIMAL FORMAT "$>>>,>>>,>>9.99" NO-UNDO.
    DEF INPUT PARAMETER ipdt-date AS DATE NO-UNDO.
    DEF INPUT PARAMETER ipc-payto AS CHAR FORMAT "X(50)" NO-UNDO.
    DEF INPUT PARAMETER ipc-payto-add1 AS CHAR FORMAT "X(50)" NO-UNDO.
    DEF INPUT PARAMETER ipc-payto-add2 AS CHAR FORMAT "X(50)" NO-UNDO.
    DEF INPUT PARAMETER ipc-payto-add3 AS CHAR FORMAT "X(50)" NO-UNDO.
/*     DEF INPUT PARAMETER ipc-payto-postalcode AS CHAR FORMAT "X(50)" NO-UNDO. */
    DEF INPUT PARAMETER ip-check-no LIKE ap-chk.check-no NO-UNDO.

    DEF VAR lc-date AS CHAR FORMAT "X(20)" NO-UNDO.

    IF ipdt-date = ? THEN 
        lc-date = "".
    ELSE
        lc-date = formatDate(ipdt-date).
    PUT "<C8><R9><#1>" ipc-amount-desc 
        "<=1><R-1><C+60>" ip-check-no    
        "<=#1><R+2><C+40>" lc-date "<C+12><B><P10>" ipd-amount "</B>" lc-font-size 
        "<=#1><R+4><B>" ipc-payto "</B>" 
        "<=#1><R+5>" ipc-payto-add1 
        "<=#1><R+6>" ipc-payto-add2 
        "<=#1><R+7>" ipc-payto-add3 
/*         "<=#1><R+8>" ipc-payto-postalcode */
        SKIP.


END PROCEDURE.

PROCEDURE writeVoidedCheck:
    DEF INPUT PARAMETER ipc-payto AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ip-check-no LIKE ap-chk.check-no NO-UNDO.

    PUT "<P50><C32><R4><B>VOID</B>" lc-font-size.
    RUN writeCheck ("*** NO DOLLARS AND NO CENTS ***",0,?,ipc-payto,"","","",ip-check-no).


END PROCEDURE.

PROCEDURE writeRegisterLine:
    DEF INPUT PARAMETER ipc-bookmark    AS CHAR              FORMAT "X(5)"       NO-UNDO.
    DEF INPUT PARAMETER ipi-line        AS INT               FORMAT ">9"         NO-UNDO.
    DEF INPUT PARAMETER ip-inv-no       LIKE ap-sel.inv-no   FORMAT "X(12)"      NO-UNDO.
    DEF INPUT PARAMETER ip-po-no        LIKE ap-inv.po-no    FORMAT ">>>>>9"      NO-UNDO.
    DEF INPUT PARAMETER ip-date         LIKE ap-inv.inv-date FORMAT "99/99/99"   NO-UNDO.
    DEF INPUT PARAMETER ip-amount       LIKE ap-sel.amt-paid FORMAT "$->>,>>9.99" NO-UNDO.
    DEF INPUT PARAMETER ip-discount     LIKE ap-sel.disc-amt FORMAT "$->>,>>9.99" NO-UNDO.
    DEF INPUT PARAMETER ip-net          AS DEC               FORMAT "$->>>,>>9.99".
    
    PUT 
        ipc-bookmark "<R+" TRIM(STRING(ipi-line)) ">" ip-inv-no 
        ipc-bookmark "<R+" TRIM(STRING(ipi-line)) "><C+" li-col-date ">" ip-date
        ipc-bookmark "<R+" TRIM(STRING(ipi-line)) "><C+" li-col-po ">" ip-po-no 
        ipc-bookmark "<R+" TRIM(STRING(ipi-line)) "><RIGHT=C+" li-col-discount "><C+" li-col-amount ">" ip-amount
        ipc-bookmark "<R+" TRIM(STRING(ipi-line)) "><RIGHT=C+" li-col-net "><C+" li-col-discount ">" ip-discount
        ipc-bookmark "<R+" TRIM(STRING(ipi-line)) "><RIGHT=C+59><C+" li-col-net ">" ip-net SKIP.


END PROCEDURE.

PROCEDURE writeRegisterHeader:
    DEF INPUT PARAMETER ipl-top         AS LOG                  NO-UNDO.
    DEF OUTPUT PARAMETER opc-bookmark   AS CHAR FORMAT "X(5)"   NO-UNDO.

    DEF VAR lc-start    AS CHAR FORMAT "X(20)" NO-UNDO.
    DEF VAR lc-rect     AS CHAR FORMAT "X(50)" NO-UNDO.
    
    lc-start = "<C20>".
    IF ipl-top THEN 
        ASSIGN 
            lc-start = lc-start + "<R24>"
            lc-rect = lc-start 
            lc-start = lc-start + "<#4>"
            opc-bookmark = "<=#4>".
    ELSE 
        ASSIGN
            lc-start = lc-start + "<R45>"
            lc-rect = lc-start 
            lc-start = lc-start + "<#5>"
            opc-bookmark = "<=#5>".

    lc-rect = lc-rect + "<FROM><C+59><R+1><RECT>".
    PUT lc-rect SKIP
        lc-start SKIP
        opc-bookmark "<R+0><C+" li-col-date "><FROM>" opc-bookmark "<R+1><C+" li-col-date "><LINE>" 
        opc-bookmark "<R+0><C+" li-col-po "><FROM>" opc-bookmark "<R+1><C+" li-col-po "><LINE>"
        opc-bookmark "<R+0><C+" li-col-amount "><FROM>" opc-bookmark "<R+1><C+" li-col-amount "><LINE>"
        opc-bookmark "<R+0><C+" li-col-discount "><FROM>" opc-bookmark "<R+1><C+" li-col-discount "><LINE>"
        opc-bookmark "<R+0><C+" li-col-net "><FROM>" opc-bookmark "<R+1><C+" li-col-net "><LINE>" SKIP.
    PUT "<B>" opc-bookmark "<C+" li-col-inv ">INVOICE NO."
        opc-bookmark "<C+" li-col-date + 1 FORMAT ">9" ">DATE"
        opc-bookmark "<C+" li-col-po + 1 FORMAT ">9" ">PO"
        opc-bookmark "<C+" li-col-amount + 3 FORMAT ">9" ">AMOUNT"
        opc-bookmark "<C+" li-col-discount + 3 FORMAT ">9" ">DISCOUNT"
        opc-bookmark "<C+" li-col-net + 3 FORMAT ">9" ">NET AMT." SKIP
        "</B>".

        

END PROCEDURE.

PROCEDURE writeRegisterSummary:
    DEF INPUT PARAMETER ipl-top AS LOG NO-UNDO.
    DEF INPUT PARAMETER ipdt-date AS DATE NO-UNDO.
    DEF INPUT PARAMETER ipd-amount AS DECIMAL NO-UNDO.
    DEF INPUT PARAMETER ipc-payto AS CHAR FORMAT "X(50)" NO-UNDO.

    DEF VAR lc-start AS CHAR FORMAT "X(20)" NO-UNDO.
    DEF VAR lc-bookmark AS CHAR NO-UNDO.
    DEF VAR lc-date AS CHAR FORMAT "X(20)" NO-UNDO.
    
    IF ipl-top THEN 
        ASSIGN 
            lc-start = "<C2><R28><#2>"
            lc-bookmark = "<=#2>".
    ELSE 
        assign
            lc-start = "<C2><R49><#3>"
            lc-bookmark = "<=#3>".
    
    IF ipdt-date = ? THEN
        lc-date = "".
    ELSE
        lc-date = formatDate(ipdt-date).

    PUT lc-start SKIP
        lc-bookmark "DATE" SKIP
        lc-bookmark "<R+1><B>" lc-date "</B>" SKIP
        lc-bookmark "<R+3>CHEQUE AMOUNT" SKIP
        lc-bookmark "<R+4><B>$" TRIM(STRING(ipd-amount,">>>,>>>,>>9.99")) FORM "X(14)" "</B>" SKIP
        lc-bookmark "<R+9>PAID TO" SKIP
        lc-bookmark "<R+10><B>" ipc-payto "</B>" SKIP.


END PROCEDURE.

FUNCTION formatDate RETURNS CHAR
  ( INPUT ip-date AS DATE) :
  /*------------------------------------------------------------------------------
  Purpose:
  Notes:
  ------------------------------------------------------------------------------*/
  DEFINE VAR out-date AS CHAR NO-UNDO.
  DEFINE VAR cMonth AS CHAR EXTENT 12 NO-UNDO INIT
    [ "Jan",    "Feb",     "Mar", 
      "Apr",      "May",          "Jun", 
      "Jul",       "Aug",       "Sep",
      "Oct",    "Nov",     "Dec" ]. 
    
    out-date =  STRING(DAY(ip-date),"99") + "-" + cmonth[MONTH(ip-date)] + "-" + STRING(YEAR(ip-date)).
    RETURN out-date.
END FUNCTION.


