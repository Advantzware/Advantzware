/* ap/ap-chkondu.p  for Onducorr 01/2020*/

/*globals*/
{sys/inc/var.i shared}
{ap/ap-chk.i}

/*local variables*/
DEF VAR lv-line-cnt AS INT NO-UNDO.
DEF VAR ll-void AS LOG NO-UNDO.
DEF VAR lc-booktemp AS CHAR FORMAT "X(30)" NO-UNDO.

/*font control*/
DEF VAR lc-font AS CHAR FORMAT "X(10)" INIT "<FArial>" NO-UNDO.
DEF VAR lc-font-size AS CHAR FORMAT "X(5)" INIT "<P8>" NO-UNDO.

/*column markers*/
DEF VAR li-col-inv AS INT INIT 0 FORMAT ">9" NO-UNDO.
DEF VAR li-col-date AS INT INIT 10 FORMAT ">9" NO-UNDO.
DEF VAR li-col-po AS INT INIT 20 FORMAT ">9" NO-UNDO.
DEF VAR li-col-amount AS INT INIT 30 FORMAT ">9" NO-UNDO.
DEF VAR li-col-invamt AS INT INIT 30 FORMAT ">9" NO-UNDO.
DEF VAR li-col-discount AS INT INIT 22 FORMAT ">9" NO-UNDO.
DEF VAR li-col-net AS INT INIT 30 FORMAT ">9" NO-UNDO.
DEFINE VARIABLE v-comp-add1 AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add2 AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add3 AS CHARACTER FORMAT "x(30)" NO-UNDO.


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

FIND FIRST company WHERE company.company EQ cocode NO-LOCK. 

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
            lv-postal = vend.r-postal
            v-comp-add1 = company.addr[1]
            v-comp-add2 = company.addr[2]
            v-comp-add3 = company.city + ", " +
                   company.state + "  " +
                   company.zip.

        IF add1 EQ "" AND add2 EQ "" THEN    /*if no remit-to address*/
            ASSIGN
                add1   = vend.add1
                add2   = vend.add2
                csz    = vend.city + ", " + vend.state
                lv-zip = vend.zip
                lv-postal = vend.postal
                .
        IF v-comp-add1 EQ "" THEN
            ASSIGN
                v-comp-add1 = v-comp-add2
                v-comp-add2 = "".

        IF v-comp-add2 EQ "" THEN
            ASSIGN
                v-comp-add2 = v-comp-add3
                v-comp-add3 = "".

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
                REPEAT WHILE ll > 12 AND CAN-FIND(FIRST wrk-chk WHERE wrk-chk.vend-no EQ v-vend-no):
                    RUN writeVoidedCheck(vend.remit,stnum).
                    ASSIGN
                        ll-void = YES
                        ll = ll - 12.
                    
                    RUN writeRegisterHeader(YES,stnum,ap-chk.check-date,ctot,
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
                                              wrk-chk.amt-paid,
                                              wrk-chk.disc-amt, 
                                              wrk-chk.line-amt).
                        IF lv-line-cnt = 12 THEN LEAVE.
                    END. /*each wrk-chk (top)*/
                    
                    RUN writeRegisterHeader(NO,stnum,ap-chk.check-date,ctot,
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
                                              wrk-chk.amt-paid,
                                              wrk-chk.disc-amt, 
                                              wrk-chk.line-amt).
                        DELETE wrk-chk.
                        IF lv-line-cnt = 12 THEN LEAVE.
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
               
                RUN writeRegisterHeader(YES,stnum,ap-chk.check-date,ctot,
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
                                          wrk-chk.amt-paid,
                                          wrk-chk.disc-amt, 
                                          wrk-chk.line-amt).
                    IF lv-line-cnt = 12 THEN LEAVE.
                END. /*each wrk-chk (top)*/
                
                RUN writeRegisterHeader(NO,stnum,ap-chk.check-date,ctot,OUTPUT lc-booktemp).
                lv-line-cnt = 0.
                FOR EACH wrk-chk WHERE wrk-chk.vend-no EQ v-vend-no:
                    lv-line-cnt = lv-line-cnt + 1.
                    RUN writeRegisterLine(lc-booktemp,
                                          lv-line-cnt,
                                          wrk-chk.inv-no,
                                          wrk-chk.po-no,
                                          wrk-chk.inv-date,
                                          wrk-chk.inv-amt,
                                          wrk-chk.amt-paid,
                                          wrk-chk.disc-amt, 
                                          wrk-chk.line-amt).
                    DELETE wrk-chk.
                    IF lv-line-cnt EQ 12 THEN LEAVE.
                END. /*each wrk-chk (bottom)*/
                IF lv-line-cnt <= 12 THEN /*PUT skip(14 - lv-line-cnt).*/ PAGE.               
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

    /*DEF VAR lc-date AS CHAR FORMAT "X(20)" NO-UNDO.

    IF ipdt-date = ? THEN 
        lc-date = "".
    ELSE
        lc-date = formatDate(ipdt-date).*/
    PUT "<C8><R2>" company.NAME  FORMAT "x(30)" SKIP
        "<C8>" v-comp-add1 FORMAT "x(30)" SKIP
        "<C8>" v-comp-add2 FORMAT "x(30)" SKIP
        "<C8>" v-comp-add3 FORMAT "x(30)" "<C50>NO CHÈQUE<C70>" ip-check-no SKIP(1)
        "<C50>DATE  <C70>" string(string(day(ipdt-date),"99") + "   " + string(MONTH(ipdt-date),"99") + "   " + string(YEAR(ipdt-date),"9999")) FORMAT "X(15)" SKIP
        "<C70> JJ  MM AAAA " SKIP(1)
        "<C8>" ipc-amount-desc  "<C70><B><P10>" ipd-amount "</B>" lc-font-size SKIP 
        "<C8>Montant en Dollar canadien" skip(1)
        "<C14>" ipc-payto SKIP
        "<C14>" ipc-payto-add1 SKIP
        "<C14>" ipc-payto-add2 SKIP
        "<C14>" ipc-payto-add3 SKIP .

   
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
    DEF INPUT PARAMETER ip-invamt       LIKE ap-inv.net  FORMAT "$->>>,>>9.99"   NO-UNDO.
    DEF INPUT PARAMETER ip-amount       LIKE ap-sel.amt-paid FORMAT "$->>>,>>9.99" NO-UNDO.
    DEF INPUT PARAMETER ip-discount     LIKE ap-sel.disc-amt FORMAT "->>>>,>>9.99" NO-UNDO.
    DEF INPUT PARAMETER ip-net          AS DEC               FORMAT "->,>>>,>>9.99".
    
    PUT 
        ipc-bookmark "<R+" TRIM(STRING(ipi-line + 2)) "><C+2>" ip-inv-no
        
        ipc-bookmark "<R+" TRIM(STRING(ipi-line + 2)) "><C+" li-col-discount ">" ip-discount
        ipc-bookmark "<R+" TRIM(STRING(ipi-line + 2)) "><C+" li-col-net ">" ip-net SKIP.


END PROCEDURE.

PROCEDURE writeRegisterHeader:
    DEF INPUT PARAMETER ipl-top         AS LOG                  NO-UNDO.
    DEF INPUT PARAMETER ip-check-no LIKE ap-chk.check-no NO-UNDO.
    DEF INPUT PARAMETER ipdt-date AS DATE NO-UNDO.
    DEF INPUT PARAMETER ipd-amount AS DECIMAL FORMAT ">>>,>>>,>>9.99" NO-UNDO.
    DEF OUTPUT PARAMETER opc-bookmark   AS CHAR FORMAT "X(5)"   NO-UNDO.

    DEF VAR lc-start    AS CHAR FORMAT "X(20)" NO-UNDO.
    DEF VAR lc-rect     AS CHAR FORMAT "X(50)" NO-UNDO.

    DEF VAR lc-date AS CHAR FORMAT "X(20)" NO-UNDO.

    IF ipdt-date = ? THEN
        lc-date = "".
    ELSE
        lc-date = formatDate(ipdt-date).
    
    lc-start = "<C4>".
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

    lc-rect = lc-rect + "<C4><FROM><C+78><R+17><RECT>".
    PUT lc-rect SKIP
        lc-start SKIP
      
        opc-bookmark "<R+2><C4" "><FROM>" opc-bookmark "<R+2><C82" "><LINE>" 
        opc-bookmark "<R+2><C42" "><FROM>" opc-bookmark "<R+17><C42" "><LINE>" 
        opc-bookmark "<R-1><C+1><B>Les Emballages OnduCorr Inc.</B> <C65> Montant en Dollar canadien "
        opc-bookmark "<R+1><C+1>Advantzware <C40> " lc-date    "<C60>"  ip-check-no 
        opc-bookmark "<R+2><C28>Escompte <C34> Montant Payé "     "<C67>Escompte <C74> Montant Payé" 
        opc-bookmark "<R+17><C60>Total<C72>" ipd-amount
        SKIP. 
   

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



