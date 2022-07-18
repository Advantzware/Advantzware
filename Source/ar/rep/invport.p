/* ---------------------------------------------- ar/rep/invport.p */
/* PRINT INVOICE   Xprint form for PremierX and PremierS           */
/* -------------------------------------------------------------------------- */
DEFINE INPUT PARAMETER ip-copy-title AS cha NO-UNDO.
DEFINE INPUT PARAMETER ip-print-s AS LOG NO-UNDO. /* for PremierS */

{sys/inc/var.i shared}

{ar/rep/invoice.i}
{custom/notesdef.i}
DEFINE        VARIABLE v-salesman     AS CHARACTER FORMAT "x(14)" NO-UNDO.
DEFINE        VARIABLE v-salesname    AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE        VARIABLE v-fob          AS CHARACTER FORMAT "x(27)" NO-UNDO.
DEFINE        VARIABLE v-shipvia      LIKE carrier.dscr NO-UNDO.
DEFINE        VARIABLE v-addr3        AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE        VARIABLE v-email        AS CHARACTER FORMAT "x(130)" NO-UNDO.
DEFINE        VARIABLE v-sold-addr3   AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE        VARIABLE v-shipto-name  AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE        VARIABLE v-shipto-addr  AS CHARACTER FORMAT "x(30)" EXTENT 2 NO-UNDO.
DEFINE        VARIABLE v-shipto-city  AS CHARACTER FORMAT "x(15)" NO-UNDO.
DEFINE        VARIABLE v-shipto-state AS CHARACTER FORMAT "x(2)" NO-UNDO.
DEFINE        VARIABLE v-shipto-zip   AS CHARACTER FORMAT "x(10)" NO-UNDO.
DEFINE        VARIABLE v-line         AS INTEGER   NO-UNDO.
DEFINE        VARIABLE v-printline    AS INTEGER   NO-UNDO.
DEFINE        VARIABLE v-t-weight     LIKE ar-invl.t-weight NO-UNDO.
DEFINE        VARIABLE v-inv-no       AS INTEGER   NO-UNDO.
DEFINE        VARIABLE v-tot-cas      AS DECIMAL   FORMAT "->>>9.9999" NO-UNDO.
DEFINE        VARIABLE v-tot-pallets  AS INTEGER   NO-UNDO.
DEFINE        VARIABLE v-tot-qty      AS INTEGER   NO-UNDO.
DEFINE        VARIABLE v-inv-date     AS DATE      INITIAL TODAY FORM "99/99/9999" NO-UNDO.
DEFINE SHARED VARIABLE v-fr-tax       AS LOGICAL   INITIAL NO NO-UNDO.
DEFINE        VARIABLE v-tax-rate     AS DECIMAL   FORMAT "->>>.99" NO-UNDO.
DEFINE        VARIABLE v-tax-code     LIKE stax.tax-code NO-UNDO.
DEFINE        VARIABLE v-tx-rate      LIKE stax.tax-rate NO-UNDO.
DEFINE        VARIABLE v-ans          AS LOGICAL   INITIAL NO NO-UNDO.
DEFINE        VARIABLE v-date-ship    AS DATE      INITIAL TODAY NO-UNDO.
DEFINE        VARIABLE v-del-no       AS INTEGER   FORMAT ">>>>>>" NO-UNDO.
DEFINE        VARIABLE v-bol-cases    LIKE oe-boll.cases NO-UNDO.
DEFINE        VARIABLE v-set-qty      AS DECIMAL   NO-UNDO.
DEFINE        VARIABLE v-part-qty     AS DECIMAL   FORMAT "999.9999" NO-UNDO.
DEFINE        VARIABLE v-net          LIKE ar-inv.gross NO-UNDO.
DEFINE        VARIABLE v-case-cnt     AS CHARACTER FORMAT "x(80)" EXTENT 5 NO-UNDO.
DEFINE        VARIABLE v-case-line    AS CHARACTER NO-UNDO.
DEFINE        VARIABLE v-part-line    AS CHARACTER NO-UNDO.
DEFINE        VARIABLE v-pc           AS cha       NO-UNDO. /* partial or complete */
DEFINE        VARIABLE v-i-dscr2      AS cha       FORM "x(30)" NO-UNDO.
DEFINE        VARIABLE lv-bol-no      LIKE oe-boll.bol-no NO-UNDO.

DEFINE BUFFER xar-inv FOR ar-inv .

DEFINE TEMP-TABLE w-sman NO-UNDO
    FIELD sman AS CHARACTER FORMAT "x(4)".

DEFINE VARIABLE v-ord-del-hdr  AS CHARACTER FORMAT "x(3)" INIT "Del".
DEFINE VARIABLE v-beeler-lines AS INTEGER.
DEFINE VARIABLE v-part-info    AS CHARACTER FORMAT "x(30)".
DEFINE VARIABLE v              AS INTEGER.
DEFINE VARIABLE v-bo-qty       AS INTEGER   FORMAT "99999" NO-UNDO.
DEFINE VARIABLE v-inv-qty      AS INTEGER   FORMAT "99999" NO-UNDO.
DEFINE VARIABLE v-inv-qtys     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-ship-qty     AS INTEGER   FORMAT "99999" NO-UNDO.
DEFINE VARIABLE v-i-no         AS CHARACTER FORMAT "x(15)" NO-UNDO.
DEFINE VARIABLE v-i-dscr       AS CHARACTER FORMAT "x(18)" NO-UNDO.
DEFINE VARIABLE v-price        AS DECIMAL   FORMAT ">>>>9.999999" NO-UNDO.
DEFINE VARIABLE v-t-price      AS DECIMAL   FORMAT ">>>>>>9.99" NO-UNDO.
DEFINE VARIABLE v-po-no        LIKE ar-invl.po-no NO-UNDO.
DEFINE VARIABLE v-bill-i       AS CHARACTER FORMAT "x(25)" NO-UNDO.
DEFINE VARIABLE v-ord-no       LIKE oe-ord.ord-no NO-UNDO.
DEFINE VARIABLE v-ord-date     LIKE oe-ord.ord-date NO-UNDO.
DEFINE VARIABLE v-ship-i       AS CHARACTER FORMAT "x(25)" NO-UNDO.
DEFINE VARIABLE v-rel-po-no    LIKE oe-rel.po-no NO-UNDO.
DEFINE VARIABLE v-price-head   AS CHARACTER FORMAT "x(5)" NO-UNDO.
DEFINE VARIABLE v-subtot-lines AS DECIMAL   NO-UNDO.
DEFINE TEMP-TABLE w-tax NO-UNDO
    FIELD w-dsc AS CHARACTER
    FIELD w-tax AS DECIMAL.
DEFINE        VARIABLE v-t-tax          AS DECIMAL   EXTENT 3 NO-UNDO.
DEFINE        VARIABLE v-bot-lab        AS CHARACTER FORMAT "x(63)" EXTENT 3 NO-UNDO.
DEFINE        VARIABLE v-lines          AS INTEGER   NO-UNDO.
DEFINE        VARIABLE v-inv-freight    LIKE ar-inv.freight NO-UNDO.
DEFINE        VARIABLE v-frt-tax        AS DECIMAL   NO-UNDO.
DEFINE        VARIABLE v-notes          AS cha       EXTENT 4 FORM "x(80)" NO-UNDO.
DEFINE        VARIABLE v-notes-line     AS INTEGER   NO-UNDO.
DEFINE        VARIABLE v-inv-total      AS DECIMAL   NO-UNDO.
DEFINE SHARED VARIABLE s-print-zero-qty AS LOG       NO-UNDO.

/* === with xprint ====*/
DEFINE        VARIABLE ls-image1        AS cha       NO-UNDO.
DEFINE        VARIABLE ls-full-img1     AS cha       FORM "x(200)" NO-UNDO.
/*ASSIGN                                          */
/*   ls-image1 = "images\premiercan.jpg"          */
/*   FILE-INFO:FILE-NAME = ls-image1              */
/*   ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".*/

DEFINE        VARIABLE v-tel            AS cha       FORM "x(30)" NO-UNDO.
DEFINE        VARIABLE v-fax            AS cha       FORM "x(30)" NO-UNDO.
DEFINE        VARIABLE v-contact        AS cha       FORM "x(20)" NO-UNDO .

DEFINE        VARIABLE v-comp-add1      AS cha       FORM "x(30)" NO-UNDO.
DEFINE        VARIABLE v-comp-add2      AS cha       FORM "x(30)" NO-UNDO.
DEFINE        VARIABLE v-comp-add3      AS cha       FORM "x(30)" NO-UNDO.
DEFINE        VARIABLE v-comp-add4      AS cha       FORM "x(30)" NO-UNDO.

DEFINE        VARIABLE cRtnChar         AS CHARACTER NO-UNDO.
DEFINE        VARIABLE lRecFound        AS LOGICAL   NO-UNDO.
DEFINE        VARIABLE lChkImage        AS LOGICAL   NO-UNDO. 
DEFINE        VARIABLE cTaxCode         AS CHARACTER NO-UNDO.
DEFINE        VARIABLE cCurCode         AS CHARACTER NO-UNDO.
DEFINE        VARIABLE cCompanyID       AS CHARACTER NO-UNDO.
DEFINE        VARIABLE lValid           AS LOGICAL   NO-UNDO.
DEFINE        VARIABLE cMessage         AS CHARACTER NO-UNDO.
DEFINE        VARIABLE opcDateStringInvDate  AS CHARACTER NO-UNDO.
DEFINE        VARIABLE opcDateStringShipDate AS CHARACTER NO-UNDO.

IF ip-copy-title  EQ "Customer Copy" THEN
    ip-copy-title = "Cópia do cliente".
    
FIND FIRST company WHERE company.company = cocode NO-LOCK NO-ERROR.
IF company.company EQ '004' THEN 
    ASSIGN 
        cCurCode   = 'CAD'
        cTaxCode   = 'HST'
        cCompanyID = 'GST# 70523 1090 RT0001'
        .
ELSE IF company.company EQ '006' THEN 
        ASSIGN 
            cCurCode   = 'AUD'
            cTaxCode   = 'GST'
            cCompanyID = 'ABN 11 620 887 149'
            .
    ELSE 
        ASSIGN 
            cCurCode   = 'USD'
            cTaxCode   = 'vendas do frete'
            cCompanyID = ''
            .
RUN sys/ref/nk1look.p (INPUT company.company, "BusinessFormLogo", "C" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound AND cRtnChar NE "" THEN 
DO:
    cRtnChar = DYNAMIC-FUNCTION (
        "fFormatFilePath",
        cRtnChar
        ).
                     
    /* Validate the N-K-1 BusinessFormLogo image file */
    RUN FileSys_ValidateFile(
        INPUT  cRtnChar,
        OUTPUT lValid,
        OUTPUT cMessage
        ) NO-ERROR.
  
    IF NOT lValid THEN 
    DO:
        MESSAGE "Unable to find image file '" + cRtnChar + "' in N-K-1 setting for BusinessFormLogo"
            VIEW-AS ALERT-BOX ERROR.
    END.
END.

IF cRtnChar NE "" THEN 
DO:
    ASSIGN 
        lChkImage    = YES
        ls-full-img1 = SEARCH(ls-full-img1)
        ls-full-img1 = cRtnChar + ">".
END.
ELSE 
DO:
    ASSIGN 
        ls-image1           = SEARCH("images\premierinv.jpg")
        FILE-INFO:FILE-NAME = ls-image1.
    ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".
END.

FIND FIRST oe-ctrl WHERE oe-ctrl.company = cocode NO-LOCK NO-ERROR.

FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,
    FIRST ar-inv WHERE RECID(ar-inv) EQ report.rec-id NO-LOCK
    BREAK BY report.key-01
    BY report.key-02:

    FIND FIRST cust WHERE cust.company = ar-inv.company
        AND cust.cust-no = ar-inv.cust-no NO-LOCK NO-ERROR.
    IF ar-inv.sold-name <> "" THEN
        ASSIGN  v-shipto-name    = ar-inv.sold-name
            v-shipto-addr[1] = ar-inv.sold-addr[1]
            v-shipto-addr[2] = ar-inv.sold-addr[2]
            v-shipto-city    = ar-inv.sold-city
            v-shipto-state   = ar-inv.sold-state
            v-shipto-zip     = ar-inv.sold-zip.
    ELSE 
    DO:
        FIND FIRST shipto WHERE shipto.company = ar-inv.company
            AND shipto.cust-no = ar-inv.cust-no
            AND shipto.ship-id = ar-inv.ship-id NO-LOCK NO-ERROR.
        IF AVAILABLE shipto THEN
            ASSIGN  v-shipto-name    = shipto.ship-name
                v-shipto-addr[1] = shipto.ship-addr[1]
                v-shipto-addr[2] = shipto.ship-addr[2]
                v-shipto-city    = shipto.ship-city
                v-shipto-state   = shipto.ship-state
                v-shipto-zip     = shipto.ship-zip.                            
    END.

    v-del-no = 0.

     
    IF ar-inv.inv-date NE ? THEN ASSIGN v-inv-date  = ar-inv.inv-date
            v-date-ship = ar-inv.inv-date.

    IF ar-inv.fob-code BEGINS "ORIG" THEN ASSIGN v-fob = "Origin".
    ELSE ASSIGN v-fob = "Destination".

    FIND FIRST carrier WHERE carrier.company = ar-inv.company AND
        carrier.carrier = ar-inv.carrier NO-LOCK NO-ERROR.
    IF AVAILABLE carrier THEN ASSIGN v-shipvia = carrier.dscr.
    ELSE ASSIGN v-shipvia = "".

    ASSIGN
        v-addr3      = ar-inv.city + ", " + ar-inv.state + "  " + ar-inv.zip
        v-sold-addr3 = v-shipto-city + ", " + v-shipto-state +
              "  " + v-shipto-zip
        v-line       = 1
        v-printline  = 0.

    IF AVAILABLE cust  THEN
        ASSIGN v-email = cust.email.
    ELSE v-email = "" .
    
    FIND FIRST stax
        {sys/ref/stax1W.i}
        AND {sys/ref/taxgroup.i stax} EQ ar-inv.tax-code
    NO-LOCK NO-ERROR.
    IF NOT AVAILABLE stax THEN
        FIND FIRST stax WHERE stax.tax-group EQ ar-inv.tax-code
            NO-LOCK NO-ERROR.
   
    IF AVAILABLE stax THEN
        ASSIGN v-tax-rate    = stax.tax-rate[1] +
                              stax.tax-rate[2] + stax.tax-rate[3]
            v-tax-code[1] = stax.tax-code[1]
            v-tax-code[2] = stax.tax-code[2]
            v-tax-code[3] = stax.tax-code[3]
            v-tx-rate[1]  = stax.tax-rate[1]
            v-tx-rate[2]  = stax.tax-rate[2]
            v-tx-rate[3]  = stax.tax-rate[3].

    ASSIGN 
        v-tot-pallets = 0.

    FOR EACH ar-invl NO-LOCK WHERE ar-invl.x-no EQ ar-inv.x-no AND
        (s-print-zero-qty OR
        NOT(ar-invl.ship-qty EQ 0 AND ar-invl.inv-qty EQ 0))
        BREAK BY ar-invl.i-no:
        DO i = 1 TO 3:
            IF ar-invl.sman[i] NE "" THEN 
            DO:
                CREATE w-sman.
                ASSIGN 
                    w-sman.sman = ar-invl.sman[i].
            END.
        END.
        ASSIGN 
            v-tot-qty     = v-tot-qty + ar-invl.ship-qty
            v-t-weight    = v-t-weight + (ROUND(ar-invl.t-weight /
                              ar-invl.qty, 2) * ar-invl.inv-qty)
            v-tot-pallets = 0.
        v-pc = "C". /* complete*/

        IF LAST-OF(ar-invl.i-no) THEN 
        DO:
            IF ar-invl.est-no NE "" THEN
            DO:
                FIND FIRST eb WHERE eb.company = ar-invl.company AND
                    eb.est-no = ar-invl.est-no AND
                    eb.e-num = ar-invl.e-num AND
                    eb.form-no = ar-invl.form-no AND
                    eb.blank-no = ar-invl.blank-no NO-LOCK NO-ERROR.

                IF ar-invl.form-no = 0 AND ar-invl.est-type = 2 THEN
                DO:
                    FOR EACH fg-set NO-LOCK WHERE fg-set.company = ar-invl.company
                        AND fg-set.set-no = ar-invl.i-no:
                        ASSIGN 
                            v-set-qty = v-set-qty + fg-set.qtyPerSet.
                    END.
                    IF v-set-qty = 0 THEN
                        ASSIGN v-set-qty = 1.
                    FOR EACH eb NO-LOCK WHERE eb.company = ar-invl.company AND
                        eb.est-no = ar-invl.est-no AND
                        eb.e-num = ar-invl.e-num AND
                        eb.form-no NE 0:
                        FIND fg-set WHERE fg-set.company = ar-invl.company AND
                            fg-set.set-no = ar-invl.i-no  AND
                            fg-set.part-no = eb.stock-no NO-LOCK NO-ERROR.

                        IF AVAILABLE fg-set AND fg-set.qtyPerSet NE 0 THEN
                            ASSIGN v-part-qty = fg-set.qtyPerSet / v-set-qty.
                        ELSE
                            ASSIGN v-part-qty = 1 / v-set-qty.


                        IF eb.cas-cnt = 0 THEN
                            ASSIGN v-tot-cas = ROUND((v-t-weight * v-part-qty) /
                                      eb.cas-wt, 2).
                        ELSE
                            ASSIGN v-tot-cas = ROUND((v-tot-qty * v-part-qty) /
                                      eb.cas-cnt, 2).
                        IF v-bol-cases NE 0 THEN
                            ASSIGN v-tot-cas = v-bol-cases.
                 
                    END. /* each eb */
                END. /* do */
                ELSE
                    IF AVAILABLE eb THEN
                    DO:
                        IF eb.cas-cnt = 0 THEN
                            ASSIGN v-tot-cas = ROUND(v-t-weight / eb.cas-wt, 2).
                        ELSE
                            ASSIGN v-tot-cas = ROUND(v-tot-qty / eb.cas-cnt, 2).
                        IF v-bol-cases NE 0 THEN
                            ASSIGN v-tot-cas = v-bol-cases.
                    /***
                    ASSIGN v-tot-pallets = v-tot-pallets +
                        ROUND((v-tot-cas  / eb.cas-pal) + .49, 0).
                    ***/
                    END. /* do */
            END. /* est-no ne "" */
            ASSIGN
                v-t-weight = 0
                v-tot-cas  = 0
                v-tot-qty  = 0.
        END. /* last-of i-no */
    END. /* each ar-invl */
    
    /** Build Salesman Id String **/
    v-salesman = "".
    FOR EACH w-sman BREAK BY w-sman.sman:
        IF FIRST-OF(w-sman.sman) THEN
            ASSIGN v-salesman = v-salesman + w-sman.sman.
        DELETE w-sman.
    END.
 
    ASSIGN 
        v-po-no    = ar-inv.po-no
        v-bill-i   = ar-inv.bill-i[1]
        v-ord-no   = ar-inv.ord-no
        v-ord-date = ar-inv.ord-date.
    IF v-salesman = "" THEN 
    DO:
        FIND FIRST oe-ord WHERE oe-ord.company EQ cocode
            AND oe-ord.ord-no   EQ ar-inv.ord-no
            NO-LOCK NO-ERROR.
        IF AVAILABLE oe-ord THEN 
        DO:
            FIND FIRST sman WHERE sman.company = cocode AND
                sman.sman = oe-ord.sman[1] NO-LOCK NO-ERROR.
            v-salesman = oe-ord.sman[1].
            IF AVAILABLE sman THEN v-salesname = sman.sname.
        END.
        IF v-salesman = "" THEN 
        DO:
            v-salesman = cust.sman.
            FIND FIRST sman WHERE sman.company = cocode AND
                sman.sman = cust.sman NO-LOCK NO-ERROR.              
            IF AVAILABLE sman THEN v-salesname = sman.sname.
        END.
    END.
        
    FIND FIRST ar-invl WHERE ar-invl.x-no = ar-inv.x-no NO-LOCK NO-ERROR.
    IF AVAILABLE ar-invl THEN
    DO:
        ASSIGN 
            v-price-head = ar-invl.pr-uom
            v-po-no      = ar-invl.po-no                  
            v-ord-no     = ar-invl.ord-no
            lv-bol-no    = ar-invl.bol-no.
        .
    END.

    {ar/rep/invport.i}  /* xprint form */

    ASSIGN
        v-subtot-lines = 0
        v-t-tax        = 0.
    FOR EACH ar-invl NO-LOCK WHERE ar-invl.x-no = ar-inv.x-no AND
        (s-print-zero-qty OR
        NOT(ar-invl.ship-qty EQ 0 AND ar-invl.inv-qty EQ 0)):
        ASSIGN 
            v-case-line = ""
            v-part-line = ""
            v-case-cnt  = "".
          
        IF v-printline > 43 THEN 
        DO:           
            PAGE.
            {ar/rep/invport.i}  /* xprint form */
            v-printline = 21.
        END.

        FIND FIRST oe-ordl WHERE oe-ordl.company = cocode AND
            oe-ordl.ord-no = ar-invl.ord-no AND
            oe-ordl.i-no = ar-invl.i-no AND
            oe-ordl.LINE = ar-invl.LINE
            NO-LOCK NO-ERROR.
        IF AVAILABLE oe-ordl THEN
            ASSIGN v-bo-qty = IF (ar-invl.qty - ar-invl.ship-qty -
                                    oe-ordl.t-ship-qty) < 0 then 0 else
                                   (ar-invl.qty - ar-invl.ship-qty -
                                    oe-ordl.t-ship-qty).
        ELSE
            ASSIGN v-bo-qty = IF ( ar-invl.qty - ar-invl.ship-qty ) < 0
                                  then 0 else ar-invl.qty - ar-invl.ship-qty.

        ASSIGN 
            v-inv-qty      = ar-invl.qty
            v-ship-qty     = ar-invl.ship-qty
            v-i-no         = ar-invl.i-no
            v-i-dscr       = ar-invl.i-name
            v-price        = ar-invl.unit-pr * (1 - (ar-invl.disc / 100))
            v-t-price      = ar-invl.amt
            v-subtot-lines = v-subtot-lines + ar-invl.amt.
        /* PremierS switch*/
        IF ip-print-s THEN v-inv-qtys = ar-invl.inv-qty / ar-invl.cas-cnt.
           
        IF ar-invl.tax AND AVAILABLE stax THEN
        DO i = 1 TO 3:
            IF stax.tax-code[i] NE "" THEN 
            DO:
                CREATE w-tax.
                ASSIGN
                    w-dsc      = stax.tax-dscr[i]
                    w-tax      = ROUND((IF stax.accum-tax THEN v-t-price
                                                           ELSE ar-invl.amt) *
                                        stax.tax-rate[i] / 100,2)
                    v-t-price  = v-t-price + w-tax
                    v-t-tax[i] = v-t-tax[i] + w-tax
                    v-lines    = v-lines + 1.
            END.
        END.
           
        IF v-t-price NE ar-invl.amt THEN 
        DO:
            CREATE w-tax.
            ASSIGN
                w-dsc   = "******ITEM TOTAL:"
                w-tax   = v-t-price
                v-lines = v-lines + 1.
        END.

        ASSIGN 
            v-po-no      = ar-invl.po-no
            v-ord-no     = ar-invl.ord-no
            v-price-head = ar-invl.pr-uom.
        v-i-dscr2 = ar-invl.part-dscr1.
        IF v-i-dscr2 = "" THEN v-i-dscr2 = ar-invl.i-dscr.
        IF v-ord-no = 0 AND v-ship-qty = 0 THEN v-ship-qty = v-inv-qty.
        PUT SPACE(1)
            v-po-no 
            ar-invl.part-no  SPACE(1)
            v-i-dscr FORM "x(30)". 
        /* PremierS switch*/
        IF ip-print-s THEN
            PUT v-inv-qtys  FORMAT "->>9.99" SPACE(2).
        ELSE PUT v-ship-qty  FORMAT "->>>>>9" SPACE(2).

        PUT v-price  FORMAT "->>,>>9.999999"                
            ar-invl.amt  FORMAT "->>>,>>9.99"                
            SKIP
            SPACE(1)
            TRIM(STRING(v-ord-no,">>>>>>>9")) SPACE(7)
            ar-invl.i-no SPACE(1)
            v-i-dscr2  SPACE(11)
            v-pc  FORM "x" SPACE(7)
            v-price-head SKIP.
                
        v-printline = v-printline + 2.
             
        PUT SKIP(1).
        v-printline = v-printline + 1.
            
            
    END. /* each ar-invl */

    IF v-prntinst THEN
    DO i = 1 TO 4:
        IF ar-inv.bill-i[i] NE "" THEN 
        DO:
       
            PUT ar-inv.bill-i[i] AT 10 SKIP.
            ASSIGN 
                v-printline = v-printline + 1.
        END.
    END. /* 1 to 4 */
        
    IF v-printline > 43 THEN 
    DO:           
        PAGE.
        {ar/rep/invport.i}  /* xprint form */
        v-printline = 21.
    END.
    ASSIGN 
        v-notes       = ""
        v-notes-line  = 0
        lv-line-chars = 80.

    {custom/notesprtA.i ar-inv v-notes 4}

    ASSIGN
        v-frt-tax     = ar-inv.freight
        v-inv-freight = IF ar-inv.f-bill THEN ar-inv.freight
/*                         ELSE IF ar-inv.freight <> 0 THEN ar-inv.freight */
                        ELSE 0.

    IF ar-inv.tax-code <> "" AND
        ar-inv.f-bill AND ar-inv.freight <> 0 AND AVAILABLE stax THEN
    DO i = 1 TO 3:

        IF stax.tax-code[i] NE "" THEN 
        DO:
            CREATE w-tax.
            ASSIGN
                w-dsc      = stax.tax-dscr[i]
                w-tax      = ROUND((IF stax.company EQ "yes" THEN v-frt-tax
                                                         ELSE ar-inv.freight) *
                                        stax.tax-rate[i] / 100,2)                 
                v-frt-tax  = v-frt-tax + w-tax
                v-t-tax[i] = v-t-tax[i] + w-tax
                v-lines    = v-lines + 1.
        END.
    END.      


    DO i = 1 TO 3:
        v-bot-lab[i] = IF v-t-tax[i] NE 0 THEN
            ((IF AVAILABLE stax THEN STRING(CAPS(stax.tax-code[i]),"x(5)") 
            ELSE FILL(" ",5) ) +
            fill(" ",6) + ":" +
            string(v-t-tax[i],"->>>>>9.99")) ELSE "".
    END.
                        
    v-inv-total = v-subtot-lines + /*v-t-tax[1] + v-t-tax[2] + v-t-tax[3] AH 03-10-10 */ ar-inv.tax-amt + v-inv-freight
        .

    PUT "<R58><C58.5><#8><FROM><R+5><C+23><RECT> " 
        "<=8> Subtotal do    :" v-subtot-lines FORM "->>,>>9.99"
        "<=8><R+1> imposto sobre  :" v-inv-freight
        "<=8><R+2> " cTaxCode FORMAT "x(15)" ":" ar-inv.tax-amt FORM "->>,>>9.99"
        "<=8><R+3>" "" 
        "<=8><R+4> Fatura         :" v-inv-total FORM "->>,>>9.99" . /* ar-inv.gross*/


    PUT "<FArial><R58><C1><#9><P12><B> OBRIGADO. </B> <P9> " SKIP
        "<R60><C1><P12> Todas as moedas exibidas em " cCurCode FORMAT "x(3)" ". <P9> " SKIP
        "<=9><R-6>" v-notes[1]
        "<=9><R-5>" v-notes[2]
        "<=9><R-4>" v-notes[3]
        "<=9><R-3>" v-notes[4]
        .
    v-printline = v-printline + 8.

   
    IF v-printline <= 66 THEN PAGE.
     
    DO TRANSACTION:
        FIND FIRST xar-inv WHERE RECID(xar-inv) = RECID(ar-inv).
        ASSIGN 
            xar-inv.printed = YES.
        xar-inv.stat = "X".
    END. /* DO TRANSACTION avail ar-inv */ 
END. /* each ar-inv */

/* END ---------------------------------- copr. 1996 Advanced Software, Inc. */
