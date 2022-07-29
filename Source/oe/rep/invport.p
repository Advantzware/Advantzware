/* ---------------------------------------------- oe/rep/invport.p */
/* PRINT INVOICE   Xprint form for Premier Pkg  (PremierX and PremierS)       */
/* -------------------------------------------------------------------------- */
/* Mod: Ticket - 103137 (Format Change for Order No. and Job No). */
DEFINE INPUT PARAMETER ip-copy-title AS cha NO-UNDO.
DEFINE INPUT PARAMETER ip-print-s AS LOG NO-UNDO. /* for PremierS */
{sys/inc/var.i shared}
//{system/TaxProcs.i}
{oe/rep/invoice.i}

DEFINE        VARIABLE v-salesman       AS CHARACTER FORMAT "x(14)" NO-UNDO.
DEFINE        VARIABLE v-salesname      AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE        VARIABLE v-fob            AS CHARACTER FORMAT "x(27)" NO-UNDO.
DEFINE        VARIABLE v-shipvia        LIKE carrier.dscr NO-UNDO. 
DEFINE        VARIABLE v-addr3          AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE        VARIABLE v-email          AS CHARACTER FORMAT "x(130)" NO-UNDO.
DEFINE        VARIABLE v-sold-addr3     AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE        VARIABLE v-shipto-name    AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE        VARIABLE v-shipto-addr    AS CHARACTER FORMAT "x(30)" EXTENT 2 NO-UNDO.
DEFINE        VARIABLE v-shipto-city    AS CHARACTER FORMAT "x(15)" NO-UNDO.
DEFINE        VARIABLE v-shipto-state   AS CHARACTER FORMAT "x(2)" NO-UNDO.
DEFINE        VARIABLE v-shipto-zip     AS CHARACTER FORMAT "x(10)" NO-UNDO.
DEFINE        VARIABLE v-line           AS INTEGER   NO-UNDO.
DEFINE        VARIABLE v-printline      AS INTEGER   NO-UNDO.
DEFINE        VARIABLE v-t-weight       LIKE inv-line.t-weight NO-UNDO.
DEFINE        VARIABLE v-tot-cas        AS DECIMAL   FORMAT "->>>9.9999" NO-UNDO.
DEFINE        VARIABLE v-tot-pallets    AS INTEGER   NO-UNDO.
DEFINE        VARIABLE v-tot-qty        AS INTEGER   NO-UNDO.
DEFINE        VARIABLE v-inv-date       AS DATE      INITIAL TODAY FORM "99/99/9999" NO-UNDO.
DEFINE SHARED VARIABLE v-fr-tax         AS LOGICAL   INITIAL NO NO-UNDO.
DEFINE SHARED VARIABLE s-print-zero-qty AS LOG       NO-UNDO.
DEFINE        VARIABLE v-tax-rate       AS DECIMAL   FORMAT "->>>.99" NO-UNDO.
DEFINE        VARIABLE v-tax-code       LIKE stax.tax-code NO-UNDO.
DEFINE        VARIABLE v-tx-rate        LIKE stax.tax-rate NO-UNDO.
DEFINE        VARIABLE v-ans            AS LOGICAL   INITIAL NO NO-UNDO.
DEFINE        VARIABLE v-date-ship      AS DATE      INITIAL TODAY NO-UNDO.
DEFINE        VARIABLE v-date-ord       AS DATE      NO-UNDO.
DEFINE        VARIABLE v-del-no         AS INTEGER   FORMAT ">>>>>>" NO-UNDO.
DEFINE        VARIABLE v-bol-cases      LIKE oe-boll.cases NO-UNDO.
DEFINE        VARIABLE v-set-qty        AS DECIMAL   NO-UNDO.
DEFINE        VARIABLE v-part-qty       AS DECIMAL   FORMAT "999.9999" NO-UNDO.
DEFINE        VARIABLE v-net            LIKE inv-head.t-inv-rev NO-UNDO.
DEFINE        VARIABLE v-case-cnt       AS CHARACTER FORMAT "x(80)" EXTENT 5 NO-UNDO.
DEFINE        VARIABLE v-case-line      AS CHARACTER NO-UNDO.
DEFINE        VARIABLE v-part-line      AS CHARACTER NO-UNDO.
DEFINE        VARIABLE v-pc             AS cha       NO-UNDO. /* partial or complete */
DEFINE        VARIABLE dTotalSalesTax   AS DECIMAL   NO-UNDO.
DEFINE        VARIABLE cCaseUOMList     AS CHARACTER NO-UNDO.
DEFINE BUFFER xinv-head  FOR inv-head .
DEFINE BUFFER xinv-line  FOR inv-line .
DEFINE BUFFER b-inv-head FOR inv-head.

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
DEFINE VARIABLE v-po-no        LIKE inv-line.po-no NO-UNDO.
DEFINE VARIABLE v-bill-i       AS CHARACTER FORMAT "x(25)" NO-UNDO.
DEFINE VARIABLE v-ord-no       LIKE oe-ord.ord-no NO-UNDO.
DEFINE VARIABLE v-ord-date     LIKE oe-ord.ord-date NO-UNDO.
DEFINE VARIABLE v-ship-i       AS CHARACTER FORMAT "x(25)" NO-UNDO.
DEFINE VARIABLE v-rel-po-no    LIKE oe-rel.po-no NO-UNDO.
DEFINE VARIABLE v-price-head   AS CHARACTER FORMAT "x(5)" NO-UNDO.
DEFINE VARIABLE v-subtot-lines AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-inv-freight  LIKE inv-head.t-inv-freight NO-UNDO.

FIND FIRST inv-head NO-LOCK NO-ERROR.
/* === with xprint ====*/
DEFINE VARIABLE ls-image1        AS cha       NO-UNDO.
DEFINE VARIABLE ls-full-img1     AS cha       FORM "x(200)" NO-UNDO.
/*ASSIGN ls-image1 = "images\premiercan.jpg"          */
/*       FILE-INFO:FILE-NAME = ls-image1.             */
/*       ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".*/

DEFINE VARIABLE v-tel            AS cha       FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-fax            AS cha       FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-contact        AS cha       FORM "x(20)" NO-UNDO .

DEFINE VARIABLE v-comp-add1      AS cha       FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add2      AS cha       FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add3      AS cha       FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add4      AS cha       FORM "x(30)" NO-UNDO.

DEFINE VARIABLE iDaysToPay       AS INTEGER   NO-UNDO.
DEFINE VARIABLE cInvDate         AS CHARACTER NO-UNDO.
DEFINE VARIABLE dOrigQty         AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cOrigUOM         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cXMLShipTo       AS CHARACTER NO-UNDO.

DEFINE VARIABLE cRtnChar         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lChkImage        AS LOGICAL   NO-UNDO. 
DEFINE VARIABLE cTaxCode         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCurCode         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCompanyID       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lValid           AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage         AS CHARACTER NO-UNDO.
/*DEFINE VARIABLE dFrtTaxAmt            AS DECIMAL   NO-UNDO.*/
/*DEFINE VARIABLE dFrtTaxRate           AS DECIMAL   NO-UNDO.*/
DEFINE VARIABLE lFirstLine       AS LOG       NO-UNDO.
//DEFINE VARIABLE dTaxTotal             AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dInvoiceTotal    AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dInvoiceSubTotal AS DECIMAL   NO-UNDO.
DEFINE VARIABLE lSuccess         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cInvSuffix       AS CHARACTER NO-UNDO.
DEFINE VARIABLE opcDateStringInvDate  AS CHARACTER NO-UNDO.
DEFINE VARIABLE opcDateStringShipDate AS CHARACTER NO-UNDO.

ASSIGN 
    cInvSuffix = ip-copy-title.

FUNCTION fRoundUp RETURNS DECIMAL ( ipdNum AS DECIMAL ):
    DEFINE VARIABLE dNumTwoRight AS DECIMAL NO-UNDO.
    dNumTwoRight = ipdNum * 100.
    IF dNumTwoRight = TRUNCATE( dNumTwoRight, 0 ) THEN
        RETURN INTEGER( dNumTwoRight ) / 100.
    ELSE
        RETURN INTEGER( TRUNCATE( dNumTwoRight, 0 ) + 1 ) / 100.

END.

/* rstark 05181205 */
{XMLOutput/XMLOutput.i &XMLOutput=XMLInvoice &Company=cocode}
RUN XMLOutput (lXMLOutput,'','','Header').
/* rstark 05181205 */

/* rstark 05291402 */
&SCOPED-DEFINE sysCtrlcXML cXMLInvoice
{XMLOutput/XMLOutput.i &cXMLOutput={&sysCtrlcXML} &Company=cocode &c=c}
/* rstark 05291402 */
    
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
    ELSE IF company.company EQ '005' THEN
            ASSIGN 
                cCurCode   = 'USD'
                cTaxCode   = 'vendas do frete'
                cCompanyID = ''
                .
        ELSE 
            ASSIGN 
                cCurCode   = 'USD'
                cTaxCode   = 'vendas do frete'
                cCompanyID = ''
                .
RUN sys/ref/nk1look.p (INPUT company.company, "CaseUOMList", "C" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cRtnChar, OUTPUT lRecFound).
cCaseUomList = cRtnChar.

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

    
ASSIGN 
    v-comp-add1 = ""
    v-comp-add2 = ""
    v-comp-add3 = " "
    v-comp-add4 = ""
    .
FIND FIRST oe-ctrl WHERE oe-ctrl.company = cocode NO-LOCK NO-ERROR.

FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,
    FIRST xinv-head WHERE RECID(xinv-head) EQ report.rec-id NO-LOCK
    BREAK BY report.key-01
    BY (IF v-sort THEN "" ELSE report.key-02)
    BY report.key-03:
                  
    FIND FIRST cust WHERE cust.company = xinv-head.company
        AND cust.cust-no = xinv-head.cust-no NO-LOCK NO-ERROR.

    /* rstark 05291402 */
    IF ccXMLOutput NE '' THEN 
    DO:
        FIND FIRST sys-ctrl NO-LOCK
            WHERE sys-ctrl.company EQ  cocode
            AND sys-ctrl.name    EQ 'cXMLInvoice' NO-ERROR.
        IF AVAILABLE sys-ctrl AND sys-ctrl.log-fld THEN 
        DO:
            FIND FIRST sys-ctrl-shipto OF sys-ctrl NO-LOCK
                WHERE sys-ctrl-shipto.cust-vend EQ YES
                AND sys-ctrl-shipto.cust-vend-no EQ xinv-head.cust-no
                AND sys-ctrl-shipto.log-fld EQ YES
                NO-ERROR.
            IF AVAILABLE sys-ctrl-shipto THEN 
                ASSIGN 
                    cXMLIdentity = sys-ctrl-shipto.char-fld
                    cXMLDTD      = 'http://xml.cxml.org/schemas/cXML/1.2.025/InvoiceDetail.dtd'.
        END.
        {XMLOutput/cXMLCust.i &cXMLSysCtrl={&sysCtrlcXML} &Company=xinv-head.company &Customer=xinv-head.cust-no}
        FIND FIRST terms 
            WHERE terms.company EQ xinv-head.company
            AND terms.t-code EQ xinv-head.terms
            NO-LOCK NO-ERROR.
        IF AVAILABLE terms THEN
            iDaysToPay = terms.net-days.
        /*changing to today to handle Ariba 3 day constraints*/
        cInvDate = STRING(YEAR(TODAY),'9999')
            + '-'
            + STRING(MONTH(TODAY),'99')
            + '-'
            + STRING(DAY(TODAY),'99')
            + 'T'
            + STRING(0,'hh:mm:ss')
            + '-05:00'.
    END.
    /* rstark 05291402 */
      
    ASSIGN  
        v-shipto-name    = xinv-head.sold-name
        v-shipto-addr[1] = xinv-head.sold-addr[1]
        v-shipto-addr[2] = xinv-head.sold-addr[2]
        v-shipto-city    = xinv-head.sold-city
        v-shipto-state   = xinv-head.sold-state
        v-shipto-zip     = xinv-head.sold-zip
        cXMLShipTo       = xinv-head.sold-no.

    v-del-no = 0.

    FIND FIRST oe-bolh WHERE oe-bolh.company = xinv-head.company AND
        oe-bolh.bol-no = xinv-head.bol-no USE-INDEX bol-no NO-LOCK NO-ERROR.
    IF AVAILABLE oe-bolh THEN 
    DO:
        /*   find first oe-relh where oe-relh.company = oe-bolh.company and
                      oe-relh.r-no = oe-bolh.r-no no-lock no-error.
           if avail oe-relh then */
        FIND FIRST shipto WHERE shipto.company  = oe-bolh.company AND
            shipto.cust-no = oe-bolh.cust-no AND
            shipto.ship-id = oe-bolh.ship-id NO-LOCK NO-ERROR.
        IF AVAILABLE shipto THEN
            ASSIGN  v-shipto-name    = shipto.ship-name
                v-shipto-addr[1] = shipto.ship-addr[1]
                v-shipto-addr[2] = shipto.ship-addr[2]
                v-shipto-city    = shipto.ship-city
                v-shipto-state   = shipto.ship-state
                v-shipto-zip     = shipto.ship-zip
                cXMLShipTo       = shipto.ship-id.

        FIND FIRST sys-ctrl-shipto NO-LOCK
            WHERE sys-ctrl-shipto.company EQ xinv-head.company
            AND sys-ctrl-shipto.NAME EQ 'cXMLShipToPrefix'
            AND sys-ctrl-shipto.cust-vend EQ YES
            AND sys-ctrl-shipto.cust-vend-no EQ xinv-head.cust-no
            NO-ERROR.
        IF AVAILABLE sys-ctrl-shipto AND sys-ctrl-shipto.char-fld NE '' THEN 
            cXMLShipTo = TRIM(sys-ctrl-shipto.char-fld) + oe-bolh.ship-id.
    END. /* avail oe-bolh */  
    IF /*NOT v-reprint OR*/ xinv-head.inv-no EQ 0 THEN
        RUN oe/get-inv#.p (ROWID(xinv-head)).

    DO TRANSACTION:
        FIND inv-head WHERE ROWID(inv-head) EQ ROWID(xinv-head).
        
        IF inv-head.inv-date NE ? THEN v-inv-date = inv-head.inv-date.

        IF inv-head.fob-code BEGINS "ORIG" THEN
            ASSIGN v-fob = "Origin".
        ELSE
            ASSIGN v-fob = "Destination".

        FIND FIRST carrier WHERE carrier.company = inv-head.company AND
            carrier.carrier = inv-head.carrier NO-LOCK NO-ERROR.
        IF AVAILABLE carrier THEN
            ASSIGN v-shipvia = carrier.dscr.
        ELSE
            ASSIGN v-shipvia = "".
        ASSIGN
            v-addr3      = inv-head.city + ", " + inv-head.state + "  " + inv-head.zip
            v-sold-addr3 = v-shipto-city + ", " + v-shipto-state +
              "  " + v-shipto-zip
            v-line       = 1
            v-printline  = 0.
        IF AVAILABLE cust  THEN
            ASSIGN v-email = cust.email.
        ELSE v-email = "" .
    
        FIND FIRST stax
            {sys/ref/stax1W.i}
            AND {sys/ref/taxgroup.i stax} EQ inv-head.tax-gr
        NO-LOCK NO-ERROR.
        IF NOT AVAILABLE stax THEN
            FIND FIRST stax WHERE stax.tax-group EQ inv-head.tax-gr
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
        FOR EACH xinv-line NO-LOCK WHERE xinv-line.r-no = inv-head.r-no AND
            (s-print-zero-qty OR
            NOT(xinv-line.ship-qty EQ 0 AND xinv-line.inv-qty EQ 0))
            BREAK BY xinv-line.i-no:

            DO i = 1 TO 3:
                IF xinv-line.sman[i] NE "" THEN 
                DO:
                    CREATE w-sman.
                    ASSIGN 
                        w-sman.sman = xinv-line.sman[i].
                END.
            END.
            ASSIGN 
                v-tot-qty     = v-tot-qty + xinv-line.ship-qty
                v-t-weight    = v-t-weight + (ROUND(xinv-line.t-weight /
                            xinv-line.qty, 2) * xinv-line.inv-qty)
                v-tot-pallets = 0
                v-pc          = "C". /* complete*/
            FOR EACH oe-bolh NO-LOCK WHERE oe-bolh.b-no = xinv-line.b-no 
            /*oe-bolh.ord-no = xinv-line.ord-no*/ :
                v-pc = "P". /* partial*/ 
                FOR EACH oe-boll FIELDS(cases partial p-c) NO-LOCK WHERE
                    oe-boll.company = oe-bolh.company AND
                    oe-boll.b-no = oe-bolh.b-no AND
                    oe-boll.i-no = xinv-line.i-no AND
                    oe-boll.ord-no = xinv-line.ord-no:

                    /** Bill Of Lading TOTAL CASES **/
                    ASSIGN 
                        v-bol-cases   = v-bol-cases + oe-boll.cases
                        v-tot-pallets = v-tot-pallets + oe-boll.cases +
                                     (IF oe-boll.partial GT 0 THEN 1 ELSE 0).
                    IF oe-boll.p-c THEN v-pc = "C". /*complete*/
              
                END. /* each oe-boll */
                ASSIGN 
                    v-date-ship = oe-bolh.bol-date
                    /* v-tot-pallets = v-tot-pallets + v-bol-cases +
                                     (if oe-boll.partial gt 0 then 1 else 0) */.
            END. /* each oe-bolh */
         
            IF LAST-OF(xinv-line.i-no) THEN 
            DO:
                IF xinv-line.est-no NE "" THEN
                DO:
                    FIND FIRST eb WHERE eb.company = xinv-line.company AND
                        eb.est-no = xinv-line.est-no AND
                        eb.e-num = xinv-line.e-num AND
                        eb.form-no = xinv-line.form-no AND
                        eb.blank-no = xinv-line.blank-no NO-LOCK NO-ERROR.

                    IF xinv-line.form-no = 0 AND xinv-line.est-type = 2 THEN
                    DO:
                        FOR EACH fg-set NO-LOCK WHERE fg-set.company = xinv-line.company
                            AND fg-set.set-no = xinv-line.i-no:
                            ASSIGN 
                                v-set-qty = v-set-qty + fg-set.QtyPerSet.
                        END.
                        IF v-set-qty = 0 THEN
                            ASSIGN v-set-qty = 1.
                        FOR EACH eb NO-LOCK WHERE eb.company = xinv-line.company AND
                            eb.est-no = xinv-line.est-no AND
                            eb.e-num = xinv-line.e-num AND
                            eb.form-no NE 0:
                            FIND fg-set WHERE fg-set.company = xinv-line.company AND
                                fg-set.set-no = xinv-line.i-no  AND
                                fg-set.part-no = eb.stock-no NO-LOCK NO-ERROR.

                            IF AVAILABLE fg-set AND fg-set.QtyPerSet NE 0 THEN
                                ASSIGN v-part-qty = fg-set.QtyPerSet / v-set-qty.
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
                        /***
                        ASSIGN v-tot-pallets = v-tot-pallets +
                             ROUND((v-tot-cas  / eb.cas-pal) + .49, 0).
                        ***/
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
        END. /* each xinv-line */
    
        /** Build Salesman Id String **/
        v-salesman = "".
        FOR EACH w-sman BREAK BY w-sman.sman:
            IF FIRST-OF(w-sman.sman) THEN
                ASSIGN v-salesman = v-salesman + w-sman.sman.
            DELETE w-sman.
        END.

        FIND FIRST oe-bolh WHERE oe-bolh.company = inv-head.company AND
            oe-bolh.bol-no = inv-head.bol-no
            USE-INDEX bol-no NO-LOCK NO-ERROR.
        IF AVAILABLE oe-bolh THEN
            ASSIGN v-rel-po-no = oe-bolh.po-no.

        FIND FIRST inv-line WHERE inv-line.r-no = inv-head.r-no
            NO-LOCK NO-ERROR.
        IF AVAILABLE inv-line THEN
        DO:
            ASSIGN 
                v-price-head = inv-line.pr-uom.
            FIND FIRST oe-ord WHERE oe-ord.company = cocode AND
                oe-ord.ord-no = inv-line.ord-no
                NO-LOCK NO-ERROR.
            IF AVAILABLE oe-ord THEN 
            DO:
                ASSIGN 
                    v-po-no       = oe-ord.po-no
                    v-bill-i      = oe-ord.bill-i[1]
                    v-ord-no      = oe-ord.ord-no
                    v-ord-date    = oe-ord.ord-date
                    cXMLPayloadID = oe-ord.spare-char-3
                    cXMLProcessID = STRING(oe-ord.ord-no)
                    .
            
            END.
            ELSE
                ASSIGN v-price-head = inv-line.pr-uom.
        END.
        

        
        /* rstark 05181205 */
        XMLLineNumber = 0.
        RUN XMLOutput (lXMLOutput,'InvoiceHeader','','Row').
        RUN XMLOutput (lXMLOutput,'RemitTo','PREMIER PACKAGING','Col').
        RUN XMLOutput (lXMLOutput,'Remit_1','3254 RELIABLE PARKWAY','Col').
        RUN XMLOutput (lXMLOutput,'Remit_2','CHICAGO, IL 60686','Col').
        RUN XMLOutput (lXMLOutput,'Customer',inv-head.cust-name,'Col').
        RUN XMLOutput (lXMLOutput,'Bill_1',inv-head.addr[1],'Col').
        RUN XMLOutput (lXMLOutput,'Bill_2',inv-head.addr[2],'Col').
        RUN XMLOutput (lXMLOutput,'Bill_3',v-addr3,'Col').
        RUN XMLOutput (lXMLOutput,'EMail',v-email,'Col').
        RUN XMLOutput (lXMLOutput,'ShipName',v-shipto-name,'Col').
        RUN XMLOutput (lXMLOutput,'Ship_1',v-shipto-addr[1],'Col').
        RUN XMLOutput (lXMLOutput,'Ship_2',v-shipto-addr[2],'Col').
        RUN XMLOutput (lXMLOutput,'Ship_3',v-sold-addr3,'Col').
        RUN XMLOutput (lXMLOutput,'InvoiceNo',STRING(inv-head.inv-no) + cInvSuffix,'Col').
        RUN XMLOutput (lXMLOutput,'InvoiceDate',v-inv-date,'Col').
        RUN XMLOutput (lXMLOutput,'ShipDate',v-date-ship,'Col').
        RUN XMLOutput (lXMLOutput,'FOB',v-fob,'Col').
        RUN XMLOutput (lXMLOutput,'ShipVia',v-shipvia,'Col').
        RUN XMLOutput (lXMLOutput,'Terms',xinv-head.terms-d,'Col').
        RUN XMLOutput (lXMLOutput,'SalesRep',v-salesname,'Col').
        RUN XMLOutput (lXMLOutput,'BOL',xinv-head.bol-no,'Col').
        RUN XMLOutput (lXMLOutput,'/InvoiceHeader','','Row').
        XMLPage = NO.
        /* rstark 05181205 */

        /* rstark 05291402 */
        cXMLLineNumber = 0. 
        RUN cXMLOutput (clXMLOutput,'Request deploymentMode="' + cXMLProduction + '"','','Row').
        RUN cXMLOutput (clXMLOutput,'InvoiceDetailRequest','','Row').

        RUN cXMLOutput (clXMLOutput,'InvoiceDetailRequestHeader ' 
            + 'invoiceDate="' + cInvDate + '" '
            + 'invoiceID="' + STRING(inv-head.inv-no) + cInvSuffix + '" '
            + 'operation="new" purpose="standard"','','Row').
        RUN cXMLOutput (clXMLOutput,'InvoiceDetailHeaderIndicator/','','Row').  
        RUN cXMLOutput (clXMLOutput,'InvoiceDetailLineIndicator isShippingInLine="yes" isAccountingInLine="yes" /','','Row').
        RUN cXMLOutput (clXMLOutput,'InvoicePartner','','Row').
        RUN cXMLOutput (clXMLOutput,'Contact role="billTo"','','Row').
        RUN cXMLOutput (clXMLOutput,'Name xml:lang="en-US"','','Row').
        RUN cXMLOutput (clXMLOutput,'',inv-head.cust-name,'Col').
        RUN cXMLOutput (clXMLOutput,'/Name','','Row').
        RUN cXMLOutput (clXMLOutput,'PostalAddress','','Row').
        RUN cXMLOutput (clXMLOutput,'Street',inv-head.addr[1],'Col').
        IF inv-head.addr[2] NE '' THEN 
            RUN cXMLOutput (clXMLOutput,'Street',inv-head.addr[2],'Col').
        RUN cXMLOutput (clXMLOutput,'City',inv-head.city,'Col').
        RUN cXMLOutput (clXMLOutput,'State',inv-head.state,'Col').
        RUN cXMLOutput (clXMLOutput,'PostalCode',inv-head.zip,'Col').
        RUN cXMLOutput (clXMLOutput,'Country isoCountryCode="US"','','Row').
        RUN cXMLOutput (clXMLOutput,'','US','Col').
        RUN cXMLOutput (clXMLOutput,'/Country','','Row').
        RUN cXMLOutput (clXMLOutput,'/PostalAddress','','Row').
        RUN cXMLOutput (clXMLOutput,'/Contact','','Row').
        RUN cXMLOutput (clXMLOutput,'/InvoicePartner','','Row').
        RUN cXMLOutput (clXMLOutput,'InvoicePartner','','Row').
        RUN cXMLOutput (clXMLOutput,'Contact addressID="Premier Packaging" role="remitTo"','','Row').
        RUN cXMLOutput (clXMLOutput,'Name xml:lang="en-US"','','Row').
        RUN cXMLOutput (clXMLOutput,'','Premier Packaging','Col').
        RUN cXMLOutput (clXMLOutput,'/Name','','Row').
        RUN cXMLOutput (clXMLOutput,'PostalAddress','','Row').
        RUN cXMLOutput (clXMLOutput,'Street','3254 Reliable Pkwy','Col').
        RUN cXMLOutput (clXMLOutput,'City','Chicago','Col').
        RUN cXMLOutput (clXMLOutput,'State','IL','Col').
        RUN cXMLOutput (clXMLOutput,'PostalCode','60686','Col').
        RUN cXMLOutput (clXMLOutput,'Country isoCountryCode="US"','','Row').
        RUN cXMLOutput (clXMLOutput,'','US','Col').
        RUN cXMLOutput (clXMLOutput,'/Country','','Row').
        RUN cXMLOutput (clXMLOutput,'/PostalAddress','','Row').
        RUN cXMLOutput (clXMLOutput,'/Contact','','Row').
        RUN cXMLOutput (clXMLOutput,'/InvoicePartner','','Row').
        RUN cXMLOutput (clXMLOutput,'InvoiceDetailShipping','','Row').
        RUN cXMLOutput (clXMLOutput,'Contact addressID="' + cXMLShipTo + '" role="shipTo"','','Row').
        RUN cXMLOutput (clXMLOutput,'Name xml:lang="en-US"','','Row').
        RUN cXMLOutput (clXMLOutput,'',v-shipto-name,'Col').
        RUN cXMLOutput (clXMLOutput,'/Name','','Row').
        RUN cXMLOutput (clXMLOutput,'PostalAddress','','Row').
        RUN cXMLOutput (clXMLOutput,'Street',v-shipto-addr[1],'Col').
        IF v-shipto-addr[2] NE "" AND v-shipto-addr[2] NE '345 Court Street' THEN
            RUN cXMLOutput (clXMLOutput,'Street',v-shipto-addr[2],'Col').
        RUN cXMLOutput (clXMLOutput,'City',v-shipto-city,'Col').
        RUN cXMLOutput (clXMLOutput,'State',v-shipto-state,'Col').
        RUN cXMLOutput (clXMLOutput,'PostalCode',v-shipto-zip,'Col').
        RUN cXMLOutput (clXMLOutput,'Country isoCountryCode="US"','','Row').
        RUN cXMLOutput (clXMLOutput,'','US','Col').
        RUN cXMLOutput (clXMLOutput,'/Country','','Row').
        RUN cXMLOutput (clXMLOutput,'/PostalAddress','','Row').
        RUN cXMLOutput (clXMLOutput,'/Contact','','Row').
        RUN cXMLOutput (clXMLOutput,'Contact role="shipFrom"','','Row').
        RUN cXMLOutput (clXMLOutput,'Name xml:lang="en-US"','','Row').
        RUN cXMLOutput (clXMLOutput,'','Premier Packaging','Col').
        RUN cXMLOutput (clXMLOutput,'/Name','','Row').
        RUN cXMLOutput (clXMLOutput,'PostalAddress','','Row').
        RUN cXMLOutput (clXMLOutput,'Street','3900 Produce Road','Col').
        RUN cXMLOutput (clXMLOutput,'City','Louisville','Col').
        RUN cXMLOutput (clXMLOutput,'State','KY','Col').
        RUN cXMLOutput (clXMLOutput,'PostalCode','40218','Col').
        RUN cXMLOutput (clXMLOutput,'Country isoCountryCode="US"','','Row').
        RUN cXMLOutput (clXMLOutput,'','US','Col').
        RUN cXMLOutput (clXMLOutput,'/Country','','Row').
        RUN cXMLOutput (clXMLOutput,'/PostalAddress','','Row').
        RUN cXMLOutput (clXMLOutput,'/Contact','','Row').
        RUN cXMLOutput (clXMLOutput,'/InvoiceDetailShipping','','Row'). 
        RUN cXMLOutput (clXMLOutput,'PaymentTerm payInNumberOfDays="' + STRING(iDaysToPay)  + '" /','','Row').
        /*         RUN cXMLOutput (clXMLOutput,'Extrinsic name="invoiceSubmissionMethod"','','Row'). */
        /*         RUN cXMLOutput (clXMLOutput,'','CSVUpload','Col').                                */
        /*         RUN cXMLOutput (clXMLOutput,'/Extrinsic','','Row').                               */
        RUN cXMLOutput (clXMLOutput,'/InvoiceDetailRequestHeader ','','Row').
        RUN cXMLOutput (clXMLOutput,'InvoiceDetailOrder','','Row').
        RUN cXMLOutput (clXMLOutput,'InvoiceDetailOrderInfo','','Row').
        RUN cXMLOutput (clXMLOutput,'OrderReference orderID="' + v-po-no + '"','','Row').
        RUN cXMLOutput (clXMLOutput,'DocumentReference payloadID="' + cXMLPayloadID + '"','','Row').
        RUN cXMLOutput (clXMLOutput,'/DocumentReference','','Row').
        RUN cXMLOutput (clXMLOutput,'/OrderReference','','Row').
        RUN cXMLOutput (clXMLOutput,'/InvoiceDetailOrderInfo','','Row').
        XMLPage = NO.
        /* rstark 05291402 */

        {oe/rep/invport.i}  /* xprint form */

        v-subtot-lines = 0.
        
        ASSIGN 
            lFirstLine = TRUE
            /*            dTotalSalesTax = 0*/
            /*            dFrttaxRate    = 0*/
            /*            dFrtTaxAmt     = 0*/
            .
        /*        EMPTY TEMP-TABLE ttTaxDetail.         */
        /*        RUN Tax_CalculateForInvHeadWithDetail(*/
        /*            INPUT  ROWID(inv-head),           */
        /*            INPUT  locode,                    */
        /*            INPUT  "QUOTATION",               */
        /*            INPUT  NO,                        */
        /*            INPUT  "GetTaxAmount",            */
        /*            OUTPUT dTaxTotal,                 */
        /*            OUTPUT dInvoiceTotal,             */
        /*            OUTPUT dInvoiceSubTotal,          */
        /*            OUTPUT TABLE ttTaxDetail,         */
        /*            OUTPUT lSuccess,                  */
        /*            OUTPUT cMessage                   */
        /*            ).                                */
        FOR EACH inv-line NO-LOCK WHERE inv-line.r-no = inv-head.r-no:

            IF NOT s-print-zero-qty AND
                inv-line.ship-qty EQ 0 AND inv-line.inv-qty EQ 0 THEN
                NEXT.

            ASSIGN 
                v-case-line = ""
                v-part-line = ""
                v-case-cnt  = "".

            v-pc = "P". /* partial*/ 
            FOR EACH oe-boll NO-LOCK WHERE oe-boll.company = inv-line.company
                AND oe-boll.bol-no = inv-head.bol-no
                /*and oe-boll.b-no = inv-line.b-no*/
                AND oe-boll.i-no = inv-line.i-no USE-INDEX bol-no:

                /** Build Case Count Display Lines **/
                IF oe-boll.cases NE 0 AND oe-boll.qty-case NE 0 THEN
                    ASSIGN v-case-line = STRING(oe-boll.cases) + " @ " +
                                     string(oe-boll.qty-case).
                ELSE ASSIGN v-case-line = "".
                IF oe-boll.partial NE 0 THEN
                    ASSIGN v-part-line = "1" + " @ " + string(oe-boll.partial).
                ELSE ASSIGN v-part-line = "".

                IF oe-boll.p-c THEN v-pc = "C". /*complete*/

                DO i = 1 TO 5:
                    IF (80 - length(v-case-cnt[i])) > length(v-case-line) AND
                        v-case-line NE "" THEN
                        ASSIGN v-case-cnt[i] = v-case-cnt[i] + v-case-line + "  "
                            v-case-line   = "".
                    IF (80 - length(v-case-cnt[i])) > length(v-part-line) AND
                        v-part-line NE "" THEN
                        ASSIGN v-case-cnt[i] = v-case-cnt[i] + v-part-line + "  "
                            v-part-line   = "".
                END. /* 1 to 5 */

            
            END. /* each oe-boll */

            IF v-printline > 60 THEN 
            DO:           
                PAGE.
                {oe/rep/invport.i}
                v-printline = 39.
            END.

            ASSIGN 
                v-line = v-line + 1
                /* v-printline = v-printline + 2 */.  
            FIND FIRST oe-ordl WHERE oe-ordl.company = cocode AND
                oe-ordl.ord-no = inv-line.ord-no AND
                oe-ordl.i-no = inv-line.i-no
                NO-LOCK NO-ERROR.
            ASSIGN 
                dOrigQty = 0 
                cOrigUom = ""
                .                                     
            IF AVAILABLE oe-ordl THEN 
            DO:
              
                ASSIGN 
                    v-bo-qty = IF (inv-line.qty - inv-line.ship-qty -
                                    oe-ordl.t-ship-qty) < 0 then 0 else
                                   (inv-line.qty - inv-line.ship-qty -
                                    oe-ordl.t-ship-qty).
                IF oe-ordl.spare-char-2 NE "" THEN 
                DO:
                    ASSIGN 
                        dOrigQty = oe-ordl.spare-dec-1
                        cOrigUom = oe-ordl.spare-char-2
                        .
                    IF (cOrigUom EQ 'CS' OR LOOKUP(cOrigUom, cCaseUOMList) GT 0)
                        AND dOrigQty NE inv-line.qty
                        AND oe-ordl.cas-cnt NE 0 THEN 
                        dOrigQty = inv-line.inv-qty / oe-ordl.cas-cnt.
                    ELSE
                        dOrigQty = inv-line.inv-qty.
                           
                END.
            END.
            ELSE
                ASSIGN v-bo-qty = IF ( inv-line.qty - inv-line.ship-qty ) < 0
                                  then 0 else inv-line.qty - inv-line.ship-qty.
            ASSIGN 
                v-inv-qty      = inv-line.qty
                v-ship-qty     = inv-line.ship-qty
                v-i-no         = inv-line.i-no
                v-i-dscr       = inv-line.i-name
                v-price        = inv-line.price * (1 - (inv-line.disc / 100))
                v-t-price      = inv-line.t-price
                v-subtot-lines = v-subtot-lines + inv-line.t-price.
            /* PremierS switch*/
            IF ip-print-s THEN v-inv-qtys = inv-line.inv-qty / inv-line.cas-cnt.
            
            ASSIGN 
                v-po-no      = inv-line.po-no
                v-ord-no     = inv-line.ord-no
                v-price-head = inv-line.pr-uom.

            PUT SPACE(1)
                v-po-no 
                inv-line.part-no  SPACE(1)
                v-i-dscr FORM "x(30)". 
            /* PremierS switch*/
            IF ip-print-s THEN
                PUT v-inv-qtys  FORMAT "->>9.99" SPACE(2).
            ELSE PUT v-ship-qty  FORMAT "->>>>>9" SPACE(2).
            PUT v-price  FORMAT ">>>,>>9.999999"                
                inv-line.t-price  FORMAT "->>>,>>9.99"                
                SKIP
                SPACE(1)
                TRIM(STRING(v-ord-no,">>>>>>>9")) SPACE(7)
                inv-line.i-no SPACE(1)
                inv-line.part-dscr1  SPACE(11)
                v-pc  FORM "x" SPACE(7)
                v-price-head SKIP
                inv-line.part-dscr2 AT 33 SKIP.

            v-printline = v-printline + 3.
             
            /* rstark 05181205 */

            XMLLineNumber = XMLLineNumber + 1.
            RUN XMLOutput (lXMLOutput,'InvoiceLine_' + STRING(XMLLineNumber),'','Row').
            RUN XMLOutput (lXMLOutput,'Column_1',v-po-no,'Col').
            RUN XMLOutput (lXMLOutput,'Column_2',inv-line.part-no,'Col').
            RUN XMLOutput (lXMLOutput,'Column_3',v-i-dscr,'Col').
            /* PremierS switch*/
            IF ip-print-s THEN RUN XMLOutput (lXMLOutput,'Column_4',v-inv-qtys,'Col').
            ELSE RUN XMLOutput (lXMLOutput,'Column_4',v-ship-qty,'Col').
            RUN XMLOutput (lXMLOutput,'Column_5','','Col').
            RUN XMLOutput (lXMLOutput,'Column_6',v-price,'Col').
            RUN XMLOutput (lXMLOutput,'Column_7',inv-line.t-price,'Col').
            RUN XMLOutput (lXMLOutput,'/InvoiceLine_' + STRING(XMLLineNumber),'','Row').
             
            XMLLineNumber = XMLLineNumber + 1.
            RUN XMLOutput (lXMLOutput,'InvoiceLine_' + STRING(XMLLineNumber),'','Row').
            RUN XMLOutput (lXMLOutput,'Column_1',v-ord-no,'Col').
            RUN XMLOutput (lXMLOutput,'Column_2',inv-line.i-no,'Col').
            RUN XMLOutput (lXMLOutput,'Column_3',inv-line.part-dscr1,'Col').
            RUN XMLOutput (lXMLOutput,'Column_4','','Col').
            RUN XMLOutput (lXMLOutput,'Column_5',v-pc,'Col').
            RUN XMLOutput (lXMLOutput,'Column_6',v-price-head,'Col').
            RUN XMLOutput (lXMLOutput,'Column_7','','Col').
            RUN XMLOutput (lXMLOutput,'/InvoiceLine_' + STRING(XMLLineNumber),'','Row').

            XMLLineNumber = XMLLineNumber + 1.
            RUN XMLOutput (lXMLOutput,'InvoiceLine_' + STRING(XMLLineNumber),'','Row').
            RUN XMLOutput (lXMLOutput,'Column_1','','Col').
            RUN XMLOutput (lXMLOutput,'Column_2','','Col').
            RUN XMLOutput (lXMLOutput,'Column_3',inv-line.part-dscr2,'Col').
            RUN XMLOutput (lXMLOutput,'Column_4','','Col').
            RUN XMLOutput (lXMLOutput,'Column_5','','Col').
            RUN XMLOutput (lXMLOutput,'Column_6','','Col').
            RUN XMLOutput (lXMLOutput,'Column_7','','Col').
            RUN XMLOutput (lXMLOutput,'/InvoiceLine_' + STRING(XMLLineNumber),'','Row').

            XMLLineNumber = XMLLineNumber + 1.
            /* rstark 05181205 */
             
            /* rstark 05291402 */

            RUN cXMLOutput (clXMLOutput,'InvoiceDetailItem invoiceLineNumber="' + STRING(inv-line.LINE) 
                + '" quantity="' + STRING(IF dOrigQty NE 0 THEN dOrigQty ELSE inv-line.inv-qty) + '"','','Row').
            RUN cXMLOutput (clXMLOutput,'UnitOfMeasure',(IF cOrigUom EQ "" THEN inv-line.pr-uom ELSE cOrigUOM),'Col').
            RUN cXMLOutput (clXMLOutput,'UnitPrice','','Row'). 
            RUN cXMLOutput (clXMLOutput,'Money currency="USD"','','Row').
            RUN cXMLOutput (clXMLOutput,'',STRING(v-price),'Col').
            RUN cXMLOutput (clXMLOutput,'/Money','','Row').
            RUN cXMLOutput (clXMLOutput,'/UnitPrice','','Row'). 
            RUN cXMLOutput (clXMLOutput,'InvoiceDetailItemReference lineNumber="' + STRING(inv-line.LINE) + '"','','Row').
            RUN cXMLOutput (clXMLOutput,'ItemID','','Row'). 
            RUN cXMLOutput (clXMLOutput,'SupplierPartID',inv-line.part-no,'Col').
            RUN cXMLOutput (clXMLOutput,'/ItemID','','Row'). 
            RUN cXMLOutput (clXMLOutput,'Description xml:lang="en-US"','','Row').
            RUN cXMLOutput (clXMLOutput,'',inv-line.i-name,'Col').
            RUN cXMLOutput (clXMLOutput,'/Description','','Row').
            RUN cXMLOutput (clXMLOutput,'/InvoiceDetailItemReference','','Row').
            RUN cXMLOutput (clXMLOutput,'SubtotalAmount','','Row'). 
            RUN cXMLOutput (clXMLOutput,'Money currency="USD"','','Row').
            RUN cXMLOutput (clXMLOutput,'',STRING(inv-line.t-price),'Col').
            RUN cXMLOutput (clXMLOutput,'/Money','','Row').
            RUN cXMLOutput (clXMLOutput,'/SubtotalAmount','','Row'). 
            /*             IF AVAIL stax AND inv-line.tax THEN                                      */
            /*             DO:                                                                      */
            /*                FOR EACH ttTaxDetail                                                  */
            /*                    WHERE ttTaxDetail.invoiceLineType  EQ "INVLINE"                   */
            /*                      AND ttTaxDetail.invoiceLineRecKey EQ inv-line.rec_key           */
            /*                      AND ttTaxDetail.isFreight         EQ NO:                        */
            /*                        dTotalSalesTax = dTotalSalesTax + ttTaxDetail.taxCodeTaxAmount*/
            /*                        .                                                             */
            /*                END.                                                                  */
            /*                IF lFirstLine THEN DO:                                                */
            /*                    FOR EACH ttTaxDetail                                              */
            /*                        WHERE ttTaxDetail.invoiceLineType   EQ "INVHEAD"              */
            /*                          AND ttTaxDetail.invoiceLineRecKey EQ inv-head.rec_key       */
            /*                          AND ttTaxDetail.isFreight         EQ YES:                   */
            /*                        ASSIGN                                                        */
            /*                            dFrtTaxRate = dFrtTaxRate + ttTaxDetail.taxCodeRate       */
            /*                            dFrtTaxAmt  = dFrtTaxAmt  + ttTaxDetail.taxCodeTaxAmount  */
            /*                            .                                                         */
            /*                    END.                                                              */
            /*                END.                                                                  */
            /*                /* line level freight calculation not needed                          */
            /*                IF inv-head.f-bill THEN DO:                                           */
            /*                    EMPTY TEMP-TABLE ttTaxDetail.                                     */
            /*                    lIsFreightTaxable = YES.                                          */
            /*                    RUN Tax_CalculateWithDetail  (                                    */
            /*                        INPUT  inv-head.company,                                      */
            /*                        INPUT  inv-head.tax-gr,                                       */
            /*                        INPUT  TRUE,   /* Is this freight */                          */
            /*                        INPUT  inv-line.t-freight,                                    */
            /*                        OUTPUT dFrtTaxAmt,                                            */
            /*                        OUTPUT TABLE ttTaxDetail                                      */
            /*                        ).                                                            */
            /*                END.                                                                  */
            /*                ELSE                                                                  */
            /*                    ASSIGN                                                            */
            /*                        lIsfreightTaxable = NO                                        */
            /*                        dFrtTaxAmt        = 0                                         */
            /*                        dFrtTaxRate       = 0                                         */
            /*                        . */                                                          */
            /*             END.                                                                     */
                                 
            RUN cXMLOutput (clXMLOutput,'InvoiceDetailLineShipping','','Row').
            RUN cXMLOutput (clXMLOutput,'InvoiceDetailShipping','','Row').
            RUN cXMLOutput (clXMLOutput,'Contact addressID="' + cXMLShipTo + '" role="shipTo"','','Row').
            RUN cXMLOutput (clXMLOutput,'Name xml:lang="en-US"','','Row').
            RUN cXMLOutput (clXMLOutput,'',v-shipto-name,'Col').
            RUN cXMLOutput (clXMLOutput,'/Name','','Row').
            RUN cXMLOutput (clXMLOutput,'PostalAddress','','Row').
            RUN cXMLOutput (clXMLOutput,'Street',v-shipto-addr[1],'Col').
            IF v-shipto-addr[2] NE "" AND v-shipto-addr[2] NE '345 Court Street' THEN 
                RUN cXMLOutput (clXMLOutput,'Street',v-shipto-addr[2],'Col').
            RUN cXMLOutput (clXMLOutput,'City',v-shipto-city,'Col').
            RUN cXMLOutput (clXMLOutput,'State',v-shipto-state,'Col').
            RUN cXMLOutput (clXMLOutput,'PostalCode',v-shipto-zip,'Col').
            RUN cXMLOutput (clXMLOutput,'Country isoCountryCode="US"','','Row').
            RUN cXMLOutput (clXMLOutput,'','US','Col').
            RUN cXMLOutput (clXMLOutput,'/Country','','Row').
            RUN cXMLOutput (clXMLOutput,'/PostalAddress','','Row').
            RUN cXMLOutput (clXMLOutput,'/Contact','','Row').
            RUN cXMLOutput (clXMLOutput,'Contact role="shipFrom"','','Row').
            RUN cXMLOutput (clXMLOutput,'Name xml:lang="en-US"','','Row').
            RUN cXMLOutput (clXMLOutput,'','Premier Packaging','Col').
            RUN cXMLOutput (clXMLOutput,'/Name','','Row').
            RUN cXMLOutput (clXMLOutput,'PostalAddress','','Row').
            RUN cXMLOutput (clXMLOutput,'Street','3900 Produce Road','Col').
            RUN cXMLOutput (clXMLOutput,'City','Louisville','Col').
            RUN cXMLOutput (clXMLOutput,'State','KY','Col').
            RUN cXMLOutput (clXMLOutput,'PostalCode','40218','Col').
            RUN cXMLOutput (clXMLOutput,'Country isoCountryCode="US"','','Row').
            RUN cXMLOutput (clXMLOutput,'','US','Col').
            RUN cXMLOutput (clXMLOutput,'/Country','','Row').
            RUN cXMLOutput (clXMLOutput,'/PostalAddress','','Row').
            RUN cXMLOutput (clXMLOutput,'/Contact','','Row').
            RUN cXMLOutput (clXMLOutput,'/InvoiceDetailShipping','','Row').
            RUN cXMLOutput (clXMLOutput,'Money currency="USD"','','Row').
            /* Assign total freight to first detail line since line-level freight not supported */
            IF lFirstLine THEN 
            DO: 
                RUN cXMLOutput (clXMLOutput,'',IF inv-head.f-bill THEN STRING(inv-head.t-inv-freight) ELSE '0','Col').
                lFirstLine = NO.
            END.
            ELSE
                RUN cXMLOutput (clXMLOutput,'','0','Col').
            RUN cXMLOutput (clXMLOutput,'/Money','','Row').
            RUN cXMLOutput (clXMLOutput,'/InvoiceDetailLineShipping','','Row').
            RUN cXMLOutput (clXMLOutput,'/InvoiceDetailItem','','Row'). 
            
            /* rstark 05291402 */
            
            PUT SKIP(1).
            v-printline = v-printline + 1.
        END. /* each inv-line */
        
        FOR EACH inv-misc NO-LOCK WHERE inv-misc.company = inv-head.company AND
            inv-misc.r-no = inv-head.r-no AND
            inv-misc.bill = "Y" BREAK BY ord-no WITH FRAME detailm:
            
              
            IF v-printline > 60 THEN 
            DO:
              
                PAGE.                
                {oe/rep/invport.i}
                v-printline = 39.
            END.
            IF FIRST(inv-misc.ord-no) THEN
            DO:
                PUT "** Miscellaneous Items **" AT 23 SKIP(1).
                ASSIGN 
                    v-printline = v-printline + 2.

                /* rstark 05181205 */
                XMLLineNumber = XMLLineNumber + 1.
                RUN XMLOutput (lXMLOutput,'InvoiceLine_' + STRING(XMLLineNumber),'','Row').
                RUN XMLOutput (lXMLOutput,'Column_1','','Col').
                RUN XMLOutput (lXMLOutput,'Column_2','','Col').
                RUN XMLOutput (lXMLOutput,'Column_3','** Miscellaneous Items **','Col').
                RUN XMLOutput (lXMLOutput,'Column_4','','Col').
                RUN XMLOutput (lXMLOutput,'Column_5','','Col').
                RUN XMLOutput (lXMLOutput,'Column_6','','Col').
                RUN XMLOutput (lXMLOutput,'Column_7','','Col').
                RUN XMLOutput (lXMLOutput,'/InvoiceLine_' + STRING(XMLLineNumber),'','Row').
                XMLLineNumber = XMLLineNumber + 1.
            /* rstark 05181205 */

            END.
                 
            PUT inv-misc.po-no AT 2 inv-misc.charge AT 17 inv-misc.dscr inv-misc.amt AT 85 SKIP.

            /* rstark 05181205 */
            XMLLineNumber = XMLLineNumber + 1.
            RUN XMLOutput (lXMLOutput,'InvoiceLine_' + STRING(XMLLineNumber),'','Row').
            RUN XMLOutput (lXMLOutput,'Column_1','','Col').
            RUN XMLOutput (lXMLOutput,'Column_2',inv-misc.charge,'Col').
            RUN XMLOutput (lXMLOutput,'Column_3',inv-misc.dscr,'Col').
            RUN XMLOutput (lXMLOutput,'Column_4','','Col').
            RUN XMLOutput (lXMLOutput,'Column_5','','Col').
            RUN XMLOutput (lXMLOutput,'Column_6','','Col').
            RUN XMLOutput (lXMLOutput,'Column_7',inv-misc.amt,'Col').
            RUN XMLOutput (lXMLOutput,'/InvoiceLine_' + STRING(XMLLineNumber),'','Row').
            /* rstark 05181205 */

            ASSIGN
                v-subtot-lines = v-subtot-lines + inv-misc.amt
                v-printline    = v-printline + 1.

        END. /* each inv-misc */

        IF v-prntinst THEN 
        DO:
            DO i = 1 TO 4:
                IF inv-head.bill-i[i] NE "" THEN 
                DO:
                    PUT inv-head.bill-i[i] AT 10 SKIP.
                    ASSIGN 
                        v-printline = v-printline + 1.
             

                    /* rstark 05181205 */
                    XMLLineNumber = XMLLineNumber + 1.
                    RUN XMLOutput (lXMLOutput,'InvoiceLine_' + STRING(XMLLineNumber),'','Row').
                    RUN XMLOutput (lXMLOutput,'Column_1','','Col').
                    RUN XMLOutput (lXMLOutput,'Column_2',inv-head.bill-i[i],'Col').
                    RUN XMLOutput (lXMLOutput,'Column_3','','Col').
                    RUN XMLOutput (lXMLOutput,'Column_4','','Col').
                    RUN XMLOutput (lXMLOutput,'Column_5','','Col').
                    RUN XMLOutput (lXMLOutput,'Column_6','','Col').
                    RUN XMLOutput (lXMLOutput,'Column_7','','Col').
                    RUN XMLOutput (lXMLOutput,'/InvoiceLine_' + STRING(XMLLineNumber),'','Row').
                /* rstark 05181205 */

                END.
            END. /* 1 to 4 */
            IF v-printline > 61 THEN 
            DO:              
                PAGE.                
                {oe/rep/invport.i}
                v-printline = 39.
            END.
         
        END.
    END. /* DO TRANSACTION */

    v-inv-freight = 0.
    FOR EACH b-inv-head WHERE b-inv-head.company = inv-head.company
        AND b-inv-head.inv-no  = inv-head.inv-no
        AND b-inv-head.multi-invoice = NO
        AND b-inv-head.f-bill  = TRUE
        NO-LOCK.
        v-inv-freight = v-inv-freight + b-inv-head.t-inv-freight.
    END.
   
    PUT "<R58><C58.5><#8><FROM><R+5><C+23><RECT> " 
        "<=8> Subtotal do    :" v-subtot-lines FORM "->>,>>9.99"
        "<=8><R+1> imposto sobre  :" v-inv-freight
        "<=8><R+2> " cTaxCode FORMAT "x(15)" ":" inv-head.t-inv-tax FORM "->>,>>9.99"
        "<=8><R+3>" "" 
        "<=8><R+4> Fatura         :" inv-head.t-inv-rev FORM "->>,>>9.99" .

    PUT "<FArial><R58><C1><P12><B> OBRIGADO. </B> <P9> " SKIP.
    PUT "<FArial><R60><C1><P12> Todas as moedas exibidas em " cCurCode FORMAT "x(3)" ". <P9> " SKIP.
    v-printline = v-printline + 8.
    IF v-printline > 61 THEN 
    DO:              
        PAGE.                
               
        v-printline = 39.
    END.
    /* rstark 05181205 */
    RUN XMLOutput (lXMLOutput,'Last',STRING(PAGE-NUMBER),'Page').
    RUN XMLOutput (lXMLOutput,'InvoiceFooter','','Row').
    RUN XMLOutput (lXMLOutput,'SubTotal',v-subtot-lines,'Col').
    RUN XMLOutput (lXMLOutput,'Freight',v-inv-freight,'Col').
    RUN XMLOutput (lXMLOutput,'SalesTax',inv-head.t-inv-tax,'Col').
    RUN XMLOutput (lXMLOutput,'TotalInvoice',inv-head.t-inv-rev,'Col').
    RUN XMLOutput (lXMLOutput,'/InvoiceFooter','','Row').
    /* rstark 05181205 */
    /*                                                                           */
    /*    FOR EACH inv-misc NO-LOCK                                              */
    /*        WHERE inv-misc.company EQ inv-head.company                         */
    /*          AND inv-misc.r-no    EQ inv-head.r-no:                           */
    /*        FOR EACH ttTaxDetail                                               */
    /*            WHERE ttTaxDetail.invoiceLineType   EQ "INVMISC"               */
    /*              AND ttTaxDetail.invoiceLineRecKey EQ inv-misc.rec_key        */
    /*              AND ttTaxDetail.isFreight         EQ NO:                     */
    /*            dTotalSalesTax = dTotalSalesTax + ttTaxDetail.taxCodeTaxAmount.*/
    /*        END.                                                               */
    /*    END.                                                                   */
    /*                                                                           */
    /* rstark 05291402 */
    RUN cXMLOutput (clXMLOutput,'/InvoiceDetailOrder','','Row'). 
    RUN cXMLOutput (clXMLOutput,'InvoiceDetailSummary','','Row').
    RUN cXMLOutput (clXMLOutput,'SubtotalAmount','','Row').
    RUN cXMLOutput (clXMLOutput,'Money currency="USD"','','Row').
    RUN cXMLOutput (clXMLOutput,'',STRING(v-subtot-lines),'Col').
    RUN cXMLOutput (clXMLOutput,'/Money','','Row').
    RUN cXMLOutput (clXMLOutput,'/SubtotalAmount','','Row').
    RUN cXMLOutput (clXMLOutput,'Tax','','Row').
    RUN cXMLOutput (clXMLOutput,'Money currency="USD"','','Row').
    RUN cXMLOutput (clXMLOutput,'',STRING(inv-head.t-inv-tax),'Col').
    RUN cXMLOutput (clXMLOutput,'/Money','','Row').
    RUN cXMLOutput (clXMLOutput,'Description xml:lang="en-US"','','Row').
    RUN cXMLOutput (clXMLOutput,'','Sales Tax','Col').
    RUN cXMLOutput (clXMLOutput,'/Description','','Row').
    /*    RUN cXMLOutput (clXMLOutput,'TaxDetail category="SalesTax" '+                                                                                  */
    /*                                'percentageRate="0"','','Row').                                                                                    */
    /*    RUN cXMLOutput (clXMLOutput,'TaxableAmount','','Row').                                                                                         */
    /*    RUN cXMLOutput (clXMLOutput,'Money currency="USD"','','Row').                                                                                  */
    /*    RUN cXMLOutput (clXMLOutput,'',STRING(v-subtot-lines),'Col').                                                                                  */
    /*    RUN cXMLOutput (clXMLOutput,'/Money','','Row').                                                                                                */
    /*    RUN cXMLOutput (clXMLOutput,'/TaxableAmount','','Row').                                                                                        */
    /*    RUN cXMLOutput (clXMLOutput,'TaxAmount','','Row').                                                                                             */
    /*    RUN cXMLOutput (clXMLOutput,'Money currency="USD"','','Row').                                                                                  */
    /*    RUN cXMLOutput (clXMLOutput,'',STRING(dTotalSalesTax),'Col').                                                                                  */
    /*    RUN cXMLOutput (clXMLOutput,'/Money','','Row').                                                                                                */
    /*    RUN cXMLOutput (clXMLOutput,'/TaxAmount','','Row').                                                                                            */
    /*    RUN cXMLOutput (clXMLOutput,'/TaxDetail','','Row').                                                                                            */
    /*     /* Seperate section for handling shipping Tax */                                                                                              */
    /*    IF inv-head.f-bill AND inv-head.t-inv-freight NE 0                                                                                             */
    /*        AND dFrtTaxRate NE 0 THEN DO:                                                                                                              */
    /*        RUN cXMLOutput (clXMLOutput,'TaxDetail purpose="shippingTax" category="sales"' + ' percentageRate="' + STRING(dFrtTaxRate) + '"','','Row').*/
    /*        RUN cXMLOutput (clXMLOutput,'TaxableAmount','','Row').                                                                                     */
    /*        RUN cXMLOutput (clXMLOutput,'Money currency="USD"','','Row').                                                                              */
    /*        RUN cXMLOutput (clXMLOutput,'',STRING(inv-head.t-inv-freight),'Col').                                                                      */
    /*        RUN cXMLOutput (clXMLOutput,'/Money','','Row').                                                                                            */
    /*        RUN cXMLOutput (clXMLOutput,'/TaxableAmount','','Row').                                                                                    */
    /*        RUN cXMLOutput (clXMLOutput,'TaxAmount','','Row').                                                                                         */
    /*        RUN cXMLOutput (clXMLOutput,'Money currency="USD"','','Row').                                                                              */
    /*        RUN cXMLOutput (clXMLOutput,'',STRING(dFrtTaxAmt),'Col').                                                                                  */
    /*        RUN cXMLOutput (clXMLOutput,'/Money','','Row').                                                                                            */
    /*        RUN cXMLOutput (clXMLOutput,'/TaxAmount','','Row').                                                                                        */
    /*        RUN cXMLOutput (clXMLOutput,'Description xml:lang="en-US"','','Row').                                                                      */
    /*        RUN cXMLOutput (clXMLOutput,'/Description','','Row').                                                                                      */
    /*        RUN cXMLOutput (clXMLOutput,'/TaxDetail','','Row').                                                                                        */
    /*    END.                                                                                                                                           */
    RUN cXMLOutput (clXMLOutput,'/Tax','','Row').
    RUN cXMLOutput (clXMLOutput,'SpecialHandlingAmount','','Row').
    RUN cXMLOutput (clXMLOutput,'Money currency="USD"','','Row').
    RUN cXMLOutput (clXMLOutput,'','0','Col').
    RUN cXMLOutput (clXMLOutput,'/Money','','Row').
    RUN cXMLOutput (clXMLOutput,'/SpecialHandlingAmount','','Row').
    RUN cXMLOutput (clXMLOutput,'ShippingAmount','','Row').
    RUN cXMLOutput (clXMLOutput,'Money currency="USD"','','Row').
    RUN cXMLOutput (clXMLOutput,'',STRING(v-inv-freight),'Col').
    RUN cXMLOutput (clXMLOutput,'/Money','','Row').
    RUN cXMLOutput (clXMLOutput,'/ShippingAmount','','Row').
    RUN cXMLOutput (clXMLOutput,'GrossAmount','','Row').
    RUN cXMLOutput (clXMLOutput,'Money currency="USD"','','Row').
    RUN cXMLOutput (clXMLOutput,'',STRING(inv-head.t-inv-rev),'Col').
    RUN cXMLOutput (clXMLOutput,'/Money','','Row').
    RUN cXMLOutput (clXMLOutput,'/GrossAmount','','Row').
    RUN cXMLOutput (clXMLOutput,'NetAmount','','Row').
    RUN cXMLOutput (clXMLOutput,'Money currency="USD"','','Row').
    RUN cXMLOutput (clXMLOutput,'',STRING(inv-head.t-inv-rev),'Col').
    RUN cXMLOutput (clXMLOutput,'/Money','','Row').
    RUN cXMLOutput (clXMLOutput,'/NetAmount','','Row').
    RUN cXMLOutput (clXMLOutput,'/InvoiceDetailSummary','','Row').
    RUN cXMLOutput (clXMLOutput,'/InvoiceDetailRequest','','Row').
    RUN cXMLOutput (clXMLOutput,'/Request','','Row').
    /* rstark 05291402 */

    IF v-printline <= 66 THEN PAGE.
    
    {XMLOutput/XMLOutput.i &c=c &XMLClose} /* rstark 05291402 */

END. /* each xinv-head */

{XMLOutput/XMLOutput.i &XMLClose} /* rstark 05181205 */

/* END ---------------------------------- copr. 1996 Advanced Software, Inc. */
