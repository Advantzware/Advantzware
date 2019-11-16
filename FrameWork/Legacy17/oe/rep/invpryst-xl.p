/* ---------------------------------------------- oe/rep/invsthpk.p */
/* PRINT INVOICE   Xprint form for South Pak             */
/* -------------------------------------------------------------------------- */
DEFINE INPUT PARAMETER ip-multi-faxout AS LOG NO-UNDO. /* fax multiple recipents or single */
DEFINE INPUT PARAMETER  ip-lines-per-page AS INTEGER NO-UNDO.

DEFINE STREAM st-fax.

{sys/inc/var.i shared}
{custom/notesdef.i}

DEFINE VARIABLE v-inst AS cha FORM "x(60)" EXTENT 4 NO-UNDO.
{oe/rep/invoice.i}

DEFINE        VARIABLE v-salesman     AS CHARACTER FORMAT "x(14)" NO-UNDO.
DEFINE        VARIABLE v-fob          AS CHARACTER FORMAT "x(27)" NO-UNDO.
DEFINE        VARIABLE v-shipvia      LIKE carrier.dscr NO-UNDO.
DEFINE        VARIABLE v-addr3        AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE        VARIABLE v-sold-addr3   AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE        VARIABLE v-shipto-name  AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE        VARIABLE v-shipto-id    AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE        VARIABLE v-shipto-phone AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE        VARIABLE v-shipto-addr  AS CHARACTER FORMAT "x(30)" EXTENT 2 NO-UNDO.
DEFINE        VARIABLE v-shipto-city  AS CHARACTER FORMAT "x(15)" NO-UNDO.
DEFINE        VARIABLE v-shipto-state AS CHARACTER FORMAT "x(2)" NO-UNDO.
DEFINE        VARIABLE v-shipto-zip   AS CHARACTER FORMAT "x(10)" NO-UNDO.
DEFINE        VARIABLE v-soldto-city  AS CHARACTER FORMAT "x(15)" NO-UNDO.
DEFINE        VARIABLE v-soldto-state AS CHARACTER FORMAT "x(2)" NO-UNDO.
DEFINE        VARIABLE v-soldto-zip   AS CHARACTER FORMAT "x(10)" NO-UNDO.

DEFINE        VARIABLE v-line         AS INTEGER   NO-UNDO.
DEFINE        VARIABLE v-printline    AS INTEGER   NO-UNDO.
DEFINE        VARIABLE v-t-weight     LIKE inv-line.t-weight NO-UNDO.
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
DEFINE        VARIABLE v-set-qty      AS INTEGER   NO-UNDO.
DEFINE        VARIABLE v-part-qty     AS DECIMAL   FORMAT "999.9999" NO-UNDO.
DEFINE        VARIABLE v-net          LIKE inv-head.t-inv-rev NO-UNDO.
DEFINE        VARIABLE v-case-cnt     AS CHARACTER FORMAT "x(80)" EXTENT 5 NO-UNDO.
DEFINE        VARIABLE v-case-line    AS CHARACTER NO-UNDO.
DEFINE        VARIABLE v-part-line    AS CHARACTER NO-UNDO.

DEFINE BUFFER xinv-head FOR inv-head .
DEFINE BUFFER xinv-line FOR inv-line .

DEFINE WORKFILE w-sman
    FIELD sman AS CHARACTER FORMAT "x(4)".

DEFINE VARIABLE v-ord-del-hdr  AS CHARACTER FORMAT "x(3)" INIT "Del".
DEFINE VARIABLE v-beeler-lines AS INTEGER.
DEFINE VARIABLE v-part-info    AS CHARACTER FORMAT "x(30)".
DEFINE VARIABLE v              AS INTEGER.
DEFINE VARIABLE v-bo-qty       AS INTEGER   FORMAT "99999" NO-UNDO.
DEFINE VARIABLE v-inv-qty      AS INTEGER   FORMAT "99999" NO-UNDO.
DEFINE VARIABLE v-ship-qty     AS INTEGER   FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE v-i-no         AS CHARACTER FORMAT "x(15)" NO-UNDO.
DEFINE VARIABLE v-i-dscr       AS CHARACTER FORMAT "x(18)" NO-UNDO.
DEFINE VARIABLE v-price        AS DECIMAL   FORMAT ">>>>9.9999" NO-UNDO.
DEFINE VARIABLE v-t-price      AS DECIMAL   FORMAT ">>>>>>9.99" NO-UNDO.
DEFINE VARIABLE v-po-no        LIKE inv-line.po-no NO-UNDO.
DEFINE VARIABLE v-bill-i       AS CHARACTER FORMAT "x(25)" NO-UNDO.
DEFINE VARIABLE v-ord-no       LIKE oe-ord.ord-no NO-UNDO.
DEFINE VARIABLE v-ord-date     LIKE oe-ord.ord-date NO-UNDO.
DEFINE VARIABLE v-due-date     LIKE oe-ord.due-date NO-UNDO.
DEFINE VARIABLE v-ship-i       AS CHARACTER FORMAT "x(25)" NO-UNDO.
DEFINE VARIABLE v-rel-po-no    LIKE oe-rel.po-no NO-UNDO.
DEFINE VARIABLE v-price-head   AS CHARACTER FORMAT "x(5)" NO-UNDO.
DEFINE VARIABLE v-subtot-lines AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-cartot-lines AS DECIMAL   NO-UNDO.
DEFINE WORKFILE w-tax
    FIELD w-dsc AS   CHARACTER
    FIELD w-tax AS   DECIMAL.
DEFINE VARIABLE v-t-tax       AS DECIMAL   EXTENT 3 NO-UNDO.
DEFINE VARIABLE v-bot-lab     AS CHARACTER FORMAT "x(63)" EXTENT 3 NO-UNDO.
DEFINE VARIABLE v-lines       AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-inv-freight LIKE inv-head.t-inv-freight NO-UNDO.
DEFINE VARIABLE v-frt-tax     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE tax-dscr-1    AS CHARACTER NO-UNDO.
DEFINE VARIABLE tax-dscr-2    AS CHARACTER NO-UNDO.
DEFINE VARIABLE tax-dscr-3    AS CHARACTER NO-UNDO.

FIND FIRST inv-head NO-LOCK NO-ERROR.
/* === with xprint ====*/
DEFINE VARIABLE ls-image1    AS cha NO-UNDO.
DEFINE VARIABLE ls-full-img1 AS cha FORM "x(50)" NO-UNDO.

ASSIGN 
    ls-image1           = "images\prystup.jpg"
    FILE-INFO:FILE-NAME = ls-image1
    ls-full-img1        = FILE-INFO:FULL-PATHNAME + ">".

DEFINE            VARIABLE v-tel              AS cha              FORM "x(30)" NO-UNDO.
DEFINE            VARIABLE v-fax              AS cha              FORM "x(30)" NO-UNDO.
DEFINE            VARIABLE v-contact          AS cha              FORM "x(20)" NO-UNDO .

DEFINE            VARIABLE v-comp-add1        AS cha              FORM "x(30)" NO-UNDO.
DEFINE            VARIABLE v-comp-add2        AS cha              FORM "x(30)" NO-UNDO.
DEFINE            VARIABLE v-comp-add3        AS cha              FORM "x(30)" NO-UNDO.
DEFINE            VARIABLE v-comp-add4        AS cha              FORM "x(30)" NO-UNDO.

/* vARIABLE FOR EXCEL OUTPUT */
DEFINE SHARED     VARIABLE LvOutputSelection  AS CHARACTER        NO-UNDO.
DEFINE SHARED     VARIABLE CallingParameter   AS CHARACTER        NO-UNDO.
/* skb 1/24/07 - Variables for excel Automation  */
DEFINE NEW SHARED VARIABLE chExcelApplication AS COMPONENT-HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE chWorkBook         AS COMPONENT-HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE chWorksheet        AS COMPONENT-HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE chHyper            AS COMPONENT-HANDLE NO-UNDO. 
DEFINE            VARIABLE v-cell             AS CHARACTER        NO-UNDO.
DEFINE            VARIABLE t-dwg              AS CHARACTER        NO-UNDO.
DEFINE            VARIABLE t-name             AS CHARACTER        FORMAT "x(40)" NO-UNDO.
DEFINE            VARIABLE t-fnd              AS LOGICAL          INIT "False" NO-UNDO.
DEFINE            VARIABLE t-seq              AS INTEGER  								NO-UNDO.
DEFINE            VARIABLE inRowCount         AS INTEGER          NO-UNDO INITIAL 1.
DEFINE            VARIABLE chFile             AS CHARACTER        NO-UNDO.
DEFINE            VARIABLE LvLineCnt          AS INTEGER          NO-UNDO.
DEFINE            VARIABLE CurrDir            AS CHARACTER        NO-UNDO.
DEFINE            VARIABLE LvCtr              AS INTEGER          NO-UNDO.
DEFINE            VARIABLE vcTemplateFile     AS CHARACTER        NO-UNDO.

/* Build a Table to keep sequence of pdf files */
DEFINE SHARED TEMP-TABLE tt-filelist
    FIELD tt-FileCtr  AS INTEGER
    FIELD tt-FileName AS CHARACTER
    INDEX filelist IS PRIMARY TT-FILECTR.

DEFINE VARIABLE CurActivePrinter AS CHARACTER        NO-UNDO.
DEFINE VARIABLE AdobePrinter     AS CHARACTER        NO-UNDO.
DEFINE VARIABLE CommandString    AS CHARACTER        NO-UNDO.
DEFINE VARIABLE WshNetwork       AS COMPONENT-HANDLE.
DEFINE VARIABLE LvFirstTimePrint AS LOGICAL          INIT NO NO-UNDO.
DEFINE VARIABLE v-lot-no         AS CHARACTER        NO-UNDO.
/**************************** Excel Initilization Starts *********************************/

DEFINE VARIABLE v-dir            AS CHARACTER        FORMAT "X(80)" NO-UNDO.

FIND FIRST users WHERE
    users.user_id EQ USERID("NOSWEAT")
    NO-LOCK NO-ERROR.

IF AVAILABLE users AND users.user_program[2] NE "" THEN
    v-dir = users.user_program[2] + "\".
ELSE
    v-dir = "c:\tmp\".

ASSIGN 
    CurActivePrinter = SESSION:PRINTER-NAME
    AdobePrinter     = "PDFcamp Printer".

/* Capture the current active printer */
CREATE "WScript.Network" WshNetwork NO-ERROR.
IF NOT(VALID-HANDLE(WshNetwork)) THEN
DO :
    MESSAGE "Unable to Create Wscript.Network" VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
END.

IF LvOutputSelection = "Email" THEN
DO:
    WshNetwork:SetDefaultPrinter(AdobePrinter). 
END.
CREATE "Excel.Application" chExcelApplication NO-ERROR.
chExcelApplication:VISIBLE = TRUE.
IF LvOutputSelection = "Email" OR LvOutputSelection = "Printer" THEN
    chExcelApplication:VISIBLE = FALSE.

IF NOT(VALID-HANDLE(chExcelApplication)) THEN
DO :
    MESSAGE "Unable to Start Excel" VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
END.

FILE-INFO:FILE-NAME = "Template\invoice-py.xlt".

/* Set the Excel Template to be used. */
ASSIGN 
    chFile = SEARCH (FILE-INFO:FULL-PATHNAME) no-error.
  
IF SEARCH (chFile) = ? THEN 
DO:
    MESSAGE 'Template File: ' FILE-INFO:FULL-PATHNAME
        'cannot be found. Please verify that the file exists.'
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    APPLY 'CLOSE':U TO THIS-PROCEDURE.
END.

CurrDir = SUBSTRING (chFile, 1, INDEX (chFile, "Template\invoice-py.xlt") - 2)
    NO-ERROR.

FOR EACH tt-filelist:
    DELETE tt-filelist.
END.
/**************************** Excel Initilization End *********************************/
    
FIND FIRST company WHERE company.company = cocode NO-LOCK NO-ERROR.

FIND FIRST oe-ctrl WHERE oe-ctrl.company = cocode NO-LOCK NO-ERROR.
FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,
    FIRST xinv-head WHERE RECID(xinv-head) EQ report.rec-id NO-LOCK
    BREAK BY report.key-01
    BY report.key-02:

      
            
    FIND FIRST cust WHERE cust.company = xinv-head.company
        AND cust.cust-no = xinv-head.cust-no NO-LOCK NO-ERROR.

    ASSIGN  
        v-shipto-name    = xinv-head.sold-name
        v-shipto-id      = xinv-head.sold-no
        v-shipto-phone   = ""
        v-shipto-addr[1] = xinv-head.sold-addr[1]
        v-shipto-addr[2] = xinv-head.sold-addr[2]
        v-shipto-city    = xinv-head.sold-city
        v-shipto-state   = xinv-head.sold-state
        v-shipto-zip     = xinv-head.sold-zip.

    v-del-no = 0.
      
    FIND FIRST oe-bolh WHERE oe-bolh.company = xinv-head.company AND
        oe-bolh.bol-no = xinv-head.bol-no USE-INDEX bol-no NO-LOCK NO-ERROR.
    IF AVAILABLE oe-bolh THEN 
    DO:
        FIND FIRST shipto WHERE shipto.company  = oe-bolh.company AND
            shipto.cust-no = oe-bolh.cust-no AND
            shipto.ship-id = oe-bolh.ship-id NO-LOCK NO-ERROR.
        IF AVAILABLE shipto THEN
            ASSIGN  v-shipto-name    = shipto.ship-name
                v-shipto-id      = shipto.ship-id
                v-shipto-phone   = shipto.area-code + shipto.phone
                v-shipto-addr[1] = shipto.ship-addr[1]
                v-shipto-addr[2] = shipto.ship-addr[2]
                v-shipto-city    = shipto.ship-city
                v-shipto-state   = shipto.ship-state
                v-shipto-zip     = shipto.ship-zip.

    END. /* avail oe-bolh */
    IF NOT v-reprint OR xinv-head.inv-no EQ 0 THEN
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
            v-soldto-city  = inv-head.city
            v-soldto-state = inv-head.state
            v-soldto-zip   = inv-head.zip
            v-sold-addr3   = v-shipto-city + ", " + v-shipto-state +
              "  " + v-shipto-zip
            v-line         = 1
            v-printline    = 0.
      
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
        FOR EACH xinv-line NO-LOCK WHERE xinv-line.r-no = inv-head.r-no
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
                v-tot-qty  = v-tot-qty + xinv-line.ship-qty
                v-t-weight = v-t-weight + (ROUND(xinv-line.t-weight /
                            xinv-line.qty, 2) * xinv-line.inv-qty).
            v-tot-pallets = 0.
            FOR EACH oe-bolh NO-LOCK WHERE oe-bolh.b-no = xinv-line.b-no:
                FOR EACH oe-boll NO-LOCK WHERE oe-boll.company = oe-bolh.company AND
                    oe-boll.b-no = oe-bolh.b-no AND
                    oe-boll.i-no = xinv-line.i-no AND
                    oe-boll.ord-no = xinv-line.ord-no :

                    /** Bill Of Lading TOTAL CASES **/
                    ASSIGN 
                        v-bol-cases   = v-bol-cases + oe-boll.cases
                        v-tot-pallets = v-tot-pallets + oe-boll.cases +
                                     (IF oe-boll.partial GT 0 THEN 1 ELSE 0).
                END. /* each oe-boll */
                ASSIGN 
                    v-date-ship = oe-bolh.bol-date.
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
                                v-set-qty = v-set-qty + fg-set.part-qty.
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

                            IF AVAILABLE fg-set AND fg-set.part-qty NE 0 THEN
                                ASSIGN v-part-qty = fg-set.part-qty / v-set-qty.
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
            ASSIGN v-rel-po-no = oe-bolh.po-no
                v-due-date  = oe-bolh.bol-date.

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
                    v-po-no    = oe-ord.po-no
                    v-bill-i   = oe-ord.bill-i[1]
                    v-ord-no   = oe-ord.ord-no
                    v-ord-date = oe-ord.ord-date
                    /* v-due-date = oe-ord.due-date*/ .
            END.
            ELSE
                ASSIGN v-price-head = inv-line.pr-uom.
        END.
        
        /* Print Header Information */
        
        ASSIGN 
            chWorkbook                        = chExcelApplication:Workbooks:Open(chfile)
            chExcelApplication:ScreenUpdating = FALSE.
        
        /************ Sold To *********************/
        /* Sold To Customer  */
        /*    chExcelApplication:Goto("R7C1") NO-ERROR.
            ASSIGN chExcelApplication:ActiveCell:Value = inv-head.cust-no. */
        
        /* Sold To Customer Name */
        chExcelApplication:Goto("R8C1") NO-ERROR.
        ASSIGN 
            chExcelApplication:ActiveCell:Value = inv-head.cust-name.
        /* Sold To Add1 */
        chExcelApplication:Goto("R9C1") NO-ERROR.
        ASSIGN 
            chExcelApplication:ActiveCell:Value = inv-head.addr[1].
        /* Sold To Add2 */
        chExcelApplication:Goto("R10C1") NO-ERROR.
        ASSIGN 
            chExcelApplication:ActiveCell:Value = inv-head.addr[2].
        /* City */
        chExcelApplication:Goto("R11C1") NO-ERROR.
        ASSIGN 
            chExcelApplication:ActiveCell:Value = v-soldto-city.
        
        /* State */
        chExcelApplication:Goto("R11C11") NO-ERROR.
        ASSIGN 
            chExcelApplication:ActiveCell:Value = v-soldto-State.

        /* Zip */
        chExcelApplication:Goto("R11C13") NO-ERROR.
        ASSIGN 
            chExcelApplication:ActiveCell:Value = v-soldto-zip.

        
        /************ Ship To *********************/
        /*     chExcelApplication:Goto("R7C1") NO-ERROR.
             ASSIGN chExcelApplication:ActiveCell:Value = v-shipto-id. */
        
        chExcelApplication:Goto("R7C28") NO-ERROR.
        ASSIGN 
            chExcelApplication:ActiveCell:Value = v-shipto-phone.
        
        /* Sold To Customer Name */
        chExcelApplication:Goto("R8C18") NO-ERROR.
        ASSIGN 
            chExcelApplication:ActiveCell:Value = v-shipto-name.
        /* Sold To Add1 */
        chExcelApplication:Goto("R9C18") NO-ERROR.
        ASSIGN 
            chExcelApplication:ActiveCell:Value = v-shipto-addr[1].
        /* Sold To Add2 */
        chExcelApplication:Goto("R10C18") NO-ERROR.
        ASSIGN 
            chExcelApplication:ActiveCell:Value = v-shipto-addr[2].

        /* City */
        chExcelApplication:Goto("R11C18") NO-ERROR.
        ASSIGN 
            chExcelApplication:ActiveCell:Value = v-shipto-city.
        /*State */        
        chExcelApplication:Goto("R11C28") NO-ERROR.
        ASSIGN 
            chExcelApplication:ActiveCell:Value = v-shipto-state.

        chExcelApplication:Goto("R11C30") NO-ERROR.
        ASSIGN 
            chExcelApplication:ActiveCell:Value = v-shipto-zip.
        
        /* Invoice# */
        chExcelApplication:Goto("R5C34") NO-ERROR.
        ASSIGN 
            chExcelApplication:ActiveCell:Value = inv-head.inv-no.
        
        /* Date */
        chExcelApplication:Goto("R5C40") NO-ERROR.
        ASSIGN 
            chExcelApplication:ActiveCell:Value = v-inv-date.

        /* Due Date */
        chExcelApplication:Goto("R5C46") NO-ERROR.
        ASSIGN 
            chExcelApplication:ActiveCell:Value = v-due-date.

        /* Cust PO */
        chExcelApplication:Goto("R8C34") NO-ERROR.
        ASSIGN 
            chExcelApplication:ActiveCell:Value = xinv-head.bol-no /*v-po-no*/ .
        
        /* Terms */
        chExcelApplication:Goto("R8C40") NO-ERROR.
        ASSIGN 
            chExcelApplication:ActiveCell:Value = xinv-head.terms-d.

        /* Our Order# */
        chExcelApplication:Goto("R8C46") NO-ERROR.
        ASSIGN 
            chExcelApplication:ActiveCell:Value = v-salesman.
 
        /* Ship Via */
        chExcelApplication:Goto("R11C34") NO-ERROR.
        ASSIGN 
            chExcelApplication:ActiveCell:Value = v-shipvia.
        
        /* Freight Terms */
        chExcelApplication:Goto("R11C40") NO-ERROR.
        ASSIGN 
            chExcelApplication:ActiveCell:Value = v-fob.
        
        /*   /* SalesPerson */
           chExcelApplication:Goto("R11C46") NO-ERROR.
           ASSIGN chExcelApplication:ActiveCell:Value = v-salesman */
        ASSIGN 
            v-subtot-lines = 0
            v-cartot-lines = 0
            v-t-tax        = 0
            inrowcount     = 14
            v-lot-no       = "" 
            v-ship-qty     = 0 .

        FOR EACH inv-line NO-LOCK WHERE inv-line.r-no = inv-head.r-no
            BY inv-line.i-no:
            ASSIGN 
                v-case-line = ""
                v-part-line = ""
                v-case-cnt  = ""
                v-line      = v-line + 1.

            FIND FIRST oe-ordl WHERE oe-ordl.company = cocode AND
                oe-ordl.ord-no = inv-line.ord-no AND
                oe-ordl.i-no = inv-line.i-no
                NO-LOCK NO-ERROR.

            IF AVAILABLE oe-ordl THEN
                ASSIGN v-i-dscr = oe-ordl.i-name  .
            ELSE v-i-dscr = inv-line.part-dscr1 .


            ASSIGN 
                v-inv-qty      = inv-line.qty
                v-ship-qty     = inv-line.ship-qty
                v-i-no         = inv-line.i-no
                v-price        = inv-line.price * (1 - (inv-line.disc / 100))
                v-t-price      = inv-line.t-price
                v-subtot-lines = v-subtot-lines + inv-line.t-price
                v-cartot-lines = v-cartot-lines + inv-line.ship-qty .

            IF AVAILABLE oe-ordl THEN 
            DO:
                v-bo-qty = inv-line.ship-qty + oe-ordl.t-ship-qty.

                FIND FIRST oe-ord
                    WHERE oe-ord.company EQ oe-ordl.company
                    AND oe-ord.ord-no  EQ oe-ordl.ord-no
                    NO-LOCK NO-ERROR.

                v-bo-qty = IF v-bo-qty GE oe-ordl.qty * (1 - (oe-ordl.under-pct / 100))
                    THEN 0
                    ELSE (oe-ordl.qty - v-bo-qty).
            END.

            ELSE
                v-bo-qty = IF inv-line.qty - inv-line.ship-qty LT 0
                    THEN 0 ELSE (inv-line.qty - inv-line.ship-qty).

            FOR EACH oe-boll
                WHERE oe-boll.company EQ inv-line.company
                AND oe-boll.ord-no  EQ inv-line.ord-no
                AND oe-boll.i-no    EQ inv-line.i-no
                AND oe-boll.line    EQ inv-line.line
                AND oe-boll.po-no   EQ inv-line.po-no
                AND CAN-FIND(FIRST oe-bolh
                WHERE oe-bolh.b-no   EQ oe-boll.b-no
                AND oe-bolh.posted EQ YES)
                NO-LOCK:

                IF oe-boll.p-c THEN v-bo-qty = 0.
            END. /* each oe-boll */
                                  
            FOR EACH oe-boll
                WHERE oe-boll.company EQ inv-line.company
                AND oe-boll.b-no    EQ inv-line.b-no
                AND oe-boll.ord-no  EQ inv-line.ord-no
                AND oe-boll.i-no    EQ inv-line.i-no
                AND oe-boll.line    EQ inv-line.line
                AND oe-boll.po-no   EQ inv-line.po-no
                AND CAN-FIND(FIRST oe-bolh
                WHERE oe-bolh.b-no   EQ oe-boll.b-no
                AND oe-bolh.posted EQ YES)
                NO-LOCK:
                /** Build Case Count Display Lines **/
                IF oe-boll.cases NE 0 AND oe-boll.qty-case NE 0 THEN
                    ASSIGN v-case-line = STRING(oe-boll.cases) + " @ " +
                                     string(oe-boll.qty-case).
                ELSE ASSIGN v-case-line = "".
                IF oe-boll.partial NE 0 THEN
                    ASSIGN v-part-line = "1" + " @ " + string(oe-boll.partial).
                ELSE ASSIGN v-part-line = "".

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

            IF inv-line.tax AND AVAILABLE stax THEN
            DO i = 1 TO 3:
                IF stax.tax-code[i] NE "" THEN 
                DO:
                    CREATE w-tax.
                    ASSIGN
                        w-dsc      = stax.tax-dscr[i]
                        w-tax      = ROUND((IF stax.accum-tax THEN v-t-price
                                                             ELSE inv-line.t-price) *
                                   stax.tax-rate[i] / 100,2)
                        v-t-price  = v-t-price + w-tax
                        v-t-tax[i] = v-t-tax[i] + w-tax
                        v-lines    = v-lines + 1.
                END.
            END.

            FIND FIRST reftable WHERE
                reftable.reftable EQ "inv-line.lot-no" AND
                reftable.rec_key EQ inv-line.rec_key
                USE-INDEX rec_key
                NO-LOCK NO-ERROR.

            IF AVAILABLE reftable THEN
                v-lot-no = reftable.CODE.

            IF v-t-price NE inv-line.t-price THEN 
            DO:
                CREATE w-tax.
                ASSIGN
                    w-dsc   = "******ITEM TOTAL:"
                    w-tax   = v-t-price
                    v-lines = v-lines + 1.
            END.
            
            v-price-head = inv-line.pr-uom.
          
            ASSIGN 
                inrowcount = inrowcount + 1.

            /* Line# */
            ASSIGN 
                v-cell = "R" + STRING(inrowcount) + "C1".
            chExcelApplication:Goto(v-cell) NO-ERROR.
            ASSIGN 
                chExcelApplication:ActiveCell:Value = inv-line.line.

            /* Item Desc */
            ASSIGN 
                v-cell = "R" + STRING(inrowcount) + "C2".
            chExcelApplication:Goto(v-cell) NO-ERROR.
            ASSIGN 
                chExcelApplication:ActiveCell:Value = v-i-dscr .

            /* Line Item PO# */
            ASSIGN 
                v-cell = "R" + STRING(inrowcount) + "C19".
            chExcelApplication:Goto(v-cell) NO-ERROR.
            ASSIGN 
                chExcelApplication:ActiveCell:Value = v-i-no /*inv-line.po-no*/ .

            /* Customer Item */
            ASSIGN 
                v-cell = "R" + STRING(inrowcount) + "C26".
            chExcelApplication:Goto(v-cell) NO-ERROR.
            ASSIGN 
                chExcelApplication:ActiveCell:Value = inv-line.ord-no.

            /* Southpak Item */
            ASSIGN 
                v-cell = "R" + STRING(inrowcount) + "C30".
            chExcelApplication:Goto(v-cell) NO-ERROR.
            ASSIGN 
                chExcelApplication:ActiveCell:Value = v-lot-no.
          
            /* Order Qty */
            ASSIGN 
                v-cell = "R" + STRING(inrowcount) + "C34".
            chExcelApplication:Goto(v-cell) NO-ERROR.
            ASSIGN 
                chExcelApplication:ActiveCell:Value = inv-line.po-no.
          
            /* Ship Qty */
            ASSIGN 
                v-cell = "R" + STRING(inrowcount) + "C41".
            chExcelApplication:Goto(v-cell) NO-ERROR.
            ASSIGN 
                chExcelApplication:ActiveCell:Value = inv-line.inv-qty .
          
            /*     /* B/O */
                 ASSIGN v-cell = "R" + STRING(inrowcount) + "C38".
                 chExcelApplication:Goto(v-cell) NO-ERROR.
                 ASSIGN chExcelApplication:ActiveCell:Value = v-bo-qty.
       
                 /* UM */
                 ASSIGN v-cell = "R" + STRING(inrowcount) + "C41".
                 chExcelApplication:Goto(v-cell) NO-ERROR.
                 ASSIGN chExcelApplication:ActiveCell:Value = v-price-head.*/

            /* Price */
            ASSIGN 
                v-cell = "R" + STRING(inrowcount) + "C45".
            chExcelApplication:Goto(v-cell) NO-ERROR.
            ASSIGN 
                chExcelApplication:ActiveCell:Value = v-price .

            /* Ext Amount */
            ASSIGN 
                v-cell = "R" + STRING(inrowcount) + "C48".
            chExcelApplication:Goto(v-cell) NO-ERROR.
            ASSIGN 
                chExcelApplication:ActiveCell:Value = inv-line.t-price .

            /*     if inv-line.tax then
                 do :
                   /* Tax */
                   ASSIGN v-cell = "R" + STRING(inrowcount) + "C50".
                   chExcelApplication:Goto(v-cell) NO-ERROR.
                   ASSIGN chExcelApplication:ActiveCell:Value = "Y" .
                 end.
                 else do :
                   /* Tax */
                   ASSIGN v-cell = "R" + STRING(inrowcount) + "C50".
                   chExcelApplication:Goto(v-cell) NO-ERROR.
                   ASSIGN chExcelApplication:ActiveCell:Value = "N" .
                 end.*/
          
            v-printline = v-printline + 2.
            IF v-printline >= 50 THEN 
            DO:
                PAGE.
                v-printline = 24.
            END.
        END. /* each inv-line */

        FOR EACH inv-misc NO-LOCK WHERE inv-misc.company = inv-head.company AND
            inv-misc.r-no = inv-head.r-no AND
            inv-misc.bill = "Y" BREAK BY ord-no WITH FRAME detailm:
            IF v-printline >= 50 THEN 
            DO:
                PAGE.
                v-printline = 24.
            END.

            IF FIRST(inv-misc.ord-no) THEN
            DO:
                ASSIGN 
                    inrowcount = inrowcount + 2.

                /* Item Desc */
                ASSIGN 
                    v-cell = "R" + STRING(inrowcount) + "C2".
                chExcelApplication:Goto(v-cell) NO-ERROR.
                ASSIGN 
                    chExcelApplication:ActiveCell:Value = "** Miscellaneous Items **"
                    inrowcount                          = inrowcount + 1
                    v-printline                         = v-printline + 2.
            END.

            /* Item Desc */
            ASSIGN 
                inrowcount = inrowcount + 1
                v-cell     = "R" + STRING(inrowcount) + "C2".

            chExcelApplication:Goto(v-cell) NO-ERROR.
            ASSIGN 
                chExcelApplication:ActiveCell:Value = inv-misc.charge + "-" +  inv-misc.dscr

                /* Ext Amount */
                v-cell                              = "R" + STRING(inrowcount) + "C49".
            chExcelApplication:Goto(v-cell) NO-ERROR.
            ASSIGN 
                chExcelApplication:ActiveCell:Value = inv-misc.amt
                v-subtot-lines                      = v-subtot-lines + inv-misc.amt
                v-printline                         = v-printline + 1.

            IF inv-misc.tax AND AVAILABLE stax THEN
            DO i = 1 TO 3:
                IF stax.tax-code[i] NE "" THEN 
                DO:
                    CREATE w-tax.
                    ASSIGN
                        w-dsc      = stax.tax-dscr[i]
                        w-tax      = IF stax.accum-tax THEN v-t-price
                              ELSE inv-misc.amt
                        w-tax      = ROUND(w-tax * (1 + (stax.tax-rate[i] / 100)),2) - w-tax
                        v-t-price  = v-t-price + w-tax
                        v-t-tax[i] = v-t-tax[i] + w-tax
                        v-lines    = v-lines + 1.
                END.
            END.

            IF v-t-price NE inv-misc.amt THEN 
            DO:
                CREATE w-tax.
                ASSIGN
                    w-dsc   = "******ITEM TOTAL:"
                    w-tax   = v-t-price
                    v-lines = v-lines + 1.
            END.
        END. /* each inv-misc */

        IF v-prntinst THEN 
        DO:
          
            {custom/notesprt.i inv-head v-inst 4}
            DO i = 1 TO 4:
                IF v-inst[i] <> "" THEN 
                DO:                
                    IF v-printline > 50 THEN 
                    DO:
                        PAGE.
                        v-printline = 0.
                   
                    END.
                 
                    ASSIGN 
                        inrowcount = inrowcount + 1
                        v-cell     = "R" + STRING(inrowcount) + "C23".

                    chExcelApplication:Goto(v-cell) NO-ERROR.
                    ASSIGN 
                        chExcelApplication:ActiveCell:Value = v-inst[i].

                    v-printline = v-printline + 1.
                END.
            END.
        END.

        IF v-printline >= 50 THEN
            v-printline = 24.

        v-frt-tax = inv-head.t-inv-freight.
        IF inv-head.tax-gr <> "" AND
            inv-head.f-bill AND inv-head.t-inv-freight <> 0 AND AVAILABLE stax THEN
        DO i = 1 TO 3:

            IF stax.tax-code[i] NE "" AND stax.tax-frt[i] THEN 
            DO:
                CREATE w-tax.
                ASSIGN
                    w-dsc      = stax.tax-dscr[i]
                    w-tax      = ROUND((IF stax.accum-tax THEN v-frt-tax
                                                         ELSE inv-head.t-inv-freight) *
                                        stax.tax-rate[i] / 100,2)                 
                    v-frt-tax  = v-frt-tax + w-tax
                    v-t-tax[i] = v-t-tax[i] + w-tax
                    v-lines    = v-lines + 1.
            END.
        END.
    END. /* DO TRANSACTION */

    DO i = 1 TO 3:
        v-bot-lab[i] = IF v-t-tax[i] NE 0 THEN
            ((IF AVAILABLE stax THEN STRING(CAPS(stax.tax-code[i]),"x(5)") 
            ELSE FILL(" ",5) ) +
            fill(" ",6) + ":" +
            string(v-t-tax[i],"->>>>>9.99")) ELSE "".
    END.
    v-inv-freight = IF inv-head.f-bill THEN inv-head.t-inv-freight ELSE 0.

    RUN util/GetBottomRow.p (INPUT 14, INPUT 30, INPUT 4, INPUT inrowcount, OUTPUT inrowcount).
    
    ASSIGN 
        v-cell = STRING(inrowcount) + ":99". 
    chExcelApplication:Rows(v-cell):SELECT.
    chExcelApplication:SELECTION:DELETE. 

    ASSIGN 
        v-cell = "R" + STRING(inrowcount) + "C39".
    chExcelApplication:Goto(v-cell) NO-ERROR.
    ASSIGN 
        chExcelApplication:ActiveCell:Value = v-cartot-lines.

    ASSIGN 
        inrowcount = inrowcount + 1 .

    ASSIGN 
        v-cell = "R" + STRING(inrowcount) + "C45".
    chExcelApplication:Goto(v-cell) NO-ERROR.
    ASSIGN 
        chExcelApplication:ActiveCell:Value = v-subtot-lines.

    ASSIGN 
        inrowcount = inrowcount + 1
        tax-dscr-1 = ""
        tax-dscr-2 = ""
        tax-dscr-3 = "".

    IF AVAILABLE cust THEN
    DO:
        FIND FIRST stax WHERE
            stax.company EQ cocode AND
            stax.tax-group EQ cust.tax-gr
            NO-LOCK NO-ERROR.

        IF AVAILABLE stax THEN
        DO:
            ASSIGN 
                tax-dscr-1 = stax.tax-dscr[1]
                tax-dscr-2 = stax.tax-dscr[2]
                tax-dscr-3 = stax.tax-dscr[3].
            RELEASE stax.
        END.
    END.

    /* v-cell = "R" + STRING(inrowcount) + "C35".
     chExcelApplication:Goto(v-cell) NO-ERROR.
     ASSIGN chExcelApplication:ActiveCell:Value = tax-dscr-1
            v-cell = "R" + STRING(inrowcount) + "C45".
 
     chExcelApplication:Goto(v-cell) NO-ERROR.
     ASSIGN chExcelApplication:ActiveCell:Value = v-t-tax[1].
 
     ASSIGN inrowcount = inrowcount + 1
            v-cell = "R" + STRING(inrowcount) + "C35".
 
     chExcelApplication:Goto(v-cell) NO-ERROR.
     ASSIGN chExcelApplication:ActiveCell:Value = tax-dscr-2
            v-cell = "R" + STRING(inrowcount) + "C45".
 
     chExcelApplication:Goto(v-cell) NO-ERROR.
     ASSIGN chExcelApplication:ActiveCell:Value = v-t-tax[2].
 
     ASSIGN inrowcount = inrowcount + 1
            v-cell = "R" + STRING(inrowcount) + "C35".
 
     chExcelApplication:Goto(v-cell) NO-ERROR.
     ASSIGN chExcelApplication:ActiveCell:Value = tax-dscr-3
            v-cell = "R" + STRING(inrowcount) + "C45".
 
     chExcelApplication:Goto(v-cell) NO-ERROR.
     ASSIGN chExcelApplication:ActiveCell:Value = v-t-tax[3].
             
     ASSIGN inrowcount = inrowcount + 1                               */
    v-cell = "R" + STRING(inrowcount) + "C45".
    chExcelApplication:Goto(v-cell) NO-ERROR.
    ASSIGN 
        chExcelApplication:ActiveCell:Value = v-inv-freight
        inrowcount                          = inrowcount + 1
        v-cell                              = "R" + STRING(inrowcount) + "C45".

    chExcelApplication:Goto(v-cell) NO-ERROR.
    ASSIGN 
        chExcelApplication:ActiveCell:Value = (v-subtot-lines + v-inv-freight) /*inv-head.t-inv-rev*/.

    chExcelApplication:Goto("R15C1") NO-ERROR.
    OS-DELETE value(v-dir + STRING(inv-head.inv-no) + ".xls").     
    OS-DELETE value(v-dir + "asi.pdf").
    OS-DELETE value(v-dir + STRING(inv-head.inv-no) + ".pdf").
    IF LvOutputSelection = "PRINTER" THEN
    DO:
        IF LvFirstTimePrint = NO THEN
        DO :
            chExcelApplication:Dialogs(8):Show.
            chWorkbook:Close(NO) NO-ERROR.
            ASSIGN 
                LvFirstTimePrint = YES.
        END.
        ELSE 
        DO :
            chExcelApplication:ActiveWindow:SelectedSheets:PrintOut().
            chWorkbook:Close(NO) NO-ERROR. 
        END.
    END.

    IF LvOutputSelection = "Email" THEN
    DO:
        chExcelApplication:ActiveSheet:SaveAs(v-dir + STRING(inv-head.inv-no) + ".xls") NO-ERROR. 	   
        NO-RETURN-VALUE chWorkbook:ExportAsFixedFormat(0, v-dir + "invoice.pdf").
        /*  chExcelApplication:ActiveWindow:SelectedSheets:PrintOut().*/
        /*       chWorkbook:Close(no) no-error.      */
        /*       chExcelApplication:Quit() no-error. */
        PAUSE 3.

        /*       OS-DELETE VALUE(v-dir + STRING(inv-head.inv-no) + ".xls").       */
        /*                                                                        */
        /*       OS-RENAME VALUE(v-dir + "asi.pdf") VALUE(v-dir + "invoice.pdf"). */
       		
        ASSIGN 
            LvCtr = LvCtr + 1.
        CREATE tt-filelist.
        ASSIGN 
            tt-FileCtr  = LvCtr
            tt-FileName = v-dir + "invoice.pdf".
    END.
    /*     ELSE IF LvOutputSelection = "Screen" THEN               */
    /*     DO:                                                     */
    /*       chExcelApplication:ActiveSheet:Protect("advance4me"). */
    /*     END.                                                    */

    /* RELEASE OBJECTS */
    RELEASE OBJECT chWorkbook NO-ERROR.
    RELEASE OBJECT chWorkSheet NO-ERROR.
    RELEASE OBJECT chHyper NO-ERROR.

END. /* each xinv-head */

/* END ---------------------------------- copr. 1996 Advanced Software, Inc. */
/* Clean up and extra processing */
/* Merge all the PDF Files */

/* ASSIGN CommandString = CurrDir + "\util\pdftk ".                               */
/*                                                                                */
/* os-delete VALUE(v-dir + "Invoice.pdf").                                        */
/*                                                                                */
/* FOR EACH tt-filelist :                                                         */
/*   assign CommandString = CommandString + " " + tt-FileName .                   */
/* END.                                                                           */
/*                                                                                */
/* assign CommandString = CommandString + " cat output " + v-dir + "Invoice.pdf". */
/* os-command silent value(CommandString).                                        */
/* FOR EACH tt-filelist :                                                         */
/*   os-delete value(tt-FileName).                                                */
/* END.                                                                           */
WshNetwork:SetDefaultPrinter(CurActivePrinter).
IF LvOutputSelection = "PRINTER" OR LvOutputSelection = "EMAIL" THEN
DO:
    chExcelApplication:Quit() NO-ERROR.
END.

chExcelApplication:ScreenUpdating = TRUE.

RELEASE OBJECT WshNetwork NO-ERROR.
RELEASE OBJECT chExcelApplication NO-ERROR.
