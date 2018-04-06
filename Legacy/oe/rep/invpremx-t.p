/* ---------------------------------------------- oe/rep/invpremx.p */
/* PRINT INVOICE   Xprint form for Premier Pkg  (PremierX and PremierS)       */
/* -------------------------------------------------------------------------- */
DEFINE INPUT PARAMETER ip-copy-title AS cha NO-UNDO.
DEFINE INPUT PARAMETER ip-print-s AS LOG NO-UNDO. /* for PremierS */
/* -------------------------------------------------- sys/inc/var.i 01/02 JLF */
/*                                                                            */
/* vars & constants include file                                              */
/*                                                                            */
/* -------------------------------------------------------------------------- */

DEFINE SHARED VARIABLE cocode           AS CHARACTER FORMAT "x(3)" NO-UNDO.
DEFINE SHARED VARIABLE locode           AS CHARACTER FORMAT "x(5)" NO-UNDO.

DEFINE SHARED VARIABLE x                AS INTEGER  NO-UNDO.
DEFINE SHARED VARIABLE y                AS INTEGER  NO-UNDO.
DEFINE SHARED VARIABLE k                AS INTEGER  NO-UNDO.

DEFINE        VARIABLE i                AS INTEGER  NO-UNDO.
DEFINE        VARIABLE j                AS INTEGER  NO-UNDO.

DEFINE        VARIABLE z                AS INTEGER  NO-UNDO.
DEFINE        VARIABLE xxx              AS DECIMAL  NO-UNDO.
DEFINE        VARIABLE yyy              AS DECIMAL  NO-UNDO.
DEFINE        VARIABLE zzz              AS DECIMAL  NO-UNDO.
DEFINE        VARIABLE tmpstore         AS cha  NO-UNDO.

/* end ---------------------------------- copr. 2001  advanced software, inc. */

 

/* invoice.i */

DEFINE SHARED VARIABLE fcust            LIKE ar-inv.cust-no.
DEFINE SHARED VARIABLE tcust            LIKE fcust INIT "zzzzzzzz".
DEFINE SHARED VARIABLE finv             LIKE ar-inv.inv-no FORMAT ">>>>>>".
DEFINE SHARED VARIABLE tinv             LIKE finv INIT 999999.
DEFINE SHARED VARIABLE v-prntinst       AS LOG  INIT YES.
DEFINE SHARED VARIABLE v-reprint        AS LOG  INIT NO.
DEFINE SHARED VARIABLE v-sort           AS LOG  FORMAT "Customer/BOL" INIT YES.
DEFINE SHARED VARIABLE v-term-id        AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE fdate            AS DATE INIT TODAY NO-UNDO.
DEFINE SHARED VARIABLE tdate            LIKE fdate.
DEFINE SHARED VARIABLE fbol             LIKE oe-bolh.bol-no FORMAT ">>>>>>" INIT 0 NO-UNDO.
DEFINE SHARED VARIABLE tbol             LIKE oe-bolh.bol-no INIT 999999 NO-UNDO.
DEFINE SHARED VARIABLE v-print-fmt      AS CHARACTER NO-UNDO FORMAT 'x'.
DEFINE SHARED VARIABLE v-print-head     AS LOG  NO-UNDO.
DEFINE SHARED VARIABLE v-lines-per-page AS INTEGER  NO-UNDO.
DEFINE SHARED VARIABLE v-print-dept     AS LOG  NO-UNDO.
DEFINE SHARED VARIABLE v-depts          AS CHARACTER NO-UNDO.

DEFINE        VARIABLE v-last-page      AS INTEGER  NO-UNDO.
DEFINE        VARIABLE v-page-tot       AS INTEGER  NO-UNDO.
DEFINE        VARIABLE v-sort-name      AS LOG  NO-UNDO.

/* rstark 05291402 */

/* rstark 05291402 */
DO TRANSACTION:
  
    DEFINE VARIABLE prt-copies AS INTEGER FORMAT ">9" INIT 1 NO-UNDO.


    FIND FIRST sys-ctrl
        WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "INVCOPYS"
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE sys-ctrl THEN 
    DO:
        CREATE sys-ctrl.
        ASSIGN
            sys-ctrl.company = cocode
            sys-ctrl.name    = "INVCOPYS"
            sys-ctrl.descrip = "Int Val: # Invoice Copies to Print. " +
                      "Log Val: Sort by Cust Name?".
      
        MESSAGE "Please enter the default number of invoice copies"
            UPDATE sys-ctrl.int-fld.
    END.
    ASSIGN
        prt-copies  = sys-ctrl.int-fld
        v-sort-name = sys-ctrl.log-fld.
 
END.
/* rstark 05291402 */

/* rstark 05291402 */
 

DEFINE        VARIABLE v-salesman       AS CHARACTER    FORMAT "x(14)" NO-UNDO.
DEFINE        VARIABLE v-salesname      AS CHARACTER    FORMAT "x(30)" NO-UNDO.
DEFINE        VARIABLE v-fob            AS CHARACTER    FORMAT "x(27)" NO-UNDO.
DEFINE        VARIABLE v-shipvia        LIKE carrier.dscr NO-UNDO. 
DEFINE        VARIABLE v-addr3          AS CHARACTER    FORMAT "x(30)" NO-UNDO.
DEFINE        VARIABLE v-email          AS CHARACTER    FORMAT "x(130)" NO-UNDO.
DEFINE        VARIABLE v-sold-addr3     AS CHARACTER    FORMAT "x(30)" NO-UNDO.
DEFINE        VARIABLE v-shipto-name    AS CHARACTER    FORMAT "x(30)" NO-UNDO.
DEFINE        VARIABLE v-shipto-addr    AS CHARACTER    FORMAT "x(30)" EXTENT 2 NO-UNDO.
DEFINE        VARIABLE v-shipto-city    AS CHARACTER    FORMAT "x(15)" NO-UNDO.
DEFINE        VARIABLE v-shipto-state   AS CHARACTER    FORMAT "x(2)" NO-UNDO.
DEFINE        VARIABLE v-shipto-zip     AS CHARACTER    FORMAT "x(10)" NO-UNDO.
DEFINE        VARIABLE v-line           AS INTEGER     NO-UNDO.
DEFINE        VARIABLE v-printline      AS INTEGER     NO-UNDO.
DEFINE        VARIABLE v-t-weight       LIKE ar-invl.t-weight NO-UNDO.
DEFINE        VARIABLE v-tot-cas        AS DECIMAL     FORMAT "->>>9.9999" NO-UNDO.
DEFINE        VARIABLE v-tot-pallets    AS INTEGER     NO-UNDO.
DEFINE        VARIABLE v-tot-qty        AS INTEGER     NO-UNDO.
DEFINE        VARIABLE v-inv-date       AS DATE    INITIAL TODAY FORM "99/99/9999" NO-UNDO.
DEFINE SHARED VARIABLE v-fr-tax         AS LOGICAL INITIAL NO NO-UNDO.
DEFINE SHARED VARIABLE s-print-zero-qty AS LOG     NO-UNDO.
DEFINE        VARIABLE v-tax-rate       AS DECIMAL     FORMAT "->>>.99" NO-UNDO.
DEFINE        VARIABLE v-tax-code       LIKE stax.tax-code NO-UNDO.
DEFINE        VARIABLE v-tx-rate        LIKE stax.tax-rate NO-UNDO.
DEFINE        VARIABLE v-ans            AS LOGICAL INITIAL NO NO-UNDO.
DEFINE        VARIABLE v-date-ship      AS DATE    INITIAL TODAY NO-UNDO.
DEFINE        VARIABLE v-date-ord       AS DATE    NO-UNDO.
DEFINE        VARIABLE v-del-no         AS INTEGER     FORMAT ">>>>>>" NO-UNDO.
DEFINE        VARIABLE v-bol-cases      LIKE oe-boll.cases NO-UNDO.
DEFINE        VARIABLE v-set-qty        AS DECIMAL     NO-UNDO.
DEFINE        VARIABLE v-part-qty       AS DECIMAL     FORMAT "999.9999" NO-UNDO.
DEFINE        VARIABLE v-net            LIKE inv-head.t-inv-rev NO-UNDO.
DEFINE        VARIABLE v-case-cnt       AS CHARACTER    FORMAT "x(80)" EXTENT 5 NO-UNDO.
DEFINE        VARIABLE v-case-line      AS CHARACTER    NO-UNDO.
DEFINE        VARIABLE v-part-line      AS CHARACTER    NO-UNDO.
DEFINE        VARIABLE v-pc             AS cha     NO-UNDO. /* partial or complete */

DEFINE BUFFER xar-inv  FOR ar-inv .
DEFINE BUFFER xar-invl FOR ar-invl .
DEFINE BUFFER b-ar-inv FOR ar-inv.

DEFINE TEMP-TABLE w-sman NO-UNDO
    FIELD sman AS CHARACTER FORMAT "x(4)".

DEFINE VARIABLE v-ord-del-hdr  AS CHARACTER FORMAT "x(3)" INIT "Del".
DEFINE VARIABLE v-beeler-lines AS INTEGER.
DEFINE VARIABLE v-part-info    AS CHARACTER FORMAT "x(30)".
DEFINE VARIABLE v              AS INTEGER.
DEFINE VARIABLE v-bo-qty       AS INTEGER  FORMAT "99999" NO-UNDO.
DEFINE VARIABLE v-inv-qty      AS INTEGER  FORMAT "99999" NO-UNDO.
DEFINE VARIABLE v-inv-qtys     AS DECIMAL  NO-UNDO.
DEFINE VARIABLE v-ship-qty     AS INTEGER  FORMAT "99999" NO-UNDO.
DEFINE VARIABLE v-i-no         AS CHARACTER FORMAT "x(15)" NO-UNDO.
DEFINE VARIABLE v-i-dscr       AS CHARACTER FORMAT "x(18)" NO-UNDO.
DEFINE VARIABLE v-price        AS DECIMAL  FORMAT ">>>>9.9999" NO-UNDO.
DEFINE VARIABLE v-t-price      AS DECIMAL  FORMAT ">>>>>>9.99" NO-UNDO.
DEFINE VARIABLE v-po-no        LIKE ar-invl.po-no NO-UNDO.
DEFINE VARIABLE v-bill-i       AS CHARACTER FORMAT "x(25)" NO-UNDO.
DEFINE VARIABLE v-ord-no       LIKE oe-ord.ord-no NO-UNDO.
DEFINE VARIABLE v-ord-date     LIKE oe-ord.ord-date NO-UNDO.
DEFINE VARIABLE v-ship-i       AS CHARACTER FORMAT "x(25)" NO-UNDO.
DEFINE VARIABLE v-rel-po-no    LIKE oe-rel.po-no NO-UNDO.
DEFINE VARIABLE v-price-head   AS CHARACTER FORMAT "x(5)" NO-UNDO.
DEFINE VARIABLE v-subtot-lines AS DECIMAL  NO-UNDO.
DEFINE TEMP-TABLE w-tax NO-UNDO
    FIELD w-dsc AS CHARACTER
    FIELD w-tax AS DECIMAL.
DEFINE    VARIABLE      v-t-tax       AS DECIMAL     EXTENT 3 NO-UNDO.
DEFINE    VARIABLE      v-bot-lab     AS CHARACTER    FORMAT "x(63)" EXTENT 3 NO-UNDO.
DEFINE    VARIABLE      v-lines       AS INTEGER     NO-UNDO.
DEFINE    VARIABLE      v-inv-freight LIKE inv-head.t-inv-freight NO-UNDO.
DEFINE    VARIABLE      v-frt-tax     AS DECIMAL     NO-UNDO.
DEFINE VARIABLE lFirstLine    AS LOGICAL NO-UNDO.

FIND FIRST ar-inv NO-LOCK NO-ERROR.
/* === with xprint ====*/
DEFINE           VARIABLE      ls-image1    AS cha       NO-UNDO.
DEFINE           VARIABLE      ls-full-img1 AS cha       FORM "x(200)" NO-UNDO.
/*ASSIGN ls-image1 = "images\premiercan.jpg"          */
/*       FILE-INFO:FILE-NAME = ls-image1.             */
/*       ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".*/

DEFINE           VARIABLE      v-tel        AS cha       FORM "x(30)" NO-UNDO.
DEFINE           VARIABLE      v-fax        AS cha       FORM "x(30)" NO-UNDO.
DEFINE           VARIABLE      v-contact    AS cha       FORM "x(20)" NO-UNDO .

DEFINE           VARIABLE      v-comp-add1  AS cha       FORM "x(30)" NO-UNDO.
DEFINE           VARIABLE      v-comp-add2  AS cha       FORM "x(30)" NO-UNDO.
DEFINE           VARIABLE      v-comp-add3  AS cha       FORM "x(30)" NO-UNDO.
DEFINE           VARIABLE      v-comp-add4  AS cha       FORM "x(30)" NO-UNDO.

DEFINE           VARIABLE      iDaysToPay   AS INTEGER       NO-UNDO.
DEFINE           VARIABLE      cInvDate     AS CHARACTER      NO-UNDO.
DEFINE           VARIABLE      dOrigQty     AS DECIMAL       NO-UNDO.
DEFINE        VARIABLE cOrigUOM     AS CHARACTER NO-UNDO.
DEFINE        VARIABLE cXMLShipTo   AS CHARACTER NO-UNDO.

DEFINE        VARIABLE cRtnChar     AS CHARACTER NO-UNDO.
DEFINE        VARIABLE lRecFound    AS LOGICAL   NO-UNDO.
DEFINE        VARIABLE lChkImage    AS LOGICAL   NO-UNDO. 
DEFINE        VARIABLE cTaxCode     AS CHARACTER NO-UNDO.
DEFINE        VARIABLE cCurCode     AS CHARACTER NO-UNDO.
DEFINE        VARIABLE cCompanyID   AS CHARACTER NO-UNDO.

/* rstark 05181205 */
/* XMLOutput.i - RStark - Task: 05181205 (XML), 05291402 (cXML) */
/* syntax: {XMLOutput/XMLOutput.i &NEW=NEW &XMLSysCtrl=XML??? &Company=cocode} -or-
           {XMLOutput/XMLOutput.i &NEW=NEW &cXMLSysCtrl=cXML??? &Company=cocode &c=c} -or-
           {XMLOutput.i &XMLOutput=XML???} -or-
           {XMLOutput.i &cXMLOutput=cXML??? &c=c} -or-
           {XMLOutput.i &XMLOutput=XML??? &Company=cocode} -or-
           {XMLOutput.i &cXMLOutput=cXML??? &Company=cocode &c=c} -or-
           {XMLOutput.i &XMLClose} -or-
           {XMLOutput.i &c=c &XMLClose} */


DEFINE SHARED VARIABLE cXMLOutput   AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE lXMLOutput   AS LOGICAL   NO-UNDO.
DEFINE SHARED VARIABLE iXMLOutput   AS INTEGER   NO-UNDO.
  
DEFINE  SHARED STREAM XMLOutput.

DEFINE VARIABLE XMLHeader       AS LOGICAL   NO-UNDO INITIAL TRUE.
DEFINE VARIABLE XMLLineNumber   AS INTEGER   NO-UNDO.
DEFINE VARIABLE XMLPage         AS LOGICAL   NO-UNDO.
  
  
  
  
    
DEFINE VARIABLE XMLTemp         AS CHARACTER NO-UNDO.
DEFINE VARIABLE XMLFile         AS CHARACTER NO-UNDO.
DEFINE VARIABLE XMLTimeStamp    AS CHARACTER NO-UNDO.
DEFINE VARIABLE XMLPayloadID    AS CHARACTER NO-UNDO.
DEFINE VARIABLE XMLIdentity     AS CHARACTER NO-UNDO.
DEFINE VARIABLE XMLIdentityCust AS CHARACTER NO-UNDO.
DEFINE VARIABLE XMLSharedSecret AS CHARACTER NO-UNDO.
DEFINE VARIABLE XMLProduction   AS CHARACTER NO-UNDO.
DEFINE VARIABLE XMLProcessID    AS CHARACTER NO-UNDO.
DEFINE VARIABLE XMLDTD          AS CHARACTER NO-UNDO.

IF lXMLOutput THEN 
DO:
    FIND FIRST sys-ctrl NO-LOCK
        WHERE sys-ctrl.company EQ  cocode
        AND sys-ctrl.name    EQ 'XMLInvoice' NO-ERROR.
    IF AVAILABLE sys-ctrl AND sys-ctrl.log-fld THEN 
    DO:
        ASSIGN
            XMLTimeStamp = STRING(YEAR(TODAY),'9999')
                             + '-'
                             + STRING(MONTH(TODAY),'99')
                             + '-'
                             + STRING(DAY(TODAY),'99')
                             + 'T'
                             + STRING(TIME,'hh:mm:ss')
                             + '-05:00'
            cXMLOutput   = sys-ctrl.char-fld
            iXMLOutput   = sys-ctrl.int-fld
            .
          
        ASSIGN
            XMLFile = '/XMLInvoice.' + STRING(TIME,'99999') + '.xml'
            XMLTemp = 'XMLOutput' + XMLFile
            .
        OUTPUT STREAM XMLOutput TO VALUE(XMLTemp).
          
    END.
    ELSE lXMLOutput = NO.
END. /* if lXMLOutput */
    
    
    
/* syntax: RUN XMLOutput (lXMLOutput,'<Label>',<Value>,'<Header|Element|Value>' */
PROCEDURE XMLOutput:
    DEFINE INPUT PARAMETER plOutput AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER pcLabel AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pcValue AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pcType AS CHARACTER NO-UNDO.

    IF NOT plOutput THEN RETURN.

    CASE pcType:
        WHEN 'Header' THEN
            PUT STREAM XMLOutput UNFORMATTED 
                '<?xml version="1.0"?>'
                '<dsXML xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">'.
        WHEN 'Close' THEN
            PUT STREAM XMLOutput UNFORMATTED 
                '</dsXML>' SKIP.
        WHEN 'Page' THEN 
            DO:
                XMLLineNumber = 0.
                IF pcLabel EQ 'Last' THEN
                    PUT STREAM XMLOutput UNFORMATTED '</Page_' pcValue '>'.
                ELSE 
                DO:
                    IF INT(pcValue) GT 1 AND XMLPage THEN
                        PUT STREAM XMLOutput UNFORMATTED '</Page_' INT(pcValue) - 1 '>'.
                    PUT STREAM XMLOutput UNFORMATTED 
                        '<Page_' pcValue '>'.
                    XMLPage = YES.
                END.
            END.
        WHEN 'Row' THEN
            PUT STREAM XMLOutput UNFORMATTED 
                '<' pcLabel '>'.
        WHEN 'Col' THEN
            IF pcValue NE '' AND pcValue NE ? THEN 
            DO:
                ASSIGN /* remove special characters with escape values */
                    pcValue = REPLACE(pcValue,'~&','~&amp;')
                    pcValue = REPLACE(pcValue,'~'','~&apos;')
                    pcValue = REPLACE(pcValue,'"','~&quot;')
                    pcValue = REPLACE(pcValue,'<','~&lt;')
                    pcValue = REPLACE(pcValue,'>','~&gt;')
                    .
                PUT STREAM XMLOutput UNFORMATTED 
                    '<' pcLabel '>' pcValue '</' pcLabel '>'.
            END.
            ELSE
                PUT STREAM XMLOutput UNFORMATTED '<' pcLabel '/>'.
    END CASE.
END PROCEDURE.
    
  

 
RUN XMLOutput (lXMLOutput,'','','Header').
/* rstark 05181205 */

/* rstark 05291402 */
/* XMLOutput.i - RStark - Task: 05181205 (XML), 05291402 (cXML) */
/* syntax: {XMLOutput/XMLOutput.i &NEW=NEW &XMLSysCtrl=XML??? &Company=cocode} -or-
           {XMLOutput/XMLOutput.i &NEW=NEW &cXMLSysCtrl=cXML??? &Company=cocode &c=c} -or-
           {XMLOutput.i &XMLOutput=XML???} -or-
           {XMLOutput.i &cXMLOutput=cXML??? &c=c} -or-
           {XMLOutput.i &XMLOutput=XML??? &Company=cocode} -or-
           {XMLOutput.i &cXMLOutput=cXML??? &Company=cocode &c=c} -or-
           {XMLOutput.i &XMLClose} -or-
           {XMLOutput.i &c=c &XMLClose} */


DEFINE SHARED VARIABLE ccXMLOutput AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE clXMLOutput AS LOGICAL   NO-UNDO.
DEFINE SHARED VARIABLE ciXMLOutput AS INTEGER   NO-UNDO.
  
DEFINE  SHARED STREAM cXMLOutput.

DEFINE VARIABLE cXMLHeader       AS LOGICAL   NO-UNDO INITIAL TRUE.
DEFINE VARIABLE cXMLLineNumber   AS INTEGER   NO-UNDO.
DEFINE VARIABLE cXMLPage         AS LOGICAL   NO-UNDO.
  
  
  
  
    
DEFINE VARIABLE cXMLTemp         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cXMLFile         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cXMLTimeStamp    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cXMLPayloadID    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cXMLIdentity     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cXMLIdentityCust AS CHARACTER NO-UNDO.
DEFINE VARIABLE cXMLSharedSecret AS CHARACTER NO-UNDO.
DEFINE VARIABLE cXMLProduction   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cXMLProcessID    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cXMLDTD          AS CHARACTER NO-UNDO.

IF clXMLOutput THEN 
DO:
    FIND FIRST sys-ctrl NO-LOCK
        WHERE sys-ctrl.company EQ  cocode
        AND sys-ctrl.name    EQ 'cXMLInvoice' NO-ERROR.
    IF AVAILABLE sys-ctrl AND sys-ctrl.log-fld THEN 
    DO:
        ASSIGN
            cXMLTimeStamp = STRING(YEAR(TODAY),'9999')
                             + '-'
                             + STRING(MONTH(TODAY),'99')
                             + '-'
                             + STRING(DAY(TODAY),'99')
                             + 'T'
                             + STRING(TIME,'hh:mm:ss')
                             + '-05:00'
            ccXMLOutput   = sys-ctrl.char-fld
            ciXMLOutput   = sys-ctrl.int-fld
            .
          
    END.
    ELSE clXMLOutput = NO.
END. /* if lXMLOutput */
    
    
    
FUNCTION getPayLoadID RETURN CHARACTER (ipProcessID AS CHARACTER):
    DEFINE VARIABLE cPayLoadID AS CHARACTER NO-UNDO.
  
    ASSIGN
        cPayLoadID = STRING(NOW)
        cPayLoadID = REPLACE(cPayLoadID,'/','')
        cPayLoadID = REPLACE(cPayLoadID,' ','')
        cPayLoadID = REPLACE(cPayLoadID,' ','')
        cPayLoadID = REPLACE(cPayLoadID,':','')
        cPayLoadID = REPLACE(cPayLoadID,'-','')
        cPayLoadID = REPLACE(cPayLoadID,'.','')
        cPayLoadID = cPayLoadID + IF ipProcessID NE '' THEN '.' + ipProcessID ELSE ''
        cPayLoadID = cPayLoadID + '.' + STRING(RANDOM(1000,9999),'9999')
        cPayLoadID = cPayLoadID + '@PremPack.com'
        .
  
    RETURN cPayLoadID.
END FUNCTION.
/* syntax: RUN XMLOutput (lXMLOutput,'<Label>',<Value>,'<Header|Element|Value>' */
PROCEDURE cXMLOutput:
    DEFINE INPUT PARAMETER plOutput AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER pcLabel AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pcValue AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pcType AS CHARACTER NO-UNDO.

    IF NOT plOutput THEN RETURN.

    CASE pcType:
        WHEN 'Header' THEN
            PUT STREAM cXMLOutput UNFORMATTED
                '<?xml version="1.0" encoding="UTF-8" ?>'
                '<!DOCTYPE cXML SYSTEM "' + cXMLDTD + '">'
                '<cXML payloadID="' getPayloadID(cXMLProcessID) '" timestamp="' cXMLTimeStamp '" xml:lang="en-US" version="1.2.024">'
                '<Header>'
                '<From>'
                '<Credential domain="NetworkID">'
                '<Identity>' + cXMLIdentityCust + '</Identity>'
                '<SharedSecret>' + cXMLSharedSecret + '</SharedSecret>'
                '</Credential>'
                '</From>'
                '<To>'
                '<Credential domain="NetworkID">'
                '<Identity>' cXMLIdentity '</Identity>'
                '</Credential>'
                '</To>'
                '<Sender>'
                '<Credential domain="NetworkID">'
                '<Identity>' + cXMLIdentityCust + '</Identity>'
                '<SharedSecret>' + cXMLSharedSecret + '</SharedSecret>'
                '</Credential>'
                '<UserAgent>Supplier</UserAgent>'
                '</Sender>'
                '</Header>'
                .
        WHEN 'Close' THEN
            PUT STREAM cXMLOutput UNFORMATTED 
                '</cXML>' SKIP.
        WHEN 'Row' THEN
            PUT STREAM cXMLOutput UNFORMATTED 
                '<' pcLabel '>'.
        WHEN 'Col' THEN
            IF pcValue NE '' AND pcValue NE ? THEN 
            DO:
                ASSIGN /* remove special characters with escape values */
                    pcValue = REPLACE(pcValue,'~&','~&amp;')
                    pcValue = REPLACE(pcValue,'~'','~&apos;')
                    pcValue = REPLACE(pcValue,'"','~&quot;')
                    pcValue = REPLACE(pcValue,'<','~&lt;')
                    pcValue = REPLACE(pcValue,'>','~&gt;')
                    .
                IF pcLabel NE '' AND pcLabel NE ? THEN
                    PUT STREAM cXMLOutput UNFORMATTED '<' pcLabel '>' pcValue '</' pcLabel '>'.
                ELSE
                    PUT STREAM cXMLOutput UNFORMATTED pcValue.
            END.
            ELSE
                PUT STREAM cXMLOutput UNFORMATTED '<' pcLabel '/>'.
    END CASE.
END PROCEDURE.
    
  

 
/* rstark 05291402 */

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
                cTaxCode   = 'Sales Tax'
                cCompanyID = ''
                .
        ELSE 
            ASSIGN 
                cCurCode   = 'USD'
                cTaxCode   = 'Sales Tax'
                cCompanyID = ''
                .
RUN sys/ref/nk1look.p (INPUT company.company, "BusinessFormLogo", "C" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cRtnChar, OUTPUT lRecFound).

IF cRtnChar NE "" THEN 
DO:
    ASSIGN 
        lChkImage    = YES
        ls-full-img1 = SEARCH(ls-full-img1)
        ls-full-img1 = cRtnChar + ">".
END.
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
    FIRST xar-inv WHERE RECID(xar-inv) EQ report.rec-id NO-LOCK
    BREAK BY report.key-01
    BY (IF v-sort THEN "" ELSE report.key-02)
    BY report.key-03:

    FIND FIRST cust WHERE cust.company = xar-inv.company
        AND cust.cust-no = xar-inv.cust-no NO-LOCK NO-ERROR.

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
                AND sys-ctrl-shipto.cust-vend-no EQ xar-inv.cust-no
                AND sys-ctrl-shipto.log-fld EQ YES
                NO-ERROR.
            IF AVAILABLE sys-ctrl-shipto THEN 
                ASSIGN 
                    cXMLIdentity = sys-ctrl-shipto.char-fld
                    cXMLDTD      = 'http://xml.cxml.org/schemas/cXML/1.2.025/InvoiceDetail.dtd'.
        END.
        /* cXMLCust.i - RStark - Task: 05291402 (cXML) */
        /* syntax: {cXMLCust.i &cXMLSysCtrl=cXML??? &Company=??? &Customer=???} */

        FIND FIRST sys-ctrl-shipto NO-LOCK
            WHERE sys-ctrl-shipto.company EQ xar-inv.company
            AND sys-ctrl-shipto.name EQ 'cXMLInvoice'
            AND sys-ctrl-shipto.cust-vend EQ YES
            AND sys-ctrl-shipto.cust-vend-no EQ xar-inv.cust-no
            NO-ERROR.
        IF AVAILABLE sys-ctrl-shipto AND sys-ctrl-shipto.log-fld THEN 
        DO:
            ASSIGN
                cXMLIdentity = sys-ctrl-shipto.char-fld
                cXMLFile     = '/cXMLInvoice.' + xar-inv.cust-no + '.' + REPLACE(REPLACE(STRING(NOW),":",""),"/","") + '.xml' 
                cXMLTemp     = 'XMLOutput' + cXMLFile
                clXMLOutput  = YES
                .
            FIND FIRST sys-ctrl NO-LOCK
                WHERE sys-ctrl.company EQ xar-inv.company
                AND sys-ctrl.name EQ 'cXMLIdentity'
                NO-ERROR.
            IF AVAILABLE sys-ctrl THEN
                ASSIGN
                    cXMLIdentityCust = sys-ctrl.char-fld
                    cXMLProduction   = (IF sys-ctrl.log-fld THEN "production" ELSE "test")
                    .
            FIND FIRST sys-ctrl NO-LOCK
                WHERE sys-ctrl.company EQ xar-inv.company
                AND sys-ctrl.name EQ 'cXMLSecret'
                NO-ERROR.
            IF AVAILABLE sys-ctrl THEN
                ASSIGN
                    cXMLSharedSecret = sys-ctrl.char-fld
                    .

            OUTPUT STREAM cXMLOutput TO VALUE(cXMLTemp).
            RUN cXMLOutput (clXMLOutput,'','','Header').
        END.
        ELSE clXMLOutput = NO.
 
        FIND FIRST terms 
            WHERE terms.company EQ xar-inv.company
            AND terms.t-code EQ xar-inv.terms
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
        v-shipto-name    = xar-inv.sold-name
        v-shipto-addr[1] = xar-inv.sold-addr[1]
        v-shipto-addr[2] = xar-inv.sold-addr[2]
        v-shipto-city    = xar-inv.sold-city
        v-shipto-state   = xar-inv.sold-state
        v-shipto-zip     = xar-inv.sold-zip
        cXMLShipTo       = STRING(xar-inv.sold-no).

    v-del-no = 0.

    /* wfk   find first oe-bolh where oe-bolh.company = xar-inv.company and
            oe-bolh.bol-no = xar-inv.bol-no use-index bol-no no-lock no-error.*/
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
            WHERE sys-ctrl-shipto.company EQ xar-inv.company
            AND sys-ctrl-shipto.NAME EQ 'cXMLShipToPrefix'
            AND sys-ctrl-shipto.cust-vend EQ YES
            AND sys-ctrl-shipto.cust-vend-no EQ xar-inv.cust-no
            NO-ERROR.
        IF AVAILABLE sys-ctrl-shipto AND sys-ctrl-shipto.char-fld NE '' THEN 
            cXMLShipTo = TRIM(sys-ctrl-shipto.char-fld) + oe-bolh.ship-id.
    END. /* avail oe-bolh */  
    IF /*NOT v-reprint OR*/ xar-inv.inv-no EQ 0 THEN
        RUN oe/get-inv#.p (ROWID(xar-inv)).

    DO TRANSACTION:
        FIND ar-inv WHERE ROWID(ar-inv) EQ ROWID(xar-inv).
        
        IF ar-inv.inv-date NE ? THEN v-inv-date = ar-inv.inv-date.

        IF ar-inv.fob-code BEGINS "ORIG" THEN
            ASSIGN v-fob = "Origin".
        ELSE
            ASSIGN v-fob = "Destination".

        FIND FIRST carrier WHERE carrier.company = ar-inv.company AND
            carrier.carrier = ar-inv.carrier NO-LOCK NO-ERROR.
        IF AVAILABLE carrier THEN
            ASSIGN v-shipvia = carrier.dscr.
        ELSE
            ASSIGN v-shipvia = "".
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
            /* ------------------------------------------------- sys/ref/stax1W.i 2/01 JLF */
            /*                                                                            */
            /* where statement - sales tax reference file                                 */
            /*                                                                            */
            /* -------------------------------------------------------------------------- */


            WHERE (stax.tax-group              BEGINS cocode
            AND  substr(stax.tax-group,1,10) EQ cocode)
 

            /* end ---------------------------------- copr. 2001  advanced software, inc. */
 
            /* wfk and 
            substr(stax.tax-group,11,length(trim(stax.tax-group)) - 10)
            eq ar-inv.tax-gr */
            NO-LOCK NO-ERROR.
        /* wfk if not avail stax then
             find first stax where stax.tax-group eq ar-inv.tax-gr
                 no-lock no-error.*/
   
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
        FOR EACH xar-invl NO-LOCK WHERE xar-invl.x-no = ar-inv.x-no AND
            (s-print-zero-qty OR
            NOT(xar-invl.ship-qty EQ 0 AND xar-invl.inv-qty EQ 0))
            BREAK BY xar-invl.i-no:

            DO i = 1 TO 3:
                IF xar-invl.sman[i] NE "" THEN 
                DO:
                    CREATE w-sman.
                    ASSIGN 
                        w-sman.sman = xar-invl.sman[i].
                END.
            END.
            ASSIGN 
                v-tot-qty     = v-tot-qty + xar-invl.ship-qty
                v-t-weight    = v-t-weight + (ROUND(xar-invl.t-weight /
                            xar-invl.qty, 2) * xar-invl.inv-qty)
                v-tot-pallets = 0
                v-pc          = "C". /* complete*/
            FOR EACH oe-bolh NO-LOCK WHERE oe-bolh.b-no = xar-invl.b-no 
            /*oe-bolh.ord-no = xar-invl.ord-no*/ :
                v-pc = "P". /* partial*/ 
                FOR EACH oe-boll FIELDS(cases partial p-c) NO-LOCK WHERE
                    oe-boll.company = oe-bolh.company AND
                    oe-boll.b-no = oe-bolh.b-no AND
                    oe-boll.i-no = xar-invl.i-no AND
                    oe-boll.ord-no = xar-invl.ord-no:

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
         
            IF LAST-OF(xar-invl.i-no) THEN 
            DO:
                IF xar-invl.est-no NE "" THEN
                DO:
                    FIND FIRST eb WHERE eb.company = xar-invl.company AND
                        eb.est-no = xar-invl.est-no AND
                        eb.e-num = xar-invl.e-num AND
                        eb.form-no = xar-invl.form-no AND
                        eb.blank-no = xar-invl.blank-no NO-LOCK NO-ERROR.

                    IF xar-invl.form-no = 0 AND xar-invl.est-type = 2 THEN
                    DO:
                        FOR EACH fg-set NO-LOCK WHERE fg-set.company = xar-invl.company
                            AND fg-set.set-no = xar-invl.i-no:
                            ASSIGN 
                                v-set-qty = v-set-qty + fg-set.QtyPerSet.
                        END.
                        IF v-set-qty = 0 THEN
                            ASSIGN v-set-qty = 1.
                        FOR EACH eb NO-LOCK WHERE eb.company = xar-invl.company AND
                            eb.est-no = xar-invl.est-no AND
                            eb.e-num = xar-invl.e-num AND
                            eb.form-no NE 0:
                            FIND fg-set WHERE fg-set.company = xar-invl.company AND
                                fg-set.set-no = xar-invl.i-no  AND
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
        END. /* each xar-invl */
    
        /** Build Salesman Id String **/
        v-salesman = "".
        FOR EACH w-sman BREAK BY w-sman.sman:
            IF FIRST-OF(w-sman.sman) THEN
                ASSIGN v-salesman = v-salesman + w-sman.sman.
            DELETE w-sman.
        END.

        /*  wfk   find first oe-bolh where oe-bolh.company = ar-inv.company and
                oe-bolh.bol-no = ar-inv.bol-no
                USE-INDEX bol-no no-lock no-error. */
        IF AVAILABLE oe-bolh THEN
            ASSIGN v-rel-po-no = oe-bolh.po-no.

        FIND FIRST ar-invl WHERE ar-invl.x-no = ar-inv.x-no
            NO-LOCK NO-ERROR.
        IF AVAILABLE ar-invl THEN
        DO:
            ASSIGN 
                v-price-head = ar-invl.pr-uom.
            FIND FIRST oe-ord WHERE oe-ord.company = cocode AND
                oe-ord.ord-no = ar-invl.ord-no
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
                ASSIGN v-price-head = ar-invl.pr-uom.
        END.
        
        /* rstark 05181205 */
        XMLLineNumber = 0.
        RUN XMLOutput (lXMLOutput,'InvoiceHeader','','Row').
        RUN XMLOutput (lXMLOutput,'RemitTo','PREMIER PACKAGING','Col').
        RUN XMLOutput (lXMLOutput,'Remit_1','3254 RELIABLE PARKWAY','Col').
        RUN XMLOutput (lXMLOutput,'Remit_2','CHICAGO, IL 60686','Col').
        RUN XMLOutput (lXMLOutput,'Customer',ar-inv.cust-name,'Col').
        RUN XMLOutput (lXMLOutput,'Bill_1',ar-inv.addr[1],'Col').
        RUN XMLOutput (lXMLOutput,'Bill_2',ar-inv.addr[2],'Col').
        RUN XMLOutput (lXMLOutput,'Bill_3',v-addr3,'Col').
        RUN XMLOutput (lXMLOutput,'EMail',v-email,'Col').
        RUN XMLOutput (lXMLOutput,'ShipName',v-shipto-name,'Col').
        RUN XMLOutput (lXMLOutput,'Ship_1',v-shipto-addr[1],'Col').
        RUN XMLOutput (lXMLOutput,'Ship_2',v-shipto-addr[2],'Col').
        RUN XMLOutput (lXMLOutput,'Ship_3',v-sold-addr3,'Col').
        RUN XMLOutput (lXMLOutput,'InvoiceNo',ar-inv.inv-no,'Col').
        RUN XMLOutput (lXMLOutput,'InvoiceDate',v-inv-date,'Col').
        RUN XMLOutput (lXMLOutput,'ShipDate',v-date-ship,'Col').
        RUN XMLOutput (lXMLOutput,'FOB',v-fob,'Col').
        RUN XMLOutput (lXMLOutput,'ShipVia',v-shipvia,'Col').
        RUN XMLOutput (lXMLOutput,'Terms',xar-inv.terms-d,'Col').
        RUN XMLOutput (lXMLOutput,'SalesRep',v-salesname,'Col').
        /*wfk        RUN XMLOutput (lXMLOutput,'BOL',xar-inv.bol-no,'Col'). */
        RUN XMLOutput (lXMLOutput,'/InvoiceHeader','','Row').
        XMLPage = NO.
        /* rstark 05181205 */

        /* rstark 05291402 */
        cXMLLineNumber = 0. 
        RUN cXMLOutput (clXMLOutput,'Request deploymentMode="' + cXMLProduction + '"','','Row').
        RUN cXMLOutput (clXMLOutput,'InvoiceDetailRequest','','Row').
        RUN cXMLOutput (clXMLOutput,'InvoiceDetailRequestHeader ' 
            + 'invoiceDate="' + cInvDate + '" '
            + 'invoiceID="' + STRING(ar-inv.inv-no) + '" '
            + 'operation="new" purpose="standard"','','Row').
        RUN cXMLOutput (clXMLOutput,'InvoiceDetailHeaderIndicator/','','Row').  
        RUN cXMLOutput (clXMLOutput,'InvoiceDetailLineIndicator isShippingInLine="yes" /','','Row').
        RUN cXMLOutput (clXMLOutput,'InvoicePartner','','Row').
        RUN cXMLOutput (clXMLOutput,'Contact role="billTo"','','Row').
        RUN cXMLOutput (clXMLOutput,'Name xml:lang="en-US"','','Row').
        RUN cXMLOutput (clXMLOutput,'',ar-inv.cust-name,'Col').
        RUN cXMLOutput (clXMLOutput,'/Name','','Row').
        RUN cXMLOutput (clXMLOutput,'PostalAddress','','Row').
        RUN cXMLOutput (clXMLOutput,'Street',ar-inv.addr[1],'Col').
        IF ar-inv.addr[2] NE '' THEN 
            RUN cXMLOutput (clXMLOutput,'Street',ar-inv.addr[2],'Col').
        RUN cXMLOutput (clXMLOutput,'City',ar-inv.city,'Col').
        RUN cXMLOutput (clXMLOutput,'State',ar-inv.state,'Col').
        RUN cXMLOutput (clXMLOutput,'PostalCode',ar-inv.zip,'Col').
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

        /* oe/rep/invpremx.i  */

        RUN XMLOutput (lXMLOutput,'',STRING(PAGE-NUMBER),'Page'). /* rstark 05181205 */
        RUN cXMLOutput (clXMLOutput,'',STRING(PAGE-NUMBER),'Page'). /* rstark 05291402 */

        PUT "<FTimes New Roman>".
        IF lChkImage THEN
            PUT  "<C3><#1><R+8><C+45>" "<IMAGE#1=" + ls-full-img1 FORM "x(200)" SKIP .
        /*         PUT "<C3><R2><#1>"                                         */
        /*             "<R+8><C+45><IMAGE#1=" ls-full-img1 SKIP. /* image */ .*/
        PUT "<=1>" SKIP. 
        
        IF company.company EQ '004'
            OR company.company EQ '006' THEN
            PUT "<C1><#2>" /*<R+10><C+35><IMAGE#2=" ls-full-img2 SKIP  /* company image */ */
                "<P10><=2><R+8>"
                "<FCourier New>"
                SPACE(4) "<P11><B>" cCompanyID FORMAT "x(30)" "</B><P10>" SKIP(1)
                SKIP(3)
                SPACE(12) "BILL TO:" SPACE(43) "SHIP TO:" SKIP
                SPACE(12) ar-inv.cust-name v-shipto-name AT 64 SKIP
                SPACE(12) ar-inv.addr[1]   v-shipto-addr[1] AT 64 SKIP
                SPACE(12) ar-inv.addr[2]  v-shipto-addr[2] AT 64 SKIP
                SPACE(12) v-addr3   v-sold-addr3 AT 64  SKIP
                SPACE(12) v-email    "</B>" SKIP    .
        ELSE IF company.company = '005' THEN
                PUT "<C1><#2>" /*<R+10><C+35><IMAGE#2=" ls-full-img2 SKIP  /* company image */ */
                    "<P10><=2><R+5>"
                    "<FCourier New>"
                    SPACE(4) "<P11><B>" cCompanyID FORMAT "x(30)" "</B><P10>" SKIP(1)
                    SPACE(12) "REMIT TO: MCI PACKAGING" SKIP
                    SPACE(12) "          PO BOX 39505" SKIP
                    SPACE(12) "          Louisville, KY 40233" SKIP (2)
                    SPACE(12) "BILL TO:" SPACE(43) "SHIP TO:" SKIP
                    SPACE(12) ar-inv.cust-name v-shipto-name AT 64 SKIP
                    SPACE(12) ar-inv.addr[1]   v-shipto-addr[1] AT 64 SKIP
                    SPACE(12) ar-inv.addr[2]  v-shipto-addr[2] AT 64 SKIP
                    SPACE(12) v-addr3   v-sold-addr3 AT 64  SKIP
                    SPACE(12) v-email    "</B>" SKIP    .
            ELSE           
                PUT "<C1><#2>" /*<R+10><C+35><IMAGE#2=" ls-full-img2 SKIP  /* company image */ */
                    "<P10><=2><R+5>"
                    "<FCourier New>"
                    SPACE(4) "<P11><B>" cCompanyID FORMAT "x(30)" "</B><P10>" SKIP(1)
                    SPACE(12) "REMIT TO: PREMIER PACKAGING" SKIP
                    SPACE(12) "          3254 RELIABLE PARKWAY" SKIP
                    SPACE(12) "          CHICAGO, IL  60686" SKIP (2)
                    SPACE(12) "BILL TO:" SPACE(43) "SHIP TO:" SKIP
                    SPACE(12) ar-inv.cust-name v-shipto-name AT 64 SKIP
                    SPACE(12) ar-inv.addr[1]   v-shipto-addr[1] AT 64 SKIP
                    SPACE(12) ar-inv.addr[2]  v-shipto-addr[2] AT 64 SKIP
                    SPACE(12) v-addr3   v-sold-addr3 AT 64  SKIP
                    SPACE(12) v-email    "</B>" SKIP    .
            
        v-printline = v-printline + 33.
        PUT "<|10><R7><C53><#3><FROM><R9><C78><RECT>" SKIP.
        PUT "<R8><C53><FROM><R8><C78><LINE>" SKIP
            "<R7><C65><FROM><R9><C65><LINE>" SKIP .
        /*"<R8><C65><FROM><R10><C65><LINE>" SKIP.*/
        
        PUT "<FArial><P12><=#3><R-2> <P10>" ip-copy-title FORM "x(20)" SKIP
            "<=#3>          INVOICE#                    " ar-inv.inv-no
            "<=#3><R+1>              DATE               " v-inv-date "<FCourier New>"    
            SKIP(1)
            .

        PUT "<|10><R21><C1><#4><FROM><R24><C80><RECT>" SKIP
            "<R22><C1><FROM><R22><C80><LINE>" SKIP    
            "<R21><C11><FROM><R24><C11><LINE>" SKIP
            "<R21><C22><FROM><R24><C22><LINE>" SKIP
            "<R21><C38><FROM><R24><C38><LINE>" SKIP
            "<R21><C52><FROM><R24><C52><LINE>" SKIP
            /* "<R21><C59><FROM><R24><C59><LINE>" */ SKIP 
            "<R21><C72><FROM><R24><C72><LINE>" SKIP
            .
        v-printline = v-printline + 3.

        FIND FIRST sman WHERE sman.company = ar-inv.company 
            AND sman.sman = v-salesman
            NO-LOCK NO-ERROR.
        v-salesname = IF AVAILABLE sman THEN sman.sname ELSE "".

        PUT "<FArial><=4>  SHIP DATE             FOB                      SHIP VIA                            TERMS                      SALESMAN NAME                   BOL#" SKIP
            "<FCourier New><=4><R+2><B> " v-date-ship FORM "99/99/9999" SPACE(2)
            v-fob FORM "x(12)" SPACE(1)
            v-shipvia FORM "x(20)" SPACE(1)
            xar-inv.terms-d FORM "x(15)" SPACE(1) 
            /*v-salesman FORM "x(8)"*/ 
            v-salesname FORM "x(23)"
            0 /* wfk xar-inv.bol-no */ "</B>"
            SKIP.


        PUT 
            /* "<|10><R28><C1><#5><FROM><R28><C80><LINE>" SKIP     
             "<|10><R31><C1><#6><FROM><R31><C80><LINE>" SKIP 
            */
            "<|10><R25><C1><#5><FROM><R27><C80><RECT>" SKIP    
            "<R25><C13><FROM><R27><C13><LINE>" SKIP
            "<R25><C25><FROM><R27><C25><LINE>"     
            "<R25><C52><FROM><R27><C52><LINE>" SKIP
            "<R25><C60><FROM><R27><C60><LINE>" SKIP
            "<R25><C65><FROM><R27><C65><LINE>" 
            "<R25><C71><FROM><R27><C71><LINE>" SKIP 
            .   
        /*puT "<FArial><=5><R+1>   Ordered     Shipped       B.O.      Item#                        Description                                              Price     UOM                 Amount" SKIP(1). */
        PUT "<FArial><=5>  CUST PO#              CUST PART#                                                                           <C62> P       PRICE                  " SKIP.
        IF ip-print-s THEN
            PUT "<FArial>  OUR ORDER#        ITEM#                                     DESCRIPTION                             INVOICED       C       (UOM)        AMOUNT" SKIP(1).
        ELSE PUT "<FArial>  OUR ORDER#        ITEM#                                     DESCRIPTION                             SHIPPED        C       (UOM)        AMOUNT" SKIP(1).

        v-printline = v-printline + 3.
           
        PUT "<FCourier New><B>".
        /* xprint form */

        ASSIGN
            v-subtot-lines = 0
            v-t-tax        = 0.
        lFirstLine = YES.
        FOR EACH ar-invl NO-LOCK WHERE ar-invl.x-no = ar-inv.x-no:

            IF NOT s-print-zero-qty AND
                ar-invl.ship-qty EQ 0 AND ar-invl.inv-qty EQ 0 THEN
                NEXT.

            ASSIGN 
                v-case-line = ""
                v-part-line = ""
                v-case-cnt  = "".

            v-pc = "P". /* partial*/ 
            FOR EACH oe-boll NO-LOCK WHERE oe-boll.company = ar-invl.company
                AND oe-boll.bol-no = ar-invl.bol-no
                /*and oe-boll.b-no = ar-invl.b-no*/
                AND oe-boll.i-no = ar-invl.i-no USE-INDEX bol-no:

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
            MESSAGE "test - printline if gt 62 then page"
                v-printline SKIP 
                "page line"  LINE-COUNTER SKIP
                "per page" PAGE-SIZE VIEW-AS ALERT-BOX.  
            IF v-printline > 62 THEN 
            DO:      
                MESSAGE "test - paging1"
                    VIEW-AS ALERT-BOX.      
                PAGE.
                /* oe/rep/invpremx.i  */

                RUN XMLOutput (lXMLOutput,'',STRING(PAGE-NUMBER),'Page'). /* rstark 05181205 */
                RUN cXMLOutput (clXMLOutput,'',STRING(PAGE-NUMBER),'Page'). /* rstark 05291402 */

                PUT "<FTimes New Roman>".
                IF lChkImage THEN
                    PUT  "<C3><#1><R+8><C+45>" "<IMAGE#1=" + ls-full-img1 FORM "x(200)" SKIP .
                /*         PUT "<C3><R2><#1>"                                         */
                /*             "<R+8><C+45><IMAGE#1=" ls-full-img1 SKIP. /* image */ .*/
                PUT "<=1>" SKIP. 
        
                IF company.company EQ '004'
                    OR company.company EQ '006' THEN
                    PUT "<C1><#2>" /*<R+10><C+35><IMAGE#2=" ls-full-img2 SKIP  /* company image */ */
                        "<P10><=2><R+8>"
                        "<FCourier New>"
                        SPACE(4) "<P11><B>" cCompanyID FORMAT "x(30)" "</B><P10>" SKIP(1)
                        SKIP(3)
                        SPACE(12) "BILL TO:" SPACE(43) "SHIP TO:" SKIP
                        SPACE(12) ar-inv.cust-name v-shipto-name AT 64 SKIP
                        SPACE(12) ar-inv.addr[1]   v-shipto-addr[1] AT 64 SKIP
                        SPACE(12) ar-inv.addr[2]  v-shipto-addr[2] AT 64 SKIP
                        SPACE(12) v-addr3   v-sold-addr3 AT 64  SKIP
                        SPACE(12) v-email    "</B>" SKIP    .
                ELSE IF company.company = '005' THEN
                        PUT "<C1><#2>" /*<R+10><C+35><IMAGE#2=" ls-full-img2 SKIP  /* company image */ */
                            "<P10><=2><R+5>"
                            "<FCourier New>"
                            SPACE(4) "<P11><B>" cCompanyID FORMAT "x(30)" "</B><P10>" SKIP(1)
                            SPACE(12) "REMIT TO: MCI PACKAGING" SKIP
                            SPACE(12) "          PO BOX 39505" SKIP
                            SPACE(12) "          Louisville, KY 40233" SKIP (2)
                            SPACE(12) "BILL TO:" SPACE(43) "SHIP TO:" SKIP
                            SPACE(12) ar-inv.cust-name v-shipto-name AT 64 SKIP
                            SPACE(12) ar-inv.addr[1]   v-shipto-addr[1] AT 64 SKIP
                            SPACE(12) ar-inv.addr[2]  v-shipto-addr[2] AT 64 SKIP
                            SPACE(12) v-addr3   v-sold-addr3 AT 64  SKIP
                            SPACE(12) v-email    "</B>" SKIP    .
                    ELSE           
                        PUT "<C1><#2>" /*<R+10><C+35><IMAGE#2=" ls-full-img2 SKIP  /* company image */ */
                            "<P10><=2><R+5>"
                            "<FCourier New>"
                            SPACE(4) "<P11><B>" cCompanyID FORMAT "x(30)" "</B><P10>" SKIP(1)
                            SPACE(12) "REMIT TO: PREMIER PACKAGING" SKIP
                            SPACE(12) "          3254 RELIABLE PARKWAY" SKIP
                            SPACE(12) "          CHICAGO, IL  60686" SKIP (2)
                            SPACE(12) "BILL TO:" SPACE(43) "SHIP TO:" SKIP
                            SPACE(12) ar-inv.cust-name v-shipto-name AT 64 SKIP
                            SPACE(12) ar-inv.addr[1]   v-shipto-addr[1] AT 64 SKIP
                            SPACE(12) ar-inv.addr[2]  v-shipto-addr[2] AT 64 SKIP
                            SPACE(12) v-addr3   v-sold-addr3 AT 64  SKIP
                            SPACE(12) v-email    "</B>" SKIP    .
            
                v-printline = v-printline + 33.
                PUT "<|10><R7><C53><#3><FROM><R9><C78><RECT>" SKIP.
                PUT "<R8><C53><FROM><R8><C78><LINE>" SKIP
                    "<R7><C65><FROM><R9><C65><LINE>" SKIP .
                /*"<R8><C65><FROM><R10><C65><LINE>" SKIP.*/
        
                PUT "<FArial><P12><=#3><R-2> <P10>" ip-copy-title FORM "x(20)" SKIP
                    "<=#3>          INVOICE#                    " ar-inv.inv-no
                    "<=#3><R+1>              DATE               " v-inv-date "<FCourier New>"    
                    SKIP(1)
                    .

                PUT "<|10><R21><C1><#4><FROM><R24><C80><RECT>" SKIP
                    "<R22><C1><FROM><R22><C80><LINE>" SKIP    
                    "<R21><C11><FROM><R24><C11><LINE>" SKIP
                    "<R21><C22><FROM><R24><C22><LINE>" SKIP
                    "<R21><C38><FROM><R24><C38><LINE>" SKIP
                    "<R21><C52><FROM><R24><C52><LINE>" SKIP
                    /* "<R21><C59><FROM><R24><C59><LINE>" */ SKIP 
                    "<R21><C72><FROM><R24><C72><LINE>" SKIP
                    .
                v-printline = v-printline + 3.

                FIND FIRST sman WHERE sman.company = ar-inv.company 
                    AND sman.sman = v-salesman
                    NO-LOCK NO-ERROR.
                v-salesname = IF AVAILABLE sman THEN sman.sname ELSE "".

                PUT "<FArial><=4>  SHIP DATE             FOB                      SHIP VIA                            TERMS                      SALESMAN NAME                   BOL#" SKIP
                    "<FCourier New><=4><R+2><B> " v-date-ship FORM "99/99/9999" SPACE(2)
                    v-fob FORM "x(12)" SPACE(1)
                    v-shipvia FORM "x(20)" SPACE(1)
                    xar-inv.terms-d FORM "x(15)" SPACE(1) 
                    /*v-salesman FORM "x(8)"*/ 
                    v-salesname FORM "x(23)"
                    0 /* wfk xar-inv.bol-no */ "</B>"
                    SKIP.


                PUT 
                    /* "<|10><R28><C1><#5><FROM><R28><C80><LINE>" SKIP     
                     "<|10><R31><C1><#6><FROM><R31><C80><LINE>" SKIP 
                    */
                    "<|10><R25><C1><#5><FROM><R27><C80><RECT>" SKIP    
                    "<R25><C13><FROM><R27><C13><LINE>" SKIP
                    "<R25><C25><FROM><R27><C25><LINE>"     
                    "<R25><C52><FROM><R27><C52><LINE>" SKIP
                    "<R25><C60><FROM><R27><C60><LINE>" SKIP
                    "<R25><C65><FROM><R27><C65><LINE>" 
                    "<R25><C71><FROM><R27><C71><LINE>" SKIP 
                    .   
                /*puT "<FArial><=5><R+1>   Ordered     Shipped       B.O.      Item#                        Description                                              Price     UOM                 Amount" SKIP(1). */
                PUT "<FArial><=5>  CUST PO#              CUST PART#                                                                           <C62> P       PRICE                  " SKIP.
                IF ip-print-s THEN
                    PUT "<FArial>  OUR ORDER#        ITEM#                                     DESCRIPTION                             INVOICED       C       (UOM)        AMOUNT" SKIP(1).
                ELSE PUT "<FArial>  OUR ORDER#        ITEM#                                     DESCRIPTION                             SHIPPED        C       (UOM)        AMOUNT" SKIP(1).

                v-printline = v-printline + 3.
           
                PUT "<FCourier New><B>".
 
                v-printline = 39. 
            END.

            ASSIGN 
                v-line      = v-line + 1
                /*v-printline = v-printline + 2 */.
            FIND FIRST oe-ordl WHERE oe-ordl.company = cocode AND
                oe-ordl.ord-no = ar-invl.ord-no AND
                oe-ordl.i-no = ar-invl.i-no
                NO-LOCK NO-ERROR.
            IF AVAILABLE oe-ordl THEN 
            DO:
            
                ASSIGN 
                    v-bo-qty = IF (ar-invl.qty - ar-invl.ship-qty -
                                    oe-ordl.t-ship-qty) < 0 then 0 else
                                   (ar-invl.qty - ar-invl.ship-qty -
                                    oe-ordl.t-ship-qty).
                IF oe-ordl.spare-char-2 NE "" THEN 
                DO:
                    ASSIGN 
                        dOrigQty = oe-ordl.spare-dec-1
                        cOrigUom = oe-ordl.spare-char-2
                        .
                    IF cOrigUom EQ 'CS' 
                        AND dOrigQty NE ar-invl.qty
                        AND oe-ordl.cas-cnt NE 0 THEN 
                        dOrigQty = ar-invl.inv-qty / oe-ordl.cas-cnt.
                    ELSE
                        dOrigQty = ar-invl.inv-qty.
                           
                END.
            END.
            ELSE
                ASSIGN v-bo-qty = IF ( ar-invl.qty - ar-invl.ship-qty ) < 0
                                  then 0 else ar-invl.qty - ar-invl.ship-qty.
            ASSIGN 
                v-inv-qty      = ar-invl.qty
                v-ship-qty     = ar-invl.ship-qty
                v-i-no         = ar-invl.i-no
                v-i-dscr       = ar-invl.i-name
                v-price        = 10 /* wfk ar-invl.price */ * (1 - (ar-invl.disc / 100))
                v-t-price      = 10 /* wfk ar-invl.t-price */
                v-subtot-lines = v-subtot-lines + 10 /* wfk ar-invl.t-price */.
            /* PremierS switch*/
            IF ip-print-s THEN v-inv-qtys = ar-invl.inv-qty / ar-invl.cas-cnt.

            IF ar-invl.tax AND AVAILABLE stax THEN
            DO i = 1 TO 3:
                IF stax.tax-code[i] NE "" THEN 
                DO:
                    CREATE w-tax.
                    ASSIGN
                        w-dsc      = stax.tax-dscr[i]
                        w-tax      = ROUND((IF stax.company EQ "yes" THEN v-t-price
                                                                  ELSE 10 /* wfk ar-invl.t-price */) *
                                        stax.tax-rate[i] / 100,2)
                        v-t-price  = v-t-price + w-tax
                        v-t-tax[i] = v-t-tax[i] + w-tax
                        v-lines    = v-lines + 1.
                END.
            END.

            IF  v-t-price NE 10 /*wfk  ar-invl.t-price*/ THEN 
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
            MESSAGE "test - line output"
                v-printline SKIP 
                "page line"  LINE-COUNTER SKIP
                "per page" PAGE-SIZE VIEW-AS ALERT-BOX. 

            PUT SPACE(1)
                v-po-no 
                ar-invl.part-no  SPACE(1)
                v-i-dscr FORM "x(30)". 
            /* PremierS switch*/
            IF ip-print-s THEN
                PUT v-inv-qtys  FORMAT "->>9.99" SPACE(3).
            ELSE PUT v-ship-qty  FORMAT "->>>>>9" SPACE(3).
            PUT v-price  FORMAT ">>>,>>9.9999"                
                10 /* wfk ar-invl.t-price */  FORMAT "->>>,>>9.99"                
                SKIP
                v-ord-no SPACE(10)
                ar-invl.i-no SPACE(1)
                ar-invl.part-dscr1  SPACE(11)
                v-pc  FORM "x" SPACE(7)
                v-price-head SKIP
                ar-invl.part-dscr2 AT 33 SKIP.

            v-printline = v-printline + 3.
             
            /* rstark 05181205 */

            XMLLineNumber = XMLLineNumber + 1.
            RUN XMLOutput (lXMLOutput,'InvoiceLine_' + STRING(XMLLineNumber),'','Row').
            RUN XMLOutput (lXMLOutput,'Column_1',v-po-no,'Col').
            RUN XMLOutput (lXMLOutput,'Column_2',ar-invl.part-no,'Col').
            RUN XMLOutput (lXMLOutput,'Column_3',v-i-dscr,'Col').
            /* PremierS switch*/
            IF ip-print-s THEN RUN XMLOutput (lXMLOutput,'Column_4',v-inv-qtys,'Col').
            ELSE RUN XMLOutput (lXMLOutput,'Column_4',v-ship-qty,'Col').
            RUN XMLOutput (lXMLOutput,'Column_5','','Col').
            RUN XMLOutput (lXMLOutput,'Column_6',v-price,'Col').
            /*            RUN XMLOutput (lXMLOutput,'Column_7',ar-invl.t-price,'Col'). */
            RUN XMLOutput (lXMLOutput,'/InvoiceLine_' + STRING(XMLLineNumber),'','Row').
             
            XMLLineNumber = XMLLineNumber + 1.
            RUN XMLOutput (lXMLOutput,'InvoiceLine_' + STRING(XMLLineNumber),'','Row').
            RUN XMLOutput (lXMLOutput,'Column_1',v-ord-no,'Col').
            RUN XMLOutput (lXMLOutput,'Column_2',ar-invl.i-no,'Col').
            RUN XMLOutput (lXMLOutput,'Column_3',ar-invl.part-dscr1,'Col').
            RUN XMLOutput (lXMLOutput,'Column_4','','Col').
            RUN XMLOutput (lXMLOutput,'Column_5',v-pc,'Col').
            RUN XMLOutput (lXMLOutput,'Column_6',v-price-head,'Col').
            RUN XMLOutput (lXMLOutput,'Column_7','','Col').
            RUN XMLOutput (lXMLOutput,'/InvoiceLine_' + STRING(XMLLineNumber),'','Row').

            XMLLineNumber = XMLLineNumber + 1.
            RUN XMLOutput (lXMLOutput,'InvoiceLine_' + STRING(XMLLineNumber),'','Row').
            RUN XMLOutput (lXMLOutput,'Column_1','','Col').
            RUN XMLOutput (lXMLOutput,'Column_2','','Col').
            RUN XMLOutput (lXMLOutput,'Column_3',ar-invl.part-dscr2,'Col').
            RUN XMLOutput (lXMLOutput,'Column_4','','Col').
            RUN XMLOutput (lXMLOutput,'Column_5','','Col').
            RUN XMLOutput (lXMLOutput,'Column_6','','Col').
            RUN XMLOutput (lXMLOutput,'Column_7','','Col').
            RUN XMLOutput (lXMLOutput,'/InvoiceLine_' + STRING(XMLLineNumber),'','Row').

            XMLLineNumber = XMLLineNumber + 1.
            /* rstark 05181205 */
             
            /* rstark 05291402 */

            RUN cXMLOutput (clXMLOutput,'InvoiceDetailItem invoiceLineNumber="' + STRING(ar-invl.LINE) 
                + '" quantity="' + STRING(IF dOrigQty NE 0 THEN dOrigQty ELSE ar-invl.inv-qty) + '"','','Row').
            RUN cXMLOutput (clXMLOutput,'UnitOfMeasure',ar-invl.pr-uom,'Col').
            RUN cXMLOutput (clXMLOutput,'UnitPrice','','Row'). 
            RUN cXMLOutput (clXMLOutput,'Money currency="USD"','','Row').
            RUN cXMLOutput (clXMLOutput,'',STRING(v-price),'Col').
            RUN cXMLOutput (clXMLOutput,'/Money','','Row').
            RUN cXMLOutput (clXMLOutput,'/UnitPrice','','Row'). 
            RUN cXMLOutput (clXMLOutput,'InvoiceDetailItemReference lineNumber="' + STRING(ar-invl.LINE) + '"','','Row').
            RUN cXMLOutput (clXMLOutput,'ItemID','','Row'). 
            RUN cXMLOutput (clXMLOutput,'SupplierPartID',ar-invl.i-no,'Col').
            RUN cXMLOutput (clXMLOutput,'/ItemID','','Row'). 
            RUN cXMLOutput (clXMLOutput,'Description xml:lang="en-US"','','Row').
            RUN cXMLOutput (clXMLOutput,'',ar-invl.i-name,'Col').
            RUN cXMLOutput (clXMLOutput,'/Description','','Row').
            RUN cXMLOutput (clXMLOutput,'/InvoiceDetailItemReference','','Row').
            RUN cXMLOutput (clXMLOutput,'SubtotalAmount','','Row'). 
            RUN cXMLOutput (clXMLOutput,'Money currency="USD"','','Row').
            /*            RUN cXMLOutput (clXMLOutput,'',STRING(ar-invl.t-price),'Col'). */
            RUN cXMLOutput (clXMLOutput,'/Money','','Row').
            RUN cXMLOutput (clXMLOutput,'/SubtotalAmount','','Row'). 
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
            IF lFirstLine THEN 
            DO: 
                /*                RUN cXMLOutput (clXMLOutput,'',STRING(ar-inv.t-inv-freight),'Col'). */
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
        END. /* each ar-invl */
        
        FOR EACH ar-invm NO-LOCK WHERE ar-invm.company = ar-inv.company AND
            ar-invm.x-no = ar-inv.x-no AND
            ar-invm.bill = "Y" BREAK BY ord-no WITH FRAME detailm:
            MESSAGE "test - in each ar-invm" v-printline
                VIEW-AS ALERT-BOX. 
            IF v-printline > 62 THEN 
            DO:
                MESSAGE "test paging 2"
                    VIEW-AS ALERT-BOX. 
                PAGE.                
                /* oe/rep/invpremx.i  */

                RUN XMLOutput (lXMLOutput,'',STRING(PAGE-NUMBER),'Page'). /* rstark 05181205 */
                RUN cXMLOutput (clXMLOutput,'',STRING(PAGE-NUMBER),'Page'). /* rstark 05291402 */

                PUT "<FTimes New Roman>".
                IF lChkImage THEN
                    PUT  "<C3><#1><R+8><C+45>" "<IMAGE#1=" + ls-full-img1 FORM "x(200)" SKIP .
                /*         PUT "<C3><R2><#1>"                                         */
                /*             "<R+8><C+45><IMAGE#1=" ls-full-img1 SKIP. /* image */ .*/
                PUT "<=1>" SKIP. 
        
                IF company.company EQ '004'
                    OR company.company EQ '006' THEN
                    PUT "<C1><#2>" /*<R+10><C+35><IMAGE#2=" ls-full-img2 SKIP  /* company image */ */
                        "<P10><=2><R+8>"
                        "<FCourier New>"
                        SPACE(4) "<P11><B>" cCompanyID FORMAT "x(30)" "</B><P10>" SKIP(1)
                        SKIP(3)
                        SPACE(12) "BILL TO:" SPACE(43) "SHIP TO:" SKIP
                        SPACE(12) ar-inv.cust-name v-shipto-name AT 64 SKIP
                        SPACE(12) ar-inv.addr[1]   v-shipto-addr[1] AT 64 SKIP
                        SPACE(12) ar-inv.addr[2]  v-shipto-addr[2] AT 64 SKIP
                        SPACE(12) v-addr3   v-sold-addr3 AT 64  SKIP
                        SPACE(12) v-email    "</B>" SKIP    .
                ELSE IF company.company = '005' THEN
                        PUT "<C1><#2>" /*<R+10><C+35><IMAGE#2=" ls-full-img2 SKIP  /* company image */ */
                            "<P10><=2><R+5>"
                            "<FCourier New>"
                            SPACE(4) "<P11><B>" cCompanyID FORMAT "x(30)" "</B><P10>" SKIP(1)
                            SPACE(12) "REMIT TO: MCI PACKAGING" SKIP
                            SPACE(12) "          PO BOX 39505" SKIP
                            SPACE(12) "          Louisville, KY 40233" SKIP (2)
                            SPACE(12) "BILL TO:" SPACE(43) "SHIP TO:" SKIP
                            SPACE(12) ar-inv.cust-name v-shipto-name AT 64 SKIP
                            SPACE(12) ar-inv.addr[1]   v-shipto-addr[1] AT 64 SKIP
                            SPACE(12) ar-inv.addr[2]  v-shipto-addr[2] AT 64 SKIP
                            SPACE(12) v-addr3   v-sold-addr3 AT 64  SKIP
                            SPACE(12) v-email    "</B>" SKIP    .
                    ELSE           
                        PUT "<C1><#2>" /*<R+10><C+35><IMAGE#2=" ls-full-img2 SKIP  /* company image */ */
                            "<P10><=2><R+5>"
                            "<FCourier New>"
                            SPACE(4) "<P11><B>" cCompanyID FORMAT "x(30)" "</B><P10>" SKIP(1)
                            SPACE(12) "REMIT TO: PREMIER PACKAGING" SKIP
                            SPACE(12) "          3254 RELIABLE PARKWAY" SKIP
                            SPACE(12) "          CHICAGO, IL  60686" SKIP (2)
                            SPACE(12) "BILL TO:" SPACE(43) "SHIP TO:" SKIP
                            SPACE(12) ar-inv.cust-name v-shipto-name AT 64 SKIP
                            SPACE(12) ar-inv.addr[1]   v-shipto-addr[1] AT 64 SKIP
                            SPACE(12) ar-inv.addr[2]  v-shipto-addr[2] AT 64 SKIP
                            SPACE(12) v-addr3   v-sold-addr3 AT 64  SKIP
                            SPACE(12) v-email    "</B>" SKIP    .
            
                v-printline = v-printline + 15.
                PUT "<|10><R7><C53><#3><FROM><R9><C78><RECT>" SKIP.
                PUT "<R8><C53><FROM><R8><C78><LINE>" SKIP
                    "<R7><C65><FROM><R9><C65><LINE>" SKIP .
                /*"<R8><C65><FROM><R10><C65><LINE>" SKIP.*/
        
                PUT "<FArial><P12><=#3><R-2> <P10>" ip-copy-title FORM "x(20)" SKIP
                    "<=#3>          INVOICE#                    " ar-inv.inv-no
                    "<=#3><R+1>              DATE               " v-inv-date "<FCourier New>"    
                    SKIP(1)
                    .

                PUT "<|10><R21><C1><#4><FROM><R24><C80><RECT>" SKIP
                    "<R22><C1><FROM><R22><C80><LINE>" SKIP    
                    "<R21><C11><FROM><R24><C11><LINE>" SKIP
                    "<R21><C22><FROM><R24><C22><LINE>" SKIP
                    "<R21><C38><FROM><R24><C38><LINE>" SKIP
                    "<R21><C52><FROM><R24><C52><LINE>" SKIP
                    /* "<R21><C59><FROM><R24><C59><LINE>" */ SKIP 
                    "<R21><C72><FROM><R24><C72><LINE>" SKIP
                    .
                v-printline = v-printline + 3.

                FIND FIRST sman WHERE sman.company = ar-inv.company 
                    AND sman.sman = v-salesman
                    NO-LOCK NO-ERROR.
                v-salesname = IF AVAILABLE sman THEN sman.sname ELSE "".

                PUT "<FArial><=4>  SHIP DATE             FOB                      SHIP VIA                            TERMS                      SALESMAN NAME                   BOL#" SKIP
                    "<FCourier New><=4><R+2><B> " v-date-ship FORM "99/99/9999" SPACE(2)
                    v-fob FORM "x(12)" SPACE(1)
                    v-shipvia FORM "x(20)" SPACE(1)
                    xar-inv.terms-d FORM "x(15)" SPACE(1) 
                    /*v-salesman FORM "x(8)"*/ 
                    v-salesname FORM "x(23)"
                    0 /* wfk xar-inv.bol-no */ "</B>"
                    SKIP.


                PUT 
                    /* "<|10><R28><C1><#5><FROM><R28><C80><LINE>" SKIP     
                     "<|10><R31><C1><#6><FROM><R31><C80><LINE>" SKIP 
                    */
                    "<|10><R25><C1><#5><FROM><R27><C80><RECT>" SKIP    
                    "<R25><C13><FROM><R27><C13><LINE>" SKIP
                    "<R25><C25><FROM><R27><C25><LINE>"     
                    "<R25><C52><FROM><R27><C52><LINE>" SKIP
                    "<R25><C60><FROM><R27><C60><LINE>" SKIP
                    "<R25><C65><FROM><R27><C65><LINE>" 
                    "<R25><C71><FROM><R27><C71><LINE>" SKIP 
                    .   
                /*puT "<FArial><=5><R+1>   Ordered     Shipped       B.O.      Item#                        Description                                              Price     UOM                 Amount" SKIP(1). */
                PUT "<FArial><=5>  CUST PO#              CUST PART#                                                                           <C62> P       PRICE                  " SKIP.
                IF ip-print-s THEN
                    PUT "<FArial>  OUR ORDER#        ITEM#                                     DESCRIPTION                             INVOICED       C       (UOM)        AMOUNT" SKIP(1).
                ELSE PUT "<FArial>  OUR ORDER#        ITEM#                                     DESCRIPTION                             SHIPPED        C       (UOM)        AMOUNT" SKIP(1).

                v-printline = v-printline + 3.
           
                PUT "<FCourier New><B>".
 
                v-printline = 21.
            END.
            IF FIRST(ar-invm.ord-no) THEN
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

            PUT ar-invm.charge AT 17 ar-invm.dscr ar-invm.amt AT 85 SKIP.

            /* rstark 05181205 */
            XMLLineNumber = XMLLineNumber + 1.
            RUN XMLOutput (lXMLOutput,'InvoiceLine_' + STRING(XMLLineNumber),'','Row').
            RUN XMLOutput (lXMLOutput,'Column_1','','Col').
            RUN XMLOutput (lXMLOutput,'Column_2',ar-invm.charge,'Col').
            RUN XMLOutput (lXMLOutput,'Column_3',ar-invm.dscr,'Col').
            RUN XMLOutput (lXMLOutput,'Column_4','','Col').
            RUN XMLOutput (lXMLOutput,'Column_5','','Col').
            RUN XMLOutput (lXMLOutput,'Column_6','','Col').
            RUN XMLOutput (lXMLOutput,'Column_7',ar-invm.amt,'Col').
            RUN XMLOutput (lXMLOutput,'/InvoiceLine_' + STRING(XMLLineNumber),'','Row').
            /* rstark 05181205 */

            ASSIGN
                v-subtot-lines = v-subtot-lines + ar-invm.amt
                v-printline    = v-printline + 1.
            IF ar-invm.tax AND AVAILABLE stax THEN
            DO i = 1 TO 3:
                IF stax.tax-code[i] NE "" THEN 
                DO:
                    CREATE w-tax.
                    ASSIGN
                        w-dsc      = stax.tax-dscr[i]
                        w-tax      = IF stax.company EQ "yes" THEN v-t-price
                              ELSE ar-invm.amt
                        w-tax      = ROUND(w-tax * (1 + (stax.tax-rate[i] / 100)),2) - w-tax
                        v-t-price  = v-t-price + w-tax
                        v-t-tax[i] = v-t-tax[i] + w-tax
                        v-lines    = v-lines + 1.
                END.
            END.

            IF v-t-price NE ar-invm.amt THEN 
            DO:
                CREATE w-tax.
                ASSIGN
                    w-dsc   = "******ITEM TOTAL:"
                    w-tax   = v-t-price
                    v-lines = v-lines + 1.
            END.

        END. /* each ar-invm */

        IF v-prntinst THEN 
        DO:
            DO i = 1 TO 4:
                IF ar-inv.bill-i[i] NE "" THEN 
                DO:
                                MESSAGE "test - print instruction"
                        v-printline SKIP 
                        "page line"  LINE-COUNTER SKIP
                        "per page" PAGE-SIZE VIEW-AS ALERT-BOX.  
                    
                    PUT ar-inv.bill-i[i] AT 10 SKIP.
                    ASSIGN 
                        v-printline = v-printline + 1.

                    /* rstark 05181205 */
                    XMLLineNumber = XMLLineNumber + 1.
                    RUN XMLOutput (lXMLOutput,'InvoiceLine_' + STRING(XMLLineNumber),'','Row').
                    RUN XMLOutput (lXMLOutput,'Column_1','','Col').
                    RUN XMLOutput (lXMLOutput,'Column_2',ar-inv.bill-i[i],'Col').
                    RUN XMLOutput (lXMLOutput,'Column_3','','Col').
                    RUN XMLOutput (lXMLOutput,'Column_4','','Col').
                    RUN XMLOutput (lXMLOutput,'Column_5','','Col').
                    RUN XMLOutput (lXMLOutput,'Column_6','','Col').
                    RUN XMLOutput (lXMLOutput,'Column_7','','Col').
                    RUN XMLOutput (lXMLOutput,'/InvoiceLine_' + STRING(XMLLineNumber),'','Row').
                /* rstark 05181205 */

                END.
            END. /* 1 to 4 */
        END.

        v-frt-tax = 100 /*wfk  ar-inv.t-inv-freight */.
        IF "" /* wfk ar-inv.tax-gr */ <> ""  AND FALSE /* ar-inv.t-inv-freight <> 0 */ AND AVAILABLE stax THEN
        DO i = 1 TO 3:

            IF stax.tax-code[i] NE "" THEN 
            DO:
                CREATE w-tax.
                ASSIGN
                    w-dsc      = stax.tax-dscr[i]
                    w-tax      = ROUND((IF stax.company EQ "yes" THEN v-frt-tax
                                                         ELSE 100) *
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
    v-inv-freight = 0.
    FOR EACH b-ar-inv WHERE b-ar-inv.company = ar-inv.company
        AND b-ar-inv.inv-no  = ar-inv.inv-no
        /* AND b-ar-inv.multi-invoice = no*/
        AND b-ar-inv.f-bill  = TRUE
        NO-LOCK.
    /*        v-inv-freight = v-inv-freight + b-ar-inv.t-inv-freight. */
    END.
    MESSAGE "test - before totals box"
        v-printline SKIP 
        "page line"  LINE-COUNTER SKIP
        "per page" PAGE-SIZE VIEW-AS ALERT-BOX.  
   
    PUT "<R58><C58><#8><FROM><R+5><C+22><RECT> " 
        "<=8> Sub Total    :" v-subtot-lines FORM "->>,>>9.99"
        "<=8><R+1> Freight      :" 100 /* v-inv-freight */
        "<=8><R+2> " cTaxCode FORMAT "x(9)" "    :" 10 /* ar-inv.t-inv-tax */ FORM "->>,>>9.99"
        "<=8><R+3>" "" 
        "<=8><R+4> Total Invoice:" 100 /* ar-inv.t-inv-rev */ FORM "->>,>>9.99" .

    PUT "<FArial><R58><C1><P12><B> THANK YOU. </B> <P9> " SKIP.
    PUT "<FArial><R60><C1><P12> All currencies displayed in " cCurCode FORMAT "x(3)" ". <P9> " SKIP.
    v-printline = v-printline + 8.
   
    /* rstark 05181205 */
    RUN XMLOutput (lXMLOutput,'Last',STRING(PAGE-NUMBER),'Page').
    RUN XMLOutput (lXMLOutput,'InvoiceFooter','','Row').
    RUN XMLOutput (lXMLOutput,'SubTotal',v-subtot-lines,'Col').
    RUN XMLOutput (lXMLOutput,'Freight',v-inv-freight,'Col').
    /*    RUN XMLOutput (lXMLOutput,'SalesTax',ar-inv.t-inv-tax,'Col'). */
    /*    RUN XMLOutput (lXMLOutput,'TotalInvoice',ar-inv.t-inv-rev,'Col'). */
    RUN XMLOutput (lXMLOutput,'/InvoiceFooter','','Row').
    /* rstark 05181205 */

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
    /*    RUN cXMLOutput (clXMLOutput,'',STRING(ar-inv.t-inv-tax),'Col'). */
    RUN cXMLOutput (clXMLOutput,'/Money','','Row').
    RUN cXMLOutput (clXMLOutput,'Description xml:lang="en-US"','','Row').
    RUN cXMLOutput (clXMLOutput,'','Sales Tax','Col').
    RUN cXMLOutput (clXMLOutput,'/Description','','Row').
    RUN cXMLOutput (clXMLOutput,'TaxDetail category="SalesTax" '+
        'percentageRate="0"','','Row').
    RUN cXMLOutput (clXMLOutput,'TaxableAmount','','Row').    
    RUN cXMLOutput (clXMLOutput,'Money currency="USD"','','Row').
    RUN cXMLOutput (clXMLOutput,'',STRING(v-subtot-lines),'Col').
    RUN cXMLOutput (clXMLOutput,'/Money','','Row').
    RUN cXMLOutput (clXMLOutput,'/TaxableAmount','','Row').    
    RUN cXMLOutput (clXMLOutput,'TaxAmount','','Row').    
    RUN cXMLOutput (clXMLOutput,'Money currency="USD"','','Row').
    /*    RUN cXMLOutput (clXMLOutput,'',STRING(ar-inv.t-inv-tax),'Col'). */
    RUN cXMLOutput (clXMLOutput,'/Money','','Row').
    RUN cXMLOutput (clXMLOutput,'/TaxAmount','','Row').    
    RUN cXMLOutput (clXMLOutput,'/TaxDetail','','Row').    
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
    /*    RUN cXMLOutput (clXMLOutput,'',STRING(ar-inv.t-inv-rev),'Col'). */
    RUN cXMLOutput (clXMLOutput,'/Money','','Row').
    RUN cXMLOutput (clXMLOutput,'/GrossAmount','','Row').
    RUN cXMLOutput (clXMLOutput,'NetAmount','','Row').
    RUN cXMLOutput (clXMLOutput,'Money currency="USD"','','Row').
    /*    RUN cXMLOutput (clXMLOutput,'',STRING(ar-inv.t-inv-rev),'Col'). */
    RUN cXMLOutput (clXMLOutput,'/Money','','Row').
    RUN cXMLOutput (clXMLOutput,'/NetAmount','','Row').
    RUN cXMLOutput (clXMLOutput,'/InvoiceDetailSummary','','Row').
    RUN cXMLOutput (clXMLOutput,'/InvoiceDetailRequest','','Row').
    RUN cXMLOutput (clXMLOutput,'/Request','','Row').
    /* rstark 05291402 */
    MESSAGE "test - if v-printline <= 66 then page " 
        v-printline
        SKIP 
        "page line"  LINE-COUNTER SKIP
        "per page" PAGE-SIZE 
        VIEW-AS ALERT-BOX. 
    IF v-printline <= 66 THEN 
    DO:
        MESSAGE "test paging 3"
            VIEW-AS ALERT-BOX. 
        PAGE.
    END.
    
    /* XMLOutput.i - RStark - Task: 05181205 (XML), 05291402 (cXML) */
    /* syntax: {XMLOutput/XMLOutput.i &NEW=NEW &XMLSysCtrl=XML??? &Company=cocode} -or-
               {XMLOutput/XMLOutput.i &NEW=NEW &cXMLSysCtrl=cXML??? &Company=cocode &c=c} -or-
               {XMLOutput.i &XMLOutput=XML???} -or-
               {XMLOutput.i &cXMLOutput=cXML??? &c=c} -or-
               {XMLOutput.i &XMLOutput=XML??? &Company=cocode} -or-
               {XMLOutput.i &cXMLOutput=cXML??? &Company=cocode &c=c} -or-
               {XMLOutput.i &XMLClose} -or-
               {XMLOutput.i &c=c &XMLClose} */


    IF clXMLOutput THEN 
    DO:
        RUN cXMLOutput (clXMLOutput,'','','Close').
        OUTPUT STREAM cXMLOutput CLOSE.
        OS-RENAME VALUE(cXMLTemp) VALUE(ccXMLOutput + cXMLFile).
    
        OUTPUT STREAM cXMLOutput TO 'XMLOutput/cXMLInvoice.log' APPEND.
        PUT STREAM cXMLOutput UNFORMATTED
            'Transmitted ' TODAY ' @ ' STRING(TIME,'hh:mm:ss am')
            ' File: ' ccXMLOutput + cXMLFile SKIP.
        OUTPUT STREAM cXMLOutput CLOSE.
    
    END. /* if lxmloutput */

/* rstark 05291402 */
END. /* each xar-inv */

/* XMLOutput.i - RStark - Task: 05181205 (XML), 05291402 (cXML) */
/* syntax: {XMLOutput/XMLOutput.i &NEW=NEW &XMLSysCtrl=XML??? &Company=cocode} -or-
           {XMLOutput/XMLOutput.i &NEW=NEW &cXMLSysCtrl=cXML??? &Company=cocode &c=c} -or-
           {XMLOutput.i &XMLOutput=XML???} -or-
           {XMLOutput.i &cXMLOutput=cXML??? &c=c} -or-
           {XMLOutput.i &XMLOutput=XML??? &Company=cocode} -or-
           {XMLOutput.i &cXMLOutput=cXML??? &Company=cocode &c=c} -or-
           {XMLOutput.i &XMLClose} -or-
           {XMLOutput.i &c=c &XMLClose} */


IF lXMLOutput THEN 
DO:
    RUN XMLOutput (lXMLOutput,'','','Close').
    OUTPUT STREAM XMLOutput CLOSE.
    OS-RENAME VALUE(XMLTemp) VALUE(cXMLOutput + XMLFile).
    
END. /* if lxmloutput */

  /* rstark 05181205 */

/* END ---------------------------------- copr. 1996 Advanced Software, Inc. */
