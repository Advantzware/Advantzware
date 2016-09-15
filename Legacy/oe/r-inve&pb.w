/* To Do ***/
/* 1) UI to verify period for date given */
/* 2) PUt statements for misc */
/* 3) UI to verify there is something to post */
/* 4)  GL Summary  report */ 

DEFINE VARIABLE list-name    AS cha       NO-UNDO.
DEFINE VARIABLE init-dir     AS CHA       NO-UNDO.
DEFINE VARIABLE lv-comp-curr AS cha       NO-UNDO.
DEFINE VARIABLE oeprep-char  AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-prof       AS DECIMAL   NO-UNDO.
/* {methods/defines/hndldefs.i} */
/* {methods/prgsecur.i} */

{custom/gcompany.i}
{custom/gloc.i}
/*{custom/getcmpny.i}*/
/*{custom/getloc.i}*/

{sys/inc/VAR.i new shared}
  /* wfk correction */
    gcompany = '001'.
    gloc = "main".
ASSIGN
    cocode = gcompany
    locode = gloc.
    
FIND FIRST company WHERE company.company EQ cocode NO-LOCK NO-ERROR.
IF AVAILABLE company THEN lv-comp-curr = company.curr-code.
    
DEFINE NEW SHARED BUFFER xoe-relh    FOR oe-relh.
DEFINE NEW SHARED BUFFER yoe-relh    FOR oe-relh.
DEFINE NEW SHARED BUFFER xoe-rell    FOR oe-rell.
DEFINE NEW SHARED BUFFER inv-line    FOR inv-line.

DEFINE            BUFFER xinv-line   FOR inv-line.
DEFINE            BUFFER tmp-oe-boll FOR oe-boll.
DEFINE            BUFFER xoe-ord     FOR oe-ord.
DEFINE            BUFFER b-oe-ordl   FOR oe-ordl.

DEFINE NEW SHARED VARIABLE v-ar-acct         LIKE ar-ctrl.receivables.
DEFINE NEW SHARED VARIABLE v-ar-freight      LIKE ar-ctrl.freight.
DEFINE NEW SHARED VARIABLE v-ar-stax         LIKE ar-ctrl.stax.
DEFINE NEW SHARED VARIABLE v-ar-sales        LIKE ar-ctrl.sales.
DEFINE NEW SHARED VARIABLE v-ar-disc         LIKE ar-ctrl.discount.
DEFINE NEW SHARED VARIABLE v-return          AS LOG       INIT NO.
DEFINE NEW SHARED VARIABLE v-start2-compress AS CHARACTER.
DEFINE NEW SHARED VARIABLE v-end2-compress   AS CHARACTER.
DEFINE NEW SHARED VARIABLE v-post            AS LOG       INIT NO.
/* from gl-ctrl record */
DEFINE NEW SHARED VARIABLE v-trnum           AS INTEGER.
DEFINE NEW SHARED VARIABLE v-back            LIKE itemfg.q-back.
DEFINE NEW SHARED VARIABLE v-balance         AS DECIMAL   FORMAT ">>>,>>>,>>9.99cr".
DEFINE NEW SHARED VARIABLE v-reduce-ord-bal  LIKE cust.ord-bal NO-UNDO.
DEFINE NEW SHARED VARIABLE v-invline         AS RECID.
DEFINE NEW SHARED VARIABLE v-invhead         AS RECID.

DEFINE NEW SHARED VARIABLE v-detail          AS LOG       FORMAT "Detail/Summary" INIT NO NO-UNDO.
DEFINE NEW SHARED VARIABLE v-gldetail        AS LOG       FORMAT "Detail/Summary" INIT NO NO-UNDO.

DEFINE            VARIABLE v-fr-tax          AS LOG       INIT NO.
DEFINE            VARIABLE v-postable        AS LOG       INIT NO.

DEFINE            VARIABLE v-xno             LIKE ar-inv.x-no. /* Unique Internial # for header */
DEFINE            VARIABLE v-xline           AS INTEGER.     /* Unique Internail # for lines */

DEFINE            VARIABLE v-inv-qty         LIKE oe-ordl.inv-qty.
DEFINE            VARIABLE v-ord-no          LIKE inv-line.ord-no.
DEFINE            VARIABLE v-ord-date        AS DATE.
DEFINE            VARIABLE v-inv-disc        AS DECIMAL   FORMAT "->>,>>9.99".
DEFINE            VARIABLE v-inv-disc-w      AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE ld-temp-amt       AS DECIMAL.
DEFINE            VARIABLE v-tax-rate        AS DECIMAL   EXTENT 4.

DEFINE            VARIABLE v-uninv-ordl-amt  LIKE oe-ordl.t-price NO-UNDO INIT 0.
DEFINE            VARIABLE v-u-inv           LIKE oe-ctrl.u-inv INIT FALSE.
DEFINE            VARIABLE v-tmp-tax-rate    AS DECIMAL   FORMAT ">,>>9.99<<<".

DEFINE            VARIABLE v-line-tot        LIKE inv-line.t-price.
DEFINE            VARIABLE v-misc-tot        LIKE inv-misc.amt.
DEFINE            VARIABLE v-line-tot-w      AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE v-post-zero-cgs   AS LOG       NO-UNDO.
DEFINE            VARIABLE v-export          LIKE sys-ctrl.char-fld NO-UNDO.
DEFINE            VARIABLE v-rec-written     AS INTEGER   NO-UNDO.
DEFINE            VARIABLE t-rec-written     AS INTEGER   NO-UNDO.
DEFINE            VARIABLE v-cost            AS DECIMAL   EXTENT 4.
DEFINE            VARIABLE v-cas-cnt         LIKE itemfg.case-count.

DEFINE            VARIABLE v-close-qty       LIKE oe-ordl.qty.
DEFINE            VARIABLE v-dcr-val         LIKE oe-ordl.cost INIT 0.
DEFINE            VARIABLE v-uom-rate        AS INTEGER.
DEFINE            VARIABLE v-sum-rel-qty     AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE v-tax             AS DECIMAL.
DEFINE            VARIABLE v-invalid         AS LOG       NO-UNDO.
DEFINE            VARIABLE lv-list-name      LIKE list-name NO-UNDO.
DEFINE            VARIABLE v-ftp-done        AS LOG       NO-UNDO.
DEFINE            VARIABLE v-print-fmt       AS cha       NO-UNDO.
DEFINE            VARIABLE ll-warned         AS LOG       NO-UNDO.
DEFINE            VARIABLE v-ttl-tax         AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE v-ttl-rate        AS DECIMAL   NO-UNDO.

DEFINE TEMP-TABLE ttInvoicePostUpdateGL
    {aoaAppSrv/ttfields.i}
    FIELD invNo             AS INTEGER   LABEL "Invoice Num" FORMAT ">9"     
    FIELD invDate           AS DATE      LABEL "Invoice Dt" FORMAT "99/99/99"
    FIELD custNo            AS CHARACTER LABEL "Cust Num" FORMAT "x(8)"
    FIELD custName          AS CHARACTER LABEL "Cust Name" FORMAT "x(25)"
    FIELD orderNumber       AS INTEGER   LABEL "Order Number" FORMAT "9"
    FIELD invoiceQty        AS INTEGER   LABEL "Invoice Qty" FORMAT "9"
    FIELD totInvoicefreight AS DECIMAL   LABEL "Total Freight" FORMAT "9.9"
    FIELD totInvoiceTax     AS DECIMAL   LABEL "Total Tax" FORMAT "9.9"
    FIELD miscTot           AS DECIMAL   LABEL "Misc Total" FORMAT "9.9"
    FIELD lineTot           AS DECIMAL   LABEL "Line Total" FORMAT "9.9"
    FIELD iInvRev           AS INTEGER   LABEL "Invoice Rev" FORMAT "99"
    FIELD weightPerTon      AS DECIMAL   LABEL "Weight" FORMAT "9.9"
    FIELD pricePerTon       AS DECIMAL   LABEL "Price" FORMAT "9.9"
    FIELD iNo               AS CHARACTER LABEL "Item" FORMAT "x(16)"
    FIELD iName             AS CHARACTER LABEL "Description" FORMAT "x(25)"
    FIELD qty               AS INTEGER   LABEL "Order" FORMAT "->>,>>>,>>9"
    FIELD invQty            AS INTEGER   LABEL "Qty Invoiced" FORMAT "->>,>>>,>>9"
    FIELD shipQty           AS INTEGER   LABEL "Qty Shipped" FORMAT "->>,>>>,>>9"
    FIELD cost              AS DECIMAL   LABEL "Cost" FORMAT "->>>,>>9.99<<<<"
    FIELD price             AS DECIMAL   LABEL "Price" FORMAT "->>>,>>9.99<<<<" DECIMALS 6
    FIELD uom               AS CHARACTER LABEL "UOM" FORMAT "x(3)"
    FIELD TotPrice          AS DECIMAL   LABEL "$ Per Ton" FORMAT "9.9"
    FIELD profit            AS DECIMAL   LABEL "Profit" FORMAT "->>>9.99%"
    .
/*  ttInvoicePostUpdateGL.invNo             = inv-head.inv-no         */
/*  ttInvoicePostUpdateGL.invDate             = inv-head.inv-date     */
/*  ttInvoicePostUpdateGL.custNo              = inv-head.cust-no      */
/*  ttInvoicePostUpdateGL.custName            = inv-head.cust-name    */
/*  ttInvoicePostUpdateGL.orderNumber         = inv-head.orderNumber  */
/*  ttInvoicePostUpdateGL.invoiceQty          = inv-head.invoiceQty   */
/*  ttInvoicePostUpdateGL.totInvoicefreight   = inv-head.t-inv-freight*/
/*  ttInvoicePostUpdateGL.totInvoiceTax       = inv-head.t-inv-tax    */
/*  ttInvoicePostUpdateGL.miscTot            = inv-head.miscTot       */
/*  ttInvoicePostUpdateGL.lineTot            = inv-head.lineTot       */
/*  ttInvoicePostUpdateGL.iInvRev          = inv-head.t-inv-rev     */
/*  ttInvoicePostUpdateGL.weightPerTon        = inv-head.weightPerTon */
/*  ttInvoicePostUpdateGL.pricePerTon         = inv-head.pricePerTon  */
/*  ttInvoicePostUpdateGL.iNo                = w-inv-line.i-no        */
/*  ttInvoicePostUpdateGL.iName             = w-inv-line.i-name       */
/*  ttInvoicePostUpdateGL.qty               = w-inv-line.qty          */
/*  ttInvoicePostUpdateGL.invQty            = w-inv-line.inv-qty      */
/*  ttInvoicePostUpdateGL.shipQty           = w-inv-line.ship-qty     */
/*  ttInvoicePostUpdateGL.price              = w-inv-line.price       */
/*  ttInvoicePostUpdateGL.uom                = w-inv-line.uom         */
/*  ttInvoicePostUpdateGL.TotPrice             = w-inv-line.t-price   */
/*  ttInvoicePostUpdateGL.profit             = w-inv-line.profit      */
DEFINE TEMP-TABLE w-report NO-UNDO LIKE report.

DEFINE TEMP-TABLE tt-gl NO-UNDO 
    FIELD row-id AS ROWID.
    
DEFINE TEMP-TABLE tt-custbal NO-UNDO
    FIELD cust-no AS CHARACTER
    FIELD ord-bal AS DECIMAL
    INDEX i1 cust-no.

DEFINE BUFFER b-inv-head FOR inv-head.
DEFINE BUFFER save-line  FOR reftable.

{oe/invwork.i new}

{oe/closchk.i new}
cocode = '001'.
locode = "whse".

RUN oe/getacct.p.
/* wfk - correction */

FIND FIRST ar-ctrl WHERE ar-ctrl.company EQ cocode NO-LOCK.

IF v-return THEN RETURN.

FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.name    EQ "INVPOST"
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE sys-ctrl THEN 
DO TRANSACTION:
    MESSAGE "Creating new System Control record (INVPOST).".
    CREATE sys-ctrl.
    ASSIGN
        sys-ctrl.company = cocode
        sys-ctrl.name    = "INVPOST"
        sys-ctrl.log-fld = NO
        sys-ctrl.descrip = "Post cost-of-goods sold when cost is zero?".
    MESSAGE sys-ctrl.descrip
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE sys-ctrl.log-fld.
END.
v-post-zero-cgs = sys-ctrl.log-fld.
FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.name    EQ "INVPRINT" NO-LOCK NO-ERROR.
v-print-fmt = IF AVAILABLE sys-ctrl THEN sys-ctrl.char-fld ELSE "".

FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.name    EQ "AREXP"
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE sys-ctrl THEN 
DO TRANSACTION:
    CREATE sys-ctrl.
    ASSIGN
        sys-ctrl.company  = cocode
        sys-ctrl.name     = "AREXP"
        sys-ctrl.descrip  = "A/R Export option"
        sys-ctrl.char-fld = "ASI".
    MESSAGE "System control record NOT found.  Please enter A/R Export Option".
    UPDATE sys-ctrl.char-fld.
END.
v-export = sys-ctrl.char-fld.

FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode NO-LOCK.
ASSIGN
    v-fr-tax = oe-ctrl.f-tax
    v-u-inv  = oe-ctrl.u-inv.

DEFINE VARIABLE is-xprint-form AS LOG       NO-UNDO.
DEFINE VARIABLE ls-fax-file    AS cha       NO-UNDO.
DEFINE VARIABLE lv-audit-dir   AS CHARACTER NO-UNDO.

DO TRANSACTION:
    {sys/inc/postdate.i}
    {sys/inc/oeprep.i}
    {sys/inc/oeclose.i}
    FIND FIRST sys-ctrl WHERE
        sys-ctrl.company EQ cocode AND
        sys-ctrl.name    EQ "AUDITDIR"
        NO-LOCK NO-ERROR.
   
    IF NOT AVAILABLE sys-ctrl THEN 
    DO:
        CREATE sys-ctrl.
        ASSIGN
            sys-ctrl.company  = cocode
            sys-ctrl.name     = "AUDITDIR"
            sys-ctrl.descrip  = "Audit Trails directory"
            sys-ctrl.char-fld = ".\AUDIT TRAILS".
    END.
  
    lv-audit-dir = sys-ctrl.char-fld.
  
    IF LOOKUP(SUBSTR(lv-audit-dir,LENGTH(lv-audit-dir),1),"/,\") > 0 THEN
        lv-audit-dir = SUBSTR(lv-audit-dir,1,LENGTH(lv-audit-dir) - 1).
  
    RELEASE sys-ctrl.
END.

&SCOPED-DEFINE use-factored

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



DEFINE VARIABLE tran-period             AS INTEGER   FORMAT ">>":U INITIAL 0 LABEL "Period" .
DEFINE VARIABLE inexport-log            AS LOG.
DEFINE VARIABLE inexport-desc           AS CHARACTER.
DEFINE VARIABLE inexport-cha            AS CHARACTER. 
DEFINE VARIABLE cStatus AS CHARACTER NO-UNDO.
DEFINE VARIABLE cReason AS CHARACTER NO-UNDO.




/* Start or4.p */
/* OS5.p */

/* ***************************  Instructions ************************** *
   1. create temp-table in definitions section
   2. place business logic to populate temp-table in pPostBOLCreateInvoice
   3. return this program via email as an attachment
 * ******************************************************************** */

/* ***************************  Definitions  ************************** */

/* Post BOL Create Invoice.rpa */

{aoaAppSrv/includes/pInvoicePostUpdateGL.i}
/* Post BOL Create Invoice.rpa */

{sys/ref/CustList.i NEW}
/* {aoa/aoaParam.w} */
  &ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : aoaAppSrv/aoaBin.p
    Purpose     : AppServer Functions and Procedures

    Syntax      : 

    Description : AppServer Functions and Procedures

    Author(s)   : Ron Stark
    Created     : 3.23.2016
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-fDateOptionDate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fDateOptionDate Procedure 
FUNCTION fDateOptionDate RETURNS DATE
  ( ipcDateOption AS CHARACTER,
    ipdtDate AS DATE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fGetParamValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetParamValue Procedure 
FUNCTION fGetParamValue RETURNS CHARACTER
  ( ipcField AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fParameters) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fParameters Procedure 
FUNCTION fParameters RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pGetColumns) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetColumns Procedure 
PROCEDURE pGetColumns :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE multiplier 150
    
    DEFINE INPUT PARAMETER iphTable            AS HANDLE    NO-UNDO.
    DEFINE INPUT PARAMETER ipcAvailableColumns AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcSelectedColumns  AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cRowType  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cField    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iWidth    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iRptWidth AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iLeft     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE idx       AS INTEGER   NO-UNDO.
    
    IF NOT VALID-HANDLE(iphTable) THEN RETURN.
        
    iphTable = iphTable:DEFAULT-BUFFER-HANDLE.
    DO idx = 1 TO NUM-ENTRIES(ipcSelectedColumns):
        ASSIGN
            cField    = ENTRY(idx,ipcSelectedColumns)
            iWidth    = MAX(iphTable:BUFFER-FIELD(cField):WIDTH,
                            LENGTH(iphTable:BUFFER-FIELD(cField):LABEL)
                            ) * {&multiplier}
            cRowType  = cRowType
                      + "|" + iphTable:BUFFER-FIELD(cField):NAME
                      + "," + STRING(iWidth)
                      + "," + STRING(iLeft)
            iLeft     = iLeft + iWidth + 50
            iRptWidth = iRptWidth + iWidth
            .
    END. /* each idx */
    iWidth = -1.
    DO idx = 1 TO NUM-ENTRIES(ipcAvailableColumns):
        ASSIGN
            cField   = ENTRY(idx,ipcAvailableColumns)
            cRowType = cRowType
                     + "|" + iphTable:BUFFER-FIELD(cField):NAME
                     + "," + STRING(iWidth)
            .
    END. /* each idx */
    
    cRowType = "ColumnMetaData," + STRING(iRptWidth) + cRowType.
    iphTable:BUFFER-CREATE.
    ASSIGN
        iphTable:BUFFER-FIELD("rowType"):BUFFER-VALUE()    = cRowType
        iphTable:BUFFER-FIELD("parameters"):BUFFER-VALUE() = fParameters()
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pGetParamValues) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetParamValues Procedure 
PROCEDURE pGetParamValues :
/*------------------------------------------------------------------------------
  Purpose:     get user-print record requested
  Parameters:  Company, Program ID, User ID, Batch Seq
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcName    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcUserID  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiBatch   AS INTEGER   NO-UNDO.

    DEFINE VARIABLE cBatch AS CHARACTER NO-UNDO.

    IF ipiBatch EQ 0 THEN
    FIND FIRST user-print NO-LOCK
         WHERE user-print.company    EQ ipcCompany
           AND user-print.program-id EQ ipcName
           AND user-print.user-id    EQ ipcUserID
           AND user-print.batch      EQ ""
         NO-ERROR.
    ELSE
    FIND FIRST user-print NO-LOCK
         WHERE user-print.company    EQ ipcCompany
           AND user-print.batch-seq  EQ ipiBatch
           AND user-print.program-id EQ ipcName
           AND user-print.batch      EQ "Batch"
         NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-fDateOptionDate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fDateOptionDate Procedure 
FUNCTION fDateOptionDate RETURNS DATE
  ( ipcDateOption AS CHARACTER,
    ipdtDate AS DATE ) :
/*------------------------------------------------------------------------------
  Purpose:  convert date option into date based on input date
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE dtDate AS DATE NO-UNDO.

    CASE ipcDateOption:
        WHEN "Fixed Date" THEN
            dtDate = ipdtDate.
        WHEN "Current Date" THEN
            dtDate = TODAY.
        WHEN "Current Date -1" THEN
            dtDate = TODAY - 1.
        WHEN "Current Date +1" THEN
            dtDate = TODAY + 1.
        WHEN "Current Date -2" THEN
            dtDate = TODAY - 2.
        WHEN "Current Date +2" THEN
            dtDate = TODAY + 2.
        WHEN "Current Date -3" THEN
            dtDate = TODAY - 3.
        WHEN "Current Date +3" THEN
            dtDate = TODAY + 3.
        WHEN "Current Date -4" THEN
            dtDate = TODAY - 4.
        WHEN "Current Date +4" THEN
            dtDate = TODAY + 4.
        WHEN "Current Date -5" THEN
            dtDate = TODAY - 5.
        WHEN "Current Date +5" THEN
            dtDate = TODAY + 5.
        WHEN "Current Date -6" THEN
            dtDate = TODAY - 6.
        WHEN "Current Date +6" THEN
            dtDate = TODAY + 6.
        WHEN "Start of this Month" THEN
            dtDate = DATE(MONTH(TODAY),1,YEAR(TODAY)).
        WHEN "End of this Month" THEN
            dtDate = DATE(MONTH(TODAY) + 1,1,YEAR(TODAY)) - 1.
        WHEN "First Day of last Month" THEN
            dtDate = DATE(MONTH(TODAY) - 1,1,YEAR(TODAY)).
        WHEN "Last Day of last Month" THEN
            dtDate = DATE(MONTH(TODAY),1,YEAR(TODAY)) - 1.
        WHEN "Start of this Year" THEN
            dtDate = DATE(1,1,YEAR(TODAY)).
        WHEN "End of this Year" THEN
            dtDate = DATE(12,31,YEAR(TODAY)).
        WHEN "First Day of Last Year" THEN
            dtDate = DATE(1,1,YEAR(TODAY) - 1).
        WHEN "Last Day of Last Year" THEN
            dtDate = DATE(12,31,YEAR(TODAY) - 1).
        WHEN "Last Sunday" THEN
            dtDate = TODAY - 7 * (IF WEEKDAY(TODAY) - 1 LE 0 THEN 1 ELSE 0) - WEEKDAY(TODAY) + 1.
        WHEN "Last Monday" THEN
            dtDate = TODAY - 7 * (IF WEEKDAY(TODAY) - 2 LE 0 THEN 1 ELSE 0) - WEEKDAY(TODAY) + 2.
        WHEN "Last Tuesday" THEN
            dtDate = TODAY - 7 * (IF WEEKDAY(TODAY) - 3 LE 0 THEN 1 ELSE 0) - WEEKDAY(TODAY) + 3.
        WHEN "Last Wednesday" THEN
            dtDate = TODAY - 7 * (IF WEEKDAY(TODAY) - 4 LE 0 THEN 1 ELSE 0) - WEEKDAY(TODAY) + 4.
        WHEN "Last Thursday" THEN
            dtDate = TODAY - 7 * (IF WEEKDAY(TODAY) - 5 LE 0 THEN 1 ELSE 0) - WEEKDAY(TODAY) + 5.
        WHEN "Last Friday" THEN
            dtDate = TODAY - 7 * (IF WEEKDAY(TODAY) - 6 LE 0 THEN 1 ELSE 0) - WEEKDAY(TODAY) + 6.
        WHEN "Last Saturday" THEN
            dtDate = TODAY - 7 * (IF WEEKDAY(TODAY) - 7 LE 0 THEN 1 ELSE 0) - WEEKDAY(TODAY) + 7.
        WHEN "Next Sunday" THEN
            dtDate = TODAY + 7 * (IF WEEKDAY(TODAY) - 1 GE 0 THEN 1 ELSE 0) - WEEKDAY(TODAY) + 1.
        WHEN "Next Monday" THEN
            dtDate = TODAY + 7 * (IF WEEKDAY(TODAY) - 2 GE 0 THEN 1 ELSE 0) - WEEKDAY(TODAY) + 2.
        WHEN "Next Tuesday" THEN
            dtDate = TODAY + 7 * (IF WEEKDAY(TODAY) - 3 GE 0 THEN 1 ELSE 0) - WEEKDAY(TODAY) + 3.
        WHEN "Next Wednesday" THEN
            dtDate = TODAY + 7 * (IF WEEKDAY(TODAY) - 4 GE 0 THEN 1 ELSE 0) - WEEKDAY(TODAY) + 4.
        WHEN "Next Thursday" THEN
            dtDate = TODAY + 7 * (IF WEEKDAY(TODAY) - 5 GE 0 THEN 1 ELSE 0) - WEEKDAY(TODAY) + 5.
        WHEN "Next Friday" THEN
            dtDate = TODAY + 7 * (IF WEEKDAY(TODAY) - 6 GE 0 THEN 1 ELSE 0) - WEEKDAY(TODAY) + 6.
        WHEN "Next Saturday" THEN
            dtDate = TODAY + 7 * (IF WEEKDAY(TODAY) - 7 GE 0 THEN 1 ELSE 0) - WEEKDAY(TODAY) + 7.
    END CASE.
        
    RETURN dtDate.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fGetParamValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetParamValue Procedure 
FUNCTION fGetParamValue RETURNS CHARACTER
  ( ipcField AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  get individual parameter field values
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cReturnValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx          AS INTEGER   NO-UNDO.

    IF AVAILABLE user-print THEN
    DO idx = 1 TO EXTENT(user-print.field-name):
        IF TRIM(user-print.field-name[idx]) EQ ipcField THEN DO:
            cReturnValue = user-print.field-value[idx].
            LEAVE.
        END. /* found screen object */
    END. /* do idx */

    RETURN cReturnValue.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fParameters) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fParameters Procedure 
FUNCTION fParameters RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cShow        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cParameter   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReturnValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx          AS INTEGER   NO-UNDO.

    cParameter = "Parameters".
    IF AVAILABLE user-print THEN
    DO idx = 1 TO EXTENT(user-print.field-name):
        IF user-print.field-name[idx] EQ "" THEN LEAVE.
        IF CAN-DO("svTitle,svAvailableColumns,svSelectedColumns",user-print.field-name[idx]) THEN NEXT.
        CASE user-print.field-name[idx]:
            WHEN "svShowParameters"   THEN
            IF user-print.field-value[idx] EQ "no" THEN
            cParameter = "NoParameters".
            WHEN "svShowReportHeader" THEN
            IF user-print.field-value[idx] EQ "no" THEN
            cShow = cShow + "0^".
            WHEN "svShowPageHeader"   THEN
            IF user-print.field-value[idx] EQ "no" THEN
            cShow = cShow + "2^".
            WHEN "svShowGroupHeader"  THEN
            IF user-print.field-value[idx] EQ "no" THEN
            cShow = cShow + "4^".
            WHEN "svShowGroupFooter"  THEN
            IF user-print.field-value[idx] EQ "no" THEN
            cShow = cShow + "5^".
            WHEN "svShowPageFooter"   THEN
            IF user-print.field-value[idx] EQ "no" THEN
            cShow = cShow + "3^".
            WHEN "svShowReportFooter" THEN
            IF user-print.field-value[idx] EQ "no" THEN
            cShow = cShow + "1^".
            OTHERWISE
            cReturnValue = cReturnValue
                         + "|" + TRIM(user-print.field-name[idx])
                         + "^" + user-print.field-value[idx]
                         .
        END CASE.
    END. /* do idx */
    ASSIGN
        cShow        = TRIM(cShow,"^") + "|"
        cReturnValue = cShow + cParameter + cReturnValue
        .
    RETURN cReturnValue.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



/* ***************************  Main Block  *************************** */
/* DEFINE VARIABLE hBin AS HANDLE NO-UNDO.     */
/* RUN aoaAppSrv/aoaBin.p PERSISTENT SET hBin. */
/* SESSION:ADD-SUPER-PROCEDURE (hBin).         */
/* RUN pPostBOLCreateInvoice ("001",0,"asi"). */


/* **********************  Internal Procedures  *********************** */

PROCEDURE pBuildCustList :
/*------------------------------------------------------------------------------
  Purpose:     Template.rpa
  Parameters:  Company, Use List?, Start Cust, End Cust, ID
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplList      AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipcStartCust AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEndCust   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcID        AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bCust FOR cust.
    
    DEFINE VARIABLE lActive AS LOGICAL NO-UNDO.
    
    EMPTY TEMP-TABLE ttCustList.

    IF iplList THEN
    RUN sys/ref/CustList.p (ipcCompany, ipcID, YES, OUTPUT lActive).
    ELSE DO:
        FOR EACH bCust NO-LOCK
            WHERE bCust.company EQ ipcCompany
              AND bCust.cust-no GE ipcStartCust
              AND bCust.cust-no LE ipcEndCust
            :
            CREATE ttCustList.
            ASSIGN 
                ttCustList.cust-no = bCust.cust-no
                ttCustList.log-fld = YES
                .
        END. /* each bcust */
    END. /* else */

END PROCEDURE.

/* {aoaAppSrv/includes/pInvoicePostUpdateGL.i} */
/* end or4.p */


/* For testing only!!!! */
        lPost = YES.
        dtPostDate = TODAY.
/*        cPostDateOption = DYNAMIC-FUNCTION("fGetParamValue","svPostDateOption")*/
/*        dtPostDate = DYNAMIC-FUNCTION("fDateOptionDate",cPostDateOption,dtPostDate)*/
/*        lCustList = DYNAMIC-FUNCTION("fGetParamValue","svCustList") EQ "yes"*/
/*        lAllCustNo = DYNAMIC-FUNCTION("fGetParamValue","svAllCustNo") EQ "yes"*/
/*        cStartCustNo = DYNAMIC-FUNCTION("fGetParamValue","svStartCustNo")*/
/*        cEndCustNo = DYNAMIC-FUNCTION("fGetParamValue","svEndCustNo")*/
/*        lAllInvNo = DYNAMIC-FUNCTION("fGetParamValue","svAllInvNo") EQ "yes"*/
/*        iStartInvNo = DYNAMIC-FUNCTION("fGetParamValue","svStartInvNo")*/
/*        iEndInvNo = DYNAMIC-FUNCTION("fGetParamValue","svEndInvNo")*/
        dtStartInvoiceDate = 1/1/2016.
/*        cStartInvoiceDateOption = DYNAMIC-FUNCTION("fGetParamValue","svStartInvoiceDateOption")*/
/*        dtStartInvoiceDate = DYNAMIC-FUNCTION("fDateOptionDate",cStartInvoiceDateOption,dtStartInvoiceDate)*/
        dtEndInvoiceDate = TODAY. 
/*        cEndInvoiceDateOption = DYNAMIC-FUNCTION("fGetParamValue","svEndInvoiceDateOption")*/
/*        dtEndInvoiceDate = DYNAMIC-FUNCTION("fDateOptionDate",cEndInvoiceDateOption,dtEndInvoiceDate)*/
/*        lInvoiceReportDetail = DYNAMIC-FUNCTION("fGetParamValue","svInvoiceReportDetail") EQ "yes"*/
/*        lGLReportDetail = DYNAMIC-FUNCTION("fGetParamValue","svGLReportDetail") EQ "yes"*/
        lPrintTon = YES.
/*        cAvailableColumns = DYNAMIC-FUNCTION("fGetParamValue","svAvailableColumns")*/
/*        cSelectedColumns = DYNAMIC-FUNCTION("fGetParamValue","svSelectedColumns")*/
gCompany = "001".
gLoc = "Main".

iStartInvNo = 7848.
iEndInvNo = 7848.

ASSIGN
    cocode = gcompany
    locode = gloc.
/* ***************************  Main Block  *************************** */    
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  lpost = YES.
   v-post = lPost.
   RUN pPrintPost.
END. /* Main block */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-tax-gr C-Win 
PROCEDURE calc-tax-gr :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipr-head-row AS ROWID.
    DEFINE INPUT PARAMETER ipi-inv-no LIKE inv-head.inv-no NO-UNDO.
    DEFINE BUFFER bf-currency FOR currency.
    DEFINE BUFFER bf-inv-head FOR inv-head.

    DEFINE VARIABLE k      AS INTEGER NO-UNDO.
    DEFINE VARIABLE dAccum AS DECIMAL NO-UNDO.

    FIND bf-inv-head WHERE ROWID(bf-inv-head) = ipr-head-row NO-LOCK NO-ERROR.

    IF NOT AVAILABLE bf-inv-head THEN
        RETURN.

    FIND FIRST bf-currency NO-LOCK
        WHERE bf-currency.company     EQ bf-inv-head.company
        AND bf-currency.c-code      EQ bf-inv-head.curr-code[1]
        AND bf-currency.ar-ast-acct NE ""
        AND bf-currency.ex-rate     GT 0
        NO-ERROR.

    ASSIGN 
        v-ttl-tax  = 0
        v-ttl-rate = 0.
    FIND FIRST stax
    {sys/ref/stax1W.i}
        AND {sys/ref/taxgroup.i stax} EQ bf-inv-head.tax-gr
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE stax THEN
        FIND FIRST stax
            WHERE stax.company = bf-inv-head.company AND
            stax.tax-group EQ bf-inv-head.tax-gr
            NO-LOCK NO-ERROR.
    dAccum = 1.
    IF AVAILABLE stax THEN 
    DO:
        DO i = 1 TO EXTENT(stax.tax-rate1):
            IF stax.tax-rate1[i] = 0 THEN NEXT.
            v-tax-rate[i] = stax.tax-rate1[i].
            IF stax.accum-tax THEN 
            DO: 
                /*        ##PN - must find effective rate since this is accumulated*/
                dAccum = dAccum  * (1 + v-tax-rate[i] / 100).
                v-tax-rate[i] = 100 * (dAccum - (v-ttl-rate / 100) - 1).
            END.
            IF stax.company EQ "yes" AND i GT 1 THEN
            DO k = 1 TO i - 1:
                v-tax-rate[i] = v-tax-rate[i] +
                    (v-tax-rate[i] * (stax.tax-rate1[k] / 100)).
            END.
            v-ttl-rate = v-ttl-rate + v-tax-rate[i].
        END.
      
        DO i = 1 TO EXTENT(stax.tax-rate1):
            IF stax.tax-rate1[i] = 0 THEN NEXT.
            ASSIGN 
                v-tax-rate[i] = ROUND(v-tax-rate[i] / v-ttl-rate *
                                     bf-inv-head.t-inv-tax,2)
                v-ttl-tax     = v-ttl-tax + v-tax-rate[i].
        END.
      
        IF bf-inv-head.t-inv-tax NE v-ttl-tax THEN
            v-tax-rate[1] = v-tax-rate[1] +
                (bf-inv-head.t-inv-tax - v-ttl-tax).
      
        DO i = 1 TO EXTENT(stax.tax-rate1):
            IF stax.tax-rate1[i] = 0 THEN NEXT.
            FIND FIRST account
                WHERE account.company EQ cocode
                AND account.actnum  EQ stax.tax-acc1[i]
                NO-LOCK NO-ERROR.
            
            IF AVAILABLE account AND v-tax-rate[i] NE 0 THEN 
            DO:
                CREATE tt-report.
                ASSIGN
                    tt-report.term-id = ""
                    tt-report.key-01  = "work-tax"
                    tt-report.key-02  = account.actnum
                    tt-report.key-03  = STRING(ipi-inv-no,"999999")
                    tt-report.key-04  = bf-inv-head.tax-gr
                    tt-report.key-05  = STRING(v-tax-rate[i] *
                                      (IF AVAILABLE bf-currency  THEN
                                         bf-currency.ex-rate ELSE 1))
                    tt-report.weight  = v-line-tot-w *
                               (v-tax-rate[i] / bf-inv-head.t-inv-tax).
            END. /* avail account */

        END. /* 1 to 3 */

    END. /* avail stax */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-tons C-Win 
PROCEDURE calc-tons :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ip-i-no LIKE itemfg.i-no NO-UNDO.
    DEFINE INPUT  PARAMETER ip-qty AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER op-weight AS DECIMAL NO-UNDO.

    DEFINE BUFFER b-itemfg FOR itemfg.


    FIND FIRST b-itemfg
        WHERE b-itemfg.company EQ cocode
        AND b-itemfg.i-no    EQ ip-i-no
        NO-LOCK NO-ERROR.
    IF AVAILABLE b-itemfg AND b-itemfg.weight-100 NE 0 THEN
        op-weight = b-itemfg.weight-100 * ip-qty / 100.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE close-order C-Win 
PROCEDURE close-order :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    IF CAN-FIND(FIRST oe-ordl WHERE 
        oe-ordl.company = oe-ord.company AND
        oe-ordl.ord-no = oe-ord.ord-no AND 
        oe-ordl.stat NE "C") THEN RETURN.

    RUN oe\close.p(RECID(oe-ord), YES).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE close-order-lines C-Win 
PROCEDURE close-order-lines :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    /* mdp adds logic to close lines from clslinchk.p */
   
    DEFINE BUFFER lb-oe-ordl FOR oe-ordl.

    FOR EACH oe-ordl NO-LOCK
        WHERE oe-ordl.company EQ oe-ord.company
        AND oe-ordl.ord-no  EQ oe-ord.ord-no:

        FIND itemfg OF oe-ordl NO-LOCK NO-ERROR.
    
        IF (oe-ordl.inv-qty  GE oe-ordl.qty * (1 - (oe-ordl.under-pct / 100)) OR
            oe-ordl.is-a-component)                                               AND
            (oe-ordl.ship-qty GE oe-ordl.qty * (1 - (oe-ordl.under-pct / 100)) OR
            CAN-FIND(FIRST b-oe-ordl {sys/inc/ordlcomp.i b-oe-ordl oe-ordl})  OR
            (AVAILABLE itemfg AND NOT itemfg.stocked)) THEN                               
        DO:
            FIND lb-oe-ordl WHERE ROWID(lb-oe-ordl) = ROWID(oe-ordl)
                EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
            IF AVAILABLE lb-oe-ordl THEN lb-oe-ordl.stat = "C".
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copy-report-to-audit-dir C-Win 
PROCEDURE copy-report-to-audit-dir :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE targetfile AS CHARACTER FORMAT "X(50)" NO-UNDO.
    DEFINE VARIABLE dirname1   AS CHARACTER FORMAT "X(20)" NO-UNDO.
    DEFINE VARIABLE dirname2   AS CHARACTER FORMAT "X(20)" NO-UNDO.
    DEFINE VARIABLE dirname3   AS CHARACTER FORMAT "X(20)" NO-UNDO.
  
    ASSIGN 
        targetfile = lv-audit-dir + "\OP\OB4\Run#"
                    + STRING(v-trnum) + ".txt"
        dirname1   = lv-audit-dir
        dirname2   = lv-audit-dir + "\OP"
        dirname3   = lv-audit-dir + "\OP\OB4".

    OS-COPY VALUE(list-name) VALUE (targetfile).

    IF SEARCH(targetfile) EQ ? THEN 
    DO:
        OS-CREATE-DIR VALUE(dirname1).
        OS-CREATE-DIR VALUE(dirname2).
        OS-CREATE-DIR VALUE(dirname3).
        OS-COPY VALUE(list-name) VALUE (targetfile).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-save-line C-Win 
PROCEDURE create-save-line :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    
    DISABLE TRIGGERS FOR LOAD OF inv-line.
    DISABLE TRIGGERS FOR LOAD OF inv-misc.

    FOR EACH inv-line WHERE inv-line.r-no EQ b-inv-head.r-no:
        CREATE save-line.
        ASSIGN
            save-line.reftable = "save-line" + STRING(v-trnum,"9999999999")
            save-line.val[1]   = inv-line.r-no
            save-line.val[2]   = inv-head.r-no
            save-line.val[3]   = INT(RECID(inv-line))
            inv-line.r-no      = inv-head.r-no.
    END.

    FOR EACH inv-misc WHERE inv-misc.r-no EQ b-inv-head.r-no:
        CREATE save-line.
        ASSIGN
            save-line.reftable = "save-line" + STRING(v-trnum,"9999999999")
            save-line.val[1]   = inv-misc.r-no
            save-line.val[2]   = inv-head.r-no
            save-line.val[3]   = INT(RECID(inv-misc))
            inv-misc.r-no      = inv-head.r-no.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-lot-no C-Win 
PROCEDURE get-lot-no :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       THIS IS TO ASSIGN THE LOT NUMBER FROM THE REFTABLE TO 
    ------------------------------------------------------------------------------*/
    FIND FIRST reftable WHERE
        reftable.reftable EQ "inv-line.lot-no" AND
        reftable.rec_key  EQ inv-line.rec_key
        USE-INDEX rec_key NO-LOCK NO-ERROR.

    IF AVAILABLE reftable THEN 
    DO:
        ASSIGN 
            ar-invl.lot-no = TRIM(reftable.CODE).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-tr-dscr C-Win 
PROCEDURE get-tr-dscr :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ip-inv-no LIKE ar-inv.inv-no NO-UNDO.
    DEFINE OUTPUT PARAMETER op-dscr LIKE gltrans.tr-dscr NO-UNDO.


    RELEASE ar-inv.
    RELEASE cust.

    FIND FIRST ar-inv
        WHERE ar-inv.company EQ cocode
        AND ar-inv.inv-no  EQ ip-inv-no
        NO-LOCK NO-ERROR.
    IF AVAILABLE ar-inv THEN
        FIND FIRST cust
            WHERE cust.company EQ ar-inv.company
            AND cust.cust-no EQ ar-inv.cust-no
            NO-LOCK NO-ERROR.
    op-dscr = TRIM(IF AVAILABLE cust THEN cust.name ELSE "Cust not on file") +
        " Inv# " + STRING(ip-inv-no,"99999999").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE list-gl C-Win 
PROCEDURE list-gl :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/


    DEFINE VARIABLE v-gl-sales    AS DECIMAL   FORMAT ">>,>>>,>>9.99cr" NO-UNDO.
    DEFINE VARIABLE v-dscr        LIKE account.dscr NO-UNDO.
    DEFINE VARIABLE v-disp-actnum LIKE account.actnum NO-UNDO.
    DEFINE VARIABLE v-disp-amt    AS DECIMAL   FORMAT ">>,>>>,>>9.99cr" NO-UNDO.
    DEFINE VARIABLE v-tmp-amt     AS DECIMAL   FORMAT ">>,>>>,>>9.99cr" NO-UNDO.
    DEFINE VARIABLE v-empty       AS DECIMAL   FORMAT ">>,>>>,>>9.99cr" NO-UNDO.
    DEFINE VARIABLE ld-t          AS DECIMAL   FORMAT "->>>>9.99" EXTENT 3 NO-UNDO.
    DEFINE VARIABLE ld-pton       AS DECIMAL   FORMAT "->>>9.999" NO-UNDO.
    DEFINE VARIABLE lv-label-ton  AS CHARACTER FORMAT "x(19)" EXTENT 2 NO-UNDO.
    DEFINE VARIABLE v-recid       AS RECID     INIT ?.
    DEFINE VARIABLE lv-rowid      AS ROWID     NO-UNDO.

    DEFINE BUFFER b-tt-report FOR tt-report.

    /*FORMAT HEADER                                                                     */
    /*       "G/L ACCOUNT NUMBER       "                                                */
    /*       "DESCRIPTION                                  "                            */
    /*       "DATE      "                                                               */
    /*       "         AMOUNT"                                                          */
    /*       lv-label-ton[1]                                                            */
    /*       SKIP                                                                       */
    /*       "-------------------------"                                                */
    /*       "---------------------------------------------"                            */
    /*       "----------"                                                               */
    /*       "---------------"                                                          */
    /*       lv-label-ton[2]                                                            */
    /*                                                                                  */
    /*    WITH NO-LABELS NO-BOX NO-UNDERLINE FRAME f-top-s PAGE-TOP WIDTH 150 STREAM-IO.*/

    /*FORMAT HEADER                                                                     */
    /*       "G/L ACCOUNT NUMBER       "                                                */
    /*       "DESCRIPTION                                  "                            */
    /*       "INVOICE#"                                                                 */
    /*       "ITEM#          "                                                          */
    /*       "         AMOUNT"                                                          */
    /*       "          TOTAL"                                                          */
    /*       lv-label-ton[1]                                                            */
    /*       SKIP                                                                       */
    /*       "-------------------------"                                                */
    /*       "---------------------------------------------"                            */
    /*       "--------"                                                                 */
    /*       "---------------"                                                          */
    /*       "---------------"                                                          */
    /*       "---------------"                                                          */
    /*       lv-label-ton[2]                                                            */
    /*                                                                                  */
    /*    WITH NO-LABELS NO-BOX NO-UNDERLINE FRAME f-top-d PAGE-TOP WIDTH 150 STREAM-IO.*/
    /*                                                                                  */
    /*FORM v-disp-actnum                                                                */
    /*     v-dscr                                                                       */
    /*           FORMAT "99/99/9999"                                           */
    /*     v-disp-amt                                                                   */
    /*     ld-pton                                                                      */
    /*     ld-t[2]                                                                      */
    /*     SKIP                                                                         */
    /*    WITH DOWN NO-BOX NO-LABELS STREAM-IO WIDTH 150 FRAME gl-sum.                  */
    /*                                                                                  */
    /*FORM account.actnum                                                               */
    /*     v-dscr                                                                       */
    /*     inv-head.inv-no   FORMAT ">>>>>>>>"                                          */
    /*     inv-line.i-no                                                                */
    /*     v-tmp-amt                                                                    */
    /*     v-empty                                                                      */
    /*     ld-pton                                                                      */
    /*     ld-t[1]                                                                      */
    /*     SKIP                                                                         */
    /*    WITH DOWN NO-BOX NO-LABELS STREAM-IO WIDTH 150 FRAME gl-det.                  */


    /*    FIND FIRST period                   */
    /*        WHERE period.company EQ gcompany*/
    /*        AND period.pst     LE  */
    /*        AND period.pend    GE  */
    /*        NO-LOCK NO-ERROR.               */
 
    /*  IF lPrintTon THEN                             */
    /*    ASSIGN                                   */
    /*     lv-label-ton[1] = "    $/TON      TONS" */
    /*     lv-label-ton[2] = "--------- ---------".*/

    post-print: 
    DO WHILE TRUE.
        SESSION:SET-WAIT-STATE ("general").

        list-name = TRIM(lv-list-name) + ".001".


        /** LIST G/L FOR LINE ITEMS **/
        {oe/r-inve&2b.i work-line "ITEMS"}

        /** LIST G/L FOR MISC. **/
        {oe/r-inve&2b.i work-misc "MISC."}

        /** LIST G/L FOR SALES TAX **/
        {oe/r-inve&2b.i work-tax "SALES TAX"}

        /** LIST G/L FOR CURRENCY GAIN/LOSS **/
        {oe/r-inve&2b.i work-curr "CURRENCY GAIN/LOSS"}
    
        /** LIST G/L FOR FG/COGS **/
        IF lGLReportDetail THEN 
        DO:
            ASSIGN
                v-disp-amt = 0
                ld-t[2]    = 0.

            FOR EACH tmp-work-job BREAK BY tmp-work-job.actnum
                BY tmp-work-job.inv-no:
                FIND FIRST account WHERE account.company = cocode AND
                    account.actnum  = tmp-work-job.actnum
                    NO-LOCK NO-ERROR.
                IF AVAILABLE account THEN
                    ASSIGN v-dscr = account.dscr.
                ELSE
                    ASSIGN v-dscr = "ACCOUNT NOT FOUND - " + tmp-work-job.actnum.

                ACCUMULATE tmp-work-job.amt (TOTAL BY tmp-work-job.actnum).
                ld-t[1] = tmp-work-job.weight / 2000.

                IF tmp-work-job.fg THEN
                    ASSIGN v-tmp-amt  = - tmp-work-job.amt
                        v-disp-amt = v-disp-amt - tmp-work-job.amt
                        ld-t[1]    = - ld-t[1].
                ELSE
                    ASSIGN v-tmp-amt  = tmp-work-job.amt
                        v-disp-amt = v-disp-amt + tmp-work-job.amt.

                ASSIGN
                    ld-t[2] = ld-t[2] + ld-t[1]
                    ld-pton = v-tmp-amt / ld-t[1].

                IF ld-pton EQ ? THEN ld-pton = 0.

                /*        DISPLAY tmp-work-job.actnum @ account.actnum*/
                /*            v-dscr                                  */
                /*            tmp-work-job.inv-no @ inv-head.inv-no   */
                /*            tmp-work-job.i-no   @ inv-line.i-no     */
                /*            v-tmp-amt                               */
                /*            ld-pton WHEN lPrintTon                     */
                /*            ld-t[1] WHEN lPrintTon                     */
                /*            WITH FRAME gl-det.                      */


                IF LAST-OF(tmp-work-job.actnum) THEN 
                DO:
                    PUT v-disp-amt TO 128.
                    IF lPrintTon THEN 
                    DO:
                        ld-pton = v-disp-amt / ld-t[2].
                        IF ld-pton EQ ? THEN ld-pton = 0.
                    /*            PUT ld-pton TO 138 ld-t[2] TO 148 SKIP(1).*/
                    END.

                    ASSIGN
                        v-disp-amt = 0
                        ld-t[2]    = 0.
                END.
            END.
        END.

        FOR EACH work-job BREAK BY work-job.actnum:
            FIND FIRST account WHERE account.company = cocode AND
                account.actnum  = work-job.actnum
                NO-LOCK NO-ERROR.
            IF AVAILABLE account THEN
                ASSIGN v-dscr = account.dscr.
            ELSE
                ASSIGN v-dscr = "ACCOUNT NOT FOUND - " + work-job.actnum.

            ASSIGN 
                v-disp-actnum = work-job.actnum
                ld-t[2]       = work-job.weight / 2000.

            IF work-job.fg THEN
                ASSIGN v-disp-amt = - work-job.amt
                    ld-t[2]    = - ld-t[2].
            ELSE
                ASSIGN v-disp-amt = work-job.amt.

            ld-pton = v-disp-amt / ld-t[2].

            IF ld-pton EQ ? THEN ld-pton = 0.

            IF NOT lGLReportDetail THEN 
            DO:
            /*        DISPLAY v-disp-actnum   */
            /*             v-dscr             */
            /*                       */
            /*             v-disp-amt         */
            /*             ld-pton WHEN lPrintTon*/
            /*             ld-t[2] WHEN lPrintTon*/
            /*           WITH FRAME gl-sum.   */
    
            END.

            ASSIGN
                v-balance = v-balance + v-disp-amt
                ld-t[3]   = ld-t[3] + ld-t[2].
        END. /* each work-job */
        /** POST FREIGHT TO G/L **/
        FIND FIRST account
            WHERE account.company EQ cocode
            AND account.actnum  EQ v-ar-freight
            NO-LOCK NO-ERROR.
        ASSIGN
            v-dscr     = IF AVAILABLE account THEN account.dscr
                   ELSE "ACCOUNT NOT FOUND - FREIGHT"
            v-disp-amt = 0
            ld-t[2]    = 0.

        IF lGLReportDetail THEN 
        DO:
            FOR EACH tt-report
                WHERE tt-report.term-id EQ ""
                AND tt-report.key-01  EQ "work-freight"
                NO-LOCK
                BREAK BY tt-report.key-02:

                ASSIGN
                    ld-t[1]    = tt-report.weight / 2000
                    v-disp-amt = v-disp-amt + dec(tt-report.key-05)
                    ld-t[2]    = ld-t[2] + ld-t[1].

                IF dec(tt-report.key-05) NE 0 THEN 
                DO:
                    ld-pton = dec(tt-report.key-05) / ld-t[1].

                    IF ld-pton EQ ? THEN ld-pton = 0.

                /*          DISPLAY v-ar-freight          @ account.actnum */
                /*                  v-dscr                                 */
                /*                  int(tt-report.key-02) @ inv-head.inv-no*/
                /*                  "FREIGHT"             @ inv-line.i-no  */
                /*                  dec(tt-report.key-05) @ v-tmp-amt      */
                /*                  ld-pton WHEN lPrintTon                    */
                /*                  ld-t[1] WHEN lPrintTon                    */
                /*              WITH FRAME gl-det.                         */

                END.
            END.

            IF v-disp-amt NE 0 THEN 
            DO:
                PUT v-disp-amt TO 128.
                IF lPrintTon THEN 
                DO:
                    ld-pton = v-disp-amt / ld-t[2].
                    IF ld-pton EQ ? THEN ld-pton = 0.
                    PUT ld-pton TO 138 ld-t[2] TO 148 SKIP(1).
                END.
                ELSE PUT SKIP.
                ASSIGN
                    v-disp-amt = 0
                    ld-t[2]    = 0.
            END.
        END.

        ASSIGN
            v-disp-actnum = v-ar-freight
            v-disp-amt    = v-post-freight
            ld-t[2]       = v-post-freight-w / 2000.

        IF NOT lGLReportDetail THEN 
        DO:
            ld-pton = v-disp-amt / ld-t[2].

            IF ld-pton EQ ? THEN ld-pton = 0.

        /*      DISPLAY v-disp-actnum      */
        /*              v-dscr             */
        /*                        */
        /*              v-disp-amt         */
        /*              ld-pton WHEN lPrintTon*/
        /*              ld-t[2] WHEN lPrintTon*/
        /*          WITH FRAME gl-sum.     */
        END.

        v-balance = v-balance + v-post-freight.
        /** POST DISCOUNT TO G/L **/
        FIND FIRST account
            WHERE account.company EQ cocode
            AND account.actnum  EQ v-ar-disc
            NO-LOCK NO-ERROR.
        ASSIGN
            v-dscr     = IF AVAILABLE account THEN account.dscr
                   ELSE "ACCOUNT NOT FOUND - DISCOUNT"
            v-disp-amt = 0
            ld-t[2]    = 0.

        IF lGLReportDetail THEN 
        DO:
            FOR EACH tt-report
                WHERE tt-report.term-id EQ ""
                AND tt-report.key-01  EQ "work-disc"
                NO-LOCK
                BREAK BY tt-report.key-02:

                ASSIGN
                    ld-t[1]    = tt-report.weight / 2000
                    v-disp-amt = v-disp-amt + dec(tt-report.key-05)
                    ld-t[2]    = ld-t[2] + ld-t[1].

                IF dec(tt-report.key-05) NE 0 THEN 
                DO:
                    ld-pton = dec(tt-report.key-05) / ld-t[1].

                    IF ld-pton EQ ? THEN ld-pton = 0.

                /*                    DISPLAY v-ar-disc             @ account.actnum*/
                /*                        v-dscr                                    */
                /*                        int(tt-report.key-02) @ inv-head.inv-no   */
                /*                        "DISCOUNT"            @ inv-line.i-no     */
                /*                        dec(tt-report.key-05) @ v-tmp-amt         */
                /*                        ld-pton                                   */
                /*                        WHEN lPrintTon                               */
                /*                        ld-t[1]                                   */
                /*                        WHEN lPrintTon                               */
                /*                        WITH FRAME gl-det.                        */

                END.
            END.

            IF v-disp-amt NE 0 THEN 
            DO:
                PUT v-disp-amt TO 128.
                IF lPrintTon THEN 
                DO:
                    ld-pton = v-disp-amt / ld-t[2].
                    IF ld-pton EQ ? THEN ld-pton = 0.
                    PUT ld-pton TO 138 ld-t[2] TO 148 SKIP(1).
                END.
                ELSE PUT SKIP.
                ASSIGN
                    v-disp-amt = 0
                    ld-t[2]    = 0.
            END.
        END.

        ASSIGN
            v-disp-actnum = v-ar-disc
            v-disp-amt    = v-post-disc
            ld-t[2]       = v-post-disc-w / 2000.

        IF NOT lGLReportDetail THEN 
        DO:
            ld-pton = v-disp-amt / ld-t[2].

            IF ld-pton EQ ? THEN ld-pton = 0.

        /*            DISPLAY v-disp-actnum */
        /*                v-dscr            */
        /*                         */
        /*                v-disp-amt        */
        /*                ld-pton           */
        /*                WHEN lPrintTon       */
        /*                ld-t[2]           */
        /*                WHEN lPrintTon       */
        /*                WITH FRAME gl-sum.*/
            
        END.

        v-balance = v-balance + v-disp-amt.
        /** POST CASH TO G/L **/
        IF v-post-cash NE 0 THEN 
        DO:
            FIND FIRST account
                WHERE account.company EQ cocode
                AND account.actnum  EQ ar-ctrl.cash-act
                NO-LOCK NO-ERROR.
            v-dscr = IF AVAILABLE account THEN account.dscr
            ELSE "ACCOUNT NOT FOUND - CASH".

            IF lGLReportDetail THEN 
            DO:
                ASSIGN
                    v-disp-amt = 0
                    ld-t[2]    = 0.

                FOR EACH tt-report
                    WHERE tt-report.term-id EQ ""
                    AND tt-report.key-01  EQ "work-cash"
                    NO-LOCK
                    BREAK BY tt-report.key-02:

                    ASSIGN
                        ld-t[1]    = tt-report.weight / 2000
                        v-disp-amt = v-disp-amt + dec(tt-report.key-05)
                        ld-t[2]    = ld-t[2] + ld-t[1].

                    IF dec(tt-report.key-05) NE 0 THEN 
                    DO:
                        ld-pton = dec(tt-report.key-05) / ld-t[1].

                        IF ld-pton EQ ? THEN ld-pton = 0.

                    /*                        DISPLAY ar-ctrl.cash-act    @ account.actnum*/
                    /*                            v-dscr                                  */
                    /*                            int(tt-report.key-02)  @ inv-head.inv-no*/
                    /*                            "CASH INVOICE"      @ inv-line.i-no     */
                    /*                            dec(tt-report.key-05)  @ v-tmp-amt      */
                    /*                            ld-pton                                 */
                    /*                            WHEN lPrintTon                             */
                    /*                            ld-t[1]                                 */
                    /*                            WHEN lPrintTon                             */
                    /*                            WITH FRAME gl-det.                      */
                        
                    END.
                END.

                IF v-disp-amt NE 0 THEN 
                DO:
                    PUT v-disp-amt TO 128.
                    IF lPrintTon THEN 
                    DO:
                        ld-pton = v-disp-amt / ld-t[2].
                        IF ld-pton EQ ? THEN ld-pton = 0.
                    /* PUT ld-pton TO 138 ld-t[2] TO 148 SKIP(1). */
                    END.
                    /* ELSE PUT SKIP. */                    
                    ASSIGN
                        v-disp-amt = 0
                        ld-t[2]    = 0.
                END.
            END.

            ASSIGN
                v-disp-actnum = ar-ctrl.cash-act
                v-disp-amt    = v-post-cash
                ld-t[2]       = v-post-cash-w / 2000.

            IF NOT lGLReportDetail THEN 
            DO:
                ld-pton = v-disp-amt / ld-t[2].

                IF ld-pton EQ ? THEN ld-pton = 0.

            /*        DISPLAY v-disp-actnum      */
            /*                v-dscr             */
            /*                          */
            /*                v-disp-amt         */
            /*                ld-pton WHEN lPrintTon*/
            /*                ld-t[2] WHEN lPrintTon*/
            /*            WITH FRAME gl-sum.     */

            END.

            v-balance = v-balance + v-disp-amt.
        END.  
        /** OFFSET ENTRY TO G/L **/
        FIND FIRST account
            WHERE account.company = cocode
            AND account.actnum  = v-ar-acct
            NO-LOCK NO-ERROR.
        ASSIGN
            v-dscr        = IF AVAILABLE account THEN account.dscr
                     ELSE "ACCOUNT NOT FOUND - OFFSET"
            v-disp-actnum = v-ar-acct
            v-disp-amt    = v-post-total.

        IF lGLReportDetail THEN 
        DO:
            ASSIGN
                ld-t[1] = v-post-total-w / 2000
                ld-pton = v-disp-amt / ld-t[1].

            IF ld-pton EQ ? THEN ld-pton = 0.

        /*      DISPLAY v-ar-acct     @ account.actnum*/
        /*              v-dscr                        */
        /*              v-disp-amt    @ v-tmp-amt     */
        /*              ld-pton WHEN lPrintTon           */
        /*              ld-t[1] WHEN lPrintTon           */
        /*          WITH FRAME gl-det.                */
   
        END.

        ELSE 
        DO:
            ASSIGN
                ld-t[2] = v-post-total-w / 2000
                ld-pton = v-disp-amt / ld-t[2].

            IF ld-pton EQ ? THEN ld-pton = 0.

        /*      DISPLAY v-disp-actnum      */
        /*              v-dscr             */
        /*                        */
        /*              v-disp-amt         */
        /*              ld-pton WHEN lPrintTon*/
        /*              ld-t[2] WHEN lPrintTon*/
        /*          WITH FRAME gl-sum.     */

        END.

        v-balance = v-balance + v-post-total.   
        /*    IF lGLReportDetail THEN                         */
        /*      PUT v-disp-amt TO 128 SKIP               */
        /*          "---------------"  TO 128 SKIP       */
        /*          "Total:" AT 86 v-balance TO 128 SKIP.*/
        /*    ELSE                                       */
        /*      PUT "---------------"  TO 104 SKIP       */
        /*          "Total:" AT 79 v-balance TO 104 SKIP.*/
    

        IF ROUND(v-balance,2) NE 0 THEN 
        DO:
            OUTPUT CLOSE.

            FIND FIRST tt-report {oe/invpost7.i} NO-LOCK NO-ERROR.
            lv-rowid = IF AVAILABLE tt-report THEN ROWID(tt-report) ELSE ?.

            RUN oe/invpost7.p (ROUND(v-balance,2), INPUT-OUTPUT lv-rowid).

            FIND tt-report WHERE ROWID(tt-report) EQ lv-rowid NO-LOCK NO-ERROR.

            IF AVAILABLE tt-report THEN 
            DO:
                CREATE b-tt-report. 
                ASSIGN
                    b-tt-report.term-id = ""
                    b-tt-report.key-01  = tt-report.key-01
                    b-tt-report.key-02  = tt-report.key-02
                    v-balance           = v-balance * -1
                    b-tt-report.key-05  = STRING(ROUND(v-balance,2),"-999,999,999.99")
                    v-recid             = RECID(b-tt-report).

                NEXT post-print.
            END.
        END.

        LEAVE.
    END. /* post-print */

    OUTPUT CLOSE.


    SESSION:SET-WAIT-STATE ("general").

    FOR EACH tt-report WHERE RECID(tt-report) NE v-recid:
        DELETE tt-report.
    END.

    SESSION:SET-WAIT-STATE ("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE list-post-inv C-Win 
PROCEDURE list-post-inv :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-list-post AS CHARACTER NO-UNDO.

    DEFINE BUFFER b-oe-boll FOR oe-boll.

    DEFINE VARIABLE ld-t            AS DECIMAL FORMAT "->>>>9.99" EXTENT 3 NO-UNDO.
    DEFINE VARIABLE ld-pton         AS DECIMAL FORMAT "->>>9.999" NO-UNDO.
    DEFINE VARIABLE v-close-line-ok AS LOGICAL INITIAL NO.
    DEFINE VARIABLE v-first         AS LOG     INIT YES.
    DEFINE VARIABLE v-tot-frt       AS DECIMAL NO-UNDO.
  
  
  
    /*  FORMAT                                                                       */
    /*    inv-head.inv-no AT 1                                                       */
    /*    inv-head.inv-date AT 8 FORMAT "99/99/99"                                   */
    /*    inv-head.cust-no AT 17                                                     */
    /*    inv-head.cust-name FORMAT "x(25)" AT 26                                    */
    /*    v-ord-no TO 59                                                             */
    /*    v-inv-qty                                                                  */
    /*    inv-head.t-inv-freight FORMAT "->,>>9.99"                                  */
    /*    inv-head.t-inv-tax FORMAT "->,>>9.99"                                      */
    /*    v-misc-tot FORMAT "->>>>9.99"                                              */
    /*    v-line-tot FORMAT "->>>>>>9.99"                                            */
    /*    inv-head.t-inv-rev TO 131                                                  */
    /*    ld-pton                                                                    */
    /*    ld-t[2]                                                                    */
    /*                                                                               */
    /*  FORMAT                                                                       */
    /*    w-inv-line.i-no AT 10 LABEL "Item"                                         */
    /*    w-inv-line.i-name FORMAT "x(25)" LABEL "Description"                       */
    /*    w-inv-line.qty FORMAT "->>,>>>,>>9" LABEL "Order"                          */
    /*    w-inv-line.inv-qty FORMAT "->>,>>>,>>9" COLUMN-LABEL "Quantities!Invoiced "*/
    /*    w-inv-line.ship-qty FORMAT "->>,>>>,>>9" LABEL "Shipped"                   */
    /*    w-inv-line.t-cost FORMAT "->>>,>>9.99<<<<" LABEL "Cost"                    */
    /*    w-inv-line.price FORMAT "->>>,>>9.99<<<<" LABEL "Price"                    */
    /*    w-inv-line.uom LABEL "UOM"                                                 */
    /*    w-inv-line.t-price COLUMN-LABEL "Extended! Price"                          */
    /*    v-prof  FORMAT "->>>9.99%" COLUMN-LABEL "Profit"                           */
    /*    w-inv-line.i-no AT 10 LABEL "Item"                                         */
    /*    w-inv-line.i-name FORMAT "x(25)" LABEL "Description"                       */
    /*    w-inv-line.qty FORMAT "->>,>>>,>>9" LABEL "Order"                          */
    /*    w-inv-line.inv-qty FORMAT "->>,>>>,>>9" COLUMN-LABEL "Quantities!Invoiced "*/
    /*    w-inv-line.ship-qty FORMAT "->>,>>>,>>9" LABEL "Shipped"                   */
    /*    w-inv-line.t-cost FORMAT "->>>,>>9.99<<<<" LABEL "Cost"                    */
    /*    w-inv-line.price FORMAT "->>>,>>9.99<<<<" LABEL "Price"                    */
    /*    w-inv-line.uom LABEL "UOM"                                                 */
    /*    w-inv-line.t-price COLUMN-LABEL "Extended! Price"                          */
    /*    ld-pton COLUMN-LABEL "!     $/Ton"                                         */
    /*    ld-t[1] COLUMN-LABEL "!      Tons"                                         */
    /*    v-prof  FORMAT "->>>9.99%" COLUMN-LABEL "Profit"                           */
    /*    WITH DOWN NO-BOX STREAM-IO WIDTH 171 FRAME invlt.                          */
    /*                                                                               */
    /*  FORMAT                                                                       */
    /*    w-ord-misc.charge AT 10 LABEL "Charge"                                     */
    /*    w-ord-misc.dscr LABEL "Description"                                        */
    /*    w-ord-misc.amt FORMAT "->>>,>>9.99" TO 71 LABEL "Price" SKIP               */
    /*    WITH STREAM-IO DOWN NO-BOX FRAME invm.                                     */
  
    SESSION:SET-WAIT-STATE ("general").

    RUN oe/invpostd.p ("").
      
    v-post = ip-list-post EQ "post".

    DISABLE TRIGGERS FOR LOAD OF inv-head.
    DISABLE TRIGGERS FOR LOAD OF inv-line.
    DISABLE TRIGGERS FOR LOAD OF oe-ord.
    DISABLE TRIGGERS FOR LOAD OF oe-ordl.
    DISABLE TRIGGERS FOR LOAD OF itemfg.
    DISABLE TRIGGERS FOR LOAD OF oe-relh.
    DISABLE TRIGGERS FOR LOAD OF oe-rell.
    
    ordblock:
    FOR EACH w-report WHERE w-report.term-id EQ "" NO-LOCK,
    
        FIRST inv-head WHERE RECID(inv-head) EQ w-report.rec-id
        
        TRANSACTION
      
        BY w-report.key-01:


        {oe/r-inve&pb.i}
    END.

    FIND CURRENT inv-head NO-LOCK NO-ERROR.
    FIND CURRENT inv-line NO-LOCK NO-ERROR.
    FIND CURRENT itemfg NO-LOCK NO-ERROR.
    FIND CURRENT oe-ordl NO-LOCK NO-ERROR.
    FIND CURRENT ar-invl NO-LOCK NO-ERROR.
    FIND CURRENT oe-ordm NO-LOCK NO-ERROR.
    FIND CURRENT cust NO-LOCK NO-ERROR.

    SESSION:SET-WAIT-STATE ("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-gl C-Win 
PROCEDURE post-gl :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-dscr LIKE gltrans.tr-dscr NO-UNDO. 
   

    /** POST TO GENERAL LEDGER ACCOUNTS TRANSACTION FILE **/
    DO TRANSACTION:
        FOR EACH tt-gl,
            FIRST gltrans WHERE ROWID(gltrans) EQ tt-gl.row-id:
            DELETE gltrans.
        END.

        EMPTY TEMP-TABLE tt-gl.
        /** POST LINE ITEMS TO G/L TRANS **/
        FOR EACH tt-report
            WHERE tt-report.term-id EQ ""
            AND tt-report.key-01  EQ "work-line"
            NO-LOCK
            BREAK BY tt-report.key-02
            BY tt-report.key-03:

            ACCUMULATE dec(tt-report.key-05) (TOTAL BY tt-report.key-03).

            IF LAST-OF(tt-report.key-03) THEN 
            DO:
                RUN get-tr-dscr (INT(tt-report.key-03), OUTPUT lv-dscr).

                CREATE tt-gl.
                CREATE gltrans.
                ASSIGN
                    tt-gl.row-id    = ROWID(gltrans)
                    gltrans.company = cocode
                    gltrans.actnum  = tt-report.key-02
                    gltrans.jrnl    = "OEINV"
                    gltrans.tr-dscr = TRIM(lv-dscr) + " LINE"
                    gltrans.tr-date = dtPostDate
                    gltrans.tr-amt  = - (ACCUMULATE TOTAL BY tt-report.key-03 dec(tt-report.key-05))
                    gltrans.period  = tran-period
                    gltrans.trnum   = v-trnum.
                RELEASE gltrans.
            END. /* last actnum */
        END. /* each work-line */
        /** POST MISC. TO G/L TRANS **/
        FOR EACH tt-report
            WHERE tt-report.term-id EQ ""
            AND tt-report.key-01  EQ "work-misc"
            NO-LOCK
            BREAK BY tt-report.key-02
            BY tt-report.key-03:

            ACCUMULATE dec(tt-report.key-05) (TOTAL BY tt-report.key-03).

            IF LAST-OF(tt-report.key-03) THEN 
            DO:
                RUN get-tr-dscr (INT(tt-report.key-03), OUTPUT lv-dscr).

                CREATE tt-gl.
                CREATE gltrans.
                ASSIGN
                    tt-gl.row-id    = ROWID(gltrans)
                    gltrans.company = cocode
                    gltrans.jrnl    = "OEINV"
                    gltrans.tr-dscr = TRIM(lv-dscr) + " MISC"
                    gltrans.tr-date = dtPostDate
                    gltrans.actnum  = tt-report.key-02
                    gltrans.tr-amt  = - (ACCUMULATE TOTAL BY tt-report.key-03 dec(tt-report.key-05))
                    gltrans.period  = tran-period
                    gltrans.trnum   = v-trnum.
            END. /* last actnum */
        END. /* each work-misc */
        /** POST SALES TAX TO G/L TRANS **/
        FOR EACH tt-report
            WHERE tt-report.term-id EQ ""
            AND tt-report.key-01  EQ "work-tax"
            NO-LOCK
            BREAK BY tt-report.key-02
            BY tt-report.key-03:

            ACCUMULATE dec(tt-report.key-05) (TOTAL BY tt-report.key-03).

            IF LAST-OF(tt-report.key-03) THEN 
            DO:
                RUN get-tr-dscr (INT(tt-report.key-03), OUTPUT lv-dscr).

                CREATE tt-gl.
                CREATE gltrans.
                ASSIGN
                    tt-gl.row-id    = ROWID(gltrans)
                    gltrans.company = cocode
                    gltrans.actnum  = tt-report.key-02
                    gltrans.jrnl    = "OEINV"
                    gltrans.tr-dscr = TRIM(lv-dscr) + " TAX"
                    gltrans.tr-date = dtPostDate
                    gltrans.tr-amt  = - (ACCUMULATE TOTAL BY tt-report.key-03 dec(tt-report.key-05))
                    gltrans.period  = tran-period
                    gltrans.trnum   = v-trnum.
                RELEASE gltrans.
            END. /* last actnum */
        END. /* each work-tax */
        /** POST CURRENCY TO G/L TRANS **/
        FOR EACH tt-report
            WHERE tt-report.term-id EQ ""
            AND tt-report.key-01  EQ "work-curr"
            NO-LOCK
            BREAK BY tt-report.key-02:

            ACCUMULATE dec(tt-report.key-05) (TOTAL BY tt-report.key-02).

            IF LAST-OF(tt-report.key-02) THEN 
            DO:
                CREATE tt-gl.
                CREATE gltrans.
                ASSIGN
                    tt-gl.row-id    = ROWID(gltrans)
                    gltrans.company = cocode
                    gltrans.actnum  = tt-report.key-02
                    gltrans.jrnl    = "OEINV"
                    gltrans.tr-dscr = "ORDER ENTRY INVOICE CURRENCY GAIN/LOSS"
                    gltrans.tr-date = dtPostDate
                    gltrans.tr-amt  = - (ACCUMULATE TOTAL BY tt-report.key-02 dec(tt-report.key-05))
                    gltrans.period  = tran-period
                    gltrans.trnum   = v-trnum.

                RELEASE gltrans.
            END. /* last actnum */
        END. /* each work-tax */

        FOR EACH tmp-work-job
            BREAK BY tmp-work-job.fg
            BY tmp-work-job.actnum
            BY tmp-work-job.inv-no:

            ACCUMULATE tmp-work-job.amt (TOTAL BY tmp-work-job.inv-no).

            IF LAST-OF(tmp-work-job.inv-no) THEN 
            DO:
                RUN get-tr-dscr (tmp-work-job.inv-no, OUTPUT lv-dscr).

                CREATE tt-gl.
                CREATE gltrans.
                ASSIGN
                    tt-gl.row-id    = ROWID(gltrans)
                    gltrans.company = cocode
                    gltrans.actnum  = tmp-work-job.actnum
                    gltrans.jrnl    = "OEINV"
                    gltrans.tr-date = dtPostDate
                    gltrans.period  = tran-period
                    gltrans.trnum   = v-trnum.

                IF tmp-work-job.fg THEN
                    ASSIGN
                        gltrans.tr-amt  = - (ACCUMULATE TOTAL BY tmp-work-job.inv-no tmp-work-job.amt)
                        gltrans.tr-dscr = TRIM(lv-dscr) + " FG".
                ELSE
                    ASSIGN
                        gltrans.tr-amt  = (ACCUMULATE TOTAL BY tmp-work-job.inv-no tmp-work-job.amt)
                        gltrans.tr-dscr = TRIM(lv-dscr) + " COGS".

                RELEASE gltrans.
            END.
        END. /* each work-job */

        /** POST FREIGHT TO G/L TRANS **/
        CREATE tt-gl.
        CREATE gltrans.
        ASSIGN
            tt-gl.row-id    = ROWID(gltrans)
            gltrans.company = cocode
            gltrans.actnum  = v-ar-freight
            gltrans.jrnl    = "OEINV"
            gltrans.tr-dscr = "ORDER ENTRY INVOICE FREIGHT"
            gltrans.tr-date = dtPostDate 
            gltrans.tr-amt  = v-post-freight
            gltrans.period  = tran-period
            gltrans.trnum   = v-trnum.
        RELEASE gltrans. 
        /** POST DISCOUNT TO G/L TRANS **/
        CREATE tt-gl.
        CREATE gltrans.
        ASSIGN
            tt-gl.row-id    = ROWID(gltrans) 
            gltrans.company = cocode
            gltrans.actnum  = v-ar-disc
            gltrans.jrnl    = "OEINV"
            gltrans.tr-dscr = "ORDER ENTRY INVOICE DISCOUNT"
            gltrans.tr-date = dtPostDate
            gltrans.tr-amt  = v-post-disc
            gltrans.period  = tran-period
            gltrans.trnum   = v-trnum.
        RELEASE gltrans.

        /** POST CASH TO G/L TRANS **/
        IF v-post-cash NE 0 THEN 
        DO:
            CREATE tt-gl.
            CREATE gltrans.
            ASSIGN
                tt-gl.row-id    = ROWID(gltrans)
                gltrans.company = cocode
                gltrans.actnum  = ar-ctrl.cash-act
                gltrans.jrnl    = "CASHR"
                gltrans.tr-dscr = "CASH RECEIPT - INVOICE"
                gltrans.tr-date = dtPostDate
                gltrans.tr-amt  = v-post-cash
                gltrans.period  = tran-period
                gltrans.trnum   = v-trnum
                v-post-cash     = - v-post-cash.
            RELEASE gltrans.
        END.
        /** OFFSET ENTRY TO G/L **/
        CREATE tt-gl.
        CREATE gltrans.
        ASSIGN
            tt-gl.row-id    = ROWID(gltrans)
            gltrans.company = cocode
            gltrans.actnum  = v-ar-acct
            gltrans.jrnl    = "OEINV"
            gltrans.tr-dscr = "ORDER ENTRY INVOICE"
            gltrans.tr-date = dtPostDate
            gltrans.tr-amt  = v-post-total
            gltrans.period  = tran-period
            gltrans.trnum   = v-trnum.
        RELEASE gltrans.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPrintPost C-Win
PROCEDURE pPrintPost:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-post      AS LOG       NO-UNDO.
    DEFINE VARIABLE v-close-line AS LOG       NO-UNDO.
    DEFINE VARIABLE cStatus      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReason      AS CHARACTER NO-UNDO.


    DO TRANSACTION:       /** GET next G/L TRANS. POSTING # **/
        REPEAT:
            FIND FIRST gl-ctrl EXCLUSIVE-LOCK
                WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
            IF AVAILABLE gl-ctrl THEN 
            DO:

                ASSIGN 
                    v-trnum       = gl-ctrl.trnum + 1 
                    gl-ctrl.trnum = v-trnum.
                FIND CURRENT gl-ctrl NO-LOCK.
                LEAVE.
            END. /* IF AVAIL gl-ctrl */
        END. /* REPEAT */
    /* gdm - 11050906 */

    END.
  
    RUN run-report.


    IF v-postable THEN 
    DO:
      IF v-balance = 0 THEN  
        lv-post = YES.

        IF lv-post THEN 
        DO:

            RUN list-post-inv ("post").

            RUN post-gl.
            RUN copy-report-to-audit-dir.

            FOR EACH tt-report:
                DELETE tt-report.
            END.

            EMPTY TEMP-TABLE tt-gl.

            /*            IF v-export EQ "Sonoco" THEN                                        */
            /*                RUN ar/sonoinv.p ("total", t-rec-written, OUTPUT v-rec-written).*/
          
            FOR EACH w-ord BREAK BY w-ord.ord-no:
                IF NOT FIRST-OF(w-ord.ord-no) THEN DELETE w-ord.
            END.

            order-close1:
            FOR EACH w-ord,
                FIRST oe-ord NO-LOCK
                WHERE oe-ord.company EQ cocode
                AND oe-ord.ord-no  EQ w-ord.ord-no
                BREAK BY oe-ord.cust-no:

                RELEASE cust.
                RUN oe/calcordt.p (ROWID(oe-ord)).
                IF LAST-OF(oe-ord.cust-no) THEN 
                DO:

                    FIND FIRST tt-custbal WHERE tt-custbal.cust-no EQ oe-ord.cust-no
                        NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE tt-custbal THEN 
                    DO:
                        CREATE tt-custbal.
                        ASSIGN 
                            tt-custbal.cust-no = oe-ord.cust-no.
                    END.

                    FIND FIRST cust NO-LOCK /* EXCLUSIVE */
                        WHERE cust.company EQ oe-ord.company
                        AND cust.cust-no EQ oe-ord.cust-no
                         NO-ERROR.

                    IF AVAILABLE cust THEN 
                    DO:
                        RUN ar/updcust1.p (NO, BUFFER cust, OUTPUT tt-custbal.ord-bal).


                        FIND CURRENT cust NO-LOCK.
                    END.
                END.


            END. /* Each w-ord */
      
            cust-bal:
            FOR EACH tt-custbal,
                FIRST cust EXCLUSIVE-LOCK WHERE cust.company EQ cocode
                AND cust.cust-no EQ tt-custbal.cust-no
                .
                cust.ord-bal = tt-custbal.ord-bal.         

                IF cust.ord-bal LT 0 THEN cust.ord-bal = 0.

            END.

            order-close2:
            FOR EACH w-ord,
                FIRST oe-ord NO-LOCK
                WHERE oe-ord.company EQ cocode
                AND oe-ord.ord-no  EQ w-ord.ord-no
                BREAK BY oe-ord.cust-no:

                RELEASE cust.



                IF NOT oeclose-log  THEN
                DO:
                    FOR EACH oe-ordl NO-LOCK WHERE
                        oe-ordl.company EQ oe-ord.company AND
                        oe-ordl.ord-no  EQ oe-ord.ord-no AND
                        oe-ordl.stat    NE "C"
                        :
                        /* No UI */
                        RUN oe/CloseOrder.p(INPUT ROWID(oe-ordl),
                            INPUT NO,
                            OUTPUT cStatus,
                            OUTPUT cReason).
                        /* No UI */
                        IF cStatus EQ 'C' THEN
                            RUN oe/closelin.p (INPUT ROWID(oe-ordl),YES).
                    END.

                    RUN close-order.
                END.
            END. /* Each w-ord */
            

            IF oeclose-log THEN
            DO:
                RUN oe/closchkinv.p (0).
            /* Contains UI, so taken out for batch mode  */
            /*                IF CAN-FIND (FIRST w-ord) THEN*/
            /*                    RUN oe/d-close.w.         */
            END.
        END.
    END.


    FOR EACH save-line WHERE save-line.reftable EQ "save-line" + STRING(v-trnum,"9999999999"):
        RUN undo-save-line.
    END.

    IF NOT v-postable OR NOT lv-post THEN 
    DO TRANSACTION:
        /* gdm - 11050906 */
        REPEAT:
            FIND FIRST gl-ctrl EXCLUSIVE-LOCK
                WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
            IF AVAILABLE gl-ctrl THEN 
            DO:

                IF gl-ctrl.trnum EQ v-trnum THEN gl-ctrl.trnum = v-trnum - 1.
                FIND CURRENT gl-ctrl NO-LOCK.
                LEAVE.
            END. /* IF AVAIL gl-ctrl */
        END. /* REPEAT */
    /* gdm - 11050906 */
    END.

    IF v-ftp-done THEN MESSAGE "File Export/FTP is completed." VIEW-AS ALERT-BOX INFORMATION.

    SESSION:SET-WAIT-STATE("").


END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    /* ---------------------------------------------------- oe/invpost.p 10/94 gb */
    /* Invoicing  - Edit Register & Post Invoicing Transactions                   */
    /* -------------------------------------------------------------------------- */
    DEFINE BUFFER xinv-head FOR inv-head.
    DEFINE VARIABLE str-tit4                AS CHARACTER FORMAT "x(20)" NO-UNDO.
    DEFINE VARIABLE lv-label-ton            AS CHARACTER FORMAT "x(20)" EXTENT 2 NO-UNDO.
    DEFINE VARIABLE v-contsrvc-export-found AS LOG       NO-UNDO.
    DEFINE VARIABLE v-goodman-export-found  AS LOG       NO-UNDO.

    /*    FORMAT HEADER                                                                 */
    /*        str-tit4 AT 58                                                            */
    /*        SKIP(1)                                                                   */
    /*        "  - Invoice - " SKIP                                                     */
    /*        "Number"  "Date" AT 10  "Cust#" AT 17 "Customer Name" AT 26 "Order#" TO 59*/
    /*        "Quantity" TO 74 "Frt" TO 84 "Tax" TO 94                                  */
    /*        "Misc" TO 104 "Items" TO 116                                              */
    /*        "Total" TO 131                                                            */
    /*        lv-label-ton[1] TO 151                                                    */
    /*        FILL("=",131) FORMAT "x(131)"                                             */
    /*        lv-label-ton[2] TO 151                                                    */
    /*        WITH FRAME r-top WIDTH 151.                                               */
  
    FIND FIRST period NO-LOCK                 
        WHERE period.company EQ gcompany
        AND period.pst     LE dtPostDate
        AND period.pend    GE dtPostDate
         NO-ERROR.
 
        
    /*        iStartInvNo = begin_inv    */
    /*        iEndInvNo = end_inv      */
    /*        dtStartInvoiceDate   = begin_date   */
    /*        dtEndInvoiceDate   = end_date     */
    /*        lInvoiceReportDetail   = lInvoiceReportDetail  */
    /*        lGLReportDetail = lGLReportDetail*/
    v-postable = NO.

    /*    IF lGLReportDetail THEN                                   */
    /*        ASSIGN                                       */
    /*            lv-label-ton[1] = "     $/Ton      Tons" */
    /*            lv-label-ton[2] = "====================".*/

    SESSION:SET-WAIT-STATE ("general").

    EMPTY TEMP-TABLE w-report.

    FOR EACH inv-head NO-LOCK
        WHERE inv-head.company  EQ cocode
        AND inv-head.printed  EQ YES
        AND inv-head.inv-no   GT 0
        AND inv-head.inv-no   GE iStartInvNo
        AND inv-head.inv-no   LE iEndInvNo
        AND inv-head.inv-date GE dtStartInvoiceDate
        AND inv-head.inv-date LE dtEndInvoiceDate
        AND inv-head.stat     NE "H"
        USE-INDEX prnt,
             
        FIRST cust NO-LOCK
        WHERE cust.company EQ cocode
        AND cust.cust-no EQ inv-head.cust-no
        AND ((cust.inv-meth EQ ? AND inv-head.multi-invoice) OR
        (cust.inv-meth NE ? AND NOT inv-head.multi-invoice))
        TRANSACTION:

        FIND FIRST xinv-head EXCLUSIVE-LOCK WHERE RECID(xinv-head) EQ recid(inv-head)
             NO-WAIT NO-ERROR.
        IF AVAILABLE xinv-head THEN 
        DO:

            CREATE w-report.
            ASSIGN
                w-report.term-id = ""
                w-report.key-01  = STRING(xinv-head.inv-no,"9999999999")
                w-report.rec-id  = RECID(xinv-head).
            CREATE tt-report.
            ASSIGN
                tt-report.term-id = ""
                tt-report.key-01  = STRING(xinv-head.inv-no,"9999999999")
                tt-report.rec-id  = RECID(xinv-head).
           
            IF inv-head.multi-invoice THEN
                IF CAN-FIND(FIRST b-inv-head
                    WHERE b-inv-head.company       EQ inv-head.company
                    AND b-inv-head.cust-no       EQ inv-head.cust-no
                    AND b-inv-head.inv-no        EQ inv-head.inv-no
                    AND b-inv-head.multi-invoice EQ NO) THEN
                    FOR EACH b-inv-head NO-LOCK
                        WHERE b-inv-head.company       EQ inv-head.company
                        AND b-inv-head.cust-no       EQ inv-head.cust-no
                        AND b-inv-head.inv-no        EQ inv-head.inv-no
                        AND b-inv-head.multi-invoice EQ NO:

                        RUN create-save-line.
                    END.

                ELSE 
                DO:
                    DELETE tt-report.
                    DELETE w-report.
                    DELETE xinv-head.
                    NEXT.
                END.

            IF cust.factored THEN
                FOR EACH inv-line NO-LOCK WHERE inv-line.r-no = inv-head.r-no:
                    IF CAN-FIND(FIRST reftable WHERE reftable.reftable EQ "FACTORED"
                        AND reftable.company  EQ inv-head.company
                        AND reftable.loc      EQ ""
                        AND reftable.code     EQ inv-line.i-no)
                        THEN 
                    DO:
                        tt-report.key-02 = "Factored".  /* for oe/rep/expfrank.p task#  09200521*/
                        LEAVE.
                    END.
                END.
        END.
    END.


    lv-list-name = list-name.

    RUN list-post-inv ("list").
  
    OUTPUT CLOSE.

    /* export invoices to factor */   
    v-ftp-done = NO.

    IF FALSE AND  inexport-log THEN 
    DO:    
        DEFINE VARIABLE v-exp-file AS cha NO-UNDO.
        v-exp-file = inexport-desc +  
            "INVOICE_" + 
            substr(STRING(YEAR(TODAY),"9999"),3,2) +
            string(MONTH(TODAY),"99") +
            string(DAY(TODAY),"99") +
            substr(STRING(TIME,"HH:MM:SS"),1,2) +
            substr(STRING(TIME,"HH:MM:SS"),4,2) +
            substr(STRING(TIME,"HH:MM:SS"),7,2) + ".dat".

        IF (v-print-fmt = "Frankstn" OR v-print-fmt = "MIRPKG" ) AND inexport-cha EQ "CIT" THEN 
        DO:
            OUTPUT TO VALUE(v-exp-file).
            RUN oe/rep/expfrank.p .
            OUTPUT CLOSE.
            OUTPUT TO VALUE(".\oe\ftpcmd2.txt").     /* ftp text file */
            PUT UNFORMATTED 
                "open cs.ftp.citonline.com" SKIP  /* ftp server ip address */
                "ftpa1526" SKIP  /* userid*/
                "none" SKIP  /* password */
                "put " v-exp-file " " '"' "$$ ID=EP003F BID='DI1526' PASSWORD=NARF" '"' SKIP         /* file to transfer */
                "quit" .
            OUTPUT CLOSE.
            OS-COMMAND SILENT VALUE("ftp -v -i -s:.\oe\ftpcmd2.txt"). 
            v-ftp-done = YES.
        END.
        ELSE
            IF inexport-cha EQ "ContSrvc" THEN 
            DO:
                v-ftp-done = YES.

                FOR EACH tt-report NO-LOCK WHERE tt-report.term-id EQ "",
                    FIRST inv-head WHERE RECID(inv-head) EQ tt-report.rec-id NO-LOCK,
                    FIRST cust NO-LOCK WHERE cust.company = inv-head.company AND
                    cust.cust-no = inv-head.cust-no AND
                    cust.an-edi-cust AND
                    NOT CAN-FIND(FIRST sys-ctrl-shipto WHERE
                    sys-ctrl-shipto.company EQ cust.company AND
                    sys-ctrl-shipto.NAME EQ "INEXPORT" AND
                    sys-ctrl-shipto.cust-vend EQ YES AND
                    sys-ctrl-shipto.cust-vend-no EQ cust.cust-no AND
                    sys-ctrl-shipto.char-fld EQ "GOODMAN")
                    :

                    v-contsrvc-export-found = YES.
                    LEAVE.
                END.

                IF v-contsrvc-export-found THEN
                DO:
                    OUTPUT TO VALUE(v-exp-file).
                    RUN oe/rep/expconts.p .
                    OUTPUT CLOSE.
                END.

                FOR EACH tt-report NO-LOCK WHERE tt-report.term-id EQ "",
                    FIRST inv-head WHERE RECID(inv-head) EQ tt-report.rec-id NO-LOCK,
                    FIRST cust NO-LOCK WHERE cust.company = inv-head.company AND
                    cust.cust-no = inv-head.cust-no AND
                    cust.an-edi-cust AND
                    CAN-FIND(FIRST sys-ctrl-shipto WHERE
                    sys-ctrl-shipto.company EQ cust.company AND
                    sys-ctrl-shipto.NAME EQ "INEXPORT" AND
                    sys-ctrl-shipto.cust-vend EQ YES AND
                    sys-ctrl-shipto.cust-vend-no EQ cust.cust-no AND
                    sys-ctrl-shipto.char-fld EQ "GOODMAN")
                    :
                    v-goodman-export-found = YES.
                    LEAVE.
                END.

                IF v-goodman-export-found THEN
                DO:
                    v-exp-file = inexport-desc +  
                        "INVOICEG_" + 
                        substr(STRING(YEAR(TODAY),"9999"),3,2) +
                        string(MONTH(TODAY),"99") +
                        string(DAY(TODAY),"99") +
                        substr(STRING(TIME,"HH:MM:SS"),1,2) +
                        substr(STRING(TIME,"HH:MM:SS"),4,2) +
                        substr(STRING(TIME,"HH:MM:SS"),7,2) + ".dat".
                    OUTPUT TO VALUE(v-exp-file).
                    RUN oe/rep/expgoodman.p .
                    OUTPUT CLOSE.
                END.
            END.

    END.
    /* end of export */

    IF v-postable THEN RUN list-gl.   

    EMPTY TEMP-TABLE tt-report.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE undo-save-line C-Win 
PROCEDURE undo-save-line :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DISABLE TRIGGERS FOR LOAD OF inv-line.
    DISABLE TRIGGERS FOR LOAD OF inv-misc.


    RELEASE inv-line.
    RELEASE inv-misc.

    FIND FIRST inv-line EXCLUSIVE-LOCK WHERE RECID(inv-line) EQ INT(save-line.val[3]) NO-ERROR.

    IF AVAILABLE inv-line THEN inv-line.r-no = save-line.val[1].

    ELSE
        FIND FIRST inv-misc EXCLUSIVE-LOCK WHERE RECID(inv-misc) EQ INT(save-line.val[3]) NO-ERROR.

    IF AVAILABLE inv-misc THEN inv-misc.r-no = save-line.val[1].

    DELETE save-line.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



