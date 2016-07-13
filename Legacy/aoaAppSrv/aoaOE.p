&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : aoaAppSrv/aoaOE.p
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

/* BOL Packing List.rpa */
DEFINE TEMP-TABLE ttBOLPackingList NO-UNDO
    {aoaAppSrv/ttFields.i}
    FIELD bolNo      AS INTEGER   LABEL "BOL No"       FORMAT ">>>>>>>9"
    FIELD shipDate   AS DATE      LABEL "Shipped"      FORMAT "99/99/9999"
    FIELD pickTicket AS CHARACTER LABEL "Pick Ticket"  FORMAT "x(8)"
    FIELD custName   AS CHARACTER LABEL "Customer"     FORMAT "x(30)"
    FIELD custNo     AS CHARACTER LABEL "Cust No"      FORMAT "x(8)"
    FIELD itemNo     AS CHARACTER LABEL "Item ID"      FORMAT "x(15)"
    FIELD poNo       AS INTEGER   LABEL "PO No"        FORMAT ">>>>>9"
    FIELD relNo      AS INTEGER   LABEL "Release No"   FORMAT ">>>>>9"
    FIELD orderNo    AS INTEGER   LABEL "Order No"     FORMAT ">>>>>9"
    FIELD jobNo      AS CHARACTER LABEL "Job No"       FORMAT "x(6)"
    FIELD ticket     AS CHARACTER LABEL "Ticket"       FORMAT "x(8)"
    FIELD tagDate    AS DATE      LABEL "Prod Date"    FORMAT "99/99/9999"
    FIELD qtyCase    AS INTEGER   LABEL "Cases"        FORMAT "->>>,>>9"
    FIELD caseBundle AS INTEGER   LABEL "Cartons/Case" FORMAT ">>>,>>9"
    FIELD partial    AS DECIMAL   LABEL "Partials"     FORMAT ">>>,>>9"
    FIELD weight     AS INTEGER   LABEL "Wgt/Case"     FORMAT ">>>>9"
    FIELD prntr      AS CHARACTER LABEL "Printer"      FORMAT "x(8)"
    FIELD xxSort     AS CHARACTER LABEL "Sort"         FORMAT "x(50)"
        INDEX sortBy IS PRIMARY rowType xxSort
        .
{sys/ref/CustList.i NEW}
/* BOL Packing List.rpa */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-fBOLPackingList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fBOLPackingList Procedure 
FUNCTION fBOLPackingList RETURNS HANDLE
  ( ipcCompany AS CHARACTER,
    ipiBatch   AS INTEGER,
    ipcUserID  AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fGetTableHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetTableHandle Procedure 
FUNCTION fGetTableHandle RETURNS HANDLE
  ( ipcProgramID AS CHARACTER )  FORWARD.

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

&IF DEFINED(EXCLUDE-pBOLPackingList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBOLPackingList Procedure 
PROCEDURE pBOLPackingList :
/*------------------------------------------------------------------------------
  Purpose:     BOL Packing List.rpa
  Parameters:  Company, Batch Seq, User ID
  Notes:       
------------------------------------------------------------------------------*/
    {aoaAppSrv/pBOLPackingList.i}
    
    /* local variables */

    /* subject business logic */
    FOR EACH oe-bolh NO-LOCK
        WHERE oe-bolh.company EQ ipcCompany
          AND oe-bolh.bol-no  GE iStartBOL
          AND oe-bolh.bol-no  LE iEndBOL
          AND oe-bolh.bol-date GE dtStartBOLDate
          AND oe-bolh.bol-date LE dtEndBOLDate
          AND oe-bolh.cust-no GE cStartCustNo
          AND oe-bolh.cust-no LE cEndCustNo,
        EACH oe-boll NO-LOCK
        WHERE oe-boll.company EQ oe-bolh.company
          AND oe-boll.b-no    EQ oe-bolh.b-no
          AND oe-boll.ord-no  GE iStartOrderNo
          AND oe-boll.ord-no  LE iEndOrderNo
        BREAK BY oe-bolh.company
              BY oe-bolh.cust-no
        :
        CREATE ttBOLPackingList.
        ASSIGN
            ttBOLPackingList.bolNo       = oe-boll.bol-no
            ttBOLPackingList.shipDate    = oe-boll.bol-date
            ttBOLPackingList.pickTicket  = ""
            ttBOLPackingList.custNo      = IF oe-boll.cust-no NE "" THEN oe-boll.cust-no
                                           ELSE oe-bolh.cust-no
            ttBOLPackingList.itemNo      = oe-boll.i-no
            ttBOLPackingList.relNo       = oe-bolh.release#
            ttBOLPackingList.orderNo     = oe-boll.ord-no
            ttBOLPackingList.jobNo       = oe-boll.job-no + STRING(oe-boll.job-no2,"99")
            ttBOLPackingList.prntr       = cPrinter
            ttBOLPackingList.xxSort      = ""
            .
        FIND FIRST cust NO-LOCK
             WHERE cust.company EQ ipcCompany
               AND cust.cust-no EQ ttBOLPackingList.custNo
             NO-ERROR.
        IF AVAILABLE cust THEN
        ttBOLPackingList.custName = cust.name. 
        FIND FIRST loadtag NO-LOCK
             WHERE loadtag.company   EQ oe-boll.company
               AND loadtag.item-type EQ NO
               AND loadtag.tag-no    EQ oe-boll.tag
               AND loadtag.job-no    EQ oe-boll.job-no
               AND loadtag.i-no      EQ oe-boll.i-no
             NO-ERROR.
        IF NOT AVAILABLE loadtag THEN NEXT.
        ASSIGN
            ttBOLPackingList.poNo       = loadtag.po-no
            ttBOLPackingList.tagDate    = loadtag.tag-date
            ttBOLPackingList.qtyCase    = loadtag.qty-case
            ttBOLPackingList.caseBundle = loadtag.case-bundle
            ttBOLPackingList.partial    = loadtag.partial
            .
        FIND FIRST rfidtag NO-LOCK
             WHERE rfidtag.company   EQ loadtag.company
               AND rfidtag.item-type EQ loadtag.item-type
               AND rfidtag.tag-no    EQ loadtag.tag-no
             NO-ERROR.
        IF AVAILABLE rfidtag AND LENGTH(rfidtag.rfidtag) GT 5 THEN
        ttBOLPackingList.ticket = SUBSTR(rfidtag.rfidtag,LENGTH(rfidtag.rfidtag) - 5).
    END. /* each oe-bolh */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pBuildCustList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBuildCustList Procedure 
PROCEDURE pBuildCustList :
/*------------------------------------------------------------------------------
  Purpose:     BOL Packing List.rpa
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-fBOLPackingList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fBOLPackingList Procedure 
FUNCTION fBOLPackingList RETURNS HANDLE
  ( ipcCompany AS CHARACTER,
    ipiBatch   AS INTEGER,
    ipcUserID  AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  BOL Packing List.rpa
    Notes:  
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttBOLPackingList.

    RUN pBOLPackingList (ipcCompany, ipiBatch, ipcUserID).

    RETURN TEMP-TABLE ttBOLPackingList:HANDLE .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fGetTableHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetTableHandle Procedure 
FUNCTION fGetTableHandle RETURNS HANDLE
  ( ipcProgramID AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    CASE ipcProgramID:
        /* BOL Packing List.rpa */
        WHEN "bolpcklst." THEN
        RETURN TEMP-TABLE ttBOLPackingList:HANDLE.
    END CASE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

