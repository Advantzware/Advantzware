&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : aoaAppSrv/aoaFG.p
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

/* Customer Inventory.rpa */
DEFINE TEMP-TABLE ttCustomerInventory NO-UNDO
    {aoaAppSrv/ttFields.i}
    FIELD custNo      AS CHARACTER          LABEL "Cust No"
    FIELD custName    LIKE cust.name        LABEL "Customer Name"
    FIELD itemClass   LIKE itemfg.class     LABEL "Class"
    FIELD itemNo      LIKE itemfg.i-no      LABEL "Item Number"
    FIELD partNo      LIKE itemfg.part-no   LABEL "Part Number"
    FIELD itemName    LIKE itemfg.i-name    LABEL "Description"
    FIELD orderLevel  LIKE itemfg.ord-level LABEL "Order Level"
    FIELD releasePO   LIKE oe-rel.po-no     LABEL "Release PO"
    FIELD qtyOnHand   LIKE itemfg.q-onh     LABEL "Qty On Hand"
    FIELD palletCount AS INTEGER            LABEL "Pallet Count" FORMAT "->>>,>>>,>>>"
    FIELD releaseQty  AS CHARACTER          LABEL "Release Qty"  FORMAT "x(11)" INITIAL "___________"
    FIELD requestDate AS DATE               LABEL "Date"         FORMAT "99/99/9999"
    FIELD xxSort1     AS CHARACTER          LABEL "Sort 1"       FORMAT "x(500)"
    FIELD xxSort2     AS CHARACTER          LABEL "Sort 2"       FORMAT "x(500)"
    .
{sys/ref/CustList.i NEW}
/* Customer Inventory.rpa */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-fCustomerInventory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fCustomerInventory Procedure 
FUNCTION fCustomerInventory RETURNS HANDLE
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

&IF DEFINED(EXCLUDE-pBuildCustList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBuildCustList Procedure 
PROCEDURE pBuildCustList :
/*------------------------------------------------------------------------------
  Purpose:     Customer Inventory.rpa
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

&IF DEFINED(EXCLUDE-pCustomerInventory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCustomerInventory Procedure 
PROCEDURE pCustomerInventory :
/*------------------------------------------------------------------------------
  Purpose:     Customer Inventory.rpa
  Parameters:  Company, Batch Seq, User ID
  Notes:       
------------------------------------------------------------------------------*/
    {aoaAppSrv/pCustomerInventory.i}

    /* local variables */
    DEFINE VARIABLE iPalletCount AS INTEGER   NO-UNDO.
    DEFINE VARIABLE rRowID       AS ROWID     NO-UNDO.
    DEFINE VARIABLE cStatus      AS CHARACTER NO-UNDO.

    /* subject business logic */
    FOR EACH ttCustList
        WHERE ttCustList.cust-no GT ""
          AND ttCustList.log-fld EQ TRUE,
        FIRST cust NO-LOCK
        WHERE cust.company EQ ipcCompany
          AND cust.cust-no EQ ttCustList.cust-no
          AND (IF NOT lIncludeInactiveCustomers THEN cust.active EQ "A" ELSE TRUE),
        EACH itemfg NO-LOCK
        WHERE itemfg.company EQ ipcCompany
          AND itemfg.cust-no EQ ttCustList.cust-no
          AND CAN-DO(cInventoryClasses,itemfg.class) EQ TRUE
        :
        ASSIGN
            iPalletCount = 0
            rRowID  = ?
            .
        FOR EACH fg-bin NO-LOCK
            WHERE fg-bin.company EQ ipcCompany
              AND fg-bin.i-no    EQ itemfg.i-no
            USE-INDEX co-ino
            BREAK BY fg-bin.qty DESC:
            ASSIGN
                iPalletCount = (IF fg-bin.case-count   EQ 0 THEN 1
                           ELSE fg-bin.case-count) *
                            (IF fg-bin.cases-unit   EQ 0 THEN 1
                           ELSE fg-bin.cases-unit) *
                            (IF fg-bin.units-pallet EQ 0 THEN 1
                           ELSE fg-bin.units-pallet)
                rRowID = ROWID(fg-bin)
                .
            LEAVE.
        END. /* each fg-bin */
        IF NOT lIncludeZeroQty AND iPalletCount EQ 0 THEN NEXT.
        FIND fg-bin NO-LOCK WHERE ROWID(fg-bin) EQ rRowID NO-ERROR.
        RELEASE oe-ordl.
        RELEASE oe-rel.
        FOR EACH oe-ordl NO-LOCK
            WHERE oe-ordl.company  EQ ipcCompany
              AND oe-ordl.i-no     EQ itemfg.i-no,
            FIRST oe-ord OF oe-ordl NO-LOCK
            WHERE oe-ord.cust-no EQ ttCustList.cust-no
              BY oe-ordl.req-date DESC
              BY oe-ordl.ord-no   DESC
            :
            LEAVE.
        END. /* each oe-ordl */
        IF AVAIL oe-ordl THEN
        FOR EACH oe-rel NO-LOCK
            WHERE oe-rel.company EQ oe-ordl.company
              AND oe-rel.ord-no  EQ oe-ordl.ord-no
              AND oe-rel.i-no    EQ oe-ordl.i-no
              AND oe-rel.line    EQ oe-ordl.line
            BY oe-rel.rel-date:
            {oe/rel-stat.i cStatus}
            IF INDEX("CZ",cStatus) EQ 0 THEN LEAVE.
        END. /* each oe-rel */
        CREATE ttCustomerInventory.
        ASSIGN
            ttCustomerInventory.xxSort1     = cust.cust-no + itemfg.class
            ttCustomerInventory.xxSort2     = IF cSort EQ "Item" THEN itemfg.i-no + itemfg.part-no
                                                                 ELSE itemfg.part-no + itemfg.i-no
            ttCustomerInventory.custNo      = cust.cust-no
            ttCustomerInventory.custName    = cust.name
            ttCustomerInventory.itemClass   = itemfg.class
            ttCustomerInventory.itemNo      = itemfg.i-no
            ttCustomerInventory.partNo      = itemfg.part-no
            ttCustomerInventory.itemName    = itemfg.i-name
            ttCustomerInventory.orderLevel  = itemfg.ord-level
            ttCustomerInventory.releasePO   = oe-rel.po-no WHEN AVAILABLE oe-rel
            ttCustomerInventory.qtyOnHand   = itemfg.q-onh
            ttCustomerInventory.palletCount = iPalletCount
            ttCustomerInventory.requestDate = oe-ordl.req-date WHEN AVAILABLE oe-rel
            .
    END. /* each ttcust */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-fCustomerInventory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fCustomerInventory Procedure 
FUNCTION fCustomerInventory RETURNS HANDLE
  ( ipcCompany AS CHARACTER,
    ipiBatch   AS INTEGER,
    ipcUserID  AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  Customer Inventory.rpa
    Notes:  
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttCustomerInventory.

    RUN pCustomerInventory (ipcCompany, ipiBatch, ipcUserID).

    RETURN TEMP-TABLE ttCustomerInventory:HANDLE .

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
        /* Customer Inventory.rpa */
        WHEN "r-cusinv." THEN
        RETURN TEMP-TABLE ttCustomerInventory:HANDLE.
    END CASE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

