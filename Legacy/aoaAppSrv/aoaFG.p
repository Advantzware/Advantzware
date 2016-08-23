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
    FIELD qtyOnHand   AS INTEGER            LABEL "Qty On Hand"  FORMAT "->>>>,>>>,>>>"
    FIELD palletCount AS INTEGER            LABEL "Pallet Count" FORMAT "->>>,>>>,>>>"
    FIELD releaseQty  AS CHARACTER          LABEL "Release Qty"  FORMAT "x(11)" INITIAL "___________"
    FIELD requestDate AS DATE               LABEL "Date"         FORMAT "99/99/9999"
    FIELD xxSort1     AS CHARACTER          LABEL "Sort 1"       FORMAT "x(500)"
    FIELD xxSort2     AS CHARACTER          LABEL "Sort 2"       FORMAT "x(500)"
        INDEX sortBy IS PRIMARY rowType xxSort1 xxSort2
        .
/* Customer Inventory.rpa */

/* Inventory Value.rpa */
DEFINE TEMP-TABLE ttInventoryValue NO-UNDO
    {aoaAppSrv/ttFields.i}
    FIELD custNo       AS CHARACTER LABEL "Customer"
    FIELD custName     AS CHARACTER LABEL "Cust Name"    FORMAT "x(30)"
    FIELD salesRep     AS CHARACTER LABEL "Rep"          FORMAT "X(3)"
    FIELD iNo          AS CHARACTER LABEL "Item"         FORMAT "x(15)"
    FIELD iName        AS CHARACTER LABEL "Description"  FORMAT "X(30)"
    FIELD tagNo        AS CHARACTER LABEL "Tag"          FORMAT "x(6)"
    FIELD tag          AS CHARACTER LABEL "Full Tag"     FORMAT "x(24)"    
    FIELD fgLotVal     AS CHARACTER LABEL "FG LOT#"      FORMAT "x(20)"
    FIELD partNo       AS CHARACTER LABEL "Cust Part"    FORMAT "X(15)"    
    FIELD procat       AS CHARACTER LABEL "FG Cat"       FORMAT "X(5)"         
    FIELD loc          AS CHARACTER LABEL "Whse"         FORMAT "X(5)"
    FIELD bin          AS CHARACTER LABEL "Bin"          FORMAT "X(8)" 
    FIELD jobNo        AS CHARACTER LABEL "Job"          FORMAT "x(10)"
    FIELD msfOnHand    AS DECIMAL   LABEL "MSF OH"       FORMAT "->>9.999" DECIMALS 3
    FIELD qtyOnHand    AS INTEGER   LABEL "Qty On Hand"  FORMAT "->>,>>>,>>9" 
    FIELD relQty       AS INTEGER   LABEL "Rel Qty"      FORMAT "->>,>>>,>>9"     
    FIELD sellPrice    AS DECIMAL   LABEL "FG Price"     FORMAT "->>>,>>9.99" 
    FIELD ordPr        AS DECIMAL   LABEL "Order Price"  FORMAT "->>>,>>9.99"     
    FIELD uomCost      AS DECIMAL   LABEL "UOM Cost"     FORMAT "->>>>>9.999" DECIMALS 3
    FIELD totCost      AS DECIMAL   LABEL "Total Cost"   FORMAT "->>>,>>9.99" 
    FIELD matCost      AS DECIMAL   LABEL "Mat Cost"     FORMAT "->>>,>>9.99" 
    FIELD labCost      AS DECIMAL   LABEL "Labor Cost"   FORMAT "->>>,>>9.99" 
    FIELD costUom      AS CHARACTER LABEL "C-UOM"        FORMAT "x(5)" 
    FIELD sellValueFg  AS DECIMAL   LABEL "Sell Val FG"  FORMAT "->>,>>>,>>9.99" 
    FIELD sellValueOrd AS DECIMAL   LABEL "Sell Val Ord" FORMAT "->>,>>>,>>9.99"  
    FIELD lastSale     AS DATE      LABEL "Last Sale"    FORMAT 99/99/9999
    FIELD viewPo       AS CHARACTER LABEL "View PO"      FORMAT "x(11)" 
    FIELD linePo       AS CHARACTER LABEL "Line PO"      FORMAT "x(10)" 
    FIELD relPo        AS CHARACTER LABEL "Rel PO"       FORMAT "x(11)"      
    FIELD daysOld      AS INTEGER   LABEL "Dats Oold"    FORMAT "->>>>>>9" 
    FIELD custNoOwned  AS CHARACTER LABEL "Cust Own"     FORMAT "X(8)" 
    FIELD setHeader    AS CHARACTER LABEL "Set Header"   FORMAT "X(15)" 
    FIELD qtyPerSet    AS INTEGER   LABEL "Qty Per Set"  FORMAT "->>,>>>,>>9" 
    FIELD recDate      AS DATE      LABEL "Rec Date"     FORMAT 99/99/9999
    FIELD xxSort       AS CHARACTER LABEL "Sort"         FORMAT "x(500)"
        INDEX sortBy IS PRIMARY rowType xxSort
        .

&SCOPED-DEFINE itemfg-index i-no job-no job-no2 loc loc-bin tag bin-cust-no
DEFINE NEW SHARED TEMP-TABLE tt-itemfg NO-UNDO
    FIELD row-id      AS ROWID
    FIELD i-no        LIKE itemfg.i-no
    FIELD i-name      LIKE itemfg.i-name
    FIELD cust-no     LIKE itemfg.cust-no
    FIELD part-no     LIKE itemfg.part-no
    FIELD part-cust   AS CHARACTER
    FIELD procat      LIKE itemfg.procat
    FIELD job-no      LIKE fg-rcpth.job-no
    FIELD job-no2     LIKE fg-rcpth.job-no2
    FIELD loc         LIKE fg-rdtlh.loc
    FIELD loc-bin     LIKE fg-rdtlh.loc-bin
    FIELD tag         LIKE fg-rdtlh.tag
    FIELD bin-cust-no LIKE fg-rdtlh.cust-no
    FIELD loc-bin-tag AS CHARACTER
        INDEX i-no                    {&itemfg-index}
        INDEX cust-no     cust-no     {&itemfg-index}
        INDEX partNo      part-cust   {&itemfg-index}
        INDEX procat      procat      {&itemfg-index}
        INDEX loc-bin-tag loc-bin-tag {&itemfg-index}
        .

{fg/rep/tt-fgbin.i NEW SHARED}
/* Inventory Value.rpa */

{sys/ref/CustList.i NEW}

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

&IF DEFINED(EXCLUDE-fInventoryValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fInventoryValue Procedure 
FUNCTION fInventoryValue RETURNS HANDLE
  ( ipcCompany AS CHARACTER,
    ipiBatch   AS INTEGER,
    ipcUserID  AS CHARACTER )  FORWARD.

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
                iPalletCount = (IF fg-bin.case-count EQ 0 THEN 1
                           ELSE fg-bin.case-count) *
                            (IF fg-bin.cases-unit    EQ 0 THEN 1
                           ELSE fg-bin.cases-unit) *
                            (IF fg-bin.units-pallet  EQ 0 THEN 1
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

&IF DEFINED(EXCLUDE-pInventoryValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInventoryValue Procedure 
PROCEDURE pInventoryValue :
/*------------------------------------------------------------------------------
  Purpose:     Inventory Value.rpa
  Parameters:  Company, Batch Seq, User ID
  Notes:       
------------------------------------------------------------------------------*/
    {aoaAppSrv/pInventoryValue.i}
    
    /* local variables */
    DEFINE VARIABLE lProcessRel      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lProcessLastSale AS LOGICAL   NO-UNDO.
    
    /* subject business logic */
    IF CAN-DO(cSelectedColumns,"Rel Qty") OR
       CAN-DO(cSelectedColumns,"Rel PO") THEN
    lProcessRel = YES.

    lProcessLastSale = CAN-DO(cSelectedColumns,"Last Sale").

    IF lCustList THEN DO:
        FIND FIRST ttCustList NO-LOCK
             WHERE ttCustList.log-fld USE-INDEX cust-No  
             NO-ERROR.
        IF AVAILABLE ttCustList THEN
        cStartCustNo = ttCustList.cust-no .
        FIND LAST ttCustList NO-LOCK
             WHERE ttCustList.log-fld USE-INDEX cust-No
             NO-ERROR.
        IF AVAILABLE ttCustList THEN 
        cEndCustNo = ttCustList.cust-no.
    END.

    EMPTY TEMP-TABLE tt-fg-bin.
    EMPTY TEMP-TABLE tt-itemfg.

    FOR EACH itemfg NO-LOCK
        WHERE itemfg.company EQ ipcCompany
          AND itemfg.cust-no GE cStartCustNo
          AND itemfg.cust-no LE cEndCustNo
          AND (IF lCustList THEN CAN-FIND(FIRST ttCustList WHERE ttCustList.cust-no EQ itemfg.cust-no
          AND ttCustList.log-fld NO-LOCK) ELSE TRUE)
          AND itemfg.i-no    GE cStartItemNo
          AND itemfg.i-no    LE cEndItemNo
          AND itemfg.procat  GE cStartProdCategory
          AND itemfg.procat  LE cEndProdCategory
          AND (itemfg.i-code EQ cItemCode OR cItemCode EQ "A")
          AND (itemfg.stat EQ "A" OR lIncludeInactiveItems)
          AND (NOT lPrintSetandComponentsOnly OR itemfg.isaset
           OR CAN-FIND(FIRST fg-set
                       WHERE fg-set.company EQ itemfg.company
                         AND fg-set.part-no EQ itemfg.i-no))
        USE-INDEX customer
        :
        RUN fg/rep/tt-fgbin.p
            (BUFFER itemfg,
             dtAsOfDate, "", "zzzzzzzzzz",
             cStartLoc, cEndLoc, cStartLocBin, cEndLocBin,
             lIncludeZeroBalance, iShowQOHOlderThanDays, YES,
             lIncludeCustomerOwnerdWarehouse).
        
        FOR EACH tt-fg-bin
            WHERE tt-fg-bin.company EQ itemfg.company
              AND tt-fg-bin.i-no    EQ itemfg.i-no
              AND (lIncludeCustomerOwnerdWarehouse OR lOnlyCustomerOwnedWarehouse
               OR (tt-fg-bin.cust-no EQ "" AND tt-fg-bin.loc NE "CUST"))
              AND (NOT lOnlyCustomerOwnedWarehouse
               OR (tt-fg-bin.cust-no NE "" OR tt-fg-bin.loc EQ "CUST"))
            USE-INDEX co-ino
            :
            IF tt-fg-bin.qty NE 0 OR lIncludeZeroBalance THEN DO:
                CREATE tt-itemfg.
                BUFFER-COPY itemfg TO tt-itemfg
                ASSIGN
                    tt-itemfg.row-id      = ROWID(itemfg) 
                    tt-itemfg.job-no      = tt-fg-bin.job-no
                    tt-itemfg.job-no2     = tt-fg-bin.job-no2
                    tt-itemfg.loc         = tt-fg-bin.loc
                    tt-itemfg.loc-bin     = tt-fg-bin.loc-bin
                    tt-itemfg.tag         = tt-fg-bin.tag
                    tt-itemfg.bin-cust-no = tt-fg-bin.cust-no
                    tt-itemfg.part-cust   = STRING(tt-itemfg.part-no,"x(20)")
                                          + STRING(tt-itemfg.cust-no,"x(20)") 
                    tt-itemfg.loc-bin-tag = STRING(tt-itemfg.loc,"x(10)")
                                          + STRING(tt-itemfg.loc-bin,"x(10)")
                                          + STRING(tt-itemfg.tag,"x(20)")
                    . 
            END. /* if qty ne 0 */
            ELSE DELETE tt-fg-bin.
        END. /* each tt-fg-bin */
    END. /* each itemfg */

    RUN pInventoryValue1
        (ipcCompany,
         cStartSalesRep,
         cEndSalesRep,
         cSort,
         lPrintSummaryByBinQty,
         lIncludeZeroBalance,
         dtAsOfDate,
         lProcessRel,
         lProcessLastSale
         ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pInventoryValue1) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInventoryValue1 Procedure 
PROCEDURE pInventoryValue1 :
/*------------------------------------------------------------------------------
  Purpose:     Inventory Value.rpa
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {aoaAppSrv/pInventoryValue1.i}

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
        /* Customer Inventory.rpa */
        WHEN "r-fgohbb." THEN
        RETURN TEMP-TABLE ttInventoryValue:HANDLE.
    END CASE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fInventoryValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fInventoryValue Procedure 
FUNCTION fInventoryValue RETURNS HANDLE
  ( ipcCompany AS CHARACTER,
    ipiBatch   AS INTEGER,
    ipcUserID  AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  Customer Inventory.rpa
    Notes:  
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttInventoryValue.

    RUN pInventoryValue (ipcCompany, ipiBatch, ipcUserID).

    RETURN TEMP-TABLE ttInventoryValue:HANDLE .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

