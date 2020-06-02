/*------------------------------------------------------------------------
    File        : CycleCountCompare.p
    Purpose     : 

    Syntax      :

    Description : Compares tagged on-hand inventory vs. what has been physically counted
                  Tagged inventory only (Sharp Shooter)

    Author(s)   : BV
    Created     : Sun Feb 17 18:28:35 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{methods/defines/hndldefs.i}
// {methods/prgsecur.i} 

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/VAR.i new shared}
cocode = gcompany.
IF cocode = "" THEN cocode = "001".

DEFINE TEMP-TABLE ttCycleCountCompare
    FIELD cCompany                   AS CHARACTER COLUMN-LABEL "Company" 
    FIELD cFGItemID                  AS CHARACTER COLUMN-LABEL "FG Item ID"
    FIELD cTag                       AS CHARACTER COLUMN-LABEL "Tag"
    FIELD cVendorTag                 AS CHARACTER COLUMN-LABEL "Vendor Tag"
    FIELD cSysLoc                    AS CHARACTER COLUMN-LABEL "System Warehouse"
    FIELD cSysLocBin                 AS CHARACTER COLUMN-LABEL "System Bin"
    FIELD cScanLoc                   AS CHARACTER COLUMN-LABEL "Scanned Warehouse"
    FIELD cScanLocBin                AS CHARACTER COLUMN-LABEL "Scanned Bin"
    FIELD dSysQty                    AS DECIMAL   COLUMN-LABEL "System Quantity"
    FIELD dScanQty                   AS DECIMAL   COLUMN-LABEL "Scanned Quantity"
    FIELD cAction                    AS CHARACTER COLUMN-LABEL "Action" 
    FIELD iCountOfScansForTagZero    AS INTEGER   COLUMN-LABEL "Count of Additional Scans for Tag - Zero"
    FIELD iCountOfScansForTagNonZero AS INTEGER   COLUMN-LABEL "Count of Additional Scans for Tag - Non-Zero"
    FIELD iCountOfBinsForTagNonZero  AS INTEGER   COLUMN-LABEL "Count Bins for Tag - Non-Zero"
    FIELD lLocationChanged           AS LOGICAL   COLUMN-LABEL "Location Changed?"
    FIELD lQuantityChanged           AS LOGICAL   COLUMN-LABEL "Quantity Changed?"
    FIELD lNotScanned                AS LOGICAL   COLUMN-LABEL "Not Scanned?"
    FIELD lMatch                     AS LOGICAL   COLUMN-LABEL "Complete Match?"
    FIELD dtReceiptDate              AS DATE      COLUMN-LABEL "Receipt Date"
    FIELD dtShipDate                 AS DATE      COLUMN-LABEL "Ship Date"
    FIELD dtScanDate                 AS DATETIME  COLUMN-LABEL "Scan DateTime"
    FIELD cScanUser                  AS CHARACTER COLUMN-LABEL "Scan User"
    FIELD iSequence                  AS INTEGER   COLUMN-LABEL "Seq #"
    FIELD cStatus                    AS CHARACTER COLUMN-LABEL "Status"
    FIELD cRecommendedAction         AS CHARACTER COLUMN-LABEL "Recommendation"
    FIELD dTotalTrans                AS DECIMAL   COLUMN-LABEL "Total Transaction Qty"
    FIELD dSnapQty                   AS DECIMAL   COLUMN-LABEL "Snapshot Qty"
    FIELD dSysMsf                    AS DECIMAL   COLUMN-LABEL "System MSF"
    FIELD dScanMsf                   AS DECIMAL   COLUMN-LABEL "Scan MSF"
    FIELD dSysCost                   AS DECIMAL   COLUMN-LABEL "System Cost"
    FIELD dScanCost                  AS DECIMAL   COLUMN-LABEL "Scanned Cost"
    FIELD cSysCostUom                AS CHARACTER COLUMN-LABEL "System Cost Uom"    
    FIELD cScanCostUom               AS CHARACTER COLUMN-LABEL "Scanned Cost Uom"
    FIELD dSysCostValue              AS DECIMAL   COLUMN-LABEL "System cost Value"
    FIELD dScanCostValue             AS DECIMAL   COLUMN-LABEL "Scan cost Value"
    FIELD cJobNo                     AS CHARACTER COLUMN-LABEL "Job#" 
    FIELD cJobNo2                    AS CHARACTER COLUMN-LABEL "Job#2"
    FIELD cSNum                      AS CHARACTER COLUMN-LABEL "Sheet#"
    FIELD cBNum                      AS CHARACTER COLUMN-LABEL "Blank#"
    FIELD cShtSize                   AS CHARACTER COLUMN-LABEL "Sheet Size"
    INDEX tag  cCompany cTag 
    INDEX item cCompany cFGItemID
    INDEX i3   cCompany cSysLoc   cSysLocBin   
    .
    
DEFINE TEMP-TABLE ttSnapShot
    FIELD cCompany                  AS CHARACTER COLUMN-LABEL "Company" 
    FIELD cFGItemID                 AS CHARACTER COLUMN-LABEL "FG Item ID"
    FIELD cTag                      AS CHARACTER COLUMN-LABEL "Tag"
    FIELD cSysLoc                   AS CHARACTER COLUMN-LABEL "System Warehouse"
    FIELD cSysLocBin                AS CHARACTER COLUMN-LABEL "System Bin"
    FIELD dSysQty                   AS DECIMAL   COLUMN-LABEL "System Quantity"
    FIELD iCountOfBinsForTagNonZero AS INTEGER   COLUMN-LABEL "Count Bins for Tag - Non-Zero"
    FIELD dtReceiptDate             AS DATE      COLUMN-LABEL "Receipt Date"
    FIELD dtShipDate                AS DATE      COLUMN-LABEL "Ship Date"
    FIELD dtScanDate                AS DATETIME  COLUMN-LABEL "Scan DateTime"
    FIELD cScanUser                 AS CHARACTER COLUMN-LABEL "Scan User"
    FIELD iSequence                 AS INTEGER   COLUMN-LABEL "Seq #"
    FIELD cStatus                   AS CHARACTER COLUMN-LABEL "Status"
    FIELD dCost                     AS DECIMAL   COLUMN-LABEL "Cost"
    FIELD cCostUom                  AS CHARACTER COLUMN-LABEL "Cost UOM"
    FIELD cJobNo                    AS CHARACTER COLUMN-LABEL "Job#" 
    FIELD cJobNo2                   AS CHARACTER COLUMN-LABEL "Job#2"
    FIELD cSNum                     AS CHARACTER COLUMN-LABEL "Sheet#"
    FIELD cBNum                     AS CHARACTER COLUMN-LABEL "Blank#"    
    INDEX tag  cCompany cTag
    INDEX ITEM cCompany cFGItemID   
    .
    
DEFINE TEMP-TABLE ttProblems
    FIELD i-no       LIKE fg-bin.i-no
    FIELD tag        LIKE fg-bin.tag
    FIELD loc1       LIKE fg-bin.loc
    FIELD i-no2      LIKE fg-bin.i-no 
    FIELD loc2       LIKE fg-bin.loc
    FIELD transTypes AS CHARACTER
    FIELD lInactiveWithBin AS LOGICAL 
    FIELD lNoitem AS LOGICAL
    FIELD lOnHandNobin AS LOGICAL     
    FIELD lNoCostFound AS LOGICAL 
    FIELD lNoMSFFound AS LOGICAL
    FIELD lDuplicateFound AS LOGICAL 
    .

    
DEF TEMP-TABLE ttToPost
  FIELD rRmRctd AS ROWID
  FIELD r-no    LIKE rm-rctd.r-no 
  .
      
DEFINE STREAM sOutput.
DEFINE STREAM sIn.

DEFINE VARIABLE gcOutputFile   AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcSnapshotFile AS CHARACTER NO-UNDO INIT "./custfiles/invSnapShotRM.csv".
DEFINE VARIABLE gcProblemsFile AS CHARACTER NO-UNDO INIT "c:\tmp\PhysInventoryProblems".

DEFINE NEW SHARED VARIABLE v-post-date    AS DATE      INITIAL TODAY.     
DEFINE NEW SHARED VARIABLE v-avgcost      AS LOGICAL.


/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fGetAction RETURNS CHARACTER 
    (ipcLocChanged AS LOGICAL, iplQtyChanged AS LOGICAL, iplNotScanned AS LOGICAL, iplMatch AS LOGICAL, ipdCntNonZero AS DECIMAL, 
    ipdCntZero AS DECIMAL, ipcLoc AS CHARACTER) FORWARD.

FUNCTION fSnapshotCreateDtTime RETURNS CHARACTER 
    (  ) FORWARD.


/* ***************************  Main Block  *************************** */

/* **********************  Internal Procedures  *********************** */

PROCEDURE exportSnapshot:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFGItemStart AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFGItemEnd AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcValidItemSelect AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFromCycleCode AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcToCycleCode AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcWhseList AS CHARACTER NO-UNDO.    
    DEFINE INPUT PARAMETER ipiSnapShotID AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipcSnapshotDesc AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE lBinDups     AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lNoCostMSF   AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lContinue    AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lInvalidItems AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cProblemTextFile AS CHARACTER NO-UNDO.
    DEFINE VARIABLE icnt       AS INT.
    oplSuccess = YES.
    gcProblemsFile = gcProblemsFile + USERID("ASI") + STRING(TIME) + ".csv".
    
    EMPTY TEMP-TABLE ttProblems.
    
    RUN pCheckBinDups (INPUT ipcCompany, INPUT ipcFGItemStart, ipcFGItemEnd, ipcWhseList, OUTPUT lBinDups ).    
         
    RUN pCheckInvalidItems (INPUT ipcCompany, INPUT ipcFGItemStart, ipcFGItemEnd, ipcWhseList, OUTPUT lNoCostMSF, OUTPUT lInvalidItems ).
 
    RUN pReportProblems.
    
    FIND FIRST ttProblems NO-ERROR. 
    IF AVAIL ttProblems THEN DO:
        MESSAGE "Some items have issues to resolve." SKIP 
            "Click OK to view a report."
            VIEW-AS ALERT-BOX.
       
        OS-COMMAND NO-WAIT START excel.exe VALUE(gcProblemsFile).
          
        MESSAGE "Continue with the snapshot anyway?" 
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO  UPDATE lChoice AS LOGICAL.
        IF NOT lChoice THEN DO:               
          oplSuccess = NO.
        RETURN.
    END.
    END.
    
    FOR EACH rm-bin NO-LOCK
        WHERE rm-bin.company EQ ipcCompany
        AND rm-bin.i-no GE ipcFGItemStart
        AND rm-bin.i-no LE ipcFGItemEnd
        AND lookup(rm-bin.loc, ipcWhseList) GT 0        
        AND rm-bin.qty NE 0
        AND rm-bin.tag NE ""
        ,
        FIRST ITEM NO-LOCK 
        WHERE ITEM.company EQ rm-bin.company
        AND ITEM.i-no      EQ rm-bin.i-no
        AND item.cc-code GE ipcFromCycleCode
        AND item.cc-code LE ipcToCycleCode
        AND (IF ipcValidItemSelect EQ "ValidOnly" THEN item.stat EQ "A" ELSE TRUE)
        :
        icnt = icnt + 1.
        /* Index is unique by stock id but we allow user to continue */
        FIND FIRST inventoryStockSnapshot EXCLUSIVE-LOCK
                    WHERE inventoryStockSnapshot.inventoryStockID EQ rm-bin.tag
                      AND inventoryStockSnapshot.inventorySnapshotID EQ ipiSnapShotID 
                    NO-ERROR.
        IF NOT AVAIL inventoryStockSnapshot THEN DO:
        CREATE inventoryStockSnapshot.
        ASSIGN                      
            inventoryStockSnapshot.inventoryStockID    = rm-bin.tag    
            inventoryStockSnapshot.company             = rm-bin.company        
            inventoryStockSnapshot.rmItemID            = ""      
            inventoryStockSnapshot.fgItemID            = rm-bin.i-no                        
            inventoryStockSnapshot.itemType            = "RM"                           
            inventoryStockSnapshot.warehouseID         = rm-bin.loc    
            inventoryStockSnapshot.locationID          = rm-bin.loc-bin
            inventoryStockSnapshot.zoneID              = ""     
            inventoryStockSnapshot.quantity            = 0    
            inventoryStockSnapshot.quantityOriginal    = rm-bin.qty        
            inventoryStockSnapshot.quantityUOM         = "EA"       
            inventoryStockSnapshot.costStandardPerUOM  = rm-bin.cost
            inventoryStockSnapshot.costUOM             = item.pur-uom
            inventoryStockSnapshot.createdTime         = DATETIME(TODAY, MTIME)        
            inventoryStockSnapshot.createdBy           = USERID("ASI")                
            inventoryStockSnapshot.inventorySnapshotID = ipiSnapShotID 
            .      
        END.
        
        FOR LAST rm-rdtlh NO-LOCK 
            WHERE rm-rdtlh.company EQ rm-bin.company 
            AND rm-rdtlh.tag EQ rm-bin.tag 
            AND rm-rdtlh.rita-code EQ "R",
            EACH rm-rcpth NO-LOCK 
            WHERE rm-rcpth.r-no EQ rm-rdtlh.r-no
            AND rm-rcpth.rita-code EQ rm-rdtlh.rita-code:

            ASSIGN
                inventoryStockSnapshot.jobID   = rm-rdtlh.job-no        
                inventoryStockSnapshot.jobID2  = rm-rdtlh.job-no2 
                inventoryStockSnapshot.formNo  = rm-rdtlh.s-num
                inventoryStockSnapshot.blankNo = rm-rdtlh.b-num
                .             
        END.            
        FIND FIRST inventoryStockSnapshot NO-LOCK NO-ERROR.
    END.
    oplSuccess = NO.
    FIND FIRST inventoryStockSnapshot EXCLUSIVE-LOCK 
        WHERE inventoryStockSnapshot.inventorySnapshotID EQ ipiSnapshotID
        NO-ERROR.
    IF AVAIL inventoryStockSnapshot THEN DO:
        oplSuccess = YES. 
        CREATE inventorySnapshot.
        ASSIGN
        inventorySnapshot.inventorySnapshotID         = ipiSnapshotID
        inventorySnapshot.snapshotType                = "PhysicalInventory"
        inventorySnapshot.itemType                    = "RM"
        inventorySnapshot.company                     = ipcCompany
        //warehouseID                                 = 
        //locationID                                  =
        inventorySnapshot.inventoryStockStatus        = "Open"
        inventorySnapshot.snapshotUser                = USERID("ASI")
        inventorySnapshot.snapshotTime                = DATETIME(today, mtime)
        inventorySnapshot.snapshotDesc                = ipcSnapshotDesc
        .
    END.
    FIND CURRENT inventorySnapshot NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE pBuildCompareTable PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Builds the compare temp-table based on parameters
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFGItemStart AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFGItemEnd AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFromCycleCode AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcToCycleCode AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcWhseList AS CHARACTER NO-UNDO.   
    DEFINE INPUT  PARAMETER ipcBinStart AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcBinEnd AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplScansOnly AS LOGICAL NO-UNDO.

    DEFINE BUFFER bf-rm-rctd FOR rm-rctd.
    DEFINE BUFFER bf-rm-bin  FOR rm-bin.

    DEFINE VARIABLE iCountBins  AS INTEGER NO-UNDO.
    DEFINE VARIABLE dMSF        AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dCalcQty    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dLFQty      AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dCost       AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dExtCost    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dShtWid     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dShtLen     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dTransQty   AS DECIMAL NO-UNDO.
    DEFINE VARIABLE iStatusCnt1 AS INTEGER NO-UNDO.
    DEFINE VARIABLE iStatusCnt2 AS INTEGER NO-UNDO.
        
    EMPTY TEMP-TABLE ttCycleCountCompare.

    FIND FIRST ce-ctrl WHERE ce-ctrl.company EQ ipcCompany NO-LOCK NO-ERROR.
    FOR EACH rm-rctd NO-LOCK 
        WHERE rm-rctd.company EQ ipcCompany
        AND rm-rctd.rita-code EQ "C"
        AND rm-rctd.tag NE ""
        AND rm-rctd.i-no GE ipcFGItemStart
        AND rm-rctd.i-no LE ipcFGItemEnd
        AND (LOOKUP(rm-rctd.loc, ipcWhseList) GT 0
             OR CAN-FIND(FIRST ttSnapShot WHERE ttSnapshot.cTag EQ rm-rctd.tag)
            )       
        AND rm-rctd.loc-bin GE ipcBinStart
        AND rm-rctd.loc-bin LE ipcBinEnd
        AND rm-rctd.qty NE 0
        AND CAN-FIND(FIRST ITEM NO-LOCK
                         WHERE ITEM.company EQ rm-rctd.company 
                           AND ITEM.i-no EQ rm-rctd.i-no
                           AND item.cc-code GE ipcFromCycleCode
                           AND item.cc-code LE ipcToCycleCode)
        :

        iStatusCnt1 = iStatusCnt1 + 1.
        IF iStatusCnt1 GT 99 THEN 
        DO:
            iStatusCnt2 = iStatusCnt2 + iStatusCnt1.
            iStatusCnt1 = 0.
                PROCESS EVENTS.
            STATUS DEFAULT "Build Compare " + STRING(iStatusCnt2).
                
        END.          
        
        /*Initial Create*/    
        FIND FIRST ttCycleCountCompare NO-LOCK /*Only one record per tag*/
            WHERE ttCycleCountCompare.cCompany EQ rm-rctd.company
            AND ttCycleCountCompare.cFGItemID EQ rm-rctd.i-no
            AND ttCycleCountCompare.cTag EQ rm-rctd.tag
            NO-ERROR.

        IF NOT AVAILABLE ttCycleCountCompare THEN 
        DO:
            CREATE ttCycleCountCompare.
            ASSIGN 
                ttCycleCountCompare.cCompany       = rm-rctd.company
                ttCycleCountCompare.cFGItemID      = rm-rctd.i-no
                ttCycleCountCompare.cTag           = rm-rctd.tag
                ttCycleCountCompare.cScanLoc       = rm-rctd.loc
                ttCycleCountCompare.cScanLocBin    = rm-rctd.loc-bin
                ttCycleCountCompare.dScanQty       = rm-rctd.qty
                ttCycleCountCompare.dScanCost      = rm-rctd.cost
                ttCycleCountCompare.dScanCostValue = rm-rctd.qty * rm-rctd.cost
                ttCycleCountCompare.iSequence      = rm-rctd.r-no
                ttCycleCountCompare.dtScanDate     = rm-rctd.enteredDT
                ttCycleCountCompare.cScanUser      = rm-rctd.enteredBy
                ttCycleCountCompare.cJobNo         = rm-rctd.job-no
                ttCycleCountCompare.cJobNo2        = STRING(rm-rctd.job-no2)
                ttCycleCountCompare.cSNum          = STRING(rm-rctd.s-num)
                ttCycleCountCompare.cBnum          = STRING(rm-rctd.b-num)
                .

        END.
    END. 
  
    /* Records for reporting of snapshot only */
    FOR EACH ttSnapshot
        WHERE ttSnapshot.cFGItemID GE ipcFgItemStart
        AND ttSnapshot.cFgItemID   LE ipcFgItemEnd
        AND lookup(ttSnapshot.cSysLoc, ipcWhseList) GT 0   
        AND ttSnapshot.cSysLocBin  GE ipcBinStart
        AND ttSnapshot.cSysLocBin  LE ipcBinEnd         
         AND CAN-FIND(FIRST ITEM NO-LOCK
                         WHERE ITEM.company EQ ttSnapshot.cCompany
                           AND ITEM.i-no EQ ttSnapshot.cFGItemID
                           AND item.cc-code GE ipcFromCycleCode
                           AND item.cc-code LE ipcToCycleCode)
        :
            
        FIND FIRST ttCycleCountCompare NO-LOCK /*Only one record per tag*/
            WHERE ttCycleCountCompare.cCompany EQ ttSnapshot.cCompany
            AND ttCycleCountCompare.cFGItemID EQ ttSnapshot.cFGItemID
            AND ttCycleCountCompare.cTag EQ ttSnapshot.cTag
            USE-INDEX tag 
            NO-ERROR.            
        IF NOT AVAILABLE ttCycleCountCompare THEN 
        DO:
            CREATE ttCycleCountCompare.
            ASSIGN 
                ttCycleCountCompare.cCompany         = ttSnapshot.cCompany
                ttCycleCountCompare.cFGItemID        = ttSnapshot.cFGItemID
                ttCycleCountCompare.cTag             = ttSnapshot.cTag
                ttCycleCountCompare.cSysLoc          = ttSnapshot.cSysLoc
                ttCycleCountCompare.cSysLocBin       = ttSnapshot.cSysLocBin
                ttCycleCountCompare.dSysQty          = ttSnapshot.dSysQty
                ttCycleCountCompare.iSequence        = ttSnapshot.iSequence
                ttCycleCountCompare.dtScanDate       = ttSnapshot.dtScanDate
                ttCycleCountCompare.cScanUser        = ttSnapshot.cScanUser
                ttCycleCountCompare.lNotScanned      = YES 
                ttCycleCountCompare.lQuantityChanged = YES      
                .
        END.
        
        dTransQty = 0.
        FOR EACH rm-rdtlh NO-LOCK 
            WHERE rm-rdtlh.rec_key GT fSnapshotCreateDtTime()                  
            AND rm-rdtlh.company EQ rm-bin.company 
            AND rm-rdtlh.tag EQ rm-bin.tag ,
            EACH rm-rcpth NO-LOCK 
            WHERE rm-rcpth.r-no EQ rm-rdtlh.r-no
            AND rm-rcpth.rita-code EQ rm-rdtlh.rita-code:
            CASE rm-rcpth.rita-code:
                WHEN "I" THEN 
                    dTransQty = dTransQty - rm-rdtlh.qty.
                WHEN "R"THEN 
                    dTransQty = dTransQty + rm-rdtlh.qty.
                WHEN "A" THEN 
                    dTransQty = dTransQty + rm-rdtlh.qty.
                OTHERWISE 
                dTransQty = dTransQty + rm-rdtlh.qty.
            END CASE.
        END.
        ASSIGN         
            ttCycleCountCompare.dSnapQty    = ttSnapShot.dSysQty 
            ttCycleCountCompare.dTotalTrans = dTransQty 
            .  
    END.
    
    FOR EACH ttCycleCountCompare
        WHERE ttCycleCountCompare.cFGItem GE ipcFGItemStart
        AND ttCycleCountCompare.cFgItem LE ipcFGItemEnd        
        :  
        /*Assess other scans for this tag*/
        FOR EACH bf-rm-rctd NO-LOCK 
            WHERE bf-rm-rctd.company EQ rm-rctd.company
            AND bf-rm-rctd.i-no EQ rm-rctd.i-no
            AND bf-rm-rctd.tag EQ rm-rctd.tag
            AND bf-rm-rctd.rita-code EQ "C"
            AND ROWID(bf-rm-rctd) NE ROWID(rm-rctd)
            :
            IF bf-rm-rctd.qty NE 0 THEN 
                ttCycleCountCompare.iCountofScansForTagNonZero = ttCycleCountCompare.iCountofScansForTagNonZero + 1.
            ELSE 
                ttCycleCountCompare.iCountofScansForTagZero = ttCycleCountCompare.iCountofScansForTagZero + 1.
        END. 
        
        /*Assess Existing System Bins for Scanned Tag*/
        FIND FIRST ttSnapshot NO-LOCK /*Only one record per tag*/
            WHERE ttSnapshot.cCompany EQ ttCycleCountCompare.cCompany
            AND ttSnapshot.cFGItemID EQ ttCycleCountCompare.cFGItemID
            AND ttSnapshot.cSysLoc  EQ ttCycleCountCompare.cScanLoc 
            AND ttSnapshot.cSysLocBin EQ ttCycleCountCompare.cScanLocBin
            AND ttSnapshot.cTag EQ ttCycleCountCompare.cTag
            USE-INDEX tag
            NO-ERROR.
            
        IF AVAILABLE ttSnapshot THEN 
        DO:
            ASSIGN 
                ttCycleCountCompare.cSysLoc    = ttSnapshot.cSysLoc
                ttCycleCountCompare.cSysLocBin = ttSnapshot.cSysLocBin
                ttCycleCountCompare.dSysQty    = ttSnapshot.dSysQty
                ttCycleCountCompare.lMatch     = (ttCycleCountCompare.dSysQty EQ ttCycleCountCompare.dScanQty
                                                    AND ttCycleCountCompare.cSysLoc EQ ttCycleCountCompare.cScanLoc
                                                    AND ttCycleCountCompare.cSysLocBin EQ ttCycleCountCompare.cScanLocBin )
                .

        END.   
        
        /*Count existing non-zero bins for tag*/
        iCountBins = 0.
        /*        FOR EACH bf-rm-bin NO-LOCK                                                              */
        /*            WHERE bf-rm-bin.company EQ rm-rctd.company                                          */
        /*            AND bf-rm-bin.i-no EQ rm-rctd.i-no                                                  */
        /*            AND bf-rm-bin.tag EQ rm-rctd.tag                                                    */
        /*            AND bf-rm-bin.qty NE 0                                                              */
        /*            :                                                                                   */
        /*            iCountBins = iCountBins + 1.                                                        */
        /*            IF NOT ttCycleCountCompare.lMatch THEN /*Get the last one found and count the rest*/*/
        /*                ASSIGN                                                                          */
        /*                    ttCycleCountCompare.cSysLoc    = bf-rm-bin.loc                              */
        /*                    ttCycleCountCompare.cSysLocBin = bf-rm-bin.loc-bin                          */
        /*                    ttCycleCountCompare.dSysQty    = bf-rm-bin.qty                              */
        /*                    .                                                                           */
        /*        END.                                                                                    */
        
        FOR EACH ttSnapshot NO-LOCK /*Only one record per tag*/
            WHERE ttSnapshot.cCompany EQ ttCycleCountCompare.cCompany
            AND ttSnapshot.cFGItemID EQ ttCycleCountCompare.cFGItemID
            AND ttSnapshot.cTag EQ ttCycleCountCompare.cTag
            AND ttSnapshot.dSysQty GE 0
            :            
            iCountBins = iCountBins + 1.
            IF NOT ttCycleCountCompare.lMatch THEN /*Get the last one found and count the rest*/
                ASSIGN
                    ttCycleCountCompare.cSysLoc    = ttSnapshot.cSysLoc
                    ttCycleCountCompare.cSysLocBin = ttSnapshot.cSysLocBin
                    ttCycleCountCompare.dSysQty    = ttSnapshot.dSysQty
                    .

        END.   
        
        ASSIGN 
            ttCycleCountCompare.lLocationChanged          = ttCycleCountCompare.cScanLocBin NE "" 
                                                            AND ttCycleCountCompare.cScanLoc NE ""
                                                            AND (ttCycleCountCompare.cScanLoc NE ttCycleCountCompare.cSysLoc 
                                                                  OR ttCycleCountCompare.cScanLocBin NE ttCycleCountCompare.cSysLocBin)
            ttCycleCountCompare.lQuantityChanged          = (ttCycleCountCompare.dScanQty NE ttCycleCountCompare.dSysQty
                                                              AND ttCycleCountCompare.dSysQty GT 0)
            ttCycleCountCompare.iCountOfBinsForTagNonZero = iCountBins
            .
        /*See if there are have been shipments for that tag after the scan was done*/
        IF ttCycleCountCompare.lQuantityChanged AND ttCycleCountCompare.dSysQty EQ 0 THEN 
        DO:
        /* RUN pGetLastTransDate(rm-rctd.company, rm-rctd.i-no, rm-rctd.tag, "S", OUTPUT ttCycleCountCompare.dtShipDate). */
        END.    
        
    END.
    
    IF NOT iplScansOnly THEN 
    DO: 
        FOR EACH rm-bin NO-LOCK
            WHERE rm-bin.company EQ ipcCompany
            AND rm-bin.i-no GE ipcFGItemStart
            AND rm-bin.i-no LE ipcFGItemEnd
            AND LOOKUP(rm-bin.loc, ipcWhseList) GT 0   
            AND rm-bin.qty NE 0
            AND rm-bin.tag NE ""
            AND CAN-FIND (FIRST ITEM NO-LOCK 
                        WHERE ITEM.company EQ rm-bin.company
                          AND ITEM.i-no    EQ rm-bin.i-no)
            :
            FIND FIRST ttCycleCountCompare NO-LOCK /*Only one record per tag*/
                WHERE ttCycleCountCompare.cCompany EQ rm-bin.company
                AND ttCycleCountCompare.cFGItemID EQ rm-bin.i-no
                AND ttCycleCountCompare.cTag EQ rm-bin.tag
                USE-INDEX tag
                NO-ERROR.
            IF NOT AVAILABLE ttCycleCountCompare THEN 
            DO:
                CREATE ttCycleCountCompare.
                ASSIGN 
                    ttCycleCountCompare.cCompany         = rm-bin.company
                    ttCycleCountCompare.cFGItemID        = rm-bin.i-no
                    ttCycleCountCompare.cTag             = rm-bin.tag
                    ttCycleCountCompare.cSysLoc          = rm-bin.loc
                    ttCycleCountCompare.cSysLocBin       = rm-bin.loc-bin
                    ttCycleCountCompare.dSysQty          = rm-bin.qty
                    ttCycleCountCompare.lNotScanned      = YES 
                    ttCycleCountCompare.lQuantityChanged = YES 
                    .
            END.
            FOR LAST rm-rdtlh NO-LOCK 
                WHERE rm-rdtlh.company EQ rm-bin.company 
                AND rm-rdtlh.tag EQ rm-bin.tag 
                AND rm-rdtlh.rita-code EQ "R",
                EACH rm-rcpth NO-LOCK 
                WHERE rm-rcpth.r-no EQ rm-rdtlh.r-no
                AND rm-rcpth.rita-code EQ rm-rdtlh.rita-code:
                ASSIGN 
                    ttCycleCountCompare.cJobNo2 = STRING(rm-rdtlh.job-no2)
                    ttCycleCountCompare.cSNum   = STRING(rm-rdtlh.s-num)
                    ttCycleCountCompare.cBNum   = STRING(rm-rdtlh.b-num)
                    .
            END.              
        /*Check for receipts after scan*/
        /* RUN pGetLastTransDate(rm-bin.company, rm-bin.i-no, rm-bin.tag, "R", OUTPUT ttCycleCountCompare.dtReceiptDate). */
                    
        END.
    END.
    
    /* Add other values */
    ASSIGN 
        iStatusCnt1 = 0
        iStatusCnt2 = 0
        .
    FOR EACH ttCycleCountCompare
        WHERE ttCycleCountCompare.cFGItem   GE ipcFGItemStart
        AND ttCycleCountCompare.cFgItem     LE ipcFGItemEnd
        AND IF ttCycleCountCompare.cScanLoc GT "" THEN 
        (ttCycleCountCompare.cScanLocBin  GE ipcBinStart
        AND ttCycleCountCompare.cScanLocBin  LE ipcBinEnd
        )
        ELSE 
        (LOOKUP(ttCycleCountCompare.cSysLoc, ipcWhseList) GT 0        
        AND ttCycleCountCompare.cSysLocBin  GE ipcBinStart
        AND ttCycleCountCompare.cSysLocBin  LE ipcBinEnd)
        :

        iStatusCnt1 = iStatusCnt1 + 1.
        IF iStatusCnt1 GT 99 THEN 
        DO:
            iStatusCnt2 = iStatusCnt2 + iStatusCnt1.
            iStatusCnt1 = 0.
            STATUS DEFAULT "Build Compare " + STRING(iStatusCnt2).
            PROCESS EVENTS.
        END.

        ttCycleCountCompare.cAction = fGetAction(
            ttCycleCountCompare.lLocationChanged, 
            (ttCycleCountCompare.lQuantityChanged OR ttCycleCountCompare.cSysLoc EQ ""), 
            ttCycleCountCompare.lNotScanned, 
            ttCycleCountCompare.lMatch,
            ttCycleCountCompare.iCountOfBinsForTagNonZero, 
            ttCycleCountCompare.iCountofScansForTagZero,
            ttCycleCountCompare.cSysLoc
            ).

        FIND FIRST item NO-LOCK 
            WHERE item.company EQ ttCycleCountCompare.cCompany 
            AND item.i-no EQ ttCycleCountCompare.cFgItemID
            NO-ERROR.

        IF NOT AVAILABLE ITEM THEN NEXT.
     
        /*Only one record per tag since duplicates were removed prior to snapshot */
        FIND FIRST rm-bin NO-LOCK 
            WHERE rm-bin.company EQ ttCycleCountCompare.cCompany 
            AND rm-bin.i-no EQ ttCycleCountCompare.cFGItemID  
            AND rm-bin.tag EQ ttCycleCountCompare.cTag  
            AND rm-bin.qty GT 0
            USE-INDEX tag NO-ERROR.
        IF NOT AVAILABLE rm-bin THEN 
            FIND FIRST rm-bin NO-LOCK 
                WHERE rm-bin.company EQ ttCycleCountCompare.cCompany 
                AND rm-bin.i-no EQ ttCycleCountCompare.cFGItemID  
                AND rm-bin.tag EQ ttCycleCountCompare.cTag  
                USE-INDEX tag NO-ERROR.
        ASSIGN  dCost = 0
                dMsf = 0
                .                
        IF AVAILABLE rm-bin THEN DO:
            RUN pGetCostMSF (INPUT ROWID(rm-bin), ttCycleCountCompare.dSysQty, OUTPUT dShtLen, OUTPUT dShtWid, OUTPUT dMSF, OUTPUT dCost).
            IF rm-bin.cost GT 0 THEN 
              dCost = rm-bin.cost.
        END.
            
        ASSIGN 
            ttCycleCountCompare.dSysMSF       = dMSF
            ttCycleCountCompare.cSysCostUom   = ITEM.pur-uom      
            ttCycleCountCompare.dSysCost      = dCost
            ttCycleCountCompare.dSysCostValue = dCost * ttCycleCountCompare.dSysQty
            .

        IF AVAILABLE rm-bin THEN 
            RUN pGetCostMSF (INPUT ROWID(rm-bin), ttCycleCountCompare.dScanQty, OUTPUT dShtLen, OUTPUT dShtWid, OUTPUT dMSF, OUTPUT dCost).
        

        ASSIGN 
            ttCycleCountCompare.dScanCost      = dCost 
            ttCycleCountCompare.cScanCostUom   = ITEM.pur-uom      
            ttCycleCountCompare.dScanCostValue = dCost * ttCycleCountCompare.dScanQty
            .
        
        IF NOT ttCycleCountCompare.lNotScanned THEN 
        DO:
            FIND FIRST rm-bin NO-LOCK /*Only one record per tag*/
                WHERE rm-bin.company EQ ttCycleCountCompare.cCompany 
                AND rm-bin.i-no EQ ttCycleCountCompare.cFGItemID  
                AND rm-bin.tag EQ ttCycleCountCompare.cTag  
                NO-ERROR.  
            IF AVAILABLE rm-bin THEN  
                FIND FIRST po-ordl  NO-LOCK WHERE po-ordl.company EQ rm-bin.company 
                    AND po-ordl.po-no EQ rm-bin.po-no
                    AND po-ordl.i-no EQ rm-bin.i-no
                    NO-ERROR.
        
            IF AVAILABLE po-ordl THEN                    
                ASSIGN 
                    ttCycleCountCompare.cJobNo  = IF po-ordl.job-no NE "" THEN STRING(po-ordl.job-no) + "-" + STRING(po-ordl.job-no2) ELSE ""
                    ttCycleCountCompare.cJobNo2 = STRING(po-ordl.job-no2)
                    ttCycleCountCompare.cSNum   = STRING(po-ordl.s-num)
                    ttCycleCountCompare.cBNum   = STRING(po-ordl.b-num )
                    .
        END.         


        ttCycleCountCompare.cShtSize = (TRIM(STRING(dShtLen,">,>>99.99")) + " X " + trim(STRING(dShtWid,">,>>99.99")) ).        
        FIND FIRST loadtag NO-LOCK 
            WHERE loadtag.company EQ ttCycleCountCompare.cCompany
            AND loadtag.item-type EQ TRUE 
            AND loadtag.tag-no EQ ttCycleCountCompare.cTag
            NO-ERROR.
        IF AVAILABLE loadtag THEN
            ttCycleCountCompare.cVendorTag = loadtag.misc-char[1].
        IF ttCycleCountCompare.cTag GT "" THEN DO:
            FOR each rm-rdtlh NO-LOCK 
                WHERE rm-rdtlh.company EQ ttCycleCountCompare.cCompany
                  AND rm-rdtlh.tag     EQ ttCycleCountCompare.cTag
                  AND rm-rdtlh.rita-code EQ "R"
                  USE-INDEX tag,
                EACH rm-rcpth NO-LOCK 
                    WHERE rm-rcpth.r-no EQ rm-rdtlh.r-no
                BY rm-rcpth.trans-date DESCENDING:                 
            
                    ttCycleCountCompare.dtReceiptDate = rm-rcpth.trans-date.
                    LEAVE.
            END.
         END.
         ELSE DO:
            FOR EACH rm-rcpth NO-LOCK 
                WHERE rm-rcpth.company EQ ttCycleCountCompare.cCompany
                  AND rm-rcpth.i-no     EQ ttCycleCountCompare.cFGItemID
                  AND rm-rcpth.rita-code EQ "R"                  
                  :

                    ttCycleCountCompare.dtReceiptDate = rm-rcpth.trans-date.
                    LEAVE.
            END.
         END.
    END. /* each ttCycleCountCompare */ 
    
END PROCEDURE.

PROCEDURE pCheckBinDups:
    /*------------------------------------------------------------------------------
     Purpose: Check Bin Duplicates
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFGItemStart AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFGItemEnd AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcWhseList AS CHARACTER NO-UNDO.        
    DEFINE OUTPUT PARAMETER oplNoDups AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lIsDups AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cDupOutputFile AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-rm-bin FOR rm-bin.

    EMPTY TEMP-TABLE ttProblems.
    lIsDups = NO.
    FOR EACH rm-bin NO-LOCK
        WHERE rm-bin.qty GT 0
        AND rm-bin.company EQ ipcCompany
        AND rm-bin.i-no GE ipcFGItemStart
        AND rm-bin.i-no LE ipcFGItemEnd
        AND lookup(rm-bin.loc, ipcWhseList) GT 0   
        AND rm-bin.tag GT "" 
        AND CAN-FIND (FIRST ITEM NO-LOCK 
                        WHERE ITEM.company EQ rm-bin.company
                          AND ITEM.i-no    EQ rm-bin.i-no)
        :
        FIND FIRST bf-rm-bin NO-LOCK
            WHERE bf-rm-bin.company EQ rm-bin.company
            AND bf-rm-bin.tag EQ rm-bin.tag
            AND ROWID(bf-rm-bin) NE ROWID(rm-bin)
            AND bf-rm-bin.qty GT 0 
            AND CAN-FIND (FIRST ITEM NO-LOCK 
                       WHERE ITEM.company EQ bf-rm-bin.company
                         AND ITEM.i-no    EQ bf-rm-bin.i-no)
            NO-ERROR.
        IF AVAILABLE bf-rm-bin THEN 
        DO:
            lIsDups = YES.
            CREATE ttProblems.
            ASSIGN 
                ttProblems.i-no = rm-bin.i-no
                ttProblems.tag  = rm-bin.tag
                ttProblems.loc1 = rm-bin.loc + " " + rm-bin.loc-bin
                ttProblems.i-no2 = bf-rm-bin.i-no                
                ttProblems.loc2 = bf-rm-bin.loc + " " + bf-rm-bin.loc-bin
                .
        END.
    END.
    

    oplNoDups = lIsDups.
    

END PROCEDURE.

PROCEDURE pCheckCountDups:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplDups AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lIsDups AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cDupOutputFile AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-rm-rctd FOR rm-rctd.

    EMPTY TEMP-TABLE ttProblems.
    lIsDups = FALSE. 
    FOR EACH rm-rctd NO-LOCK
        WHERE rm-rctd.company EQ cocode
          AND rm-rctd.rita-code EQ "C".
        FIND FIRST bf-rm-rctd NO-LOCK
            WHERE bf-rm-rctd.company EQ rm-rctd.company
              AND bf-rm-rctd.tag EQ rm-rctd.tag
              AND ROWID(bf-rm-rctd) NE ROWID(rm-rctd) 
              AND bf-rm-rctd.rita-code NE "P"
            USE-INDEX tag 
            NO-ERROR.
        IF AVAILABLE bf-rm-rctd THEN 
        DO:
            lIsDups = TRUE. 
            CREATE ttProblems.
            ASSIGN 
                ttProblems.i-no       = rm-rctd.i-no
                ttProblems.tag        = rm-rctd.tag
                ttProblems.transTypes = rm-rctd.rita-code + "," + bf-rm-rctd.rita-code
                .
        END.
    END.
    oplDups = lIsDups.
    cDupOutputFile = "c:\tmp\dupCountTagsRm.csv".
    IF lIsDups THEN 
    DO:
        OUTPUT STREAM sOutput TO VALUE(cDupOutputFile).
        PUT STREAM sOutput UNFORMATTED 
            "Item#,Tag#,Transaction Types Found" SKIP.
        FOR EACH ttProblems:
            PUT STREAM sOutput UNFORMATTED  
                '"' ttProblems.i-no '",'
                '="' ttProblems.tag '",'
                '"' ttProblems.transTypes '",'
                SKIP.              
        END.
        OUTPUT STREAM sOutput CLOSE.
    
        MESSAGE "Cannot post because some tags were counted more than once." SKIP 
            "Click OK to view duplicate tag records."
            VIEW-AS ALERT-BOX.
        OS-COMMAND NO-WAIT VALUE(cDupOutputFile).
    END.
END PROCEDURE.

PROCEDURE pCheckInvalidItems:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFGItemStart AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFGItemEnd AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcWhseList AS CHARACTER NO-UNDO.  
    DEFINE OUTPUT PARAMETER oplNoCostMSF AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplInvalidItems AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE lNoCost AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lNoMSF AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lInvalidItems AS LOGICAL NO-UNDO.
    DEFINE VARIABLE dShtLen AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dShtWid AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dMsf AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dCost AS DECIMAL NO-UNDO.
    DEFINE VARIABLE cProblemList AS CHARACTER NO-UNDO FORMAT "x(40)".
    
    ASSIGN 
        lNoCost = NO 
        lNoMSF = NO
        lInvalidItems = FALSE
        .
        
  
    FOR EACH rm-bin NO-LOCK
        WHERE rm-bin.qty GT 0
          AND rm-bin.tag GT "" 
          AND rm-bin.i-no GE ipcFgItemStart
          AND rm-bin.i-no LE ipcFgItemEnd
           AND LOOKUP(rm-bin.loc, ipcWhseList) GT 0
        :
        FIND FIRST ITEM no-lock
            WHERE ITEM.company EQ cocode 
              AND ITEM.i-no EQ rm-bin.i-no
            NO-ERROR.
        IF NOT AVAIL item OR (item.q-onh EQ 0 AND rm-bin.qty GT 0) OR (item.stat EQ "I" AND rm-bin.qty GT 0) THEN DO:
            CREATE ttProblems.             
            ASSIGN 
                ttProblems.loc1              = rm-bin.loc + rm-bin.loc-bin
                ttProblems.i-no             = rm-bin.i-no
                ttProblems.tag              = rm-bin.tag                
                // ttProblems.lInactiveWithBin = (AVAIL(item) AND item.stat EQ "I" AND rm-bin.qty GT 0)
                ttProblems.lNoitem          = NOT AVAIL(item)
                ttProblems.lOnHandNobin     = (AVAIL(item) AND item.q-onh EQ 0 AND rm-bin.qty GT 0) 
                lInvalidItems                   = TRUE     
                .
        END.       
        RUN pGetCostMSF (INPUT ROWID(rm-bin), rm-bin.qty, OUTPUT dShtLen, OUTPUT dShtWid, OUTPUT dMSF, OUTPUT dCost).
        IF dMsf EQ 0 THEN            
            lNoMSF = YES. 
        IF dCost EQ 0 THEN 
            lNoCost = YES.
            
          

        ASSIGN  oplNoCostMSF = lNoCost OR lNoMSF      
                oplInvalidItems = lInvalidItems          
                cProblemList = (IF lNoCost THEN "No Cost " ELSE "")
                                + (IF lNoCost AND lNoMSF THEN " AND " ELSE ",")
                                + (IF lNoMSF THEN "No MSF" else "")
                                .        
            IF lNoCost OR lNoMSF THEN DO:
                CREATE ttProblems.
                ASSIGN 
                    ttProblems.loc1             = rm-bin.loc + " " + rm-bin.loc-bin
                    ttProblems.i-no             = rm-bin.i-no
                    ttProblems.tag              = rm-bin.tag
                    ttProblems.lNoCostFound     = lNoCost
                    ttProblems.lNoMSFFound      = lNoMSF
                    lInvalidItems               = TRUE    
                    .
            END.
    END.
    
                  

END PROCEDURE.

PROCEDURE pCreateTransferCounts:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER ipdtTransDate AS DATE NO-UNDO.
    DEFINE VARIABLE iNextRno LIKE rm-rctd.r-no NO-UNDO.
    DEFINE BUFFER b-rm-rctd FOR rm-rctd.
    DEFINE VARIABLE dTransDate AS DATE      NO-UNDO.
    DEFINE VARIABLE iTransTime     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lv-tag     AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-rm-rctd FOR rm-rctd.
    ASSIGN 
        dTransDate = (IF ipdtTransDate EQ ? THEN TODAY ELSE ipdtTransDate)
        iTransTime = TIME. 

    /* Code placed here will execute PRIOR to standard behavior. */
    iNextRno = 0.

    FOR EACH ttCycleCountCompare NO-LOCK 
        WHERE lNotScanned = FALSE 
        AND (lLocationChanged OR ttCycleCountCompare.cSysLoc NE ttCycleCountCompare.cScanLoc
               OR ttCycleCountCompare.cSysLoc EQ "")
        :   
        FIND FIRST rm-bin NO-LOCK 
            WHERE rm-bin.company EQ  ttCycleCountCompare.cCompany
              AND rm-bin.i-no    EQ ttCycleCountCompare.cFGItemID   
              AND rm-bin.tag     EQ ttCycleCountCompare.cTag        
              AND rm-bin.loc     EQ  ttCycleCountCompare.cSysLoc    
              AND rm-bin.loc-bin EQ  ttCycleCountCompare.cSysLocBin    
              NO-ERROR.
        /* In case the inventory moved from another location */
        IF NOT AVAIL rm-bin THEN 
        FIND FIRST rm-bin NO-LOCK 
            WHERE rm-bin.company EQ  ttCycleCountCompare.cCompany
              AND rm-bin.i-no    EQ ttCycleCountCompare.cFGItemID   
              AND rm-bin.tag     EQ ttCycleCountCompare.cTag           
              AND rm-bin.qty     GT 0
              NO-ERROR.        
        IF NOT AVAIL rm-bin THEN 
            NEXT.      
        RUN sys/ref/asiseq.p (INPUT rm-bin.company, INPUT "rm_rcpt_seq", OUTPUT iNextRNo) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            MESSAGE "Could not obtain next sequence #, please contact ASI: " RETURN-VALUE
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.              
        /* Finding a count record from within the past week on assumption it will    */
        /* be part of the current physical                                           */
        FIND FIRST bf-rm-rctd EXCLUSIVE-LOCK
            WHERE bf-rm-rctd.company EQ  ttCycleCountCompare.cCompany
            AND bf-rm-rctd.i-no    EQ ttCycleCountCompare.cFGItemID   
            AND bf-rm-rctd.tag     EQ ttCycleCountCompare.cTag        
            AND bf-rm-rctd.loc     EQ  ttCycleCountCompare.cScanLoc    
            AND bf-rm-rctd.loc-bin EQ  ttCycleCountCompare.cScanLocBin
            AND bf-rm-rctd.rita-code EQ "C"
            AND bf-rm-rctd.rct-date GE TODAY - 7
            NO-ERROR.    
        IF AVAIL bf-rm-rctd THEN DO:
             IF ipdtTransDate NE ? THEN
                bf-rm-rctd.rct-date = ipdtTransDate.
 
        END.
        FIND CURRENT bf-rm-rctd NO-LOCK NO-ERROR. 
        /* ttCycleCountCompare.cSysLoc/bin is the original location of the tag, so 0 that out */
        FIND FIRST item WHERE item.company = rm-bin.company
            AND item.i-no = rm-bin.i-no NO-LOCK NO-ERROR.
        CREATE rm-rctd.
        ASSIGN 
            rm-rctd.r-no       = iNextRno
            rm-rctd.loc        = rm-bin.loc
            rm-rctd.loc-bin    = rm-bin.loc-bin
            rm-rctd.company    = rm-bin.company
            rm-rctd.rita-code  = "C"
            rm-rctd.s-num      = 0
            rm-rctd.rct-date   = dTransDate
            rm-rctd.trans-time = TIME
            rm-rctd.qty        = 0
            rm-rctd.i-no       = rm-bin.i-no
            rm-rctd.i-name     = item.i-name
            rm-rctd.tag        = rm-bin.tag
            rm-rctd.po-no      = STRING(rm-bin.po-no)
            lv-tag             = rm-bin.tag
            rm-rctd.cost       = (IF AVAIL bf-rm-rctd THEN bf-rm-rctd.cost ELSE 0)
            .


        IF rm-rctd.pur-uom = "" THEN
            rm-rctd.pur-uom = rm-rctd.cost-uom.

        IF rm-rctd.cost     EQ ? THEN rm-rctd.cost = 0.
        
        /* from addon/rm/b-trans.w */
        ASSIGN 
            rm-rctd.user-id  = USERID("nosweat")
            rm-rctd.upd-date = TODAY
            rm-rctd.upd-time = TIME
            rm-rctd.enteredBy = (IF AVAIL bf-rm-rctd THEN bf-rm-rctd.enteredBy ELSE userid("nosweat"))
            rm-rctd.enteredDT = (IF AVAIL bf-rm-rctd THEN bf-rm-rctd.enteredDT ELSE DATETIME(TODAY, MTIME))
            .
        IF AVAILABLE ITEM THEN 
        DO:
            rm-rctd.pur-uom = ITEM.cons-uom.
            RELEASE ITEM.
        END.  
        CREATE ttToPost.
        ASSIGN ttToPost.rRmRctd = ROWID(rm-rctd)
               ttToPost.r-no    = rm-rctd.r-no
               . 
        FIND FIRST rm-rdtlh WHERE
            rm-rdtlh.company = rm-bin.company AND
            rm-rdtlh.tag = lv-tag AND
            rm-rdtlh.rita-code = "R"
            USE-INDEX tag
            NO-LOCK NO-ERROR.

        IF AVAILABLE rm-rdtlh THEN 
        DO:
            FIND FIRST rm-rcpth OF rm-rdtlh NO-LOCK NO-ERROR.

            IF AVAILABLE rm-rcpth THEN 
            DO:
                IF rm-rcpth.po-no GT "" THEN 
                ASSIGN                     
                    rm-rctd.po-no   = rm-rcpth.po-no.
                ASSIGN 
                    rm-rctd.po-line = MAX(rm-rcpth.po-line, 1)
                    rm-rctd.job-no  = rm-rcpth.job-no
                    rm-rctd.job-no2 = rm-rcpth.job-no2.
                RELEASE rm-rcpth.
            END.
            
            RELEASE rm-rdtlh.
        END.      
        

    END.  /* for each rm-bin*/

END PROCEDURE.

PROCEDURE pCreateZeroCount:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/        
    DEFINE INPUT  PARAMETER ipdtTransDate AS DATE NO-UNDO.
    DEFINE VARIABLE iNextRNo LIKE rm-rctd.r-no NO-UNDO.
    DEFINE BUFFER b-rm-rctd FOR rm-rctd.
        
    DEFINE VARIABLE dTransDate AS DATE      NO-UNDO.
    DEFINE VARIABLE cTag       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cIno       AS CHARACTER NO-UNDO.
    dTransDate = (IF ipdtTransDate EQ ? THEN TODAY ELSE ipdtTransDate ).

    iNextRNo = 0.
     
    FOR EACH ttCycleCountCompare NO-LOCK 
        WHERE lNotScanned = TRUE,    
        FIRST rm-bin NO-LOCK 
        WHERE /* rm-bin.r-no   EQ ttCycleCountCompare.iSequence    
        AND */ rm-bin.company EQ  ttCycleCountCompare.cCompany
        AND rm-bin.i-no    EQ ttCycleCountCompare.cFGItemID   
        AND rm-bin.tag     EQ ttCycleCountCompare.cTag        
        AND rm-bin.loc     EQ ttCycleCountCompare.cSysLoc    
        AND rm-bin.loc-bin EQ ttCycleCountCompare.cSysLocBin    
        AND rm-bin.qty     NE 0
        :
            
        RUN sys/ref/asiseq.p (INPUT rm-bin.company, INPUT "rm_rcpt_seq", OUTPUT iNextRNo) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            MESSAGE "Could not obtain next sequence #, please contact ASI: " RETURN-VALUE
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.     
        FIND FIRST item WHERE item.company = rm-bin.company
            AND item.i-no = rm-bin.i-no NO-LOCK NO-ERROR.
        IF NOT AVAILABLE ITEM THEN 
            NEXT.
        CREATE rm-rctd.
        ASSIGN 
            rm-rctd.r-no       = iNextRNo
            rm-rctd.loc        = rm-bin.loc
            rm-rctd.loc-bin    = rm-bin.loc-bin
            rm-rctd.company    = rm-bin.company
            rm-rctd.rita-code  = "C"
            rm-rctd.s-num      = 0
            rm-rctd.rct-date   = dTransDate
            rm-rctd.trans-time = TIME
            rm-rctd.i-no       = rm-bin.i-no
            rm-rctd.i-name     = item.i-name
            rm-rctd.tag        = rm-bin.tag
            cTag               = rm-bin.tag
            rm-rctd.enteredBy  = "Not Scanned"
            .
        
        IF rm-rctd.cost     EQ ? THEN rm-rctd.cost = 0.
          
        /* from addon/rm/b-trans.w */
        ASSIGN 
            rm-rctd.user-id  = USERID("nosweat")
            rm-rctd.upd-date = TODAY
            rm-rctd.upd-time = TIME.
            
        CREATE ttToPost.
        ASSIGN ttToPost.rRmRctd = ROWID(rm-rctd)
               ttToPost.r-no    = rm-rctd.r-no
               .

      
        IF AVAILABLE ITEM THEN 
        DO:
            rm-rctd.pur-uom = ITEM.cons-uom.
            RELEASE ITEM.
        END.  

        FIND FIRST rm-rdtlh WHERE
            rm-rdtlh.company = rm-bin.company AND
            rm-rdtlh.tag = cTag AND
            rm-rdtlh.rita-code = "R"
            USE-INDEX tag
            NO-LOCK NO-ERROR.

        IF AVAILABLE rm-rdtlh THEN 
        DO:

            FIND FIRST rm-rcpth OF rm-rdtlh NO-LOCK NO-ERROR.

            IF AVAILABLE rm-rcpth THEN 
            DO:

                ASSIGN 
                    rm-rctd.po-no   = rm-rcpth.po-no
                    rm-rctd.po-line = MAX(rm-rcpth.po-line, 1) /* verify this, make sure scan does this */
                    rm-rctd.job-no  = rm-rcpth.job-no
                    rm-rctd.job-no2 = rm-rcpth.job-no2.
                RELEASE rm-rcpth.
            END.
            
            RELEASE rm-rdtlh.
        END.      
     
  
    END.  /* for each rm-bin*/


END PROCEDURE.

PROCEDURE pExportTempTable PRIVATE: 
    /*------------------------------------------------------------------------------ 
     Purpose: Exports the contents of any temp-table into CSV    
     Notes: 
    ------------------------------------------------------------------------------*/ 
    DEFINE INPUT  PARAMETER iphTT AS HANDLE NO-UNDO. 
    DEFINE INPUT  PARAMETER ipcFileName AS CHARACTER NO-UNDO. 
    DEFINE INPUT  PARAMETER iplHeader AS LOGICAL NO-UNDO.
    DEFINE INPUT  PARAMETER iplComplete AS LOGICAL NO-UNDO.
    DEFINE INPUT  PARAMETER iplQtyChanged AS LOGICAL NO-UNDO.
    DEFINE INPUT  PARAMETER iplSnapshotOnly AS LOGICAL NO-UNDO.
    DEFINE INPUT  PARAMETER iplLocChanged AS LOGICAL NO-UNDO.
    DEFINE INPUT  PARAMETER iplDupsInScan AS LOGICAL NO-UNDO.  
    
    DEFINE VARIABLE hQuery       AS HANDLE    NO-UNDO. 
    DEFINE VARIABLE iIndex       AS INTEGER   NO-UNDO. 
    DEFINE VARIABLE cTTName      AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE cQuery       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTrueOrFalse AS CHARACTER NO-UNDO.
    
    ASSIGN 
        cTTName      = iphTT:NAME
        cTrueOrFalse = IF (iplComplete OR iplQtyChanged OR iplSnapshotOnly OR iplLocChanged OR iplDupsInScan) THEN "FALSE" ELSE "TRUE"
        cQuery  = "FOR EACH " + cTTName + " WHERE " + cTrueOrFalse + " " 
        . 
    IF iplComplete       THEN cQuery = cQuery + "OR " + cTTName + ".lMatch "           + " = TRUE ".
    IF iplQtyChanged     THEN cQuery = cQuery + "OR " + cTTName + ".lQuantityChanged " + " = TRUE ".    
    IF iplSnapshotOnly   THEN cQuery = cQuery + "OR " + cTTName + ".lNotScanned "      + " = TRUE ".
    IF iplLocChanged     THEN cQuery = cQuery + "OR " + cTTName + ".lLocationChanged " + " = TRUE ".
    IF iplDupsInScan     THEN cQuery = cQuery + "OR " + cTTName + ".iCountOfScansForTagNonZero  " + " > 1 ".
    
    IF iplHeader THEN 
    DO:
        OUTPUT STREAM sOutput to VALUE(ipcFileName). 
        DO iIndex = 1 TO iphTT:DEFAULT-BUFFER-HANDLE:NUM-FIELDS: 
            PUT STREAM sOutput UNFORMATTED iphTT:DEFAULT-BUFFER-HANDLE:buffer-field(iIndex):COLUMN-LABEL + ",". 
        END. 
        PUT STREAM sOutput UNFORMATTED SKIP. 
    END.
    ELSE 
        OUTPUT STREAM sOutput to VALUE(ipcFileName) APPEND. 
        
    CREATE QUERY hQuery. 
    hQuery:SET-BUFFERS (iphTT:DEFAULT-BUFFER-HANDLE). 
    hQuery:QUERY-PREPARE(cQuery). 
    hQuery:QUERY-OPEN().
    REPEAT:   
        hQuery:GET-NEXT().   
        IF hQuery:QUERY-OFF-END THEN LEAVE.   
        DO iIndex = 1 TO iphTT:DEFAULT-BUFFER-HANDLE:NUM-FIELDS: 
            /* Was inserting an = sign for tag but that stopped working - new excel version? */
            IF iphTT:DEFAULT-BUFFER-HANDLE:buffer-field(iIndex):COLUMN-LABEL BEGINS  "Tag" THEN 
                PUT STREAM sOutput UNFORMATTED  
                    '="' iphTT:DEFAULT-BUFFER-HANDLE:buffer-field(iIndex):buffer-value '",'.             
            ELSE 
                PUT STREAM sOutput UNFORMATTED  
                    '"' iphTT:DEFAULT-BUFFER-HANDLE:buffer-field(iIndex):buffer-value '",'. 
        END. 
        PUT STREAM sOutput UNFORMATTED SKIP. 
    END. 
    OUTPUT STREAM sOutput CLOSE.


END PROCEDURE.

PROCEDURE pGetCostMSF:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprBinRow AS ROWID NO-UNDO.
    DEFINE INPUT  PARAMETER ipdQty AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdLen AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdWid AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdMSF AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCost AS DECIMAL NO-UNDO.
    
    DEFINE VARIABLE dMSF     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dCalcQty AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dLFQty   AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dCost    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dExtCost AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dShtWid  AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dShtLen  AS DECIMAL NO-UNDO.
    DEFINE VARIABLE iPoNum   AS INTEGER NO-UNDO.
    DEFINE VARIABLE iPoLine  AS INTEGER NO-UNDO.
    def    var      cJobNo   as char.
    def    var      cJobNo2  as char.
    def    var      cSnum    as char.
    def    var      cBnum    as char. 
    
    FIND FIRST rm-bin NO-LOCK
        WHERE ROWID(rm-bin) EQ iprBinRow
        NO-ERROR.  
    IF NOT AVAILABLE rm-bin THEN 
        RETURN.    
  
    FIND FIRST item NO-LOCK 
        WHERE item.company EQ rm-bin.company 
        AND item.i-no EQ rm-bin.i-no
        NO-ERROR.

    IF NOT AVAILABLE ITEM THEN RETURN.

    IF TRIM(rm-bin.tag) EQ "" THEN
        FOR EACH rm-rcpth NO-LOCK
            WHERE rm-rcpth.company      EQ rm-bin.company
            AND rm-rcpth.i-no         EQ rm-bin.i-no
            AND rm-rcpth.rita-code    NE "S"
            USE-INDEX i-no,

            EACH rm-rdtlh NO-LOCK
            WHERE rm-rdtlh.r-no         EQ rm-rcpth.r-no
            AND rm-rdtlh.rita-code    EQ rm-rcpth.rita-code
            AND rm-rdtlh.loc          EQ rm-bin.loc
            AND rm-rdtlh.loc-bin      EQ rm-bin.loc-bin
            AND rm-rdtlh.tag          EQ rm-bin.tag
            USE-INDEX rm-rdtl
    
            BY rm-rcpth.trans-date
            BY rm-rcpth.r-no:

            IF rm-rcpth.po-no NE "" THEN
                ASSIGN
                    iPoNum  = INTEGER(rm-rcpth.po-no )
                    iPoLine = rm-rcpth.po-line .
            LEAVE.
        END.

    ELSE
        FOR EACH rm-rdtlh NO-LOCK
            WHERE rm-rdtlh.company      EQ rm-bin.company
            AND rm-rdtlh.loc          EQ rm-bin.loc
            AND rm-rdtlh.loc-bin      EQ rm-bin.loc-bin
            AND rm-rdtlh.tag          EQ rm-bin.tag
            AND rm-rdtlh.rita-code    NE "S"
            USE-INDEX tag,
        
            EACH rm-rcpth NO-LOCK 
            WHERE rm-rcpth.r-no         EQ rm-rdtlh.r-no
            AND rm-rcpth.rita-code    EQ rm-rdtlh.rita-code
            AND rm-rcpth.i-no         EQ ITEM.i-no
            USE-INDEX r-no
    
            BY rm-rcpth.trans-date
            BY rm-rcpth.r-no:

            IF rm-rcpth.po-no NE "" THEN
                ASSIGN
                    iPoNum  = INTEGER(rm-rcpth.po-no )
                    iPoLine = rm-rcpth.po-line .
            LEAVE.
        END.

    dCalcQty = ipdQty.
    dLFQty = dCalcQty.

    IF ITEM.cons-uom NE "LF" THEN
        RUN sys/ref/convquom.p(ITEM.cons-uom, "LF", ITEM.basis-w,
            (IF ITEM.r-wid EQ 0 THEN ITEM.s-len
            ELSE 12),
            (IF ITEM.r-wid EQ 0 THEN ITEM.s-wid
            ELSE ITEM.r-wid),
            ITEM.s-dep,                    
            dCalcQty, OUTPUT dLFQty).
    ELSE
        dLFQty = dCalcQty.   
                             
    IF AVAILABLE ITEM THEN 
        dMSF = IF item.r-wid GT 0 THEN dLFQty * ITEM.r-wid / 12 / 1000
        ELSE dCalcQty * ITEM.s-wid * ITEM.s-len / 144 / 1000.
              
     
    FIND FIRST ce-ctrl WHERE ce-ctrl.company EQ cocode NO-LOCK NO-ERROR.
        
    /* System Cost */
    dCost = IF ce-ctrl.r-cost THEN ITEM.avg-cost ELSE rm-bin.cost.
    IF dCost EQ ? THEN dCost = 0.
    IF dCost EQ 0 AND ce-ctrl.r-cost EQ NO THEN 
    DO:          
        IF AVAILABLE rm-bin THEN 
            dCost = rm-bin.cost.
    END.

    ASSIGN 
        dShtWid = 0  
        dShtLen = 0          
        .
       
    IF ITEM.i-code EQ "R" THEN 
    DO:
        IF item.industry = "1" THEN
            ASSIGN
                dShtWid = ITEM.case-w 
                dShtLen = ITEM.case-l
                . 
        ELSE
            ASSIGN
                dShtWid = ITEM.s-wid 
                dShtLen = ITEM.s-len
                .
    END.
    ELSE 
    DO:             

        FIND FIRST po-ordl  NO-LOCK WHERE po-ordl.company EQ rm-bin.company 
            AND po-ordl.po-no EQ rm-bin.po-no
            AND po-ordl.i-no EQ rm-bin.i-no
            AND (po-ordl.line = iPoLine OR iPOLine EQ 0)
            NO-ERROR
            .

        IF AVAILABLE po-ordl THEN                    
            ASSIGN 
                cJobNo  = IF po-ordl.job-no NE "" THEN STRING(po-ordl.job-no) + "-" + STRING(po-ordl.job-no2) ELSE ""
                cJobNo2 = STRING(po-ordl.job-no2)
                cSNum   = STRING(po-ordl.s-num)
                cBNum   = STRING(po-ordl.b-num )
                .
        FIND FIRST job-mat NO-LOCK
            WHERE job-mat.company EQ rm-bin.company
            AND job-mat.job-no  EQ SUBSTRING(cJobNo,1,6)
            AND job-mat.job-no2 EQ INTEGER(cJobNo2)
            AND job-mat.i-no EQ ITEM.i-no
            AND job-mat.frm EQ INTEGER(cSNum)
            AND job-mat.blank-no EQ INTEGER(cBNum) 
            NO-ERROR 
            .
        IF AVAILABLE job-mat THEN
            ASSIGN
                dShtWid = job-mat.wid 
                dShtLen = job-mat.len
                .
    END. /* If not Real */
    if dMSF eq 0 then 
        dMSF =  dCalcQty * dShtWid * dShtLen / 144 / 1000.
    ASSIGN 
        opdCost = dCost
        opdLen  = dShtLen
        opdWid  = dShtWid
        opdMSF  = dMSF
        .

END PROCEDURE.

PROCEDURE pGetLastTransDate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Finds a transaction given inputs and returns the date of the latest 
     transaction for given type
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFGItemID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcTag AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcRita AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdtLastDate AS DATE NO-UNDO.

    FOR EACH fg-rcpth NO-LOCK 
        WHERE fg-rcpth.company EQ ipcCompany
        AND fg-rcpth.i-no EQ ipcFGItemID
        AND fg-rcpth.rita-code EQ ipcRita,
        EACH fg-rdtlh NO-LOCK 
        WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no
        AND fg-rdtlh.company EQ fg-rcpth.company
        AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
        AND fg-rdtlh.tag EQ ipcTag
        BY fg-rcpth.trans-date DESCENDING :
        opdtLastDate = fg-rcpth.trans-date.
        LEAVE.
    END.
END PROCEDURE.

PROCEDURE pImportSnapShot PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipiSnapShotID  LIKE inventoryStockSnapshot.inventorySnapshotID NO-UNDO.
    
    FOR EACH inventoryStockSnapshot NO-LOCK 
        WHERE inventoryStockSnapshot.inventorySnapshotID = ipiSnapShotID 
        :
        CREATE ttSnapshot.
        ASSIGN 
            ttSnapShot.cCompany   = inventoryStockSnapshot.company
            ttSnapShot.cFGItemID  = inventoryStockSnapshot.fgItemID
            ttSnapShot.cTag       = inventoryStockSnapshot.inventoryStockID
            ttSnapShot.cSysLoc    = inventoryStockSnapshot.warehouseID
            ttSnapShot.cSysLocBin = inventoryStockSnapshot.locationID
            ttSnapShot.dSysQty    = inventoryStockSnapshot.quantityOriginal
            ttSnapShot.dCost      = inventoryStockSnapshot.costStandardPerUOM
            ttSnapShot.cCostUom   = inventoryStockSnapshot.costUOM
            ttSnapShot.cJobNo     = inventoryStockSnapshot.jobID
            ttSnapShot.cJobNo2    = STRING(inventoryStockSnapshot.jobID2)
            .
    END.

END PROCEDURE.

PROCEDURE postRM:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdtTransDate AS DATE NO-UNDO.
    DEFINE INPUT  PARAMETER ipiTransTime AS INTEGER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFGItemStart AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFGItemEnd AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFromCycleCode AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcToCycleCode AS CHARACTER NO-UNDO.    
    DEFINE INPUT  PARAMETER ipcWhseList AS CHARACTER NO-UNDO.    
    DEFINE INPUT  PARAMETER ipcBinStart AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcBinEnd AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplSkipUnscanned AS LOGICAL NO-UNDO.
        
    DEFINE VARIABLE lDupsExist  AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lRemoveZero AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lPostWithDups AS LOGICAL NO-UNDO.
    
    MESSAGE 'Remove all zero counts (all locations)?' SKIP
        VIEW-AS ALERT-BOX
        QUESTION BUTTONS YES-NO UPDATE lRemoveZero.
    IF lRemoveZero THEN 
        RUN pRemoveZeroCounts .

    RUN pCheckCountDups (OUTPUT lDupsExist).
    IF lDupsExist THEN 
    DO:
        MESSAGE 'Continue posting with duplicates?' SKIP
            VIEW-AS ALERT-BOX
            QUESTION BUTTONS YES-NO UPDATE lPostWithDups.
        IF NOT lPostWithDups THEN 
        RETURN.
    END.
    IF NOT iplSkipUnscanned THEN 
        RUN pCreateZeroCount (ipdtTransDate).
    RUN pCreateTransferCounts (ipdtTransDate).
    
    RUN pRemoveMatches (ipcCompany, ipcFGItemStart, ipcFGItemEnd, ipcWhseList, 
        ipcBinStart, ipcBinEnd).
        
    RUN pPostCounts (ipcCompany, ipdtTransDate, ipiTransTime, ipcFGItemStart, ipcFGItemEnd,  ipcFromCycleCode, ipcToCycleCode, ipcWhseList, 
        ipcBinStart, ipcBinEnd).
    MESSAGE "Posting Complete"
        VIEW-AS ALERT-BOX.
END PROCEDURE.

PROCEDURE pPostCounts:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdtTransDate AS DATE NO-UNDO.
    DEFINE INPUT  PARAMETER ipiTransTime AS INTEGER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFGItemStart AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFGItemEnd AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFromCycleCode AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcToCycleCode AS CHARACTER NO-UNDO.    
    DEFINE INPUT  PARAMETER ipcWhseList AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcBinStart AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcBinEnd AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER b-rm-bin FOR rm-bin.

    DEFINE VARIABLE v-temp-cost AS DECIMAL FORMAT ">>>>>9.99".
    DEFINE VARIABLE v-dunne     AS LOG     INITIAL NO.
    DEFINE VARIABLE next_r-no   LIKE rm-rcpth.r-no.
    DEFINE VARIABLE v_r-no      LIKE rm-rcpth.r-no.
    DEFINE VARIABLE v-r-qty     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE v-i-qty     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE v-t-qty     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lv-uom      LIKE rm-rcpth.pur-uom NO-UNDO.
    DEFINE VARIABLE ld-qty      LIKE rm-rdtlh.qty NO-UNDO.
    DEFINE VARIABLE ld-cst      LIKE rm-rdtlh.cost NO-UNDO.
    DEFINE VARIABLE v-printed   AS LOGI    NO-UNDO.
        

    DEFINE VARIABLE v-cum-qty   AS DECIMAL EXTENT 2 FORMAT ">>>>>9.999".
    postit:
    DO /* TRANSACTION */ ON ERROR UNDO postit, LEAVE postit:
        /* Set date first so can sort by it */
        FOR EACH rm-rctd EXCLUSIVE-LOCK 
            WHERE rm-rctd.company   EQ cocode
            AND rm-rctd.rita-code EQ "C"   
            AND rm-rctd.tag NE ""
            AND rm-rctd.i-no GE ipcFGItemStart
            AND rm-rctd.i-no LE ipcFGItemEnd
            AND LOOKUP(rm-rctd.loc, ipcWhseList) GT 0 
            AND rm-rctd.loc-bin GE ipcBinStart
            AND rm-rctd.loc-bin LE ipcBinEnd
            AND rm-rctd.qty GE 0  
            AND CAN-FIND(FIRST ITEM NO-LOCK
                         WHERE ITEM.company EQ rm-rctd.company 
                           AND ITEM.i-no EQ rm-rctd.i-no
                           AND item.cc-code GE ipcFromCycleCode
                           AND item.cc-code LE ipcToCycleCode)
            
            :
            IF ipdtTransDate NE ? AND ipdtTransDate NE rm-rctd.rct-date THEN 
              rm-rctd.rct-date = ipdtTransDate.
            IF ipiTransTime NE ? AND ipiTransTime NE 0 THEN 
                rm-rctd.trans-time = ipiTransTime.              
            FIND FIRST ttToPost
                WHERE ttToPost.rRmRctd EQ ROWID(rm-rctd)
                NO-ERROR.
            IF NOT AVAIL ttToPost THEN DO:
              CREATE ttToPost.
              ASSIGN ttToPost.rRmRctd = ROWID(rm-rctd)
                     ttToPost.r-no    = rm-rctd.r-no
                     .
            END. 
        END.
        
        FOR EACH ttToPost,
           FIRST rm-rctd EXCLUSIVE-LOCK 
            WHERE ROWID(rm-rctd) EQ ttToPost.rRmrctd
            ,  
            FIRST ITEM EXCLUSIVE-LOCK 
            WHERE item.company EQ cocode
            AND item.i-no    EQ rm-rctd.i-no
            USE-INDEX i-no

            BREAK BY rm-rctd.i-no
            BY rm-rctd.rct-date
            BY rm-rctd.tag:

            ASSIGN
                item.last-count = 0
                item.q-onh      = 0                
                item.last-date  = rm-rctd.rct-date
                .
           FIND FIRST ttCycleCountCompare NO-LOCK /*Only one record per tag*/
               WHERE ttCycleCountCompare.cCompany EQ rm-rctd.company
                AND ttCycleCountCompare.cFGItemID EQ rm-rctd.i-no
                AND ttCycleCountCompare.cTag      EQ rm-rctd.tag
                NO-ERROR.
            IF AVAIL ttCycleCountCompare AND rm-rctd.qty NE 0 AND rm-rctd.cost EQ 0 THEN 
              rm-rctd.cost = ttCycleCountCompare.dSysCost.
              
            /** Find Bin & if not available then create it **/
            FIND FIRST rm-bin
                WHERE rm-bin.company EQ cocode
                AND rm-bin.loc     EQ rm-rctd.loc
                AND rm-bin.i-no    EQ rm-rctd.i-no
                AND rm-bin.loc-bin EQ rm-rctd.loc-bin
                AND rm-bin.tag     EQ rm-rctd.tag
                NO-ERROR.

            IF NOT AVAILABLE rm-bin THEN 
            DO:
                IF rm-rctd.cost EQ 0 AND rm-rctd.qty GT 0 THEN 
                ASSIGN  
                        rm-rctd.cost     = IF v-avgcost THEN ITEM.avg-cost ELSE ITEM.last-cost
                        rm-rctd.cost-uom = ITEM.cons-uom.
                CREATE rm-bin.
                ASSIGN
                    rm-bin.company = rm-rctd.company
                    rm-bin.loc     = rm-rctd.loc
                    rm-bin.loc-bin = rm-rctd.loc-bin
                    rm-bin.tag     = rm-rctd.tag
                    rm-bin.i-no    = rm-rctd.i-no
                    rm-bin.cost    = rm-rctd.cost
                    rm-bin.po-no   = INTEGER(rm-rctd.po-no)
                    .

            END. /* not avail rm-bin */

            rm-bin.qty = rm-rctd.qty.

            /* Update bin with any transactions after this cycle count */
            FOR EACH rm-rcpth
                WHERE rm-rcpth.company    EQ cocode
                AND rm-rcpth.i-no       EQ item.i-no
                AND rm-rcpth.trans-date GE rm-rctd.rct-date
                NO-LOCK USE-INDEX i-no,

                EACH rm-rdtlh
                WHERE rm-rdtlh.r-no      EQ rm-rcpth.r-no
                AND rm-rdtlh.rita-code EQ rm-rcpth.rita-code
                AND rm-rdtlh.loc       EQ rm-bin.loc
                AND rm-rdtlh.loc-bin   EQ rm-bin.loc-bin
                AND rm-rdtlh.tag       EQ rm-bin.tag
                NO-LOCK

                BY rm-rcpth.trans-date
                BY rm-rcpth.r-no
                BY RECID(rm-rdtlh):

                IF rm-rcpth.trans-date EQ rm-rctd.rct-date AND
                    rm-rcpth.r-no       LT rm-rctd.r-no       THEN NEXT. 

                {rm/rm-mkbin.i}
            END.           

            IF LAST-OF(rm-rctd.i-no) THEN 
            DO:
                v-temp-cost = 0.

                FOR EACH rm-bin
                    WHERE rm-bin.company EQ cocode
                    AND rm-bin.i-no    EQ item.i-no
                    ON ERROR UNDO postit, LEAVE:

                    ASSIGN
                        item.q-onh      = item.q-onh + rm-bin.qty
                        item.last-count = item.last-count + rm-bin.qty
                        v-temp-cost     = v-temp-cost + (rm-bin.qty * rm-bin.cost).
                END. /* each rm-bin */

                IF item.q-onh EQ 0 THEN item.avg-cost = 0.

                /** Calculate new average cost for item **/
                ELSE
                    IF v-temp-cost GT 0 THEN item.avg-cost = v-temp-cost / item.q-onh.

                item.q-avail = item.q-onh + item.q-ono - item.q-comm.
            END. /* last-of rm-rctd.i-no */

            {rm/rm-rctd.i rm-rcpth rm-rdtlh rm-rctd} /* Create History Records */
            /* create rm-rcpth.
             {rm/rm-rcpt.i rm-rcpth rm-rctd}     /* Create Header History Records */
             CREATE rm-rdtlh.
             {rm/rm-rdtl.i rm-rdtlh rm-rctd}   /* Create Detail History Records */
             */
            DELETE rm-rctd.
        END. /* for each rm-rctd */
        IF ipiTransTime NE 0 AND ipiTransTime NE ? THEN DO:
            FOR EACH ttToPost, 
                EACH rm-rdtlh EXCLUSIVE-LOCK 
                    WHERE rm-rdtlh.r-no EQ ttToPost.r-no
                :
                rm-rdtlh.trans-time = ipiTransTime.
            END.
        END.
        v-dunne = TRUE.
    END. /* postit */

END PROCEDURE.

PROCEDURE pRemoveMatches:
    /*------------------------------------------------------------------------------
     Purpose: Delete cycle counts that match quantity and location or just loc changed
     Notes:   If the quantity is the same and the location changed, a transfer was
              created, so the count can be removed
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFGItemStart AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFGItemEnd AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcWhseList AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcBinStart AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcBinEnd AS CHARACTER NO-UNDO.
        
    FOR EACH ttCycleCountCompare NO-LOCK 
        WHERE ttCycleCountCompare.lMatch = TRUE        
        AND ttCycleCountCompare.cFGItem     GE ipcFGItemStart
        AND ttCycleCountCompare.cFgItem     LE ipcFGItemEnd
        AND IF ttCycleCountCompare.cScanLoc GT "" THEN 
        (
        LOOKUP(ttCycleCountCompare.cScanLoc, ipcWhseList) GT 0 
        AND ttCycleCountCompare.cScanLocBin  GE ipcBinStart
        AND ttCycleCountCompare.cScanLocBin  LE ipcBinEnd
        )
        ELSE 
        (
        LOOKUP(ttCycleCountCompare.cSysLoc, ipcWhseList) GT 0   
        AND ttCycleCountCompare.cSysLocBin  GE ipcBinStart
        AND ttCycleCountCompare.cSysLocBin  LE ipcBinEnd
        ) 
        ,    
        EACH rm-rctd EXCLUSIVE-LOCK 
        WHERE rm-rctd.company  EQ ttCycleCountCompare.cCompany 
        AND rm-rctd.i-no     EQ ttCycleCountCompare.cFGItemID  
        AND rm-rctd.tag      EQ ttCycleCountCompare.cTag        
        AND rm-rctd.loc      EQ ttCycleCountCompare.cScanLoc    
        AND rm-rctd.loc-bin  EQ ttCycleCountCompare.cScanLocBin  
        AND rm-rctd.qty      EQ ttCycleCountCompare.dScanQty    
        AND rm-rctd.r-no     EQ ttCycleCountCompare.iSequence
        
       
        :
        DELETE rm-rctd.
    END. 

END PROCEDURE.

PROCEDURE pRemoveZeroCounts:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    FOR EACH rm-rctd EXCLUSIVE-LOCK 
        WHERE rm-rctd.company EQ cocode
        AND rm-rctd.rita-code EQ "C"
        AND rm-rctd.qty EQ 0
        :
        DELETE rm-rctd.
    END. 
          
END PROCEDURE.

PROCEDURE pReportProblems PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* output stream */
/* put header for each problem type */
/* for each ttProblems for each problem type */
DEFINE VARIABLE cProblemList AS CHARACTER NO-UNDO.
    FIND FIRST ttProblems NO-ERROR.
    IF AVAIL ttProblems THEN DO:
        OUTPUT STREAM sOutput TO VALUE(gcProblemsFile) APPEND.
        PUT STREAM sOutput SKIP(1) "Problem Description,Item#,Tag#,2nd Item,Loc/bin,Loc/bin2" SKIP (1).
        FOR EACH ttProblems
            BREAK BY ttProblems.tag:

            cProblemList = "".
            IF ttProblems.lNoCostFound THEN 
                cProblemList = cProblemList + " No Cost,".
            IF ttProblems.lNoMSFFound THEN 
                cProblemList = cProblemList + " No MSF,".
            IF ttProblems.lInactiveWithBin THEN 
                cProblemList = cProblemList + " Inactive Item with quantity,".
            IF ttProblems.lNoitem THEN 
                cProblemList = cProblemList + " No item for bin,".
            IF ttProblems.lOnHandNobin THEN 
                cProblemList = cProblemList + " On Hand Qty but no bin,".
            IF ttProblems.lDuplicateFound THEN 
                cProblemList = cProblemList + " Duplicate Tag,".
            cProblemList = TRIM(TRIM(cProblemList,",")).
            IF FIRST-OF(ttProblems.tag) THEN 
            EXPORT STREAM sOutput DELIMITER ","
                cProblemList
                ttProblems.i-no 
                ttProblems.tag 
                ttProblems.transTypes  
                ttProblems.i-no2 
                ttProblems.Loc1  
                ttProblems.Loc2  
                .        
        END.
        OUTPUT STREAM sOutput CLOSE.
    END.
END PROCEDURE.

PROCEDURE reportComparison:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcOutputFile AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdtTransDate AS DATE NO-UNDO.
    DEFINE INPUT  PARAMETER ipiTransTime AS INTEGER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFGItemStart AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFGItemEnd AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFromCycleCode AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcToCycleCode AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcWhseList AS CHARACTER NO-UNDO.    
    DEFINE INPUT  PARAMETER ipcBinStart AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcBinEnd AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplScansOnly AS LOGICAL NO-UNDO.
    DEFINE INPUT  PARAMETER iplComplete AS LOGICAL NO-UNDO.
    DEFINE INPUT  PARAMETER iplQtyChanged AS LOGICAL NO-UNDO.
    DEFINE INPUT  PARAMETER iplSnapshotOnly AS LOGICAL NO-UNDO.
    DEFINE INPUT  PARAMETER iplDupsInSnapshot AS LOGICAL NO-UNDO.
    DEFINE INPUT  PARAMETER iplDupsInScan AS LOGICAL NO-UNDO.
    DEFINE INPUT  PARAMETER ipiSnapshotID AS INTEGER NO-UNDO.
    DEFINE INPUT  PARAMETER iplSkipUnscanned AS LOGICAL NO-UNDO.

    DEFINE VARIABLE lChoosePost AS LOGICAL NO-UNDO.
        
    STATUS DEFAULT "Import Snapshot" .       
    RUN pImportSnapshot (INPUT ipiSnapshotID).
    
    STATUS DEFAULT "Build Compare Table". 
    RUN pBuildCompareTable(ipcCompany, ipcFGItemStart, ipcFGItemEnd, ipcFromCycleCode, ipcToCycleCode, ipcWhseList, 
        ipcBinStart, ipcBinEnd, YES /* scans only */).
    gcOutputFile = ipcOutputFile.
    PROCESS EVENTS.
    STATUS DEFAULT "Exporting Report".
    RUN pExportTempTable(TEMP-TABLE ttCycleCountCompare:HANDLE, gcOutputFile, YES /* header */, iplComplete, 
        iplQtyChanged, iplSnapshotOnly, iplDupsInSnapshot, iplDupsInScan).
    OS-COMMAND NO-WAIT VALUE(gcOutputFile).
    STATUS DEFAULT "Done".
    
    MESSAGE 'Post Counts?' SKIP
        VIEW-AS ALERT-BOX
        QUESTION BUTTONS YES-NO UPDATE lChoosePost.
        
    IF lChoosePost THEN 
        RUN postRM (ipcCompany, ipdtTransDate, ipiTransTime, ipcFGItemStart, ipcFGItemEnd, ipcFromCycleCode, ipcToCycleCode, ipcWhseList, 
            ipcBinStart, ipcBinEnd, iplSkipUnscanned). 
        
END PROCEDURE.


/* ************************  Function Implementations ***************** */

FUNCTION fGetAction RETURNS CHARACTER 
    (ipcLocChanged AS LOGICAL, iplQtyChanged AS LOGICAL, iplNotScanned AS LOGICAL, iplMatch AS LOGICAL, ipdCntNonZero AS DECIMAL, 
    ipdCntZero AS DECIMAL, ipcLoc AS CHARACTER  ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/	

    DEFINE VARIABLE cresult AS CHARACTER NO-UNDO.
    cResult = "Count Posted".
    
        
    IF  iplQtyChanged THEN cResult = "Count Posted".
    
    IF  ipcLocChanged /* AND iplQtyChanged */ THEN 
        cResult = "Count Posted, zero count created for original location".
        
    /* Not in snapshot but may have been in another warehouse */
    IF  ipcLocChanged AND iplQtyChanged AND ipcLoc = "" THEN 
         cResult = "Count Posted".          
                      
    IF  ipdCntNonZero > 1 THEN 
        ASSIGN cResult = "Cannot Post - Remove Dupcate" .
        
    IF  iplNotScanned THEN cResult = "Zero Count auto-created". 
        
    IF  ipdCntZero > 1 THEN ASSIGN cResult = "Cannot Post - Remove Duplicate".       
        
    IF  iplMatch THEN  cResult = "Count auto-deleted".

    RETURN cresult.
		
END FUNCTION.

FUNCTION fSnapshotCreateDtTime RETURNS CHARACTER 
    (  ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/	

    DEFINE VARIABLE cresult AS CHARACTER NO-UNDO.
		
    FILE-INFO:FILE-NAME = gcSnapshotFile.
    IF FILE-INFO:PATHNAME NE ? THEN 
        ASSIGN cResult = STRING(YEAR(FILE-INFO:FILE-CREATE-DATE),"9999")
                 + STRING(MONTH(FILE-INFO:FILE-CREATE-DATE),"99")
                 + STRING(DAY(FILE-INFO:FILE-CREATE-DATE),"99")
                 + STRING(FILE-INFO:FILE-CREATE-TIME,"99999")
            .
    ELSE 
        cResult = 
            STRING(YEAR(TODAY),"9999")
            + STRING(MONTH(TODAY),"99")
            + STRING(DAY(TODAY),"99")
            + STRING(60 * 60 * 7,"99999")
            .
  
    RETURN cresult.
		
END FUNCTION.
