
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
DEFINE NEW SHARED VARIABLE v-trnum AS INTEGER.
cocode = gcompany.
IF cocode = "" THEN cocode = "001".

DEFINE TEMP-TABLE ttCycleCountCompare
    FIELD cCompany                   AS CHARACTER COLUMN-LABEL "Company" 
    FIELD cFGItemID                  AS CHARACTER COLUMN-LABEL "FG Item ID"
    FIELD cTag                       AS CHARACTER COLUMN-LABEL "Tag"
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
    
DEFINE TEMP-TABLE ttDupTags
    FIELD i-no       LIKE fg-bin.i-no
    FIELD tag        LIKE fg-bin.tag
    FIELD transTypes AS CHARACTER.
DEFINE TEMP-TABLE w-fg-rctd NO-UNDO LIKE fg-rctd.
{fg/fullset.i NEW}   
{oe/invwork.i new} 
DEFINE STREAM sOutput.
DEFINE STREAM sIn.

DEFINE            VARIABLE gcOutputFile   AS CHARACTER NO-UNDO.
DEFINE            VARIABLE gcSnapshotFile AS CHARACTER NO-UNDO INIT ".\custfiles\invSnapShotFG.csv".

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
    DEFINE INPUT PARAMETER ipcWhseStart AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcWhseEnd AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lBinDups AS LOGICAL NO-UNDO.
    
    RUN pCheckBinDups (OUTPUT lBinDups ).
    /* IF lBinDups THEN 
         RETURN. */
      
    FOR EACH fg-bin NO-LOCK
        WHERE fg-bin.company EQ ipcCompany
        AND fg-bin.i-no GE ipcFGItemStart
        AND fg-bin.i-no LE ipcFGItemEnd
        AND fg-bin.loc GE ipcWhseStart
        AND fg-bin.loc LE ipcWhseEnd
        AND fg-bin.qty NE 0
        AND fg-bin.tag NE ""
        :
        CREATE ttSnapshot.
        ASSIGN 
            ttSnapShot.cCompany   = fg-bin.company
            ttSnapShot.cFGItemID  = fg-bin.i-no
            ttSnapShot.cTag       = fg-bin.tag
            ttSnapShot.cSysLoc    = fg-bin.loc
            ttSnapShot.cSysLocBin = fg-bin.loc-bin
            ttSnapShot.dSysQty    = fg-bin.qty
            ttSnapShot.dCost      = fg-bin.std-tot-cost
            ttSnapShot.cCostUom   = fg-bin.pur-uom 
            .            
      
        FOR LAST fg-rdtlh NO-LOCK 
            WHERE fg-rdtlh.company EQ fg-bin.company 
            AND fg-rdtlh.tag EQ fg-bin.tag 
            /* AND fg-rdtlh.rita-code EQ "R" */
            USE-INDEX tag,
            EACH fg-rcpth NO-LOCK 
            WHERE fg-rcpth.r-no EQ fg-rdtlh.r-no
            AND fg-rcpth.rita-code EQ fg-rdtlh.rita-code:
            ASSIGN 
                ttSnapShot.cJobNo  = fg-rdtlh.job-no 
                ttSnapShot.cJobNo2 = STRING(fg-rdtlh.job-no2)
                /*              ttSnapShot.cSNum    = STRING(fg-rdtlh.s-num )*/
                /*              ttSnapShot.cBNum    = STRING(fg-rdtlh.b-num) */
                .
        END.            
    END.
    
    OUTPUT STREAM sOutput TO VALUE(gcSnapshotFile).
    FOR EACH ttSnapShot:
        EXPORT STREAM sOutput DELIMITER "," ttSnapShot.
    END.
    OUTPUT STREAM sOutput CLOSE.
    
END PROCEDURE.


PROCEDURE pBuildCompareTable PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Builds the compare temp-table based on parameters
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFGItemStart AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFGItemEnd AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcWhseStart AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcWhseEnd AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcBinStart AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcBinEnd AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplScansOnly AS LOGICAL NO-UNDO.
  
    DEFINE BUFFER bf-fg-rctd FOR fg-rctd.
    DEFINE BUFFER bf-fg-bin  FOR fg-bin.

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
    FOR EACH fg-rctd NO-LOCK 
        WHERE fg-rctd.company EQ ipcCompany
        AND fg-rctd.rita-code EQ "C"
        AND fg-rctd.tag NE ""
        AND fg-rctd.i-no GE ipcFGItemStart
        AND fg-rctd.i-no LE ipcFGItemEnd
        AND fg-rctd.loc GE ipcWhseStart
        AND fg-rctd.loc LE ipcWhseEnd
        AND fg-rctd.loc-bin GE ipcBinStart
        AND fg-rctd.loc-bin LE ipcBinEnd
        AND fg-rctd.qty NE 0
        :
        /*Initial Create*/    
        FIND FIRST ttCycleCountCompare NO-LOCK /*Only one record per tag*/
            WHERE ttCycleCountCompare.cCompany EQ fg-rctd.company
            AND ttCycleCountCompare.cFGItemID EQ fg-rctd.i-no
            AND ttCycleCountCompare.cTag EQ fg-rctd.tag
            NO-ERROR.
        IF NOT AVAILABLE ttCycleCountCompare THEN 
        DO:
            iStatusCnt1 = iStatusCnt1 + 1.
            IF iStatusCnt1 GT 99 THEN 
            DO:
                iStatusCnt2 = iStatusCnt2 + iStatusCnt1.
                iStatusCnt1 = 0.
                STATUS DEFAULT "Build Compare " + STRING(iStatusCnt2).
                PROCESS EVENTS.
            END.
            
            CREATE ttCycleCountCompare.
            ASSIGN 
                ttCycleCountCompare.cCompany       = fg-rctd.company
                ttCycleCountCompare.cFGItemID      = fg-rctd.i-no
                ttCycleCountCompare.cTag           = fg-rctd.tag
                ttCycleCountCompare.cScanLoc       = fg-rctd.loc
                ttCycleCountCompare.cScanLocBin    = fg-rctd.loc-bin
                ttCycleCountCompare.dScanQty       = fg-rctd.qty
                ttCycleCountCompare.dScanCost      = fg-rctd.cost
                ttCycleCountCompare.dScanCostValue = fg-rctd.qty * fg-rctd.cost
                ttCycleCountCompare.iSequence      = fg-rctd.r-no
                ttCycleCountCompare.dtScanDate     = fg-rctd.enteredDT
                ttCycleCountCompare.cScanUser      = fg-rctd.enteredBy
                ttCycleCountCompare.cJobNo         = fg-rctd.job-no
                ttCycleCountCompare.cJobNo2        = STRING(fg-rctd.job-no2)
                ttCycleCountCompare.cSNum          = STRING(fg-rctd.s-num)
                ttCycleCountCompare.cBnum          = STRING(fg-rctd.b-num)
                .
        END.
    END. 
  
    /* Records for reporting of snapshot only */
    FOR EACH ttSnapshot
        WHERE ttSnapshot.cFGItemID GE ipcFgItemStart
        AND ttSnapshot.cFgItemID   LE ipcFgItemEnd
        AND ttSnapshot.cSysLoc     GE ipcWhseStart
        AND ttSnapshot.cSysLoc     LE ipcWhseEnd
        AND ttSnapshot.cSysLocBin  GE ipcBinStart
        AND ttSnapshot.cSysLocBin  LE ipcBinEnd         
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
        FOR EACH fg-rdtlh NO-LOCK 
            WHERE fg-rdtlh.rec_key GT fSnapshotCreateDtTime()                  
            AND fg-rdtlh.company EQ fg-bin.company 
            AND fg-rdtlh.tag EQ fg-bin.tag ,
            EACH fg-rcpth NO-LOCK 
            WHERE fg-rcpth.r-no EQ fg-rdtlh.r-no
            AND fg-rcpth.rita-code EQ fg-rdtlh.rita-code:
            CASE fg-rcpth.rita-code:
                WHEN "S" THEN 
                    dTransQty = dTransQty - fg-rdtlh.qty.
                WHEN "R"THEN 
                    dTransQty = dTransQty + fg-rdtlh.qty.
                WHEN "A" THEN 
                    dTransQty = dTransQty + fg-rdtlh.qty.
                OTHERWISE 
                dTransQty = dTransQty + fg-rdtlh.qty.
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
        FOR EACH bf-fg-rctd NO-LOCK 
            WHERE bf-fg-rctd.company EQ fg-rctd.company
            AND bf-fg-rctd.i-no EQ fg-rctd.i-no
            AND bf-fg-rctd.tag EQ fg-rctd.tag
            AND bf-fg-rctd.rita-code EQ "C"
            AND ROWID(bf-fg-rctd) NE ROWID(fg-rctd)
            :
            IF bf-fg-rctd.qty NE 0 THEN 
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
                ttCycleCountCompare.lMatch     = (ttCycleCountCompare.dSysQty EQ ttCycleCountCompare.dScanQty)
                .
        END.   
        
        /*Count existing non-zero bins for tag*/
        iCountBins = 0.
        /*        FOR EACH bf-fg-bin NO-LOCK                                                              */
        /*            WHERE bf-fg-bin.company EQ fg-rctd.company                                          */
        /*            AND bf-fg-bin.i-no EQ fg-rctd.i-no                                                  */
        /*            AND bf-fg-bin.tag EQ fg-rctd.tag                                                    */
        /*            AND bf-fg-bin.qty NE 0                                                              */
        /*            :                                                                                   */
        /*            iCountBins = iCountBins + 1.                                                        */
        /*            IF NOT ttCycleCountCompare.lMatch THEN /*Get the last one found and count the rest*/*/
        /*                ASSIGN                                                                          */
        /*                    ttCycleCountCompare.cSysLoc    = bf-fg-bin.loc                              */
        /*                    ttCycleCountCompare.cSysLocBin = bf-fg-bin.loc-bin                          */
        /*                    ttCycleCountCompare.dSysQty    = bf-fg-bin.qty                              */
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
            ttCycleCountCompare.lLocationChanged          = (ttCycleCountCompare.cScanLoc NE ttCycleCountCompare.cSysLoc 
                                                    OR ttCycleCountCompare.cScanLocBin NE ttCycleCountCompare.cSysLocBin)
            ttCycleCountCompare.lQuantityChanged          = (ttCycleCountCompare.dScanQty NE ttCycleCountCompare.dSysQty)
            ttCycleCountCompare.iCountOfBinsForTagNonZero = iCountBins
            .
        /*See if there are have been shipments for that tag, after the scan was done*/
        IF ttCycleCountCompare.lQuantityChanged AND ttCycleCountCompare.dSysQty EQ 0 THEN 
        DO:
        /* RUN pGetLastTransDate(fg-rctd.company, fg-rctd.i-no, fg-rctd.tag, "S", OUTPUT ttCycleCountCompare.dtShipDate). */
        END.    
         
    END.
    
    IF NOT iplScansOnly THEN 
    DO: 
        FOR EACH fg-bin NO-LOCK
            WHERE fg-bin.company EQ ipcCompany
            AND fg-bin.i-no GE ipcFGItemStart
            AND fg-bin.i-no LE ipcFGItemEnd
            AND fg-bin.loc GE ipcWhseStart
            AND fg-bin.loc LE ipcWhseEnd
            AND fg-bin.qty NE 0
            AND fg-bin.tag NE ""
            :
            FIND FIRST ttCycleCountCompare NO-LOCK /*Only one record per tag*/
                WHERE ttCycleCountCompare.cCompany EQ fg-bin.company
                AND ttCycleCountCompare.cFGItemID EQ fg-bin.i-no
                AND ttCycleCountCompare.cTag EQ fg-bin.tag
                USE-INDEX tag
                NO-ERROR.
            IF NOT AVAILABLE ttCycleCountCompare THEN 
            DO:
                CREATE ttCycleCountCompare.
                ASSIGN 
                    ttCycleCountCompare.cCompany         = fg-bin.company
                    ttCycleCountCompare.cFGItemID        = fg-bin.i-no
                    ttCycleCountCompare.cTag             = fg-bin.tag
                    ttCycleCountCompare.cSysLoc          = fg-bin.loc
                    ttCycleCountCompare.cSysLocBin       = fg-bin.loc-bin
                    ttCycleCountCompare.dSysQty          = fg-bin.qty
                    ttCycleCountCompare.lNotScanned      = YES 
                    ttCycleCountCompare.lQuantityChanged = YES 
                    .
            END.
            FOR LAST fg-rdtlh NO-LOCK 
                WHERE fg-rdtlh.company EQ fg-bin.company 
                AND fg-rdtlh.tag EQ fg-bin.tag 
                AND fg-rdtlh.rita-code EQ "R",
                EACH fg-rcpth NO-LOCK 
                WHERE fg-rcpth.r-no EQ fg-rdtlh.r-no
                AND fg-rcpth.rita-code EQ fg-rdtlh.rita-code:
                ASSIGN 
                    ttCycleCountCompare.cJobNo2 = STRING(fg-rdtlh.job-no2)
                    /*                    ttCycleCountCompare.cSNum   = STRING(fg-rdtlh.s-num)*/
                    /*                    ttCycleCountCompare.cBNum   = STRING(fg-rdtlh.b-num)*/
                    .
            END.              
        /*Check for receipts after scan*/
        /* RUN pGetLastTransDate(fg-bin.company, fg-bin.i-no, fg-bin.tag, "R", OUTPUT ttCycleCountCompare.dtReceiptDate). */
                    
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
        (
        ttCycleCountCompare.cScanLoc     GE ipcWhseStart
        AND ttCycleCountCompare.cScanLoc     LE ipcWhseEnd
        AND ttCycleCountCompare.cScanLocBin  GE ipcBinStart
        AND ttCycleCountCompare.cScanLocBin  LE ipcBinEnd
        )
        ELSE 
        (ttCycleCountCompare.cSysLoc     GE ipcWhseStart
        AND ttCycleCountCompare.cSysLoc     LE ipcWhseEnd
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
            ttCycleCountCompare.lQuantityChanged, 
            ttCycleCountCompare.lNotScanned, 
            ttCycleCountCompare.lMatch,
            ttCycleCountCompare.iCountOfBinsForTagNonZero, 
            ttCycleCountCompare.iCountofScansForTagZero,
            ttCycleCountCompare.cSysLoc
            ).

        FIND FIRST itemfg NO-LOCK 
            WHERE itemfg.company EQ ttCycleCountCompare.cCompany 
            AND itemfg.i-no EQ ttCycleCountCompare.cFgItemID
            NO-ERROR.

        IF NOT AVAILABLE itemfg THEN NEXT.
        
        /* Sys MSF */
        dCalcQty = ttCycleCountCompare.dSysQty.
     
        dLFQty = dCalcQty.   
                             
        ASSIGN          
            dMSF = dCalcQty * itemfg.t-sqft / 1000.
        IF dMSF EQ ? THEN 
            dMsf = 0.
        
        ttCycleCountCompare.dSysMSF            = dMSF.
        
        /* Scan MSF */
        dCalcQty = ttCycleCountCompare.dScanQty.
      
        dLFQty = dCalcQty.

        FIND FIRST fg-bin NO-LOCK /*Only one record per tag*/
            WHERE fg-bin.company EQ ttCycleCountCompare.cCompany 
            AND fg-bin.i-no EQ ttCycleCountCompare.cFGItemID  
            AND fg-bin.tag EQ ttCycleCountCompare.cTag  
            USE-INDEX tag NO-ERROR.
        IF AVAILABLE fg-bin THEN 
            ASSIGN
                dCost = fg-bin.std-tot-cost * fg-bin.qty.

        /* Calculate Cost */
        
        IF AVAILABLE fg-bin AND fg-bin.pur-uom EQ "CS" AND fg-bin.case-count NE 0 THEN
            ASSIGN
                dCost = dCost  / fg-bin.case-count.
        ELSE
        DO:
            FIND FIRST uom 
                WHERE uom.uom  EQ itemfg.prod-uom
                AND uom.mult NE 0
                NO-LOCK NO-ERROR.
            IF AVAILABLE uom THEN
                ASSIGN
                    dCost = dCost  / uom.mult.
            ELSE
                ASSIGN
                    dCost = dCost  / 1000.
        END.
       
        ASSIGN 
            ttCycleCountCompare.dSysCost      = dCost 
            ttCycleCountCompare.cSysCostUom   = IF AVAILABLE fg-bin THEN fg-bin.pur-uom ELSE itemfg.pur-uom      
            ttCycleCountCompare.dSysCostValue = dCost * ttCycleCountCompare.dSysQty
            .
        
        IF NOT ttCycleCountCompare.lNotScanned THEN 
        DO:
            FIND FIRST fg-bin NO-LOCK /*Only one record per tag*/
                WHERE fg-bin.company EQ ttCycleCountCompare.cCompany 
                AND fg-bin.i-no EQ ttCycleCountCompare.cFGItemID  
                AND fg-bin.tag EQ ttCycleCountCompare.cTag  
                USE-INDEX tag
                NO-ERROR.  
            IF AVAILABLE fg-bin THEN  
                FIND FIRST po-ordl  NO-LOCK WHERE po-ordl.company EQ fg-bin.company 
                    AND po-ordl.po-no EQ INTEGER(fg-bin.po-no)
                    AND po-ordl.i-no EQ fg-bin.i-no
                    NO-ERROR.
        
            IF AVAILABLE po-ordl THEN                    
                ASSIGN 
                    ttCycleCountCompare.cJobNo  = IF po-ordl.job-no NE "" THEN STRING(po-ordl.job-no) + "-" + STRING(po-ordl.job-no2) ELSE ""
                    ttCycleCountCompare.cJobNo2 = STRING(po-ordl.job-no2)
                    ttCycleCountCompare.cSNum   = STRING(po-ordl.s-num)
                    ttCycleCountCompare.cBNum   = STRING(po-ordl.b-num )
                    .                 
        END.

    END. 
END PROCEDURE.

PROCEDURE pCheckBinDups:
    /*------------------------------------------------------------------------------
     Purpose: Check Bin Duplicates
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplNoDups AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lIsDups AS LOGICAL NO-UNDO.
    DEFINE BUFFER bf-fg-bin FOR fg-bin.

    EMPTY TEMP-TABLE ttDupTags.
    lIsDups = NO.
    FOR EACH fg-bin WHERE fg-bin.qty GT 0
        AND fg-bin.tag GT "" NO-LOCK.
        FIND FIRST bf-fg-bin NO-LOCK
            WHERE bf-fg-bin.company EQ fg-bin.company
            AND bf-fg-bin.tag EQ fg-bin.tag
            AND ROWID(bf-fg-bin) NE ROWID(fg-bin)
            AND bf-fg-bin.qty GT 0 
            NO-ERROR.
        IF AVAILABLE bf-fg-bin THEN 
        DO:
            lIsDups = YES.
            CREATE ttDupTags.
            ASSIGN 
                ttDupTags.i-no = fg-bin.i-no
                ttDupTags.tag  = fg-bin.tag.
        END.
    END.

    OUTPUT TO c:\tmp\dupbinTags.csv.
    FOR EACH ttDupTags:
        EXPORT DELIMITER "," ttDupTags.
    END.
    OUTPUT CLOSE.
    
    oplNoDups = lIsDups.
    IF lIsDups THEN 
    DO:
        MESSAGE "Cannot initialize because some tags exist in more than one bin." SKIP 
            "Click OK to view duplicate tag records."
            VIEW-AS ALERT-BOX.
        OS-COMMAND NO-WAIT START excel.exe VALUE("c:\tmp\dupbinTags.csv").
    END.
END PROCEDURE.

PROCEDURE pCheckCountDups:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplDups AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lIsDups AS LOGICAL NO-UNDO.
    DEFINE BUFFER bf-fg-rctd FOR fg-rctd.

    EMPTY TEMP-TABLE ttDupTags.
    lIsDups = FALSE. 
    FOR EACH fg-rctd NO-LOCK
        WHERE fg-rctd.company EQ cocode
        AND fg-rctd.rita-code EQ "C".
        FIND FIRST bf-fg-rctd NO-LOCK
            WHERE bf-fg-rctd.company EQ fg-rctd.company
            AND bf-fg-rctd.tag EQ fg-rctd.tag
            AND ROWID(bf-fg-rctd) NE ROWID(fg-rctd)
            AND bf-fg-rctd.rita-code NE "P"
            USE-INDEX tag 
            NO-ERROR.
        IF AVAILABLE bf-fg-rctd THEN 
        DO:
            lIsDups = TRUE. 
            CREATE ttDupTags.
            ASSIGN 
                ttDupTags.i-no       = fg-rctd.i-no
                ttDupTags.tag        = fg-rctd.tag
                ttDupTags.transTypes = fg-rctd.rita-code + "," + bf-fg-rctd.rita-code
                .
        END.
    END.
    oplDups = lIsDups.
    IF lIsDups THEN 
    DO:
        OUTPUT TO c:\tmp\dupCountTags.csv.
        FOR EACH ttDupTags:
            EXPORT DELIMITER "," ttDupTags.
        END.
        OUTPUT CLOSE.
    

        MESSAGE "Cannot initialize because some tags were counted more than once." SKIP 
            "Click OK to view duplicate tag records."
            VIEW-AS ALERT-BOX.
        OS-COMMAND NO-WAIT START excel.exe VALUE("c:\tmp\dupCountTags.csv").
    END.
END PROCEDURE.

PROCEDURE pCreateTransfers:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
   
    DEFINE VARIABLE iNextRno LIKE fg-rctd.r-no NO-UNDO.
    DEFINE BUFFER b-fg-rctd FOR fg-rctd.
    DEFINE VARIABLE dTransDate AS DATE      NO-UNDO.
    DEFINE VARIABLE lv-tag     AS CHARACTER NO-UNDO.
    dTransDate = TODAY.

    /* Code placed here will execute PRIOR to standard behavior. */
    iNextRno = 0.
    FIND LAST b-fg-rctd USE-INDEX fg-rctd NO-LOCK NO-ERROR.
    IF AVAILABLE b-fg-rctd AND b-fg-rctd.r-no GT iNextRno THEN iNextRno = b-fg-rctd.r-no.

    FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
    IF AVAILABLE fg-rcpth AND fg-rcpth.r-no GT iNextRno THEN iNextRno = fg-rcpth.r-no.

    DO WHILE TRUE:
        iNextRno = iNextRno + 1.
        FIND FIRST fg-rcpth WHERE fg-rcpth.r-no EQ iNextRno USE-INDEX r-no NO-LOCK NO-ERROR.
        IF AVAILABLE fg-rcpth THEN NEXT.
        FIND FIRST b-fg-rctd WHERE b-fg-rctd.r-no EQ iNextRno USE-INDEX fg-rctd NO-LOCK NO-ERROR.
        IF AVAILABLE b-fg-rctd THEN NEXT.
        LEAVE.
    END.    
 
    FOR EACH ttCycleCountCompare NO-LOCK 
        WHERE lNotScanned = FALSE 
        AND lLocationChanged
        AND ttCycleCountCompare.cSysLoc GT ""
        AND ttCycleCountCompare.cSysLocBin GT "",    
        FIRST fg-bin NO-LOCK 
        WHERE /* fg-bin.r-no   EQ ttCycleCountCompare.iSequence    
        AND */ fg-bin.company EQ  ttCycleCountCompare.cCompany
        AND fg-bin.i-no    EQ ttCycleCountCompare.cFGItemID   
        AND fg-bin.tag     EQ ttCycleCountCompare.cTag        
        AND fg-bin.loc     EQ  ttCycleCountCompare.cSysLoc    
        AND fg-bin.loc-bin EQ  ttCycleCountCompare.cSysLocBin    
        :
            
        FIND FIRST itemfg WHERE itemfg.company = fg-bin.company
            AND itemfg.i-no = fg-bin.i-no NO-LOCK NO-ERROR.
        CREATE fg-rctd.
        ASSIGN 
            fg-rctd.r-no       = iNextRno
            fg-rctd.loc        = fg-bin.loc
            fg-rctd.loc-bin    = fg-bin.loc-bin
            fg-rctd.company    = fg-bin.company
            fg-rctd.rita-code  = "T"
            fg-rctd.s-num      = 0
            fg-rctd.rct-date   = dTransDate
            fg-rctd.trans-time = TIME
            fg-rctd.qty        = fg-bin.qty
            fg-rctd.qty-case   = (fg-bin.case-count)
            fg-rctd.cases-unit = (fg-bin.cases-unit)
            fg-rctd.i-no       = fg-bin.i-no
            fg-rctd.i-name     = itemfg.i-name
            fg-rctd.job-no     = fg-bin.job-no
            fg-rctd.job-no2    = fg-bin.job-no2
            fg-rctd.po-no      = fg-bin.po-no
            fg-rctd.tag        = fg-bin.tag
            fg-rctd.cust-no    = fg-bin.cust-no
            fg-rctd.loc2       = ttCycleCountCompare.cScanLoc 
            fg-rctd.loc-bin2   = ttCycleCountCompare.cScanLocBin
            fg-rctd.tag2       = ttCycleCountCompare.cTag
            fg-rctd.updated-by = "ASI"
            .
                               
        ASSIGN 
            fg-rctd.cases    = (TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0))
            fg-rctd.partial  = (fg-bin.partial-count)
            fg-rctd.t-qty    = (fg-bin.qty)
            fg-rctd.ext-cost = fg-rctd.t-qty /
                                  (IF fg-bin.pur-uom EQ "M" THEN 1000 ELSE 1) *
                                  fg-bin.std-tot-cost
            fg-rctd.cost     = fg-rctd.ext-cost / fg-rctd.t-qty
            fg-rctd.cost-uom = fg-bin.pur-uom
            .
       
        IF fg-rctd.pur-uom = "" THEN
            fg-rctd.pur-uom = fg-rctd.cost-uom.
        IF fg-rctd.ext-cost EQ ? THEN fg-rctd.ext-cost = 0.
        IF fg-rctd.cost     EQ ? THEN fg-rctd.cost = 0.
        
        
        ASSIGN 
            fg-rctd.user-id  = USERID("nosweat")
            fg-rctd.upd-date = TODAY
            fg-rctd.upd-time = TIME.
        IF AVAILABLE itemfg THEN 
        DO:
            fg-rctd.pur-uom = itemfg.cons-uom.
            RELEASE itemfg.
        END.  
    /* fg-rctd job, PO must match the fg-bin to post */
    /*
    FIND FIRST fg-rdtlh WHERE
        fg-rdtlh.company = fg-bin.company AND
        fg-rdtlh.tag = lv-tag AND
        fg-rdtlh.rita-code = "R"
        USE-INDEX tag
        NO-LOCK NO-ERROR.

    IF AVAILABLE fg-rdtlh THEN 
    DO:

        FIND FIRST fg-rcpth NO-LOCK 
            WHERE fg-rcpth.r-no EQ fg-rdtlh.r-no 
            NO-ERROR.

        IF AVAILABLE fg-rcpth THEN 
        DO:

            ASSIGN 
                fg-rctd.po-no   = fg-rcpth.po-no
                fg-rctd.po-line = MAX(fg-rcpth.po-line, 1)
                fg-rctd.job-no  = fg-rcpth.job-no
                fg-rctd.job-no2 = fg-rcpth.job-no2.
            RELEASE fg-rcpth.
        END.
            
        RELEASE fg-rdtlh.
    END.      
    */
    END.  /* for each fg-bin*/


END PROCEDURE.

PROCEDURE pCreateZeroCount:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/        
    DEFINE VARIABLE iNextRNo LIKE fg-rctd.r-no NO-UNDO.
    DEFINE BUFFER b-fg-rctd FOR fg-rctd.
    
    DEFINE VARIABLE dTransDate AS DATE      NO-UNDO.
    DEFINE VARIABLE cTag       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cIno       AS CHARACTER NO-UNDO.
    ASSIGN 
        dTransDate = TODAY
        iNextRNo   = 0
        .
    FIND LAST b-fg-rctd USE-INDEX fg-rctd NO-LOCK NO-ERROR.
    IF AVAILABLE b-fg-rctd AND b-fg-rctd.r-no GT iNextRno THEN iNextRno = b-fg-rctd.r-no.

    FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
    IF AVAILABLE fg-rcpth AND fg-rcpth.r-no GT iNextRno THEN iNextRno = fg-rcpth.r-no.

    DO WHILE TRUE:
        iNextRno = iNextRno + 1.
        FIND FIRST fg-rcpth WHERE fg-rcpth.r-no EQ iNextRno USE-INDEX r-no NO-LOCK NO-ERROR.
        IF AVAILABLE fg-rcpth THEN NEXT.
        FIND FIRST b-fg-rctd WHERE b-fg-rctd.r-no EQ iNextRno USE-INDEX fg-rctd NO-LOCK NO-ERROR.
        IF AVAILABLE b-fg-rctd THEN NEXT.
        LEAVE.
    END.
    
    FOR EACH ttCycleCountCompare NO-LOCK 
        WHERE lNotScanned = TRUE,    
        FIRST fg-bin NO-LOCK 
        WHERE /* fg-bin.r-no   EQ ttCycleCountCompare.iSequence    
        AND */ fg-bin.company EQ  ttCycleCountCompare.cCompany
        AND fg-bin.i-no    EQ ttCycleCountCompare.cFGItemID   
        AND fg-bin.tag     EQ ttCycleCountCompare.cTag        
        AND fg-bin.loc     EQ  ttCycleCountCompare.cSysLoc    
        AND fg-bin.loc-bin EQ  ttCycleCountCompare.cSysLocBin    
        :
            
        FIND FIRST itemfg WHERE itemfg.company = fg-bin.company
            AND itemfg.i-no = fg-bin.i-no NO-LOCK NO-ERROR.
        CREATE fg-rctd.
        ASSIGN 
            fg-rctd.r-no       = iNextRNo
            fg-rctd.loc        = fg-bin.loc
            fg-rctd.loc-bin    = fg-bin.loc-bin
            fg-rctd.company    = fg-bin.company
            fg-rctd.rita-code  = "C"
            fg-rctd.s-num      = 0
            fg-rctd.rct-date   = dTransDate
            fg-rctd.trans-time = TIME
            fg-rctd.qty-case   = (fg-bin.case-count)
            fg-rctd.cases-unit = (fg-bin.cases-unit)
            fg-rctd.i-no       = fg-bin.i-no
            fg-rctd.i-name     = itemfg.i-name
            fg-rctd.job-no     = fg-bin.job-no
            fg-rctd.job-no2    = fg-bin.job-no2
            fg-rctd.tag        = fg-bin.tag
            fg-rctd.cust-no    = fg-bin.cust-no
            fg-rctd.updated-by = USERID("ASI")
            .
        
        ASSIGN 
            fg-rctd.std-cost     = fg-bin.qty /
                                     (IF fg-bin.pur-uom EQ "M" THEN 1000 ELSE 1) *
                                     fg-bin.std-tot-cost
            fg-rctd.cases        = 0
            fg-rctd.partial      = 0
            fg-rctd.t-qty        = 0
            fg-rctd.units-pallet = 1 /* normal default */
            fg-rctd.cost-uom     = fg-bin.pur-uom.
            
        FIND FIRST fg-rdtlh WHERE
            fg-rdtlh.company = fg-bin.company AND
            fg-rdtlh.tag = cTag AND
            fg-rdtlh.rita-code = "R"
            USE-INDEX tag
            NO-LOCK NO-ERROR.

        IF AVAILABLE fg-rdtlh THEN 
        DO:

            FIND FIRST fg-rcpth NO-LOCK 
                WHERE fg-rcpth.r-no EQ  fg-rdtlh.r-no 
                NO-ERROR.
            IF AVAILABLE fg-rcpth THEN 
            DO:
                ASSIGN 
                    fg-rctd.po-no   = fg-rcpth.po-no
                    fg-rctd.po-line = MAX(fg-rcpth.po-line, 1) /* verify this, make sure scan does this */
                    fg-rctd.job-no  = fg-rcpth.job-no
                    fg-rctd.job-no2 = fg-rcpth.job-no2.
                RELEASE fg-rcpth.
            END.
            
            RELEASE fg-rdtlh.
        END.
                                        
        IF fg-rctd.pur-uom = "" THEN
            fg-rctd.pur-uom = fg-rctd.cost-uom.
        IF fg-rctd.ext-cost EQ ? THEN fg-rctd.ext-cost = 0.
        IF fg-rctd.cost     EQ ? THEN fg-rctd.cost = 0.
        iNextRno = iNextRno + 1.
  
    END.  /* for each fg-bin*/


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
     Notes:  Should be merged with fg/rep/fg-cst1N.i
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprBinRow AS ROWID NO-UNDO.
    DEFINE INPUT  PARAMETER ipdQty AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdMSF AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCost AS DECIMAL NO-UNDO.
    
    DEFINE VARIABLE v-ext         AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lv-sell-price LIKE itemfg.sell-price NO-UNDO.
    DEFINE VARIABLE lv-sell-uom   LIKE itemfg.sell-uom NO-UNDO.
    DEFINE VARIABLE lv-case-count LIKE itemfg.case-count NO-UNDO.
    
    FIND FIRST fg-bin NO-LOCK
        WHERE ROWID(fg-bin) EQ iprBinRow
        NO-ERROR.  
    IF NOT AVAILABLE fg-bin THEN 
        RETURN.
    FIND FIRST itemfg NO-LOCK 
        WHERE itemfg.company EQ fg-bin.company
          AND itemfg.i-no    EQ fg-bin.i-no
        NO-ERROR. 
    IF NOT AVAILABLE itemfg THEN 
        RETURN.
    ASSIGN       
        opdCost = fg-bin.std-tot-cost * ipdQty
        .

    /* Calculate Cost */
    IF fg-bin.pur-uom EQ "CS" AND fg-bin.case-count NE 0 THEN
        opdCost = opdCost / fg-bin.case-count.
    ELSE
        IF fg-bin.pur-uom EQ "L" THEN opdCost = opdCost / ipdQty.
        ELSE 
        DO:
            FIND FIRST uom NO-LOCK
                WHERE uom.uom  EQ itemfg.prod-uom
                AND uom.mult NE 0
                NO-ERROR.
            IF AVAILABLE uom THEN opdCost  = opdCost / uom.mult.
            ELSE opdCost = opdCost  / 1000.
        END.

    ASSIGN
        lv-sell-price = itemfg.sell-price
        lv-sell-uom   = itemfg.sell-uom
        lv-case-count = itemfg.case-count
        .

    IF TRIM(fg-bin.job-no) NE "" THEN
        FOR EACH job-hdr
            WHERE job-hdr.company EQ fg-bin.company
            AND job-hdr.job-no  EQ fg-bin.job-no
            AND job-hdr.job-no2 EQ fg-bin.job-no2
            AND job-hdr.i-no    EQ fg-bin.i-no
            AND job-hdr.ord-no  NE 0
            USE-INDEX job-no NO-LOCK,
            FIRST oe-ordl
            WHERE oe-ordl.company EQ job-hdr.company
            AND oe-ordl.ord-no  EQ job-hdr.ord-no
            AND oe-ordl.i-no    EQ job-hdr.i-no
            AND oe-ordl.job-no  EQ job-hdr.job-no
            AND oe-ordl.job-no2 EQ job-hdr.job-no2
            AND (oe-ordl.pr-uom NE "CS" OR oe-ordl.cas-cnt NE 0)
            USE-INDEX item-ord NO-LOCK
            BY job-hdr.ord-no DESCENDING
            :
            ASSIGN
                lv-sell-price = oe-ordl.price
                lv-sell-uom   = oe-ordl.pr-uom
                lv-case-count = oe-ordl.cas-cnt
                .
            LEAVE.
        END.

    /* Calculate Selling Price */
    IF lv-sell-uom EQ "CS" AND lv-case-count NE 0 THEN
        v-ext = (ipdQty * lv-sell-price) / lv-case-count.
    ELSE 
    DO:
        FIND FIRST uom NO-LOCK
            WHERE uom.uom  EQ lv-sell-uom
            AND uom.mult NE 0
            NO-ERROR
            .
        v-ext = ipdQty * lv-sell-price /
            (IF AVAILABLE uom THEN uom.mult ELSE 1000).
    END.

    IF itemfg.sell-uom EQ "L" THEN
        IF ipdQty LE 0 THEN v-ext = 0.
        ELSE v-ext = lv-sell-price.
          
    ASSIGN
        opdMSF = ipdQty * itemfg.t-sqft / 1000
        v-ext = ROUND(v-ext,2).

    IF ipdQty EQ ? THEN ipdQty = 0.
    IF opdMSF EQ ? THEN opdMSF = 0.
    IF opdCost EQ ? THEN opdCost = 0.
    IF v-ext EQ ? THEN v-ext = 0.
            
    ipdQty        = 0.


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

    INPUT STREAM sIn FROM VALUE(gcSnapshotFile).
    REPEAT: 
        CREATE ttSnapshot.
        IMPORT STREAM sIn DELIMITER "," ttSnapShot.
    END.
    INPUT STREAM sIn CLOSE.    
    

END PROCEDURE.

PROCEDURE postFG:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFGItemStart AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFGItemEnd AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcWhseStart AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcWhseEnd AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcBinStart AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcBinEnd AS CHARACTER NO-UNDO.
        
    DEFINE VARIABLE lDupsExist  AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lRemoveZero AS LOGICAL NO-UNDO.
    
    MESSAGE 'Remove all zero counts (all locations)?' SKIP
        VIEW-AS ALERT-BOX
        QUESTION BUTTONS YES-NO UPDATE lRemoveZero.
    IF lRemoveZero THEN 
        RUN pRemoveZeroCounts .

    RUN pCheckCountDups (OUTPUT lDupsExist).
    IF lDupsExist THEN 
        RETURN.

    RUN pCreateZeroCount.
    RUN pCreateTransfers.

    RUN pPostTransfers.

    RUN pRemoveMatches (ipcCompany, ipcFGItemStart, ipcFGItemEnd, ipcWhseStart,ipcWhseEnd, 
        ipcBinStart, ipcBinEnd).

    RUN pPostCounts (ipcCompany, ipcFGItemStart, ipcFGItemEnd, ipcWhseStart,ipcWhseEnd, 
        ipcBinStart, ipcBinEnd).
    MESSAGE "Posting Complete"
        VIEW-AS ALERT-BOX.
END PROCEDURE.

PROCEDURE pPostCounts:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:  Modify for FG
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFGItemStart AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFGItemEnd AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcWhseStart AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcWhseEnd AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcBinStart AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcBinEnd AS CHARACTER NO-UNDO.
        
    DEFINE VARIABLE save_id       AS RECID.
    DEFINE VARIABLE v-qty-onh     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-temp-cost   AS DECIMAL   FORMAT "->>>>>9.99" NO-UNDO.
    DEFINE VARIABLE time_stamp    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-cum-qty     AS DECIMAL   FORMAT "->>>>>>9" NO-UNDO.
    DEFINE VARIABLE v-tot-value   AS DECIMAL   FORMAT "->>>,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE v-sell-price  LIKE itemfg.sell-price .
    DEFINE VARIABLE v-tot-price   AS DECIMAL   FORMAT "->>>,>>>,>>9.99".
    DEFINE VARIABLE v-item-tot    AS DECIMAL   FORMAT "->>>,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE v-std-cost    AS DECIMAL   FORMAT ">>>,>>9.99<<" NO-UNDO.
    DEFINE VARIABLE v-q-adj-ytd   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-adj-qty     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-dscr        LIKE account.dscr NO-UNDO.
    DEFINE VARIABLE v-disp-actnum LIKE account.actnum NO-UNDO.
    DEFINE VARIABLE v-disp-amt    AS DECIMAL   FORMAT ">>,>>>,>>9.99cr" NO-UNDO.
    DEFINE VARIABLE v-cost        LIKE itemfg.std-tot-cost EXTENT 4 NO-UNDO.
    DEFINE VARIABLE v-uom         LIKE itemfg.prod-uom NO-UNDO.
    DEFINE VARIABLE begin_userid  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE end_userid    AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER b-fg-rctd FOR fg-rctd.
    DEFINE BUFFER b-itemfg  FOR itemfg.
    DEFINE BUFFER b-fg-bin  FOR fg-bin.
    DEFINE BUFFER b2-fg-bin FOR fg-bin.
    DEFINE VARIABLE v-post-date AS DATE INIT TODAY NO-UNDO.
    DEFINE VARIABLE v-gl        AS LOG  INIT NO NO-UNDO.
    {sys/form/r-top.i}
    ASSIGN 
        begin_userid = ""
        end_userid   = "zzzzzzzzzzzzzzz"
        .
    EMPTY TEMP-TABLE w-fg-rctd.

    postit:
    DO TRANSACTION ON ERROR UNDO postit, LEAVE postit:
      
        FOR EACH fg-rctd
            WHERE fg-rctd.company EQ cocode
            AND fg-rctd.rita-code EQ "C"   
            AND fg-rctd.tag NE ""
            AND fg-rctd.i-no GE ipcFGItemStart
            AND fg-rctd.i-no LE ipcFGItemEnd
            AND fg-rctd.loc GE ipcWhseStart
            AND fg-rctd.loc LE ipcWhseEnd
            AND fg-rctd.loc-bin GE ipcBinStart
            AND fg-rctd.loc-bin LE ipcBinEnd
            AND fg-rctd.qty NE 0  
            NO-LOCK,  
            FIRST itemfg
            WHERE itemfg.company EQ cocode
            AND itemfg.i-no    EQ fg-rctd.i-no
            AND itemfg.isaset
            AND itemfg.alloc   
            NO-LOCK:

            RUN fg/fullset.p (ROWID(itemfg)).

            FOR EACH tt-fg-set,
                FIRST b-itemfg
                WHERE b-itemfg.company EQ cocode
                AND b-itemfg.i-no    EQ tt-fg-set.part-no
                NO-LOCK:

                x = 1.
                FOR EACH w-fg-rctd BY w-fg-rctd.r-no DESCENDING:
                    LEAVE.
                END.
                IF AVAILABLE w-fg-rctd THEN x = w-fg-rctd.r-no + 1.
                FOR EACH b-fg-rctd NO-LOCK BY b-fg-rctd.r-no DESCENDING:
                    IF b-fg-rctd.r-no GE X THEN X = b-fg-rctd.r-no + 1.
                    LEAVE.
                END.

                FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
                IF AVAILABLE fg-rcpth AND fg-rcpth.r-no GE x THEN x = fg-rcpth.r-no + 1.

                CREATE w-fg-rctd.
                BUFFER-COPY fg-rctd TO w-fg-rctd
                    ASSIGN
                    w-fg-rctd.i-no   = b-itemfg.i-no
                    w-fg-rctd.i-name = b-itemfg.i-name
                    w-fg-rctd.r-no   = x.

                FIND FIRST fg-bin
                    WHERE fg-bin.company EQ cocode
                    AND fg-bin.i-no    EQ itemfg.i-no
                    AND fg-bin.loc     EQ fg-rctd.loc
                    AND fg-bin.loc-bin EQ fg-rctd.loc-bin
                    AND fg-bin.tag     EQ fg-rctd.tag
                    AND fg-bin.job-no  EQ fg-rctd.job-no
                    AND fg-bin.job-no2 EQ fg-rctd.job-no2
                    AND fg-bin.cust-no EQ fg-rctd.cust-no
                    USE-INDEX co-ino NO-ERROR.
                v-adj-qty = (IF AVAILABLE fg-bin THEN fg-bin.qty ELSE 0) * tt-fg-set.part-qty-dec.

                FIND FIRST fg-bin
                    WHERE fg-bin.company EQ cocode
                    AND fg-bin.i-no    EQ b-itemfg.i-no
                    AND fg-bin.loc     EQ fg-rctd.loc
                    AND fg-bin.loc-bin EQ fg-rctd.loc-bin
                    AND fg-bin.tag     EQ fg-rctd.tag
                    AND fg-bin.job-no  EQ fg-rctd.job-no
                    AND fg-bin.job-no2 EQ fg-rctd.job-no2
                    AND fg-bin.cust-no EQ fg-rctd.cust-no
                    USE-INDEX co-ino NO-ERROR.
                v-adj-qty = (IF AVAILABLE fg-bin THEN fg-bin.qty ELSE 0) - v-adj-qty.

                IF v-adj-qty LT 0 THEN v-adj-qty = 0.

                ASSIGN 
                    w-fg-rctd.t-qty = (fg-rctd.t-qty * tt-fg-set.part-qty-dec) + v-adj-qty.
            END. /* each tt-set */
        END. /* each fg-rctd */
  
        {fg/fg-cpost.i w-}

        {fg/fg-cpostRange.i}

        IF v-gl THEN 
        DO:
            /** GET next G/L TRANS. POSTING # **/
            /* gdm - 11050906 */
            REPEAT:
                FIND FIRST gl-ctrl EXCLUSIVE-LOCK
                    WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
                IF AVAILABLE gl-ctrl THEN 
                DO:
                    ASSIGN 
                        v-trnum       = gl-ctrl.trnum + 1
                        gl-ctrl.trnum = v-trnum.
                    FIND CURRENT gl-ctrl NO-LOCK NO-ERROR.
                    LEAVE.
                END. /* IF AVAIL gl-ctrl */
            END. /* REPEAT */
            /* gdm - 11050906 */

            FOR EACH work-job BREAK BY work-job.actnum:
                CREATE gltrans.
                ASSIGN
                    gltrans.company = cocode
                    gltrans.actnum  = work-job.actnum
                    gltrans.jrnl    = "OEINV"
                    gltrans.tr-date = udate
                    gltrans.period  = uperiod
                    gltrans.trnum   = v-trnum.

                IF work-job.fg THEN
                    ASSIGN
                        gltrans.tr-amt  = - work-job.amt
                        gltrans.tr-dscr = "ORDER ENTRY INVOICE FG".
                ELSE
                    ASSIGN
                        gltrans.tr-amt  = work-job.amt
                        gltrans.tr-dscr = "ORDER ENTRY INVOICE COGS".
            END. /* each work-job */
        END.
    END. /* postit */

    SESSION:SET-WAIT-STATE("").


END PROCEDURE.

PROCEDURE pPostTransfers:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:  Modify for FG
    ------------------------------------------------------------------------------*/
    MESSAGE "Posting all transfers (all locations).  Press OK to continue."
        VIEW-AS ALERT-BOX.
    RUN fg/fgpstall.w (?, "T").

END PROCEDURE.

PROCEDURE pRemoveMatches:
    /*------------------------------------------------------------------------------
     Purpose: Delete cycle counts that match quantity and location
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFGItemStart AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFGItemEnd AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcWhseStart AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcWhseEnd AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcBinStart AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcBinEnd AS CHARACTER NO-UNDO.
        
    FOR EACH ttCycleCountCompare NO-LOCK 
        WHERE ttCycleCountCompare.lMatch = TRUE
        AND ttCycleCountCompare.cFGItem     GE ipcFGItemStart
        AND ttCycleCountCompare.cFgItem     LE ipcFGItemEnd
        AND IF ttCycleCountCompare.cScanLoc GT "" THEN 
        (
        ttCycleCountCompare.cScanLoc         GE ipcWhseStart
        AND ttCycleCountCompare.cScanLoc     LE ipcWhseEnd
        AND ttCycleCountCompare.cScanLocBin  GE ipcBinStart
        AND ttCycleCountCompare.cScanLocBin  LE ipcBinEnd
        )
        ELSE 
        (ttCycleCountCompare.cSysLoc        GE ipcWhseStart
        AND ttCycleCountCompare.cSysLoc     LE ipcWhseEnd
        AND ttCycleCountCompare.cSysLocBin  GE ipcBinStart
        AND ttCycleCountCompare.cSysLocBin  LE ipcBinEnd) 
        ,    
        EACH fg-rctd EXCLUSIVE-LOCK 
        WHERE fg-rctd.company  EQ ttCycleCountCompare.cCompany 
        AND fg-rctd.i-no     EQ ttCycleCountCompare.cFGItemID  
        AND fg-rctd.tag      EQ ttCycleCountCompare.cTag        
        AND fg-rctd.loc      EQ ttCycleCountCompare.cScanLoc    
        AND fg-rctd.loc-bin  EQ ttCycleCountCompare.cScanLocBin  
        AND fg-rctd.qty      EQ ttCycleCountCompare.dScanQty    
        AND fg-rctd.r-no     EQ ttCycleCountCompare.iSequence
        
       
        :
        DELETE fg-rctd.
    END. 

END PROCEDURE.

PROCEDURE pRemoveZeroCounts:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    FOR EACH fg-rctd EXCLUSIVE-LOCK 
        WHERE fg-rctd.company EQ cocode
        AND fg-rctd.rita-code EQ "C"
        AND fg-rctd.qty EQ 0
        :
        DELETE fg-rctd.
    END.

END PROCEDURE.

PROCEDURE reportComparison:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcOutputFile AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFGItemStart AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFGItemEnd AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcWhseStart AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcWhseEnd AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcBinStart AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcBinEnd AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplScansOnly AS LOGICAL NO-UNDO.
    DEFINE INPUT  PARAMETER iplComplete AS LOGICAL NO-UNDO.
    DEFINE INPUT  PARAMETER iplQtyChanged AS LOGICAL NO-UNDO.
    DEFINE INPUT  PARAMETER iplSnapshotOnly AS LOGICAL NO-UNDO.
    DEFINE INPUT  PARAMETER iplDupsInSnapshot AS LOGICAL NO-UNDO.
    DEFINE INPUT  PARAMETER iplDupsInScan AS LOGICAL NO-UNDO.
    DEFINE VARIABLE setFromHistory AS LOGICAL NO-UNDO.
        
    STATUS DEFAULT "Import Snapshot" .       
    RUN pImportSnapshot.
    
    STATUS DEFAULT "Build Compare Table". 
    RUN pBuildCompareTable(ipcCompany, ipcFGItemStart, ipcFGItemEnd, ipcWhseStart,ipcWhseEnd, 
        ipcBinStart, ipcBinEnd, YES /* scans only */).
    gcOutputFile = ipcOutputFile.
    
    STATUS DEFAULT "Exporting Report".
    RUN pExportTempTable(TEMP-TABLE ttCycleCountCompare:HANDLE, gcOutputFile, YES /* header */, iplComplete, 
        iplQtyChanged, iplSnapshotOnly, iplDupsInSnapshot, iplDupsInScan).
    OS-COMMAND SILENT  VALUE(gcOutputFile).
    STATUS DEFAULT "Done".
    
    MESSAGE 'Post Counts?' SKIP
        VIEW-AS ALERT-BOX
        QUESTION BUTTONS YES-NO UPDATE setFromHistory.
        
    IF setFromHistory THEN 
        RUN postFG (ipcCompany, ipcFGItemStart, ipcFGItemEnd, ipcWhseStart,ipcWhseEnd, 
            ipcBinStart, ipcBinEnd). 
        
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
    IF  ipcLocChanged THEN cResult = "Transfer auto-created".
        
    IF  iplQtyChanged THEN cResult = "Count Posted".
        
    IF  ipcLocChanged AND iplQtyChanged THEN 
        cResult = "Transfer auto-created,Count Posted".
        
    /* Not in snapshot so just post count */
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
