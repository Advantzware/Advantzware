
/*------------------------------------------------------------------------
    File        : InventoryProcs.p
    Purpose     : 

    Syntax      :

    Description : All procedures for creating and printing Loadtags for FG, RM, and WIP

    Author(s)   : BV
    Created     : Sun Mar 03 18:31:30 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{Inventory/ttInventory.i SHARED}
{custom/formtext.i NEW}
{fg/invrecpt.i NEW}
{fg/fgPostBatch.i}

DEFINE VARIABLE giLengthUniquePrefix       AS INTEGER   INITIAL 20.
DEFINE VARIABLE giLengthAlias              AS INTEGER   INITIAL 25.

DEFINE VARIABLE giIDTemp                   AS INTEGER. /*TESTING ONLY DELETE BEFORE COMMIT*/

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */

FUNCTION fCanDeleteInventoryStock RETURNS LOGICAL 
	(ipcInventoryStockID AS CHARACTER) FORWARD.

FUNCTION fGetNextSnapshotID RETURNS INTEGER PRIVATE
    (  ) FORWARD.

FUNCTION fGetNextStockIDAlias RETURNS CHARACTER 
    (ipcCompany AS CHARACTER,
    ipcUniquePrefix AS CHARACTER) FORWARD.

FUNCTION fGetNextStockID RETURNS CHARACTER 
    (ipcType AS CHARACTER) FORWARD.

FUNCTION fGetNextTransactionID RETURNS INTEGER
    (  ) FORWARD.

FUNCTION fGetNumberSuffix RETURNS INTEGER PRIVATE
    (ipcFullText AS CHARACTER,
    ipiStartChar AS INTEGER) FORWARD.

FUNCTION fGetSnapshotCompareStatus RETURNS CHARACTER PRIVATE
    (ipcCompany AS CHARACTER,
     ipcTag AS CHARACTER,
     ipdQuantity AS DECIMAL,
     ipcWarehouseID AS CHARACTER,
     ipcLocationID AS CHARACTER) FORWARD.

FUNCTION fGetRowBGColor RETURNS INTEGER PRIVATE
    (ipcInventoryStatus AS CHARACTER) FORWARD.

FUNCTION fCalculateQuantitySubUnits RETURNS DECIMAL
    (ipdQuantityTotalQty AS DECIMAL,
     ipdQuantitySubUnitCount AS DECIMAL) FORWARD.

FUNCTION fCalculateQuantityUnitCount RETURNS DECIMAL
    (ipdQuantitySubUnitCount AS DECIMAL, 
     ipdQuantitySubUnitsPerUnit AS DECIMAL) FORWARD.

FUNCTION fCalculateQuantityUnits RETURNS INTEGER
    (ipdQuantitySubUnits AS DECIMAL, 
     ipdQuantitySubUnitsPerUnit AS DECIMAL,
     ipdQuantityPartialSubUnit AS DECIMAL) FORWARD.

FUNCTION fCalculateQuantityPartialSubUnit RETURNS DECIMAL
    (ipdQuantityTotal AS DECIMAL, 
     ipdQuantitySubUnits AS DECIMAL,
     ipdQuantitySubUnitCount AS DECIMAL) FORWARD.
     
FUNCTION fCalculateQuantityTotal RETURNS DECIMAL
    (ipdQuantitySubUnits AS DECIMAL, 
     ipdSubUnitCount AS DECIMAL,
     ipdQuantityPartialSubUnit AS DECIMAL) FORWARD.

FUNCTION fCalculateTagCountInTTbrowse RETURNS INTEGER
    (ipcInventoryStatus AS CHARACTER) FORWARD.

FUNCTION fGetVendorTagFromLoadTag RETURNS CHARACTER
    (ipcCompany  AS CHARACTER,
     iplItemType AS LOGICAL,
     ipcTag      AS CHARACTER) FORWARD.            
/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

PROCEDURE Inventory_CheckPOUnderOver:
/*------------------------------------------------------------------------------
 Purpose: To Check under under/over quantities for PO
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcJobNo       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiJobNo2      AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcItem        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcPoNo        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiReceivedQty AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER lCopied        AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipcFGUnderOver AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiFGUnderOver AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER iplFGUnderOver AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipriFGRctd     AS ROWID     NO-UNDO.
    
    DEFINE VARIABLE dReceivedQty AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lIsUOMEA     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE dPostedPOQty AS DECIMAL   NO-UNDO.
    
    
    FOR EACH fg-rctd NO-LOCK
        WHERE fg-rctd.company     EQ ipcCompany 
          AND(fg-rctd.rita-code   EQ "R" OR fg-rctd.rita-code EQ "E")
          AND TRIM(fg-rctd.job-no)EQ ipcJobNo
          AND fg-rctd.job-no2     EQ ipiJobNo2
          AND fg-rctd.i-no        EQ ipcItem
          AND fg-rctd.po-no       EQ ipcPoNo
          AND(ROWID(fg-rctd)      NE ipriFgRctd OR lCopied):
        dReceivedQty = dReceivedQty + fg-rctd.t-qty.     
    END.

    dReceivedQty = dReceivedQty + ipiReceivedQty.
    
    FIND FIRST po-ordl NO-LOCK 
         WHERE po-ordl.company  EQ ipcCompany
          AND po-ordl.po-no     EQ INTEGER(ipcPoNo)
          AND po-ordl.i-no      EQ ipcItem
          AND po-ordl.job-no    EQ ipcJobNo
          AND po-ordl.job-no2   EQ ipiJobNo2
          AND po-ordl.item-type EQ NO
        NO-ERROR.
 
    IF AVAILABLE po-ordl THEN DO:
        RUN Inventory_GetReceivedQuantityPO(
            INPUT ipcCompany,
            INPUT ipcJobNo,
            INPUT ipiJobNo2,
            INPUT ipcItem,
            INPUT ipcPoNo,
            OUTPUT dPostedPOQty
            ).
        
        dReceivedQty = dReceivedQty + dPostedPOQty.
  
        RUN sys/ref/ea-um-fg.p(
            INPUT  po-ordl.pr-qty-uom, 
            OUTPUT lIsUOMEA
            ).
        IF NOT lIsUOMEA THEN
            RUN sys/ref/convquom.p(
                INPUT "EA",
                INPUT po-ordl.pr-qty-uom,
                INPUT 0,
                INPUT 0,
                INPUT 0,
                INPUT 0,
                INPUT-OUTPUT dReceivedQty
                ).
        IF iplFGUnderOver THEN DO:
            IF(ipcFGUnderOver EQ "OverRuns Only" OR ipcFGUnderOver EQ "UnderRuns and OverRun") AND 
                dReceivedQty GT po-ordl.ord-qty * (1 + (po-ordl.over-pct / 100)) THEN DO:
                IF ipiFGUnderOver EQ 1 THEN DO: 
                   MESSAGE "The PO Quantity entered is more than the" STRING(po-ordl.over-pct,">>9.99%") SKIP 
                      "Overrun allowed for this PO line Item, and excess overruns are not allowed."
                      VIEW-AS ALERT-BOX ERROR. 
                   RETURN ERROR.
                END.           
                ELSE 
                   MESSAGE "The PO Quantity entered is more than the" STRING(po-ordl.over-pct,">>9.99%") SKIP 
                       "Overrun allowed for this PO line Item..."
                       VIEW-AS ALERT-BOX WARNING. 
            END.  
            ELSE IF(ipcFGUnderOver EQ "UnderRuns Only" OR ipcFGUnderOver EQ "UnderRuns and OverRun") AND 
                     dReceivedQty LT po-ordl.ord-qty * (1 - (po-ordl.under-pct / 100)) THEN DO:
                IF ipiFGUnderOver EQ 1 THEN DO:   
                    MESSAGE "The PO Quantity entered is less than the" STRING(po-ordl.under-pct,">>9.99%") SKIP 
                        "Underrun allowed for this PO line Item, and excess underruns are not allowed."
                        VIEW-AS ALERT-BOX ERROR. 
                    RETURN ERROR.        
                END.         
                ELSE 
                   MESSAGE "The PO Quantity entered is less than the" STRING(po-ordl.under-pct,">>9.99%") SKIP 
                       "Underrun allowed for this PO line Item..."
                       VIEW-AS ALERT-BOX WARNING.             
            END.                                 
        END.                    
    END.       
END PROCEDURE.

PROCEDURE Inventory_CheckJobUnderOver:
/*------------------------------------------------------------------------------
 Purpose: To Check under under/over quantities for PO
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcJobNo       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiJobNo2      AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcItem        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcPoNo        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiReceivedQty AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER lCopied        AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipcFGUnderOver AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiFGUnderOver AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER iplFGUnderOver AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipriFGRctd     AS ROWID     NO-UNDO.
   
    DEFINE VARIABLE dReceivedQty   AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dPostedJobQty  AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dOverPct       AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dUnderPct      AS DECIMAL NO-UNDO.
    
    FOR EACH fg-rctd NO-LOCK
        WHERE fg-rctd.company     EQ ipcCompany 
          AND(fg-rctd.rita-code   EQ "R" OR fg-rctd.rita-code EQ "E")
          AND TRIM(fg-rctd.job-no)EQ ipcJobNo
          AND fg-rctd.job-no2     EQ ipiJobNo2
          AND fg-rctd.i-no        EQ ipcItem
          AND fg-rctd.po-no       EQ ipcPoNo
          AND(ROWID(fg-rctd)      NE ipriFgRctd OR lCopied):
        dReceivedQty = dReceivedQty + fg-rctd.t-qty.     
    END.
    dReceivedQty = dReceivedQty + ipiReceivedQty.
    
    FIND FIRST job-hdr NO-LOCK
         WHERE job-hdr.company EQ ipcCompany                       
           AND job-hdr.i-no    EQ ipcItem
           AND job-hdr.job-no  EQ ipcJobNo
           AND job-hdr.job-no2 EQ ipiJobNo2
        NO-ERROR.
    IF AVAILABLE job-hdr THEN DO: 
        
        RUN Inventory_GetReceivedQuantityJob(
            INPUT  ipcCompany,
            INPUT  ipcJobNo,
            INPUT  ipiJobNo2,
            INPUT  ipcItem,
            OUTPUT dPostedJobQty
            ).
        dReceivedQty = dReceivedQty + dPostedJobQty.
            
        FIND FIRST oe-ordl NO-LOCK
             WHERE oe-ordl.company EQ job-hdr.company
               AND oe-ordl.ord-no  EQ job-hdr.ord-no
               AND oe-ordl.i-no    EQ job-hdr.i-no
            NO-ERROR.
         FIND FIRST oe-ord NO-LOCK
              WHERE oe-ord.company EQ job-hdr.company
                AND oe-ord.ord-no  EQ job-hdr.ord-no
             NO-ERROR.
        ASSIGN     
            dOverPct = (IF AVAILABLE oe-ordl     THEN oe-ordl.over-pct 
                        ELSE IF AVAILABLE oe-ord THEN oe-ord.over-pct  
                        ELSE 0)                     
            dUnderPct = (IF AVAILABLE oe-ordl     THEN oe-ordl.under-pct 
                         ELSE IF AVAILABLE oe-ord THEN oe-ord.under-pct  
                         ELSE 0) 
            .
                        
        IF iplFGUnderOver THEN DO:
            IF (ipcFGUnderOver EQ "OverRuns Only" OR ipcFGUnderOver EQ "UnderRuns and OverRun") AND 
                dReceivedQty GT job-hdr.qty * (1 + dOverPct / 100) THEN DO:
                IF ipiFGUnderOver EQ 1 THEN DO:   
                    MESSAGE "The Job Quantity entered is more than the" STRING(dOverPct,">>9.99%") SKIP 
                          "Overrun allowed for this Job, and excess overruns are not allowed."
                          VIEW-AS ALERT-BOX ERROR. 
                          RETURN ERROR.
               END.        
               ELSE 
                   MESSAGE "The Job Quantity entered is more than the" STRING(dOverPct,">>9.99%") SKIP 
                       "Overrun allowed for this Job..."
                       VIEW-AS ALERT-BOX WARNING. 
            END.  
            ELSE IF (ipcFGUnderOver EQ "UnderRuns Only" OR ipcFGUnderOver EQ "UnderRuns and OverRun") AND 
                     dReceivedQty LT job-hdr.qty * (1 - dUnderPct / 100) THEN DO:
                IF ipiFGUnderOver EQ 1 THEN DO:   
                     MESSAGE "The Job Quantity entered is less than the" STRING(dUnderPct,">>9.99%") SKIP 
                        "Underrun allowed for this Job, and excess underruns are not allowed."
                        VIEW-AS ALERT-BOX ERROR.
                        RETURN ERROR.
                END.          
                ELSE 
                   MESSAGE "The Job Quantity entered is less than the" STRING(dUnderPct,">>9.99%") SKIP 
                       "Underrun allowed for this Job..."
                       VIEW-AS ALERT-BOX WARNING.             
            END.                                 
        END. 
    END.           
END PROCEDURE.

PROCEDURE Inventory_GetDefaultBin:
/*------------------------------------------------------------------------------
 Purpose: To get the default bin defined for a user in reftable
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER  ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcFGDefBin AS CHARACTER NO-UNDO.
    
    FIND FIRST reftable NO-LOCK
         WHERE reftable.company  EQ ipcCompany 
           AND reftable.reftable EQ "Xref"
           AND reftable.code     EQ USERID("ASI")
           AND reftable.loc      EQ "FGDefBin"
          NO-ERROR.

    IF AVAILABLE reftable THEN
        opcFGDefBin = reftable.code2.

END PROCEDURE.

PROCEDURE Inventory_GetDefaultWhse:
/*------------------------------------------------------------------------------
 Purpose: To get the default location defined for a user in reftable
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER  ipcCompany       AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcFGDefBin      AS CHARACTER NO-UNDO.
    
    FIND FIRST reftable NO-LOCK
         WHERE reftable.company  EQ ipcCompany 
           AND reftable.reftable EQ "Xref"
           AND reftable.code     EQ USERID("ASI")
           AND reftable.loc      EQ "FGDefWhse"
          NO-ERROR.

    IF AVAILABLE reftable THEN
            opcFGDefBin = reftable.code2.
    
END PROCEDURE.

PROCEDURE Inventory_GetReceivedQuantityJob:
/*------------------------------------------------------------------------------
 Purpose: Returns the Production Qty for a Given Job & Item
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcJobNo    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiJobNo2   AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItem     AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdQuantity AS DECIMAL   NO-UNDO.

    RUN fg/GetProductionQty.p(
        INPUT  ipcCompany,
        INPUT  ipcJobNo,
        INPUT  ipiJobNo2,
        INPUT  ipcItem,
        INPUT  NO,
        OUTPUT opdQuantity
        ).

END PROCEDURE.

PROCEDURE Inventory_GetReceivedQuantityPO:
/*------------------------------------------------------------------------------
 Purpose:  Returns the Production Qty for a Given PO & Item
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcJobNo    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiJobNo2   AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItem     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcPoNo     AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdQuantity AS DECIMAL   NO-UNDO.
    
   FOR EACH fg-rcpth FIELDS(r-no rita-code) NO-LOCK 
       WHERE fg-rcpth.company   EQ ipcCompany
         AND fg-rcpth.job-no    EQ ipcJobNo
         AND fg-rcpth.job-no2   EQ ipiJobNo2
         AND fg-rcpth.po-no     EQ ipcPoNo
         AND fg-rcpth.i-no      EQ ipcItem
         AND fg-rcpth.rita-code EQ "R" 
       USE-INDEX item-po,
       EACH fg-rdtlh FIELDS(qty)NO-LOCK
       WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
         AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code:
       opdQuantity = opdQuantity + fg-rdtlh.qty.
    END.  /*each fg history*/    

END PROCEDURE.

PROCEDURE Inventory_GetStatusDescription:
/*------------------------------------------------------------------------------
 Purpose: To get the status description based on status ID
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER  ipcStatusID    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcDescription AS CHARACTER NO-UNDO.
    
    FIND FIRST inventoryStatusType NO-LOCK
         WHERE inventoryStatusType.statusID EQ ipcStatusID
         NO-ERROR.
    
    IF AVAILABLE inventoryStatusType
        THEN opcDescription = inventoryStatusType.description.
        

END PROCEDURE.

PROCEDURE Inventory_UpdateFGBinStatusID:
/*------------------------------------------------------------------------------
 Purpose: Updates the FG statusID
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipriFGBin   AS ROWID     NO-UNDO.
    DEFINE INPUT  PARAMETER ipcStatusID AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess  AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage  AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-fg-bin FOR fg-bin.
    
    FIND FIRST bf-fg-bin EXCLUSIVE-LOCK
         WHERE ROWID(bf-fg-bin) EQ ipriFGBin
         NO-ERROR NO-WAIT.
    IF LOCKED bf-fg-bin THEN DO:
        ASSIGN
            oplSuccess = NO
            opcMessage = "fg-bin record locked"
            .            
        RETURN.    
    END.

    IF NOT AVAILABLE bf-fg-bin THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "Invalid fg-bin record"
            .
        RETURN. 
    END.
    
    ASSIGN
        bf-fg-bin.statusID = ipcStatusID
        oplSuccess         = TRUE
        opcMessage         = "Success"
        .
    
    RELEASE bf-fg-bin.
END PROCEDURE.

PROCEDURE Inventory_UpdateFGBinOnHold:
/*------------------------------------------------------------------------------
 Purpose: Updates the onHold status
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipriFGBin   AS ROWID     NO-UNDO.
    DEFINE INPUT  PARAMETER iplOnHold   AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess  AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage  AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-fg-bin FOR fg-bin.
    
    FIND FIRST bf-fg-bin EXCLUSIVE-LOCK
         WHERE ROWID(bf-fg-bin) EQ ipriFGBin
         NO-ERROR NO-WAIT.
    IF LOCKED bf-fg-bin THEN DO:
        ASSIGN
            oplSuccess = NO
            opcMessage = "fg-bin record locked"
            .            
        RETURN.    
    END.

    IF NOT AVAILABLE bf-fg-bin THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "Invalid fg-bin record"
            .
        RETURN. 
    END.
    
    ASSIGN
        bf-fg-bin.onHold   = iplOnHold
        oplSuccess         = TRUE
        opcMessage         = "Success"
        .
    
    RELEASE bf-fg-bin.
END PROCEDURE.

PROCEDURE Inventory_ValidateStatusID:
/*------------------------------------------------------------------------------
 Purpose: Verify if an input status is valid 
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcStatusID      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValidStatusID AS LOGICAL   NO-UNDO.
    
    oplValidStatusID = CAN-FIND(FIRST InventoryStatusType
                                WHERE InventoryStatusType.statusID EQ ipcStatusID).
END PROCEDURE.

PROCEDURE RecalculateQuantities:
/*------------------------------------------------------------------------------
 Purpose: To add the location into itemfg-loc based on history records and oe-rel
          and recalculate the quantities
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriItemFG AS ROWID     NO-UNDO.
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-itemFG FOR itemFG.
    
    FIND FIRST bf-itemFG NO-LOCK
         WHERE ROWID(bf-itemFG) EQ ipriItemFG
            NO-ERROR.

    IF AVAILABLE bf-itemfg THEN DO:      
        FOR EACH fg-rcpth NO-LOCK
            WHERE fg-rcpth.company   EQ ipcCompany
              AND fg-rcpth.i-no      EQ bf-itemFG.i-no       
              USE-INDEX tran,
            EACH fg-rdtlh NO-LOCK
            WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
              AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code 
            BREAK BY fg-rcpth.loc
                  BY fg-rcpth.po-no
                  BY fg-rcpth.job-no:
                   
            IF FIRST-OF(fg-rcpth.loc) 
                AND fg-rcpth.loc NE "" 
                AND NOT CAN-FIND (FIRST itemfg-loc 
                     WHERE itemfg-loc.company EQ fg-rcpth.company 
                       AND itemfg-loc.i-no    EQ bf-itemFG.i-no 
                       AND itemfg-loc.loc     EQ fg-rcpth.loc) THEN  
                                    
                RUN CreateItemFGLoc(
                    INPUT ipcCompany,
                    INPUT fg-rcpth.i-no,
                    INPUT fg-rcpth.loc
                    ).
                   
                    
            IF FIRST-OF(fg-rcpth.po-no) AND fg-rcpth.loc NE "" THEN DO: 
                FIND FIRST po-ord NO-LOCK 
                     WHERE po-ord.company EQ ipcCompany
                       AND po-ord.po-no   EQ INT(fg-rcpth.po-no)
                       NO-ERROR.
                       
                IF AVAILABLE po-ord 
                    AND po-ord.loc NE "" 
                    AND NOT CAN-FIND(FIRST itemfg-loc 
                        WHERE itemfg-loc.company  EQ ipcCompany
                          AND itemfg-loc.i-no     EQ fg-rcpth.i-no 
                          AND itemfg-loc.loc      EQ po-ord.loc) THEN
                                  
                    RUN CreateItemFGLoc(
                        INPUT ipcCompany,
                        INPUT fg-rcpth.i-no,
                        INPUT po-ord.loc
                        ).                                          
            END. 
            
            IF FIRST-OF(fg-rcpth.job-no) AND fg-rcpth.loc NE "" THEN DO: 
                FIND FIRST job-hdr NO-LOCK 
                     WHERE job-hdr.company EQ ipcCompany
                       AND job-hdr.i-no    EQ fg-rcpth.i-no
                       AND job-hdr.job-no  EQ fg-rcpth.job-no
                       AND job-hdr.job-no2 EQ fg-rcpth.job-no2
                       NO-ERROR.
                       
                IF AVAILABLE job-hdr 
                    AND job-hdr.loc NE "" 
                    AND NOT CAN-FIND(FIRST itemfg-loc
                        WHERE itemfg-loc.company EQ ipcCompany
                          AND itemfg-loc.i-no    EQ fg-rcpth.i-no
                          AND itemfg-loc.loc     EQ job-hdr.loc) THEN
                                   
                    RUN CreateItemFGLoc(
                        INPUT ipcCompany,
                        INPUT fg-rcpth.i-no,
                        INPUT job-hdr.loc
                        ). 
            END.                                 
        END. 
        
        FOR EACH oe-ordl NO-LOCK 
            WHERE oe-ordl.company EQ bf-itemFG.company 
              AND oe-ordl.opened  EQ YES 
              AND oe-ordl.i-no    EQ bf-itemFG.i-no 
              AND oe-ordl.stat    NE "C" 
              AND CAN-FIND(FIRST oe-ord OF oe-ordl 
                           WHERE oe-ord.type    NE "T" 
                             AND oe-ord.deleted EQ NO)      
            USE-INDEX item:
            IF oe-ordl.is-a-component THEN DO:
                FIND FIRST fg-set NO-LOCK 
                     WHERE fg-set.company EQ oe-ordl.company 
                       AND fg-set.part-no EQ oe-ordl.i-no
                        NO-ERROR.
            
                IF AVAILABLE fg-set THEN DO: 
                    FIND FIRST itemfg NO-LOCK 
                    WHERE itemfg.company EQ oe-ordl.company 
                      AND itemfg.i-no    EQ fg-set.set-no
                       NO-ERROR.
                    IF AVAILABLE bf-itemFG AND bf-itemFG.alloc NE YES THEN 
                        NEXT.              
                END.
            END. /* oe-ordl.is-a-component */   
            FOR EACH oe-rel NO-LOCK 
                WHERE oe-rel.company EQ oe-ordl.company 
                  AND oe-rel.ord-no  EQ oe-ordl.ord-no 
                  AND oe-rel.LINE    EQ oe-ordl.LINE 
                  AND oe-rel.stat    NE "Z" 
                  AND oe-rel.stat    NE "C":     
               IF NOT CAN-FIND(FIRST itemfg-loc 
                               WHERE itemfg-loc.company EQ ipcCompany 
                                 AND itemfg-loc.loc     EQ oe-rel.spare-char-1 
                                 AND itemfg-loc.i-no    EQ bf-itemFG.i-no) THEN
               RUN CreateItemFGLoc(
                   INPUT ipcCompany,
                   INPUT bf-itemfg.i-no,
                   INPUT oe-rel.spare-char-1
                   ).                                   
            END. /* EACH oe-rel */
        END. /* EACH oe-ordl */
             
        RUN fg/fg-calcbcst.p(
            INPUT ROWID(bf-itemFG)
            ).
        RUN fg/fg-mkbin.p(
            INPUT RECID(bf-itemFG)
            ).
        RUN fg/fg-reset.p(
            INPUT RECID(bf-itemfg)
            ).      
    END.
END PROCEDURE.

PROCEDURE BuildFGInventoryStockForItem:
    /*------------------------------------------------------------------------------
     Purpose: Create InventoryStock records from fg
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT        PARAMETER ipriFGItem           AS ROWID     NO-UNDO.
    DEFINE INPUT        PARAMETER ipdtAsOf             AS DATE      NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiCountOfProcessed AS INTEGER   NO-UNDO.
    
    RUN pBuildFGInventoryStockForItem (
            ipriFGItem, 
            ipdtAsOf, 
            INPUT-OUTPUT iopiCountOfProcessed, 
            NO, /* Purge */
            ""  /* Output Path */
            ).    
END PROCEDURE.

PROCEDURE BuildRMInventoryStockForItem:
    /*------------------------------------------------------------------------------
     Purpose: Create InventoryStock records from RM
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT        PARAMETER ipriItem             AS ROWID     NO-UNDO.
    DEFINE INPUT        PARAMETER ipdtAsOf             AS DATE      NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiCountOfProcessed AS INTEGER   NO-UNDO.
    
    RUN pBuildRMInventoryStockForItem (
            ipriItem, 
            ipdtAsOf, 
            INPUT-OUTPUT iopiCountOfProcessed, 
            NO, /* Purge */
            ""  /* Output Path */
            ).    
END PROCEDURE.

PROCEDURE CreateItemFGLoc:
/*------------------------------------------------------------------------------
 Purpose: To Create record in itemfg-loc table
 Notes:
------------------------------------------------------------------------------*/     
    DEFINE INPUT PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItem      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcLocation  AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-itemfg-loc FOR itemfg-loc.
    
    CREATE bf-itemfg-loc.
    ASSIGN
        bf-itemfg-loc.company = ipcCompany
        bf-itemfg-loc.i-no   = ipcItem
        bf-itemfg-loc.loc    = ipcLocation
        .
    RELEASE bf-itemfg-loc.

END PROCEDURE.

PROCEDURE pBuildFGInventoryStockForItem PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a FG Item, build bins - will purge and export too 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT        PARAMETER ipriFGItem           AS ROWID     NO-UNDO.
    DEFINE INPUT        PARAMETER ipdtAsOf             AS DATE      NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiCountOfProcessed AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER iplPurge             AS LOGICAL   NO-UNDO.
    DEFINE INPUT        PARAMETER ipcExportPath        AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lCreated   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage   AS CHARACTER NO-UNDO.
    
    FIND FIRST itemfg NO-LOCK
        WHERE ROWID(itemfg) EQ ipriFGItem
        NO-ERROR.
    IF AVAILABLE itemfg THEN 
    DO:
        FOR EACH  fg-rcpth NO-LOCK
            WHERE fg-rcpth.company    EQ itemfg.company
              AND fg-rcpth.i-no       EQ itemfg.i-no
              AND fg-rcpth.trans-date LE ipdtAsOf
            USE-INDEX tran,
            EACH  fg-rdtlh NO-LOCK
            WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
              AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
            BY fg-rcpth.trans-date
            BY fg-rdtlh.trans-time
            BY fg-rcpth.r-no
            :
/*             FIND FIRST inventoryStock */
/*                  WHERE inventoryStock.company      EQ fg-rcpth.company */
/*                    AND inventoryStock.fgItemID     EQ fg-rcpth.i-no */
/*                    AND inventoryStock.jobID        EQ fg-rcpth.job-no */
/*                    AND inventoryStock.jobID2       EQ fg-rcpth.job-no2 */
/*                    AND inventoryStock.warehouseID  EQ fg-rdtlh.loc */
/*                    AND inventoryStock.locationID   EQ fg-rdtlh.loc-bin */
/*                    AND inventoryStock.stockIDAlias EQ fg-rdtlh.tag */
/*                    AND inventoryStock.customerID   EQ fg-rdtlh.cust-no */
/*                  NO-ERROR. */
            FIND FIRST inventoryStock EXCLUSIVE-LOCK
                 WHERE inventoryStock.sourceID EQ fg-rdtlh.rec_key /* Unable to find a unique record. Temporarily using source ID */
                 NO-ERROR.
            IF NOT AVAILABLE inventoryStock THEN 
            DO:
                CREATE inventoryStock.
                ASSIGN
                    inventoryStock.company                    = fg-rcpth.company
                    inventoryStock.jobID                      = fg-rcpth.job-no
                    inventoryStock.jobID2                     = fg-rcpth.job-no2
                    inventoryStock.warehouseID                = fg-rdtlh.loc
                    inventoryStock.locationID                 = fg-rdtlh.loc-bin                            
                    inventoryStock.customerID                 = fg-rdtlh.cust-no
                    inventoryStock.fgItemID                   = fg-rcpth.i-no
                    inventoryStock.poID                       = INTEGER(fg-rcpth.po-no)
                    inventoryStock.createdTime                = fg-rcpth.trans-date
                    inventoryStock.quantityUOM                = gcFGUOM
                    inventoryStock.primaryID                  = inventoryStock.fgItemID
                    inventoryStock.itemType                   = gcItemTypeFG
                    inventoryStock.quantityPerSubUnit         = fg-rdtlh.qty-case
                    inventoryStock.quantityOfSubUnits         = fg-rdtlh.cases
                    inventoryStock.quantitySubUnitsPerUnit    = fg-rdtlh.stacks-unit
                    inventoryStock.quantity                   = fg-rdtlh.qty
                    inventoryStock.quantityPartial            = fg-rdtlh.partial
                    inventoryStock.quantityOfUnits            = fCalculateQuantityUnits (
                                                                    inventoryStock.quantityOfSubUnits,
                                                                    inventoryStock.quantitySubUnitsPerUnit,
                                                                    inventoryStock.quantityPartial
                                                                )
                    inventoryStock.quantityOfUnitsOriginal    = inventoryStock.quantityOfUnits
                    inventoryStock.quantityPartialOriginal    = inventoryStock.quantityPartial
                    inventoryStock.quantityOfSubUnitsOriginal = inventoryStock.quantityOfSubUnits
                    inventoryStock.costStandardPerUOM         = fg-rdtlh.cost
                    inventoryStock.createdTime                = DATETIME(fg-rdtlh.trans-date, fg-rdtlh.trans-time)
                    inventoryStock.createdBy                  = fg-rcpth.create-by
                    inventoryStock.lastTransBy                = fg-rcpth.update-by
                    inventoryStock.bolID                      = STRING(fg-rcpth.b-no)
                    inventoryStock.lot                        = fg-rdtlh.stack-code
                    inventoryStock.dimEachLen                 = itemfg.t-len
                    inventoryStock.dimEachWid                 = itemfg.t-wid
                    inventoryStock.dimEachDep                 = itemfg.t-dep
                    inventoryStock.inventoryStockLen          = itemfg.unitLength
                    inventoryStock.inventoryStockWid          = itemfg.unitWidth
                    inventoryStock.inventoryStockDep          = itemfg.unitHeight
                    inventoryStock.dimEachUOM                 = gcUOMInches
                    inventoryStock.inventoryStockUOM          = gcUOMInches
                    inventoryStock.basisWeight                = itemfg.weight-100
                    inventoryStock.basisWeightUOM             = gcUOMWeightBasis
                    inventoryStock.weightUOM                  = gcUOMWeight
                    inventoryStock.costStandardMat            = fg-rdtlh.std-mat-cost
                    inventoryStock.costStandardLab            = fg-rdtlh.std-lab-cost
                    inventoryStock.costStandardVOH            = fg-rdtlh.std-var-cost
                    inventoryStock.costStandardFOH            = fg-rdtlh.std-fix-cost
/*                     inventoryStock.sourceID                   = IF fg-rcpth.job-no NE "" THEN */
/*                                                                     FILL(" ",6 - LENGTH(LEFT-TRIM(TRIM(fg-rcpth.job-no)))) + STRING(fg-rcpth.job-no2,"99") */
/*                                                                 ELSE */
/*                                                                     "" */
                    inventoryStock.sourceID                   = fg-rdtlh.rec_key
                    inventoryStock.sourceType                 = gcInventorySourceTypeJob
                    inventoryStock.inventoryStatus            = gcStatusStockInitial
                    inventoryStock.inventoryStockID           = fGetNextStockID (
                                                                    inventoryStock.itemType
                                                                )
                    inventoryStock.tag                        = IF fg-rdtlh.tag NE "" THEN
                                                                    fg-rdtlh.tag
                                                                ELSE
                                                                    fGetNextStockIDAlias (
                                                                        inventoryStock.company,
                                                                        inventoryStock.primaryID
                                                                    )
                    inventoryStock.costUOM                    = IF fg-rcpth.pur-uom NE "" THEN
                                                                    fg-rcpth.pur-uom
                                                                ELSE
                                                                    itemfg.pur-uom
                    inventoryStock.poLine                     = IF fg-rcpth.po-no NE "" THEN
                                                                    1
                                                                ELSE
                                                                    0
                    .
            END.

            CASE fg-rcpth.rita-code:
                WHEN "S" THEN
                    ASSIGN
                        inventoryStock.quantity        = inventoryStock.quantity - fg-rdtlh.qty
                        inventoryStock.inventoryStatus = gcStatusStockConsumed 
                        .
                WHEN "C" THEN 
                    ASSIGN
                        inventoryStock.quantity        = fg-rdtlh.qty
                        inventoryStock.inventoryStatus = gcStatusStockConsumed
                        .
                WHEN "R" THEN 
                    ASSIGN
                        inventoryStock.quantityOriginal = fg-rdtlh.qty
                        inventoryStock.quantity         = fg-rdtlh.qty
                        inventoryStock.inventoryStatus  = gcStatusStockReceived
                        .
                OTHERWISE 
                    inventoryStock.quantity = inventoryStock.quantity + fg-rdtlh.qty.
            END CASE.
            
            IF inventoryStock.quantity EQ 0 THEN
                ASSIGN
                    inventoryStock.consumedBy   = fg-rcpth.create-by
                    inventoryStock.consumedTime = DATETIME(fg-rdtlh.trans-date, fg-rdtlh.trans-time)
                    .                
            ASSIGN
                inventoryStock.lastTransTime = DATETIME(fg-rdtlh.trans-date, fg-rdtlh.trans-time)
                iopiCountOfProcessed         = iopiCountOfProcessed + 1.
                .
        END.
    END.        
END PROCEDURE.

PROCEDURE pBuildRMInventoryStockForItem PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a Item, build bins - will purge and export too 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT        PARAMETER ipriItem             AS ROWID     NO-UNDO.
    DEFINE INPUT        PARAMETER ipdtAsOf             AS DATE      NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiCountOfProcessed AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER iplPurge             AS LOGICAL   NO-UNDO.
    DEFINE INPUT        PARAMETER ipcExportPath        AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lCreated   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage   AS CHARACTER NO-UNDO.
    
    FIND FIRST item NO-LOCK
        WHERE ROWID(item) EQ ipriItem
        NO-ERROR.
    IF AVAILABLE item THEN 
    DO:
        FOR EACH  rm-rcpth NO-LOCK
            WHERE rm-rcpth.company    EQ item.company
              AND rm-rcpth.i-no       EQ item.i-no
              AND rm-rcpth.trans-date LE ipdtAsOf,
            EACH  rm-rdtlh NO-LOCK
            WHERE rm-rdtlh.r-no      EQ rm-rcpth.r-no
              AND rm-rdtlh.rita-code EQ rm-rcpth.rita-code
            BY rm-rcpth.trans-date
            BY rm-rdtlh.trans-time
            BY rm-rcpth.r-no
            :
/*             FIND FIRST inventoryStock */
/*                  WHERE inventoryStock.company      EQ rm-rcpth.company */
/*                    AND inventoryStock.fgItemID     EQ rm-rcpth.i-no */
/*                    AND inventoryStock.jobID        EQ rm-rcpth.job-no */
/*                    AND inventoryStock.jobID2       EQ rm-rcpth.job-no2 */
/*                    AND inventoryStock.warehouseID  EQ rm-rdtlh.loc */
/*                    AND inventoryStock.locationID   EQ rm-rdtlh.loc-bin */
/*                    AND inventoryStock.stockIDAlias EQ rm-rdtlh.tag */
/*                    AND inventoryStock.customerID   EQ rm-rdtlh.cust-no */
/*                  NO-ERROR. */
            FIND FIRST inventoryStock EXCLUSIVE-LOCK
                 WHERE inventoryStock.sourceID EQ rm-rdtlh.rec_key /* Unable to find a unique record. Temporarily using source ID */
                 NO-ERROR.
            IF NOT AVAILABLE inventoryStock THEN 
            DO:
                CREATE inventoryStock.
                ASSIGN
                    inventoryStock.company                    = rm-rcpth.company
                    inventoryStock.jobID                      = rm-rcpth.job-no
                    inventoryStock.jobID2                     = rm-rcpth.job-no2
                    inventoryStock.warehouseID                = rm-rdtlh.loc
                    inventoryStock.locationID                 = rm-rdtlh.loc-bin                            
                    inventoryStock.rmItemID                   = rm-rcpth.i-no
                    inventoryStock.poID                       = INTEGER(rm-rcpth.po-no)
                    inventoryStock.createdTime                = DATETIME(rm-rdtlh.trans-date, rm-rdtlh.trans-time)
                    inventoryStock.bolID                      = rm-rdtlh.BOL
                    inventoryStock.primaryID                  = inventoryStock.rmItemID
                    inventoryStock.itemType                   = gcItemTypeRM
                    inventoryStock.quantityPerSubUnit         = 1
                    inventoryStock.quantityOfSubUnits         = 1
                    inventoryStock.quantitySubUnitsPerUnit    = 1
                    inventoryStock.quantity                   = rm-rdtlh.qty
                    inventoryStock.quantityPartial            = 0
                    inventoryStock.quantityOfUnits            = fCalculateQuantityUnits (
                                                                    inventoryStock.quantityOfSubUnits,
                                                                    inventoryStock.quantitySubUnitsPerUnit,
                                                                    inventoryStock.quantityPartial
                                                                )
                    inventoryStock.quantityOfUnitsOriginal    = inventoryStock.quantityOfUnits
                    inventoryStock.quantityPartialOriginal    = inventoryStock.quantityPartial
                    inventoryStock.quantityOfSubUnitsOriginal = inventoryStock.quantityOfSubUnits
                    inventoryStock.costStandardPerUOM         = rm-rdtlh.cost
                    inventoryStock.createdTime                = rm-rdtlh.enteredDT
                    inventoryStock.createdBy                  = rm-rdtlh.enteredBy
                    inventoryStock.lastTransBy                = rm-rdtlh.enteredBy
                    inventoryStock.dimEachLen                 = item.s-len
                    inventoryStock.dimEachWid                 = item.s-wid
                    inventoryStock.dimEachDep                 = item.s-dep
                    inventoryStock.dimEachUOM                 = gcUOMInches
                    inventoryStock.inventoryStockUOM          = gcUOMInches
                    inventoryStock.basisWeight                = item.weight-100
                    inventoryStock.basisWeightUOM             = gcUOMWeightBasis
                    inventoryStock.weightUOM                  = gcUOMWeight
/*                     inventoryStock.sourceID                   = IF rm-rcpth.job-no NE "" THEN */
/*                                                                     FILL(" ",6 - LENGTH(LEFT-TRIM(TRIM(rm-rcpth.job-no)))) + STRING(rm-rcpth.job-no2,"99") */
/*                                                                 ELSE */
/*                                                                     "" */
                    inventoryStock.sourceID                   = rm-rdtlh.rec_key
                    inventoryStock.sourceType                 = gcInventorySourceTypeJob
                    inventoryStock.inventoryStatus            = gcStatusStockInitial
                    inventoryStock.inventoryStockID           = fGetNextStockID (
                                                                    inventoryStock.itemType
                                                                )
                    inventoryStock.tag                        = IF rm-rdtlh.tag NE "" THEN
                                                                    rm-rdtlh.tag
                                                                ELSE
                                                                    fGetNextStockIDAlias (
                                                                        inventoryStock.company,
                                                                        inventoryStock.primaryID
                                                                    )
                    inventoryStock.costUOM                    = IF rm-rcpth.pur-uom NE "" THEN
                                                                    rm-rcpth.pur-uom
                                                                ELSE
                                                                    item.pur-uom
                    inventoryStock.poLine                     = IF rm-rcpth.po-no NE "" THEN
                                                                    rm-rcpth.po-line
                                                                ELSE
                                                                    0
                    .                  
            END.

            CASE rm-rcpth.rita-code:
                WHEN "S" THEN
                    ASSIGN
                        inventoryStock.quantity        = inventoryStock.quantity - rm-rdtlh.qty
                        inventoryStock.inventoryStatus = gcStatusStockConsumed 
                        .
                WHEN "C" THEN 
                    ASSIGN
                        inventoryStock.quantity        = rm-rdtlh.qty
                        inventoryStock.inventoryStatus = gcStatusStockConsumed
                        .
                WHEN "R" THEN 
                    ASSIGN
                        inventoryStock.quantityOriginal = rm-rdtlh.qty
                        inventoryStock.quantity         = rm-rdtlh.qty
                        inventoryStock.inventoryStatus  = gcStatusStockReceived
                        .
                OTHERWISE 
                    inventoryStock.quantity = inventoryStock.quantity + rm-rdtlh.qty.
            END CASE.
            
            IF inventoryStock.quantity EQ 0 THEN
                ASSIGN
                    inventoryStock.consumedBy   = rm-rdtlh.enteredBy
                    inventoryStock.consumedTime = DATETIME(rm-rcpth.trans-date, rm-rdtlh.trans-time)
                    .                
            ASSIGN
                inventoryStock.lastTransTime = DATETIME(rm-rcpth.trans-date, rm-rdtlh.trans-time)
                iopiCountOfProcessed         = iopiCountOfProcessed + 1.
                .
        END.
    END.        
END PROCEDURE.

PROCEDURE CheckInventoryStockTagID:
    /*------------------------------------------------------------------------------
     Purpose: Checks to see if passed ID is an alias or a true stock ID
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany          AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLookupID         AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcInventoryStockID AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcTag              AS CHARACTER NO-UNDO.

    FIND FIRST inventoryStock NO-LOCK 
        WHERE inventoryStock.company EQ ipcCompany
          AND inventoryStock.tag     EQ ipcLookupID
        NO-ERROR.
    IF AVAILABLE inventoryStock THEN 
        ASSIGN
            opcInventoryStockID = inventoryStock.inventoryStockID
            opcTag              = inventoryStock.tag
            . 
    ELSE DO:
        FIND FIRST loadtag NO-LOCK
             WHERE loadtag.company     EQ ipcCompany
               AND loadtag.item-type   EQ NO
               AND loadtag.tag-no      EQ ipcLookupID NO-ERROR.
        IF NOT AVAILABLE loadtag THEN
            FIND FIRST loadtag NO-LOCK
                 WHERE loadtag.company     EQ ipcCompany
                   AND loadtag.item-type   EQ YES
                   AND loadtag.tag-no      EQ ipcLookupID NO-ERROR.
        
        IF AVAILABLE loadtag THEN
            ASSIGN
                opcInventoryStockID = ""
                opcTag              = ipcLookupID
                .
        ELSE       
            ASSIGN
                opcInventoryStockID = ipcLookupID
                opcTag              = ""
                . 
    END.

END PROCEDURE.

PROCEDURE CreateInventoryStockFromLoadtag:
    /*------------------------------------------------------------------------------
     Purpose: Given existing ttInventoryStockLoadtag table, generate the "Actual"
     inventory with original quantity transfering to .quantity and registering the status as
     received.
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcInventoryStockID AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplCreateReceipt    AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER iplPost             AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplCreated          AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage          AS CHARACTER NO-UNDO.
           
    /*Copy Loadtags to inventoryStock*/
    FIND FIRST ttInventoryStockLoadtag NO-LOCK
        WHERE ttInventoryStockLoadtag.inventoryStockID EQ ipcInventoryStockID
        NO-ERROR. 
    IF AVAILABLE ttInventoryStockLoadtag THEN 
        RUN pCreateStockFromLoadtag(
            BUFFER ttInventoryStockLoadtag, 
            INPUT  iplCreateReceipt, 
            INPUT  iplPost, 
            OUTPUT oplCreated, 
            OUTPUT opcMessage
            ).        
    ELSE 
        ASSIGN 
            oplCreated = NO
            opcMessage = "Invalid Inventory Stock ID"
            .
                  

END PROCEDURE.

PROCEDURE CreateInventoryStockSnapshotFromInputsFG:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipriFgBin     AS ROWID     NO-UNDO.
    DEFINE INPUT  PARAMETER ipiSnapshotID AS INTEGER   NO-UNDO.  
    DEFINE OUTPUT PARAMETER oplCreated    AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage    AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-fg-bin FOR fg-bin.
    DEFINE BUFFER bf-itemfg FOR itemfg.
    
    FIND FIRST bf-fg-bin NO-LOCK 
        WHERE ROWID(bf-fg-bin) EQ ipriFgBin
        NO-ERROR.
    IF AVAILABLE bf-fg-bin THEN 
    DO: 

        CREATE inventoryStockSnapshot.
        ASSIGN
            inventoryStockSnapshot.inventoryStockID        = fGetNextStockID(gcItemTypeFG)
            inventoryStockSnapshot.company                 = bf-fg-bin.company
            inventoryStockSnapshot.jobID                   = bf-fg-bin.job-no
            inventoryStockSnapshot.jobID2                  = bf-fg-bin.job-no2
            inventoryStockSnapshot.inventoryStatus         = gcStatusSnapshotNotScanned
            inventoryStockSnapshot.itemType                = gcItemTypeFG
            inventoryStockSnapshot.fgItemID                = bf-fg-bin.i-no
            inventoryStockSnapshot.warehouseID             = bf-fg-bin.loc
            inventoryStockSnapshot.locationID              = bf-fg-bin.loc-bin
            inventoryStockSnapshot.orderID                 = bf-fg-bin.ord-no
            inventoryStockSnapshot.customerID              = bf-fg-bin.cust-no
            inventoryStockSnapshot.dimEachUOM              = "IN"
            inventoryStockSnapshot.quantityUOM             = bf-fg-bin.pur-uom
            inventoryStockSnapshot.costStandardPerUOM      = bf-fg-bin.std-tot-cost
            inventoryStockSnapshot.quantity                = bf-fg-bin.qty
            inventoryStockSnapshot.quantityPerSubUnit      = bf-fg-bin.qty
            inventoryStockSnapshot.createdTime             = NOW
            inventoryStockSnapshot.lastTransBy             = USERID(gcDBUser)
            inventoryStockSnapshot.lastTransTime           = inventoryStockSnapshot.createdTime
            inventoryStockSnapshot.createdBy               = USERID(gcDBUser)
            inventoryStockSnapshot.inventorySnapshotID     = ipiSnapshotID
            inventoryStockSnapshot.sourceID                = inventoryStockSnapshot.inventoryStockID
            inventoryStockSnapshot.sourceType              = gcSourceTypeSnapshot
            inventoryStockSnapshot.primaryID               = inventoryStockSnapshot.fgItemID
            inventoryStockSnapshot.tag                     = bf-fg-bin.tag
            oplCreated                                     = TRUE
            opcMessage                                     = "Inventory Stock Created for " + inventoryStockSnapshot.inventoryStockID
            .
            
                    
        FIND FIRST bf-itemfg NO-LOCK 
             WHERE bf-itemfg.company EQ bf-fg-bin.company
               AND bf-itemfg.i-no    EQ bf-fg-bin.i-no
               NO-ERROR.
        IF AVAILABLE bf-itemfg THEN 
            ASSIGN 
                inventoryStockSnapshot.basisWeight    = bf-itemfg.weight-100
                inventoryStockSnapshot.basisWeightUOM = "LBS/100"
                .                        
    END.
    ELSE 
        ASSIGN 
            oplCreated = NO
            opcMessage = "Invalid Finished Good" 
            .    


END PROCEDURE.

PROCEDURE CreateInventoryStockFromInputsFG:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipriJobMch                 AS ROWID     NO-UNDO.  /*Last Operation*/
    DEFINE INPUT  PARAMETER ipriJobMat                 AS ROWID     NO-UNDO.  /*Board Material*/
    DEFINE INPUT  PARAMETER ipriJobHdr                 AS ROWID     NO-UNDO.  /*Job Header*/
    DEFINE INPUT  PARAMETER ipdQuantityTotal           AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdQuantityPerSubUnit      AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiQuantitySubUnitsPerUnit AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcQuantityUOM             AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcInventoryStockID        AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplCreated                 AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage                 AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cDefaultLocation    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound              AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cAliasCreateMessage AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAliasCreated       AS LOGICAL   NO-UNDO.


    DEFINE BUFFER bf-job-mch FOR job-mch.
    DEFINE BUFFER bf-job-mat FOR job-mat.
    DEFINE BUFFER bf-itemfg  FOR itemfg.
    DEFINE BUFFER bf-job-hdr FOR job-hdr.
    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
    DEFINE BUFFER bf-po-ordl FOR po-ordl.
    DEFINE BUFFER bf-oe-boll FOR oe-boll.
    DEFINE BUFFER bf-oe-rel  FOR oe-rel.
    
    FIND FIRST bf-job-mch NO-LOCK 
        WHERE ROWID(bf-job-mch) EQ ipriJobMch
        NO-ERROR.
        
    FIND FIRST bf-job-mat NO-LOCK 
        WHERE ROWID(bf-job-mat) EQ ipriJobMat
        NO-ERROR.
        
    FIND FIRST bf-job-hdr NO-LOCK 
        WHERE ROWID(bf-job-hdr) EQ ipriJobHdr
        NO-ERROR.

    IF AVAILABLE bf-job-mch AND AVAILABLE bf-job-mat AND AVAILABLE bf-job-hdr THEN 
    DO:
        CREATE inventoryStock.
        ASSIGN 
            inventoryStock.company                 = bf-job-mch.company
            inventoryStock.machineID               = bf-job-mch.m-code
            inventoryStock.jobID                   = bf-job-mch.job-no
            inventoryStock.jobID2                  = bf-job-mch.job-no2
            inventoryStock.formNo                  = bf-job-mch.frm
            inventoryStock.blankNo                 = bf-job-mch.blank-no
            inventoryStock.passNo                  = bf-job-mch.pass
            inventoryStock.inventoryStatus         = gcStatusStockPreLoadtag
            inventoryStock.itemType                = gcItemTypeFG
            inventoryStock.fgItemID                = bf-job-hdr.i-no
            inventoryStock.rmItemID                = bf-job-mat.rm-i-no
            inventoryStock.dimEachLen              = bf-job-mat.len
            inventoryStock.dimEachWid              = bf-job-mat.wid
            inventoryStock.dimEachDep              = bf-job-mat.dep
            inventoryStock.primaryID               = inventoryStock.fgItemID
            inventoryStock.warehouseID             = bf-job-hdr.loc
            inventoryStock.orderID                 = bf-job-hdr.ord-no
            inventoryStock.customerID              = bf-job-hdr.cust-no
            inventoryStock.quantityUOM             = ipcQuantityUOM
            inventoryStock.quantityOriginal        = ipdQuantityTotal
            inventoryStock.quantitySubUnitsPerUnit = ipiQuantitySubUnitsPerUnit
            inventoryStock.quantityPerSubUnit      = ipdQuantityPerSubUnit
            inventoryStock.lastTransTime           = NOW
            inventoryStock.lastTransBy             = USERID(gcDBUser)
            inventoryStock.inventoryStockID        = fGetNextStockID (
                                                         inventoryStock.itemType
                                                     )
            inventoryStock.tag                     = fGetNextStockIDAlias (
                                                         inventoryStock.company,
                                                         inventoryStock.primaryID
                                                     )
            inventoryStock.inventoryStatus         = gcStatusStockInitial
            opcInventoryStockID                    = inventoryStock.inventoryStockID
            oplCreated                             = TRUE
            opcMessage                             = "Inventory Stock Created"
            .

        FIND FIRST bf-itemfg NO-LOCK 
             WHERE bf-itemfg.company EQ bf-job-mat.company
               AND bf-itemfg.i-no    EQ bf-job-mat.rm-i-no
               NO-ERROR.
        IF AVAILABLE bf-itemfg THEN 
            ASSIGN 
                inventoryStock.basisWeight    = bf-itemfg.weight-100
                inventoryStock.basisWeightUOM = "LBS/100"
                .           

        IF bf-job-hdr.ord-no NE 0 THEN
        DO:
            FIND FIRST bf-oe-ordl NO-LOCK
                 WHERE bf-oe-ordl.company EQ bf-job-hdr.company
                   AND bf-oe-ordl.i-no    EQ bf-job-hdr.i-no
                   AND bf-oe-ordl.ord-no  EQ bf-job-hdr.ord-no
                   NO-ERROR.
            IF AVAIL bf-oe-ordl THEN DO:
                ASSIGN
/*                     inventoryStock.poID      = bf-oe-ordl.po-no */
                    inventoryStock.orderLine = bf-oe-ordl.line
                    .
                
                FIND FIRST bf-po-ordl NO-LOCK
                     WHERE bf-po-ordl.company EQ bf-job-hdr.company
                       AND bf-po-ordl.po-no   EQ INTEGER(bf-oe-ordl.po-no)
                       AND bf-po-ordl.line    EQ bf-oe-ordl.line
                       NO-ERROR.
                IF AVAILABLE bf-po-ordl THEN
                    inventoryStock.poLine = bf-po-ordl.line.

                FIND FIRST bf-oe-boll NO-LOCK
                     WHERE bf-oe-boll.company  EQ bf-oe-ordl.company
                       AND bf-oe-boll.ord-no   EQ bf-oe-ordl.ord-no
                       AND bf-oe-boll.line     EQ bf-oe-ordl.line
                     NO-ERROR.
                IF AVAILABLE bf-oe-boll THEN DO:                
                    inventoryStock.bolID = STRING(bf-oe-boll.bol-no).
                    
                    FIND FIRST bf-oe-rel NO-LOCK
                         WHERE bf-oe-rel.r-no EQ bf-oe-boll.r-no
                         NO-ERROR.
                    IF AVAILABLE bf-oe-rel THEN
                        ASSIGN
                            inventoryStock.releaseID   = bf-oe-rel.rel-no
                            inventoryStock.releaseLine = bf-oe-rel.line
                            .                
                END.
            END.            
        END.
        RUN sys/ref/nk1look.p (
            INPUT  bf-job-mch.company,
            INPUT  "WIPTAGSDefaultLocation",
            INPUT  "C",
            INPUT  NO,
            INPUT  NO,
            INPUT  "",
            INPUT  "",
            OUTPUT cDefaultLocation,OUTPUT lFound
            ).
        IF lFound THEN
            inventoryStock.locationID = cDefaultLocation. 
    END.
    ELSE 
        ASSIGN 
            oplCreated = NO
            opcMessage = "Invalid Machine or Material or Job Header Inputs" 
            .   

    RELEASE bf-job-mch.
    RELEASE bf-job-mat.
    RELEASE bf-itemfg.
    RELEASE bf-job-hdr.
    RELEASE bf-oe-ordl.   
    RELEASE bf-po-ordl. 
    RELEASE bf-oe-boll.  
    RELEASE bf-oe-rel.      
END PROCEDURE.

PROCEDURE CreateInventoryStockFromInputsPO:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipriPOOrd                  AS ROWID     NO-UNDO.  /* PO */
    DEFINE INPUT  PARAMETER ipriPOOrdl                 AS ROWID     NO-UNDO.  /*PO Line*/
    DEFINE INPUT  PARAMETER ipdQuantityTotal           AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdQuantityPerSubUnit      AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiQuantitySubUnitsPerUnit AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcQuantityUOM             AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcInventoryStockID        AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplCreated                 AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage                 AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cDefaultLocation    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound              AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cAliasCreateMessage AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAliasCreated       AS LOGICAL   NO-UNDO.


    DEFINE BUFFER bf-job-mch FOR job-mch.
    DEFINE BUFFER bf-job-mat FOR job-mat.
    DEFINE BUFFER bf-itemfg  FOR itemfg.
    DEFINE BUFFER bf-job-hdr FOR job-hdr.
    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
    DEFINE BUFFER bf-po-ordl FOR po-ordl.
    DEFINE BUFFER bf-po-ord  FOR po-ord.
    DEFINE BUFFER bf-oe-bolh FOR oe-bolh.
    DEFINE BUFFER bf-oe-rel  FOR oe-rel.
    
    FIND FIRST bf-po-ord NO-LOCK 
        WHERE ROWID(bf-po-ord) EQ ipriPOOrd
        NO-ERROR.
        
    FIND FIRST bf-po-ordl NO-LOCK 
        WHERE ROWID(bf-po-ordl) EQ ipriPOOrdl
        NO-ERROR.
        
    IF AVAILABLE bf-po-ord AND AVAILABLE bf-po-ordl THEN 
    DO:
        CREATE inventoryStock.
        ASSIGN 
            inventoryStock.company                 = bf-po-ord.company
            inventoryStock.jobID                   = bf-po-ordl.job-no
            inventoryStock.poID                    = bf-po-ordl.po-no
            inventoryStock.jobID2                  = bf-po-ordl.job-no2
            inventoryStock.formNo                  = bf-po-ordl.s-num
            inventoryStock.blankNo                 = bf-po-ordl.b-num
            inventoryStock.inventoryStatus         = gcStatusStockPreLoadtag
            inventoryStock.itemType                = IF bf-po-ordl.item-type THEN
                                                         gcItemTypeRM 
                                                     ELSE
                                                         gcItemTypeFG
            inventoryStock.dimEachLen              = bf-po-ordl.s-len
            inventoryStock.dimEachWid              = bf-po-ordl.s-wid
            inventoryStock.dimEachDep              = bf-po-ordl.s-dep
            inventoryStock.warehouseID             = bf-po-ord.loc
            inventoryStock.orderID                 = bf-po-ordl.ord-no
            inventoryStock.customerID              = bf-po-ordl.cust-no
            inventoryStock.quantityUOM             = ipcQuantityUOM
            inventoryStock.quantityOriginal        = ipdQuantityTotal
            inventoryStock.quantitySubUnitsPerUnit = ipiQuantitySubUnitsPerUnit
            inventoryStock.quantityPerSubUnit      = ipdQuantityPerSubUnit
            inventoryStock.poLine                  = bf-po-ordl.line
            inventoryStock.lastTransTime           = NOW
            inventoryStock.lastTransBy             = USERID(gcDBUser)
            inventoryStock.inventoryStockID        = fGetNextStockID (
                                                         inventoryStock.itemType
                                                     )
            inventoryStock.inventoryStatus         = gcStatusStockInitial
            opcInventoryStockID                    = inventoryStock.inventoryStockID
            .
        
        IF inventoryStock.itemType EQ gcItemTypeFG THEN
            ASSIGN
                inventoryStock.fgItemID  = bf-po-ordl.i-no
                inventoryStock.primaryID = inventoryStock.fgItemID
                .
        ELSE IF inventoryStock.itemType EQ gcItemTypeRM THEN
            ASSIGN
                inventoryStock.rmItemID  = bf-po-ordl.i-no
                inventoryStock.primaryID = inventoryStock.rmItemID
                .

        inventoryStock.tag = fGetNextStockIDAlias (
                                 inventoryStock.company,
                                 inventoryStock.primaryID
                                 ).

        FIND FIRST bf-itemfg NO-LOCK 
             WHERE bf-itemfg.company EQ bf-po-ordl.company
               AND bf-itemfg.i-no    EQ bf-po-ordl.i-no
               NO-ERROR.
        IF AVAILABLE bf-itemfg THEN 
            ASSIGN 
                inventoryStock.basisWeight    = bf-itemfg.weight-100
                inventoryStock.basisWeightUOM = "LBS/100"
                .           

        IF bf-po-ordl.ord-no NE 0 THEN
        DO:
            FIND FIRST bf-oe-ordl NO-LOCK
                 WHERE bf-oe-ordl.company EQ bf-po-ordl.company
                   AND bf-oe-ordl.ord-no  EQ bf-po-ordl.ord-no
                   AND bf-oe-ordl.i-no    EQ bf-po-ordl.i-no
                   NO-ERROR.
 
            IF AVAIL bf-oe-ordl THEN
                inventoryStock.orderLine = bf-oe-ordl.line.                
            
            FIND FIRST bf-oe-bolh NO-LOCK
                 WHERE bf-oe-bolh.company  EQ bf-po-ordl.company
                   AND bf-oe-bolh.ord-no   EQ bf-po-ordl.ord-no
                   NO-ERROR.
            IF AVAILABLE bf-oe-bolh THEN DO:                
                inventoryStock.bolID = STRING(bf-oe-bolh.bol-no).
                
                FIND FIRST bf-oe-rel NO-LOCK
                     WHERE bf-oe-rel.r-no EQ bf-oe-bolh.r-no
                       NO-ERROR.
                IF AVAILABLE bf-oe-rel THEN
                    ASSIGN
                        inventoryStock.releaseID   = bf-oe-rel.rel-no
                        inventoryStock.releaseLine = bf-oe-rel.line
                        .                
            END.                     
        END.
        RUN sys/ref/nk1look.p (
            bf-po-ordl.company,
            "WIPTAGSDefaultLocation",
            "C",
            NO,
            NO,
            "",
            "",
            OUTPUT cDefaultLocation,OUTPUT lFound
            ).
        IF lFound THEN
            inventoryStock.locationID = cDefaultLocation. 
            
        ASSIGN
            oplCreated = YES
            opcMessage = "Tag Created"
            .
    END.
    ELSE 
        ASSIGN 
            oplCreated = NO
            opcMessage = "Invalid PO" 
            .   

    RELEASE bf-job-mch.
    RELEASE bf-job-mat.
    RELEASE bf-itemfg.
    RELEASE bf-job-hdr.
    RELEASE bf-oe-ordl.   
    RELEASE bf-po-ordl. 
    RELEASE bf-oe-bolh.  
    RELEASE bf-oe-rel.      
END PROCEDURE.

PROCEDURE CreatePreLoadtagsFromInputsRM:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

END PROCEDURE.

PROCEDURE CreatePreLoadtagsFromInputsWIP:
    /*------------------------------------------------------------------------------
     Purpose:  Given critical inputs for WIP process, generate the Pre-Loadtags
     for processing.
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipriJobMch                 AS ROWID     NO-UNDO.  /*Last Operation*/
    DEFINE INPUT  PARAMETER ipriJobMat                 AS ROWID     NO-UNDO.  /*Board Material*/
    DEFINE INPUT  PARAMETER ipdQuantityTotal           AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdQuantityPerSubUnit      AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiQuantitySubUnitsPerUnit AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcQuantityUOM             AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplCreated                 AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage                 AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cDefaultLocation AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound           AS LOGICAL   NO-UNDO.

    DEFINE BUFFER bf-job-mch FOR job-mch.
    DEFINE BUFFER bf-job-mat FOR job-mat.
    DEFINE BUFFER bf-item    FOR item.
    DEFINE BUFFER bf-job-hdr FOR job-hdr.
    
    EMPTY TEMP-TABLE ttInventoryStockPreLoadtag.
    FIND FIRST bf-job-mch NO-LOCK 
        WHERE ROWID(bf-job-mch) EQ ipriJobMch
        NO-ERROR.
    FIND FIRST bf-job-mat NO-LOCK 
        WHERE ROWID(bf-job-mat) EQ ipriJobMat
        NO-ERROR.
    IF AVAILABLE bf-job-mch AND AVAILABLE bf-job-mat THEN 
    DO:
        CREATE ttInventoryStockPreLoadtag.
        ASSIGN 
            ttInventoryStockPreLoadtag.company                 = bf-job-mch.company
            ttInventoryStockPreLoadtag.machineID               = bf-job-mch.m-code
            ttInventoryStockPreLoadtag.jobID                   = bf-job-mch.job-no
            ttInventoryStockPreLoadtag.jobID2                  = bf-job-mch.job-no2
            ttInventoryStockPreLoadtag.formNo                  = bf-job-mch.frm
            ttInventoryStockPreLoadtag.blankNo                 = bf-job-mch.blank-no
            ttInventoryStockPreLoadtag.passNo                  = bf-job-mch.pass
            ttInventoryStockPreLoadtag.inventoryStatus         = gcStatusStockPreLoadtag
            ttInventoryStockPreLoadtag.itemType                = gcItemTypeWIP
            ttInventoryStockPreLoadtag.rmItemID                = bf-job-mat.rm-i-no
            ttInventoryStockPreLoadtag.dimEachLen              = bf-job-mat.len
            ttInventoryStockPreLoadtag.dimEachWid              = bf-job-mat.wid
            ttInventoryStockPreLoadtag.dimEachDep              = bf-job-mat.dep
            ttInventoryStockPreLoadtag.dimEachUOM              = "IN"
            ttInventoryStockPreLoadtag.quantityTotal           = ipdQuantityTotal
            ttInventoryStockPreLoadtag.quantityUOM             = ipcQuantityUOM
            ttInventoryStockPreLoadtag.quantitySubUnitsPerUnit = ipiQuantitySubUnitsPerUnit
            ttInventoryStockPreLoadtag.quantityPerSubUnit      = ipdQuantityPerSubUnit
            ttInventoryStockPreLoadtag.lastTransTime           = NOW
            ttInventoryStockPreLoadtag.lastTransBy             = USERID(gcDBUser).
        RUN pGetWIPID(BUFFER ttInventoryStockPreLoadtag, OUTPUT ttInventoryStockPreLoadtag.wipItemID).
        ttInventoryStockPreLoadtag.primaryID = ttInventoryStockPreLoadtag.wipItemID.
        RUN RecalcQuantityUnits(ipdQuantityTotal, INPUT-OUTPUT ttInventoryStockPreLoadtag.quantityPerSubUnit, INPUT-OUTPUT ttInventoryStockPreLoadtag.quantitySubUnitsPerUnit, 
            OUTPUT ttInventoryStockPreLoadtag.quantityOfSubUnits, OUTPUT ttInventoryStockPreLoadtag.quantityOfUnits, OUTPUT ttInventoryStockPreLoadtag.quantityPartial).
            
        FIND FIRST bf-item NO-LOCK 
             WHERE bf-item.company EQ bf-job-mat.company
               AND bf-item.i-no    EQ bf-job-mat.rm-i-no
             NO-ERROR.
        IF AVAILABLE bf-item THEN 
            ASSIGN 
                ttInventoryStockPreLoadtag.basisWeight    = bf-item.basis-w
                ttInventoryStockPreLoadtag.basisWeightUOM = "LBS/MSF"
                .
        FIND FIRST bf-job-hdr NO-LOCK 
             WHERE bf-job-hdr.company EQ bf-job-mch.company
               AND bf-job-hdr.job     EQ bf-job-mch.job
               AND bf-job-hdr.job-no  EQ bf-job-mch.job-no
               AND bf-job-hdr.job-no2 EQ bf-job-mch.job-no2
             NO-ERROR.
        IF AVAILABLE bf-job-hdr THEN 
            ASSIGN 
                ttInventoryStockPreLoadtag.fgItemID    = bf-job-hdr.i-no
                ttInventoryStockPreLoadtag.warehouseID = bf-job-hdr.loc
                ttInventoryStockPreLoadtag.orderID     = bf-job-hdr.ord-no
                ttInventoryStockPreLoadtag.customerID  = bf-job-hdr.cust-no
                .
        RUN sys/ref/nk1look.p (
            INPUT  bf-job-mch.company,
            INPUT  "WIPTAGSDefaultLocation",
            INPUT  "C",
            INPUT  NO,
            INPUT  NO,
            INPUT  "",
            INPUT  "",
            OUTPUT cDefaultLocation,OUTPUT lFound
            ).
        IF lFound THEN
            ttInventoryStockPreLoadtag.locationID = cDefaultLocation.
    END.
    ELSE 
        ASSIGN 
            oplCreated = NO
            opcMessage = "Invalid Machine or Material Inputs" 
            .    

    RELEASE bf-job-hdr.
    RELEASE bf-job-mat.
    RELEASE bf-job-mch.
    RELEASE bf-item.
END PROCEDURE.

PROCEDURE CreatePrintInventory:
    /*------------------------------------------------------------------------------
     Purpose: Creates temporary table to send data to a text file
     Notes: 
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcInventoryStockID LIKE inventoryTransaction.inventoryStockID NO-UNDO.
    
    DEFINE VARIABLE cCustName LIKE oe-ord.cust-name NO-UNDO.
    DEFINE VARIABLE cMachName LIKE mach.m-dscr      NO-UNDO.
    DEFINE VARIABLE cItemName LIKE item.i-name      NO-UNDO.
    
    DEFINE VARIABLE hdJobProcs AS HANDLE NO-UNDO.
    RUN jc/JobProcs.p PERSISTENT SET hdJobProcs.
    
    FIND FIRST inventoryStock NO-LOCK
         WHERE inventoryStock.inventoryStockID = ipcinventoryStockID
         NO-ERROR.
    IF AVAILABLE inventoryStock THEN DO:
        CREATE ttPrintInventoryStock.
        BUFFER-COPY inventoryStock TO ttPrintInventoryStock.
        
        FIND FIRST oe-ord NO-LOCK
             WHERE oe-ord.company = inventoryStock.company AND
                   oe-ord.ord-no  = inventoryStock.orderID AND
                   oe-ord.cust-no = inventoryStock.customerID NO-ERROR.
        IF AVAILABLE oe-ord THEN
            ASSIGN cCustName = oe-ord.cust-name.
        ELSE DO:
            FIND FIRST cust NO-LOCK
                WHERE cust.company = inventoryStock.company AND
                      cust.cust-no = inventoryStock.customerID  NO-ERROR.
            IF AVAILABLE cust THEN
                ASSIGN cCustName = cust.name.
        END.
                 
        FIND FIRST mach NO-LOCK
             WHERE mach.company = inventoryStock.company AND
                   mach.m-code  = inventoryStock.machineID NO-ERROR.
        IF AVAILABLE mach THEN
            ASSIGN cMachName = mach.m-dscr.
        
        FIND FIRST item NO-LOCK
             WHERE item.company = inventoryStock.company AND
                   item.i-no    = inventoryStock.rmItemID NO-ERROR.
        IF AVAILABLE item THEN
            ASSIGN cItemName = item.i-name.

        /* Get Next machine ID for the given machine */
        RUN Job_GetNextOperation IN hdJobProcs (
            INPUT  inventoryStock.company, 
            INPUT  inventoryStock.jobID, 
            INPUT  inventoryStock.jobID2,
            INPUT  inventoryStock.formNo,
            INPUT  inventoryStock.pass,
            INPUT  inventoryStock.machineID,
            OUTPUT ttPrintInventoryStock.nextMachineID            
            ).

        /* Get description of the next machine */
        IF ttPrintInventoryStock.nextMachineID NE "" THEN DO:
            FIND FIRST mach NO-LOCK
                 WHERE mach.company EQ inventoryStock.company 
                   AND mach.m-code  EQ ttPrintInventoryStock.nextMachineID
                 NO-ERROR.
            IF AVAILABLE mach THEN
                ttPrintInventoryStock.nextMachineName = mach.m-dscr.        
        END.

        ASSIGN
            ttPrintInventoryStock.jobNumber    = LEFT-TRIM(TRIM(inventoryStock.jobID))
            ttPrintInventoryStock.jobNumber    = FILL(" ",6 - LENGTH(ttPrintInventoryStock.jobNumber)) + ttPrintInventoryStock.jobNumber
            ttPrintInventoryStock.jobRunNumber = inventoryStock.jobID2
            ttPrintInventoryStock.jobID        = ttPrintInventoryStock.jobNumber + "-" + STRING(ttPrintInventoryStock.jobRunNumber,"99")
            ttPrintInventoryStock.jobIDTrimmed = LEFT-TRIM(ttPrintInventoryStock.jobID)
            ttPrintInventoryStock.jobIDFull    = ttPrintInventoryStock.jobID + "." + STRING(inventoryStock.formNo,"99") + "." + STRING(inventoryStock.blankNo,"99")
            ttPrintInventoryStock.jobIDFullTrimmed = LEFT-TRIM(ttPrintInventoryStock.jobIDFull) 
            ttPrintInventoryStock.customerName = cCustName
            ttPrintInventoryStock.machineName  = cMachName
            ttPrintInventoryStock.rmItemName   = cItemName.
    END.
    
    IF VALID-HANDLE(hdJobProcs) THEN
        DELETE PROCEDURE hdJobProcs.
END PROCEDURE.

PROCEDURE CreatePrintInventoryForRM:
    /*------------------------------------------------------------------------------
     Purpose: Creates temporary table to send data to a text file
     Notes: 
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcInventoryStockID LIKE inventoryTransaction.inventoryStockID NO-UNDO.

    DEFINE VARIABLE hdJobProcs AS HANDLE NO-UNDO.
    
    RUN jc/JobProcs.p PERSISTENT SET hdJobProcs.
    
    FIND FIRST inventoryStock NO-LOCK
         WHERE inventoryStock.inventoryStockID EQ ipcinventoryStockID
         NO-ERROR.
    IF AVAILABLE inventoryStock THEN DO:
        CREATE ttPrintInventoryStockRM.
        
        ASSIGN
            ttPrintInventoryStockRM.tagID         = inventoryStock.tag
            ttPrintInventoryStockRM.poID          = inventoryStock.poID
            ttPrintInventoryStockRM.jobID         = inventoryStock.jobID
            ttPrintInventoryStockRM.jobID2        = inventoryStock.jobID2
            ttPrintInventoryStockRM.blankNo       = inventoryStock.blankNo
            ttPrintInventoryStockRM.company       = inventoryStock.company
            ttPrintInventoryStockRM.tagDate       = DATE(inventoryStock.createdTime)
            ttPrintInventoryStockRM.locationID    = inventoryStock.locationID
            ttPrintInventoryStockRM.warehouseID   = inventoryStock.warehouseID
            ttPrintInventoryStockRM.partialQty    = inventoryStock.quantityPartial
            .
        
        RUN GetOperation IN hdJobProcs (
            inventoryStock.company, 
            inventoryStock.jobID, 
            inventoryStock.jobID2,
            inventoryStock.formNo,
            "First", 
            INPUT-OUTPUT ttPrintInventoryStockRM.firstMachine
            ).
        
        RUN GetOperation IN hdJobProcs (
            inventoryStock.company, 
            inventoryStock.jobID, 
            inventoryStock.jobID2,
            inventoryStock.formNo,
            "Internal", 
            INPUT-OUTPUT ttPrintInventoryStockRM.firstInternalMachine
            ).

        RUN GetOperation IN hdJobProcs (
            inventoryStock.company, 
            inventoryStock.jobID, 
            inventoryStock.jobID2,
            inventoryStock.formNo,
            "Press", 
            INPUT-OUTPUT ttPrintInventoryStockRM.firstPress
            ).

        FIND FIRST company NO-LOCK
             WHERE company.company EQ inventoryStock.company
             NO-ERROR.
        IF AVAILABLE company THEN
            ttPrintInventoryStockRM.companyName = company.name.
            
        FIND FIRST po-ord NO-LOCK
             WHERE po-ord.company EQ inventoryStock.company
               AND po-ord.po-no   EQ inventoryStock.poID
             NO-ERROR.
        IF AVAILABLE po-ord THEN DO:
            FIND FIRST vend NO-LOCK
                 WHERE vend.company EQ po-ord.company
                   AND vend.vend-no EQ po-ord.vend-no
                 NO-ERROR.
                 
            ASSIGN
                ttPrintInventoryStockRM.acknowledgement        = po-ord.acknowledge
                ttPrintInventoryStockRM.shipToAddress[1]       = po-ord.addr[1]
                ttPrintInventoryStockRM.shipToAddress[2]       = po-ord.addr[2]
                ttPrintInventoryStockRM.billTo                 = po-ord.bill-to
                ttPrintInventoryStockRM.buyer                  = po-ord.buyer
                ttPrintInventoryStockRM.shippingCarrier        = po-ord.carrier
                ttPrintInventoryStockRM.city                   = po-ord.city
                ttPrintInventoryStockRM.contact                = po-ord.contact
                ttPrintInventoryStockRM.currencyCode[1]        = po-ord.curr-code[1]
                ttPrintInventoryStockRM.currencyCode[2]        = po-ord.curr-code[2]
                ttPrintInventoryStockRM.customerNumber         = po-ord.cust-no
                ttPrintInventoryStockRM.deleted                = po-ord.deleted
                ttPrintInventoryStockRM.requiredDate           = po-ord.due-date
                ttPrintInventoryStockRM.exRate                 = po-ord.ex-rate
                ttPrintInventoryStockRM.fobOriginDest          = po-ord.fob-code
                ttPrintInventoryStockRM.freightPayment         = po-ord.frt-pay
                ttPrintInventoryStockRM.lastShipDate           = po-ord.last-ship-date
                ttPrintInventoryStockRM.opened                 = po-ord.opened
                ttPrintInventoryStockRM.overrunPct             = po-ord.over-pct
                ttPrintInventoryStockRM.dateChanged            = po-ord.po-change-date
                ttPrintInventoryStockRM.poDate                 = po-ord.po-date
                ttPrintInventoryStockRM.printed                = po-ord.printed
                ttPrintInventoryStockRM.received               = po-ord.received
                ttPrintInventoryStockRM.shippingAddress[1]     = po-ord.ship-addr[1]
                ttPrintInventoryStockRM.shippingAddress[2]     = po-ord.ship-addr[2]
                ttPrintInventoryStockRM.shippingCity           = po-ord.ship-city
                ttPrintInventoryStockRM.shipTo                 = po-ord.ship-id
                ttPrintInventoryStockRM.shippingName           = po-ord.ship-name
                ttPrintInventoryStockRM.shipToNumber           = po-ord.ship-no
                ttPrintInventoryStockRM.shippingState          = po-ord.ship-state
                ttPrintInventoryStockRM.shippingZip            = po-ord.ship-zip
                ttPrintInventoryStockRM.specialInstructions[1] = po-ord.spec-i[1]
                ttPrintInventoryStockRM.specialInstructions[2] = po-ord.spec-i[2]
                ttPrintInventoryStockRM.specialInstructions[3] = po-ord.spec-i[3]
                ttPrintInventoryStockRM.specialInstructions[4] = po-ord.spec-i[4]
                ttPrintInventoryStockRM.stat                   = po-ord.stat
                ttPrintInventoryStockRM.state                  = po-ord.state
                ttPrintInventoryStockRM.totalCost              = po-ord.t-cost
                ttPrintInventoryStockRM.totalFreight           = po-ord.t-freight
                ttPrintInventoryStockRM.tax                    = po-ord.tax
                ttPrintInventoryStockRM.salesTaxGroup          = po-ord.tax-gr
                ttPrintInventoryStockRM.taxExemptNo            = po-ord.tax-id
                ttPrintInventoryStockRM.paymentTerms           = po-ord.terms
                ttPrintInventoryStockRM.type                   = po-ord.type
                ttPrintInventoryStockRM.underrunPct            = po-ord.under-pct
                ttPrintInventoryStockRM.updatedDate            = po-ord.upd-date
                ttPrintInventoryStockRM.updatedTime            = po-ord.upd-time
                ttPrintInventoryStockRM.zipCode                = po-ord.zip
                ttPrintInventoryStockRM.vendor                 = po-ord.vend-no
                ttPrintInventoryStockRM.vendorName             = IF AVAILABLE vend THEN vend.name ELSE ""
                .
        END.
        
        FIND FIRST po-ordl NO-LOCK
             WHERE po-ordl.company EQ inventoryStock.company
               AND po-ordl.po-no   EQ inventoryStock.poID
               AND po-ordl.line    EQ inventoryStock.poLine
             NO-ERROR.
        IF AVAILABLE po-ordl THEN DO:
            ASSIGN
                ttPrintInventoryStockRM.accountNo               = po-ordl.actnum
                ttPrintInventoryStockRM.setupCharge             = po-ordl.adders$
                ttPrintInventoryStockRM.blankNo                 = po-ordl.b-num
                ttPrintInventoryStockRM.costs                   = po-ordl.cons-cost
                ttPrintInventoryStockRM.consumptionQuantity     = po-ordl.cons-qty
                ttPrintInventoryStockRM.uom                     = po-ordl.cons-uom
                ttPrintInventoryStockRM.purchasedUOM            = po-ordl.pr-uom
                ttPrintInventoryStockRM.discount                = po-ordl.disc
                ttPrintInventoryStockRM.description[1]          = po-ordl.dscr[1]
                ttPrintInventoryStockRM.description[2]          = po-ordl.dscr[2]
                ttPrintInventoryStockRM.name                    = po-ordl.i-name
                ttPrintInventoryStockRM.itemID                  = po-ordl.i-no
                ttPrintInventoryStockRM.itemType                = po-ordl.item-type
                ttPrintInventoryStockRM.internalJobNumber       = po-ordl.j-no
                ttPrintInventoryStockRM.line                    = po-ordl.line
                ttPrintInventoryStockRM.customerOrderNo         = po-ordl.ord-no
                ttPrintInventoryStockRM.orderQuantity           = po-ordl.ord-qty
                ttPrintInventoryStockRM.purchaseQuantityUOM     = po-ordl.pr-qty-uom
                ttPrintInventoryStockRM.purchaseCount           = po-ordl.pur-cnt
                ttPrintInventoryStockRM.releaseQuantity         = po-ordl.rel-qty
                ttPrintInventoryStockRM.sheetLen                = po-ordl.s-len
                ttPrintInventoryStockRM.sheetNo                 = po-ordl.s-num
                ttPrintInventoryStockRM.sheetWid                = po-ordl.s-wid
                ttPrintInventoryStockRM.shippingInstructions[1] = po-ordl.ship-i[1]
                ttPrintInventoryStockRM.shippingInstructions[2] = po-ordl.ship-i[2]
                ttPrintInventoryStockRM.shippingInstructions[3] = po-ordl.ship-i[3]
                ttPrintInventoryStockRM.shippingInstructions[4] = po-ordl.ship-i[4]
                ttPrintInventoryStockRM.totalInvoiced           = po-ordl.t-inv-qty
                ttPrintInventoryStockRM.totalReceived           = po-ordl.t-rec-qty
                ttPrintInventoryStockRM.totalQuantityReleased   = po-ordl.t-rel-qty
                ttPrintInventoryStockRM.vendorItemID            = po-ordl.vend-i-no
                ttPrintInventoryStockRM.unitCost                = po-ordl.cost
                .
        END.
    END.
    
    DELETE OBJECT hdJobProcs.
END.

PROCEDURE CreatePrintInventoryForFG:
    /*------------------------------------------------------------------------------
     Purpose: Creates temporary table to send data to a text file
     Notes: 
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcInventoryStockID LIKE inventoryTransaction.inventoryStockID NO-UNDO.
        
    DEFINE VARIABLE iIndex     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cJobNumber AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cText      AS CHARACTER NO-UNDO.    
    DEFINE VARIABLE cDeptNote  AS CHARACTER FORM "X(80)" EXTENT 18 NO-UNDO.
        
    FIND FIRST inventoryStock NO-LOCK
         WHERE inventoryStock.inventoryStockID EQ ipcinventoryStockID
         NO-ERROR.
    IF AVAILABLE inventoryStock THEN DO:
        CREATE ttPrintInventoryStockFG.
        
        FIND FIRST oe-ord NO-LOCK
             WHERE oe-ord.company EQ inventoryStock.company
               AND oe-ord.ord-no  EQ inventoryStock.orderID
               AND oe-ord.cust-no EQ inventoryStock.customerID 
             NO-ERROR.
        IF AVAILABLE oe-ord THEN DO:
            FIND FIRST oe-ordl NO-LOCK
                 WHERE oe-ordl.company EQ inventoryStock.company
                   AND oe-ordl.ord-no  EQ inventoryStock.orderID
                   AND oe-ordl.i-no    EQ inventoryStock.fgItemID 
                 NO-ERROR.
            IF AVAILABLE oe-ordl THEN DO:
                FIND FIRST oe-rel NO-LOCK
                     WHERE oe-rel.company  EQ inventoryStock.company
                       AND oe-rel.i-no     EQ oe-ordl.i-no
                       AND oe-rel.ord-no   EQ oe-ordl.ord-no
                       AND oe-rel.line     EQ oe-ordl.line
                       AND oe-rel.link-no  NE 0
                     NO-ERROR.
                IF AVAILABLE oe-rel THEN
                    ASSIGN
                        ttPrintInventoryStockFG.relDate  = oe-rel.rel-date
                        ttPrintInventoryStockFG.relLotNo = oe-rel.lot-no
                        .
                             
                ASSIGN
                    ttPrintInventoryStockFG.custPartID = oe-ordl.part-no
                    ttPrintInventoryStockFG.poID       = oe-ordl.po-no-po
                    ttPrintInventoryStockFG.custPOID   = oe-ordl.po-no
                    ttPrintInventoryStockFG.estID      = oe-ordl.est-no
                    ttPrintInventoryStockFG.partDesc1  = oe-ordl.part-dscr1
                    ttPrintInventoryStockFG.partDesc2  = oe-ordl.part-dscr2
                    ttPrintInventoryStockFG.linenum    = oe-ordl.e-num  
                    ttPrintInventoryStockFG.runShip    = STRING(oe-ordl.whsed,"R&S/WHSE")              
                    ttPrintInventoryStockFG.dueDate    = (IF oe-ord.due-date NE ? THEN
                                                            oe-ord.due-date
                                                         ELSE IF oe-ordl.req-date NE ? THEN
                                                            oe-ordl.req-date
                                                         ELSE 
                                                            TODAY
                                                         )
                    .
            END.
                
            ASSIGN
                ttPrintInventoryStockFG.customerName = oe-ord.cust-name
                ttPrintInventoryStockFG.soldCode     = oe-ord.sold-id
                ttPrintInventoryStockFG.soldName     = oe-ord.sold-name
                ttPrintInventoryStockFG.soldAdd1     = oe-ord.sold-add[1]
                ttPrintInventoryStockFG.soldAdd2     = oe-ord.sold-add[2]
                ttPrintInventoryStockFG.soldCity     = oe-ord.sold-city
                ttPrintInventoryStockFG.soldState    = oe-ord.sold-state
                ttPrintInventoryStockFG.soldZip      = oe-ord.sold-zip
                .

            FIND FIRST soldto NO-LOCK
                 WHERE soldto.company EQ inventoryStock.company
                   AND soldto.cust-no EQ oe-ord.cust-no
                   AND soldto.sold-id EQ oe-ord.sold-id
                   USE-INDEX sold-id 
                 NO-ERROR.
            IF AVAIL soldto THEN
                ttPrintInventoryStockFG.soldCtry = soldto.country.                
        END.
        ELSE DO:
            FIND FIRST cust NO-LOCK
                WHERE cust.company EQ inventoryStock.company
                  AND cust.cust-no EQ inventoryStock.customerID  
                NO-ERROR.
            IF AVAILABLE cust THEN
                ASSIGN
                    ttPrintInventoryStockFG.customerName = cust.name
                    ttPrintInventoryStockFG.palletID     = cust.spare-int-1 + 1
                    .
        END.
                         
        FIND FIRST itemfg NO-LOCK
             WHERE itemfg.company EQ inventoryStock.company
               AND itemfg.i-no    EQ inventoryStock.fgItemID
             NO-ERROR.
        IF AVAILABLE itemfg THEN DO:
            FIND FIRST eb NO-LOCK
                 WHERE eb.company  EQ itemfg.company
                   AND eb.est-no   EQ itemfg.est-no
                   AND eb.stock-no EQ itemfg.i-no
                 NO-ERROR.
            IF AVAILABLE eb THEN
                ASSIGN
                    ttPrintInventoryStockFG.caseID   = eb.cas-no
                    ttPrintInventoryStockFG.palletNo = eb.tr-no
                    .

            FOR EACH tt-formtext:
                DELETE tt-formtext.
            END.
            
            FOR EACH  notes NO-LOCK 
                WHERE notes.rec_key   EQ itemfg.rec_key
                  AND notes.note_code EQ "SN":
                cText = cText + " " + TRIM(notes.note_text) + CHR(10).
            END.
            
            DO iIndex = 1 TO 8:
                CREATE tt-formtext.
                ASSIGN tt-line-no = iIndex
                       tt-length  = 80.
            END.
            
            RUN custom/formtext.p (cText).
            
            iIndex = 0.           
            
            FOR EACH tt-formtext:
                iIndex = iIndex + 1.
                IF iIndex <= 8 THEN 
                    ttPrintInventoryStockFG.sn[iIndex] = tt-formtext.tt-text.      
            END.
                
            ASSIGN
                ttPrintInventoryStockFG.fgItemName  = itemfg.i-name
                ttPrintInventoryStockFG.upcID       = itemfg.upc-no
                ttPrintInventoryStockFG.flute       = itemfg.flute
                ttPrintInventoryStockFG.test        = itemfg.test
                ttPrintInventoryStockFG.sheetWt     = itemfg.weight-100 / 100
                ttPrintInventoryStockFG.style       = itemfg.style
                ttPrintInventoryStockFG.fgPartDesc1 = itemfg.part-dscr1
                ttPrintInventoryStockFG.fgPartDesc2 = itemfg.part-dscr2
                ttPrintInventoryStockFG.fgPartDesc3 = itemfg.part-dscr3
                ttPrintInventoryStockFG.palletType  = itemfg.trno
                ttPrintInventoryStockFG.zone        = itemfg.spare-char-4
                .
        END.
        
        IF ttPrintInventoryStockFG.style NE "" THEN
        DO:
            FIND FIRST style NO-LOCK
                 WHERE style.company EQ inventoryStock.company 
                   AND style.style   EQ ttPrintInventoryStockFG.style
                 NO-ERROR.
            IF AVAIL style THEN
                ttPrintInventoryStockFG.styleDesc = style.dscr.
        END.

        FIND FIRST job-hdr NO-LOCK
             WHERE job-hdr.job-no   EQ inventoryStock.jobID
               AND job-hdr.job-no2  EQ inventoryStock.jobID2
               AND job-hdr.frm      EQ inventoryStock.formNo
               AND job-hdr.blank-no EQ inventoryStock.blankNo
             NO-ERROR.
        IF AVAILABLE job-hdr THEN DO:
            FOR EACH shipto NO-LOCK
               WHERE shipto.company EQ inventoryStock.company
                 AND shipto.cust-no EQ job-hdr.cust-no
                 USE-INDEX ship-id
                 BREAK BY shipto.ship-no DESC:
                IF LAST(shipto.ship-no) THEN DO:
                    ASSIGN
                        ttPrintInventoryStockFG.shipNo    = shipto.ship-id
                        ttPrintInventoryStockFG.shipName  = shipto.ship-name
                        ttPrintInventoryStockFG.shipAdd1  = shipto.ship-add[1]
                        ttPrintInventoryStockFG.shipAdd2  = shipto.ship-add[2]
                        ttPrintInventoryStockFG.shipCity  = shipto.ship-city
                        ttPrintInventoryStockFG.shipState = shipto.ship-state
                        ttPrintInventoryStockFG.shipCtry  = shipto.country
                        ttPrintInventoryStockFG.shipZip   = shipto.ship-zip
                        .
                    LEAVE.
                END.
            END.
            
            ttPrintInventoryStockFG.dueDateJobHdr = IF job-hdr.due-date NE ? THEN 
                                                        STRING(job-hdr.due-date, "99/99/9999")
                                                    ELSE 
                                                        "".
        END.

        FIND FIRST job NO-LOCK 
             WHERE job.company EQ inventoryStock.company
               AND job.job-no  EQ inventoryStock.jobID
               AND job.job-no2 EQ inventoryStock.jobID2  
             NO-ERROR.
        IF AVAILABLE job THEN DO:
            ttPrintInventoryStockFG.dueDateJob  = IF job.due-date NE ? THEN 
                                                      STRING(job.due-date, "99/99/9999")
                                                  ELSE
                                                      "".
            IF AVAILABLE job-hdr THEN
                ttPrintInventoryStockFG.quantityJob = job-hdr.qty.
        END.
        
        FIND FIRST company NO-LOCK
             WHERE company.company  Eq inventoryStock.company
             NO-ERROR.
        IF AVAILABLE company THEN
            ttPrintInventoryStockFG.vendor = company.name.
        
        RUN oerep/ldtagSSCC.p (
            inventoryStock.company,
            inventoryStock.customerID,
            OUTPUT ttPrintInventoryStockFG.sscc
            ).
            
        ASSIGN
            cJobNumber                                 = IF inventoryStock.jobID NE "" THEN
                                                             LEFT-TRIM(TRIM(inventoryStock.jobID))
                                                         ELSE
                                                             ""
            cJobNumber                                 = IF inventoryStock.jobID NE "" THEN
                                                             FILL(" ",6 - LENGTH(cJobNumber)) + cJobNumber
                                                         ELSE
                                                             ""
            ttPrintInventoryStockFG.jobID              = IF inventoryStock.jobID NE "" THEN
                                                             cJobNumber + "-" + STRING(inventoryStock.jobID2,"99")
                                                         ELSE
                                                             ""
            ttPrintInventoryStockFG.fgItemID           = inventoryStock.fgItemID
            ttPrintInventoryStockFG.orderID            = inventoryStock.orderID
            ttPrintInventoryStockFG.quantityPerSubUnit = inventoryStock.quantityPerSubUnit
            ttPrintInventoryStockFG.quantityOfSubUnits = inventoryStock.quantityOfSubUnits
            ttPrintInventoryStockFG.quantity           = inventoryStock.quantity
            ttPrintInventoryStockFG.inventoryStockLen  = inventoryStock.inventoryStockLen
            ttPrintInventoryStockFG.inventoryStockWid  = inventoryStock.inventoryStockWid
            ttPrintInventoryStockFG.inventoryStockDep  = inventoryStock.inventoryStockDep
            ttPrintInventoryStockFG.weightTotal        = inventoryStock.weightTotal
            ttPrintInventoryStockFG.weightTare         = inventoryStock.weightTare
            ttPrintInventoryStockFG.weightNet          = inventoryStock.weightNet
            ttPrintInventoryStockFG.tag                = inventoryStock.tag
            ttPrintInventoryStockFG.quantityPartial    = inventoryStock.quantityPartial
            ttPrintInventoryStockFG.counterID          = INTEGER(SUBSTRING(inventoryStock.tag, 16, 5))
            ttPrintInventoryStockFG.uom                = inventoryStock.weightUOM
            ttPrintInventoryStockFG.tagCountTotal      = 1
            ttPrintInventoryStockFG.locationID         = inventoryStock.locationID
            ttPrintInventoryStockFG.warehouseID        = inventoryStock.warehouseID
            .
    END.
END PROCEDURE.

PROCEDURE DeleteInventoryStock:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcInventoryStockID AS CHARACTER NO-UNDO.
    
    DO TRANSACTION:
        FIND FIRST inventoryStock EXCLUSIVE-LOCK
             WHERE inventoryStock.inventoryStockID EQ ipcInventoryStockID
             NO-ERROR.
        IF AVAILABLE inventoryStock THEN
        DELETE inventoryStock.
    END. /* do trans */
END PROCEDURE.

PROCEDURE DeleteInventoryTransaction:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiTransactionID AS INTEGER NO-UNDO.
    
    DO TRANSACTION:
        FIND FIRST inventoryTransaction EXCLUSIVE-LOCK
             WHERE inventoryTransaction.inventoryTransactionID EQ ipiTransactionID
             NO-ERROR.
        IF AVAILABLE inventoryTransaction THEN
            DELETE inventoryTransaction.
    END. /* do trans */
END PROCEDURE.


PROCEDURE PostReceivedInventory:
    /*------------------------------------------------------------------------------
     Purpose: Change status of inventory stock from pending to posted.
     Notes: 
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany          LIKE inventoryStock.company                NO-UNDO.
    DEFINE INPUT PARAMETER ipcInventoryStockID LIKE inventoryTransaction.inventoryStockID NO-UNDO.
       
    FIND FIRST inventoryTransaction NO-LOCK
         WHERE inventoryTransaction.company           EQ ipcCompany
           AND inventoryTransaction.inventoryStockID  EQ ipcInventoryStockID
           AND inventoryTransaction.transactionType   EQ gcTransactionTypeReceive
           AND inventoryTransaction.transactionStatus EQ gcStatusTransactionInitial
         NO-ERROR.
    IF AVAILABLE inventoryTransaction THEN
        RUN PostTransaction(inventoryTransactionID).    
END PROCEDURE.

PROCEDURE CreateTransactionInitializedFromJob:
    /*------------------------------------------------------------------------------
     Purpose: Creates the inventoryStock from a Job
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany                 AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcJobno                   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcMachine                 AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiJobno2                  AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiFormno                  AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiBlankno                 AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcRMItem                  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdQuantityTotal           AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdQuantityPerSubUnit      AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiQuantitySubUnitsPerUnit AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcQuantityUOM             AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItemType                AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplCreated                 AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage                 AS CHARACTER NO-UNDO.

    DEFINE VARIABLE iCountOfLoadtags          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dQuantityPerFullLoadtag   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE iCountOfFullLoadtags      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dQuantityOfPartialLoadtag AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cInventoryStockID         AS CHARACTER NO-UNDO.

    FIND FIRST job-hdr NO-LOCK
         WHERE job-hdr.company  EQ ipcCompany
           AND job-hdr.job-no   EQ ipcJobno
           AND job-hdr.job-no2  EQ ipiJobno2
    NO-ERROR.
    IF NOT AVAILABLE job-hdr THEN DO:
        ASSIGN
            oplCreated = FALSE
            opcMessage = "Invalid Job Header".
        RETURN.
    END.
    
    FIND FIRST job NO-LOCK
         WHERE job.company EQ job-hdr.company
           AND job.job     EQ job-hdr.job
           AND job.job-no  EQ job-hdr.job-no
           AND job.job-no2 EQ job-hdr.job-no2
         NO-ERROR.  
    IF NOT AVAILABLE job-hdr THEN DO:
        ASSIGN
            oplCreated = FALSE
            opcMessage = "Invalid Job".
        RETURN.
    END.
               
    FIND FIRST job-mch NO-LOCK
         WHERE job-mch.company  EQ job-hdr.company
           AND job-mch.job      EQ job-hdr.job
           AND job-mch.job-no   EQ job-hdr.job-no
           AND job-mch.job-no2  EQ job-hdr.job-no2
           AND job-mch.m-code   EQ ipcMachine
           AND job-mch.frm      EQ ipiFormno
         NO-ERROR.
    IF NOT AVAILABLE job-mch THEN DO:
        ASSIGN
            oplCreated = FALSE
            opcMessage = "Invalid Machine".
        RETURN.
    END.

    FIND FIRST job-mat NO-LOCK  
         WHERE job-mat.company  EQ job-hdr.company
           AND job-mat.job      EQ job-hdr.job 
           AND job-mat.job-no   EQ job-hdr.job-no
           AND job-mat.job-no2  EQ job-hdr.job-no2
           AND job-mat.frm      EQ ipiFormNo
           AND (IF ipcRMItem    EQ "" THEN 
                    TRUE
                ELSE
                    job-mat.rm-i-no EQ ipcRMItem
               ) 
         NO-ERROR.  
    IF NOT AVAILABLE job-mat THEN DO:
        ASSIGN
            oplCreated = FALSE
            opcMessage = "Invalid Material".
        RETURN.
    END.

    ASSIGN 
        ipdQuantityPerSubUnit      = MAX(1, ipdQuantityPerSubUnit)
        ipiQuantitySubUnitsPerUnit = MAX(1, ipiQuantitySubUnitsPerUnit)
        iCountOfFullLoadtags       = fCalculateQuantitySubUnits (
                                         ipdQuantityTotal,
                                         ipdQuantityPerSubUnit
                                     )
        dQuantityOfPartialLoadtag  = fCalculateQuantityPartialSubUnit (
                                         ipdQuantityTotal,
                                         ipdQuantityPerSubUnit,
                                         iCountOfFullLoadtags
                                     ) 
        .

    IF dQuantityOfPartialLoadtag NE 0 AND iCountOfFullLoadtags EQ 1 THEN 
        ASSIGN 
            dQuantityOfPartialLoadtag = dQuantityOfPartialLoadtag + ipdQuantityPerSubUnit
            iCountOfFullLoadtags      = iCountOfFullLoadtags - 1
            .
        
    CASE ipcItemType:
        WHEN gcItemTypeWIP THEN DO:
            RUN CreatePreLoadtagsFromInputsWIP (
                INPUT  ROWID(job-mch),
                INPUT  ROWID(job-mat), 
                INPUT  ipdQuantityTotal,
                INPUT  ipdQuantityPerSubUnit,
                INPUT  ipiQuantitySubUnitsPerUnit,
                INPUT  ipcQuantityUOM,
                OUTPUT oplCreated,
                OUTPUT opcMessage
                ).

            RUN CreateInventoryLoadtagsFromPreLoadtags.

            FOR EACH ttInventoryStockLoadtag:
                ASSIGN 
                    oplCreated = NO
                    opcMessage = "". 
                RUN CreateInventoryStockFromLoadtag (
                    INPUT  ttInventoryStockLoadtag.inventoryStockID,
                    INPUT  YES,
                    INPUT  NO,
                    OUTPUT oplCreated,
                    OUTPUT opcMessage
                    ).
            END.   
        END.
        WHEN gcItemTypeFG THEN DO:
            DO iCountOfLoadtags = 1 TO iCountOfFullLoadtags:
                RUN CreateInventoryStockFromInputsFG (
                    INPUT  ROWID(job-mch),
                    INPUT  ROWID(job-mat), 
                    INPUT  ROWID(job-hdr),
                    INPUT  ipdQuantityPerSubUnit,
                    INPUT  ipdQuantityPerSubUnit,
                    INPUT  ipiQuantitySubUnitsPerUnit,
                    INPUT  ipcQuantityUOM,
                    OUTPUT cInventoryStockID,
                    OUTPUT oplCreated,
                    OUTPUT opcMessage
                    ).    
                
                IF oplCreated THEN
                    RUN CreateInventoryStockReceipt (
                        INPUT ipcCompany,
                        INPUT cInventoryStockID,
                        INPUT TRUE, /* Create Receipt Transaction */
                        INPUT FALSE, /* Post Receipt Transaction */
                        OUTPUT oplCreated,
                        OUTPUT opcMessage
                        ).                
            END. 
            IF dQuantityOfPartialLoadtag NE 0 THEN DO:
                RUN CreateInventoryStockFromInputsFG (
                    INPUT  ROWID(job-mch),
                    INPUT  ROWID(job-mat), 
                    INPUT  ROWID(job-hdr),
                    INPUT  dQuantityOfPartialLoadtag,
                    INPUT  dQuantityOfPartialLoadtag,
                    INPUT  ipiQuantitySubUnitsPerUnit,
                    INPUT  ipcQuantityUOM,
                    OUTPUT cInventoryStockID,
                    OUTPUT oplCreated,
                    OUTPUT opcMessage
                    ).            
                
                IF oplCreated THEN
                    RUN CreateInventoryStockReceipt (
                        INPUT  ipcCompany,
                        INPUT  cInventoryStockID,
                        INPUT  TRUE, /* Create Receipt Transaction */
                        INPUT  FALSE, /* Post Receipt Transaction */
                        OUTPUT oplCreated,
                        OUTPUT opcMessage
                        ).
            END.            
        END.
        WHEN gcItemTypeRM THEN DO:
        END.
    END CASE.
END PROCEDURE.

PROCEDURE CreateTransactionInitializedFromPO:
    /*------------------------------------------------------------------------------
     Purpose: Creates the inventoryStock from a PO
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany                 AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPONo                    AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiLine                    AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItem                    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdQuantityTotal           AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdQuantityPerSubUnit      AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiQuantitySubUnitsPerUnit AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcQuantityUOM             AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplCreated                 AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage                 AS CHARACTER NO-UNDO.

    DEFINE VARIABLE iCountOfLoadtags          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dQuantityPerFullLoadtag   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE iCountOfFullLoadtags      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dQuantityOfPartialLoadtag AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cInventoryStockID         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemType                 AS CHARACTER NO-UNDO.
    
    FIND FIRST po-ord NO-LOCK
         WHERE po-ord.company  EQ ipcCompany
           AND po-ord.po-no    EQ ipiPONo
         NO-ERROR.
    IF NOT AVAILABLE po-ord THEN DO:
        ASSIGN
            oplCreated = FALSE
            opcMessage = "Invalid PO".
        RETURN.
    END.

    FIND FIRST po-ordl NO-LOCK
         WHERE po-ordl.company  EQ ipcCompany
           AND po-ordl.po-no    EQ ipiPONo
           AND po-ordl.line     EQ ipiLine
           AND po-ordl.i-no     EQ ipcItem
         NO-ERROR.
    IF NOT AVAILABLE po-ordl THEN DO:
        ASSIGN
            oplCreated = FALSE
            opcMessage = "Invalid PO Line".
        RETURN.
    END.

    ASSIGN 
        ipdQuantityPerSubUnit      = MAX(1, ipdQuantityPerSubUnit)
        ipiQuantitySubUnitsPerUnit = MAX(1, ipiQuantitySubUnitsPerUnit)
        iCountOfFullLoadtags       = fCalculateQuantitySubUnits (
                                         ipdQuantityTotal,
                                         ipdQuantityPerSubUnit
                                     )
        dQuantityOfPartialLoadtag  = fCalculateQuantityPartialSubUnit (
                                         ipdQuantityTotal,
                                         ipdQuantityPerSubUnit,
                                         iCountOfFullLoadtags
                                     ) 
        cItemType                  = IF po-ordl.item-type THEN
                                         gcItemTypeRM
                                     ELSE
                                         gcItemTypeFG
        .

    IF dQuantityOfPartialLoadtag NE 0 AND iCountOfFullLoadtags EQ 1 THEN 
        ASSIGN 
            dQuantityOfPartialLoadtag = dQuantityOfPartialLoadtag + ipdQuantityPerSubUnit
            iCountOfFullLoadtags      = iCountOfFullLoadtags - 1
            .

    DO iCountOfLoadtags = 1 TO iCountOfFullLoadtags:
        RUN CreateInventoryStockFromInputsPO (
            ROWID(po-ord), 
            ROWID(po-ordl),
            ipdQuantityPerSubUnit,
            ipdQuantityPerSubUnit,
            ipiQuantitySubUnitsPerUnit,
            ipcQuantityUOM,
            OUTPUT cInventoryStockID,
            OUTPUT oplCreated,
            OUTPUT opcMessage
            ).    
        
        IF oplCreated THEN
            RUN CreateInventoryStockReceipt (
                ipcCompany,
                cInventoryStockID,
                TRUE, /* Create Receipt Transaction */
                FALSE, /* Post Receipt Transaction */
                OUTPUT oplCreated,
                OUTPUT opcMessage
                ).
    END. 
    IF dQuantityOfPartialLoadtag NE 0 THEN DO:
        RUN CreateInventoryStockFromInputsPO (
            ROWID(po-ord), 
            ROWID(po-ordl),
            dQuantityOfPartialLoadtag,
            dQuantityOfPartialLoadtag,
            ipiQuantitySubUnitsPerUnit,
            ipcQuantityUOM,
            OUTPUT cInventoryStockID,
            OUTPUT oplCreated,
            OUTPUT opcMessage
            ).    
        
        IF oplCreated THEN
            RUN CreateInventoryStockReceipt (
                ipcCompany,
                cInventoryStockID,
                TRUE, /* Create Receipt Transaction */
                FALSE, /* Post Receipt Transaction */
                OUTPUT oplCreated,
                OUTPUT opcMessage
                ).
    END.    
    
    ASSIGN
        oplCreated = TRUE
        opcMessage = "Tag(s) Created Successfully"
        .        
END PROCEDURE.

PROCEDURE CreateTransactionReceived:
    /*------------------------------------------------------------------------------
     Purpose: Given the Loadtag buffer, create the Stock inventory
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcInventoryStockID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplPost AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplCreated AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
     
    DEFINE VARIABLE iInventoryTransactionID AS INTEGER   NO-UNDO.
    
    DEFINE BUFFER bf-inventoryStock FOR inventoryStock.
    
    FIND FIRST bf-inventoryStock NO-LOCK
         WHERE bf-inventoryStock.company          EQ ipcCompany 
           AND bf-inventoryStock.inventoryStockID EQ ipcInventoryStockID NO-ERROR.
    IF AVAILABLE bf-inventoryStock THEN DO:
        RUN pCreateTransactionAndReturnID(bf-inventoryStock.company, bf-inventoryStock.inventoryStockID, gcTransactionTypeReceive, 
                bf-inventoryStock.quantityOriginal, bf-inventoryStock.quantityUOM, bf-inventoryStock.warehouseID, bf-inventoryStock.locationID, 
                OUTPUT iInventoryTransactionID, OUTPUT oplCreated, OUTPUT opcMessage).
        IF iplPost THEN 
            RUN PostTransaction(iInventoryTransactionID).
    END.
    RELEASE bf-inventoryStock.
    
END PROCEDURE.

PROCEDURE CreateTransactionTransfer:
    /*------------------------------------------------------------------------------
     Purpose: Wrapper function to create a transfer
     Notes: 0 quantity transaction
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany          AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcInventoryStockID AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcWarehouseID      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLocationID       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplPost             AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplCreated          AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage          AS CHARACTER NO-UNDO.

    DEFINE VARIABLE iTransactionID AS INTEGER NO-UNDO.

    RUN pCreateTransactionAndReturnID(ipcCompany, ipcInventoryStockID, gcTransactionTypeTransfer, 0, "", ipcWarehouseID, ipcLocationID, 
        OUTPUT iTransactionID, OUTPUT oplCreated, OUTPUT opcMessage).
    IF iplPost THEN 
        RUN PostTransaction(iTransactionID).

END PROCEDURE.

PROCEDURE CreateTransactionAdjustQuantity:
    /*------------------------------------------------------------------------------
     Purpose: Wrapper function to create a transfer
     Notes: 0 quantity transaction
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany          AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcInventoryStockID AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdQuantity         AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcQuantityUOM      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplPost             AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplCreated          AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage          AS CHARACTER NO-UNDO.

    DEFINE VARIABLE iTransactionID AS INTEGER NO-UNDO.

    DEFINE BUFFER bf-inventoryStock FOR inventoryStock.
    
    FIND FIRST bf-inventoryStock NO-LOCK
         WHERE bf-inventoryStock.company          EQ ipcCompany 
           AND bf-inventoryStock.inventoryStockID EQ ipcInventoryStockID
         NO-ERROR.
    IF NOT AVAILABLE bf-inventoryStock THEN DO:
        ASSIGN
            oplCreated = FALSE
            opcMessage = "Invalid InventoryStockID"
            .
        RETURN.
    END.

    RUN pCreateTransactionAndReturnID (
        ipcCompany, 
        ipcInventoryStockID, 
        gcTransactionTypeAdjustQty,
        ipdQuantity,
        ipcQuantityUOM,
        "",   /* WarehouseID */
        "",   /* Location ID */
        OUTPUT iTransactionID, 
        OUTPUT oplCreated, 
        OUTPUT opcMessage
        ).
     
    IF iplPost AND oplCreated THEN
        RUN PostTransaction(iTransactionID).
          
    ASSIGN
        oplCreated = TRUE
        opcMessage = "Inventory Transaction: " + STRING(iTransactionID) + " is created"
        .        
     
    RELEASE bf-inventoryStock.
END PROCEDURE.

PROCEDURE CreateTransactionCompare:
    /*------------------------------------------------------------------------------
     Purpose: Wrapper function to create a compare transaction
     Notes: 0 quantity transaction
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany          AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcStockIDAlias     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdQuantity         AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcQuantityUOM      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcWarehouseID      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLocationID       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplPost             AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplCreated          AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage          AS CHARACTER NO-UNDO.

    DEFINE VARIABLE iTransactionID AS INTEGER NO-UNDO.
    
    FIND FIRST inventoryTransaction EXCLUSIVE-LOCK
         WHERE inventoryTransaction.company EQ ipcCompany 
           AND inventoryTransaction.tag     EQ ipcStockIDAlias NO-ERROR.
           
    IF AVAILABLE inventoryTransaction THEN DO:
        ASSIGN
            inventoryTransaction.quantityChange         = ipdQuantity
            inventoryTransaction.quantityUOM            = ipcQuantityUOM
            inventoryTransaction.warehouseID            = ipcWarehouseID
            inventoryTransaction.locationID             = ipcLocationID
            inventoryTransaction.scannedTime            = NOW
            inventoryTransaction.scannedBy              = USERID(gcDBUser)
            oplCreated                                  = TRUE
            opcMessage                                  = "Transaction Updated"
            .
            
        RETURN.       
    END.
           
    RUN pCreateTransactionAndReturnID(ipcCompany, ipcStockIDAlias, gcTransactionTypeCompare, ipdQuantity, ipcQuantityUOM, ipcWarehouseID, ipcLocationID, 
        OUTPUT iTransactionID, OUTPUT oplCreated, OUTPUT opcMessage).    
            
             
END PROCEDURE.

PROCEDURE CreateTransactionConsume:
    /*------------------------------------------------------------------------------
     Purpose: Wrapper function to create an Issue/Consume transaction
     Notes: No location 
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcInventoryStockID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdQuantityConsumed AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcQuantityUOM AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplPost AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplCreated AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE VARIABLE iTransactionID AS INTEGER NO-UNDO.

    RUN pCreateTransactionAndReturnID(ipcCompany, ipcInventoryStockID, gcTransactionTypeConsume, - ipdQuantityConsumed, ipcQuantityUOM, "", "", 
        OUTPUT iTransactionID, OUTPUT oplCreated, OUTPUT opcMessage).
    IF iplPost THEN 
        RUN PostTransaction(iTransactionID).
    
END PROCEDURE.

PROCEDURE GenerateSnapshotRecords:
    /*------------------------------------------------------------------------------
     Purpose: Generate Snapshot records
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcType       AS CHARACTER NO-UNDO. /* FG, RM, WIP */
    DEFINE INPUT  PARAMETER ipcCompany    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcWarehouse  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLocation   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplCreated    AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage    AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE iInventorySnapshotID  AS INTEGER   NO-UNDO.
    
    IF ipcType EQ gcItemTypeFG THEN DO:
        IF CAN-FIND ( FIRST fg-bin NO-LOCK
                      WHERE fg-bin.company    EQ ipcCompany
                        AND fg-bin.loc        EQ ipcWarehouse
                        AND fg-bin.loc-bin    EQ ipcLocation
                        AND fg-bin.qty        NE 0
                        AND fg-bin.tag        NE "" ) THEN
            RUN pCreateSnapshotAndReturnID (
                ipcCompany,
                gcSnapshotTypeCount,
                ipcWarehouse,
                ipcLocation,
                gcSourceTypeSnapshot,
                gcItemTypeFG,
                OUTPUT iInventorySnapshotID,
                OUTPUT oplCreated,
                OUTPUT opcMessage
                ).
            

        FOR EACH fg-bin NO-LOCK
            WHERE fg-bin.company    EQ ipcCompany
              AND fg-bin.loc        EQ ipcWarehouse
              AND fg-bin.loc-bin    EQ ipcLocation
              AND fg-bin.qty        NE 0
              AND fg-bin.tag        NE "":
            RUN CreateInventoryStockSnapshotFromInputsFG (
                ROWID(fg-bin),
                iInventorySnapshotID,
                OUTPUT oplCreated,
                OUTPUT opcMessage
                ).
        END.
    END.
    
END PROCEDURE.

PROCEDURE BuildPhyScanBrowseFromSnapshotLocation:
    /*------------------------------------------------------------------------------
     Purpose: Rebuilds browse temp-table
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany          AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcWarehouseID      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcLocationID       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcTransactionType  AS CHARACTER NO-UNDO.

    FOR EACH inventoryStockSnapshot NO-LOCK
        WHERE inventoryStockSnapshot.company     EQ ipcCompany
          AND inventoryStockSnapshot.warehouseID EQ ipcWarehouseID
          AND inventoryStockSnapshot.locationID  EQ ipcLocationID:
        
        FIND FIRST inventoryTransaction  NO-LOCK
             WHERE inventoryTransaction.tag             EQ inventoryStockSnapshot.tag
               AND inventoryTransaction.transactionType EQ ipcTransactionType
             NO-ERROR.

        FIND FIRST ttPhysicalBrowseInventory NO-LOCK
             WHERE ttPhysicalBrowseInventory.company EQ inventoryStockSnapshot.company
               AND ttPhysicalBrowseInventory.tag     EQ inventoryStockSnapshot.tag
             NO-ERROR.
        IF NOT AVAILABLE ttPhysicalBrowseInventory THEN DO:
            CREATE ttPhysicalBrowseInventory.
            ASSIGN
                ttPhysicalBrowseInventory.company          = inventoryStockSnapshot.company
                ttPhysicalBrowseInventory.inventoryStockID = inventoryStockSnapshot.inventoryStockID
                ttPhysicalBrowseInventory.tag              = inventoryStockSnapshot.tag
                ttPhysicalBrowseInventory.itemType         = inventoryStockSnapshot.itemType
                ttPhysicalBrowseInventory.itemID           = IF inventoryStockSnapshot.fgItemID NE "" THEN
                                                                 inventoryStockSnapshot.fgItemID
                                                             ELSE IF inventoryStockSnapshot.rmItemID NE "" THEN
                                                                 inventoryStockSnapshot.rmItemID
                                                             ELSE
                                                                 inventoryStockSnapshot.wipItemID
                ttPhysicalBrowseInventory.quantity         = IF AVAILABLE inventoryTransaction THEN
                                                                 inventoryTransaction.quantityChange
                                                             ELSE    
                                                                 0
                ttPhysicalBrowseInventory.origQuantity     = inventoryStockSnapshot.quantity
                ttPhysicalBrowseInventory.customerID       = inventoryStockSnapshot.customerID
                ttPhysicalBrowseInventory.lastTransTime    = NOW
                ttPhysicalBrowseInventory.locationID       = IF AVAILABLE inventoryTransaction THEN
                                                                 inventoryTransaction.locationID
                                                             ELSE    
                                                                 ""
                ttPhysicalBrowseInventory.origLocationID   = inventoryStockSnapshot.locationID
                ttPhysicalBrowseInventory.warehouseID      = IF AVAILABLE inventoryTransaction THEN
                                                                 inventoryTransaction.warehouseID
                                                             ELSE    
                                                                 ""
                ttPhysicalBrowseInventory.origWarehouseID  = inventoryStockSnapshot.warehouseID
                ttPhysicalBrowseInventory.location         = IF AVAILABLE inventoryTransaction THEN
                                                                 inventoryTransaction.warehouseID +
                                                                 FILL(" ", 5 - LENGTH(inventoryTransaction.warehouseID)) +
                                                                 inventoryTransaction.locationID            
                                                             ELSE    
                                                                 ttPhysicalBrowseInventory.warehouseID +
                                                                 FILL(" ", 5 - LENGTH(ttPhysicalBrowseInventory.warehouseID)) +
                                                                 ttPhysicalBrowseInventory.locationID            
                ttPhysicalBrowseInventory.origLocation     = ttPhysicalBrowseInventory.origWarehouseID +
                                                             FILL(" ", 5 - LENGTH(ttPhysicalBrowseInventory.origWarehouseID)) +
                                                             ttPhysicalBrowseInventory.origLocationID            
                ttPhysicalBrowseInventory.inventoryStatus  = IF AVAILABLE inventoryTransaction THEN
                                                                 fGetSnapshotCompareStatus ( 
                                                                 ttPhysicalBrowseInventory.company,
                                                                 ttPhysicalBrowseInventory.tag,
                                                                 ttPhysicalBrowseInventory.quantity,
                                                                 ttPhysicalBrowseInventory.warehouseID,
                                                                 ttPhysicalBrowseInventory.locationID
                                                                 )
                                                             ELSE
                                                                gcStatusSnapshotNotScanned                                                         
                .
        END.  
    END.
END PROCEDURE.

PROCEDURE BuildPhyScanBrowseFromTransactionLocation:
    /*------------------------------------------------------------------------------
     Purpose: Rebuilds browse temp-table
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany          AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcWarehouseID      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcLocationID       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcTransactionType  AS CHARACTER NO-UNDO.

    FOR EACH inventoryTransaction NO-LOCK
        WHERE inventoryTransaction.company         EQ ipcCompany
          AND inventoryTransaction.transactionType EQ ipcTransactionType 
          AND inventoryTransaction.warehouseID     EQ ipcWarehouseID
          AND inventoryTransaction.locationID      EQ ipcLocationID:

        FIND FIRST ttPhysicalBrowseInventory NO-LOCK
             WHERE ttPhysicalBrowseInventory.tag EQ inventoryTransaction.tag
             NO-ERROR.
        IF NOT AVAILABLE ttPhysicalBrowseInventory THEN DO:
            CREATE ttPhysicalBrowseInventory.
            ASSIGN            
                ttPhysicalBrowseInventory.company          = inventoryTransaction.company
                ttPhysicalBrowseInventory.inventoryStockID = inventoryTransaction.inventoryStockID
                ttPhysicalBrowseInventory.tag              = inventoryTransaction.tag
                ttPhysicalBrowseInventory.quantity         = inventoryTransaction.quantityChange
                ttPhysicalBrowseInventory.lastTransTime    = inventoryTransaction.scannedTime
                ttPhysicalBrowseInventory.locationID       = inventoryTransaction.locationID
                ttPhysicalBrowseInventory.warehouseID      = inventoryTransaction.warehouseID
                ttPhysicalBrowseInventory.location         = inventoryTransaction.warehouseID +
                                                             FILL(" ", 5 - LENGTH(inventoryTransaction.warehouseID)) +
                                                             inventoryTransaction.locationID                                                   
                . 
                    
            FIND FIRST inventoryStockSnapshot NO-LOCK
                WHERE inventoryStockSnapshot.company EQ inventoryTransaction.company
                  AND inventoryStockSnapshot.tag     EQ inventoryTransaction.tag
                NO-ERROR.
            IF AVAILABLE inventoryStockSnapshot THEN
                ASSIGN
                    ttPhysicalBrowseInventory.itemType        = inventoryStockSnapshot.itemType
                    ttPhysicalBrowseInventory.itemID          = IF inventoryStockSnapshot.fgItemID NE "" THEN
                                                                    inventoryStockSnapshot.fgItemID
                                                                ELSE IF inventoryStockSnapshot.rmItemID NE "" THEN
                                                                    inventoryStockSnapshot.rmItemID
                                                                ELSE
                                                                    inventoryStockSnapshot.wipItemID
                    ttPhysicalBrowseInventory.origQuantity    = inventoryStockSnapshot.quantity
                    ttPhysicalBrowseInventory.origLocationID  = inventoryStockSnapshot.locationID
                    ttPhysicalBrowseInventory.origWarehouseID = inventoryStockSnapshot.warehouseID
                    ttPhysicalBrowseInventory.origLocation    = inventoryStockSnapshot.warehouseID +
                                                                FILL(" ", 5 - LENGTH(inventoryStockSnapshot.warehouseID)) +
                                                                inventoryStockSnapshot.locationID                                               
                    .
            ELSE DO:
                FIND FIRST loadtag NO-LOCK
                    WHERE loadtag.company EQ inventoryTransaction.company
                      AND loadtag.tag-no  EQ inventoryTransaction.tag
                    NO-ERROR.
                IF AVAILABLE loadtag THEN
                    ASSIGN
                        ttPhysicalBrowseInventory.itemType = IF loadtag.item-type THEN
                                                                 gcItemTypeRM
                                                             ELSE
                                                                 gcItemTypeFG
                        ttPhysicalBrowseInventory.itemID   = loadtag.i-no
                        .
                
            END.
    
            ttPhysicalBrowseInventory.inventoryStatus = fGetSnapshotCompareStatus (
                                                        ttPhysicalBrowseInventory.company,
                                                        ttPhysicalBrowseInventory.tag,
                                                        ttPhysicalBrowseInventory.quantity,
                                                        ttPhysicalBrowseInventory.warehouseID,
                                                        ttPhysicalBrowseInventory.locationID
                                                        ).
        END.
    END.
END PROCEDURE.

PROCEDURE BuildPhyScanBrowseFromTransactionUser:
    /*------------------------------------------------------------------------------
     Purpose: Rebuilds browse temp-table
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany          AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcUser             AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcTransactionType  AS CHARACTER NO-UNDO.

    FOR EACH inventoryTransaction NO-LOCK
        WHERE inventoryTransaction.company         EQ ipcCompany
          AND inventoryTransaction.transactionType EQ ipcTransactionType 
          AND inventoryTransaction.scannedBy       EQ ipcUser:

        FIND FIRST ttPhysicalBrowseInventory NO-LOCK
             WHERE ttPhysicalBrowseInventory.tag EQ inventoryTransaction.tag
             NO-ERROR.
        IF NOT AVAILABLE ttPhysicalBrowseInventory THEN DO:
            CREATE ttPhysicalBrowseInventory.
            ASSIGN            
                ttPhysicalBrowseInventory.company          = inventoryTransaction.company
                ttPhysicalBrowseInventory.inventoryStockID = inventoryTransaction.inventoryStockID
                ttPhysicalBrowseInventory.tag              = inventoryTransaction.tag
                ttPhysicalBrowseInventory.quantity         = inventoryTransaction.quantityChange
                ttPhysicalBrowseInventory.lastTransTime    = inventoryTransaction.scannedTime
                ttPhysicalBrowseInventory.locationID       = inventoryTransaction.locationID
                ttPhysicalBrowseInventory.warehouseID      = inventoryTransaction.warehouseID
                ttPhysicalBrowseInventory.location         = inventoryTransaction.warehouseID +
                                                             FILL(" ", 5 - LENGTH(inventoryTransaction.warehouseID)) +
                                                             inventoryTransaction.locationID                                                   
                . 
                    
            FIND FIRST inventoryStockSnapshot NO-LOCK
                WHERE inventoryStockSnapshot.company EQ inventoryTransaction.company
                  AND inventoryStockSnapshot.tag     EQ inventoryTransaction.tag
                NO-ERROR.
            IF AVAILABLE inventoryStockSnapshot THEN
                ASSIGN
                    ttPhysicalBrowseInventory.itemType        = inventoryStockSnapshot.itemType
                    ttPhysicalBrowseInventory.itemID          = IF inventoryStockSnapshot.fgItemID NE "" THEN
                                                                    inventoryStockSnapshot.fgItemID
                                                                ELSE IF inventoryStockSnapshot.rmItemID NE "" THEN
                                                                    inventoryStockSnapshot.rmItemID
                                                                ELSE
                                                                    inventoryStockSnapshot.wipItemID
                    ttPhysicalBrowseInventory.origQuantity    = inventoryStockSnapshot.quantity
                    ttPhysicalBrowseInventory.origLocationID  = inventoryStockSnapshot.locationID
                    ttPhysicalBrowseInventory.origWarehouseID = inventoryStockSnapshot.warehouseID
                    ttPhysicalBrowseInventory.origLocation    = inventoryStockSnapshot.warehouseID +
                                                                FILL(" ", 5 - LENGTH(inventoryStockSnapshot.warehouseID)) +
                                                                inventoryStockSnapshot.locationID                                               
                    .
            ELSE DO:
                FIND FIRST loadtag NO-LOCK
                    WHERE loadtag.company EQ inventoryTransaction.company
                      AND loadtag.tag-no  EQ inventoryTransaction.tag
                    NO-ERROR.
                IF AVAILABLE loadtag THEN
                    ASSIGN
                        ttPhysicalBrowseInventory.itemType = IF loadtag.item-type THEN
                                                                 gcItemTypeRM
                                                             ELSE
                                                                 gcItemTypeFG
                        ttPhysicalBrowseInventory.itemID   = loadtag.i-no
                        .
                
            END.
    
            ttPhysicalBrowseInventory.inventoryStatus = fGetSnapshotCompareStatus ( 
                                                        ttPhysicalBrowseInventory.company,
                                                        ttPhysicalBrowseInventory.tag,
                                                        ttPhysicalBrowseInventory.quantity,
                                                        ttPhysicalBrowseInventory.warehouseID,
                                                        ttPhysicalBrowseInventory.locationID
                                                        ).
        END.
    END.
END PROCEDURE.

PROCEDURE pAdjustTransactionQuantity:
    /*------------------------------------------------------------------------------
     Purpose: Confirm Tag as Not scanned
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany          AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTag              AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdQuantity         AS DECIMAL   NO-UNDO.    
    DEFINE OUTPUT PARAMETER oplCreated          AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage          AS CHARACTER NO-UNDO.
    
    FIND FIRST ttPhysicalBrowseInventory EXCLUSIVE-LOCK
         WHERE ttPhysicalBrowseInventory.company EQ ipcCompany
           AND ttPhysicalBrowseInventory.tag     EQ ipcTag
         NO-ERROR.
    
    IF AVAILABLE ttPhysicalBrowseInventory THEN DO:       
        RUN CreateTransactionCompare (
            ttPhysicalBrowseInventory.company,
            ttPhysicalBrowseInventory.tag,
            ipdQuantity,
            "",    /* Blank Quantity EOM */
            ttPhysicalBrowseInventory.warehouseID,
            ttPhysicalBrowseInventory.locationID,
            FALSE, /* Post transaction */
            OUTPUT oplCreated,
            OUTPUT opcMessage
            ).
    
        IF oplCreated THEN 
            ASSIGN
                ttPhysicalBrowseInventory.lastTransTime   = NOW
                ttPhysicalBrowseInventory.quantity        = ipdQuantity
                ttPhysicalBrowseInventory.inventoryStatus = fGetSnapshotCompareStatus (
                                                            ipcCompany,
                                                            ipcTag,
                                                            ipdQuantity,
                                                            ttPhysicalBrowseInventory.warehouseID,
                                                            ttPhysicalBrowseInventory.locationID
                                                            )
                .
    END.
END PROCEDURE.

PROCEDURE SubmitPhysicalCountScan:
    /*------------------------------------------------------------------------------
     Purpose: Submit Physical Count Scan
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcWarehouseID AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLocationID  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTag         AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplSetParamLoc AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplCreated     AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage     AS CHARACTER NO-UNDO.
    
    FIND FIRST inventoryStockSnapshot NO-LOCK
         WHERE inventoryStockSnapshot.company EQ ipcCompany
           AND inventoryStockSnapshot.tag     EQ ipcTag
         NO-ERROR.
           
    FIND FIRST ttPhysicalBrowseInventory EXCLUSIVE-LOCK
         WHERE ttPhysicalBrowseInventory.company EQ ipcCompany
           AND ttPhysicalBrowseInventory.tag     EQ ipcTag
         NO-ERROR.
    
    IF AVAILABLE inventoryStockSnapshot THEN DO:
        IF NOT AVAILABLE ttPhysicalBrowseInventory THEN DO:
            CREATE ttPhysicalBrowseInventory.
            ASSIGN
                ttPhysicalBrowseInventory.company          = inventoryStockSnapshot.company
                ttPhysicalBrowseInventory.inventoryStockID = inventoryStockSnapshot.inventoryStockID
                ttPhysicalBrowseInventory.tag              = inventoryStockSnapshot.tag
                ttPhysicalBrowseInventory.itemType         = inventoryStockSnapshot.itemType
                ttPhysicalBrowseInventory.itemID           = IF inventoryStockSnapshot.fgItemID NE "" THEN
                                                                 inventoryStockSnapshot.fgItemID
                                                             ELSE IF inventoryStockSnapshot.rmItemID NE "" THEN
                                                                 inventoryStockSnapshot.rmItemID
                                                             ELSE
                                                                 inventoryStockSnapshot.wipItemID
                ttPhysicalBrowseInventory.quantity         = inventoryStockSnapshot.quantity
                ttPhysicalBrowseInventory.customerID       = inventoryStockSnapshot.customerID
                ttPhysicalBrowseInventory.origQuantity     = inventoryStockSnapshot.quantity
                ttPhysicalBrowseInventory.origLocationID   = inventoryStockSnapshot.locationID
                ttPhysicalBrowseInventory.origWarehouseID  = inventoryStockSnapshot.warehouseID
                ttPhysicalBrowseInventory.origLocation     = ttPhysicalBrowseInventory.origWarehouseID +
                                                             FILL(" ", 5 - LENGTH(ttPhysicalBrowseInventory.origWarehouseID)) +
                                                             ttPhysicalBrowseInventory.origLocationID            
                .
        END.             
        
        IF NOT iplSetParamLoc THEN
            ASSIGN
                ttPhysicalBrowseInventory.quantity         = inventoryStockSnapshot.quantity
                ttPhysicalBrowseInventory.locationID       = inventoryStockSnapshot.locationID 
                ttPhysicalBrowseInventory.warehouseID      = inventoryStockSnapshot.warehouseID 
                ttPhysicalBrowseInventory.location         = ttPhysicalBrowseInventory.warehouseID +
                                                             FILL(" ", 5 - LENGTH(ttPhysicalBrowseInventory.warehouseID)) +
                                                             ttPhysicalBrowseInventory.locationID
                .       
    END. 
    ELSE DO:
        FIND FIRST loadtag NO-LOCK
             WHERE loadtag.company EQ ipcCompany
			   AND loadtag.tag-no  EQ ipcTag NO-ERROR.
        IF NOT AVAILABLE loadtag THEN DO:
            ASSIGN
		  oplCreated = FALSE
		  opcMessage = "Invalid Tag"
		  .
            RETURN.
        END.
        
        IF NOT AVAILABLE ttPhysicalBrowseInventory THEN DO:
            CREATE ttPhysicalBrowseInventory.
            ASSIGN            
                ttPhysicalBrowseInventory.company  = loadtag.company
                ttPhysicalBrowseInventory.tag      = ipcTag
                ttPhysicalBrowseInventory.itemID   = loadtag.i-no            
                ttPhysicalBrowseInventory.quantity = loadtag.qty
                .
            
            IF NOT iplSetParamLoc THEN
                ASSIGN
                    ttPhysicalBrowseInventory.locationID      = loadtag.loc-bin
                    ttPhysicalBrowseInventory.warehouseID     = loadtag.loc 
                    ttPhysicalBrowseInventory.location        = ttPhysicalBrowseInventory.warehouseID +
                                                                FILL(" ", 5 - LENGTH(ttPhysicalBrowseInventory.warehouseID)) +
                                                                ttPhysicalBrowseInventory.locationID
                    .
        END.    
    END.

    IF iplSetParamLoc THEN
        ASSIGN
            ttPhysicalBrowseInventory.locationID      = ipcLocationID
            ttPhysicalBrowseInventory.warehouseID     = ipcWarehouseID
            ttPhysicalBrowseInventory.location        = ipcWarehouseID +
                                                        FILL(" ", 5 - LENGTH(ipcWarehouseID)) +
                                                        ipcLocationID
            .

    RUN CreateTransactionCompare (
        ipcCompany,
        ipcTag,
        ttPhysicalBrowseInventory.quantity,
        "",    /* Blank Quantity EOM */
        ttPhysicalBrowseInventory.warehouseID,
        ttPhysicalBrowseInventory.locationID,
        FALSE, /* Post transaction */
        OUTPUT oplCreated,
        OUTPUT opcMessage
        ).
    
            
    ASSIGN
        ttPhysicalBrowseInventory.lastTransTime   = NOW
        ttPhysicalBrowseInventory.inventoryStatus = fGetSnapshotCompareStatus (
                                                    ipcCompany,
                                                    ttPhysicalBrowseInventory.tag,
                                                    ttPhysicalBrowseInventory.quantity,
                                                    ttPhysicalBrowseInventory.warehouseID,
                                                    ttPhysicalBrowseInventory.locationID
                                                    )
        .                                           
END PROCEDURE.

PROCEDURE RebuildRMBrowse:
    /*------------------------------------------------------------------------------
     Purpose: Rebuilds browse temp-table
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcJobno      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcMachine    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiJobno2     AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiFormno     AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiBlankno    AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcRMItem     AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiTotTags    AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opiTotOnHand  AS INTEGER   NO-UNDO.
    
    EMPTY TEMP-TABLE ttBrowseInventory.
    
    FOR EACH inventoryStock NO-LOCK
       WHERE inventoryStock.company   EQ ipcCompany
         AND inventoryStock.jobID     EQ ipcJobno
         AND inventoryStock.jobID2    EQ ipiJobno2   
         AND (IF ipcMachine           EQ "" THEN TRUE 
              ELSE inventoryStock.MachineID EQ ipcMachine)
         AND (IF ipcRMItem            EQ "" THEN TRUE
              ELSE inventoryStock.rmItemID  EQ ipcRMItem)
         AND inventoryStock.formNo    EQ ipiFormno   
         AND inventoryStock.blankNo   EQ ipiBlankno
        :
        CREATE ttBrowseInventory.
        BUFFER-COPY inventoryStock EXCEPT inventoryStock.locationID TO ttBrowseInventory.
        ASSIGN
            ttBrowseinventory.locationID = inventoryStock.warehouseID +
                                           FILL(" ", 5 - LENGTH(inventoryStock.warehouseID)) +
                                           inventoryStock.locationID
            opiTotTags                   = opiTotTags + 1.
         
        IF inventoryStock.inventoryStatus EQ gcStatusStockReceived THEN
            opiTotOnHand = opiTotOnHand + 1.
        
    END.
        
END PROCEDURE.

PROCEDURE RebuildBrowseTTFromPO:
    /*------------------------------------------------------------------------------
     Purpose: Rebuilds browse temp-table
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPONo       AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiLine       AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItem       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItemType   AS CHARACTER NO-UNDO.
    
    EMPTY TEMP-TABLE ttBrowseInventory.
    
    FOR EACH inventoryStock NO-LOCK
       WHERE inventoryStock.company   EQ ipcCompany
         AND inventoryStock.poID      EQ ipiPONo
         AND inventoryStock.poLine    EQ ipiLine
         AND (IF ipcItemType EQ gcItemTypeFG THEN
                  inventoryStock.fgItemID EQ ipcItem
              ELSE
                  TRUE
             )
         AND (IF ipcItemType EQ gcItemTypeRM THEN
                  inventoryStock.rmItemID EQ ipcItem
              ELSE
                  TRUE
             )
         AND (IF ipcItemType EQ gcItemTypeWIP THEN
                  inventoryStock.wipItemID EQ ipcItem
              ELSE
                  TRUE
             )
        :
        CREATE ttBrowseInventory.
        BUFFER-COPY inventoryStock EXCEPT inventoryStock.locationID TO ttBrowseInventory.
        ttBrowseinventory.locationID = inventoryStock.warehouseID +
                                       FILL(" ", 5 - LENGTH(inventoryStock.warehouseID)) +
                                       inventoryStock.locationID.         
    END.
        
END PROCEDURE.

PROCEDURE RebuildWIPBrowseTT:
    /*------------------------------------------------------------------------------
     Purpose: Rebuilds browse temp-table
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcJobno      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcMachine    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiJobno2     AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiFormno     AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiBlankno    AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opiTotTags    AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opiTotOnHand  AS INTEGER   NO-UNDO.
    
    EMPTY TEMP-TABLE ttBrowseInventory.
    
    FOR EACH inventoryStock NO-LOCK
       WHERE inventoryStock.company   EQ ipcCompany
         AND inventoryStock.jobID     EQ ipcJobno
         AND inventoryStock.jobID2    EQ ipiJobno2   
         AND (IF ipcMachine           EQ "" THEN TRUE 
              ELSE inventoryStock.MachineID EQ ipcMachine)
         AND inventoryStock.formNo    EQ ipiFormno:
        CREATE ttBrowseInventory.
        BUFFER-COPY inventoryStock EXCEPT inventoryStock.locationID TO ttBrowseInventory.
        ASSIGN
            ttBrowseinventory.locationID = inventoryStock.warehouseID +
                                           FILL(" ", 5 - LENGTH(inventoryStock.warehouseID)) +
                                           inventoryStock.locationID
            opiTotTags                   = opiTotTags + 1.
         
        IF inventoryStock.inventoryStatus EQ gcStatusStockReceived THEN
            opiTotOnHand = opiTotOnHand + 1.
        
    END.
        
END PROCEDURE.

PROCEDURE pCreateTransactionAndReturnID PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcInventoryStockID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcTransactionType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdQuantityChange AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcQuantityUOM AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcWarehouseID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcLocationID AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiInventoryTransactionID AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplCreated AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    CREATE inventoryTransaction.
    ASSIGN 
        inventoryTransaction.inventoryTransactionID = fGetNextTransactionID()
        opiInventoryTransactionID                   = inventoryTransaction.inventoryTransactionID
        inventoryTransaction.transactionType        = ipcTransactionType
        inventoryTransaction.company                = ipcCompany
        inventoryTransaction.createdBy              = USERID(gcDBUser)
        inventoryTransaction.createdTime            = NOW
        inventoryTransaction.scannedBy              = USERID(gcDBUser)
        inventoryTransaction.scannedTime            = inventoryTransaction.createdTime
        inventoryTransaction.quantityChange         = ipdQuantityChange
        inventoryTransaction.quantityUOM            = ipcQuantityUOM
        inventoryTransaction.warehouseID            = ipcWarehouseID
        inventoryTransaction.locationID             = ipcLocationID
        inventoryTransaction.transactionTime        = inventoryTransaction.createdTime  /*Default to Created Time, Not Posted*/
        inventoryTransaction.transactionStatus      = gcStatusTransactionInitial
        oplCreated                                  = YES
        opcMessage                                  = "Transaction Created.  ID: " + STRING(opiInventoryTransactionID)
        .
    RUN CheckInventoryStockTagID(ipcCompany, ipcInventoryStockID, OUTPUT inventoryTransaction.inventoryStockID, OUTPUT inventoryTransaction.tag).
    
    RELEASE inventoryTransaction.

END PROCEDURE.

PROCEDURE pCreateSnapshotAndReturnID PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany         AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcSnapshotType    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcWarehouse       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLocation        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcInventoryStatus AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItemType        AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiSnapshotID      AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplCreated         AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage         AS CHARACTER NO-UNDO.
        
    CREATE inventorySnapshot.
    ASSIGN 
        inventorySnapshot.inventorySnapshotID  = fGetNextSnapshotID()
        inventorySnapshot.snapshotType         = ipcSnapshotType
        inventorySnapshot.itemType             = ipcItemType
        inventorySnapshot.company              = ipcCompany
        inventorySnapshot.warehouseID          = ipcWarehouse
        inventorySnapshot.locationID           = ipcLocation
        inventorySnapshot.inventoryStockStatus = ipcInventoryStatus
        inventorySnapshot.snapshotUser         = USERID(gcDBUser)
        inventorySnapshot.snapshotTime         = NOW
        oplCreated                             = YES
        opcMessage                             = "Snapshot Created. ID: " + STRING(inventorySnapshot.inventorySnapshotID)
        opiSnapshotID                          = inventorySnapshot.inventorySnapshotID
        .
END PROCEDURE.

PROCEDURE pAddQuantity PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Adds additional quantity to base quantity.  Converts UOM if necessary
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipdQuantityChange AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcQuantityChangeUOM AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER ipdQuantityExisting AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcQuantityExistingUOM AS CHARACTER NO-UNDO.

    DEFINE VARIABLE dQuantityChangeInExistingUOM AS DECIMAL.

    IF ipcQuantityChangeUOM NE ipcQuantityExistingUOM THEN 
    DO:
        dQuantityChangeInExistingUOM = ipdQuantityChange.
    /*ConvertUOM and add*/
    END.
    ELSE 
        dQuantityChangeInExistingUOM = ipdQuantityChange.
    
    ipdQuantityExisting = ipdQuantityExisting + dQuantityChangeInExistingUOM.

END PROCEDURE.

PROCEDURE pCreateLoadtagFromPreLoadtag PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given Buffer for pre-loadtag and overall quantity, 
        create the Loadtag 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttInventoryStockPreLoadtag FOR ttInventoryStockPreLoadtag.
    DEFINE INPUT PARAMETER ipdQuantity AS DECIMAL NO-UNDO.
 
    DEFINE VARIABLE lAliasCreated       AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cAliasCreateMessage AS CHARACTER NO-UNDO.
    
    CREATE ttInventoryStockLoadtag.
    BUFFER-COPY ipbf-ttInventoryStockPreLoadtag TO ttInventoryStockLoadtag.
    ASSIGN 
        ttInventoryStockLoadtag.inventoryStockID = fGetNextStockID(ttInventoryStockLoadtag.itemType) /*Unique ID*/
        ttInventoryStockLoadtag.quantityOriginal = ipdQuantity
        ttInventoryStockLoadtag.inventoryStatus  = gcStatusStockLoadtag
        .
    /*Ensure the partial and unit counts are calculated correctly for this specific quantity*/
    RUN RecalcQuantityUnits(ttInventoryStockLoadtag.quantityOriginal, 
        INPUT-OUTPUT ttInventoryStockLoadtag.quantityPerSubUnit, INPUT-OUTPUT ttInventoryStockLoadtag.quantitySubUnitsPerUnit,
        OUTPUT ttInventoryStockLoadtag.quantityOfSubUnits, OUTPUT ttInventoryStockLoadtag.quantityOfUnits, OUTPUT ttInventoryStockLoadtag.quantityPartial).
    
    /*Build Readable Tag Number and register it on Alias table*/
    /*ttInventoryStockLoadtag.tag = fGetNextStockIDAlias(ttInventoryStockLoadtag.company, ttInventoryStockLoadtag.primaryID).*/ 
END PROCEDURE.

PROCEDURE CreateInventoryStockReceipt:
    /*------------------------------------------------------------------------------
     Purpose: Given the inventoryStockID, creates a received inventory transaction.
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany          AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcInventoryStockID AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplCreateReceipt    AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER iplPost             AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplCreated          AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage          AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE iInventoryTransactionID AS INTEGER   NO-UNDO.

    FIND FIRST inventoryStock NO-LOCK
         WHERE inventoryStock.company          EQ ipcCompany
           AND inventoryStock.inventoryStockID EQ ipcInventoryStockID
           NO-ERROR.
    IF AVAILABLE inventoryStock THEN DO:
        IF iplCreateReceipt THEN DO:
            FIND FIRST inventoryTransaction NO-LOCK
                 WHERE inventoryTransaction.company           EQ ipcCompany
                   AND inventoryTransaction.inventoryStockID  EQ ipcInventoryStockID
                   AND inventoryTransaction.transactionStatus EQ gcTransactionTypeReceive
                   NO-ERROR.
            IF NOT AVAILABLE inventoryTransaction THEN
                RUN pCreateTransactionAndReturnID (
                    inventoryStock.company, 
                    inventoryStock.inventoryStockID, 
                    gcTransactionTypeReceive, 
                    inventoryStock.quantityOriginal, 
                    inventoryStock.quantityUOM, 
                    inventoryStock.warehouseID, 
                    inventoryStock.locationID, 
                    OUTPUT iInventoryTransactionID, 
                    OUTPUT oplCreated, 
                    OUTPUT opcMessage
                    ).             
        END.
        IF iplCreateReceipt AND iplPost THEN 
            RUN PostTransaction (
                iInventoryTransactionID
                ).
    END.
    ELSE
        ASSIGN
            oplCreated = FALSE
            opcMessage = "Invalid Inventory Stock"
            .

    RELEASE inventoryStock.
END.

PROCEDURE pCreateStockFromLoadtag PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given the Loadtag buffer, create the Stock inventory
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttInventoryStockLoadtag FOR ttInventoryStockLoadtag.
    DEFINE INPUT PARAMETER iplCreateReceipt AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplPost AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplCreated AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
     
    DEFINE VARIABLE cPrimaryID              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iInventoryTransactionID AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cTag                    AS CHARACTER NO-UNDO.
    
    cTag = fGetNextStockIDAlias (
               ipbf-ttInventoryStockLoadtag.company, 
               ipbf-ttInventoryStockLoadtag.primaryID
           ).
           
    CREATE inventoryStock.
    BUFFER-COPY ipbf-ttInventoryStockLoadtag EXCEPT rec_key TO inventoryStock.
    ASSIGN 
        inventoryStock.inventoryStatus = gcStatusStockInitial
        inventoryStock.tag             = cTag
        oplCreated                     = YES
        opcMessage                     = "Inventory Stock Created for " + inventoryStock.inventoryStockID
        .
    IF iplCreateReceipt THEN 
        RUN pCreateTransactionAndReturnID(inventoryStock.company, inventoryStock.inventoryStockID, gcTransactionTypeReceive, 
            inventoryStock.quantityOriginal, inventoryStock.quantityUOM, inventoryStock.warehouseID, inventoryStock.locationID, 
            OUTPUT iInventoryTransactionID, OUTPUT oplCreated, OUTPUT opcMessage).
    IF iplCreateReceipt AND iplPost THEN 
        RUN PostTransaction(iInventoryTransactionID).
    RELEASE inventoryStock.
    
END PROCEDURE.

PROCEDURE pGenerateLoadtagDataFile PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:  Generates the data file for a given loadtag
 Notes:
------------------------------------------------------------------------------*/


END PROCEDURE.

PROCEDURE GetFullUnitQuantity:
    /*------------------------------------------------------------------------------
     Purpose: Given Quantity Per SubUnit and Count of SubUnits pre Unit, return the
     quantity of a full unit
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER iopdQuantityPerSubUnit AS DECIMAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiQuantitySubUnitsPerUnit AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdQuantityPerUnit AS DECIMAL NO-UNDO.

    ASSIGN 
        iopdQuantityPerSubUnit      = MAX(iopdQuantityPerSubUnit,1)
        iopiQuantitySubUnitsPerUnit = MAX(iopiQuantitySubUnitsPerUnit,1)
        opdQuantityPerUnit          = iopdQuantityPerSubUnit * iopiQuantitySubUnitsPerUnit
        .

END PROCEDURE.

PROCEDURE pGetWIPID PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Gets the WIP ID fields for a PreLoadtag buffer 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttInventoryStockPreLoadtag FOR ttInventoryStockPreLoadtag.
    DEFINE OUTPUT PARAMETER opcWIPID AS CHARACTER NO-UNDO.

    opcWIPID = STRING(ipbf-ttInventoryStockPreLoadtag.machineID,"x(6)") + STRING(ipbf-ttInventoryStockPreLoadtag.jobID,"x(6)") 
        + STRING(ipbf-ttInventoryStockPreLoadtag.jobID2,"99") + STRING(ipbf-ttInventoryStockPreLoadtag.formNo,"99")  
        + STRING(ipbf-ttInventoryStockPreLoadtag.blankNo,"99").

    IF TRIM(opcWIPID) EQ "" THEN opcWIPID = "WIPITEM".

END PROCEDURE.

PROCEDURE PostTransaction:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiInventoryTransactionID AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bf-inventoryTransaction FOR inventoryTransaction.

    FIND FIRST inventoryTransaction EXCLUSIVE-LOCK
        WHERE inventoryTransaction.inventoryTransactionID EQ ipiInventoryTransactionID
        NO-ERROR.
    FIND FIRST inventoryStock EXCLUSIVE-LOCK
        WHERE inventoryStock.inventoryStockID EQ inventoryTransaction.inventoryStockID
        NO-ERROR. 
    IF AVAILABLE inventoryTransaction AND AVAILABLE inventoryStock THEN 
    DO:
        IF inventoryTransaction.quantityChange NE 0 AND inventoryTransaction.transactionType NE gcTransactionTypeAdjustQty THEN 
            RUN pAddQuantity(inventoryTransaction.quantityChange, inventoryTransaction.quantityUOM, INPUT-OUTPUT inventoryStock.quantity, inventoryStock.quantityUOM). 
        IF inventoryTransaction.quantityChange NE 0 AND inventoryTransaction.transactionType EQ gcTransactionTypeAdjustQty THEN 
            RUN pAddQuantity(inventoryTransaction.quantityChange, inventoryTransaction.quantityUOM, INPUT-OUTPUT inventoryStock.quantityOriginal, inventoryStock.quantityUOM). 
        IF inventoryTransaction.warehouseID NE "" THEN 
            inventoryStock.warehouseID = inventoryTransaction.warehouseID.
        IF inventoryTransaction.locationID NE "" THEN 
            inventoryStock.locationID = inventoryTransaction.locationID.
        CASE inventoryTransaction.transactionType:
            WHEN gcTransactionTypeReceive THEN 
                DO:
                    ASSIGN 
                        inventoryStock.inventoryStatus = gcStatusStockReceived.
                END.
            WHEN gcTransactionTypeTransfer THEN 
                DO:
                    ASSIGN 
                        inventoryStock.lastTransBy   = USERID(gcDBUser)
                        inventoryStock.lastTransTime = NOW
                        .           
                END.
            WHEN gcTransactionTypeConsume OR 
            WHEN gcTransactionTypeShip THEN 
                DO:
                    IF inventoryStock.quantity EQ 0 THEN 
                    DO: 
                        ASSIGN 
                            inventoryStock.consumedBy   = USERID(gcDBUser)
                            inventoryStock.consumedTime = NOW
                            inventoryStock.inventoryStatus = gcStatusStockConsumed.
                    END.
                    ELSE 
                        ASSIGN 
                            inventoryStock.consumedBy   = ""
                            inventoryStock.consumedTime = ?.
                END.
             WHEN gcTransactionTypeAdjustQty THEN
                 DO:
                     RUN RecalcQuantityUnits (
                         inventoryStock.quantityOriginal, 
                         INPUT-OUTPUT inventoryStock.quantityPerSubUnit,
                         INPUT-OUTPUT inventoryStock.quantitySubUnitsPerUnit,
                         OUTPUT inventoryStock.quantityOfSubUnits,
                         OUTPUT inventoryStock.quantityOfUnits,
                         OUTPUT inventoryStock.quantityPartial
                         ).
            
                     FIND FIRST bf-inventoryTransaction EXCLUSIVE-LOCK
                          WHERE bf-inventoryTransaction.company           EQ inventoryTransaction.company
                            AND bf-inventoryTransaction.inventoryStockID  EQ inventoryTransaction.inventoryStockID
                            AND bf-inventoryTransaction.transactionType   EQ gcTransactionTypeReceive
                            AND bf-inventoryTransaction.transactionStatus EQ gcStatusTransactionInitial
                          NO-ERROR.
                     IF AVAILABLE bf-inventoryTransaction THEN
                         bf-inventoryTransaction.quantityChange = inventoryStock.quantityOriginal.
                 END.
        END CASE. 
        ASSIGN 
            inventoryTransaction.transactionStatus = gcStatusTransactionPosted
            inventoryTransaction.postedBy          = USERID(gcDBUser)
            inventoryTransaction.postedTime        = NOW
            .
    END.
    RELEASE inventoryTransaction.
    RELEASE bf-inventoryTransaction.
    RELEASE inventoryStock.

END PROCEDURE.

PROCEDURE RecalcQuantityUnits:
    /*------------------------------------------------------------------------------
     Purpose: Given a quantity and unit count, return units and partial
     Notes:
     Syntax: RUN RecalcQuantityUnits IN hInventoryProcs (dQuantityTotal, INPUT-OUTPUT dCaseCount, INPUT-OUTPUT iCasesPerPallet, 
            OUTPUT iCases, OUTPUT iPallets, OUTPUT dPartial).
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipdQuantityTotal AS DECIMAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopdQuantityPerSubUnit AS DECIMAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiQuantitySubUnitsPerUnit AS INTEGER NO-UNDO. 
    DEFINE OUTPUT PARAMETER opiQuantityOfSubUnits AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiQuantityOfUnits AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdQuantityPartialSubUnit AS DECIMAL NO-UNDO.

    ASSIGN 
        iopdQuantityPerSubUnit      = MAX(1,iopdQuantityPerSubUnit) 
        iopiQuantitySubUnitsPerUnit = MAX(1,iopiQuantitySubUnitsPerUnit)
        opiQuantityOfSubUnits       = TRUNC(ipdQuantityTotal / iopdQuantityPerSubUnit, 0)
        opdQuantityPartialSubUnit   = ipdQuantityTotal - iopdQuantityPerSubUnit * opiQuantityOfSubUnits
        opiQuantityOfUnits          = INTEGER(TRUNC(opiQuantityOfSubUnits / iopiQuantitySubUnitsPerUnit, 0)) 
        + INTEGER((opiQuantityOfSubUnits MODULO iopiQuantitySubUnitsPerUnit) NE 0)
        .  
    
END PROCEDURE.

PROCEDURE CreateInventoryLoadtagsFromPreLoadtags:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE iCountOfLoadtags          AS INTEGER NO-UNDO.
    DEFINE VARIABLE dQuantityPerFullLoadtag   AS DECIMAL NO-UNDO.
    DEFINE VARIABLE iCountOfFullLoadtags      AS INTEGER NO-UNDO.
    DEFINE VARIABLE dQuantityOfPartialLoadtag AS DECIMAL NO-UNDO.
    
    EMPTY TEMP-TABLE ttInventoryStockLoadtag.
    /*Process Inputs to "explode" the loadtag records required based on inputs*/
    FOR EACH ttInventoryStockPreLoadtag:
        ttInventoryStockPreLoadtag.countOfLoadtags = MAX(ttInventoryStockPreLoadtag.countOfLoadtags,1).    

        RUN GetFullUnitQuantity(INPUT-OUTPUT ttInventoryStockPreLoadtag.quantityPerSubUnit, INPUT-OUTPUT ttInventoryStockPreLoadtag.quantitySubUnitsPerUnit, OUTPUT dQuantityPerFullLoadtag).

        ASSIGN 
            iCountOfFullLoadtags      = INTEGER(TRUNC(ttInventoryStockPreLoadtag.quantityTotal / dQuantityPerFullLoadtag, 0))
            dQuantityOfPartialLoadtag = ttInventoryStockPreLoadtag.quantityTotal - dQuantityPerFullLoadtag * iCountOfFullLoadtags
            .

        IF dQuantityOfPartialLoadtag NE 0 AND iCountOfFullLoadtags EQ ttInventoryStockPreLoadtag.countOfLoadtags THEN 
            ASSIGN 
                dQuantityOfPartialLoadtag = dQuantityOfPartialLoadtag + dQuantityPerFullLoadtag
                iCountOfFullLoadtags      = iCountOfFullLoadtags - 1
                .    

        DO iCountOfLoadtags = 1 TO iCountOfFullLoadtags:
            RUN pCreateLoadtagFromPreLoadtag(BUFFER ttInventoryStockPreLoadtag, dQuantityPerFullLoadtag).
        END. 
        IF dQuantityOfPartialLoadtag NE 0 THEN 
            RUN pCreateLoadtagFromPreLoadtag(BUFFER ttInventoryStockPreLoadtag, dQuantityOfPartialLoadtag).    
    END.

END PROCEDURE.

PROCEDURE ValidateBin:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLoc AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcBin AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValidBin AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lActiveLoc AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lActiveBin AS LOGICAL NO-UNDO.

    RUN ValidateLoc IN THIS-PROCEDURE (INPUT ipcCompany, INPUT ipcLoc, OUTPUT lActiveLoc).
    
    lActiveBin = CAN-FIND(FIRST fg-bin NO-LOCK 
        WHERE fg-bin.company EQ ipcCompany  
        AND fg-bin.loc     EQ ipcLoc 
        AND fg-bin.loc-bin EQ ipcBin
        AND fg-bin.i-no    EQ ""
        AND fg-bin.active  EQ TRUE).
    
    oplValidBin = lActiveLoc AND lActiveBin.
    

END PROCEDURE.

PROCEDURE ValidateLoc:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLoc AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValidLoc AS LOGICAL NO-UNDO.
    
    oplValidLoc = CAN-FIND(FIRST loc NO-LOCK 
        WHERE loc.company EQ ipcCompany  
        AND loc.loc     EQ ipcLoc 
        AND loc.active  EQ TRUE).
        

END PROCEDURE.

PROCEDURE ValidatePO:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPO      AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValidPO AS LOGICAL NO-UNDO.
    
    oplValidPO = CAN-FIND(FIRST po-ord NO-LOCK 
        WHERE po-ord.company EQ ipcCompany  
          AND po-ord.po-no   EQ ipiPO).
        

END PROCEDURE.

PROCEDURE ValidateCust:
    /*------------------------------------------------------------------------------
     Purpose: Validate Customer number
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCustID  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid   AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    oplValid = CAN-FIND(FIRST cust NO-LOCK 
                        WHERE cust.company EQ ipcCompany  
                          AND cust.cust-no EQ ipcCustID).
    
    IF oplValid THEN
        opcMessage = "Success".
    ELSE
        opcMessage = "Invalid Customer '" + ipcCustID + "'".
END PROCEDURE.

PROCEDURE ValidatePOLine:
    /*------------------------------------------------------------------------------
     Purpose: Validation for PO Line
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPOID        AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPOLine      AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValidPOLine AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage     AS CHARACTER NO-UNDO.

    oplValidPOLine = CAN-FIND(FIRST po-ordl NO-LOCK 
                              WHERE po-ordl.company EQ ipcCompany  
                                AND po-ordl.po-no   EQ ipiPOID
                                AND po-ordl.line    EQ ipiPOLine). 

    IF NOT oplValidPOLine THEN
        opcMessage = "Invalid PO Line".
    ELSE
        opcMessage = "Success".                                
END PROCEDURE.

PROCEDURE Inventory_GetLoadTagJob:
    /*------------------------------------------------------------------------------
     Purpose: Procedure to fetch job-no and job-no2 for a loadtag
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItemType AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTag      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcJobID    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiJobID2   AS INTEGER   NO-UNDO.    
    DEFINE OUTPUT PARAMETER oplValidTag AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage  AS CHARACTER NO-UNDO.    

    DEFINE VARIABLE lItemType AS LOGICAL NO-UNDO.
        
    DEFINE BUFFER bf-loadtag FOR loadtag.
    
    RUN ValidateLoadTag (
        INPUT  ipcCompany,
        INPUT  ipcItemType,
        INPUT  ipcTag,
        OUTPUT oplValidTag,
        OUTPUT opcMessage
        ) NO-ERROR.
    IF NOT oplValidTag THEN
        RETURN.

    lItemType = ipcItemType EQ gcItemTypeRM.
    
    FIND FIRST bf-loadtag NO-LOCK
         WHERE bf-loadtag.company   EQ ipcCompany
           AND bf-loadtag.item-type EQ lItemType
           AND bf-loadtag.tag-no    EQ ipcTag
         NO-ERROR.
    IF AVAILABLE bf-loadtag THEN
        ASSIGN
            opcJobID  = bf-loadtag.job-no
            opiJobID2 = bf-loadtag.job-no2
            .
    
    RELEASE bf-loadtag.    
END PROCEDURE.

PROCEDURE ValidateLoadTag:
    /*------------------------------------------------------------------------------
     Purpose: Validation for tag-no in loadtag table
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItemType AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTag      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValidTag AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage  AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lItemType AS LOGICAL NO-UNDO.

    IF ipcItemType NE gcItemTypeFG AND 
       ipcItemType NE gcItemTypeRM THEN DO:
        ASSIGN
            oplValidTag = FALSE
            opcMessage  = "Invalid Item Type"
            .
        RETURN.
    END.

    lItemType = ipcItemType EQ gcItemTypeRM.

    oplValidTag = CAN-FIND(FIRST loadtag NO-LOCK 
                           WHERE loadtag.company   EQ ipcCompany  
                             AND loadtag.item-type EQ lItemType
                             AND loadtag.tag-no    EQ ipcTag).

    IF NOT oplValidTag THEN
        opcMessage = "Invalid Tag".
    ELSE
        opcMessage = "Success".
END PROCEDURE.

PROCEDURE GetItemListForPO:
    /*------------------------------------------------------------------------------
     Purpose: Returns blank no list for a given jobID
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT        PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipiPONo         AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipcStatus       AS CHARACTER NO-UNDO. /* "A" - All, "O" - Opened Only, "C" - Closed Only */
    DEFINE INPUT-OUTPUT PARAMETER opcItemList     AS CHARACTER NO-UNDO.

    DEFINE BUFFER buf-po-ordl FOR po-ordl.
    
    FOR EACH buf-po-ordl NO-LOCK
        WHERE buf-po-ordl.company EQ ipcCompany
          AND buf-po-ordl.po-no   EQ ipiPONo
          AND (ipcStatus  EQ "A"                             OR
               (ipcStatus EQ "C" AND NOT buf-po-ordl.opened) OR
               (ipcStatus EQ "O" AND buf-po-ordl.opened)
              ):
        opcItemList = IF opcItemList EQ "" THEN (STRING(buf-po-ordl.line,"99") + "-" + STRING(buf-po-ordl.i-no))
                      ELSE IF INDEX(opcItemList,STRING(buf-po-ordl.line,"99") + "-" + STRING(buf-po-ordl.i-no)) GT 0 THEN opcItemList
                      ELSE (opcItemList + "," + STRING(buf-po-ordl.line,"99") + "-" + STRING(buf-po-ordl.i-no)).
    END.

    RELEASE buf-po-ordl.
END PROCEDURE.

PROCEDURE LocationParser:
/*------------------------------------------------------------------------------
 Purpose: Location parser
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcLocation    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcWarehouseID AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcLocationID  AS CHARACTER NO-UNDO.    
    
    ASSIGN    
        opcWarehouseID = SUBSTRING(ipcLocation, 1, 5)
        opcLocationID  = SUBSTRING(ipcLocation, 6)
        .
     
END PROCEDURE.

PROCEDURE GetWarehouseList:
/*------------------------------------------------------------------------------
 Purpose: Get the list of warehouse in a string for a given company
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany            AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplActiveOnly         AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcWarehouseListItems AS CHARACTER NO-UNDO.
    
    FOR EACH loc NO-LOCK
        WHERE (IF ipcCompany EQ "" THEN
                   TRUE
               ELSE
                   loc.company = ipcCompany)
          AND (IF iplActiveOnly THEN 
                   loc.active = TRUE
               ELSE 
                   TRUE)      
        BY loc.loc:
        opcWarehouseListItems = IF opcWarehouseListItems = "" THEN 
                                    loc.loc
                                ELSE IF INDEX(opcWarehouseListItems ,loc.loc) GT 0 THEN 
                                    opcWarehouseListItems
                                ELSE
                                    opcWarehouseListItems + "," + loc.loc
                                .    
        
    END.
     
END PROCEDURE.

PROCEDURE pCanFindInventoryStock:
    /*------------------------------------------------------------------------------
     Purpose: Validate Inventory Stock
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany             AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTag                 AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValidInventoryStock AS LOGICAL   NO-UNDO.
    
    oplValidInventoryStock = CAN-FIND(FIRST inventoryStock NO-LOCK
         WHERE inventoryStock.company EQ ipcCompany
           AND inventoryStock.tag     EQ ipcTag).        

END PROCEDURE.

PROCEDURE pCanFindInventoryStockLocation:
    /*------------------------------------------------------------------------------
     Purpose: Validate Inventory Stock Location
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany             AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTag                 AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcWarehouseID         AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLocationID          AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValidInvStockLoc    AS LOGICAL   NO-UNDO.
    
    oplValidInvStockLoc = CAN-FIND(FIRST inventoryStock NO-LOCK
         WHERE inventoryStock.company      EQ ipcCompany
           AND inventoryStock.tag          EQ ipcTag
           AND inventoryStock.warehouseID  EQ ipcWarehouseID
           AND inventoryStock.locationID   EQ ipcLocationID).        

END PROCEDURE.

PROCEDURE pGetInventoryStockJobDetails:
    /*------------------------------------------------------------------------------
     Purpose: Fetch Job details of a Inventory Stock record
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany             AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTag                 AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcJobno               AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiJobno2              AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opiFormno              AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opiBlankno             AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMachine             AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValidInvStock       AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage             AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-inventoryStock FOR inventoryStock.
    DEFINE BUFFER bf-loadtag        FOR loadtag.
    
    FIND FIRST bf-inventoryStock NO-LOCK
         WHERE bf-inventoryStock.company          EQ ipcCompany
           AND bf-inventoryStock.inventoryStockID EQ ipcTag
         NO-ERROR.
        
    IF NOT AVAILABLE bf-inventoryStock THEN
        FIND FIRST bf-inventoryStock NO-LOCK
             WHERE bf-inventoryStock.company EQ ipcCompany
               AND bf-inventoryStock.tag     EQ ipcTag
             NO-ERROR.
    
    IF NOT AVAILABLE bf-inventoryStock THEN
        FIND FIRST bf-inventoryStock NO-LOCK
             WHERE bf-inventoryStock.company   EQ ipcCompany
               AND bf-inventoryStock.tagVendor EQ ipcTag
             NO-ERROR.    
    
    IF AVAILABLE bf-inventoryStock THEN DO:
        ASSIGN 
            opcJobno         = bf-inventoryStock.jobID
            opcMachine       = bf-inventoryStock.machineID
            opiJobno2        = bf-inventoryStock.jobID2
            opiFormno        = bf-inventoryStock.formNo
            opiBlankno       = bf-inventoryStock.blankNo
            oplValidInvStock = TRUE
            opcMessage       = "Success"
            .
        RETURN.
    END.
    ELSE DO:
        /* Find first loadtag for FG item */
        FIND FIRST bf-loadtag NO-LOCK
             WHERE bf-loadtag.company   EQ ipcCompany
               AND bf-loadtag.item-type EQ NO
               AND bf-loadtag.tag-no    EQ ipcTag
             NO-ERROR.
        IF NOT AVAILABLE bf-loadtag THEN
            /* Find first loadtag for RM item */
            FIND FIRST bf-loadtag NO-LOCK
                 WHERE bf-loadtag.company   EQ ipcCompany
                   AND bf-loadtag.item-type EQ YES
                   AND bf-loadtag.tag-no    EQ ipcTag 
                 NO-ERROR.

        /* If loadtag is not available then search for vendor tag in loadtag table,
           This is a temporary code fix an may have to be removed after conversion to 
           inventoryStock tables as loadtag.misc-char is not indexed and may affect
           the application performance */
        IF NOT AVAILABLE bf-loadtag THEN
            FIND FIRST bf-loadtag NO-LOCK
                 WHERE bf-loadtag.company      EQ ipcCompany
                   AND bf-loadtag.misc-char[1] EQ ipcTag
                 NO-ERROR.
         
        IF AVAILABLE bf-loadtag THEN DO:
            ASSIGN 
                opcJobno         = bf-loadtag.job-no
                opcMachine       = ""
                opiJobno2        = bf-loadtag.job-no2
                opiFormno        = bf-loadtag.form-no
                opiBlankno       = bf-loadtag.blank-no
                oplValidInvStock = TRUE
                opcMessage       = "Success"
                .
            RETURN.
        END.
        ELSE       
            ASSIGN
                oplValidInvStock = FALSE
                opcMessage       = "Invalid Tag"
                .
    END.
END PROCEDURE.

PROCEDURE pGetInventoryStockDetails:
    /*------------------------------------------------------------------------------
     Purpose: Fetch Details of a Inventory Stock record into temp-table
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany             AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcStockIDAlias        AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValidInvStock       AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage             AS CHARACTER NO-UNDO.     
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttInventoryStockDetails.
        
    FIND FIRST inventoryStock NO-LOCK
         WHERE inventoryStock.company EQ ipcCompany
           AND inventoryStock.tag     EQ ipcStockIDAlias
         NO-ERROR.
    
    EMPTY TEMP-TABLE ttInventoryStockDetails.
    
    IF AVAILABLE inventoryStock THEN DO:
        CREATE ttInventoryStockDetails.
        BUFFER-COPY inventoryStock TO ttInventoryStockDetails.
        
        ASSIGN
            oplValidInvStock = TRUE
            opcMessage       = ""
            .
    END.
    ELSE
        ASSIGN
            oplValidInvStock    = FALSE
            opcMessage          = "Invalid Tag"
            .
END PROCEDURE.

PROCEDURE GetPOOrderLineDetails:
    /*------------------------------------------------------------------------------
     Purpose: Fetch Details of a po-ordl record into temp-table
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany             AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPONo                AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiLine                AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItem                AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValidOrdLine        AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage             AS CHARACTER NO-UNDO.     
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttPOOrderLineDetails.
        
    FIND FIRST po-ordl NO-LOCK
         WHERE po-ordl.company EQ ipcCompany
           AND po-ordl.po-no   EQ ipiPONo
           AND po-ordl.line    EQ ipiLine
           AND po-ordl.i-no    EQ ipcItem
         NO-ERROR.
    
    EMPTY TEMP-TABLE ttPOOrderLineDetails.
    
    IF AVAILABLE po-ordl THEN DO:
        CREATE ttPOOrderLineDetails.
        BUFFER-COPY po-ordl TO ttPOOrderLineDetails.
        
        ASSIGN
            oplValidOrdLine = TRUE
            opcMessage      = ""
            .
    END.
    ELSE
        ASSIGN
            oplValidOrdLine = FALSE
            opcMessage      = "Invalid PO order line"
            .
END PROCEDURE.

PROCEDURE GetRMLoadTagDetails:
    DEFINE INPUT  PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTag      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcJobNo    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiJobNo2   AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opiFormNo   AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opiBlankNo  AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcQtyUOM   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcItemName AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValidTag AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage  AS CHARACTER NO-UNDO.

    FIND FIRST loadtag NO-LOCK
        WHERE loadtag.company   EQ ipcCompany
          AND loadtag.item-type EQ TRUE
          AND loadtag.tag-no    EQ ipcTag NO-ERROR.
    IF NOT AVAILABLE loadtag THEN DO:
        oplValidTag = FALSE.
        RETURN.
    END.

    FOR EACH rm-rcpth NO-LOCK
         WHERE rm-rcpth.company   EQ loadtag.company
           AND rm-rcpth.i-no      EQ loadtag.i-no
           AND rm-rcpth.rita-code EQ "I":
        FIND FIRST rm-rdtlh NO-LOCK
             WHERE rm-rdtlh.r-no      EQ rm-rcpth.r-no
               AND rm-rdtlh.rita-code EQ rm-rcpth.rita-code
               AND rm-rdtlh.tag       EQ loadtag.tag-no
             NO-ERROR.
        IF AVAILABLE rm-rdtlh THEN DO:
            ASSIGN
                oplValidTag = TRUE
                opcJobNo    = rm-rdtlh.job-no
                opiJobNo2   = rm-rdtlh.job-no2
                opiFormNo   = rm-rdtlh.s-num
                opiBlankNo  = rm-rdtlh.b-num
                opcItemName = rm-rcpth.i-name
                opcQtyUOM   = rm-rcpth.pur-uom
                .
            LEAVE.
        END.
    END.
END PROCEDURE.

PROCEDURE PostFinishedGoodsForUser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT        PARAMETER ipcCompany        AS CHARACTER NO-UNDO.
DEFINE INPUT        PARAMETER ipcTransType      AS CHARACTER NO-UNDO.
DEFINE INPUT        PARAMETER ipcUsername       AS CHARACTER NO-UNDO.
DEFINE INPUT        PARAMETER iplPromptForClose AS LOGICAL   NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER ioplSuccess       AS LOGICAL   NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopcMessage       AS CHARACTER NO-UNDO.

    /* Create  workfile records for the finished goods being posted */
    RUN fg/fgRecsByUser.p (
        INPUT ipcCompany,
        INPUT ipcTransType, 
        INPUT ipcUsername, 
        INPUT TABLE w-fg-rctd BY-REFERENCE
        ) NO-ERROR.
    
    IF ERROR-STATUS:ERROR THEN DO:
       ASSIGN
           iopcMessage = ERROR-STATUS:GET-MESSAGE(1)
           ioplSuccess = NO
           .
           
       RETURN.
    END.
    
    /* Posts FG items */
    RUN fg/fgpostBatch.p ( 
        INPUT TODAY,             /* Post date      */
        INPUT NO,                /* tg-recalc-cost */
        INPUT ipcTransType, 	 /* Transfer  */
        INPUT NO,                /* Send fg emails */
        INPUT YES,               /* creates work GL */
        INPUT iplPromptForClose, /* Executes closing orders logic based input */   
        INPUT TABLE w-fg-rctd  BY-REFERENCE,
        INPUT TABLE tt-fgemail BY-REFERENCE,
        INPUT TABLE tt-email   BY-REFERENCE,
        INPUT TABLE tt-inv     BY-REFERENCE
        )NO-ERROR.
    
    IF ERROR-STATUS:ERROR THEN DO:
       ASSIGN
           iopcMessage = ERROR-STATUS:GET-MESSAGE(1)
           ioplSuccess = NO
           .
           
       RETURN.
    END.
   
END PROCEDURE.

PROCEDURE Inventory_CreateWIPInventoryStockForIssuedRM:
    DEFINE INPUT  PARAMETER ipriRmrdtlh AS ROWID     NO-UNDO.
    DEFINE INPUT  PARAMETER ipriRmrcpth AS ROWID     NO-UNDO.
    DEFINE INPUT  PARAMETER ipriRmbin   AS ROWID     NO-UNDO.
    DEFINE INPUT  PARAMETER ipriJobmat  AS ROWID     NO-UNDO.
    DEFINE INPUT  PARAMETER ipriItem    AS ROWID     NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess  AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage  AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-rm-rdtlh FOR rm-rdtlh.
    DEFINE BUFFER bf-rm-rcpth FOR rm-rcpth.
    DEFINE BUFFER bf-rm-bin   FOR rm-bin.
    DEFINE BUFFER bf-job-mat  FOR job-mat.
    DEFINE BUFFER bf-item     FOR item.
    DEFINE BUFFER bf-po-ordl  FOR po-ordl.

    DEFINE VARIABLE dLength    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dWidth     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dDepth     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cVendorTag AS CHARACTER NO-UNDO.
                        
    FIND FIRST bf-rm-rdtlh NO-LOCK
         WHERE ROWID(bf-rm-rdtlh) EQ ipriRmrdtlh NO-ERROR.
    IF NOT AVAILABLE bf-rm-rdtlh THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "Invalid rm-rdtlh record"
            .
        RETURN.
    END.

    FIND FIRST bf-rm-rcpth NO-LOCK
         WHERE ROWID(bf-rm-rcpth) EQ ipriRmrcpth NO-ERROR.
    IF NOT AVAILABLE bf-rm-rcpth THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "Invalid rm-rcpth record"
            .
        RETURN.
    END.

    FIND FIRST bf-rm-bin NO-LOCK
         WHERE ROWID(bf-rm-bin) EQ ipriRmbin NO-ERROR.
    IF NOT AVAILABLE bf-rm-bin THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "Invalid rm-bin record"
            .
        RETURN.
    END.

    FIND FIRST bf-job-mat NO-LOCK
         WHERE ROWID(bf-job-mat) EQ ipriJobmat NO-ERROR.

    FIND FIRST bf-item NO-LOCK
         WHERE ROWID(bf-item) EQ ipriItem NO-ERROR.
    IF NOT AVAILABLE bf-item THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "Invalid item record"
            .
        RETURN.
    END.

    IF bf-rm-rdtlh.tag EQ "" THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "Empty tag value sent in receipt" 
            .
        RETURN.    
    END.
    
    FIND FIRST inventoryStock NO-LOCK
         WHERE inventoryStock.company          EQ bf-rm-rdtlh.company
           AND inventoryStock.inventoryStockID EQ bf-rm-rdtlh.tag
         NO-ERROR.
    IF AVAILABLE inventoryStock THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "InventoryStock already exists with tag " + bf-rm-rdtlh.tag 
            .
        RETURN.    
    END.
    
    FIND FIRST bf-po-ordl NO-LOCK
         WHERE bf-po-ordl.company EQ bf-rm-rcpth.company            
           AND bf-po-ordl.po-no   EQ INTEGER(bf-rm-rcpth.po-no)
           AND bf-po-ordl.line    EQ bf-rm-rcpth.po-line
         NO-ERROR.
    IF AVAILABLE bf-po-ordl THEN
        ASSIGN
            dLength = bf-po-ordl.s-len
            dWidth  = bf-po-ordl.s-wid
            dDepth  = bf-po-ordl.s-dep
            .

    cVendorTag = fGetVendorTagFromLoadTag (
                    cVendorTag,
                    TRUE, /* Item Type: True - RM, False - FG */
                    bf-rm-rdtlh.tag
                    ).
                                             
    CREATE inventoryStock.
    ASSIGN
        inventoryStock.company                    = bf-rm-rdtlh.company
        inventoryStock.jobID                      = bf-rm-rdtlh.job-no
        inventoryStock.jobID2                     = bf-rm-rdtlh.job-no2
        inventoryStock.formNo                     = bf-rm-rdtlh.s-num
        inventoryStock.blankNo                    = bf-rm-rdtlh.b-num
        inventoryStock.warehouseID                = bf-rm-rdtlh.loc
        inventoryStock.locationID                 = bf-rm-rdtlh.loc-bin
        inventoryStock.customerID                 = bf-rm-bin.cust-no
        inventoryStock.WIPItemID                  = bf-rm-rdtlh.i-no
        inventoryStock.poID                       = INTEGER(bf-rm-rcpth.po-no)
        inventoryStock.poLine                     = bf-rm-rcpth.po-line
        inventoryStock.itemType                   = gcItemTypeWIP
        inventoryStock.quantity                   = bf-rm-rdtlh.qty
        inventoryStock.quantityOriginal           = bf-rm-rdtlh.qty
        inventoryStock.primaryID                  = bf-rm-rdtlh.i-no
        inventoryStock.costStandardPerUOM         = bf-rm-rdtlh.cost
        inventoryStock.quantityPerSubUnit         = 1
        inventoryStock.quantityOfSubUnits         = 1
        inventoryStock.quantitySubUnitsPerUnit    = 1
        inventoryStock.quantityPartial            = 0
        inventoryStock.quantityOfUnits            = 1
        inventoryStock.quantityOfUnitsOriginal    = 1
        inventoryStock.quantityPartialOriginal    = 0
        inventoryStock.quantityOfSubUnitsOriginal = 1        
        inventoryStock.dimEachLen                 = IF AVAILABLE bf-job-mat AND bf-job-mat.len NE 0 THEN
                                                        bf-job-mat.len
                                                    ELSE IF bf-item.s-len NE 0 AND bf-item.i-code EQ "R" THEN
                                                        bf-item.s-len
                                                    ELSE
                                                        dLength
        inventoryStock.dimEachWid                 = IF AVAILABLE bf-job-mat AND bf-job-mat.wid NE 0 THEN
                                                        bf-job-mat.wid
                                                    ELSE IF bf-item.s-wid NE 0 AND bf-item.i-code EQ "R" THEN
                                                        bf-item.s-wid
                                                    ELSE
                                                        dWidth
        inventoryStock.dimEachDep                 = IF bf-item.s-dep NE 0 AND bf-item.i-code EQ "R" THEN
                                                        bf-item.s-dep
                                                    ELSE
                                                        dDepth
        inventoryStock.quantityUOM                = bf-rm-rcpth.pur-uom
        inventoryStock.dimEachUOM                 = gcUOMInches
        inventoryStock.costUOM                    = bf-rm-rcpth.pur-uom
        inventoryStock.basisWeightUOM             = gcUOMWeightBasisLBSPerSQFT
        inventoryStock.weightUOM                  = gcUOMWeightPound
        inventoryStock.basisWeight                = bf-item.basis-w
        inventoryStock.costStandardMat            = costStandardPerUOM
        inventoryStock.sourceID                   = bf-rm-rcpth.po-no
        inventoryStock.sourceType                 = gcInventorySourceTypePO
        inventoryStock.inventoryStatus            = gcStatusStockReceived
        inventoryStock.inventoryStockID           = IF bf-rm-rdtlh.tag NE "" THEN
                                                        bf-rm-rdtlh.tag
                                                    ELSE
                                                        fGetNextStockID (
                                                            inventoryStock.itemType
                                                        )
        inventoryStock.tag                        = inventoryStock.inventoryStockID
        inventoryStock.createdTime                = NOW
        inventoryStock.createdBy                  = USERID(gcDBUser)
        inventoryStock.lastTransBy                = USERID(gcDBUser)
        NO-ERROR.

    IF ERROR-STATUS:ERROR THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "Error creating inventoryStock"
            .            
        RETURN.            
    END.

    IF cVendorTag NE "" THEN
        inventoryStock.tagVendor = cVendorTag.

    ASSIGN
        oplSuccess = TRUE
        opcMessage = "InventoryStock created successfully"
        .
    
    RELEASE bf-rm-rdtlh.
    RELEASE bf-rm-rcpth.
    RELEASE bf-rm-bin.
    RELEASE bf-job-mat.
    RELEASE bf-item.
    RELEASE bf-po-ordl.                 
END PROCEDURE.

PROCEDURE UpdateTagStatusID:
/*------------------------------------------------------------------------------
 Purpose: Updates statusID for a FG bin
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprifgbin   AS ROWID     NO-UNDO.
    DEFINE INPUT  PARAMETER ipcStatusID AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess  AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage  AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-fg-bin FOR fg-bin.
    
    oplSuccess = YES.
     
    FIND FIRST bf-fg-bin EXCLUSIVE-LOCK
         WHERE ROWID(bf-fg-bin) EQ iprifgbin
         NO-WAIT NO-ERROR.
    /* Checks whether bin available or not */
    IF NOT AVAILABLE bf-fg-bin THEN DO:
        ASSIGN
            oplSuccess = NO
            opcMessage = "fg-bin not available"
            .
 
        RETURN.
    END.
    /* Checks whether row locked or not */
    IF LOCKED bf-fg-bin THEN DO:
        ASSIGN
            oplSuccess = NO
            opcMessage = "fg-bin record locked"
            .
            
        RETURN.
    
    END. 
    /* Updates bin status ID */
    bf-fg-bin.statusID = ipcStatusID.
    RELEASE bf-fg-bin.        
END.
/* ************************  Function Implementations ***************** */

FUNCTION fCanDeleteInventoryStock RETURNS LOGICAL 
	(ipcInventoryStockID AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RETURN CAN-FIND(FIRST inventoryStock
                    WHERE inventoryStock.inventoryStockID EQ ipcInventoryStockID
                      AND inventoryStock.inventoryStatus  EQ "Created").	

END FUNCTION.

FUNCTION fGetNextSnapshotID RETURNS INTEGER PRIVATE
    (  ):
    /*------------------------------------------------------------------------------
     Purpose: Returns the next snapshot ID
     Notes:
    ------------------------------------------------------------------------------*/	
    
    giIDTemp = NEXT-VALUE(snapshotid_seq).
    RETURN giIDTemp.
		
END FUNCTION.

FUNCTION fGetNextStockIDAlias RETURNS CHARACTER 
    ( ipcCompany AS CHARACTER , ipcUniquePrefix AS CHARACTER ):

    /*------------------------------------------------------------------------------
     Purpose: Returns the next Alias (tag number) to use for a given unique prefix
     this will search prefix across the loadtag types and return a character string for the full
     alias (Tag)
     Notes:
    ------------------------------------------------------------------------------*/

    DEFINE VARIABLE iNextTag   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iLastFGTag AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iLastRMTag AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iLastAlias AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cAlias     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iStartChar AS INTEGER   NO-UNDO.
    
    DEFINE BUFFER bf-inventoryStock FOR inventoryStock.
    
    iStartChar = LENGTH(ipcUniquePrefix) + 1.
    FIND LAST loadtag NO-LOCK
        WHERE loadtag.company     EQ ipcCompany
        AND loadtag.item-type   EQ NO
        AND loadtag.is-case-tag EQ NO
        AND loadtag.tag-no      BEGINS ipcUniquePrefix
        AND SUBSTR(loadtag.tag-no,1,15) EQ ipcUniquePrefix
        USE-INDEX tag NO-ERROR.
    iLastFGTag = (IF AVAILABLE loadtag THEN fGetNumberSuffix(loadtag.tag-no, iStartChar) ELSE 0) + 1.

    FIND LAST loadtag NO-LOCK
        WHERE loadtag.company     EQ ipcCompany
        AND loadtag.item-type   EQ YES
        AND loadtag.is-case-tag EQ NO
        AND loadtag.tag-no      BEGINS ipcUniquePrefix
        AND SUBSTR(loadtag.tag-no,1,15) EQ ipcUniquePrefix
        USE-INDEX tag NO-ERROR.
    iLastRMTag = (IF AVAILABLE loadtag THEN fGetNumberSuffix(loadtag.tag-no, iStartChar) ELSE 0) + 1.
    
    FIND LAST bf-inventoryStock NO-LOCK     
        WHERE bf-inventoryStock.company   EQ ipcCompany
          AND bf-inventoryStock.primaryID EQ ipcUniquePrefix
          AND bf-inventoryStock.tag       NE ""
        NO-ERROR.

    iLastAlias = (IF AVAILABLE bf-inventoryStock THEN fGetNumberSuffix(bf-inventoryStock.tag, iStartChar) ELSE 0) + 1.
    iNextTag = MAX(iLastFGTag, iLastRMTag, iLastAlias).

    cAlias = ipcUniquePrefix + FILL(" ", giLengthUniquePrefix - iStartChar + 1).
    cAlias = cAlias + STRING(iNextTag, FILL("9",giLengthAlias - LENGTH(cAlias))).

    RELEASE bf-inventoryStock.
    RETURN cAlias.

		
END FUNCTION.

FUNCTION fGetNextStockID RETURNS CHARACTER 
    (ipcType AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose: Returns the next stock ID
     Notes:
    ------------------------------------------------------------------------------*/	
    giIDTemp = NEXT-VALUE(invstockid_seq).
    
    RETURN ipcType + STRING(giIDTemp,"999999999999").

		
END FUNCTION.

FUNCTION fGetNextTransactionID RETURNS INTEGER
    (  ):
    /*------------------------------------------------------------------------------
     Purpose: Returns the next transaction ID
     Notes:
    ------------------------------------------------------------------------------*/    
    giIDTemp = NEXT-VALUE(invtrans_seq).
    RETURN giIDTemp.

END FUNCTION.

FUNCTION fGetNumberSuffix RETURNS INTEGER PRIVATE
    (ipcFullText AS CHARACTER , ipiStartChar AS INTEGER):
    /*------------------------------------------------------------------------------
     Purpose: Returns an integer given a large string with a number 
     Notes:
    ------------------------------------------------------------------------------*/	
    DEFINE VARIABLE iCountBeginningChars AS INTEGER.
    DEFINE VARIABLE iNumberSuffix        AS INTEGER.
    
    iNumberSuffix = INTEGER(SUBSTRING(ipcFullText, ipiStartChar, (LENGTH(ipcFullText) - ipiStartChar + 1))) NO-ERROR.

    IF ERROR-STATUS:ERROR THEN
        iNumberSuffix = 0.
    
    RETURN iNumberSuffix.
END FUNCTION.

FUNCTION fGetSnapshotCompareStatus RETURNS CHARACTER
    (ipcCompany AS CHARACTER , ipcTag AS CHARACTER , ipdQuantity AS DECIMAL , ipcWarehouseID AS CHARACTER , ipcLocationID AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose: Gets the compare status
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE opcStatus                   AS CHARACTER NO-UNDO.

    DEFINE VARIABLE lLocationChanged            AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lQuantityChanged            AS LOGICAL   NO-UNDO.
    
    opcStatus = gcStatusSnapshotTagNotFound.

    FIND FIRST inventoryStockSnapshot NO-LOCK
         WHERE inventoryStockSnapshot.company EQ ipcCompany
           AND inventoryStockSnapshot.tag     EQ ipcTag
         NO-ERROR.

    IF AVAILABLE inventoryStockSnapshot THEN DO:
        IF ipdQuantity NE inventoryStockSnapshot.quantity THEN
            ASSIGN
                lQuantityChanged = TRUE
                opcStatus        = gcStatusSnapshotQtyChange.
        
        IF ipcWarehouseID NE inventoryStockSnapshot.warehouseID OR
           ipcLocationID  NE inventoryStockSnapshot.locationID THEN
            ASSIGN
                lLocationChanged = TRUE
                opcStatus        = gcStatusSnapshotLocChange.
           
        IF lLocationChanged AND lQuantityChanged THEN
            opcStatus = gcStatusSnapshotQtyAndLocChange.
            
        IF NOT lLocationChanged AND NOT lQuantityChanged THEN
            opcStatus = gcStatusSnapshotCompleteMatch.    
    END.
    
    FIND FIRST inventoryTransaction NO-LOCK
         WHERE inventoryTransaction.company         EQ ipcCompany
           AND inventoryTransaction.tag             EQ ipcTag
           AND inventoryTransaction.transactionType EQ gcTransactionTypeCompare NO-ERROR.
    IF AVAILABLE inventoryTransaction
        AND inventoryTransaction.quantityChange EQ 0
        AND inventoryTransaction.scannedTime EQ inventoryTransaction.createdTime THEN
        opcStatus = gcStatusSnapshotNotScannedConf.
    
    FIND FIRST loadtag NO-LOCK
         WHERE loadtag.company EQ ipcCompany
           AND loadtag.tag-no  EQ ipcTag
         NO-ERROR.
    IF AVAILABLE loadtag AND NOT AVAILABLE inventoryStockSnapshot THEN
        opcStatus = gcStatusSnapshotTagNotFound.

    RETURN opcStatus.

END FUNCTION.

FUNCTION fGetRowBGColor RETURNS INTEGER
    (ipcInventoryStatus AS CHARACTER):

    DEFINE VARIABLE iColor AS INTEGER NO-UNDO.

    CASE ipcInventoryStatus:
        WHEN gcStatusSnapshotNotScanned      THEN
            iColor = 8. /* Grey */            
        WHEN gcStatusSnapshotNotScannedConf  THEN
            iColor = 14. /* Yellow */
        WHEN gcStatusSnapshotCompleteMatch   THEN
            iColor = 10. /* Green */
        WHEN gcStatusSnapshotLocChange       THEN
            iColor = 11. /* Cyan */
        WHEN gcStatusSnapshotQtyChange       THEN
            iColor = 11. /* Cyan */
        WHEN gcStatusSnapshotQtyAndLocChange THEN
            iColor = 20. /* Blue */
        WHEN gcStatusSnapshotTagNotFound     THEN
            iColor = 12. /* Red */
    END CASE.
    
    RETURN iColor.
    
END FUNCTION.

FUNCTION fCalculateQuantitySubUnits RETURNS DECIMAL
    (ipdQuantityTotal AS DECIMAL, ipdQuantitySubUnitCount AS DECIMAL):
    RETURN TRUNC(ipdQuantityTotal / ipdQuantitySubUnitCount, 0).
END FUNCTION.

FUNCTION fCalculateQuantityUnitCount RETURNS DECIMAL
    (ipdQuantitySubUnitCount AS DECIMAL, ipdQuantitySubUnitsPerUnit AS DECIMAL):
    RETURN ipdQuantitySubUnitCount * ipdQuantitySubUnitsPerUnit.
END FUNCTION.

FUNCTION fCalculateQuantityUnits RETURNS INTEGER
    (ipdQuantitySubUnits AS DECIMAL, ipdQuantitySubUnitsPerUnit AS DECIMAL, ipdQuantityPartialSubUnit AS DECIMAL):
     IF ipdQuantitySubUnitsPerUnit EQ 0 THEN
         ipdQuantitySubUnitsPerUnit = 1.
     RETURN INTEGER(TRUNC(ipdQuantitySubUnits / ipdQuantitySubUnitsPerUnit, 0)) +
            INTEGER((ipdQuantitySubUnits MODULO ipdQuantitySubUnitsPerUnit) NE 0) + 
            INTEGER(ipdQuantityPartialSubUnit GT 0).
END FUNCTION.     

FUNCTION fCalculateQuantityPartialSubUnit RETURNS DECIMAL
    (ipdQuantityTotal AS DECIMAL, ipdQuantitySubUnits AS DECIMAL, ipdQuantitySubUnitCount AS DECIMAL):
     RETURN ipdQuantityTotal - ipdQuantitySubUnits * ipdQuantitySubUnitCount.
END FUNCTION.     

FUNCTION fCalculateQuantityTotal RETURNS DECIMAL
    (ipdQuantitySubUnits AS DECIMAL, ipdSubUnitCount AS DECIMAL, ipdQuantityPartialSubUnit AS DECIMAL):
     RETURN (ipdQuantitySubUnits * ipdSubUnitCount) + ipdQuantityPartialSubUnit.
END FUNCTION.

FUNCTION fCalculateTagCountInTTbrowse RETURNS INTEGER
    (ipcInventoryStatus AS CHARACTER):
    DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
    
    FOR EACH ttBrowseInventory
        WHERE (IF ipcInventoryStatus EQ "" THEN
                   TRUE
               ELSE
                   ttBrowseInventory.inventoryStatus EQ ipcInventoryStatus):
        iCount = iCount + 1.
    END.
    
    RETURN iCount.
END FUNCTION.    

FUNCTION fGetVendorTagFromLoadTag RETURNS CHARACTER
    (ipcCompany AS CHARACTER, iplItemType AS LOGICAL, ipcTag AS CHARACTER):
    DEFINE VARIABLE cVendorTag AS CHARACTER NO-UNDO.

    FIND FIRST loadtag NO-LOCK
         WHERE loadtag.company   EQ ipcCompany
           AND loadtag.item-type EQ iplItemType
           AND loadtag.tag-no    EQ ipcTag
        NO-ERROR.
    IF AVAILABLE loadtag THEN
        cVendorTag = loadtag.misc-char[1].

    RETURN cVendorTag.
END FUNCTION.
