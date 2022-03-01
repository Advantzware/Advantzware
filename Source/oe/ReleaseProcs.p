
/*------------------------------------------------------------------------
    File        : ReleaseProcs.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : DEVA$!
    Created     : Sun Feb 14 07:17:17 EST 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{sharpshooter/ttReleaseItem.i}
{sharpshooter/ttReleaseTag.i}

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */
PROCEDURE BuildReleaseItems:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiReleaseID AS INTEGER   NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttReleaseItem.
    
    RUN pBuildReleaseItems (
        INPUT  ipcCompany,
        INPUT  ipiReleaseID,
        INPUT-OUTPUT TABLE ttReleaseItem
        ).
END PROCEDURE.

PROCEDURE pBuildReleaseItems PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiReleaseID AS INTEGER   NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttReleaseItem.

    DEFINE VARIABLE dRoundUp AS DECIMAL NO-UNDO.
    
    DEFINE VARIABLE iQuantityScanned AS INTEGER NO-UNDO.
    DEFINE VARIABLE iUnitsScanned    AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bf-ssrelbol FOR ssrelbol.
    DEFINE BUFFER bf-itemfg   FOR itemfg.
    DEFINE BUFFER bf-oe-relh  FOR oe-relh.
    DEFINE BUFFER bf-oe-rell  FOR oe-rell.
    DEFINE BUFFER bf-oe-ordl  FOR oe-ordl.
    DEFINE BUFFER bf-po-ordl  FOR po-ordl.
    
    MAIN-BLOCK:
    DO ON ERROR UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
        ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

        FIND FIRST bf-oe-relh NO-LOCK
             WHERE bf-oe-relh.company  EQ ipcCompany
               AND bf-oe-relh.release# EQ ipiReleaseID
             NO-ERROR.
        IF NOT AVAILABLE bf-oe-relh THEN
            RETURN.
            
        FOR EACH bf-oe-rell NO-LOCK
            WHERE bf-oe-rell.company EQ bf-oe-relh.company
              AND bf-oe-rell.r-no    EQ bf-oe-relh.r-no:
            FIND FIRST ttReleaseItem
                 WHERE ttReleaseItem.company    EQ bf-oe-rell.company
                   AND ttReleaseItem.releaseID  EQ bf-oe-relh.release#
                   AND ttReleaseItem.itemID     EQ bf-oe-rell.i-no
                   AND ttReleaseItem.orderID    EQ bf-oe-rell.ord-no
                   AND ttReleaseItem.customerPO EQ bf-oe-rell.po-no
                 NO-ERROR.
            IF NOT AVAILABLE ttReleaseItem THEN DO:
                CREATE ttReleaseItem.
                ASSIGN
                    ttReleaseItem.company    = bf-oe-relh.company
                    ttReleaseItem.releaseID  = bf-oe-relh.release#
                    ttReleaseItem.orderID    = bf-oe-rell.ord-no
                    ttReleaseItem.itemID     = bf-oe-rell.i-no
                    ttReleaseItem.jobID      = bf-oe-rell.job-no
                    ttReleaseItem.jobID2     = bf-oe-rell.job-no2
                    ttReleaseItem.customerPO = bf-oe-rell.po-no
                    ttReleaseItem.orderID    = bf-oe-rell.ord-no
                    .

                FIND FIRST bf-itemfg NO-LOCK
                     WHERE bf-itemfg.company EQ bf-oe-rell.company
                       AND bf-itemfg.i-no    EQ bf-oe-rell.i-no
                     NO-ERROR.
                IF AVAILABLE bf-itemfg THEN
                    ASSIGN
                        ttReleaseItem.quantityOnHand = bf-itemfg.q-onh
                        ttReleaseItem.itemName       = bf-itemfg.i-name                    
                        .
            END.

            FIND FIRST bf-oe-ordl NO-LOCK
                 WHERE bf-oe-ordl.company EQ bf-oe-rell.company
                   AND bf-oe-ordl.ord-no  EQ bf-oe-rell.ord-no
                   AND bf-oe-ordl.line    EQ bf-oe-rell.line
                 NO-ERROR.
            IF AVAILABLE bf-oe-ordl THEN DO:
                dRoundup = bf-oe-rell.qty / (bf-oe-ordl.cas-cnt * bf-oe-ordl.cases-unit).
                { sys/inc/roundup.i dRoundup}
                ASSIGN
                    ttReleaseItem.quantityOfUnitsRelease = ttReleaseItem.quantityOfUnitsRelease + dRoundup
                    ttReleaseItem.underRunPercent        = bf-oe-ordl.under-pct 
                    ttReleaseItem.overRunPercent         = bf-oe-ordl.over-pct
                    .
            END.

            ttReleaseItem.quantityRelease = ttReleaseItem.quantityRelease + bf-oe-rell.qty.      
            
            IF ttReleaseItem.jobID NE "" THEN
                RUN fg/GetProductionQty.p (
                    INPUT  ttReleaseItem.company,
                    INPUT  ttReleaseItem.jobID,
                    INPUT  ttReleaseItem.jobID2,
                    INPUT  ttReleaseItem.itemID,
                    INPUT  NO, /* User fg-act */
                    OUTPUT ttReleaseItem.quantityReceivedJob
                    ).
            
            IF ttReleaseItem.customerPO NE "" THEN DO:
                FIND FIRST bf-po-ordl NO-LOCK
                     WHERE bf-po-ordl.company EQ bf-oe-rell.company
                       AND bf-po-ordl.po-no   EQ INTEGER(ttReleaseItem.customerPO)
                       AND bf-po-ordl.i-no    EQ bf-oe-rell.i-no
                     NO-ERROR.
                IF AVAILABLE bf-po-ordl THEN
                    ttReleaseItem.quantityReceivedPO = bf-po-ordl.t-rec-qty.
            END.
            
            ASSIGN
                iQuantityScanned = 0
                iUnitsScanned    = 0
                .
                                                                    
            FOR EACH bf-ssrelbol NO-LOCK
                WHERE bf-ssrelbol.company  EQ bf-oe-relh.company
                  AND bf-ssrelbol.release# EQ bf-oe-relh.release#
                  AND bf-ssrelbol.i-no     EQ bf-oe-rell.i-no
                  AND bf-ssrelbol.ord-no   EQ bf-oe-rell.ord-no
                  AND bf-ssrelbol.po-no    EQ bf-oe-rell.po-no:
                ASSIGN
                    iQuantityScanned = iQuantityScanned + bf-ssrelbol.qty
                    iUnitsScanned    = iUnitsScanned + 1
                    .
            END.

            ASSIGN
                ttReleaseItem.quantityOfUnitsScanned = iUnitsScanned
                ttReleaseItem.quantityScanned        = iQuantityScanned
                .
        END.                        
    END.
END PROCEDURE.

PROCEDURE BuildReleaseTags:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiReleaseID AS INTEGER   NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttReleaseTag.

    RUN pBuildReleaseTags (
        INPUT  ipcCompany,
        INPUT  ipiReleaseID,
        INPUT-OUTPUT TABLE ttReleaseTag
        ).
END PROCEDURE.

PROCEDURE pBuildReleaseTags PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiReleaseID AS INTEGER   NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttReleaseTag.
    
    DEFINE VARIABLE iSequence AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bf-ssrelbol FOR ssrelbol.
  
    MAIN-BLOCK:
    DO ON ERROR UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
        ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
        FOR EACH bf-ssrelbol NO-LOCK
            WHERE bf-ssrelbol.company  EQ ipcCompany
              AND bf-ssrelbol.release# EQ ipiReleaseID
            BY bf-ssrelbol.seq:
            CREATE ttReleaseTag.
            ASSIGN
                iSequence                             = iSequence + 1
                ttReleaseTag.company                  = bf-ssrelbol.company
                ttReleaseTag.releaseID                = bf-ssrelbol.release#
                ttReleaseTag.tag                      = bf-ssrelbol.tag
                ttReleaseTag.itemID                   = bf-ssrelbol.i-no
                ttReleaseTag.itemName                 = bf-ssrelbol.i-name
                ttReleaseTag.orderID                  = bf-ssrelbol.ord-no
                ttReleaseTag.jobID                    = bf-ssrelbol.job-no
                ttReleaseTag.jobID2                   = bf-ssrelbol.job-no2
                ttReleaseTag.location                 = bf-ssrelbol.loc
                ttReleaseTag.bin                      = bf-ssrelbol.loc-bin
                ttReleaseTag.customerID               = bf-ssrelbol.cust-no
                ttReleaseTag.quantityOfSubUnits       = bf-ssrelbol.cases
                ttReleaseTag.quantityInSubUnit        = bf-ssrelbol.qty-case
                ttReleaseTag.quantityOfSubUnitsInUnit = bf-ssrelbol.cases-unit
                ttReleaseTag.quantityPartial          = bf-ssrelbol.partial
                ttReleaseTag.quantity                 = bf-ssrelbol.qty
                ttReleaseTag.quantityTotal            = bf-ssrelbol.t-qty
                ttReleaseTag.lineID                   = bf-ssrelbol.line
                ttReleaseTag.sequenceID               = iSequence
                ttReleaseTag.custPoNo                 = bf-ssrelbol.po-no
                ttReleaseTag.trailerID                = bf-ssrelbol.trailer#
                ttReleaseTag.sourceRowID              = ROWID(bf-ssrelbol)
                .
        END.
    END.
END PROCEDURE.

PROCEDURE CreateReleaseTag:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany           AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiReleaseID         AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTag               AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcReleaseTrailerID  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTagTrailerID      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplValidateQtyExceed AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER iplSelectReleaseQty  AS LOGICAL   NO-UNDO.    
    DEFINE OUTPUT PARAMETER oplError             AS LOGICAL   NO-UNDO.   
    DEFINE OUTPUT PARAMETER opcMessage           AS CHARACTER NO-UNDO. 

    RUN pCreateReleaseTag (
        INPUT  ipcCompany,
        INPUT  ipiReleaseID,
        INPUT  ipcTag,
        INPUT  ipcReleaseTrailerID,
        INPUT  ipcTagTrailerID,
        INPUT  iplValidateQtyExceed,
        INPUT  iplSelectReleaseQty,
        OUTPUT oplError,   
        OUTPUT opcMessage
        ).
END PROCEDURE.


PROCEDURE CreateBOLFromRelease:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany                    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiReleaseID                  AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER iplCreateBOLLinesFromSSRelBOL AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcUserName                   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess                    AS LOGICAL   NO-UNDO. 
    DEFINE OUTPUT PARAMETER opcMessage                    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiBOLID                      AS INTEGER   NO-UNDO.
    
    RUN pCreateBOLFromRelease (
        INPUT  ipcCompany,
        INPUT  ipiReleaseID,
        INPUT  iplCreateBOLLinesFromSSRelBOL,
        INPUT  ipcUserName,
        OUTPUT oplSuccess,
        OUTPUT opcMessage,
        OUTPUT opiBOLID
        ).

END PROCEDURE.

PROCEDURE pCreateBOLFromRelease PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany                    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiReleaseID                  AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER iplCreateBOLLinesFromSSRelBOL AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcUserName                   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess                    AS LOGICAL   NO-UNDO. 
    DEFINE OUTPUT PARAMETER opcMessage                    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiBOLID                      AS INTEGER   NO-UNDO.
    
    DEFINE VARIABLE hdOrderProcs AS HANDLE NO-UNDO.
    
    RUN oe/OrderProcs.p PERSISTENT SET hdOrderProcs.
    
    RUN OrderProcsPostReleases IN hdOrderProcs (
        INPUT  ipcCompany,
        INPUT  ipiReleaseID,
        INPUT  iplCreateBOLLinesFromSSRelBOL,
        INPUT  ipcUserName,
        OUTPUT oplSuccess,
        OUTPUT opcMessage,
        OUTPUT opiBOLID
        ).

    DELETE PROCEDURE hdOrderProcs.
END PROCEDURE.

PROCEDURE pCreateReleaseTag PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Creates a ssrelbol record that is currently being used as table to
          store tag scans on a release
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany           AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiReleaseID         AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTag               AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcReleaseTrailerID  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTagTrailerID      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplValidateQtyExceed AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER iplSelectReleaseQty  AS LOGICAL   NO-UNDO.      /* TRUE - Selects quantity upto release quantity, FALSE - Selects entire pallet quantity */
    DEFINE OUTPUT PARAMETER oplError             AS LOGICAL   NO-UNDO.   
    DEFINE OUTPUT PARAMETER opcMessage           AS CHARACTER NO-UNDO. 
    
    DEFINE VARIABLE iNextSequenceID       AS INTEGER NO-UNDO.
    DEFINE VARIABLE dScannedQuantity      AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dTotalScannedQuantity AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dTotalReleaseQuantity AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQuantityToScan       AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dScannedQuantityCheck AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dReleaseQuantityCheck AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dTagQuantityScanned   AS DECIMAL NO-UNDO.
    
    DEFINE VARIABLE iQuantityOfSubUnits AS INTEGER NO-UNDO.
    DEFINE VARIABLE iPartial            AS INTEGER NO-UNDO.
        
    DEFINE BUFFER bf-ssrelbol FOR ssrelbol.
    DEFINE BUFFER bf-oe-relh  FOR oe-relh.
    DEFINE BUFFER bf-oe-rell  FOR oe-rell.
    DEFINE BUFFER bf-oe-ord   FOR oe-ord.
    DEFINE BUFFER bf-oe-ordl  FOR oe-ordl.
    DEFINE BUFFER bf-loadtag  FOR loadtag.
    DEFINE BUFFER bf-fg-bin   FOR fg-bin.

    MAIN-BLOCK:
    DO ON ERROR UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
        ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:    
        FIND FIRST bf-oe-relh NO-LOCK
             WHERE bf-oe-relh.company  EQ ipcCompany
               AND bf-oe-relh.release# EQ ipiReleaseID
             NO-ERROR.
        IF NOT AVAILABLE bf-oe-relh THEN DO:
            ASSIGN
                oplError   = TRUE
                opcMessage = "Invalid Release # '" + STRING(ipiReleaseID) + "'"
                .
            
            RETURN.
        END.
    
        IF NOT bf-oe-relh.printed THEN DO:
            ASSIGN
                oplError   = TRUE
                opcMessage = "Release # '" + STRING(ipiReleaseID) + "' is not printed"
                .
            
            RETURN.
        END.
        
        IF bf-oe-relh.posted THEN DO:
            ASSIGN
                oplError   = TRUE
                opcMessage = "Release # '" + STRING(ipiReleaseID) + "' is already posted"
                .
            
            RETURN.
        END.
            
        FIND FIRST bf-loadtag NO-LOCK
             WHERE bf-loadtag.company   EQ bf-oe-relh.company
               AND bf-loadtag.item-type EQ NO
               AND bf-loadtag.tag-no    EQ ipcTag
             NO-ERROR.
        IF NOT AVAILABLE bf-loadtag THEN DO:
            ASSIGN
                oplError   = TRUE
                opcMessage = "Invalid Tag # '" + ipcTag + "'"
                .
                    
            RETURN.
        END.
       
        FIND FIRST bf-oe-rell NO-LOCK
             WHERE bf-oe-rell.company EQ bf-oe-relh.company
               AND bf-oe-rell.r-no    EQ bf-oe-relh.r-no
               AND bf-oe-rell.i-no    EQ bf-loadtag.i-no
             NO-ERROR.
        IF NOT AVAILABLE bf-oe-rell THEN DO:
            ASSIGN
                oplError   = TRUE
                opcMessage = "Release has no item matching Tag # '" + ipcTag + "' item '" + bf-loadtag.i-no + "'"
                .
                    
            RETURN.        
        END.

        FIND FIRST bf-fg-bin NO-LOCK
             WHERE bf-fg-bin.company EQ bf-oe-relh.company
               AND bf-fg-bin.tag     EQ bf-loadtag.tag-no
               AND bf-fg-bin.i-no    EQ bf-loadtag.i-no
               AND bf-fg-bin.qty     GT 0
             NO-ERROR.
        IF NOT AVAILABLE bf-fg-bin THEN DO:
            ASSIGN
                 oplError   = TRUE
                 opcMessage = "Tag # '" + ipcTag + "' has no inventory"
                 .
                     
            RETURN.        
        END.
                   
        FIND FIRST cust NO-LOCK
             WHERE cust.company EQ bf-oe-relh.company
               AND cust.cust-no EQ bf-oe-relh.cust-no
             NO-ERROR. 
        IF AVAIL cust AND AVAIL bf-fg-bin THEN DO:
            IF ((cust.tagStatus EQ "" AND bf-fg-bin.onHold) OR (cust.tagStatus EQ "H" AND NOT bf-fg-bin.onHold)) THEN DO:
                ASSIGN
                    oplError   = TRUE
                    opcMessage = "Bin Tag status did not match with Customer Tag status."
                    .
       
                RETURN.
            END.              
        END.
        /* Perform fg-bin on hold validation. Currently zMessage 53 is used to control the popup */

        IF ipcReleaseTrailerID NE "" THEN DO:
            IF NOT CAN-FIND(FIRST truck 
                            WHERE truck.company    EQ bf-oe-relh.company 
                              AND truck.truck-code EQ ipcReleaseTrailerID) THEN DO:
                ASSIGN
                    oplError   = TRUE
                    opcMessage = "Invalid Trailer#. '" + ipcReleaseTrailerID + "'"
                    .
   
                RETURN.            
            END.
        END.
        ELSE
            ipcReleaseTrailerID = bf-oe-relh.trailer.

        IF ipcTagTrailerID NE "" THEN DO:
            IF NOT CAN-FIND(FIRST truck 
                            WHERE truck.company    EQ bf-oe-relh.company 
                              AND truck.truck-code EQ ipcTagTrailerID) THEN DO:
                ASSIGN
                    oplError   = TRUE
                    opcMessage = "Invalid Trailer#. '" + ipcTagTrailerID + "'"
                    .
   
                RETURN.            
            END.
        END.
        ELSE
            ipcTagTrailerID = IF ipcReleaseTrailerID NE "" THEN 
                                  ipcReleaseTrailerID
                              ELSE
                                  bf-oe-relh.trailer.
        
        IF bf-oe-rell.ord-no NE 0 THEN DO:
            FIND FIRST bf-oe-ord NO-LOCK
                 WHERE bf-oe-ord.company EQ bf-oe-rell.company
                   AND bf-oe-ord.ord-no  EQ bf-oe-rell.ord-no
                   AND bf-oe-ord.opened  EQ TRUE
                 NO-ERROR.
            IF NOT AVAILABLE bf-oe-ord THEN DO:
    /*            ASSIGN                                                                             */
    /*                oplError   = TRUE                                                              */
    /*                opcMessage = "Order # '" + STRING(bf-oe-rell.ord-no) + "' is invalid or closed"*/
    /*                .                                                                              */
    /*                                                                                               */
    /*            RETURN.                                                                            */
            END.  
        END. 
                    
        FOR EACH bf-fg-bin NO-LOCK
            WHERE bf-fg-bin.company  EQ bf-oe-relh.company
              AND bf-fg-bin.tag      EQ bf-loadtag.tag-no
              AND bf-fg-bin.i-no     EQ bf-loadtag.i-no
              AND bf-fg-bin.job-no   EQ bf-loadtag.job-no
              AND bf-fg-bin.job-no2  EQ bf-loadtag.job-no2
              AND bf-fg-bin.qty      GT 0
              AND ((AVAILABLE bf-oe-ord AND bf-fg-bin.cust-no EQ bf-oe-ord.cust-no) OR
                  bf-fg-bin.cust-no EQ "")
            USE-INDEX tag
            BREAK BY bf-fg-bin.cust-no DESC
                  BY bf-fg-bin.qty:
    
            IF (bf-fg-bin.cust-no EQ bf-oe-relh.cust-no AND LAST-OF(bf-fg-bin.cust-no)) OR LAST(bf-fg-bin.cust-no) THEN 
                LEAVE.
        END.         

        IF AVAILABLE bf-fg-bin THEN
            dScannedQuantity = bf-fg-bin.qty.       
        ELSE
            dScannedQuantity = bf-loadtag.pallet-count.
        
        dTagQuantityScanned = 0.
        
        /* Verify if the tag is already scanned before for any release */        
        FIND FIRST bf-ssrelbol NO-LOCK
             WHERE bf-ssrelbol.company EQ bf-oe-relh.company
               AND bf-ssrelbol.tag#    EQ ipcTag
               AND bf-ssrelbol.i-no    EQ bf-loadtag.i-no
             NO-ERROR.
        IF AVAILABLE bf-ssrelbol THEN DO:
            /* Fetch the total scanned quantity for the tag for all releases */
            FOR EACH bf-ssrelbol NO-LOCK
                WHERE bf-ssrelbol.company EQ ipcCompany
                  AND bf-ssrelbol.tag#    EQ ipcTag
                  AND bf-ssrelbol.i-no    EQ bf-loadtag.i-no:
                dTagQuantityScanned = dTagQuantityScanned + bf-ssrelbol.qty.
            END.

            IF dTagQuantityScanned GE dScannedQuantity THEN DO:            
                ASSIGN
                     oplError   = TRUE
                     opcMessage = "Tag # '" + ipcTag + "' is already scanned for Release # '" + STRING(bf-oe-relh.release#) + "'"
                     .
    
                RETURN.
            END.
            
            /* Negate the partial tag quantity from the tag quantity */            
            dScannedQuantity = dScannedQuantity - dTagQuantityScanned.
        END.
                
        /* Code to find the release line that, tag quantity needs to be applied to */
        RUN pGetReleaseLineForTag (
            INPUT  bf-oe-relh.company,
            INPUT  bf-oe-relh.release#,
            INPUT  bf-oe-relh.r-no,
            INPUT  bf-loadtag.i-no,
            INPUT  dScannedQuantity,
            BUFFER bf-oe-rell
            ).
        
        IF NOT AVAILABLE bf-oe-rell THEN
            FIND FIRST bf-oe-rell NO-LOCK
                 WHERE bf-oe-rell.company EQ bf-oe-relh.company
                   AND bf-oe-rell.r-no    EQ bf-oe-relh.r-no
                   AND bf-oe-rell.i-no    EQ bf-loadtag.i-no
                 NO-ERROR.
        
        /* Fetch quantity scanned so far */
        RUN pGetScannedQuantity (
            INPUT  bf-oe-relh.company,
            INPUT  bf-oe-relh.release#,
            INPUT  bf-loadtag.i-no,
            INPUT  bf-oe-rell.ord-no,
            INPUT  bf-oe-rell.po-no,
            OUTPUT dTotalScannedQuantity
            ).        
        
        /* Fetch release quantity for the scanned item */                
        RUN pGetReleaseQuantity (
            INPUT  bf-oe-relh.company,
            INPUT  bf-oe-relh.release#,
            INPUT  bf-loadtag.i-no,
            INPUT  bf-oe-rell.ord-no,
            INPUT  bf-oe-rell.po-no,
            OUTPUT dTotalReleaseQuantity
            ).
                
        IF iplValidateQtyExceed AND (dTotalScannedQuantity + dScannedQuantity) GT dTotalReleaseQuantity THEN DO:
            ASSIGN
                 oplError   = TRUE
                 opcMessage = "Tag quantity exceeds Scheduled Release quantity"
                 .
            
            system.SharedConfig:Instance:SetValue("ReleaseProcs_QuantityExceededPrompt", "TRUE").
            
            RETURN.             
        END.

        IF iplSelectReleaseQty THEN
            dQuantityToScan = dTotalReleaseQuantity - dTotalScannedQuantity.
        ELSE
            dQuantityToScan = dScannedQuantity.
        
        dQuantityToScan = MINIMUM (dQuantityToScan, dScannedQuantity).

        IF iplSelectReleaseQty AND dQuantityToScan LE 0 THEN DO:
            ASSIGN
                 oplError   = TRUE
                 opcMessage = "Scanned quantity exceeded the scheduled release quantity '" + STRING(dTotalReleaseQuantity) + "'. Please scan again choose YES to import pallet quantity."
                 .

            RETURN.
        END.
        
        /* Fetching last record's seq using FOR LAST does not seem to work, hence using FOR EACH with LEAVE statement */
        /* Source https://stackoverflow.com/questions/14234893/for-last-query-giving-wrong-result */
        FOR EACH bf-ssrelbol NO-LOCK
            WHERE bf-ssrelbol.company  EQ bf-oe-relh.company
              AND bf-ssrelbol.release# EQ bf-oe-relh.release#
            BY bf-ssrelbol.seq DESCENDING:
            LEAVE.
        END.
        
        IF AVAILABLE bf-ssrelbol THEN
            iNextSequenceID = bf-ssrelbol.seq.

        CREATE bf-ssrelbol.
        ASSIGN
            bf-ssrelbol.seq        = iNextSequenceID + 1
            bf-ssrelbol.company    = bf-oe-relh.company
            bf-ssrelbol.release#   = bf-oe-relh.release#
            bf-ssrelbol.tag#       = bf-loadtag.tag-no
            bf-ssrelbol.trailer#   = ipcTagTrailerID
            bf-ssrelbol.i-no       = bf-loadtag.i-no
            bf-ssrelbol.i-name     = bf-loadtag.i-name          
            bf-ssrelbol.ord-no     = bf-oe-rell.ord-no 
            bf-ssrelbol.job-no     = bf-loadtag.job-no
            bf-ssrelbol.job-no2    = bf-loadtag.job-no2
            bf-ssrelbol.loc        = bf-loadtag.loc
            bf-ssrelbol.loc-bin    = bf-loadtag.loc-bin 
            bf-ssrelbol.qty        = dQuantityToScan
            bf-ssrelbol.cases      = bf-loadtag.case-bundle
            bf-ssrelbol.qty-case   = bf-loadtag.qty-case
            bf-ssrelbol.cases-unit = bf-loadtag.case-bundle
            bf-ssrelbol.partial    = bf-loadtag.partial
            bf-ssrelbol.line       = 0
            bf-ssrelbol.po-no      = bf-oe-rell.po-no
            .
            
        IF AVAILABLE bf-fg-bin THEN
            ASSIGN
                bf-ssrelbol.loc        = bf-fg-bin.loc
                bf-ssrelbol.loc-bin    = bf-fg-bin.loc-bin
                bf-ssrelbol.cust-no    = bf-fg-bin.cust-no
                bf-ssrelbol.cases      = TRUNCATE((bf-ssrelbol.qty - bf-fg-bin.partial-count) / bf-fg-bin.case-count,0)
                bf-ssrelbol.qty-case   = bf-fg-bin.case-count
                bf-ssrelbol.cases-unit = bf-fg-bin.cases-unit
                bf-ssrelbol.partial    = bf-fg-bin.partial-count
                .

        IF iplSelectReleaseQty THEN DO:
            ASSIGN
                iQuantityofSubUnits = TRUNCATE((bf-ssrelbol.qty - bf-ssrelbol.partial) / bf-ssrelbol.qty-case, 0)
                iPartial            = bf-ssrelbol.qty - (iQuantityofSubUnits * bf-ssrelbol.qty-case)
                .

            ASSIGN
                bf-ssrelbol.cases   = iQuantityOfSubUnits
                bf-ssrelbol.partial = iPartial
                .
        END.
                                    
        FIND FIRST bf-oe-ordl NO-LOCK
             WHERE bf-oe-ordl.company EQ bf-oe-rell.company
               AND bf-oe-ordl.ord-no  EQ bf-oe-rell.ord-no
               AND bf-oe-ordl.i-no    EQ bf-oe-rell.i-no
             NO-ERROR.          
        IF AVAILABLE bf-oe-ordl THEN            
            bf-ssrelbol.line = bf-oe-ordl.line.
        
        IF ipcReleaseTrailerID NE "" AND bf-oe-relh.trailer EQ "" THEN DO:
            FIND CURRENT bf-oe-relh EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE bf-oe-relh THEN
                bf-oe-relh.trailer = ipcReleaseTrailerID.
        END.
    END.        
END PROCEDURE.

PROCEDURE DeleteReleaseTag:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipriSSRelBol AS ROWID NO-UNDO.
    
    RUN pDeleteReleaseTag (
        INPUT ipriSSRelBol
        ).
END PROCEDURE.

PROCEDURE pDeleteReleaseTag PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipriSSRelBol AS ROWID NO-UNDO.
    
    DEFINE BUFFER bf-ssrelbol FOR ssrelbol.
    
    MAIN-BLOCK:
    DO ON ERROR UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
        ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:     
        FIND FIRST bf-ssrelbol EXCLUSIVE-LOCK
             WHERE ROWID(bf-ssrelbol) EQ ipriSSRelBol
             NO-ERROR.
        IF AVAILABLE bf-ssrelbol THEN
            DELETE bf-ssrelbol.
    END.
END PROCEDURE.

PROCEDURE pGetReleaseLineForTag PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiReleaseID AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiRNo       AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItemID    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdQuantity  AS DECIMAL   NO-UNDO.
    DEFINE PARAMETER BUFFER opbf-oe-rell FOR oe-rell.
        
    RUN pBuildReleaseItems (
        INPUT  ipcCompany,
        INPUT  ipiReleaseID,
        INPUT-OUTPUT TABLE ttReleaseItem
        ).

    FIND FIRST ttReleaseItem
         WHERE ttReleaseItem.itemID EQ ipcItemID 
           AND ttReleaseItem.quantityScanned + ipdQuantity EQ ttReleaseItem.quantityRelease
         NO-ERROR.

    IF NOT AVAILABLE ttReleaseItem THEN         
        FIND FIRST ttReleaseItem
             WHERE ttReleaseItem.itemID EQ ipcItemID 
               AND ttReleaseItem.quantityScanned + ipdQuantity LE ttReleaseItem.quantityRelease + (ttReleaseItem.quantityRelease * (ttReleaseItem.overRunPercent / 100))
             NO-ERROR.

    IF NOT AVAILABLE ttReleaseItem THEN         
        FIND FIRST ttReleaseItem
             WHERE ttReleaseItem.itemID EQ ipcItemID 
               AND ttReleaseItem.quantityScanned LT ttReleaseItem.quantityRelease
             NO-ERROR.

    IF NOT AVAILABLE ttReleaseItem THEN         
        FIND FIRST ttReleaseItem
             WHERE ttReleaseItem.itemID          EQ ipcItemID
               AND ttReleaseItem.quantityScanned EQ 0
             NO-ERROR.
             
    IF NOT AVAILABLE ttReleaseItem THEN         
        FIND FIRST ttReleaseItem 
             WHERE ttReleaseItem.itemID EQ ipcItemID
             NO-ERROR.

    IF AVAILABLE ttReleaseItem THEN
        FIND FIRST opbf-oe-rell NO-LOCK
             WHERE opbf-oe-rell.company  EQ ttReleaseItem.company
               AND opbf-oe-rell.r-no     EQ ipiRNo
               AND opbf-oe-rell.i-no     EQ ttReleaseItem.itemID
               AND opbf-oe-rell.ord-no   EQ ttReleaseItem.orderID
               AND opbf-oe-rell.po-no    EQ ttReleaseItem.customerPO
             NO-ERROR.
    
    EMPTY TEMP-TABLE ttReleaseItem.
END PROCEDURE.

PROCEDURE Release_HasBOLScanned:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiReleaseID     AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplHasBOLScanned AS LOGICAL   NO-UNDO.
    
    RUN pHasBOLScanned (
        INPUT  ipcCompany,
        INPUT  ipiReleaseID,
        OUTPUT oplHasBOLScanned
        ).
END PROCEDURE.

PROCEDURE pHasBOLScanned PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiReleaseID     AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplHasBOLScanned AS LOGICAL   NO-UNDO.

    oplHasBOLScanned = CAN-FIND(FIRST ssrelbol NO-LOCK
                                WHERE ssrelbol.company  EQ ipcCompany
                                  AND ssrelbol.release# EQ ipiReleaseID).
END PROCEDURE.

PROCEDURE Release_GetScannedQuantity:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany         AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiReleaseID       AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItemID          AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiOrderID         AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCustomerPO      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdScannedQuantity AS DECIMAL   NO-UNDO.
    
    RUN pGetScannedQuantity (
        INPUT  ipcCompany,
        INPUT  ipiReleaseID,
        INPUT  ipcItemID,
        INPUT  ipiOrderID,
        INPUT  ipcCustomerPO,
        OUTPUT opdScannedQuantity
        ).
END PROCEDURE.

PROCEDURE pGetScannedQuantity PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany         AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiReleaseID       AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItemID          AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiOrderID         AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCustomerPO      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdScannedQuantity AS DECIMAL   NO-UNDO.
    
    DEFINE BUFFER bf-ssrelbol FOR ssrelbol.
    
    FOR EACH bf-ssrelbol NO-LOCK
        WHERE bf-ssrelbol.company  EQ ipcCompany
          AND bf-ssrelbol.release# EQ ipiReleaseID
          AND bf-ssrelbol.i-no     EQ ipcItemID
          AND (bf-ssrelbol.ord-no  EQ ipiOrderID OR ipiOrderID EQ 0)
          AND (bf-ssrelbol.po-no   EQ ipcCustomerPO OR ipcCustomerPO EQ ""):
        opdScannedQuantity = opdScannedQuantity + bf-ssrelbol.qty.
    END.    
END PROCEDURE.

PROCEDURE Release_GetReleaseQuantity:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany         AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiReleaseID       AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItemID          AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiOrderID         AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCustomerPO      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdReleaseQuantity AS DECIMAL   NO-UNDO.
    
    RUN pGetReleaseQuantity (
        INPUT  ipcCompany,
        INPUT  ipiReleaseID,
        INPUT  ipcItemID,
        INPUT  ipiOrderID,
        INPUT  ipcCustomerPO,        
        OUTPUT opdReleaseQuantity
        ).
END PROCEDURE.

PROCEDURE pGetReleaseQuantity PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany         AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiReleaseID       AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItemID          AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiOrderID         AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCustomerPO      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdReleaseQuantity AS DECIMAL   NO-UNDO.
    
    DEFINE BUFFER bf-oe-relh FOR oe-relh.
    DEFINE BUFFER bf-oe-rell FOR oe-rell.
    
    FIND FIRST bf-oe-relh NO-LOCK
         WHERE bf-oe-relh.company  EQ ipcCompany
           AND bf-oe-relh.release# EQ ipiReleaseID
         NO-ERROR.
    IF NOT AVAILABLE bf-oe-relh THEN
        RETURN.
    
    FOR EACH bf-oe-rell NO-LOCK
        WHERE bf-oe-rell.company EQ bf-oe-relh.company
          AND bf-oe-rell.r-no    EQ bf-oe-relh.r-no
          AND bf-oe-rell.i-no    EQ ipcItemID
          AND (bf-oe-rell.ord-no EQ ipiOrderID OR ipiOrderID EQ 0)
          AND (bf-oe-rell.po-no  EQ ipcCustomerPO OR ipcCustomerPO EQ ""):
        opdReleaseQuantity = opdReleaseQuantity + bf-oe-rell.qty.
    END.
END PROCEDURE.

