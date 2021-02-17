
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
    
    DEFINE BUFFER bf-ssrelbol FOR ssrelbol.
    DEFINE BUFFER bf-itemfg   FOR itemfg.
    DEFINE BUFFER bf-oe-relh  FOR oe-relh.
    DEFINE BUFFER bf-oe-rell  FOR oe-rell.
    DEFINE BUFFER bf-oe-ordl  FOR oe-ordl.

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
                 WHERE ttReleaseItem.company   EQ bf-oe-rell.company
                   AND ttReleaseItem.releaseID EQ bf-oe-relh.release#
                   AND ttReleaseItem.itemID    EQ bf-oe-rell.i-no
                 NO-ERROR.
            IF NOT AVAILABLE ttReleaseItem THEN DO:
                CREATE ttReleaseItem.
                ASSIGN
                    ttReleaseItem.company   = bf-oe-relh.company
                    ttReleaseItem.releaseID = bf-oe-relh.release#
                    ttReleaseItem.orderID   = bf-oe-rell.ord-no
                    ttReleaseItem.itemID    = bf-oe-rell.i-no
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
                ttReleaseItem.quantityOfUnitsRelease = ttReleaseItem.quantityOfUnitsRelease + dRoundup .
            END.

            ttReleaseItem.quantityRelease = ttReleaseItem.quantityRelease + bf-oe-rell.qty.      

            FOR EACH bf-ssrelbol NO-LOCK
                WHERE bf-ssrelbol.company  EQ bf-oe-relh.company
                  AND bf-ssrelbol.release# EQ bf-oe-relh.release#
                  AND bf-ssrelbol.i-no     EQ bf-oe-rell.i-no:
                ASSIGN
                    ttReleaseItem.quantityOfUnitsScanned = ttReleaseItem.quantityOfUnitsScanned + 1
                    ttReleaseItem.quantityScanned        = ttReleaseItem.quantityScanned + bf-ssrelbol.qty
                    .
            END.
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
    
    DEFINE BUFFER bf-ssrelbol FOR ssrelbol.
  
    MAIN-BLOCK:
    DO ON ERROR UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
        ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
        FOR EACH bf-ssrelbol NO-LOCK
            WHERE bf-ssrelbol.company  EQ ipcCompany
              AND bf-ssrelbol.release# EQ ipiReleaseID:
            CREATE ttReleaseTag.
            ASSIGN
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
                ttReleaseTag.sequenceID               = bf-ssrelbol.seq
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
    DEFINE INPUT  PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiReleaseID AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTag       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTrailerID AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError     AS LOGICAL   NO-UNDO.   
    DEFINE OUTPUT PARAMETER opcMessage   AS CHARACTER NO-UNDO. 

    RUN pCreateReleaseTag (
        INPUT  ipcCompany,
        INPUT  ipiReleaseID,
        INPUT  ipcTag,
        INPUT  ipcTrailerID,
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
    DEFINE INPUT  PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiReleaseID AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTag       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTrailerID AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError     AS LOGICAL   NO-UNDO.   
    DEFINE OUTPUT PARAMETER opcMessage   AS CHARACTER NO-UNDO. 
    
    DEFINE VARIABLE iNextSequenceID AS INTEGER NO-UNDO.
    
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
       
        FIND FIRST bf-oe-rell
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
        
        IF bf-oe-rell.ord-no NE 0 THEN DO:
            FIND FIRST bf-oe-ord
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
            
            FIND FIRST bf-oe-ordl
                 WHERE bf-oe-ordl.company EQ bf-oe-rell.company
                   AND bf-oe-ordl.ord-no  EQ bf-oe-rell.ord-no
                   AND bf-oe-ordl.i-no    EQ bf-oe-rell.i-no
                 NO-ERROR.          
        END.
        
        FIND FIRST bf-ssrelbol NO-LOCK
             WHERE bf-ssrelbol.company  EQ bf-oe-relh.company
               AND bf-ssrelbol.release# EQ bf-oe-relh.release#
               AND bf-ssrelbol.tag#     EQ ipcTag
             NO-ERROR.
        IF AVAILABLE bf-ssrelbol THEN DO:
            ASSIGN
                 oplError   = TRUE
                 opcMessage = "Tag # '" + ipcTag + "' is already scanned for Release # '" + STRING(bf-oe-relh.release#) + "'"
                 .
                     
            RETURN.        
        END.
    
        FIND FIRST bf-fg-bin
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
        /* Perform fg-bin on hold validation. Currently zMessage 53 is used to control the popup */
        
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
        
        FOR EACH bf-ssrelbol NO-LOCK
            WHERE bf-ssrelbol.company  EQ bf-oe-relh.company
               AND bf-ssrelbol.release# EQ bf-oe-relh.release#:
            iNextSequenceID = iNextSequenceID + 1.
        END.
        
        CREATE bf-ssrelbol.
        ASSIGN
            bf-ssrelbol.seq        = iNextSequenceID + 1
            bf-ssrelbol.company    = bf-oe-relh.company
            bf-ssrelbol.release#   = bf-oe-relh.release#
            bf-ssrelbol.tag#       = bf-loadtag.tag-no
            bf-ssrelbol.i-no       = bf-loadtag.i-no
            bf-ssrelbol.i-name     = bf-loadtag.i-name          
            bf-ssrelbol.ord-no     = bf-oe-rell.ord-no 
            bf-ssrelbol.job-no     = bf-loadtag.job-no
            bf-ssrelbol.job-no2    = bf-loadtag.job-no2
            bf-ssrelbol.loc        = bf-loadtag.loc
            bf-ssrelbol.loc-bin    = bf-loadtag.loc-bin 
            bf-ssrelbol.qty        = bf-loadtag.pallet-count
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
                bf-ssrelbol.qty        = bf-fg-bin.qty
                bf-ssrelbol.cases      = TRUNC((bf-fg-bin.qty - bf-fg-bin.partial-count) / bf-fg-bin.case-count,0)
                bf-ssrelbol.qty-case   = bf-fg-bin.case-count
                bf-ssrelbol.cases-unit = bf-fg-bin.cases-unit
                bf-ssrelbol.partial    = bf-fg-bin.partial-count
                .
    
        IF AVAILABLE bf-oe-ordl THEN            
            bf-ssrelbol.line = bf-oe-ordl.line.
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

