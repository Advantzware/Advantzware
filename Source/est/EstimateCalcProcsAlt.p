
/*------------------------------------------------------------------------
    File        : EstimateCalcProcs.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : BV
    Created     : Thu Jan 24 16:45:11 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{est/dsEstimate.i}

DEFINE VARIABLE giID AS INT64.

/* ************************  Function Prototypes ********************** */
FUNCTION fGetNextID RETURNS INT64 PRIVATE
    (  ) FORWARD.

/* **********************  Internal Procedures  *********************** */

PROCEDURE CalculateEstimate:
    /*------------------------------------------------------------------------------
     Purpose: Primary Public Procedure for calculating the estimate
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstimateNo AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcErrorMessage AS CHARACTER NO-UNDO.

    RUN pBuild(ipcCompany, ipcEstimateNo, OUTPUT oplError, OUTPUT opcErrorMessage).

END PROCEDURE.

PROCEDURE ExportAll:
/*------------------------------------------------------------------------------
 Purpose:  Given a file path, generate a export of data
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipcFile AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcFileType AS CHARACTER NO-UNDO.

DEFINE VARIABLE hDSEstimate AS HANDLE.

hDSEstimate = DATASET dsEstimate:HANDLE.

RUN Output_TempTableToJSON (hDSEstimate, ipcFile).

END PROCEDURE.

PROCEDURE pAddError PRIVATE:

    /*------------------------------------------------------------------------------
     Purpose: Registers an error in the ttEstimateError table for display
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiEstimateID LIKE ttEstimate.estimateID.
    DEFINE INPUT PARAMETER ipiEstimateFormID LIKE ttEstimateForm.estimateFormID.
    DEFINE INPUT PARAMETER ipiEstimateBlankID LIKE ttEstimateBlank.estimateBlankID.
    DEFINE INPUT PARAMETER ipcError AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcErrorType AS CHARACTER NO-UNDO.

    CREATE ttEstimateError.
    ASSIGN 
        ttEstimateError.estimateID      = ipiEstimateID
        ttEstimateError.estimateFormID  = ipiEstimateFormID
        ttEstimateError.estimateBlankID = ipiEstimateBlankID
        ttEstimateError.errorType       = ipcErrorType
        ttEstimateError.errorMessage    = ipcError
        .
        
END PROCEDURE.

PROCEDURE pAddEstBlank PRIVATE:

    /*------------------------------------------------------------------------------
     Purpose: given an eb buffer, create the EstimateBlank
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstimateForm  FOR ttEstimateForm.
    DEFINE PARAMETER BUFFER ipbf-ttEstimateItem  FOR ttEstimateItem.
    DEFINE PARAMETER BUFFER opbf-ttEstimateBlank FOR ttEstimateBlank.
    
    CREATE opbf-ttEstimateBlank.
    ASSIGN 
        opbf-ttEstimateBlank.estimateFormID  = ipbf-ttEstimateForm.estimateFormID
        opbf-ttEstimateBlank.estimateBlankID = fGetNextID()
        opbf-ttEstimateBlank.estimateItemID  = ipbf-ttEstimateItem.estimateItemID
        .
    
END PROCEDURE.

PROCEDURE pAddEstBlankFromEb PRIVATE:

    /*------------------------------------------------------------------------------
     Purpose: given an eb buffer, create the EstimateBlank
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-eb              FOR eb.
    DEFINE PARAMETER BUFFER ipbf-ttEstimateForm  FOR ttEstimateForm.
    DEFINE PARAMETER BUFFER ipbf-ttEstimateItem  FOR ttEstimateItem.
    DEFINE PARAMETER BUFFER opbf-ttEstimateBlank FOR ttEstimateBlank.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcErrorMessage AS CHARACTER NO-UNDO.
    
    RUN pAddEstBlank(BUFFER ipbf-ttEstimateForm, BUFFER ipbf-ttEstimateItem, BUFFER opbf-ttEstimateBlank).
    
    ASSIGN 
        opbf-ttEstimateBlank.blankNo = ipbf-eb.blank-no
        opbf-ttEstimateBlank.formNo  = ipbf-eb.form-no
        .

END PROCEDURE.

PROCEDURE pAddEstForm PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: create the EstForm for an est header and form no
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstimate FOR ttEstimate.
    DEFINE INPUT PARAMETER ipiFormNo AS INTEGER NO-UNDO.
    DEFINE PARAMETER BUFFER opbf-ttEstimateForm FOR ttEstimateForm.
    
    CREATE opbf-ttEstimateForm.
    ASSIGN 
        opbf-ttEstimateForm.estimateID     = ipbf-ttEstimate.estimateID
        opbf-ttEstimateForm.estimateFormID = fGetNextID()
        opbf-ttEstimateForm.formNo         = ipiFormNo
        .
        
END PROCEDURE.

PROCEDURE pAddEstFormFromEf PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: given an ef buffer, create the EstimtateForm
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ef             FOR ef.
    DEFINE PARAMETER BUFFER ipbf-ttEstimate     FOR ttEstimate.
    DEFINE PARAMETER BUFFER opbf-ttEstimateForm FOR ttEstimateForm.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcErrorMessage AS CHARACTER NO-UNDO.
    
    RUN pAddEstForm(BUFFER ipbf-ttEstimate, ipbf-ef.form-no, BUFFER opbf-ttEstimateForm).
    
    ASSIGN 
        opbf-ttEstimateForm.dimWidthGross  = ipbf-ef.gsh-wid 
        opbf-ttEstimateForm.dimLengthGross = ipbf-ef.gsh-len
        opbf-ttEstimateForm.dimDepthGross  = ipbf-ef.gsh-dep
        opbf-ttEstimateForm.dimWidthNet    = ipbf-ef.nsh-wid 
        opbf-ttEstimateForm.dimLengthNet   = ipbf-ef.nsh-len
        opbf-ttEstimateForm.dimDepthNet    = ipbf-ef.nsh-dep 
        .

END PROCEDURE.

PROCEDURE pAddEstimate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: given a company and estimateNo buffer, create the Estimate
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstimateID AS CHARACTER NO-UNDO.
    DEFINE PARAMETER BUFFER opbf-ttEstimate FOR ttEstimate.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL.
    DEFINE OUTPUT PARAMETER opcErrorMessage AS CHARACTER.
    
    DEFINE BUFFER bf-est FOR est.
    
    FIND FIRST bf-est NO-LOCK
        WHERE bf-est.company EQ ipcCompany
        AND bf-est.est-no EQ ipcEstimateID
        NO-ERROR. 
    IF AVAILABLE bf-est THEN 
    DO:
        CREATE opbf-ttEstimate.
        ASSIGN 
            opbf-ttEstimate.estimateID = fGetNextID()
            opbf-ttEstimate.company    = bf-est.company
            opbf-ttEstimate.estimateNo = bf-est.est-no
            .
    END.
    ELSE
        ASSIGN
            oplError        = YES
            opcErrorMessage = "Invalid Estimate" + CHR(13) + "Estimate: " + ipcEstimateID + CHR(13) + "Company: " + ipcCompany
            .
     
END PROCEDURE.

PROCEDURE pAddEstItem PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Create an ttEstimateItem given eb and other key ids
     Notes: ce/print4p.i
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-eb             FOR eb.
    DEFINE PARAMETER BUFFER ipbf-ttEstimate     FOR ttEstimate.
    DEFINE PARAMETER BUFFER opbf-ttEstimateItem FOR ttEstimateItem.

    CREATE opbf-ttEstimateItem.
    ASSIGN 
        opbf-ttEstimateItem.estimateID     = ipbf-ttEstimate.estimateID
        opbf-ttEstimateItem.estimateItemID = fGetNextID()
        opbf-ttEstimateItem.isPurchased    = ipbf-eb.pur-man
        opbf-ttEstimateItem.partID         = ipbf-eb.part-no
        opbf-ttEstimateItem.partName       = ipbf-eb.part-dscr1
        opbf-ttEstimateItem.itemID         = ipbf-eb.stock-no
        .
END PROCEDURE.

PROCEDURE pAddEstMaterial PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Create estCostMaterial from an item buffer and returns the buffer
 Notes:
------------------------------------------------------------------------------*/
   
    
END PROCEDURE.

PROCEDURE pAddEstOperation PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Creates an estCostOperation based on est-op and form
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-est-op FOR est-op.
   
    
END PROCEDURE.

PROCEDURE pAddEstQuantity PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Given an estimate, quantity and count of releases, add a quantity record
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstimate FOR ttEstimate.
    DEFINE INPUT PARAMETER ipdQuantity AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipiReleaseCount AS INTEGER NO-UNDO.
    DEFINE PARAMETER BUFFER opbf-ttEstimateQuantity FOR ttEstimateQuantity.
    
    CREATE opbf-ttEstimateQuantity.
    ASSIGN 
        opbf-ttEstimateQuantity.estimateID         = ipbf-ttEstimate.estimateID
        opbf-ttEstimateQuantity.estimateQuantityID = fGetNextID()
        opbf-ttEstimateQuantity.quantityMaster     = ipdQuantity
        opbf-ttEstimateQuantity.quantityOfReleases = ipiReleaseCount
        opbf-ttEstimateQuantity.quantityUOM        = "EA"
        opbf-ttEstimateQuantity.isInteger          = YES
        .
        

END PROCEDURE.

PROCEDURE pBuild PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Builds the dataset for the estimate calculation
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstimateID AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcErrorMessage AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-ttEstimate FOR ttEstimate.
    RUN pClearData.
    RUN pAddEstimate(ipcCompany, ipcEstimateID, BUFFER bf-ttEstimate, OUTPUT oplError, OUTPUT opcErrorMessage).
    IF NOT oplError THEN 
        RUN pBuildQuantities(BUFFER bf-ttEstimate, OUTPUT oplError, OUTPUT opcErrorMessage).
    IF NOT oplError THEN 
        RUN pBuildStructure(BUFFER bf-ttEstimate, OUTPUT oplError, OUTPUT opcErrorMessage).
    IF NOT oplError THEN 
        RUN pBuildItemQuantities(BUFFER bf-ttEstimate, OUTPUT oplError, OUTPUT opcErrorMessage).
    //DATASET dsEstimate:FILL().
    
END PROCEDURE.

PROCEDURE pBuildBlankAndFormQuantities PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstimate FOR ttEstimate.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcErrorMessage AS CHARACTER NO-UNDO.
    
    FOR EACH ttEstimateQuantity
        WHERE ttEstimateQuantity.estimateID EQ ipbf-ttEstimate.estimateID,
        EACH ttEstimateForm
        WHERE ttEstimateForm.estimateID EQ ipbf-ttEstimate.estimateID,
        EACH ttEstimateBlank
        WHERE ttEstimateBlank.estimateFormID EQ ttEstimateForm.estimateFormID,
        FIRST ttEstimateItem
        BREAK BY ttEstimateBlank.estimateFormID:
        IF FIRST-OF (ttEstimateBlank.estimateFormID) THEN DO:
            CREATE ttEstimateFormQuantity.
            ASSIGN 
                ttEstimateFormQuantity.estimateFormID = ttEstimateBlank.estimate
        END.
        CREATE ttEstimateBlank    
        
    END.

END PROCEDURE.

PROCEDURE pBuildItemQuantities PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Given an estimateID, build the item quantities required
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstimate FOR ttEstimate.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcErrorMessage AS CHARACTER NO-UNDO.
    
    FOR EACH ttEstimateQuantity
        WHERE ttEstimateQuantity.estimateID EQ ipbf-ttEstimate.estimateID,
        EACH ttEstimateItem
        WHERE ttEstimateItem.estimateID EQ ttEstimateQuantity.estimateID:
        CREATE ttEstimateItemQuantity.
        ASSIGN 
            ttEstimateItemQuantity.estimateItemQuantityID = fGetNextID()
            ttEstimateItemQuantity.estimateItemID = ttEstimateItem.estimateItemID
            ttEstimateItemQuantity.estimateQuantityID = ttEstimateQuantity.estimateQuantityID
            ttEstimateItemQuantity.quantityRequested = IF ttEstimateItem.quantityRequested NE 0 THEN ttEstimateItem.quantityRequested 
                                                        ELSE ttEstimateItem.quantityPerSet * ttEstimateQuantity.quantityMaster
            ttEstimateItemQuantity.quantityYielded = = IF ttEstimateItem.quantityYielded NE 0 THEN ttEstimateItem.quantityYielded
                                                        ELSE ttEstimateItem.quantityPerSet * ttEstimateQuantity.quantityMaster
            ttEstimateQuantity.quantityTotal = ttEstimateItemQuantity.q
    END.

END PROCEDURE.

PROCEDURE pBuildStructure PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: given company and estimate, build the items
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstimate FOR ttEstimate.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcErrorMessage AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-eb              FOR eb.
    DEFINE BUFFER bf-ef              FOR ef.
    DEFINE BUFFER bf-ttEstimateItem  FOR ttEstimateItem.
    DEFINE BUFFER bf-ttEstimateForm  FOR ttEstimateForm.
    DEFINE BUFFER bf-ttEstimateBlank FOR ttEstimateBlank.
        
    /*Build Items, Forms and Blanks*/
    FOR EACH bf-eb NO-LOCK 
        WHERE bf-eb.company EQ ipbf-ttEstimate.company
        AND bf-eb.est-no EQ ipbf-ttEstimate.estimateNo
        BY bf-eb.form-no DESCENDING: 
        FIND FIRST bf-ttEstimateItem
            WHERE bf-ttEstimateItem.estimateID EQ ipbf-ttEstimate.estimateID
            AND bf-ttEstimateItem.partID EQ bf-eb.part-no
            NO-ERROR.
        IF NOT AVAILABLE bf-ttEstimateItem THEN 
            RUN pAddEstItem(BUFFER bf-eb, BUFFER ipbf-ttEstimate, BUFFER bf-ttEstimateItem).
        IF bf-eb.form-no EQ 0 THEN 
            RUN pAddEstForm(BUFFER ipbf-ttEstimate, 0, BUFFER bf-ttEstimateForm).
        FIND FIRST bf-ttEstimateForm
            WHERE bf-ttEstimateForm.estimateID EQ ipbf-ttEstimate.estimateID
            AND bf-ttEstimateForm.formNo EQ bf-eb.form-no
            NO-ERROR.
        IF NOT AVAILABLE bf-ttEstimateForm THEN 
        DO: 
            FIND FIRST bf-ef NO-LOCK 
                WHERE bf-ef.company EQ ipbf-ttEstimate.company
                AND bf-ef.est-no EQ ipbf-ttEstimate.estimateNo
                AND bf-ef.form-no EQ bf-eb.form-no
                NO-ERROR.
            RUN pAddEstFormFromEf(BUFFER bf-ef, BUFFER ipbf-ttEstimate, BUFFER bf-ttEstimateForm, 
                OUTPUT oplError, OUTPUT opcErrorMessage).
        END.
        RUN pAddEstBlankFromEb(BUFFER bf-eb, BUFFER bf-ttEstimateForm, BUFFER bf-ttEstimateItem, BUFFER bf-ttEstimateBlank, 
            OUTPUT oplError, OUTPUT opcErrorMessage).
    END. /*Build EstItems*/

END PROCEDURE.

PROCEDURE pBuildQuantities PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a ttEstimate buffer, add EstimateQuantityRecords
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstimate FOR ttEstimate.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcErrorMessage AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-est-qty            FOR est-qty.
    DEFINE BUFFER bf-ttEstimateQuantity FOR ttEstimateQuantity.
    
    DEFINE VARIABLE iQtyCount AS INTEGER NO-UNDO.
    
    FIND FIRST bf-est-qty NO-LOCK 
        WHERE bf-est-qty.company EQ ipbf-ttEstimate.company
        AND bf-est-qty.est-no  EQ ipbf-ttEstimate.estimateNo
        NO-ERROR.
    
    IF AVAILABLE bf-est-qty THEN 
    DO iQtyCount = 1 TO 20:
        IF bf-est-qty.qty[iQtyCount] NE 0 THEN 
            RUN pAddEstQuantity(BUFFER ipbf-ttEstimate, bf-est-qty.qty[iQtyCount], MAX(bf-est-qty.qty[iQtyCount + 20], 1) , BUFFER bf-ttEstimateQuantity).
    END.        

END PROCEDURE.

PROCEDURE pClearData PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Clears all local data
     Notes:
    ------------------------------------------------------------------------------*/

    EMPTY TEMP-TABLE ttEstimate.
    EMPTY TEMP-TABLE ttEstimateQuantity.
    EMPTY TEMP-TABLE ttEstimateItem.
    EMPTY TEMP-TABLE ttEstimateForm.
    EMPTY TEMP-TABLE ttEstimateBlank.
    EMPTY TEMP-TABLE ttEstimateError.
    EMPTY TEMP-TABLE ttEstimateOperation.
    EMPTY TEMP-TABLE ttEstimateMaterial.

END PROCEDURE.

/* ************************  Function Implementations ***************** */


FUNCTION fGetNextID RETURNS INT64 PRIVATE
    (  ):
    /*------------------------------------------------------------------------------
     Purpose: increment the ID for a unique id
     Notes:
    ------------------------------------------------------------------------------*/	
    
    giID = giID + 1.
    RETURN giID.

END FUNCTION.

