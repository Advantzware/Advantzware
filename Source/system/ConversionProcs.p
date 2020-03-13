
/*------------------------------------------------------------------------
    File        : ConversionProcs.p
    Purpose     : 

    Syntax      :

    Description : All UOM and Conversion Handling Procedures		

    Author(s)   : BV
    Created     : Thu Mar 12 22:26:33 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

PROCEDURE Conversion_ValueFromEAToUOMForItem:
    /*------------------------------------------------------------------------------
     Purpose: Given a RowID for an item (itemfg or oe-ordl) take an each value and convert
     to a new UOM
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriItem AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER ipdValueInEA AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcConvertToUOM AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdValueInUOM AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    RUN pConvertValueForItem(ipriItem, ipdValueInEA, ipcConvertToUOM, "FromEAToUOM", OUTPUT opdValueInUOM, OUTPUT oplError, OUTPUT opcMessage).

END PROCEDURE.

PROCEDURE Conversion_ValueFromUOMToEAForItem:
    /*------------------------------------------------------------------------------
     Purpose: Given a RowID for an item (itemfg or oe-ordl) take a UOM value and convert
     to EA
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriItem AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER ipdValueInUOM AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcConvertFromUOM AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdValueInEA AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    RUN pConvertValueForItem(ipriItem, ipdValueInUOM, ipcConvertFromUOM, "ToEAFromUOM", OUTPUT opdValueInEA, OUTPUT oplError, OUTPUT opcMessage).

END PROCEDURE.

PROCEDURE Conversion_ValueFromUOMToUOMForItem:
    /*------------------------------------------------------------------------------
     Purpose: Given a RowID for an item (itemfg or oe-ordl) take a UOM value and convert
     to another UOM
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriItem AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER ipdValueInFromUOM AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcConvertFromUOM AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcConvertToUOM AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdValueInToUOM AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE VARIABLE dValueInEA AS DECIMAL NO-UNDO.

    RUN pConvertValueForItem(ipriItem, ipdValueInFromUOM, ipcConvertFromUOM, "ToEAFromUOM", OUTPUT dValueInEA, OUTPUT oplError, OUTPUT opcMessage).
    IF NOT oplError AND dValueInEA NE 0 THEN 
        RUN pConvertValueForItem(ipriItem, dValueInEA, ipcConvertToUOM, "FromEAToUOM", OUTPUT opdValueInToUOM, OUTPUT oplError, OUTPUT opcMessage).

END PROCEDURE.

PROCEDURE pCheckPurposeForItemUOM PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given an ItemUOM Buffer and purpose, confirm UOM is valid for that
     purpose
     Notes:  If purpose is "", used for any purpose
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-itemUOM FOR itemUoM.
    DEFINE INPUT PARAMETER ipcPurpose AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopcMessage AS CHARACTER NO-UNDO.

    CASE ipcPurpose:
        WHEN "Stock" THEN 
            oplError = NOT ipbf-itemUOM.isStock.
        WHEN "Purchase" THEN 
            oplError = NOT ipbf-itemUOM.canPurchase.
        WHEN "Sell" THEN 
            oplError = NOT ipbf-itemUOM.canSell.            
    END CASE. 
    IF oplError THEN 
        iopcMessage = iopcMessage + " not valid for " + ipcPurpose.             

END PROCEDURE.

PROCEDURE pConvertValueForItem PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriItem AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER ipdValue AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcConvertUOM AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFromOrTo AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdValueConverted AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-itemfg FOR itemfg.
    DEFINE BUFFER bf-item   FOR ITEM.

    DEFINE VARIABLE dConversionFactor AS DECIMAL NO-UNDO.

    RUN pGetBuffers(ipriItem, BUFFER bf-itemfg, BUFFER bf-item).

    IF AVAILABLE bf-itemfg THEN 
    DO:
        RUN pGetConversionFactorForItem(bf-itemfg.company, bf-itemfg.i-no, "FG", "", ipcConvertUOM, OUTPUT dConversionFactor, OUTPUT oplError, OUTPUT opcMessage).    
    END.
    ELSE IF AVAILABLE bf-item THEN 
        DO:
            RUN pGetConversionFactorForItem(bf-item.company, bf-item.i-no, "RM", "", ipcConvertUOM, OUTPUT dConversionFactor, OUTPUT oplError, OUTPUT opcMessage).
        END.
        ELSE 
            ASSIGN 
                oplError   = YES
                opcMessage = "Invalid item row id"
                .    
    IF NOT oplError AND dConversionFactor NE 0 THEN 
        IF ipcFromOrTo = "FromEAToUOM" THEN 
            opdValueConverted = ipdValue / dConversionFactor.
        ELSE 
            opdValueConverted = ipdValue * dConversionFactor.
        
END PROCEDURE.

PROCEDURE pGetBuffers PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: given a rowid, set key buffers
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriRowid AS ROWID NO-UNDO.
    DEFINE PARAMETER BUFFER opbf-itemfg FOR itemfg.
    DEFINE PARAMETER BUFFER opbf-item   FOR ITEM.

    FIND FIRST oe-ordl NO-LOCK 
        WHERE ROWID(oe-ordl) EQ ipriRowid
        NO-ERROR.
    IF AVAILABLE oe-ordl AND oe-ordl.i-no NE "" THEN 
        FIND FIRST opbf-itemfg NO-LOCK
            WHERE opbf-itemfg.company EQ oe-ordl.company
            AND opbf-itemfg.i-no EQ oe-ordl.i-no    
            NO-ERROR.
    IF NOT AVAILABLE opbf-itemfg THEN 
        FIND FIRST opbf-itemfg NO-LOCK
            WHERE ROWID(opbf-itemfg) EQ ipriRowID
            NO-ERROR.
    IF NOT AVAILABLE opbf-itemfg THEN 
        FIND FIRST opbf-item NO-LOCK 
            WHERE ROWID(opbf-item) EQ ipriRowID
            NO-ERROR.
        
END PROCEDURE.

PROCEDURE pGetConversionFactorForItem PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Given attributes, find if there is an item specific conversion factor.
     Return explanation for error or 0 factor
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcPurpose AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcUOM AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdConversionFactor AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    opcMessage = "UOM:" + ipcUOM + " for " + ipcItemType + " item:" + ipcItemID.

    FIND FIRST itemUoM NO-LOCK 
        WHERE itemUoM.company EQ ipcCompany
        AND itemUoM.itemID EQ ipcItemID
        AND itemUoM.itemType EQ ipcItemType
        AND itemUoM.UOM EQ ipcUOM
        NO-ERROR.
    /*Refactor - Assumes itemUom.BaseUOM EQ "EA"*/
    IF AVAILABLE itemUoM THEN 
    DO:
        IF itemUoM.inactive THEN  
            ASSIGN 
                opcMessage = opcMessage + " is inactive"
                oplError   = YES
                .
        ELSE IF itemUoM.convFactor EQ 0 THEN 
                ASSIGN 
                    opcMessage = opcMessage + " has a 0 conversion factor"
                    oplError   = YES
                    .
            ELSE 
            DO:
                RUN pCheckPurposeForItemUOM (BUFFER itemUOM, ipcPurpose, OUTPUT oplError, INPUT-OUTPUT opcMessage). 
                IF NOT oplError THEN  
                    ASSIGN 
                        opdConversionFactor = itemUoM.convFactor.
            END. 
    END.
    ELSE
        ASSIGN
            opcMessage = opcMessage + " not found"
            oplError   = YES. 

    

END PROCEDURE.

FUNCTION fConvertValueToEAFromUOM RETURNS DECIMAL
    ( ipcCompany AS CHARACTER, ipcItemType AS CHARACTER, ipcItemID AS CHARACTER, ipdValueToConvert AS DECIMAL, ipcValueToConvertUom AS CHARACTER):

    /*------------------------------------------------------------------------------
     Purpose: Given an item, quantity and UOM, return the quantity in EA UOM
     Notes:  Assumes EA UOM is EA
    ------------------------------------------------------------------------------*/

    DEFINE VARIABLE opdValueConverted AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lError            AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dConversionFactor AS DECIMAL   NO-UNDO.
    
    RUN pGetConversionFactorForItem(ipcCompany, ipcItemID, ipcItemType, "", ipcValueToConvertUOM, OUTPUT dConversionFactor, OUTPUT lError, OUTPUT cMessage).
    
    IF NOT lError AND dConversionFactor NE 0 THEN
        opdValueConverted = ipdValueToConvert * dConversionFactor.
    ELSE 
        opdValueConverted = ipdValueToConvert.
    
    RETURN opdValueConverted.

END FUNCTION.

FUNCTION fConvertValueFromEAToUOM RETURNS DECIMAL
    ( ipcCompany AS CHARACTER, ipcItemType AS CHARACTER, ipcItemID AS CHARACTER, ipdValueInEA AS DECIMAL, ipcValueToConvertUom AS CHARACTER):

    /*------------------------------------------------------------------------------
     Purpose: Given an item, quantity and UOM, return the quantity in EA UOM
     Notes:  Assumes EA UOM is EA
    ------------------------------------------------------------------------------*/

    DEFINE VARIABLE opdValueConverted AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lError            AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dConversionFactor AS DECIMAL   NO-UNDO.
        
    RUN pGetConversionFactorForItem(ipcCompany, ipcItemID, ipcItemType, "", ipcValueToConvertUOM, OUTPUT dConversionFactor, OUTPUT lError, OUTPUT cMessage).
            
    IF NOT lError AND dConversionFactor NE 0 THEN 
        opdValueConverted = ipdValueInEA / dConversionFactor.
    ELSE 
        opdValueConverted = ipdValueInEA.
    
    RETURN opdValueConverted.

END FUNCTION.