
/*------------------------------------------------------------------------
    File        : ConversionTester.p
    Purpose     : 

    Syntax      :

    Description : Tester for ConversionProcs.p

    Author(s)   : BV
    Created     : Fri Mar 13 00:39:19 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE VARIABLE ghSession           AS HANDLE.
DEFINE VARIABLE ghConversionProcs   AS HANDLE.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

RUN system\session.p PERSISTENT SET ghSession.
SESSION:ADD-SUPER-PROCEDURE (ghSession).
RUN spSetSessionParam ("Company", "001").


//RUN pTestGetConversionList.
RUN pTestConversionForItem.
//RUN pTestExtendedCalc.
//RUN pTestIsEAFunction.

/* **********************  Internal Procedures  *********************** */

PROCEDURE pTestConversionForItem PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE dOldValue AS DECIMAL NO-UNDO INITIAL 20.
    DEFINE VARIABLE dNewValue AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lError AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO INITIAL "001".
    DEFINE VARIABLE cItemID AS CHARACTER NO-UNDO INITIAL "ZOV10X10X10".
         
    FIND FIRST itemfg NO-LOCK 
        WHERE itemfg.company EQ cCompany
        AND itemfg.i-no EQ cItemID
        NO-ERROR.
    RUN Conv_QuantityFromUOMToUOMForItem(ROWID(itemfg), dOldValue, "M", "EA", OUTPUT dNewValue, OUTPUT lError, OUTPUT cMessage).
    MESSAGE dNewValue SKIP(2)
        cMessage
    VIEW-AS ALERT-BOX.
    RUN Conv_QuantityFromUOMtoUOMForItem(ROWID(itemfg), dOldValue, "ZO", "EA", OUTPUT dNewValue, OUTPUT lError, OUTPUT cMessage).
    MESSAGE "ZO to EA: "dNewValue SKIP(2)
        cMessage
    VIEW-AS ALERT-BOX.
    RUN Conv_QuantityFromUOMtoUOM(cCompany, cItemID, "FG", dOldValue, "ZO", "EA", 0,0,0,0,0, OUTPUT dNewValue, OUTPUT lError, OUTPUT cMessage).
    MESSAGE "ZO to EA: (no overrides)" dNewValue SKIP(2)
        cMessage
    VIEW-AS ALERT-BOX.
    RUN Conv_QuantityFromUOMtoUOM(cCompany, cItemID, "FG", dOldValue, "CS", "EA", 0,0,0,0,0, OUTPUT dNewValue, OUTPUT lError, OUTPUT cMessage).
    MESSAGE "CS to EA: (no overrides)" dNewValue SKIP(2)
        cMessage
    VIEW-AS ALERT-BOX.
    RUN Conv_QuantityFromUOMtoUOM(cCompany, cItemID, "FG", dOldValue, "CS", "EA", 0,0,0,0,50, OUTPUT dNewValue, OUTPUT lError, OUTPUT cMessage).
    MESSAGE "CS to EA: (overrides)" dNewValue SKIP(2)
        cMessage
    VIEW-AS ALERT-BOX.
    
    RUN Conv_QuantityFromUOMtoUOM(cCompany, cItemID, "FG", dOldValue, "MSF", "EA", 0,12,12,0,50, OUTPUT dNewValue, OUTPUT lError, OUTPUT cMessage).
    MESSAGE "MSF to EA: (overrides)" dNewValue SKIP(2)
        cMessage
    VIEW-AS ALERT-BOX.
    
    RUN Conv_ValueFromUOMToUOMForItem(ROWID(itemfg), dOldValue, "M", "EA", OUTPUT dNewValue, OUTPUT lError, OUTPUT cMessage).
    MESSAGE dNewValue SKIP(2)
        cMessage
    VIEW-AS ALERT-BOX.
    RUN Conv_ValueFromUOMtoUOMForItem(ROWID(itemfg), dOldValue, "ZO", "EA", OUTPUT dNewValue, OUTPUT lError, OUTPUT cMessage).
    MESSAGE "ZO to EA: "dNewValue SKIP(2)
        cMessage
    VIEW-AS ALERT-BOX.
    RUN Conv_ValueFromUOMtoUOM(cCompany, cItemID, "FG", dOldValue, "ZO", "EA", 0,0,0,0,0, OUTPUT dNewValue, OUTPUT lError, OUTPUT cMessage).
    MESSAGE "ZO to EA: (no overrides)" dNewValue SKIP(2)
        cMessage
    VIEW-AS ALERT-BOX.
    RUN Conv_ValueFromUOMtoUOM(cCompany, cItemID, "FG", dOldValue, "CS", "EA", 0,0,0,0,0, OUTPUT dNewValue, OUTPUT lError, OUTPUT cMessage).
    MESSAGE "CS to EA: (no overrides)" dNewValue SKIP(2)
        cMessage
    VIEW-AS ALERT-BOX.
    RUN Conv_ValueFromUOMtoUOM(cCompany, cItemID, "FG", dOldValue, "CS", "EA", 0,0,0,0,50, OUTPUT dNewValue, OUTPUT lError, OUTPUT cMessage).
    MESSAGE "CS to EA: (overrides)" dNewValue SKIP(2)
        cMessage
    VIEW-AS ALERT-BOX.
    
    RUN Conv_ValueFromUOMtoUOM(cCompany, cItemID, "FG", dOldValue, "MSF", "EA", 0,12,12,0,50, OUTPUT dNewValue, OUTPUT lError, OUTPUT cMessage).
    MESSAGE "MSF to EA: (overrides)" dNewValue SKIP(2)
        cMessage
    VIEW-AS ALERT-BOX.
    
END PROCEDURE.

PROCEDURE pTestExtendedCalc PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE dQty AS DECIMAL NO-UNDO INITIAL 6000.
    DEFINE VARIABLE cQtyUOM AS CHARACTER NO-UNDO INITIAL "CS".
    DEFINE VARIABLE dPrice AS DECIMAL NO-UNDO INITIAL 60.
    DEFINE VARIABLE cPriceUOM AS CHARACTER NO-UNDO INITIAL "ZO".
    DEFINE VARIABLE dTotalPrice AS DECIMAL NO-UNDO.
    
    DEFINE VARIABLE lError AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.    
    FIND FIRST itemfg NO-LOCK 
        WHERE itemfg.company EQ '001'
        AND itemfg.i-no EQ 'ZOV10X10X10'
        NO-ERROR.
    RUN Conv_CalcExtendedValueForItem(ROWID(itemfg), dQty, cQtyUOM, dPrice, cPriceUOM, OUTPUT dTotalPrice, OUTPUT lError, OUTPUT cMessage).
    MESSAGE dTotalPrice SKIP(2)
        cMessage
    VIEW-AS ALERT-BOX.



END PROCEDURE.

PROCEDURE pTestGetConversionList PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    
    DEFINE VARIABLE lError AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.    
    DEFINE VARIABLE cUOMList AS CHARACTER NO-UNDO.
    
    FIND FIRST itemfg NO-LOCK 
        WHERE itemfg.company EQ '001'
        AND itemfg.i-no EQ 'ZOV10X10X10'
        NO-ERROR.
    RUN Conv_GetValidPriceUOMsForItem(ROWID(itemfg), OUTPUT cUOMList, OUTPUT lError, OUTPUT cMessage).   
    MESSAGE "Prices" cUomList SKIP(2)
        cMessage
    VIEW-AS ALERT-BOX.
    RUN Conv_GetValidOrderQtyUOMsForItem(ROWID(itemfg), OUTPUT cUOMlist, OUTPUT lError, OUTPUT cMessage).   
    MESSAGE "Order Quantity" cUomList SKIP(2)
        cMessage
    VIEW-AS ALERT-BOX.

END PROCEDURE.

PROCEDURE pTestIsEAFunction PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    
    DEFINE VARIABLE lError AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cUOMList AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE i AS INTEGER NO-UNDO.
    DEFINE VARIABLE cEAList AS CHARACTER NO-UNDO.
    
    FIND FIRST itemfg NO-LOCK 
        WHERE itemfg.company EQ '001'
        AND itemfg.i-no EQ 'ZOV10X10X10'
        NO-ERROR.
    
    RUN Conv_GetValidPriceUOMsForItem(ROWID(itemfg), OUTPUT cUOMList, OUTPUT lError, OUTPUT cMessage).
    DO i = 1 TO NUM-ENTRIES(cUOMLIst):
        IF DYNAMIC-FUNCTION("Conv_IsEAUOM",itemfg.company, itemfg.i-no, ENTRY(i,cUOMList)) THEN 
            cEAList = cEAList + ENTRY(i,cUOMList) + ",".
    END.
    cEAList = TRIM(cEAList,",").
    MESSAGE 
        "Full List: " cUOMLIst SKIP(2)
        "EA List: " cEAList
    VIEW-AS ALERT-BOX. 
END PROCEDURE.
