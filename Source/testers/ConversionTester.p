
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

DEFINE            VARIABLE ghSession         AS HANDLE.
DEFINE            VARIABLE ghOutput          AS HANDLE.
DEFINE            VARIABLE ghConversionProcs AS HANDLE.
DEFINE NEW SHARED VARIABLE cocode            AS CHARACTER NO-UNDO INIT "001".

DEFINE TEMP-TABLE ttResults
    FIELD cItemID            AS CHARACTER
    FIELD cConversionType    AS CHARACTER
    FIELD cItemType          AS CHARACTER 
    FIELD dOldValue          AS DECIMAL
    FIELD cOldUOM            AS CHARACTER 
    FIELD cNewUOM            AS CHARACTER
    FIELD dNewValueLegacy    AS DECIMAL
    FIELD dNewValueNewProc   AS DECIMAL 
    FIELD dConvFactorLegacy  AS DECIMAL 
    FIELD dConvFactorNewProc AS DECIMAL 
    FIELD lMatchExact        AS LOGICAL
    FIELD lMatchRounded      AS LOGICAL
    FIELD lError             AS LOGICAL 
    FIELD cMessage           AS CHARACTER
    .   

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

RUN system\session.p PERSISTENT SET ghSession.
SESSION:ADD-SUPER-PROCEDURE (ghSession).
RUN spSetSessionParam ("Company", "001").
RUN system\OutputProcs.p PERSISTENT SET ghOutput.
SESSION:ADD-SUPER-PROCEDURE (ghOutput).

//RUN pTestGetConversionList.
//RUN pTestConversionForItem.
//RUN pTestExtendedCalc.
//RUN pTestIsEAFunction.
//RUN pTestAndCompareAllFGItems.
//RUN pTestAndCompareAllRMItems.
RUN pTestAndCompareAllPOLines.
/* **********************  Internal Procedures  *********************** */


PROCEDURE pTestAndCompareAllFGItems PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE dOldValue  AS DECIMAL   INITIAL 1.
    DEFINE VARIABLE cUOMList   AS CHARACTER INITIAL "MSF,SF,LF,M,EA,C,DOZ,CWT,TON,LB".
    DEFINE VARIABLE iIndexFrom AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iIndexTo   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cFromUOM   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cToUOM     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iIndexType AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lSuccess   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage   AS CHARACTER NO-UNDO.
    
    EMPTY TEMP-TABLE ttResults.
    
    FOR EACH itemfg NO-LOCK
        WHERE itemfg.company EQ "001"
        AND itemfg.i-no EQ 'ZOV10x10x10'
        :
        DO iIndexFrom = 1 TO NUM-ENTRIES(cUomList):
            cFromUOM = ENTRY(iIndexFrom, cUOMList).
            DO iIndexTo = 1 TO NUM-ENTRIES(cUOMlist):
                cToUOM = ENTRY(iIndexTo, cUOMList).                        
                DO iIndexType = 1 TO 2:
                    CREATE ttResults.
                    ASSIGN 
                        ttResults.cItemID   = itemfg.i-no
                        ttResults.cItemType = "FG"
                        ttResults.dOldValue = dOldValue
                        ttResults.cOldUOM   = cFromUOM
                        ttResults.cNewUOM   = cToUOM
                        .
                    IF iIndexType EQ 1 THEN 
                    DO:
                        ttResults.cConversionType = "Quantity".
                        RUN sys/ref/convquom.p(cFromUOM, cToUOM, 
                            itemfg.weight-100, itemfg.t-len, itemfg.t-wid, itemfg.t-dep,
                            dOldValue, 
                            OUTPUT ttResults.dNewValueLegacy).
                        RUN Conv_QuantityFromUOMtoUOM(itemfg.company, itemfg.i-no, "FG", 
                            dOldValue, cOldUOM, cNewUOM, 
                            0, 0, 0, 0, 0,
                            OUTPUT ttResults.dNewValueNewProc, OUTPUT ttResults.lError, OUTPUT ttResults.cMessage).
                        ASSIGN 
                            ttResults.dConvFactorLegacy  = dOldValue / dNewValueLegacy
                            ttResults.dConvFactorNewProc = dOldValue / dNewValueNewProc
                            .  
                            
                    END. 
                    ELSE 
                    DO:
                        ttResults.cConversionType = "Value".
                        RUN sys/ref/convcuom.p(cFromUOM, cToUOM, 
                            0, itemfg.t-len, itemfg.t-wid, itemfg.t-dep,
                            dOldValue, 
                            OUTPUT ttResults.dNewValueLegacy).
                        RUN Conv_ValueFromUOMtoUOM(itemfg.company, itemfg.i-no, "FG", 
                            dOldValue, cOldUOM, cNewUOM, 
                            0, 0, 0, 0, 0,
                            OUTPUT ttResults.dNewValueNewProc, OUTPUT ttResults.lError, OUTPUT ttResults.cMessage).
                        ASSIGN 
                            ttResults.dConvFactorLegacy  = dNewValueLegacy / dOldValue
                            ttResults.dConvFactorNewProc = dNewValueNewProc / dOldValue
                            .  
                            
                    END.                       
                    ASSIGN 
                        ttResults.lMatchExact   = ttResults.dNewValueNewProc EQ ttResults.dNewValueLegacy
                        ttResults.lMatchRounded = ROUND(ttResults.dNewValueNewProc,2) EQ ROUND(ttResults.dNewValueLegacy,2)
                        .
                END.
            END.
        END.
    END.
    RUN Output_TempTableToCSV(TEMP-TABLE ttResults:HANDLE, 
                              "C:\tmp\ConversionResultsItemfg.csv", 
                              YES,
                              OUTPUT lSuccess,
                              OUTPUT cMessage).                              
    
END PROCEDURE.

PROCEDURE pTestAndCompareAllPOLines PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE dBasisWeight AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lSuccess     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage     AS CHARACTER NO-UNDO.
        
    EMPTY TEMP-TABLE ttResults.
    
    FOR EACH po-ordl NO-LOCK
        WHERE po-ordl.company EQ "001"
        AND po-ordl.stat NE "C"
        AND po-ordl.i-no EQ 'RM2625'
        :
        dBasisWeight = 0.
        IF po-ordl.item-type THEN 
        DO:
            FIND FIRST ITEM NO-LOCK 
                WHERE item.company EQ po-ordl.company
                AND item.i-no EQ po-ordl.i-no
                NO-ERROR.
            IF AVAILABLE ITEM THEN 
                dBasisWeight = item.basis-w.
        END.
        
        CREATE ttResults.
        ASSIGN 
            ttResults.cItemID         = po-ordl.i-no
            ttResults.dOldValue       = po-ordl.ord-qty
            ttResults.cItemType       = IF po-ordl.item-type THEN "RM" ELSE "FG"
            ttResults.cOldUOM         = CAPS(po-ordl.pr-qty-uom)
            ttResults.cNewUOM         = "EA"
            ttResults.cConversionType = "Quantity"
            .
        RUN sys/ref/convquom.p(ttResults.cOldUOM, ttResults.cNewUOM, 
            dBasisWeight, po-ordl.s-len, po-ordl.s-len, po-ordl.s-len,
            ttResults.dOldValue,
            OUTPUT ttResults.dNewValueLegacy).
        RUN Conv_QuantityFromUOMtoUOM(po-ordl.company, po-ordl.i-no, ttResults.cItemType, 
            ttResults.dOldValue, ttResults.cOldUOM, ttResults.cNewUOM, 
            dBasisWeight, po-ordl.s-len, po-ordl.s-len, po-ordl.s-len, 0,
            OUTPUT ttResults.dNewValueNewProc, OUTPUT ttResults.lError, OUTPUT ttResults.cMessage).
        ASSIGN 
            ttResults.dConvFactorLegacy  = ttResults.dOldValue / ttResults.dNewValueLegacy
            ttResults.dConvFactorNewProc = ttResults.dOldValue / ttResults.dNewValueNewProc
            ttResults.lMatchExact        = ttResults.dNewValueNewProc EQ ttResults.dNewValueLegacy
            ttResults.lMatchRounded      = ROUND(ttResults.dNewValueNewProc,2) EQ ROUND(ttResults.dNewValueLegacy,2)
            ttResults.cMessage           = STRING(po-ordl.po-no) + "-" + STRING(po-ordl.line) + "|" + ttResults.cMessage
            .

        CREATE ttResults.
        ASSIGN 
            ttResults.cItemID         = po-ordl.i-no
            ttResults.cItemType       = IF po-ordl.item-type THEN "RM" ELSE "FG"
            ttResults.dOldValue       = po-ordl.cost
            ttResults.cOldUOM         = CAPS(po-ordl.pr-uom)
            ttResults.cNewUOM         = "EA"
            ttResults.cConversionType = "Cost"
            .
        RUN sys/ref/convcuom.p(ttResults.cOldUOM, ttResults.cNewUOM, 
            dBasisWeight, po-ordl.s-len, po-ordl.s-len, po-ordl.s-len,
            ttResults.dOldValue,
            OUTPUT ttResults.dNewValueLegacy).
        RUN Conv_ValueFromUOMtoUOM(po-ordl.company, po-ordl.i-no, ttResults.cItemType, 
            ttResults.dOldValue, ttResults.cOldUOM, ttResults.cNewUOM, 
            dBasisWeight, po-ordl.s-len, po-ordl.s-len, po-ordl.s-len, 0,
            OUTPUT ttResults.dNewValueNewProc, OUTPUT ttResults.lError, OUTPUT ttResults.cMessage).
        ASSIGN 
            ttResults.dConvFactorLegacy  = ttResults.dOldValue / ttResults.dNewValueLegacy
            ttResults.dConvFactorNewProc = ttResults.dOldValue / ttResults.dNewValueNewProc
            ttResults.lMatchExact        = ttResults.dNewValueNewProc EQ ttResults.dNewValueLegacy
            ttResults.lMatchRounded      = ROUND(ttResults.dNewValueNewProc,2) EQ ROUND(ttResults.dNewValueLegacy,2)
            ttResults.cMessage           = STRING(po-ordl.po-no) + "-" + STRING(po-ordl.line) + "|" + ttResults.cMessage
            .
    END.


    RUN Output_TempTableToCSV(TEMP-TABLE ttResults:HANDLE, 
                              "C:\tmp\ConversionResultsPoLines.csv", 
                              YES,
                              OUTPUT lSuccess,
                              OUTPUT cMessage).
    
END PROCEDURE.

PROCEDURE pTestAndCompareAllRMItems PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE dOldValue  AS DECIMAL   INITIAL 1.
    DEFINE VARIABLE cUOMList   AS CHARACTER INITIAL "EA,LB".
    DEFINE VARIABLE iIndexFrom AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iIndexTo   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cFromUOM   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cToUOM     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iIndexType AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lSuccess   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage   AS CHARACTER NO-UNDO. 
    
    EMPTY TEMP-TABLE ttResults.
    
    FOR EACH item NO-LOCK
        WHERE item.company EQ "001"
        AND item.i-code EQ "R"
        AND item.i-no EQ '200B38X40'
        :
        DO iIndexFrom = 1 TO NUM-ENTRIES(cUomList):
            cFromUOM = ENTRY(iIndexFrom, cUOMList).
            DO iIndexTo = 1 TO NUM-ENTRIES(cUOMlist):
                cToUOM = ENTRY(iIndexTo, cUOMList).                        
                DO iIndexType = 1 TO 2:
                    CREATE ttResults.
                    ASSIGN 
                        ttResults.cItemID   = item.i-no
                        ttResults.cItemType = "RM"
                        ttResults.dOldValue = dOldValue
                        ttResults.cOldUOM   = cFromUOM
                        ttResults.cNewUOM   = cToUOM
                        .
                    IF iIndexType EQ 1 THEN 
                    DO:
                        ttResults.cConversionType = "Quantity".
                        RUN sys/ref/convquom.p(cFromUOM, cToUOM, 
                            item.basis-w, item.s-len, item.s-wid, item.s-dep,
                            dOldValue, 
                            OUTPUT ttResults.dNewValueLegacy).
                        RUN Conv_QuantityFromUOMtoUOM(item.company, item.i-no, "RM", 
                            dOldValue, cOldUOM, cNewUOM, 
                            0, 0, 0, 0, 0,
                            OUTPUT ttResults.dNewValueNewProc, OUTPUT ttResults.lError, OUTPUT ttResults.cMessage).
                        ASSIGN 
                            ttResults.dConvFactorLegacy  = dOldValue / dNewValueLegacy
                            ttResults.dConvFactorNewProc = dOldValue / dNewValueNewProc
                            .  
                            
                    END. 
                    ELSE 
                    DO:
                        ttResults.cConversionType = "Value".
                        RUN sys/ref/convcuom.p(cFromUOM, cToUOM, 
                            item.basis-w, item.s-len, item.s-wid, item.s-dep,
                            dOldValue, 
                            OUTPUT ttResults.dNewValueLegacy).
                        RUN Conv_ValueFromUOMtoUOM(item.company, item.i-no, "RM", 
                            dOldValue, cOldUOM, cNewUOM, 
                            0, 0, 0, 0, 0,
                            OUTPUT ttResults.dNewValueNewProc, OUTPUT ttResults.lError, OUTPUT ttResults.cMessage).
                        ASSIGN 
                            ttResults.dConvFactorLegacy  = dNewValueLegacy / dOldValue
                            ttResults.dConvFactorNewProc = dNewValueNewProc / dOldValue
                            .  
                            
                    END.                       
                    ASSIGN 
                        ttResults.lMatchExact   = ttResults.dNewValueNewProc EQ ttResults.dNewValueLegacy
                        ttResults.lMatchRounded = ROUND(ttResults.dNewValueNewProc,2) EQ ROUND(ttResults.dNewValueLegacy,2)
                        .
                END.
            END.
        END.
    END.
    RUN Output_TempTableToCSV(TEMP-TABLE ttResults:HANDLE, 
                              "C:\tmp\ConversionResultsItem.csv", 
                              YES,
                              OUTPUT lSuccess,
                              OUTPUT cMessage).
    
END PROCEDURE.

PROCEDURE pTestConversionForItem PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE dOldValue AS DECIMAL   NO-UNDO INITIAL 20.
    DEFINE VARIABLE dNewValue AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lError    AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCompany  AS CHARACTER NO-UNDO INITIAL "001".
    DEFINE VARIABLE cItemID   AS CHARACTER NO-UNDO INITIAL "ZOV10X10X10".
         
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
    DEFINE VARIABLE dQty        AS DECIMAL   NO-UNDO INITIAL 6000.
    DEFINE VARIABLE cQtyUOM     AS CHARACTER NO-UNDO INITIAL "CS".
    DEFINE VARIABLE dPrice      AS DECIMAL   NO-UNDO INITIAL 60.
    DEFINE VARIABLE cPriceUOM   AS CHARACTER NO-UNDO INITIAL "ZO".
    DEFINE VARIABLE dTotalPrice AS DECIMAL   NO-UNDO.
    
    DEFINE VARIABLE lError      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage    AS CHARACTER NO-UNDO.    
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
    
    DEFINE VARIABLE lError   AS LOGICAL   NO-UNDO.
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
    
    DEFINE VARIABLE lError   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cUOMList AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE i        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cEAList  AS CHARACTER NO-UNDO.
    
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
