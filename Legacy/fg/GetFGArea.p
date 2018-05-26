
/*------------------------------------------------------------------------
    File        : GetFGArea.p
    Purpose     : 

    Syntax      :

    Description : Given a Finished Good item code and desired UOM, the system will return the correct per each area value

    Author(s)   : BV
    Created     : Thu Apr 19 12:06:04 EDT 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ipriItemfg AS ROWID NO-UNDO.
DEFINE INPUT PARAMETER ipcAreaUOM AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opdArea AS DECIMAL NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

FIND FIRST itemfg NO-LOCK 
    WHERE ROWID(itemfg) EQ ipriItemfg 
    NO-ERROR.
IF AVAILABLE itemfg THEN DO:
    IF itemfg.isaset THEN 
        RUN pGetSetArea(BUFFER itemfg, INPUT ipcAreaUOM, OUTPUT opdArea).
    ELSE 
        RUN pGetItemArea(BUFFER itemfg, INPUT ipcAreaUOM, OUTPUT opdArea).
END.

/* **********************  Internal Procedures  *********************** */

PROCEDURE pGetItemArea:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE PARAMETER BUFFER ipbf-itemfg FOR itemfg.
DEFINE INPUT PARAMETER ipcUOM AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opdArea AS DECIMAL NO-UNDO.

DEFINE VARIABLE dSqin AS DECIMAL NO-UNDO.
DEFINE VARIABLE dSqft AS DECIMAL NO-UNDO.

ASSIGN
    dSqin = ipbf-itemfg.t-sqin
    dSqft = ipbf-itemfg.t-sqft
    .
IF ipbf-itemfg.spare-int-2 NE 1 AND (dSqin EQ 0.0 OR dSqft EQ 0.0) THEN DO:  /*not locked*/ 
    IF dSqin EQ 0 THEN
        dSqin = ipbf-itemfg.t-len * ipbf-itemfg.t-wid.
    IF dSqft EQ 0 THEN
        dSqft = dSqin / 144.
END.
CASE ipcUom: 
    WHEN "SQIN" THEN  
        opdArea = dSqin.
    WHEN "SF" OR WHEN "SQFT" THEN 
        opdArea = dSqft.
    WHEN "MSF" THEN 
        opdArea = dSqft / 1000.
END CASE.
    
END PROCEDURE.

PROCEDURE pGetSetArea:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE PARAMETER BUFFER ipbf-itemfg FOR itemfg.
DEFINE INPUT PARAMETER ipcUOM AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opdTotalArea AS DECIMAL NO-UNDO.

DEFINE VARIABLE iPartCount AS INTEGER NO-UNDO.
DEFINE VARIABLE dItemArea AS DECIMAL NO-UNDO.
DEFINE VARIABLE dQtyPerSet AS DECIMAL NO-UNDO.

DEFINE BUFFER bf-itemfg FOR itemfg.

ASSIGN 
    iPartCount = 0
    dItemArea = 0
    .
/*Sum up the areas of the components*/
FOR EACH fg-set FIELDS(part-no part-qty qtyPerSet) NO-LOCK  
    WHERE fg-set.company EQ ipbf-itemfg.company 
    AND fg-set.set-no EQ ipbf-itemfg.i-no
    ,
    FIRST bf-itemfg NO-LOCK
        WHERE bf-itemfg.company EQ fg-set.company 
        AND bf-itemfg.i-no EQ fg-set.part-no
       :
        dItemArea = 0.
        RUN pGetItemArea(BUFFER bf-itemfg, ipcUOM, OUTPUT dItemArea).
        ASSIGN 
            dQtyPerSet = IF fg-set.qtyPerSet EQ 0 THEN DECIMAL(fg-set.part-qty) ELSE fg-set.qtyPerSet
            dQtyPerSet = IF dQtyPerSet EQ 0 THEN 1 ELSE dQtyPerSet
            dQtyPerSet = IF dQtyPerSet GE 0 THEN dQtyPerSet ELSE (-1 / dQtyPerSet)
            opdTotalArea = opdTotalArea + dItemArea * dQtyPerSet
            iPartCount = iPartCount + 1.
END.

/*If 2-pc box/single-item set or problem with the set, use the header area*/
IF iPartCount LE 1 OR opdTotalArea EQ 0 THEN 
    RUN pGetItemArea(BUFFER ipbf-itemfg, ipcUOM, OUTPUT opdTotalArea).


END PROCEDURE.

