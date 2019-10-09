
/*------------------------------------------------------------------------
    File        : CalcPacking.p
    Purpose     : 

    Syntax      :

    Description : Calculates the Packing Counts and other information for a given estimate blank (eb)

    Author(s)   : BV
    Created     : Sun Feb 25 12:26:53 EST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ipriEb AS ROWID NO-UNDO.

DEFINE VARIABLE dUnitsPerPallet AS DECIMAL   NO-UNDO.
DEFINE VARIABLE iCountOnPallet  AS INTEGER   NO-UNDO.
DEFINE VARIABLE iLayers         AS INTEGER   NO-UNDO.
DEFINE VARIABLE iStacks         AS INTEGER   NO-UNDO.
DEFINE VARIABLE cStackCode      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPackCode       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lError          AS LOGICAL   NO-UNDO.
    
   
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
FIND eb WHERE ROWID(eb) EQ ipriEb.
    
IF eb.tr-cnt = 0 THEN 
    eb.tr-cnt = eb.cas-cnt * eb.cas-pal.

IF eb.stock-no = "" THEN 
DO:
    FIND FIRST ce-ctrl NO-LOCK  
        WHERE ce-ctrl.company EQ eb.company 
        AND ce-ctrl.loc EQ eb.loc
        NO-ERROR.
    IF AVAILABLE ce-ctrl THEN 
        ASSIGN 
            eb.cas-no = ce-ctrl.def-case
            eb.tr-no  = ce-ctrl.def-pal.      
END.
FIND FIRST cust NO-LOCK  
    WHERE cust.company EQ eb.company 
    AND cust.cust-no EQ eb.cust-no
    NO-ERROR.
IF AVAILABLE cust THEN 
DO: 
    ASSIGN
        eb.cas-no = IF cust.case-bundle NE '' THEN cust.case-bundle ELSE eb.cas-no 
        eb.tr-no  = IF cust.pallet NE '' THEN cust.pallet ELSE eb.tr-no
        .
    RUN est/packCodeOverride.p (eb.company, eb.cust-no, eb.style, OUTPUT cPackCode).
    IF cPackCode NE '' THEN eb.cas-no = cPackCode.
    FIND FIRST shipto NO-LOCK 
        WHERE shipto.company EQ cust.company
        AND shipto.ship-id EQ eb.ship-id
        NO-ERROR.
    IF AVAILABLE shipto AND shipto.pallet NE '' THEN 
        eb.tr-no = shipto.pallet. 
END.         
FIND item NO-LOCK 
    WHERE item.company EQ eb.company 
    AND item.i-no EQ eb.cas-no
    NO-ERROR.
IF AVAILABLE item THEN 
    ASSIGN 
        eb.cas-cnt = (item.box-case)
        eb.cas-len = (item.case-l)
        eb.cas-wid = (item.case-w)
        eb.cas-dep = (item.case-d)
        eb.cas-pal = (item.case-pall)
        eb.cas-wt  = (item.avg-w)         
        .
FIND FIRST item NO-LOCK 
    WHERE item.company EQ eb.company 
    AND  item.i-no EQ eb.tr-no
    NO-ERROR.
IF AVAILABLE item THEN 
    ASSIGN 
        eb.tr-len = (item.case-l)
        eb.tr-wid = (item.case-w)
        eb.tr-dep = (item.case-d)
        .
    
RUN cec/kpallet.p (RECID(eb), 
    OUTPUT dUnitsPerPallet, 
    OUTPUT iCountOnPallet,
    OUTPUT iStacks, 
    OUTPUT cStackCode, 
    OUTPUT lError).

IF NOT lError THEN 
DO:
    iLayers = dUnitsPerPallet / iStacks.
    {sys/inc/roundup.i iLayers}

    ASSIGN
        eb.cas-pal    = dUnitsPerPallet
        eb.tr-cnt     = iCountOnPallet
        eb.tr-cas     = iLayers
        eb.stacks     = iStacks
        eb.stack-code = cStackCode.
END.


