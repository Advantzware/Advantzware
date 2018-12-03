
/*------------------------------------------------------------------------
    File        : PriceProcs.p
    Purpose     : Replaces oe/GetPriceMatrix.p  
                           oe/GetPriceMatrixPrice.p 
                           oe/GetPriceTotal.p
                           oe/oe-price.i (contents)
                           oe/oe-pric1.i
                           oe/oe-pric2.i
                           oe/oe-rpric.i (contents)

    Syntax      :

    Description : Persistent Procedure for housing logic related to 
    returning the correct price given inputs for finished goods

    Author(s)   : BV
    Created     : Mon Apr 30 15:40:43 EDT 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttFGItemClass
    FIELD cClassID       AS CHARACTER 
    FIELD dClassQuantity AS DECIMAL
    .
     
DEFINE TEMP-TABLE ttItemLines
    FIELD riLine          AS ROWID
    FIELD lIsPrimary      AS LOGICAL
    FIELD cCompany        AS CHARACTER 
    FIELD cFGItemID       AS CHARACTER
    FIELD cCustID         AS CHARACTER 
    FIELD cShipID         AS CHARACTER  
    FIELD cFGItemClass    AS CHARACTER 
    FIELD dQuantity       AS DECIMAL  
    FIELD dQuantityLookup AS DECIMAL
    FIELD dPrice          AS DECIMAL 
    FIELD cPriceUOM       AS CHARACTER
    FIELD dPriceTotal     AS DECIMAL 
    FIELD lMatrixExists   AS LOGICAL
    FIELD dDiscount       AS DECIMAL 
    FIELD iCaseCount      AS INTEGER
    FIELD cTableType      AS CHARACTER 
    .

{oe/ttPriceHold.i "SHARED"}
    

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fUseLastPrice RETURNS LOGICAL PRIVATE
    (ipcCompany AS CHARACTER) FORWARD.


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

PROCEDURE CheckPriceHold:
    /*------------------------------------------------------------------------------
     Purpose: Checks Price Hold for passed criteria.  Adds record to ttPriceHold table.
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFGItemID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustID   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcShipID   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdQuantity AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplPriceHold AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcPriceHoldReason AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cPriceHoldSetting     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lPriceHoldSet         AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lQtyInRange           AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lQtyMatch             AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lEffectiveDateAge     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE iEffectiveDateAgeDays AS INTEGER   NO-UNDO.

    RUN pGetPriceHoldCriteria(ipcCompany, 
        OUTPUT lPriceHoldSet, OUTPUT lQtyInRange, OUTPUT lQtyMatch, OUTPUT lEffectiveDateAge, OUTPUT iEffectiveDateAgeDays).
    IF NOT lPriceHoldSet THEN 
    DO:
        ASSIGN 
            oplPriceHold       = NO
            opcPriceHoldReason = "OEPriceHold Not Activated"
            .
    END.
    ELSE 
    DO:
        EMPTY TEMP-TABLE ttPriceHold.
        RUN pAddPriceHold(0, ipcCompany, ipcFGItemID, ipcCustID, ipcShipID, ipdQuantity,
            lQtyMatch, lQtyInRange, lEffectiveDateAge, iEffectiveDateAgeDays). 
        FIND FIRST ttPriceHold NO-LOCK NO-ERROR.
        IF AVAILABLE ttPriceHold THEN 
            ASSIGN
                oplPriceHold       = ttPriceHold.lPriceHold
                opcPriceHoldReason = ttPriceHold.cPriceHoldReason
                .
    END.

END PROCEDURE.

PROCEDURE CheckPriceHoldForOrder:
    /*------------------------------------------------------------------------------
     Purpose: Given an oe-ord rowid, check all order lines to see if Price Hold criteria
     is met.  Return price hold.  
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriOeOrd AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER iplPrompt AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplUpdateDB AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplPriceHold AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcPriceHoldReason AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-oe-ord  FOR oe-ord.
    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.

    DEFINE VARIABLE cPriceHoldSetting     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lPriceHoldSet         AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lQtyInRange           AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lQtyMatch             AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lEffectiveDateAge     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE iEffectiveDateAgeDays AS INTEGER   NO-UNDO.
 
    FIND FIRST bf-oe-ord NO-LOCK 
        WHERE ROWID(bf-oe-ord) EQ ipriOeOrd
        NO-ERROR.

    RUN pGetPriceHoldCriteria(bf-oe-ord.company, 
        OUTPUT lPriceHoldSet, OUTPUT lQtyInRange, OUTPUT lQtyMatch, OUTPUT lEffectiveDateAge, OUTPUT iEffectiveDateAgeDays).
    IF NOT lPriceHoldSet THEN 
    DO:
        ASSIGN 
            oplPriceHold       = NO
            opcPriceHoldReason = "OEPriceHold Not Activated"
            .
        RETURN.
    END.
    IF AVAILABLE bf-oe-ord THEN 
    DO:
        EMPTY TEMP-TABLE ttPriceHold.
        FOR EACH bf-oe-ordl OF bf-oe-ord WHERE bf-oe-ordl.i-no NE "" NO-LOCK:

            RUN pAddPriceHold(ROWID(bf-oe-ordl), bf-oe-ordl.company, bf-oe-ordl.i-no, bf-oe-ordl.cust-no, bf-oe-ordl.ship-id, bf-oe-ordl.qty,
                lQtyMatch, lQtyInRange, lEffectiveDateAge, iEffectiveDateAgeDays).
        END.
    END.
    FIND FIRST ttPriceHold NO-LOCK
        WHERE ttPriceHold.lPriceHold
        NO-ERROR.
    IF AVAILABLE ttPriceHold THEN
        ASSIGN 
            oplPriceHold       = YES
            opcPriceHoldReason = ttPriceHold.cPriceHoldReason
            .
    ELSE 
        ASSIGN 
            oplPriceHold       = NO
            opcPriceHoldReason = ""
            .
    IF iplPrompt AND oplPriceHold THEN 
        RUN oe/dPriceHoldPrompt.w.
    IF iplUpdateDB THEN 
    DO:
        FIND FIRST bf-oe-ord EXCLUSIVE-LOCK 
            WHERE ROWID(bf-oe-ord) EQ ipriOeOrd
            NO-ERROR.
        IF AVAILABLE bf-oe-ord THEN 
            ASSIGN          
                bf-oe-ord.priceHold       = oplPriceHold
                bf-oe-ord.priceHoldReason = opcPriceHoldReason
                .
        FIND CURRENT bf-oe-ord NO-LOCK.
    END.
    RELEASE bf-oe-ord.
    
END PROCEDURE.

PROCEDURE CheckPriceMatrix:
    /*------------------------------------------------------------------------------
     Purpose:  Performs Check based on OEPriceMatrixCheck NK1
        Returns information to prompt or not and to block entry to just warn
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.  /*Company*/
    DEFINE INPUT PARAMETER ipcFGItemID AS CHARACTER NO-UNDO.  /*FG Item ID*/
    DEFINE INPUT PARAMETER ipcCustID AS CHARACTER NO-UNDO.  /*Customer Scope of FG Item*/
    DEFINE INPUT PARAMETER ipcShipID AS CHARACTER NO-UNDO.  /*Ship to Scope of Customer  - optional*/
    DEFINE INPUT PARAMETER ipdQuantity AS DECIMAL NO-UNDO. /*to get price matrix appropriate for qty*/
    DEFINE INPUT PARAMETER ipdPrice AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplPrompt AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplBlockEntry AS LOGICAL NO-UNDO.
    
    DEFINE BUFFER bf-oe-prmtx FOR oe-prmtx.
    DEFINE BUFFER bf-itemfg   FOR itemfg.
    DEFINE BUFFER bf-cust     FOR cust.
        
    DEFINE VARIABLE lMatrixFound   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lQtyMatch      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iLevel         AS INTEGER   NO-UNDO. 
    DEFINE VARIABLE lCheckActive   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lAllowMax      AS LOGICAL   NO-UNDO .
    DEFINE VARIABLE cCheckCriteria AS CHARACTER NO-UNDO .
    DEFINE VARIABLE lLessThanMax   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lBlockEntry    AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE dPriceMtx      AS DECIMAL   NO-UNDO .
    DEFINE VARIABLE cPriceUOM      AS CHARACTER NO-UNDO.
    
    RUN pGetOEPriceMatrixCheckSettings(ipcCompany, OUTPUT oplPrompt, OUTPUT lAllowMax, OUTPUT oplBlockEntry).
    IF oplPrompt THEN 
    DO:
        RUN pSetBuffers(ipcCompany, ipcFGItemID, ipcCustID, BUFFER bf-itemfg, BUFFER bf-cust).
        RUN pGetPriceMatrix(BUFFER bf-itemfg, BUFFER bf-cust, BUFFER bf-oe-prmtx, ipcShipID, OUTPUT lMatrixFound, OUTPUT cMessage).
        IF lMatrixFound THEN 
        DO:
            RUN pGetQtyMatchInfo(BUFFER bf-oe-prmtx, ipdQuantity, 0, OUTPUT iLevel, OUTPUT lQtyMatch).
            RUN pGetPriceAtLevel(BUFFER bf-oe-prmtx, iLevel, bf-itemfg.sell-price, bf-itemfg.sell-uom, OUTPUT dPriceMtx, OUTPUT cPriceUOM).
            IF dPriceMtx NE ipdPrice THEN 
                opcMessage = cMessage + " but price should be " + STRING(dPriceMtx) + " not " + STRING(ipdPrice).
            ELSE 
            DO:                
                IF NOT lQtyMatch THEN 
                DO:
                    IF bf-oe-prmtx.qty[iLevel] GE 99999999 AND lAllowMax THEN 
                        ASSIGN 
                            opcMessage    = cMessage + " and Quantity of " + STRING(ipdQuantity) + " matched at max level"
                            oplPrompt     = NO
                            oplBlockEntry = NO.
                    ELSE 
                        ASSIGN 
                            opcMessage = cMessage + " but Quantity of " + STRING(ipdQuantity) + " not matched at any level". 
                END.    
                ELSE 
                    ASSIGN 
                        opcMessage    = cMessage + " and Quantity of " + STRING(ipdQuantity) + " matched at level " + string(iLevel)
                        oplPrompt     = NO
                        oplBlockEntry = NO.
            END.
        END.
        ELSE 
            opcMessage = cMessage.
    END.
    ELSE 
        ASSIGN 
            oplPrompt     = NO
            opcMessage    = "OEPriceMatrixCheck not active"
            oplBlockEntry = NO.
       

END PROCEDURE.

PROCEDURE CalculateLinePrice:
    /*------------------------------------------------------------------------------
     Purpose: Given an order line rowid, determine appropriate price from price matrix
     Can also pass FG Item ID and Qty since these may not be saved to the oe-ordl yet
     Notes: originally oe-price.i - replaces oe-price.p
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriLine AS ROWID NO-UNDO. /*main rowid of order or invoice line*/ 
    DEFINE INPUT PARAMETER ipcFGItemID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcShipID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdQuantity AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER iplUpdateDB AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplMatrixExists AS LOGICAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopdPrice AS DECIMAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopcPriceUOM AS CHARACTER NO-UNDO.
     
    DEFINE VARIABLE cType    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lReprice AS LOGICAL   NO-UNDO.
    
    /*Build the ttItemLines table - will only be one record if not auto-reprice - only FG Items of "Stock"*/
    RUN pBuildLineTable(ipriLine, ipcFGITemID, ipcCustID, ipcShipID, ipdQuantity, OUTPUT cType, OUTPUT lReprice).
    
    /*Build table that holds total quantity for each common FG Item Class amongst siblings,if cust.auto-reprice*/
    IF lReprice THEN RUN pBuildClassQuantityTable.  
    
    /*Go through each item line that is i-code "S" (including primary)*/ 
    FOR EACH ttItemLines:
        IF fUseLastPrice(ttItemLines.cCompany) THEN /*NK1 LastPric Rules*/
        DO:
            RUN pGetLastPrice(ttItemLines.riLine, 
                ttItemLines.cCompany, 
                ttItemLines.cFGItemID, 
                INPUT-OUTPUT ttItemLines.dPrice, 
                INPUT-OUTPUT ttItemLines.cPriceUOM).
        END.
        
        /*Continue to Reprice based on price matrix*/
        IF lReprice THEN /*Cust.auto-reprice*/
        DO:
            /*Get combined quantity for item class*/ 
            FIND FIRST ttFGItemClass NO-LOCK 
                WHERE ttFGItemClass.cClassID EQ ttItemLines.cFGItemClass
                NO-ERROR.
            IF AVAILABLE ttFGItemClass THEN 
                ttItemLines.dQuantityLookup = ttFGItemClass.dClassQuantity.
        END.
        IF ttItemLines.dQuantityLookup EQ 0 THEN 
            ttItemLines.dQuantityLookup = ttItemLines.dQuantity.
        
        /*Get Price from Matching Price Matrix (note if no match found, price not changed*/    
        RUN GetPriceMatrixPriceSimple (ttItemLines.cCompany,
            ttItemLines.cFGItemID, 
            ttItemLines.cCustID, 
            ttItemLines.cShipID,
            ttItemLines.dQuantityLookup,  
            OUTPUT ttItemLines.lMatrixExists, 
            INPUT-OUTPUT ttItemLines.dPrice, 
            INPUT-OUTPUT ttItemLines.cPriceUOM ).    
                
        IF ttItemLines.lIsPrimary THEN 
            ASSIGN 
                oplMatrixExists = ttItemLines.lMatrixExists
                iopdPrice       = ttItemLines.dPrice
                iopcPriceUOM    = ttItemLines.cPriceUOM
                .
        RUN GetPriceTotal (ttItemLines.dQuantity,
            ttItemLines.dPrice,
            ttItemLines.cPriceUOM,
            ttItemLines.iCaseCount,
            ttItemLines.dDiscount,
            OUTPUT ttItemLines.dPriceTotal).   
    END.
    IF iplUpdateDB THEN 
    DO:
        /*Assign order from ttItemLines*/
        FOR EACH ttItemLines NO-LOCK 
            WHERE ttItemLines.cTableType EQ "oe-ordl":
            FIND FIRST oe-ordl EXCLUSIVE-LOCK 
                WHERE ROWID(oe-ordl) EQ ttItemLines.riLine
                NO-ERROR.
            IF AVAILABLE oe-ordl THEN 
            DO: 
                ASSIGN
                    oe-ordl.price   = ttItemLines.dPrice
                    oe-ordl.pr-uom  = ttItemLines.cPriceUOM
                    oe-ordl.t-price = ttItemLines.dPriceTotal
                    .
                RELEASE oe-ordl.
            END.
        END.
        FOR EACH ttItemLines NO-LOCK 
            WHERE ttItemLines.cTableType EQ "inv-line":
            FIND FIRST inv-line EXCLUSIVE-LOCK 
                WHERE ROWID(inv-line) EQ ttItemLines.riLine
                NO-ERROR.
            IF AVAILABLE inv-line THEN 
            DO: 
                ASSIGN
                    inv-line.price   = ttItemLines.dPrice
                    inv-line.pr-uom  = ttItemLines.cPriceUOM
                    inv-line.t-price = ttItemLines.dPriceTotal
                    .
                RELEASE inv-line.
            END.
        END.
    END. /*iplUpdateDB*/

END PROCEDURE.

PROCEDURE GetPriceMatrix:
    /*------------------------------------------------------------------------------
     Purpose: Returns a Rowid of a valid price matrix, given 3 key inputs
     Notes: Replaces oe/GetPriceMatrix.p - NOTE
     Prior versions of the price matrix captured 108 characters in the .i-no field and
     characters 101-108 where converted to the "Effective Date".  This should be converted
     to use the true .eff-date field on the price matrix.  The assumption of this program
     is that this conversion has taken place and that the price matrix .i-no field is 
     equivalent to the itemfg.i-no field.
     
     Syntax Example:
    RUN oe/PriceProcs.p PERSISTENT SET hdPriceProcs.
    RUN GetPriceMatrix in hdPriceProcs (cocode, oe-ordl.i-no, oe-ord.cust-no, oe-ord.ship-id,
                                        OUTPUT riMatrix, OUTPUT lFound, OUTPUT cMessage).
        
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.  /*Company*/
    DEFINE INPUT PARAMETER ipcFGItemID AS CHARACTER NO-UNDO.  /*FG Item ID*/
    DEFINE INPUT PARAMETER ipcCustID AS CHARACTER NO-UNDO.  /*Customer Scope of FG Item*/
    DEFINE INPUT PARAMETER ipcShipID AS CHARACTER NO-UNDO.  /*Ship to Scope of Customer  - optional*/
    DEFINE OUTPUT PARAMETER opriOePrmtx AS ROWID NO-UNDO.  /*Outputs the rowid for the price matrix that matches*/
    DEFINE OUTPUT PARAMETER oplMatchFound AS LOGICAL NO-UNDO.  /*Logical that can determine if find on rowid should be done*/
    DEFINE OUTPUT PARAMETER opcMatchDetail AS CHARACTER NO-UNDO.  /*Clarifies the match criteria or failure to match*/

    DEFINE BUFFER bf-itemfg   FOR itemfg.
    DEFINE BUFFER bf-cust     FOR cust.
    DEFINE BUFFER bf-oe-prmtx FOR oe-prmtx.


    RUN pSetBuffers(ipcCompany, ipcFGItemId, ipcCustID, BUFFER bf-itemfg, BUFFER bf-cust).
            
    /*Find match given buffers */  
    RUN pGetPriceMatrix(BUFFER bf-itemfg, BUFFER bf-cust, BUFFER bf-oe-prmtx, ipcShipID, OUTPUT oplMatchFound, OUTPUT opcMatchDetail).
    IF oplMatchFound AND AVAILABLE bf-oe-prmtx THEN 
        opriOePrmtx = ROWID(bf-oe-prmtx).

END PROCEDURE.

PROCEDURE GetPriceMatrixLevel:
    /*------------------------------------------------------------------------------
     Purpose: Returns a price level based on matrix information provided
     Notes:
         Syntax Example:
    RUN oe/PriceProcs.p PERSISTENT SET hdPriceProcs.
    RUN GetPriceMatrixLevel in hdPriceProcs (cocode, itemfg.i-no, cust.cust-no, shipto.ship-id,
                                             oe-ordl.qty,
                                             OUTPUT iLevel).
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.  /*Company*/
    DEFINE INPUT PARAMETER ipcFGItemID AS CHARACTER NO-UNDO.  /*FG Item ID*/
    DEFINE INPUT PARAMETER ipcCustID AS CHARACTER NO-UNDO.  /*Customer Scope of FG Item*/
    DEFINE INPUT PARAMETER ipcShipID AS CHARACTER NO-UNDO.  /*Ship to Scope of Customer  - optional*/
    DEFINE INPUT PARAMETER ipdQuantity AS DECIMAL NO-UNDO. /*to get price matrix appropriate for qty*/
    DEFINE OUTPUT PARAMETER opiLevel AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bf-oe-prmtx FOR oe-prmtx.
    DEFINE BUFFER bf-itemfg   FOR itemfg.
    DEFINE BUFFER bf-cust     FOR cust.
    
    DEFINE VARIABLE lMatrixFound AS LOGICAL.
    DEFINE VARIABLE lQtyMatch    AS LOGICAL.
    DEFINE VARIABLE cMessage     AS CHARACTER. 
    
    RUN pSetBuffers(ipcCompany, ipcFGItemId, ipcCustID, BUFFER bf-itemfg, BUFFER bf-cust).
    RUN pGetPriceMatrix(BUFFER bf-itemfg, BUFFER bf-cust, BUFFER bf-oe-prmtx, ipcShipID, OUTPUT lMatrixFound, OUTPUT cMessage).
    RUN pGetQtyMatchInfo(BUFFER bf-oe-prmtx, ipdQuantity, 1, OUTPUT opiLevel, OUTPUT lQtyMatch).

END PROCEDURE.

PROCEDURE GetPriceMatrixPrice:
    /*------------------------------------------------------------------------------
     Purpose: Returns a Price and Price UOM, given Item, Customer and ShipTo criteria.
     Also allows for override of a 
     Notes:
    Syntax Example:
    RUN oe/PriceProcs.p PERSISTENT SET hdPriceProcs.
    RUN GetPriceMatrixPrice in hdPriceProcs (cocode, oe-ordl.i-no, oe-ord.cust-no, oe-ord.ship-id,
                                             oe-ordl.qty, 0,
                                             OUTPUT lMatrixMatchFound, OUTPUT cMatrixMatchMessage,
                                             OUTPUT oe-ordl.price, OUTPUT oe-ordl.pr-uom, 
                                             OUTPUT lQtyMatchFound, OUTPUT lQtyWithinMatrixRange).
        
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.  /*Company*/
    DEFINE INPUT PARAMETER ipcFGItemID AS CHARACTER NO-UNDO.  /*FG Item ID*/
    DEFINE INPUT PARAMETER ipcCustID AS CHARACTER NO-UNDO.  /*Customer Scope of FG Item*/
    DEFINE INPUT PARAMETER ipcShipID AS CHARACTER NO-UNDO.  /*Ship to Scope of Customer  - optional*/
    DEFINE INPUT PARAMETER ipdQuantity AS DECIMAL NO-UNDO. /*to get price matrix appropriate for qty*/
    DEFINE INPUT PARAMETER ipiLevelOverride AS INTEGER NO-UNDO.  /*for use to get specific price level from matrix - set to 0 to determine based on qty*/

    /*Outputs*/
    DEFINE OUTPUT PARAMETER oplMatrixMatchFound AS LOGICAL NO-UNDO.  /*Logical that specifies if a matrix was found, at all*/
    DEFINE OUTPUT PARAMETER opcMatrixMatchDetail AS CHARACTER NO-UNDO.  /*Clarifies the match criteria or failure to match*/
    DEFINE INPUT-OUTPUT PARAMETER iopdPrice AS DECIMAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopcUom AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplQtyDistinctMatch AS LOGICAL NO-UNDO.  /*match on exact quantity*/
    DEFINE OUTPUT PARAMETER oplQtyWithinRange AS LOGICAL NO-UNDO. /*quantity within range*/

    /*main matrix buffer*/
    DEFINE BUFFER bf-oe-prmtx FOR oe-prmtx.
    DEFINE BUFFER bf-itemfg   FOR itemfg.
    DEFINE BUFFER bf-cust     FOR cust.

    DEFINE VARIABLE iLevel            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iLevelStart       AS INTEGER   NO-UNDO.
    /*for calculation of discount method*/
    DEFINE VARIABLE dItemSellPrice    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cItemSellPriceUOM AS CHARACTER NO-UNDO.
    
    
    RUN pSetBuffers(ipcCompany, ipcFGItemId, ipcCustID, BUFFER bf-itemfg, BUFFER bf-cust).
        
    /*use internal procedure to find the matching matrix*/
    RUN pGetPriceMatrix(BUFFER bf-itemfg, BUFFER bf-cust, BUFFER bf-oe-prmtx, ipcShipID, OUTPUT oplMatrixMatchFound, OUTPUT opcMatrixMatchDetail).
    
    IF NOT oplMatrixMatchFound OR  NOT AVAILABLE bf-oe-prmtx THEN RETURN.
    
    /*Set the default starting level to the customer specific starting level*/
    IF AVAILABLE bf-cust THEN 
        iLevelStart = MAXIMUM(1, bf-cust.cust-level).
    ELSE 
        iLevelStart = 1.
    
    IF AVAILABLE bf-itemfg THEN  /*Set the starting sell price and UOM if the matrix is discount method*/ 
        ASSIGN 
            dItemSellPrice    = bf-itemfg.sell-price
            cItemSellPriceUOM = bf-itemfg.sell-uom
            .       
             
    RUN pGetQtyMatchInfo(BUFFER bf-oe-prmtx, ipdQuantity, iLevelStart, OUTPUT iLevel, OUTPUT oplQtyDistinctMatch).
    IF iLevel GT 0 THEN 
        oplQtyWithinRange = YES.
    
    IF ipiLevelOverride NE 0 THEN 
    DO:
        IF iLevelStart LE ipiLevelOverride THEN
            iLevel = ipiLevelOverride.
        ELSE 
            iLevel = iLevelStart.
    END.
    IF oplQtyWithinRange THEN
        RUN pGetPriceAtLevel(BUFFER bf-oe-prmtx, iLevel, dItemSellPrice, cItemSellPriceUom, OUTPUT iopdPrice, OUTPUT iopcUom).
    ELSE 
        ASSIGN 
            oplMatrixMatchFound = NO
            opcMatrixMatchDetail = opcMatrixMatchDetail + " but price level " + STRING(iLevel) + " not valid."
            .

END PROCEDURE.

PROCEDURE GetPriceMatrixPriceSimple:
    /*------------------------------------------------------------------------------
        Purpose: Simpler version of GetPriceMatrixPrice with wrapper and defined variables
        Notes:
       Syntax Example:
       RUN oe/PriceProcs.p PERSISTENT SET hdPriceProcs.
       RUN GetPriceMatrixPriceSimple in hdPriceProcs (cocode, oe-ordl.i-no, oe-ord.cust-no,
                                                      oe-ordl.qty,
                                                       OUTPUT lMatrixMatchFound,
                                                       OUTPUT oe-ordl.price, OUTPUT oe-ordl.pr-uom).
           
       ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.  /*Company*/
    DEFINE INPUT PARAMETER ipcFGItemID AS CHARACTER NO-UNDO.  /*FG Item ID*/
    DEFINE INPUT PARAMETER ipcCustID AS CHARACTER NO-UNDO.  /*Customer Scope of FG Item*/
    DEFINE INPUT PARAMETER ipcShipID AS CHARACTER NO-UNDO.  /*ShipID */
    DEFINE INPUT PARAMETER ipdQuantity AS DECIMAL NO-UNDO. /*to get price matrix appropriate for qty*/
    
    /*Outputs*/
    DEFINE OUTPUT PARAMETER oplMatrixMatchFound AS LOGICAL NO-UNDO.  /*Logical that specifies if a matrix was found, at all*/
    DEFINE INPUT-OUTPUT PARAMETER iopdPrice AS DECIMAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopcUom AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cMessage              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lQtyMatchFound        AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lQtyWithinMatrixRange AS LOGICAL   NO-UNDO.
    
    RUN GetPriceMatrixPrice(ipcCompany, ipcFGITemID, ipcCustID, ipcShipID, ipdQuantity, 0,
        OUTPUT oplMatrixMatchFound, OUTPUT cMessage, 
        INPUT-OUTPUT iopdPrice, INPUT-OUTPUT iopcUOM, 
        OUTPUT lQtyMatchFound, OUTPUT lQtyWithinMatrixRange).

END PROCEDURE.

PROCEDURE GetPriceTotal:
    /*------------------------------------------------------------------------------
     Purpose: Given Price Line Inputs, calculate a total price
     Notes: Replaces oe/GetPriceTotal.p
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipdQuantity AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdPrice AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcPriceUOM AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiCaseCount AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipdDiscount AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdPriceTotal AS DECIMAL NO-UNDO.

    DEFINE VARIABLE cFGUomList AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-uom FOR uom.

    RUN sys/ref/uom-ea.p (OUTPUT cFGUomList).

    IF ipdQuantity EQ 0 THEN
        opdPriceTotal = 0.
    CASE ipcPriceUOM:
        WHEN "LOT" OR WHEN "L" THEN 
            opdPriceTotal = ipdPrice * IF ipdQuantity LT 0 THEN -1 ELSE 1.
        WHEN "CS" THEN 
            DO:
                IF ipiCaseCount EQ 0 THEN 
                    ipiCaseCount = 1.
                opdPriceTotal = ipdQuantity / ipiCaseCount * ipdPrice.
            END. 
        WHEN "C" THEN 
            opdPriceTotal = ipdQuantity / 100 * ipdPrice.
        WHEN "M" THEN            
            opdPriceTotal = ipdQuantity / 1000 * ipdPrice.
    END CASE.    
    IF opdPriceTotal EQ 0 THEN 
    DO:
        IF LOOKUP(ipcPriceUom, cFGUomList) GT 0 THEN
            opdPriceTotal = ipdQuantity * ipdPrice.

        ELSE 
        DO:
            FIND FIRST bf-uom
                WHERE bf-uom.uom  EQ ipcPriceUom
                AND bf-uom.mult NE 0
                NO-LOCK NO-ERROR.
            IF AVAIL bf-uom THEN
                opdPriceTotal = ipdQuantity / bf-uom.mult * ipdPrice.
            ELSE
                opdPriceTotal = ipdQuantity * ipdPrice.
        END.
    END.
    opdPriceTotal = opdPriceTotal - opdPriceTotal * ipdDiscount / 100.

END PROCEDURE.

PROCEDURE pAddLineTableItem PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Adds a line table Item to temp-table
     Notes: Agnostic of data type.
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-itemfg FOR itemfg. 
    DEFINE INPUT PARAMETER ipriLine AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER iplIsPrimary AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcFGItemID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustID AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER ipcShipID AS CHARACTER NO-UNDO.  
    DEFINE INPUT PARAMETER ipdQuantity AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdPrice AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcPriceUOM AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdDiscount AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcTableType AS CHARACTER NO-UNDO.
    
    FIND FIRST ttItemLines
        WHERE ttItemLines.riLine EQ ipriLine
        NO-ERROR.
    IF NOT AVAILABLE ttItemLines THEN 
    DO:
        CREATE ttItemLines.
        ASSIGN 
            ttItemLines.riLine       = ipriLine
            ttItemLines.lIsPrimary   = iplIsPrimary
            ttItemLines.cCompany     = ipbf-itemfg.company
            ttItemLines.cFGItemID    = ipcFGItemID
            ttItemLines.cCustID      = ipcCustID
            ttItemLines.cShipID      = ipcShipID
            ttItemLines.cFGItemClass = ipbf-itemfg.class
            ttItemLines.dQuantity    = ipdQuantity
            ttItemLines.dPrice       = ipdPrice
            ttItemLines.cPriceUOM    = ipcPriceUOM
            ttItemLines.iCaseCount   = ipbf-itemfg.case-count
            ttItemLines.dDiscount    = ipdDiscount
            ttItemLines.cTableType   = ipcTableType
            .
    END.

END PROCEDURE.

PROCEDURE pAddPriceHold PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Adds a price hold record, given subject parameters and settings 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriLine AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFGItemID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcShipID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdQuantity AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER iplQuantityMatch AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplQuantityInRange AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplEffectiveDateAge AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER ipiEffectiveDateAgeDays AS INTEGER NO-UNDO.

    DEFINE BUFFER bf-itemfg   FOR itemfg.
    DEFINE BUFFER bf-cust     FOR cust.
    DEFINE BUFFER bf-oe-prmtx FOR oe-prmtx.

    CREATE ttPriceHold.
    ASSIGN 
        ttPriceHold.riLine    = ipriLine
        ttPriceHold.cFGItemID = ipcFGItemID
        ttPriceHold.cCustID   = ipcCustID
        ttPriceHold.cShipID   = ipcShipID
        ttPriceHold.dQuantity = ipdQuantity
        .

                        
    RUN pSetBuffers(ipcCompany, ipcFGItemID, ipcCustID, BUFFER bf-itemfg, BUFFER bf-cust).            
    /*use internal procedure to find the matching matrix*/
    IF AVAIL bf-itemfg AND bf-itemfg.i-code EQ "S" THEN  
        RUN pGetPriceMatrix(BUFFER bf-itemfg, BUFFER bf-cust, BUFFER bf-oe-prmtx, ipcShipID, 
            OUTPUT ttPriceHold.lMatrixMatch, OUTPUT ttPriceHold.cMatrixMatch).
    ELSE 
    DO:
        ASSIGN 
            ttPriceHold.lPriceHold       = NO
            ttPriceHold.cPriceHoldDetail = ttPriceHold.cFGItemID + " ignored since it is Custom Box and not Stock"
            ttPriceHold.cPriceHoldReason = "Not a Stock item"
            .
        RETURN.
    END.
    IF NOT ttPriceHold.lMatrixMatch OR  NOT AVAILABLE bf-oe-prmtx THEN 
    DO:
        ASSIGN 
            ttPriceHold.lPriceHold       = YES
            ttPriceHold.cPriceHoldDetail = ttPriceHold.cMatrixMatch
            ttPriceHold.cPriceHoldReason = "No matrix found".
        RETURN.           
    END.
    IF AVAILABLE bf-oe-prmtx THEN
        ASSIGN 
            ttPriceHold.dtEffectiveDate = bf-oe-prmtx.eff-date.
        
    /*Test Effective Date Age if activated*/
    IF NOT ttPriceHold.lPriceHold AND iplEffectiveDateAge AND (TODAY - bf-oe-prmtx.eff-date) GT ipiEffectiveDateAgeDays THEN 
    DO:
        ASSIGN 
            ttPriceHold.lPriceHold           = YES
            ttPriceHold.lEffectiveDateTooOld = YES 
            ttPriceHold.cPriceHoldDetail     = "Effective date of " + STRING(bf-oe-prmtx.eff-date,"99/99/9999") + " older than " + STRING(ipiEffectiveDateAgeDays) + " days"
            ttPriceHold.cPriceHoldReason     = "Eff. date too old".           .
    END. 
    /*Test Price Level if activated*/
    IF NOT ttPriceHold.lPriceHold THEN 
        RUN pGetQtyMatchInfo(BUFFER bf-oe-prmtx, ipdQuantity, 0, OUTPUT ttPriceHold.iQuantityLevel, OUTPUT ttPriceHold.lQuantityMatch).
    IF NOT ttPriceHold.lPriceHold AND iplQuantityMatch AND NOT ttPriceHold.lQuantityMatch THEN 
    DO:
        ASSIGN 
            ttPriceHold.lPriceHold       = YES
            ttPriceHold.cPriceHoldDetail = "No distinct level found for quantity of " + STRING(ipdQuantity)  
            ttPriceHold.cPriceHoldReason = "Quantity not matched".
    END.
    IF NOT ttPriceHold.lPriceHold AND iplQuantityInRange AND ttPriceHold.iQuantityLevel EQ 0 THEN 
    DO:
        ASSIGN 
            ttPriceHold.lPriceHold          = YES
            ttPriceHold.lQuantityOutOfRange = YES 
            ttPriceHold.cPriceHoldDetail    = "Quantity of " + STRING(ipdQuantity) + " out of range of price matrix levels"
            ttPriceHold.cPriceHoldReason    = "Quantity not in range".
    END.     
        

END PROCEDURE.

PROCEDURE pBuildClassQuantityTable PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a RowID this will build a temp-table for FG Item Classes 
     and quantities
     Notes: Works for RI of Oe-ordl, inv-line or ar-invl
    ------------------------------------------------------------------------------*/

    EMPTY TEMP-TABLE ttFGItemClass.
    FOR EACH ttItemLines NO-LOCK:
        FIND FIRST ttFGItemClass EXCLUSIVE-LOCK  
            WHERE ttFGItemClass.cClassID EQ ttItemLines.cFGItemClass
            NO-ERROR.
        IF NOT AVAILABLE ttFGItemClass THEN 
        DO: 
            CREATE ttFGItemClass.
            ASSIGN 
                ttFGItemClass.cClassID = ttItemLines.cFGItemClass.
        END.
        ttFGItemClass.dClassQuantity = ttFGItemClass.dClassQuantity + ttItemLines.dQuantity.    
    END.    

END PROCEDURE.

PROCEDURE pBuildLineTable PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Builds the agnostic temp-table for processing based on Row ID
     Notes: in addition to building temp-table, also returns itemfg and cust buffers
     and "type"
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriLine AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER ipcFGItemID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcShipID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdQuantity AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcType AS CHARACTER NO-UNDO.  /*oe-ordl, ar-invl, inv-line*/ 
    DEFINE OUTPUT PARAMETER oplReprice AS LOGICAL NO-UNDO.

    EMPTY TEMP-TABLE ttItemLines.
    FIND FIRST oe-ordl NO-LOCK 
        WHERE ROWID(oe-ordl) EQ ipriLine
        NO-ERROR.
    FIND FIRST inv-line NO-LOCK
        WHERE ROWID(inv-line) EQ ipriLine
        NO-ERROR. 
    IF AVAILABLE oe-ordl THEN 
    DO:
        {oe/PriceProcsLineBuilder.i &HeaderTable="oe-ord" &LineTable="oe-ordl" &LineQuantity="qty"}
    END.
    ELSE IF AVAILABLE inv-line THEN 
        DO:
            {oe/PriceProcsLineBuilder.i &HeaderTable="inv-head" &LineTable="inv-line" &LineQuantity="inv-qty"}
        END.     

END PROCEDURE.

PROCEDURE pGetLastPrice PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Returns the "Last Price" logic - updates the parameter values only successful
     Agnostic to the rowid passed in (could be oe-ordl, inv-line, ar-invl)
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriLine AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFGItemID AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER opdPrice AS DECIMAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER opcPriceUOM AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-oe-ordl  FOR oe-ordl.
    DEFINE BUFFER bf-oe-ord   FOR oe-ord.
    DEFINE BUFFER bf-ar-invl  FOR ar-invl.
    DEFINE BUFFER bf-ar-inv   FOR ar-inv.
    DEFINE BUFFER bf-inv-line FOR inv-line.
    DEFINE BUFFER bf-inv-head FOR inv-head.
    
    DEFINE VARIABLE dtLast AS DATE NO-UNDO.

    dtLast = 01/01/0001.
    
    /*find last order line for the item*/
    FOR EACH bf-oe-ordl
        WHERE bf-oe-ordl.company EQ ipcCompany
        AND bf-oe-ordl.i-no    EQ ipcFGItemID
        AND ROWID(bf-oe-ordl)  NE ipriLine
        NO-LOCK,
        FIRST bf-oe-ord
        WHERE bf-oe-ord.company  EQ bf-oe-ordl.company
        AND bf-oe-ord.ord-no   EQ bf-oe-ordl.ord-no
        AND bf-oe-ord.cust-no  EQ bf-oe-ordl.cust-no
        AND bf-oe-ord.ord-date GT dtLast
        NO-LOCK
        BY bf-oe-ord.ord-date DESCENDING:
        LEAVE.
    END.
    IF AVAILABLE bf-oe-ordl THEN
        ASSIGN
            dtLast      = bf-oe-ord.ord-date
            opdPrice    = bf-oe-ordl.price
            opcPriceUOM = bf-oe-ordl.pr-uom.

    FOR EACH bf-ar-invl
        WHERE bf-ar-invl.company EQ ipcCompany
        AND bf-ar-invl.i-no    EQ ipcFGItemID
        AND ROWID(bf-ar-invl)  NE ipriLine
        NO-LOCK,
        FIRST bf-ar-inv
        WHERE bf-ar-inv.x-no     EQ bf-ar-invl.x-no
        AND bf-ar-inv.cust-no  EQ bf-ar-invl.cust-no
        AND bf-ar-inv.inv-date GT dtLast
        NO-LOCK
        BY bf-ar-inv.inv-date:
        LEAVE.
    END.
    IF AVAILABLE bf-ar-invl THEN
        ASSIGN
            dtLast      = bf-ar-inv.inv-date
            opdPrice    = bf-ar-invl.unit-pr
            opcPriceUOM = bf-ar-invl.pr-qty-uom.

    FOR EACH bf-inv-line
        WHERE bf-inv-line.company EQ ipcCompany
        AND bf-inv-line.i-no    EQ ipcFGItemID
        AND ROWID(bf-inv-line)    NE ipriLine
        NO-LOCK,
        FIRST bf-inv-head
        WHERE bf-inv-head.r-no     EQ bf-inv-line.r-no
        AND bf-inv-head.cust-no  EQ bf-inv-line.cust-no
        AND bf-inv-head.inv-date GT dtLast
        NO-LOCK
        BY bf-inv-head.inv-date:
        LEAVE.
    END.
    IF AVAILABLE bf-inv-line THEN
        ASSIGN
            dtLast      = bf-inv-head.inv-date
            opdPrice    = bf-inv-line.price
            opcPriceUOM = bf-inv-line.pr-uom.

END PROCEDURE.

PROCEDURE pGetOEPriceMatrixCheckSettings PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    /*------------------------------------------------------------------------------
     Purpose: Returns Price hold Criteria Settings
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplCheckMatrix AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplAllowMax AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplBlockEntry AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE lFound  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO. 
   
    
    RUN sys/ref/nk1look.p (ipcCompany, 
        "OEPriceMatrixCheck",
        "L" /* Logical */,
        NO /* check by cust */,
        YES /* use cust not vendor */,
        "" /* cust */,
        "" /* ship-to*/,
        OUTPUT cReturn, 
        OUTPUT lFound).
    oplCheckMatrix = (lFound AND cReturn EQ "YES").
    IF oplCheckMatrix THEN 
    DO:
        RUN sys/ref/nk1look.p (ipcCompany,
            "OEPriceMatrixCheck", 
            "I" /* Logical */,
            NO /* check by cust */,
            YES /* use cust not vendor */,
            "" /* cust */,
            "" /* ship-to*/,
            OUTPUT cReturn,
            OUTPUT lFound).
        oplAllowMax = (lFound AND INT(cReturn) EQ 1).
        RUN sys/ref/nk1look.p (ipcCompany, 
            "OEPriceMatrixCheck", 
            "C" /* Logical */,
            NO /* check by cust */,
            YES /* use cust not vendor */,
            "" /* cust */,
            "" /* ship-to*/,
            OUTPUT cReturn,
            OUTPUT lFound).
        oplBlockEntry =  (lFound AND cReturn EQ "Block Entry").
    END.
END PROCEDURE.

PROCEDURE pGetPriceAtLevel PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Accepts a matrix buffer and level (and starting sell price or uom if discount method)
     Notes: 
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-oe-prmtx FOR oe-prmtx.
    DEFINE INPUT PARAMETER ipiLevel AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipdItemSellPrice AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemSellPriceUom AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdPrice AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcUom AS CHARACTER NO-UNDO.

    IF ipiLevel GT 0 AND ipiLevel LE EXTENT(ipbf-oe-prmtx.price) THEN DO:  /*31620 - protect against a level request out of range of the array*/
           
        IF ipbf-oe-prmtx.meth THEN
            ASSIGN 
                opdPrice = ipbf-oe-prmtx.price[ipiLevel]
                opcUom   = ipbf-oe-prmtx.uom[ipiLevel].
        ELSE /*discount method - discount off of item price*/
            ASSIGN 
                opdPrice = ipdItemSellPrice - 
                ROUND((ipdItemSellPrice * ipbf-oe-prmtx.discount[ipiLevel]) / 100, 2)
                opcUom   = ipcItemSellPriceUom.
            
    END.

END PROCEDURE.

PROCEDURE pGetPriceMatrix PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Private Buffer parameter based call to Get the Price Matrix
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-itemfg   FOR itemfg.
    DEFINE PARAMETER BUFFER ipbf-cust     FOR cust.
    DEFINE PARAMETER BUFFER opbf-oe-prmtx FOR oe-prmtx.
    DEFINE INPUT PARAMETER ipcShipID AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplMatchFound AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMatchDetail AS CHARACTER NO-UNDO.
 
    DEFINE BUFFER bf-shipto FOR shipto.
  
    DEFINE VARIABLE cMsgItemFG     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMsgItemProcat AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMsgCustID     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMsgCustType   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMsgShipID     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMsgBlankInd   AS CHARACTER NO-UNDO INIT "[blank]".
    
    IF NOT AVAILABLE ipbf-itemfg THEN 
    DO:
        ASSIGN 
            opcMatchDetail = "Invalid FG Item ID"
            oplMatchFound  = NO
            .
        RETURN. 
    END. 
    IF NOT AVAILABLE ipbf-cust THEN 
    DO:
        ASSIGN 
            opcMatchDetail = "Invalid Cust ID"
            oplMatchFound  = NO 
            .
        RETURN.
    END.
    IF ipcShipID NE "" THEN 
    DO:
        FIND FIRST bf-shipto NO-LOCK 
            WHERE bf-shipto.company EQ ipbf-cust.company
            AND bf-shipto.cust-no EQ ipbf-cust.cust-no
            AND bf-shipto.ship-id EQ ipcShipID
            NO-ERROR.
        IF NOT AVAILABLE bf-shipto THEN 
        DO:
            ASSIGN 
                opcMatchDetail = "Ship To ID provided is not valid for Cust " + ipbf-cust.cust-no
                oplMatchFound  = NO 
                .
            RETURN.
        END.
    END.
    IF ipbf-itemfg.i-code NE "S" THEN 
    DO:
        ASSIGN 
            opcMatchDetail = "This FG item is configured as a non-inventoried (Not stocked) item"
            oplMatchFound  = NO 
            .
        RETURN.
    END.
    /*Find match */  
    FOR EACH opbf-oe-prmtx NO-LOCK 
        WHERE opbf-oe-prmtx.company EQ ipbf-itemfg.company
        /*Match on item # or match matrix with blank item #*/
        AND (opbf-oe-prmtx.i-no EQ ipbf-itemfg.i-no OR opbf-oe-prmtx.i-no EQ "" )
        /*Match on cust-no or match matrix with blank cust-no*/
        AND (opbf-oe-prmtx.cust-no EQ ipbf-cust.cust-no OR opbf-oe-prmtx.cust-no EQ "")
        /*Match on customer type or match on blank customer type - disregard this criteria if the matrix has a customer number at which
        point only the customer number match matters.  Cust Type only applicable when customer number is blank.
        This allows for customer type to change in AF1 without having to update the Price Matrix type for that customer.*/
        AND (opbf-oe-prmtx.custype EQ ipbf-cust.type OR opbf-oe-prmtx.custype EQ "" OR opbf-oe-prmtx.cust-no NE "")
        /*Match on product category or match on blank product category - disregard this criteria if the matrix has a FG Item # at which
        point only the FG Item # match matters.  Product Category only applicable when customer number is blank.
        This allows for Product Category to change in IF1 without having to update the Price Matrix for that item.*/
        AND (opbf-oe-prmtx.procat EQ ipbf-itemfg.procat OR opbf-oe-prmtx.procat EQ "" OR opbf-oe-prmtx.i-no NE "")
        /*Match on ship ID or match matrix with blank ship ID*/
        AND (opbf-oe-prmtx.custShipID EQ ipcShipID OR opbf-oe-prmtx.custShipID EQ "")
        /*Must be effecitve*/
        AND (opbf-oe-prmtx.eff-date LE TODAY)
        /*must not be expired*/
        AND (opbf-oe-prmtx.exp-date GE TODAY OR opbf-oe-prmtx.exp-date EQ ? OR opbf-oe-prmtx.exp-date EQ 01/01/0001)
        /* Can't be all blank */
        AND NOT (opbf-oe-prmtx.cust-no EQ "" AND opbf-oe-prmtx.i-no EQ "" AND opbf-oe-prmtx.procat EQ "" AND opbf-oe-prmtx.custype EQ "" 
        AND opbf-oe-prmtx.custShipID EQ "")
    /*Sort the resulting data set so that actual matches take priority over blank matches*/
        BY opbf-oe-prmtx.i-no DESCENDING
        BY opbf-oe-prmtx.cust-no DESCENDING 
        BY opbf-oe-prmtx.procat DESCENDING 
        BY opbf-oe-prmtx.custype DESCENDING 
        BY opbf-oe-prmtx.custShipID DESCENDING
        BY opbf-oe-prmtx.eff-date DESCENDING 
        :
        LEAVE.  /*After first/best match, leave*/
    END.

    /*Initialize return message for match*/
    ASSIGN 
        opcMatchDetail = "Match "
        cMsgItemFG     = "FGItemID=" + ipbf-itemfg.i-no
        cMsgItemProcat = "FGProdCat=" + ipbf-itemfg.procat
        cMsgCustID     = "CustID=" + ipbf-cust.cust-no
        cMsgCustType   = "CustType=" + ipbf-cust.type
        cMsgShipID     = "ShipID=" + ipcShipID
        .   
        
    /*Determine if match found from query and use matrix result to fill message criteria*/
    IF AVAILABLE opbf-oe-prmtx THEN 
    DO:
        ASSIGN 
            oplMatchFound = YES
            .
        ASSIGN 
            opcMatchDetail = "Match "
            cMsgItemFG     = "FGItemID="
            cMsgItemProcat = "FGProdCat="
            cMsgCustID     = "CustID="
            cMsgCustType   = "CustType="
            cMsgShipID     = "ShipID="
            cMsgCustID     = IF opbf-oe-prmtx.cust-no EQ "" THEN cMsgCustID + cMsgBlankInd ELSE cMsgCustID + opbf-oe-prmtx.cust-no
            cMsgItemFG     = IF opbf-oe-prmtx.i-no EQ "" THEN cMsgItemFG + cMsgBlankInd ELSE cMsgItemFG + opbf-oe-prmtx.i-no
            cMsgItemProcat = IF opbf-oe-prmtx.procat EQ "" THEN cMsgItemProcat + cMsgBlankInd ELSE cMsgItemProcat + opbf-oe-prmtx.procat
            cMsgCustType   = IF opbf-oe-prmtx.custype EQ "" THEN cMsgCustType + cMsgBlankInd ELSE cMsgCustType + opbf-oe-prmtx.custype 
            cMsgShipID     = IF opbf-oe-prmtx.custShipID EQ "" THEN cMsgShipID + cMsgBlankInd ELSE cMsgShipID + opbf-oe-prmtx.custShipID
            .
    END.    
    ELSE 
        ASSIGN 
            oplMatchFound  = NO
            opcMatchDetail = opcMatchDetail + "not " 
            .
 
    /*Build final return message*/
    opcMatchDetail = opcMatchDetail + "found: " + cMsgItemFG + " " + cMsgItemProcat + " " + cMsgCustID + " " + cMsgCustType + " " + cMsgShipID.


END PROCEDURE.

PROCEDURE pGetQtyMatchInfo PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a matrix buffer and a quantity target, determine:
         1. Price Level
         2. Price at Price Level
         3. Uom at Price Level
         4. If Quantity has a distinct match
         5. If Quantity is within the range of quantities listed
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-oe-prmtx FOR oe-prmtx.
    DEFINE INPUT PARAMETER ipdQuantityTarget AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipiLevelStart AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiQtyLevel AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplQtyDistinctMatch AS LOGICAL NO-UNDO.

    DEFINE VARIABLE iLevel AS INTEGER NO-UNDO.

    ASSIGN 
        opiQtyLevel         = 0
        oplQtyDistinctMatch = NO 
        ipiLevelStart       = IF ipiLevelStart EQ 0 THEN 1 ELSE ipiLevelStart
        .
       IF NOT AVAIL ipbf-oe-prmtx THEN RETURN .
 
    /*process matrix array completely, one time*/
    DO iLevel = ipiLevelStart TO EXTENT(ipbf-oe-prmtx.qty): /* IF customer has higher starting level set otherwise start with 1st level*/
        IF ipdQuantityTarget LE ipbf-oe-prmtx.qty[iLevel] THEN /*As soon as a qty level is found, greater than qty, all set*/
        DO:
            IF ipdQuantityTarget EQ ipbf-oe-prmtx.qty[iLevel] AND ipbf-oe-prmtx.qty[iLevel] NE 0 THEN 
                oplQtyDistinctMatch = YES.
            IF opiQtyLevel = 0 THEN 
                opiQtyLevel = iLevel.
        END. /*Qty LE oe-prmtx qty*/
    END.

END PROCEDURE.

PROCEDURE pSetBuffers PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Sets Buffers for FG Item and Customers
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFGItemID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustID AS CHARACTER NO-UNDO.
    DEFINE PARAMETER BUFFER opbf-itemfg FOR itemfg.
    DEFINE PARAMETER BUFFER opbf-cust   FOR cust.

    FIND FIRST opbf-itemfg NO-LOCK 
        WHERE opbf-itemfg.company EQ ipcCompany
        AND opbf-itemfg.i-no EQ ipcFGItemID
        NO-ERROR.
     
    FIND FIRST opbf-cust NO-LOCK 
        WHERE opbf-cust.company EQ ipcCompany
        AND opbf-cust.cust-no EQ ipcCustID
        NO-ERROR.
    
END PROCEDURE.


/* ************************  Function Implementations ***************** */

FUNCTION fCheckPriceHold RETURNS LOGICAL PRIVATE
    ( ipcCompany AS CHARACTER ):
    /*------------------------------------------------------------------------------
     Purpose: Checks the logical value of OEPriceHold NK1
     Notes: 
    ------------------------------------------------------------------------------*/	
    DEFINE VARIABLE lCheckPriceHold AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lFound          AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cReturn         AS CHARACTER NO-UNDO. 
    
    RUN sys/ref/nk1look.p (ipcCompany,
        "OEPriceHold",
        "L",
        NO,
        NO,
        "",
        "",
        OUTPUT cReturn,
        OUTPUT lFound).

    lCheckPriceHold = lFound AND cReturn EQ "YES".
    RETURN lCheckPriceHold.
		
END FUNCTION.

FUNCTION fUseLastPrice RETURNS LOGICAL PRIVATE
    ( ipcCompany AS CHARACTER ):
    /*------------------------------------------------------------------------------
     Purpose: Returns whether LastPrice is option set for NK1 SELLPRIC 
     Notes:
    ------------------------------------------------------------------------------*/	
    DEFINE VARIABLE lUseLastPrice AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lFound        AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cReturn       AS CHARACTER NO-UNDO. 
    
    RUN sys/ref/nk1look.p (ipcCompany,
        "SellPric",
        "C",
        NO,
        NO,
        "",
        "",
        OUTPUT cReturn,
        OUTPUT lFound).

    lUseLastPrice = lFound AND cReturn EQ "LastPric".
    RETURN lUseLastPrice.
	
END FUNCTION.

PROCEDURE pGetPriceHoldCriteria PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Returns Price hold Criteria Settings
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplPriceCheck AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplQtyInRange AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplQtyMatch AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplEffectiveDateAge AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opiEffectiveDateAgeDays AS INTEGER NO-UNDO.

    DEFINE VARIABLE lFound    AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cCriteria AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE cAge      AS CHARACTER NO-UNDO.
    
    RUN sys/ref/nk1look.p (ipcCompany,
        "OEPriceHold",
        "C",
        NO,
        NO,
        "",
        "",
        OUTPUT cCriteria,
        OUTPUT lFound).

    IF lFound AND cCriteria NE "" THEN 
    DO: 
        ASSIGN 
            oplPriceCheck       = YES
            oplQtyInRange       = LOOKUP("QtyInRange",cCriteria) GT 0
            oplQtyMatch         = LOOKUP("QtyMatch",cCriteria) GT 0
            oplEffectiveDateAge = LOOKUP("EffDateAge",cCriteria) GT 0
            .
    END.
    IF oplEffectiveDateAge THEN 
    DO:
        RUN sys/ref/nk1look.p (ipcCompany,
            "OEPriceHold",
            "I",
            NO,
            NO,
            "",
            "",
            OUTPUT cAge,
            OUTPUT lFound).
        IF lFound AND cAge NE "" THEN 
            opiEffectiveDateAgeDays = INTEGER(cAge).
        IF opiEffectiveDateAgeDays EQ 0 THEN 
            opiEffectiveDateAgeDays = 365.
    END.
END PROCEDURE.

