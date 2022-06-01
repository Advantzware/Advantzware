DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcBeginCustomer AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcEndCustomer AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcBeginRelDate AS DATE NO-UNDO.
DEFINE INPUT PARAMETER ipcEndRelDate AS DATE NO-UNDO.
DEFINE INPUT PARAMETER ipcBeginFGItem AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcEndFGItem AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iplZeroOrderLine AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER ipExecute AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER ipFilePath AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE ttOrderLineChange
    FIELD customerID    AS CHARACTER
    FIELD shipID        AS CHARACTER
    FIELD orderNo       AS INTEGER
    FIELD orderline     AS INTEGER
    FIELD itemCode      AS CHARACTER
    FIELD orderQty      AS DECIMAL
    FIELD shippedQty    AS DECIMAL
    FIELD qtyUOM        AS CHARACTER
    FIELD priceOld      AS DECIMAL
    FIELD priceNew      AS DECIMAL
    FIELD priceUOMOld   AS CHARACTER
    FIELD priceUOMNew   AS CHARACTER
    FIELD totalPriceOld AS DECIMAL
    FIELD totalPriceNew AS DECIMAL
    FIELD orderStatus   AS CHARACTER
    FIELD orderDate     AS DATE
    FIELD pctChange     AS DECIMAL
    FIELD note          AS CHARACTER
    .
DEFINE VARIABLE i        AS INTEGER   NO-UNDO.
DEFINE VARIABLE j        AS INTEGER   NO-UNDO.
DEFINE VARIABLE hdPrice  AS HANDLE    NO-UNDO.
DEFINE VARIABLE hdOutput AS HANDLE    NO-UNDO.
DEFINE VARIABLE lError   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
RUN oe\PriceProcs.p PERSISTENT SET hdPrice.
RUN system\OutputProcs.p PERSISTENT SET hdOutput.
DEFINE VARIABLE dNewPrice    AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dPriceChange AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cNewPriceUOM AS CHARACTER NO-UNDO.
DEFINE VARIABLE cOldPriceUOM AS CHARACTER NO-UNDO.
DEFINE VARIABLE lFound       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lFoundInv    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE dNewPriceInv AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cNewPriceUOMInv AS CHARACTER NO-UNDO.
DEFINE VARIABLE dOldPrice    AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dOldPriceTot AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cReturnValue AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lPriceHold   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cPriceHoldMessage AS CHARACTER NO-UNDO.
DEFINE VARIABLE lQuotePriceMatrix AS LOGICAL   NO-UNDO.


RUN sys/ref/nk1look.p (INPUT ipcCompany, "QuotePriceMatrix", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cReturnValue, OUTPUT lRecFound).
IF lRecFound THEN
    lQuotePriceMatrix = LOGICAL(cReturnValue) NO-ERROR. 
    

MAIN-LOOP:
FOR EACH oe-ordl NO-LOCK
    WHERE oe-ordl.company EQ ipcCompany
    AND oe-ordl.stat NE "C"
    AND oe-ordl.cust-no GE ipcBeginCustomer
    AND oe-ordl.cust-no LE ipcEndCustomer
    AND oe-ordl.i-no GE ipcBeginFGItem
    AND oe-ordl.i-no LE ipcEndFGItem
    ,
    FIRST oe-ord NO-LOCK
    WHERE oe-ord.company EQ oe-ordl.company
    AND oe-ord.ord-no EQ oe-ordl.ord-no      
    :

    FIND FIRST oe-rel NO-LOCK
        WHERE oe-rel.company EQ oe-ordl.company
        AND oe-rel.ord-no EQ oe-ordl.ord-no
        AND oe-rel.i-no EQ oe-ordl.i-no
        AND oe-rel.LINE EQ oe-ordl.LINE NO-ERROR.
 
    IF AVAILABLE oe-rel THEN       
        FIND FIRST oe-rell NO-LOCK
            WHERE oe-rell.company  EQ oe-rel.company
            AND oe-rell.ord-no   EQ oe-rel.ord-no
            AND oe-rell.i-no     EQ oe-rel.i-no
            AND oe-rell.line     EQ oe-rel.line
            AND oe-rell.rel-no   EQ oe-rel.rel-no
            AND oe-rell.b-ord-no EQ oe-rel.b-ord-no
            AND oe-rell.po-no    EQ oe-rel.po-no
            USE-INDEX ord-no NO-ERROR.
    ELSE RELEASE oe-rell.
 
    IF AVAILABLE oe-rell THEN
    DO:
        FIND FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no NO-LOCK NO-ERROR.
        IF AVAILABLE oe-relh AND oe-relh.rel-date GE ipcBeginRelDate AND oe-relh.rel-date LE ipcEndRelDate THEN.
        ELSE NEXT MAIN-LOOP.
    END.
    ELSE IF AVAILABLE oe-rel THEN
        DO:
            IF oe-rel.rel-date GE ipcBeginRelDate AND oe-rel.rel-date LE ipcEndRelDate THEN.
            ELSE NEXT MAIN-LOOP.
        END.
    ELSE NEXT MAIN-LOOP.    

    ASSIGN 
        i            = i + 1
        dNewPrice    = 0
        dPriceChange = 0
        dOldPrice    = oe-ordl.price
        cOldPriceUOM = oe-ordl.pr-uom
        dOldPriceTot = oe-ordl.t-price.
    
    RUN Price_CalculateLinePrice IN hdPrice (ROWID(oe-ordl),oe-ordl.i-no,oe-ordl.cust-no,oe-ordl.ship-id,0,ipExecute,OUTPUT lFound, INPUT-OUTPUT dNewPrice, INPUT-OUTPUT cNewPriceUOM).
    
    FIND FIRST inv-line NO-LOCK 
         WHERE inv-line.company EQ oe-ordl.company
         AND inv-line.ord-no EQ oe-ordl.ord-no
         AND inv-line.i-no EQ oe-ordl.i-no 
         AND inv-line.LINE EQ oe-ordl.LINE NO-ERROR.
         
    IF AVAILABLE inv-line AND ipExecute THEN     
    RUN Price_CalculateLinePrice IN hdPrice (ROWID(inv-line),oe-ordl.i-no,oe-ordl.cust-no,oe-ordl.ship-id,0,ipExecute,OUTPUT lFoundInv, INPUT-OUTPUT dNewPriceInv, INPUT-OUTPUT cNewPriceUOMInv).
       
    IF iplZeroOrderLine AND NOT lFound THEN
       dNewPrice = 0.
    ELSE IF NOT lFound THEN 
    dNewPrice = dOldPrice.
    
    dPriceChange = (dNewPrice / dOldPrice - 1 ) * 100.
    IF (dOldPrice NE dNewPrice OR NOT ipExecute) OR ( iplZeroOrderLine AND NOT lFound ) OR (NOT lFound AND lQuotePriceMatrix)         
        THEN 
    DO:
        j = j + 1.
        CREATE ttOrderLineChange.
        ASSIGN 
            ttOrderLineChange.customerID    = oe-ordl.cust-no
            ttOrderLineChange.shipID        = oe-ordl.ship-id
            ttOrderLineChange.orderNo       = oe-ordl.ord-no
            ttOrderLineChange.orderLine     = oe-ordl.line
            ttOrderLineChange.itemCode      = oe-ordl.i-no
            ttOrderLineChange.orderQty      = oe-ordl.qty
            ttOrderLineChange.shippedQty    = oe-ordl.ship-qty
            ttOrderLineChange.qtyUOM        = "EA"
            ttOrderLineChange.priceOld      = dOldPrice
            ttOrderLineChange.priceNew      = dNewPrice
            ttOrderLineChange.priceUOMOld   = cOldPriceUOM
            ttOrderLineChange.priceUOMNew   = cNewPriceUOM
            ttOrderLineChange.totalPriceOld = dOldPriceTot            
            ttOrderLineChange.orderStatus   = oe-ordl.stat
            ttOrderLineChange.orderDate     = oe-ord.ord-date
            ttOrderLineChange.pctChange     = (dNewPrice / oe-ordl.price - 1)
            ttOrderLineChange.note          = IF ttOrderLineChange.pctChange GT .07 AND ttOrderLineChange.pctChange LT .11 THEN "OK - About 9%" ELSE "Review".
        .
         RUN Conv_CalcTotalPrice (
            oe-ordl.company,
            oe-ordl.i-no,
            oe-ordl.qty,
            dNewPrice,
            cNewPriceUOM,
            oe-ordl.disc,
            oe-ordl.cas-cnt,
            OUTPUT ttOrderLineChange.totalPriceNew).

    END.
    IF NOT lFound AND ipExecute THEN
    DO:
        FIND CURRENT oe-ordl EXCLUSIVE-LOCK NO-ERROR.
        IF iplZeroOrderLine THEN do:           
            oe-ordl.price = 0.
            oe-ordl.t-price = 0.              
            IF AVAILABLE ttOrderLineChange THEN
            ttOrderLineChange.note = ttOrderLineChange.note + ", Price Set to zero" .
        END.
        ELSE do:      
            oe-ordl.price = dNewPrice.
            oe-ordl.t-price = ttOrderLineChange.totalPriceNew.
        END.
        FIND CURRENT oe-ordl NO-LOCK NO-ERROR.
        
        IF lQuotePriceMatrix OR iplZeroOrderLine THEN do: 
            IF NOT oe-ord.priceHold THEN
            DO:
                FIND CURRENT oe-ord EXCLUSIVE-LOCK NO-ERROR.
                 ASSIGN
                     oe-ord.priceHold = YES
                     oe-ord.priceHoldReason = "No matrix found".
                FIND CURRENT oe-ord NO-LOCK NO-ERROR.
                IF AVAILABLE ttOrderLineChange THEN
                 ttOrderLineChange.note = ttOrderLineChange.note + ", Order put on Price Hold".
            END.  
            ELSE DO:
                IF AVAILABLE ttOrderLineChange THEN
                    ttOrderLineChange.note = ttOrderLineChange.note + ", Order already on Price Hold".
            END.
        END.    
    END.
END.
RUN Output_TempTableToCSV IN hdOutput (TEMP-TABLE ttOrderLineChange:HANDLE, ipFilePath ,YES,YES, OUTPUT lError, OUTPUT cMessage).
