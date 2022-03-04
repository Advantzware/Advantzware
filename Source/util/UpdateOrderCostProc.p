DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER ipcBeginOrdDate AS DATE NO-UNDO.
DEFINE INPUT PARAMETER ipcEndOrdDate AS DATE NO-UNDO.
DEFINE INPUT PARAMETER ipiBeginOrder AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER ipiEndOrder AS INTEGER NO-UNDO. 
DEFINE INPUT PARAMETER ipcOrderStatus AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipExecute AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER ipFilePath AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE ttOrderLineChange
    FIELD customerID      AS CHARACTER
    FIELD shipID          AS CHARACTER
    FIELD orderNo         AS INTEGER
    FIELD orderline       AS INTEGER
    FIELD itemCode        AS CHARACTER
    FIELD orderDate       AS DATE
    FIELD lineCostM       AS DECIMAL   LABEL "Cost Per M"
    FIELD lineTotCost     AS DECIMAL   LABEL "Line Total Cost"
    FIELD OrderTotCost    AS DECIMAL   LABEL "Order Total Cost"
    FIELD newlineCostM    AS DECIMAL   LABEL "Change Line CostM"
    FIELD newlineTotCost  AS DECIMAL   LABEL "Change Order Line Cost"
    FIELD newOrderTotCost AS DECIMAL   LABEL "Change Order Total Cost"  
    FIELD note            AS CHARACTER
    .

DEFINE            VARIABLE hdOutput         AS HANDLE    NO-UNDO.
DEFINE            VARIABLE lError           AS LOGICAL   NO-UNDO.
DEFINE            VARIABLE cMessage         AS CHARACTER NO-UNDO. 
DEFINE            VARIABLE dOrderTotalCost  AS DECIMAL   NO-UNDO.

DEFINE NEW SHARED VARIABLE lv-qty           AS INTEGER   NO-UNDO.
DEFINE NEW SHARED VARIABLE qty              AS INTEGER   NO-UNDO.
DEFINE NEW SHARED VARIABLE v-shared-rel     AS INTEGER   NO-UNDO.

DEFINE            VARIABLE dNewOrdLineCostM AS DECIMAL   NO-UNDO.

RUN system\OutputProcs.p PERSISTENT SET hdOutput.

{oe/chkordl.i NEW}
{oe/relemail.i NEW}
{sys/inc/var.i new shared }
{ce/print4.i "new shared"}
{ce/print42.i "new shared"}

DEFINE            BUFFER bf-oe-rel  FOR oe-rel.
DEFINE            BUFFER bf-oe-ordl FOR oe-ordl.
DEFINE            BUFFER bf-oe-ord  FOR oe-ord.

DEFINE NEW SHARED BUFFER xest       FOR est.
DEFINE NEW SHARED BUFFER xeb        FOR eb.
DEFINE NEW SHARED BUFFER xef        FOR ef.

RUN spGetSessionParam("Location", OUTPUT locode).
ASSIGN
    cocode = ipcCompany
    .
    
{sys/inc/vendItemCost.i}
{sys/ref/fgoecost.i}

FUNCTION get-itemfg-cost RETURNS DECIMAL
    ( ipv-item AS CHARACTER /* parameter-definitions */ )  FORWARD.
    
FUNCTION get-order-total RETURNS DECIMAL
    ( ipiorder AS INTEGER, ipiOrderLine AS INTEGER /* parameter-definitions */ )  FORWARD.    
    
FUNCTION fUseNewEstimating RETURNS LOGICAL PRIVATE
    (ipcCompany AS CHARACTER ) FORWARD.    
    

FOR EACH bf-oe-ordl NO-LOCK
    WHERE bf-oe-ordl.company EQ ipcCompany     
    AND bf-oe-ordl.ord-no GE ipiBeginOrder
    AND bf-oe-ordl.ord-no LE ipiEndOrder
    ,
    FIRST bf-oe-ord NO-LOCK
    WHERE bf-oe-ord.company EQ bf-oe-ordl.company
    AND bf-oe-ord.ord-date GE ipcBeginOrdDate
    AND bf-oe-ord.ord-date LE ipcEndOrdDate
    AND ((bf-oe-ord.opened EQ YES AND ipcOrderStatus EQ "O") OR (bf-oe-ord.opened EQ NO AND ipcOrderStatus EQ "C") OR ipcOrderStatus EQ "A")
    AND bf-oe-ord.ord-no EQ bf-oe-ordl.ord-no      
    BREAK BY bf-oe-ordl.ord-no:              
        
    CREATE ttOrderLineChange.
    ASSIGN
        ttOrderLineChange.orderNo      = bf-oe-ordl.ord-no
        ttOrderLineChange.customerID   = bf-oe-ordl.cust-no
        ttOrderLineChange.shipID       = bf-oe-ordl.ship-id             
        ttOrderLineChange.orderLine    = bf-oe-ordl.line
        ttOrderLineChange.itemCode     = bf-oe-ordl.i-no
            
        ttOrderLineChange.orderDate    = bf-oe-ord.ord-date   
        ttOrderLineChange.lineCostM    = bf-oe-ordl.cost
        ttOrderLineChange.lineTotCost  = bf-oe-ordl.t-cost
        ttOrderLineChange.OrderTotCost = bf-oe-ord.t-cost
        .              
            
    RUN pGetNewCostM(BUFFER bf-oe-ordl, OUTPUT dNewOrdLineCostM).  
    ttOrderLineChange.newlineCostM    = dNewOrdLineCostM.
    ttOrderLineChange.newlineTotCost  = ttOrderLineChange.newlineCostM * bf-oe-ordl.qty / 1000.
                       
    IF ipExecute AND ttOrderLineChange.lineCostM NE ttOrderLineChange.newlineCostM THEN
    DO:                 
        FIND CURRENT bf-oe-ordl EXCLUSIVE-LOCK NO-ERROR.
        ASSIGN
            bf-oe-ordl.cost   = ttOrderLineChange.newlineCostM
            bf-oe-ordl.t-cost = ttOrderLineChange.newlineCostM * bf-oe-ordl.qty / 1000.
        ttOrderLineChange.note = "Cost Changed".
        FIND CURRENT bf-oe-ordl NO-LOCK NO-ERROR.                
    END.                
    IF ipExecute AND LAST-OF(bf-oe-ordl.ord-no) THEN
    DO:
        IF bf-oe-ord.t-cost NE get-order-total(ttOrderLineChange.orderNo, ttOrderLineChange.orderLine) THEN 
        DO:
            FIND CURRENT bf-oe-ord EXCLUSIVE-LOCK NO-ERROR.
            ASSIGN                       
                bf-oe-ord.t-cost = get-order-total(ttOrderLineChange.orderNo, ttOrderLineChange.orderLine).                      
            FIND CURRENT bf-oe-ord NO-LOCK NO-ERROR.
        END.
    END.
       
END.
FOR EACH ttOrderLineChange NO-LOCK
    BREAK BY ttOrderLineChange.orderNo
    BY ttOrderLineChange.orderLine:
    IF FIRST-OF(ttOrderLineChange.orderLine) THEN
        dOrderTotalCost =  get-order-total(ttOrderLineChange.orderNo, ttOrderLineChange.orderLine).
     
    ttOrderLineChange.newOrderTotCost =  dOrderTotalCost.              
END.

RUN Output_TempTableToCSV IN hdOutput (TEMP-TABLE ttOrderLineChange:HANDLE, ipFilePath ,YES,YES, OUTPUT lError, OUTPUT cMessage).

PROCEDURE pGetNewCostM:
    /*------------------------------------------------------------------------------
     Purpose: 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-oe-ordl FOR oe-ordl.
    DEFINE OUTPUT PARAMETER opdNewOrdCost  AS DECIMAL NO-UNDO.

    DEFINE VARIABLE ld-cost      AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lv-uom       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-cost       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCostFromEst AS DECIMAL   NO-UNDO.
    
    IF ipbf-oe-ordl.job-no EQ "" THEN 
    DO:
        FIND FIRST itemfg
            WHERE itemfg.company EQ cocode
            AND itemfg.i-no = ipbf-oe-ordl.i-no
            NO-LOCK NO-ERROR.
            
        FIND FIRST po-ord WHERE po-ord.company EQ cocode
            AND po-ord.po-no   EQ ipbf-oe-ordl.po-no-po
            NO-LOCK NO-ERROR.    
                
        FIND FIRST po-ordl  WHERE po-ordl.company   EQ cocode
            AND po-ordl.i-no      EQ ipbf-oe-ordl.i-no
            AND po-ordl.po-no     EQ ipbf-oe-ordl.po-no-po
            AND po-ordl.item-type EQ NO
            USE-INDEX item-ordno NO-LOCK NO-ERROR.
            
        IF AVAILABLE po-ordl AND AVAILABLE po-ordl THEN
        DO:                
            IF po-ordl.cons-uom EQ "M" THEN
                opdNewOrdCost = po-ordl.cons-cost. 
            ELSE 
            DO:                 
                RUN sys/ref/convcuom.p (po-ordl.cons-uom, "M", 0, 0, 0, 0,
                    po-ordl.cons-cost, OUTPUT ld-cost).

                opdNewOrdCost = ld-cost.                          
            END.
            IF lNewVendorItemCost THEN .
            ELSE 
                FIND FIRST e-itemfg-vend WHERE
                    e-itemfg-vend.company EQ po-ordl.company AND
                    e-itemfg-vend.i-no EQ po-ordl.i-no AND
                    e-itemfg-vend.vend-no EQ po-ord.vend-no AND
                    e-itemfg-vend.est-no EQ ""
                    NO-LOCK NO-ERROR.

            IF AVAILABLE e-itemfg-vend THEN
            DO:
                opdNewOrdCost = DECIMAL(DEC(opdNewOrdCost) * (1 + (e-itemfg-vend.markup / 100.0 ))).
            END.
        END. 
        ELSE 
        DO:
              
            IF AVAILABLE itemfg THEN
                ASSIGN 
                    lv-uom        = IF itemfg.prod-uom NE "" THEN itemfg.prod-uom ELSE "M"
                    v-cost        = get-itemfg-cost(itemfg.i-no)
                    opdNewOrdCost = DECIMAL(get-itemfg-cost(itemfg.i-no)).
                    
            IF lv-uom NE "M" THEN 
            DO:
                RUN sys/ref/convcuom.p(lv-uom, "M", 0, 0, 0, 0,
                    v-cost, OUTPUT ld-cost).
                ASSIGN 
                    opdNewOrdCost = ld-cost
                    .                        
            END. 
        END.
                
    END.
            
    IF ipbf-oe-ordl.est-no NE "" THEN
    DO:
        RUN getCostFromEstimate( BUFFER ipbf-oe-ordl, INPUT ipbf-oe-ordl.est-no, OUTPUT dCostFromEst).
        IF dCostFromEst GT 0 THEN opdNewOrdCost =  dCostFromEst .
    END.    
   
END PROCEDURE.

PROCEDURE getCostFromEstimate :
    /*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-oe-ordl FOR oe-ordl.
    DEFINE INPUT PARAMETER ipcEstimateNo AS CHARACTER NO-UNDO.     
    DEFINE OUTPUT PARAMETER opdNewOrdCost AS DECIMAL NO-UNDO.
    
    DEFINE VARIABLE cProgramList AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iEbCnt       AS INTEGER   NO-UNDO.
    
    DEFINE BUFFER bf-estCostHeader FOR estCostHeader.
    DEFINE BUFFER bf-estCostItem   FOR estCostItem.
    
    IF ipcEstimateNo NE "" AND NOT AVAILABLE xest THEN
        FIND FIRST xest NO-LOCK 
            WHERE xest.company EQ cocode 
            AND xest.est-no EQ ipcEstimateNo 
            NO-ERROR.

    IF AVAILABLE xest THEN 
    DO:
        IF fUseNewEstimating(cocode) THEN 
        DO:        
            IF ipbf-oe-ordl.job-no NE "" THEN 
                FIND FIRST bf-estCostHeader NO-LOCK 
                    WHERE bf-estCostHeader.company EQ cocode
                    AND bf-estCostHeader.estimateNo EQ ipcEstimateNo
                    AND bf-estCostHeader.jobID EQ ipbf-oe-ordl.job-no
                    AND bf-estCostHeader.jobID2 EQ INT(ipbf-oe-ordl.job-no2)
                    NO-ERROR.
            IF NOT AVAILABLE bf-estCostHeader THEN 
                FIND FIRST bf-estCostHeader NO-LOCK 
                    WHERE bf-estCostHeader.company EQ cocode
                    AND bf-estCostHeader.estimateNo EQ ipcEstimateNo
                    AND bf-estCostHeader.quantityMaster LE INT(ipbf-oe-ordl.qty)
                    NO-ERROR.
            IF NOT AVAILABLE bf-estCostHeader THEN 
                FIND FIRST bf-estCostHeader NO-LOCK 
                    WHERE bf-estCostHeader.company EQ cocode
                    AND bf-estCostHeader.estimateNo EQ ipcEstimateNo
                    NO-ERROR.
            IF AVAILABLE bf-estCostHeader THEN 
                FIND FIRST bf-estCostItem NO-LOCK 
                    WHERE bf-estCostItem.estCostHeaderID EQ bf-estCostHeader.estCostHeaderID
                    AND bf-estCostItem.itemID EQ ipbf-oe-ordl.i-no
                    NO-ERROR.
            IF NOT AVAILABLE bf-estCostItem THEN 
                FIND FIRST bf-estCostItem NO-LOCK 
                    WHERE bf-estCostItem.estCostHeaderID EQ bf-estCostHeader.estCostHeaderID
                    AND bf-estCostItem.customerPart EQ ipbf-oe-ordl.part-no
                    NO-ERROR. 
            IF AVAILABLE bf-estCostItem THEN 
                opdNewOrdCost = DECIMAL((IF v-full-cost THEN bf-estCostItem.costTotalFull ELSE bf-estCostItem.costTotalFactory) /
                    (bf-estCostItem.quantityRequired / 1000)).
        END.
        ELSE 
        DO:   
            FIND FIRST xeb NO-LOCK
                WHERE xeb.company EQ xest.company 
                AND xeb.est-no EQ  xest.est-no
                AND xeb.part-no = ipbf-oe-ordl.part-no
                NO-ERROR.
            IF NOT AVAILABLE xeb THEN 
            DO:
                iEbCnt = 0.
                FOR EACH xeb 
                    WHERE xeb.company = cocode 
                    AND xeb.est-no = ipcEstimateNo            
                    NO-LOCK:
                    iEbCnt = iEbCnt + 1.
                END.
                /* If there is only one record, use it 03191507 */
                IF iEbCnt EQ 1 THEN
                    FIND FIRST xeb
                        WHERE xeb.company = cocode 
                        AND xeb.est-no = ipcEstimateNo
                        NO-LOCK NO-ERROR.
            END.
         
            IF AVAILABLE xeb THEN
                FIND FIRST xef
                    WHERE xef.company = cocode 
                    AND xef.est-no = ipcEstimateNo
                    AND (xef.form-no = xeb.form-no OR xeb.form-no = 0)
                    NO-LOCK NO-ERROR.
                
            ASSIGN
                cProgramList = "ce/print4.p,ce/box/print42.p,ce/tan/print4.p," +
                         "ce/com/print4.p,cec/print4.p,cec/box/print42.p," +
                         "cec/tan/print4.p,cec/com/print4.p"
                qty          = INT(ipbf-oe-ordl.qty)
                v-shared-rel = ipbf-oe-ordl.rel.
    
            IF AVAILABLE xeb AND AVAILABLE xef                              AND
                xest.est-type NE 3                                          AND
                xest.est-type NE 4                                          AND
                xest.est-type NE 8                                          THEN 
            DO:
                     
                RUN VALUE(ENTRY(xest.est-type,cProgramList)).     
                                   
                opdNewOrdCost = DECIMAL((IF v-full-cost THEN tt-tot ELSE ord-cost) /
                    (INT(ipbf-oe-ordl.qty) / 1000)).
            END.
        END.
    END.

END PROCEDURE.



FUNCTION get-itemfg-cost RETURNS DECIMAL
    ( ipv-item AS CHARACTER /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER bfItemfg FOR itemfg.
    DEFINE VARIABLE v-cost AS DECIMAL NO-UNDO.
    v-cost = 0.
    FIND FIRST bfItemfg WHERE bfItemfg.company = cocode
        AND bfItemfg.i-no    = ipv-item
        NO-LOCK NO-ERROR.
    IF AVAIL(bfItemfg) THEN
        v-cost = bfItemfg.total-std-cost.
    FIND FIRST fg-ctrl WHERE fg-ctrl.company = cocode NO-LOCK NO-ERROR.
    IF AVAILABLE fg-ctrl THEN 
    DO:
        IF fg-ctrl.inv-meth = "A" AND bfItemfg.avg-cost GT 0 THEN
            v-cost = bfItemfg.avg-cost.
        ELSE
            IF fg-ctrl.inv-meth = "L" AND bfItemfg.last-cost GT 0 THEN
                v-cost = bfItemfg.last-cost.
    END.   
    RETURN v-cost.   /* Function return value. */

END FUNCTION.


FUNCTION get-order-total RETURNS DECIMAL
    ( ipiorder AS INTEGER, ipiOrderLine AS INTEGER /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE dReturnCost AS DECIMAL NO-UNDO.
    DEFINE BUFFER bf-ttOrderLineChange FOR ttOrderLineChange.
    FOR EACH bf-ttOrderLineChange 
        WHERE bf-ttOrderLineChange.orderNo EQ ipiorder
        AND bf-ttOrderLineChange.orderLine EQ ipiOrderLine NO-LOCK :
        dReturnCost = dReturnCost + bf-ttOrderLineChange.newlineTotCost .
    END.
    RETURN dReturnCost.   /* Function return value. */

END FUNCTION.


FUNCTION fUseNewEstimating RETURNS LOGICAL PRIVATE
    (ipcCompany AS CHARACTER ):
    /*------------------------------------------------------------------------------
     Purpose:  Use new estimating
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lCEVersion AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cRtnChar   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lRecFound  AS LOGICAL   NO-UNDO.
    
    RUN sys/ref/nk1look.p (ipcCompany, "CEVersion", "C" /* Character */, NO /* check by cust */, 
        INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
        OUTPUT cRtnChar, OUTPUT lRecFound).
    lCEVersion = lRecFound AND cRtnChar EQ "New".
    
    RETURN lCEVersion. 
END FUNCTION.
