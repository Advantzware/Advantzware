
DEFINE INPUT PARAMETER   iprEb             AS ROWID NO-UNDO.
DEFINE INPUT PARAMETER   ipdQty            AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER   ipcUom            AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER  opdCostPerUom  AS DECIMAL NO-UNDO.
DEFINE OUTPUT PARAMETER  opdCostSetup       AS DECIMAL NO-UNDO.
DEFINE OUTPUT PARAMETER  opcCostUOM         AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER  opcVend           AS CHARACTER NO-UNDO.

{custom/globdefs.i}
{custom/gcompany.i}
{sys/inc/var.i NEW SHARED}
{sys/inc/varasgn.i}

DEFINE TEMP-TABLE ttLowestCosts
    FIELD cVendor AS CHARACTER 
    FIELD dCostPerUOM AS DECIMAL 
    FIELD dCostSetup AS DECIMAL 
    FIELD dCostExtended AS DECIMAL 
    FIELD cCostUom AS CHARACTER 
    FIELD lSelected AS LOGICAL
    INDEX idxTotalCost dCostExtended ASCENDING.

DEFINE VARIABLE dLastRunLevel   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cUOM            AS CHARACTER NO-UNDO.
DEFINE VARIABLE dGrossSheetLength         LIKE item.s-len NO-UNDO.
DEFINE VARIABLE dGrossSheetWidth          LIKE item.s-wid NO-UNDO.
DEFINE VARIABLE dGrossSheetDepth          LIKE item.s-dep NO-UNDO.
DEFINE VARIABLE llSelectedFound AS LOG     NO-UNDO.
DEFINE VARIABLE dQtyInVendorUOM AS DECIMAL NO-UNDO.

FIND eb WHERE ROWID(eb) EQ iprEB NO-LOCK NO-ERROR.
FIND FIRST ef NO-LOCK
    WHERE ef.company EQ eb.company
    AND ef.est-no  EQ eb.est-no
    AND ef.form-no EQ eb.form-no
    NO-ERROR.
IF NOT AVAILABLE ef THEN
    RETURN.

ASSIGN
    dGrossSheetLength = ef.gsh-len
    dGrossSheetWidth = ef.gsh-wid
    dGrossSheetDepth = ef.gsh-dep.

FIND itemfg NO-LOCK  
    WHERE itemfg.company EQ eb.company
    AND itemfg.i-no EQ eb.stock-no
    NO-ERROR.
IF NOT AVAILABLE itemfg THEN
    RETURN.

EMPTY TEMP-TABLE ttLowestCosts.

/* Check for selected vendor for qty */
/* If no selected vendor, choose lowest cost */
IF eb.pur-man THEN
    each-vend:
    FOR EACH e-itemfg-vend NO-LOCK
        WHERE e-itemfg-vend.company  EQ eb.company
        AND e-itemfg-vend.est-no   EQ eb.est-no
        AND e-itemfg-vend.eqty     EQ eb.eqty
        AND e-itemfg-vend.form-no  EQ eb.form-no
        AND e-itemfg-vend.blank-no EQ eb.blank-no
        BREAK BY e-itemfg-vend.vend-no:
        
        ASSIGN
            cUOM = e-itemfg-vend.std-uom                   
            dQtyInVendorUOM = ipdQty.
            
        IF cUOM EQ "" THEN cUOM = ipcUom.
        
        /* Convert given UOM to that of e-itemfg-vend for comparison */
        IF ipcUOM NE cUOM THEN 
        DO:
            RUN sys/ref/convquom.p(ipcUom, cUOM,
                itemfg.weight-100, dGrossSheetLength, dGrossSheetWidth, dGrossSheetDepth,
                dQtyInVendorUOM, OUTPUT dQtyInVendorUOM).
        END.                                    
        FIND FIRST ttLowestCosts
            WHERE ttLowestCosts.cVendor EQ e-itemfg-vend.vend-no
            NO-ERROR.
        IF NOT AVAILABLE ttLowestCosts THEN DO:
            CREATE ttLowestCosts.
            ASSIGN 
                ttLowestCosts.cVendor = e-itemfg-vend.vend-no
                ttLowestCosts.cCostUom = cUOM
                .
        END.
        dLastRunLevel = 0.
        DO i = 1 TO EXTENT(e-itemfg-vend.run-qty):
            IF e-itemfg-vend.run-qty[i] LT dQtyInVendorUOM THEN NEXT.
            ASSIGN
                 ttLowestCosts.dCostPerUOM = e-itemfg-vend.run-cost[i]
                 ttLowestCosts.dCostSetup = e-itemfg-vend.setups[i]
                 ttLowestCosts.dCostExtended = ttLowestCosts.dCostPerUOM * dQtyInVendorUOM + ttLowestCosts.dCostSetup
                 ttLowestCosts.lSelected = e-itemfg-vend.selected[i]
                 .
                LEAVE.    
        END. /* Do i = 1 to ... */
   END. /* each e-itemfg-vend */

FIND FIRST ttLowestCosts NO-LOCK 
    WHERE ttLowestCosts.lSelected
    NO-ERROR.
IF NOT AVAILABLE ttLowestCosts THEN 
    FIND FIRST ttLowestCosts NO-LOCK 
    NO-ERROR.
IF AVAILABLE ttLowestCosts THEN 
    ASSIGN 
        opdCostPerUom = ttLowestCosts.dCostPerUOM
        opdCostSetup = ttLowestCosts.dCostSetup
        opcCostUOM = ttLowestCosts.cCostUom
        opcVend = ttLowestCosts.cVendor
        .        
