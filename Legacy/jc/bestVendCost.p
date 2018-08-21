
DEFINE INPUT PARAMETER   iprEb             AS ROWID NO-UNDO.
DEFINE INPUT PARAMETER   ipdQty            AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER   ipcUom            AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER  opdLowestRunCost  AS DECIMAL NO-UNDO.
DEFINE OUTPUT PARAMETER  opdEstSetup       AS DECIMAL NO-UNDO.
DEFINE OUTPUT PARAMETER  opcEstUOM         AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER  opcVend           AS CHARACTER NO-UNDO.

{custom/globdefs.i}
{custom/gcompany.i}
{sys/inc/var.i NEW SHARED}
{sys/inc/varasgn.i}

DEFINE VARIABLE dLastRunLevel   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cUOM            AS CHARACTER NO-UNDO.
DEFINE VARIABLE dGrossSheetLength         LIKE item.s-len NO-UNDO.
DEFINE VARIABLE dGrossSheetWidth          LIKE item.s-wid NO-UNDO.
DEFINE VARIABLE dGrossSheetDepth          LIKE item.s-dep NO-UNDO.
DEFINE VARIABLE llSelectedFound AS LOG     NO-UNDO.

FIND eb WHERE ROWID(eb) EQ iprEB NO-LOCK NO-ERROR.
FIND FIRST ef
    WHERE ef.company EQ eb.company
    AND ef.est-no  EQ eb.est-no
    AND ef.form-no EQ eb.form-no
    NO-LOCK NO-ERROR.

IF NOT AVAILABLE ef THEN
    RETURN.

ASSIGN
    dGrossSheetLength = ef.gsh-len
    dGrossSheetWidth = ef.gsh-wid
    dGrossSheetDepth = ef.gsh-dep.

FIND itemfg WHERE itemfg.company EQ eb.company
    AND itemfg.i-no EQ eb.stock-no
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE itemfg THEN
    RETURN.

/* Check for selected vendor for qty */
/* If no selected vendor, choose lowest cost */
llSelectedFound = NO.
IF eb.pur-man THEN
    each-vend:
    FOR EACH e-itemfg-vend NO-LOCK
        WHERE e-itemfg-vend.company  EQ eb.company
        AND e-itemfg-vend.est-no   EQ eb.est-no
        AND e-itemfg-vend.eqty     EQ eb.eqty
        AND e-itemfg-vend.form-no  EQ eb.form-no
        AND e-itemfg-vend.blank-no EQ eb.blank-no
        BREAK BY e-itemfg-vend.vend-no:                  

        /* Convert give UOM to that of e-itemfg-vend for comparison */
        IF ipcUOM NE e-itemfg-vend.std-uom THEN 
        DO:
            DEFINE VARIABLE ip-qty AS DECIMAL.
            DEFINE VARIABLE op-qty AS DECIMAL.

            RUN sys/ref/convquom.p(ipcUom, e-itemfg-vend.std-uom,
                itemfg.weight-100, dGrossSheetLength, dGrossSheetWidth, dGrossSheetDepth,
                ip-qty, OUTPUT op-qty).
        END.                                    

    
        dLastRunLevel = 0.
        DO i = 1 TO EXTENT(e-itemfg-vend.run-qty):
            IF e-itemfg-vend.run-qty[i] EQ 0 THEN
                NEXT.
        
            IF  ipdQty GT dLastRunLevel   THEN 
            DO:
                IF e-itemfg-vend.run-cost[i] LT opdLowestRunCost
                    OR e-itemfg-vend.SELECTED[i] 
                    OR opdLowestRunCost EQ 0 THEN 
                DO:
                    ASSIGN 
                        opdLowestRunCost = e-itemfg-vend.run-cost[i]
                        opcVend          = e-itemfg-vend.vend-no
                        opdEstSetup      = e-itemfg-vend.setups[i]
                        cUOM             = e-itemfg-vend.std-uom
                        opcEstUOM        = e-itemfg-vend.std-uom.

                    IF e-itemfg-vend.SELECTED[i] THEN
                        llSelectedFound = TRUE.
                END. /* if cost is lower or selected */
            
            /* User checked this one off, so use it and exit loop */
            END. /* if qty is greater than previous max */

            dLastRunLevel = e-itemfg-vend.run-qty[i].

            /* If checked off then use this one and don't look at others */
            IF llSelectedFound THEN
                LEAVE each-vend.

        END. /* Do i = 1 to ... */
   
    END. /* If pur-man */
