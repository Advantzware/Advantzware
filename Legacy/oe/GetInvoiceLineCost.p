/* -------------------------------------------------- oe/invlpost.p 08/00 JLF */
/* set o/e invoice line cost                                                  */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
DEFINE INPUT  PARAMETER ipriInvl                        AS   ROWID   NO-UNDO.
DEFINE OUTPUT PARAMETER opdCostPerUOMDirectLabor        AS   DECIMAL NO-UNDO.
DEFINE OUTPUT PARAMETER opdCostPerUOMFixedOverhead      AS   DECIMAL NO-UNDO.
DEFINE OUTPUT PARAMETER opdCostPerUOMVariableOverhead   AS   DECIMAL NO-UNDO.
DEFINE OUTPUT PARAMETER opdCostPerUOMDirectMaterial     AS   DECIMAL NO-UNDO.
DEFINE OUTPUT PARAMETER opdCostPerUOMTotal              AS   DECIMAL NO-UNDO.
DEFINE OUTPUT PARAMETER opdCostTotalExtended            AS   DECIMAL NO-UNDO.

DEFINE VARIABLE cCompany                    AS CHARACTER NO-UNDO.
DEFINE VARIABLE iBNo                        AS INTEGER   NO-UNDO.
DEFINE VARIABLE iOrderNo                    AS INTEGER   NO-UNDO.
DEFINE VARIABLE cFGItemID                   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCustomerPONo               AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cJobNo                      AS CHARACTER NO-UNDO.
DEFINE VARIABLE iJobNo2                     AS INTEGER   NO-UNDO.
DEFINE VARIABLE dQtyInvoiced                AS DECIMAL   NO-UNDO.

DEFINE VARIABLE cCostUOM                    AS CHARACTER NO-UNDO.
DEFINE VARIABLE iQtyBOL                     AS INTEGER   NO-UNDO. 

DEFINE VARIABLE dCostPerUOMAverage          AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dCostPerUOMLast             AS DECIMAL   NO-UNDO. 
DEFINE VARIABLE dCostPerUOMFixedOverhead    AS DECIMAL   NO-UNDO.    
DEFINE VARIABLE dCostPerUOMDirectLabor      AS DECIMAL   NO-UNDO.    
DEFINE VARIABLE dCostPerUOMDirectMaterial   AS DECIMAL   NO-UNDO.   
DEFINE VARIABLE dCostPerUOMTotal            AS DECIMAL   NO-UNDO.    
DEFINE VARIABLE dCostPerUOMVariableOverhead AS DECIMAL   NO-UNDO.

DEFINE VARIABLE fgb-pur-uom                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-cost                      AS DECIMAL   DECIMALS 4 EXTENT 4 NO-UNDO.
DEFINE VARIABLE v-cost-m                    AS DECIMAL   DECIMALS 4 EXTENT 4 NO-UNDO.

DEFINE TEMP-TABLE ttItemfgCost
    FIELD cFGItemID                   AS CHARACTER
    FIELD cCostUOM                    AS CHARACTER  
    FIELD dCostPerUOMAverage          AS DECIMAL 
    FIELD dCostPerUOMLast             AS DECIMAL 
    FIELD dCostPerUOMFixedOverhead    AS DECIMAL 
    FIELD dCostPerUOMDirectMaterial   AS DECIMAL 
    FIELD dCostPerUOMTotal            AS DECIMAL 
    FIELD dCostPerUOMVariableOverhead AS DECIMAL 
    FIELD dCostPerUOMDirectLabor      AS DECIMAL 
    INDEX i1 cFGItemID.


DEFINE BUFFER b-fg-rcpth FOR fg-rcpth.
DEFINE BUFFER b-fg-rdtlh FOR fg-rdtlh.

FIND inv-line NO-LOCK 
    WHERE ROWID(inv-line) EQ ipriInvl 
    NO-ERROR.

IF NOT AVAILABLE inv-line THEN
    FIND ar-invl NO-LOCK 
        WHERE ROWID(ar-invl) EQ ipriInvl 
        NO-ERROR.

IF AVAILABLE inv-line OR AVAILABLE ar-invl THEN 
DO:
    IF AVAILABLE inv-line THEN
        ASSIGN
            cCompany      = inv-line.company
            iBNo          = inv-line.b-no
            iOrderNo      = inv-line.ord-no
            cFGItemID     = inv-line.i-no
            cCustomerPONo = inv-line.po-no
            cJobNo        = inv-line.job-no
            iJobNo2       = inv-line.job-no2
            dQtyInvoiced  = inv-line.inv-qty.
    ELSE
        ASSIGN
            cCompany      = ar-invl.company
            iBNo          = ar-invl.b-no
            iOrderNo      = ar-invl.ord-no
            cFGItemID     = ar-invl.i-no
            cCustomerPONo = ar-invl.po-no
            cJobNo        = ar-invl.job-no
            iJobNo2       = ar-invl.job-no2
            dQtyInvoiced  = ar-invl.inv-qty.

    RELEASE inv-line.
    RELEASE ar-invl.

    FIND FIRST itemfg NO-LOCK 
        WHERE itemfg.company EQ cCompany
        AND itemfg.i-no EQ cFGItemID
        NO-ERROR.

    FOR EACH oe-boll NO-LOCK  
        WHERE oe-boll.company EQ cCompany
        AND oe-boll.b-no    EQ iBNo
        AND oe-boll.ord-no  EQ iOrderNo
        AND oe-boll.i-no    EQ cFGItemID
        AND oe-boll.po-no   EQ cCustomerPONo
        AND oe-boll.qty     NE 0
        USE-INDEX b-no :

        ASSIGN 
            dCostPerUOMAverage          = 0  
            dCostPerUOMLast             = 0   
            dCostPerUOMFixedOverhead    = 0     
            dCostPerUOMDirectLabor      = 0    
            dCostPerUOMDirectMaterial   = 0   
            dCostPerUOMTotal            = 0     
            dCostPerUOMVariableOverhead = 0
            cCostUOM                    = "".

        FIND FIRST fg-bin
            WHERE fg-bin.company EQ cCompany
            AND fg-bin.i-no    EQ oe-boll.i-no
            AND fg-bin.tag     EQ oe-boll.tag
            AND fg-bin.loc     EQ oe-boll.loc
            AND fg-bin.loc-bin EQ oe-boll.loc-bin
            AND fg-bin.job-no  EQ oe-boll.job-no
            AND fg-bin.job-no2 EQ oe-boll.job-no2
            NO-LOCK NO-ERROR.

        IF AVAILABLE fg-bin THEN
            ASSIGN 
                dCostPerUOMAverage          = fg-bin.avg-cost     
                dCostPerUOMLast             = fg-bin.last-cost     
                dCostPerUOMFixedOverhead    = fg-bin.std-fix-cost     
                dCostPerUOMDirectLabor      = fg-bin.std-lab-cost     
                dCostPerUOMDirectMaterial   = fg-bin.std-mat-cost    
                dCostPerUOMTotal            = fg-bin.std-tot-cost     
                dCostPerUOMVariableOverhead = fg-bin.std-var-cost 
                cCostUOM                    = fg-bin.pur-uom.

        IF NOT AVAILABLE fg-bin AND AVAILABLE itemfg AND oe-boll.tag NE "" THEN 
        DO:
            each-fg:
                FOR EACH b-fg-rcpth WHERE b-fg-rcpth.company   EQ itemfg.company
                    AND b-fg-rcpth.i-no      EQ itemfg.i-no
                    AND b-fg-rcpth.rita-code EQ "R"
                    USE-INDEX tran NO-LOCK  ,
            
                    FIRST b-fg-rdtlh WHERE b-fg-rdtlh.r-no    EQ b-fg-rcpth.r-no 
                    AND b-fg-rdtlh.rita-code EQ b-fg-rcpth.rita-code
                    AND b-fg-rdtlh.tag EQ oe-boll.tag
                    NO-LOCK
                    BY b-fg-rcpth.trans-date DESCENDING:            
                
                    ASSIGN
                        dCostPerUOMAverage          = b-fg-rdtlh.avg-cost    
                        dCostPerUOMLast             = b-fg-rdtlh.last-cost    
                        dCostPerUOMFixedOverhead    = b-fg-rdtlh.std-fix-cost   
                        dCostPerUOMDirectLabor      = b-fg-rdtlh.std-lab-cost   
                        dCostPerUOMDirectMaterial   = b-fg-rdtlh.std-mat-cost    
                        dCostPerUOMTotal            = b-fg-rdtlh.std-tot-cost    
                        dCostPerUOMVariableOverhead = b-fg-rdtlh.std-var-cost    
                        cCostUOM                    = b-fg-rcpth.pur-uom.


                    LEAVE each-fg. 
                END. /* each fg-rcp */
            END. /* else do */
        END.  /* not avail bin */

               
        ASSIGN
            iQtyBOL  = iQtyBOL + oe-boll.qty
            cCostUOM = "".

        IF dCostPerUOMTotal NE 0 THEN
            ASSIGN
                v-cost-m[1] = dCostPerUOMDirectLabor
                v-cost-m[2] = dCostPerUOMFixedOverhead
                v-cost-m[3] = dCostPerUOMVariableOverhead
                v-cost-m[4] = dCostPerUOMDirectMaterial
                cCostUOM    = fgb-pur-uom.
       
        ELSE DO:      
            FIND FIRST job-hdr NO-LOCK 
                WHERE job-hdr.company EQ cCompany
                AND job-hdr.job-no EQ cJobNo
                AND job-hdr.job-no2 EQ iJobNo2      
                AND job-hdr.i-no EQ cFGItemID
                NO-ERROR.            
            IF AVAILABLE job-hdr AND job-hdr.std-tot-cost NE 0 THEN
                ASSIGN
                    v-cost-m[1] = job-hdr.std-lab-cost
                    v-cost-m[2] = job-hdr.std-fix-cost
                    v-cost-m[3] = job-hdr.std-var-cost
                    v-cost-m[4] = job-hdr.std-mat-cost
                    cCostUOM    = "M".
       
            ELSE   
                ASSIGN
                    v-cost-m[1] = itemfg.std-lab-cost
                    v-cost-m[2] = itemfg.std-fix-cost
                    v-cost-m[3] = itemfg.std-var-cost
                    v-cost-m[4] = itemfg.std-mat-cost
                    cCostUOM = itemfg.prod-uom.
        END.
        
        IF cCostUOM EQ "" 
            THEN cCostUOM = itemfg.prod-uom.

        DO i = 1 TO 4:

            IF cCostUOM NE "M" THEN DO:
                RUN sys/ref/convcuom.p(cCostUOM, "M", 0, 0, 0, 0,
                    dCostPerUOMDirectLabor, OUTPUT v-cost-m[i]).
                       
            v-cost[i] = v-cost[i] + (v-cost-m[i] * oe-boll.qty / 1000 ).
        END.

    END. /*end each oe-boll*/

    DO i = 1 TO 4:
        v-cost[i] = v-cost[i] / (iQtyBOL / 1000) .
    
        IF v-cost[i] EQ ? THEN v-cost[i] = 0.
    END.
  
    ASSIGN
        opdCostPerUOMDirectLabor      = v-cost[1]
        opdCostPerUOMFixedOverhead    = v-cost[2]
        opdCostPerUOMVariableOverhead = v-cost[3]
        opdCostPerUOMDirectMaterial   = v-cost[4]
        opdCostPerUOMTotal            = v-cost[1] + v-cost[2] + v-cost[3] + v-cost[4].
          
    IF opdCostPerUOMTotal EQ ? THEN opdCostPerUOMTotal = 0.
          
    opdCostTotalExtended = opdCostPerUOMTotal * dQtyInvoiced / 1000.

    IF opdCostTotalExtended EQ ? THEN opdCostTotalExtended = 0.

END.  
          
 
