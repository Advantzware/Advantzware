
/*------------------------------------------------------------------------
    File        : GetCostInvl.p
    Purpose     : 

    Syntax      :

    Description : Gets the cost for an invoice line given a rowid for 
        either inv-line or ar-invl.  Returns 4 cost/M details, a total cost/M, 
        a total extended Cost, and Source indicator for debugging purposes

    Author(s)   : BV
    Created     : Thu Dec 06 13:51:43 EST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT  PARAMETER ipriInvl                        AS   ROWID   NO-UNDO.
DEFINE OUTPUT PARAMETER opdCostPerUOMDL                 AS   DECIMAL NO-UNDO.
DEFINE OUTPUT PARAMETER opdCostPerUOMFO                 AS   DECIMAL NO-UNDO.
DEFINE OUTPUT PARAMETER opdCostPerUOMVO                 AS   DECIMAL NO-UNDO.
DEFINE OUTPUT PARAMETER opdCostPerUOMDM                 AS   DECIMAL NO-UNDO.
DEFINE OUTPUT PARAMETER opdCostPerUOMTotal              AS   DECIMAL NO-UNDO.
DEFINE OUTPUT PARAMETER opcCostUOM                      AS   CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opdCostTotalExtended            AS   DECIMAL NO-UNDO.
DEFINE OUTPUT PARAMETER opcCostSource                   AS   CHARACTER NO-UNDO.

{sys/inc/var.i shared}  /*REFACTOR - to run conversion program*/

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
RUN pGetInvoiceLineCost(ipriInvl, 
    OUTPUT opdCostPerUOMDL,
    OUTPUT opdCostPerUOMFO, 
    OUTPUT opdCostPerUOMVO,
    OUTPUT opdCostPerUOMDM,
    OUTPUT opdCostPerUOMTotal, 
    OUTPUT opcCostUOM, 
    OUTPUT opdCostTotalExtended,
    OUTPUT opcCostSource).
                        


/* **********************  Internal Procedures  *********************** */

PROCEDURE pCalculateCostPerUOMFromBOL PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given criteria to find BOL lines, calculate the total cost of goods
            shipped, and the total quantity shipped
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany           AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiBNo               AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiOrderNo           AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFGItemID          AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustomerPONo      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMTotal  AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMDL     AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMFO     AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMVO     AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMDM     AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCostUOM          AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcSource           AS CHARACTER NO-UNDO.

    DEFINE VARIABLE dQtyShippedInM          AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dQtyShippedLineInM      AS DECIMAL   NO-UNDO.

    DEFINE VARIABLE dCostPerUOMTotalLine    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCostPerUOMDLLine       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCostPerUOMFOLine       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCostPerUOMVOLine       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCostPerUOMDMLine       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cCostUOMLine            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dCostPerUOMTotalLineDef AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCostPerUOMDLLineDef    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCostPerUOMFOLineDef    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCostPerUOMVOLineDef    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCostPerUOMDMLineDef    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cCostUOMLineDef         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dCostTotal              AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCostDL                 AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCostFO                 AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCostVO                 AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCostDM                 AS DECIMAL   NO-UNDO.    
    
    DEFINE VARIABLE lFound                  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cBOLLine                AS CHARACTER NO-UNDO.

    FOR EACH oe-boll NO-LOCK  
        WHERE oe-boll.company EQ ipcCompany
        AND oe-boll.b-no    EQ ipiBNo
        AND oe-boll.ord-no  EQ ipiOrderNo
        AND oe-boll.i-no    EQ ipcFGItemID
        AND oe-boll.po-no   EQ ipcCustomerPONo
        AND oe-boll.qty     NE 0
        USE-INDEX b-no :
            
        ASSIGN 
            dCostPerUOMTotalLine = 0
            dCostPerUOMDLLine    = 0
            dCostPerUOMFOLine    = 0
            dCostPerUOMVOLine    = 0
            dCostPerUOMDMLine    = 0
            cBOLLine             = " for BOL Line:" + STRING(oe-boll.bol-no,">>>>>>>") + "-" + STRING(oe-boll.bol-line,">>>")
            dQtyShippedLineInM   = oe-boll.qty / 1000
            dQtyShippedInM       = dQtyShippedInM + dQtyShippedLineInM
            cCostUOMLine         = ""
            .
        RUN pGetCostFromItem(oe-boll.company,
            oe-boll.i-no,
            OUTPUT dCostPerUOMTotalLineDef,
            OUTPUT dCostPerUOMDLLineDef,
            OUTPUT dCostPerUOMFOLineDef,
            OUTPUT dCostPerUOMVOLineDef,
            OUTPUT dCostPerUOMDMLineDef,
            OUTPUT cCostUOMLineDef,
            OUTPUT lFound).
        IF cCostUOMLineDef EQ "" THEN cCostUOMLineDef = "M".
        /*Find matching bin first*/
        RUN pGetCostFromBin(oe-boll.company, 
            oe-boll.i-no,
            oe-boll.tag,
            oe-boll.loc,
            oe-boll.loc-bin,
            oe-boll.job-no,
            oe-boll.job-no2,
            OUTPUT dCostPerUOMTotalLine,
            OUTPUT dCostPerUOMDLLine,
            OUTPUT dCostPerUOMFOLine,
            OUTPUT dCostPerUOMVOLine,
            OUTPUT dCostPerUOMDMLine,
            OUTPUT cCostUOMLine,
            OUTPUT lFound).
        IF lFound THEN 
            opcSource = opcSource + "FGBin" + cBOLLine + "," .
        ELSE 
        DO:
            /*Find matching receipt*/
            RUN pGetCostFromReceipt(oe-boll.company,
                oe-boll.i-no,
                oe-boll.tag,
                oe-boll.job-no,
                oe-boll.job-no2,
                OUTPUT dCostPerUOMTotalLine,
                OUTPUT dCostPerUOMDLLine,
                OUTPUT dCostPerUOMFOLine,
                OUTPUT dCostPerUOMVOLine,
                OUTPUT dCostPerUOMDMLine,
                OUTPUT cCostUOMLine,
                OUTPUT lFound).
            IF lFound THEN 
                opcSource = opcSource + "Rcpt" + cBOLLine + "," .
            ELSE 
            DO:
                /*Get the cost from Job*/
                RUN pGetCostFromJob(oe-boll.company,
                    oe-boll.i-no,
                    oe-boll.job-no,
                    oe-boll.job-no2,
                    OUTPUT dCostPerUOMTotalLine,
                    OUTPUT dCostPerUOMDLLine,
                    OUTPUT dCostPerUOMFOLine,
                    OUTPUT dCostPerUOMVOLine,
                    OUTPUT dCostPerUOMDMLine,
                    OUTPUT cCostUOMLine,
                    OUTPUT lFound).
                IF lFound THEN 
                    opcSource = opcSource + "Job" + cBOLLine + ",".
            END.    
        END.                
        IF NOT lFound THEN 
        DO:
            /*if no matches found, use standard costs from item*/
            ASSIGN 
                dCostPerUOMTotalLine = dCostPerUOMTotalLineDef
                dCostPerUOMDLLine = dCostPerUOMDLLineDef
                dCostPerUOMFOLine = dCostPerUOMFOLineDef
                dCostPerUOMVOLine = dCostPerUOMVOLineDef
                dCostPerUOMDMLine = dCostPerUOMDMLineDef
                cCostUOMLine = cCostUOMLineDef
                opcSource = opcSource + "Item" + cBOLLine + ",".
        END.
        IF cCostUOMLine EQ "" THEN cCostUOMLine = cCostUOMLineDef.
        IF cCostUOMLine NE "M" THEN /*convert all to per M*/
        DO:
            RUN pConvertCostToM(cCostUOMLine, dCostPerUOMTotalLine, OUTPUT dCostPerUOMTotalLine).
            RUN pConvertCostToM(cCostUOMLine, dCostPerUOMDLLine, OUTPUT dCostPerUOMDLLine).
            RUN pConvertCostToM(cCostUOMLine, dCostPerUOMFOLine, OUTPUT dCostPerUOMFOLine).
            RUN pConvertCostToM(cCostUOMLine, dCostPerUOMVOLine, OUTPUT dCostPerUOMVOLine).
            RUN pConvertCostToM(cCostUOMLine, dCostPerUOMDMLine, OUTPUT dCostPerUOMDMLine).
        END.         
        
        /*sum the total costs in order to calculate average cost per M*/
        ASSIGN 
            dCostTotal = dCostTotal + dCostPerUOMTotalLine * dQtyShippedLineInM  
            dCostDL    = dCostDL + dCostPerUOMDLLine * dQtyShippedLineInM
            dCostFO    = dCostFO + dCostPerUOMFOLine * dQtyShippedLineInM
            dCostVO    = dCostVO + dCostPerUOMVOLine * dQtyShippedLineInM
            dCostDM    = dCostDM + dCostPerUOMDMLine * dQtyShippedLineInM 
            .
            
    END.  /*Each oe-boll*/
    
    /*Calculate Average Costs for the total BOL qty*/
    ASSIGN 
        opcCostUOM         = "M"
        opdCostPerUOMDL    = dCostDL / dQtyShippedInM
        opdCostPerUOMFO    = dCostFO / dQtyShippedInM
        opdCostPerUOMVO    = dCostVO / dQtyShippedInM
        opdCostPerUOMDM    = dCostDM / dQtyShippedInM
        opdCostPerUOMTotal = opdCostPerUOMDL + opdCostPerUOMFO + opdCostPerUOMVO + opdCostPerUOMDM
        .
     
    
END PROCEDURE.

PROCEDURE pConvertCostToM PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Converts cost to UOM
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcUOM AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdCost AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCost AS DECIMAL NO-UNDO.
    
    RUN sys/ref/convcuom.p(ipcUOM, "M", 0, 0, 0, 0, ipdCost, OUTPUT opdCost).

END PROCEDURE.

PROCEDURE pGetCostFromBin PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given bin inputs, finds a mathing bin and returns costs
     Notes: Could add additional "loose" matches like not requiring loc and loc-bin
     or just match on job
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFGItemID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcTag AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcWhs AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcBin AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcJobNo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiJobNo2 AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMTotal AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMDL AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMFO AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMVO AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMDM AS DECIMAL NO-UNDO.  
    DEFINE OUTPUT PARAMETER opcCostUOM AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplFound AS LOGICAL NO-UNDO.
            

    FIND FIRST fg-bin NO-LOCK 
        WHERE fg-bin.company EQ ipcCompany
        AND fg-bin.i-no EQ ipcFGItemID
        AND fg-bin.tag EQ ipcTag
        AND fg-bin.loc EQ ipcWhs
        AND fg-bin.loc-bin EQ ipcBin
        AND fg-bin.job-no EQ ipcJobNo
        AND fg-bin.job-no2 EQ ipiJobNo2
        NO-ERROR.
    IF AVAILABLE fg-bin AND fg-bin.std-tot-cost NE 0 THEN 
        ASSIGN 
            oplFound           = YES
            opdCostPerUOMTotal = fg-bin.std-tot-cost
            opdCostPerUOMDL    = fg-bin.std-lab-cost
            opdCostPerUOMFO    = fg-bin.std-fix-cost
            opdCostPerUOMVO    = fg-bin.std-var-cost
            opdCostPerUOMDM    = fg-bin.std-mat-cost
            opcCostUOM         = fg-bin.pur-uom
            .

END PROCEDURE.

PROCEDURE pGetCostFromItem PRIVATE:
    /*------------------------------------------------------------------------------
         Purpose: Given bin inputs, finds a mathing bin and returns costs
         Notes: 
        ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFGItemID AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMTotal AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMDL AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMFO AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMVO AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMDM AS DECIMAL NO-UNDO.  
    DEFINE OUTPUT PARAMETER opcCostUOM AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplFound AS LOGICAL NO-UNDO.
    
    FIND FIRST itemfg NO-LOCK 
        WHERE itemfg.company EQ ipcCompany
        AND itemfg.i-no EQ ipcFGItemID
        NO-ERROR.
    IF AVAILABLE itemfg THEN DO:
        ASSIGN 
            oplFound           = YES
            opcCostUOM         = itemfg.prod-uom
            .
        IF itemfg.std-tot-cost NE 0 THEN 
            ASSIGN 
                opdCostPerUOMTotal = itemfg.std-tot-cost
                opdCostPerUOMDL    = itemfg.std-lab-cost
                opdCostPerUOMFO    = itemfg.std-fix-cost
                opdCostPerUOMVO    = itemfg.std-var-cost
                opdCostPerUOMDM    = itemfg.std-mat-cost
                .
        ELSE DO:
            FIND FIRST fg-ctrl NO-LOCK 
                WHERE fg-ctrl.company EQ itemfg.company
                NO-ERROR.
            IF AVAILABLE fg-ctrl AND fg-ctrl.inv-meth EQ "A" THEN 
                opdCostPerUOMTotal = itemfg.avg-cost.
            ELSE 
                opdCostPerUOMTotal = itemfg.last-cost.
        END.
    END.

END PROCEDURE.

PROCEDURE pGetCostFromJob PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given Job inputs, finds matching job and returns costs
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany           AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFGItemID          AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcJobNo             AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiJobNo2            AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMTotal  AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMDL     AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMFO     AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMVO     AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMDM     AS DECIMAL NO-UNDO.  
    DEFINE OUTPUT PARAMETER opcCostUOM          AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplFound            AS LOGICAL NO-UNDO.

    FIND FIRST job-hdr NO-LOCK 
        WHERE job-hdr.company EQ ipcCompany
        AND job-hdr.job-no EQ ipcJobNo
        AND job-hdr.job-no2 EQ ipiJobNo2      
        AND job-hdr.i-no EQ ipcFGItemID
        NO-ERROR.            
    IF AVAILABLE job-hdr AND job-hdr.std-tot-cost NE 0 THEN
        ASSIGN
            oplFound           = YES
            opcCostUOM         = "M"
            opdCostPerUOMTotal = job-hdr.std-tot-cost
            opdCostPerUOMDL    = job-hdr.std-lab-cost
            opdCostPerUOMFO    = job-hdr.std-fix-cost
            opdCostPerUOMVO    = job-hdr.std-var-cost
            opdCostPerUOMDM    = job-hdr.std-mat-cost
            .

END PROCEDURE.

PROCEDURE pGetCostFromReceipt PRIVATE:
    /*------------------------------------------------------------------------------
        Purpose: Given bin inputs, finds a mathing receipt tag and returns costs
        Notes: Could add additional "loose" matches like not requiring loc and loc-bin
        or just match on job
       ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFGItemID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcTag AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcJobNo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiJobNo2 AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMTotal AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMDL AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMFO AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMVO AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMDM AS DECIMAL NO-UNDO.  
    DEFINE OUTPUT PARAMETER opcCostUOM AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplFound AS LOGICAL NO-UNDO.
    
    DEFINE BUFFER b-fg-rcpth FOR fg-rcpth.
    DEFINE BUFFER b-fg-rdtlh FOR fg-rdtlh.
        
    
    IF ipcTag NE "" THEN 
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
                opdCostPerUOMFO    = b-fg-rdtlh.std-fix-cost   
                opdCostPerUOMDL    = b-fg-rdtlh.std-lab-cost   
                opdCostPerUOMDM    = b-fg-rdtlh.std-mat-cost    
                opdCostPerUOMTotal = b-fg-rdtlh.std-tot-cost    
                opdCostPerUOMVO    = b-fg-rdtlh.std-var-cost    
                opcCostUOM         = b-fg-rcpth.pur-uom
                oplFound           = YES
                .
            LEAVE each-fg. 
        END. /* each fg-rcp */
    END.
    
END PROCEDURE.

PROCEDURE pGetInvoiceLineCost PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Main Wrapper Program for Main Block
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipriInvl                AS ROWID     NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMDL         AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMFO         AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMVO         AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMDM         AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMTotal      AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCostUOM              AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostTotalExtended    AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCostSource           AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cCompany      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iBNo          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iOrderNo      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cFGItemID     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustomerPONo AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE cJobNo        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iJobNo2       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dQtyInvoiced  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lFound        AS LOGICAL   NO-UNDO.


    RUN pGetKeyCriteriaFromInvl(ipriInvl, 
        OUTPUT cCompany, 
        OUTPUT iBNo, 
        OUTPUT iOrderNo,
        OUTPUT cFGItemID,
        OUTPUT cCustomerPONo,
        OUTPUT cJobNo,
        OUTPUT iJobNo2,
        OUTPUT dQtyInvoiced,
        OUTPUT lFound).
    IF lFound THEN 
    DO:        
        RUN pCalculateCostPerUOMFromBOL(cCompany,
            iBNo,
            iOrderNo,
            cFGItemID,
            cCustomerPONo,
            OUTPUT opdCostPerUOMTotal,
            OUTPUT opdCostPerUOMDL,
            OUTPUT opdCostPerUOMFO,
            OUTPUT opdCostPerUOMVO,
            OUTPUT opdCostPerUOMDM,
            OUTPUT opcCostUOM,
            OUTPUT opcCostSource).
        IF opdCostPerUOMTotal EQ 0 OR opdCostPerUOMTotal EQ ? THEN 
        DO:
            RUN pGetCostFromItem(cCompany,
                cFGItemID,
                OUTPUT opdCostPerUOMTotal,
                OUTPUT opdCostPerUOMDL,
                OUTPUT opdCostPerUOMFO,
                OUTPUT opdCostPerUOMVO,
                OUTPUT opdCostPerUOMDM,
                OUTPUT opcCostUOM,
                OUTPUT lFound).
            IF lFound THEN
                opcCostSource = "ItemFallback".
        END.
        IF opcCostUOM NE "M" THEN 
        DO: 
            RUN pConvertCostToM(opcCostUOM, opdCostPerUOMTotal, OUTPUT opdCostPerUOMTotal).
            RUN pConvertCostToM(opcCostUOM, opdCostPerUOMDL, OUTPUT opdCostPerUOMDL).
            RUN pConvertCostToM(opcCostUOM, opdCostPerUOMFO, OUTPUT opdCostPerUOMFO).
            RUN pConvertCostToM(opcCostUOM, opdCostPerUOMVO, OUTPUT opdCostPerUOMVO).
            RUN pConvertCostToM(opcCostUOM, opdCostPerUOMDM, OUTPUT opdCostPerUOMDM).
            opcCostUOM = "M".
        END.
        IF opdCostPerUOMTotal EQ ? THEN opdCostPerUOMTotal = 0.
        IF opdCostPerUOMDL EQ ? THEN opdCostPerUOMDL = 0.
        IF opdCostPerUOMFO EQ ? THEN opdCostPerUOMFO = 0.
        IF opdCostPerUOMVO EQ ? THEN opdCostPerUOMVO = 0.
        IF opdCostPerUOMDM EQ ? THEN opdCostPerUOMDM = 0.
        opdCostTotalExtended = opdCostPerUOMTotal * dQtyInvoiced / 1000.
    END.
    ELSE 
        opcCostSource = "Invalid Inv Line".
        
END PROCEDURE.

PROCEDURE pGetKeyCriteriaFromInvl PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriInvl AS ROWID NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCompany AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiBNo AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiOrderNo AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcFGItemID AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCustomerPONo AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcJobNo AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiJobNo2 AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdQtyInvoiced AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplFound AS LOGICAL NO-UNDO.

    FIND inv-line NO-LOCK 
        WHERE ROWID(inv-line) EQ ipriInvl 
        NO-ERROR.
    IF NOT AVAILABLE inv-line THEN
        FIND ar-invl NO-LOCK 
            WHERE ROWID(ar-invl) EQ ipriInvl
            NO-ERROR.

    IF AVAILABLE inv-line OR AVAILABLE ar-invl THEN 
    DO:
        oplFound = YES.
        IF AVAILABLE inv-line THEN
            ASSIGN
                opcCompany      = inv-line.company
                opiBNo          = inv-line.b-no
                opiOrderNo      = inv-line.ord-no
                opcFGItemID     = inv-line.i-no
                opcCustomerPONo = inv-line.po-no
                opcJobNo        = inv-line.job-no
                opiJobNo2       = inv-line.job-no2
                opdQtyInvoiced  = inv-line.inv-qty
                .
        ELSE
            ASSIGN
                opcCompany      = ar-invl.company
                opiBNo          = ar-invl.b-no
                opiOrderNo      = ar-invl.ord-no
                opcFGItemID     = ar-invl.i-no
                opcCustomerPONo = ar-invl.po-no
                opcJobNo        = ar-invl.job-no
                opiJobNo2       = ar-invl.job-no2
                opdQtyInvoiced  = ar-invl.inv-qty
                .

        RELEASE inv-line.
        RELEASE ar-invl.
    END.
END PROCEDURE.

