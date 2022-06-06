/*------------------------------------------------------------------------
    File        : POProcs.p
    Purpose     : 

    Syntax      :

    Description : Holds procedures for entering, editing and processing purchase orders

    Author(s)   : Rahul Rawat
    Created     : Wed Mar 25 02:17:43 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE tt-eiv NO-UNDO
    FIELD run-qty  AS DECIMAL DECIMALS 3 EXTENT 20
    FIELD run-cost AS DECIMAL DECIMALS 4 EXTENT 20
    FIELD setups   AS DECIMAL DECIMALS 2 EXTENT 20.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

PROCEDURE CheckPOLineStatus:
/*------------------------------------------------------------------------------
 Purpose: To Check a PO line status
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiPoNo    AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiPoLine  AS INTEGER   NO-UNDO.
    
    FIND FIRST po-ordl NO-LOCK
         WHERE po-ordl.company EQ ipcCompany
           AND po-ordl.po-no   EQ ipiPoNo
           AND po-ordl.line    EQ ipiPoLine
         NO-ERROR.
    IF AVAILABLE po-ordl AND NOT po-ordl.opened THEN DO:
        RUN DisplayMessage("19").
        RETURN ERROR.
    END. 


END PROCEDURE.
    
PROCEDURE pCalLineTotalCostAndConsQty PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  calculate po-ordl.t-cost po-ordl.cons-qty field 
        Note - 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriPOOrdl AS ROWID NO-UNDO.
       
    DEFINE VARIABLE ld-qty   LIKE po-ordl.ord-qty NO-UNDO.
    DEFINE VARIABLE dBasis-w AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dLen     LIKE po-ordl.s-len NO-UNDO.
    DEFINE VARIABLE dWid     LIKE po-ordl.s-wid NO-UNDO.
    DEFINE VARIABLE dDep     LIKE po-ordl.s-len NO-UNDO.    
    DEFINE VARIABLE lError   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE dOrdQty  LIKE po-ordl.ord-qty NO-UNDO.
    DEFINE VARIABLE dConsQty LIKE po-ordl.ord-qty NO-UNDO.
    
    DEFINE BUFFER bf-po-ordl FOR po-ordl.    
    
    FIND bf-po-ordl EXCLUSIVE-LOCK 
        WHERE ROWID(bf-po-ordl) EQ ipriPOOrdl NO-ERROR.
        
    IF bf-po-ordl.item-type THEN 
    DO:  
        FIND FIRST item NO-LOCK
            WHERE item.company EQ bf-po-ordl.company
            AND item.i-no    EQ bf-po-ordl.i-no
            NO-ERROR.            
    END.
    
    IF NOT AVAILABLE item THEN
        FIND FIRST itemfg NO-LOCK
            WHERE itemfg.company EQ bf-po-ordl.company
            AND itemfg.i-no    EQ bf-po-ordl.i-no
            NO-ERROR.
         
    ASSIGN
        dBasis-w = IF AVAILABLE item THEN item.basis-w ELSE 0
        dDep     = IF AVAILABLE ITEM THEN item.s-dep ELSE 0
        dLen     = bf-po-ordl.s-len
        dWid     = bf-po-ordl.s-wid
        ld-qty   = bf-po-ordl.ord-qty
        dOrdQty  = bf-po-ordl.ord-qty
        .    
    IF dLen EQ 0 AND AVAILABLE ITEM AND
        ITEM.i-code EQ "R" AND item.r-wid GT 0 THEN
    DO:
        dLen = 12.

        IF bf-po-ordl.pr-qty-uom EQ "ROLL" THEN
        DO:
            FIND FIRST uom NO-LOCK
                WHERE uom.uom EQ "ROLL" NO-ERROR.
    
            IF AVAILABLE uom THEN
                ASSIGN
                    bf-po-ordl.pr-qty-uom = "LF".
            dOrdQty = dOrdQty * uom.mult.
        END.
    END.     
    IF AVAILABLE bf-po-ordl THEN
    DO:
        dConsQty = dOrdQty.
  
        IF bf-po-ordl.cons-uom NE bf-po-ordl.pr-qty-uom AND
            (bf-po-ordl.item-type                           OR
            NOT DYNAMIC-FUNCTION("Conv_IsEAUOM",bf-po-ordl.company, bf-po-ordl.i-no, bf-po-ordl.cons-uom) OR
            NOT DYNAMIC-FUNCTION("Conv_IsEAUOM",bf-po-ordl.company, bf-po-ordl.i-no, bf-po-ordl.pr-qty-uom)) THEN 
        DO:
            
            IF (bf-po-ordl.pr-qty-uom EQ "CS") AND AVAIL(itemfg) THEN 
            DO:  
                /* for CS, convert to EA first */      
                dConsQty = dConsQty * itemfg.case-count.

                RUN sys/ref/convquom.p (INPUT "EA", 
                    INPUT (bf-po-ordl.cons-uom),
                    dBasis-w, dLen, dWid, dDep,
                    dConsQty,
                    OUTPUT dConsQty).    
                
            END.
            ELSE 
            DO:
                IF AVAILABLE itemfg THEN 
                    RUN Conv_QuantityFromUOMtoUOM(itemfg.company, 
                        itemfg.i-no, "FG", 
                        dConsQty, bf-po-ordl.pr-qty-uom,
                        bf-po-ordl.cons-uom, 
                        dBasis-w, dLen, dWid, dDep, 0, 
                        OUTPUT  dConsQty, OUTPUT lError, OUTPUT cMessage).
                ELSE 
                
                    RUN sys/ref/convquom.p(INPUT (bf-po-ordl.pr-qty-uom),
                        INPUT (bf-po-ordl.cons-uom),
                        dBasis-w, dLen, dWid, dDep,
                        dConsQty,
                        OUTPUT dConsQty).
            END.

        END.
        bf-po-ordl.cons-qty = dConsQty.                
    
        IF LOOKUP(bf-po-ordl.pr-uom,"L,LOT") GT 0 THEN
            bf-po-ordl.t-cost = bf-po-ordl.cost.
        ELSE 
        DO:               
            IF bf-po-ordl.pr-qty-uom NE bf-po-ordl.pr-uom       AND
                (bf-po-ordl.item-type                           OR
                NOT DYNAMIC-FUNCTION("Conv_IsEAUOM",bf-po-ordl.company, bf-po-ordl.i-no, bf-po-ordl.pr-qty-uom) OR
                NOT DYNAMIC-FUNCTION("Conv_IsEAUOM",bf-po-ordl.company, bf-po-ordl.i-no, bf-po-ordl.pr-uom))   THEN
            DO:   
                FIND FIRST itemfg
                    WHERE itemfg.company EQ bf-po-ordl.company
                    AND itemfg.i-no    EQ bf-po-ordl.i-no
                    NO-LOCK NO-ERROR.
                IF AVAILABLE itemfg THEN               
                    RUN Conv_QuantityFromUOMtoUOM(itemfg.company, 
                        itemfg.i-no, "FG", 
                        ld-qty,  bf-po-ordl.pr-qty-uom ,
                        bf-po-ordl.pr-uom, 
                        dBasis-w, dLen, dWid, dDep, 0, 
                        OUTPUT  ld-qty, OUTPUT lError, OUTPUT cMessage).                   
                ELSE
                    RUN sys/ref/convquom.p(bf-po-ordl.pr-qty-uom, bf-po-ordl.pr-uom,
                        dBasis-w, dLen, dWid, dDep,
                        ld-qty, OUTPUT ld-qty).  
            END.                     
            bf-po-ordl.t-cost = (ld-qty * bf-po-ordl.cost) + bf-po-ordl.setup.
        
        END.
    
        bf-po-ordl.t-cost = ROUND(bf-po-ordl.t-cost * ((100 - bf-po-ordl.disc) / 100),2).
    END.
    RELEASE bf-po-ordl.     
    
END PROCEDURE.    

PROCEDURE pGetPOLineAdderData PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Main logic to process all adders for a given PO line
        Note - this has the ability to override key dimensions for conversion purposes
        or for Screen-value processing
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPoNo     AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPoLine   AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdPoLen    AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdPoWid    AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdPoDep    AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCostUom  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOM AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostSetup AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcAdderText AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-po-ordl FOR po-ordl.
    DEFINE BUFFER bf-po-ordl-add FOR po-ordl-add.
    DEFINE BUFFER bf-item FOR ITEM.
    
    DEFINE VARIABLE dCostPerUOM AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lError AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    
    
        FOR EACH bf-po-ordl-add NO-LOCK    
            WHERE bf-po-ordl-add.company EQ ipcCompany
            AND bf-po-ordl-add.po-no   EQ ipiPoNo
            AND bf-po-ordl-add.line    EQ ipiPoLine,
            FIRST bf-item NO-LOCK 
            WHERE bf-item.company  EQ bf-po-ordl-add.company
            AND bf-item.i-no     EQ bf-po-ordl-add.adder-i-no
            AND bf-item.mat-type EQ "A" :
            
            IF ipcCostUOM NE bf-po-ordl-add.pr-uom THEN 
            DO:
                RUN Conv_ValueFromUOMToUOM (
                    INPUT  bf-po-ordl-add.company,
                    INPUT  bf-po-ordl-add.adder-i-no,
                    INPUT  "RM",
                    INPUT  bf-po-ordl-add.cost,
                    INPUT  bf-po-ordl-add.pr-uom, 
                    INPUT  ipcCostUOM,
                    INPUT  bf-item.basis-w,
                    INPUT  ipdPOLen,
                    INPUT  ipdPOWid,
                    INPUT  ipdPODep,
                    INPUT  0,
                    OUTPUT dCostPerUOM,
                    OUTPUT lError,
                    OUTPUT cMessage
                    ).
            END.
            ELSE 
                dCostPerUOM = bf-po-ordl-add.cost.
                 
            ASSIGN 
                opdCostPerUOM = opdCostPerUOM + dCostPerUOM
                opdCostSetup = opdCostSetup + bf-po-ordl-add.setup
                opcAdderText = opcAdderText + SUBSTR(bf-item.i-name,1,18) +
                    FILL(' ',19 - LENGTH(SUBSTR(bf-item.i-name,1,18))) +
                    STRING(bf-po-ordl-add.cost,'-z,zz9.99') + STRING(bf-po-ordl-add.setup,'-zzz9.99') + CHR(10)
                .          
        END.

END PROCEDURE.

PROCEDURE PO_CalLineTotalandTax:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iproPoOrd AS ROWID NO-UNDO.

    DEFINE VARIABLE deTaxAmount    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE deFrtTaxAmount AS DECIMAL NO-UNDO.   

    FIND po-ord WHERE ROWID(po-ord) EQ iproPoOrd NO-ERROR.

    IF AVAILABLE po-ord THEN 
    DO:
        ASSIGN
            po-ord.tax    = 0
            po-ord.t-cost = 0.

        /* add freight whether tax gr is blank or not */
        IF po-ord.fob-code EQ "ORIG" AND po-ord.frt-pay NE "P" THEN
            ASSIGN
                po-ord.t-cost = po-ord.t-freight.

        IF po-ord.tax-gr NE "" THEN 
        DO:    
            IF po-ord.fob-code EQ "ORIG" AND po-ord.frt-pay NE "P" THEN
            DO:
                RUN Tax_Calculate(INPUT po-ord.company,
                    INPUT po-ord.tax-gr,
                    INPUT TRUE,
                    INPUT po-ord.t-freight,
                    INPUT "", 
                    OUTPUT deFrtTaxAmount).
    
                ASSIGN
                    po-ord.tax = deFrtTaxAmount.
            END.   
        END.

        FOR EACH po-ordl NO-LOCK
            WHERE po-ordl.company EQ po-ord.company
            AND po-ordl.po-no   EQ po-ord.po-no:

            po-ord.t-cost = po-ord.t-cost + po-ordl.t-cost.
  
            IF po-ordl.tax THEN
            DO:
                RUN Tax_Calculate(INPUT po-ord.company,
                    INPUT po-ord.tax-gr,
                    INPUT FALSE,
                    INPUT po-ordl.t-cost,
                    INPUT po-ordl.i-no, 
                    OUTPUT deTaxAmount).
                po-ord.tax = po-ord.tax + (deTaxAmount).
            END.
    
            IF po-ordl.stat EQ "U" AND
                po-ord.stat NE "H"  AND
                po-ord.opened       THEN po-ord.stat = "U".
        END.

        ASSIGN
            po-ord.tax    = ROUND(po-ord.tax,2)
            po-ord.t-cost = ROUND(po-ord.t-cost,2) + po-ord.tax.
    END.
END PROCEDURE.

PROCEDURE PO_GetAddersCostInCostUOM:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPoNo     AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPoLine   AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdPoLen    AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdPoWid    AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdPoDep    AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCostUom  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOM AS DECIMAL   NO-UNDO.    
    
    DEFINE VARIABLE dCostSetup AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cAdderText AS CHARACTER NO-UNDO.
    
    RUN pGetPOLineAdderData(ipcCompany, ipiPoNo, ipiPoLine, ipdPoLen, ipdPoWid, ipdPODep, ipcCostUOM, OUTPUT opdCostPerUOM, OUTPUT dCostSetup, OUTPUT cAdderText).
    
END PROCEDURE.

PROCEDURE PO_GetAddersText:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPoNo       AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPoLine     AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcAdderText  AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE dCostSetup AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCostPerUOM AS DECIMAL NO-UNDO.
    
    RUN pGetPOLineAdderData(ipcCompany, ipiPoNo, ipiPoLine, 0, 0, 0, "", OUTPUT dCostPerUOM, OUTPUT dCostSetup, OUTPUT opcAdderText).

END PROCEDURE.

PROCEDURE PO_CreatePoAdders:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiPoNo       AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiPoLine     AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER IpcItemID     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdCostPerUom AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipdSetupCost  AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipcPriceUom   AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-Po-ordl-add FOR po-ordl-add.
    
    CREATE bf-po-ordl-add. 
    ASSIGN 
        bf-po-ordl-add.company    = ipcCompany
        bf-po-ordl-add.po-no      = ipiPoNo
        bf-po-ordl-add.line       = ipiPoLine
        bf-Po-ordl-add.adder-i-no = ipcItemID
        bf-Po-ordl-add.cost       = ipdCostPerUom
        bf-po-ordl-add.setup      = ipdSetupCost
        bf-po-ordl-add.pr-uom     = ipcPriceUom
        .
        
    RELEASE bf-po-ordl-add.
END PROCEDURE.

PROCEDURE PO_GetLineScoresAndTypes:
/*------------------------------------------------------------------------------
 Purpose: Procedure to fetch the score size and types from reftable
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPoID       AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPoLine     AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdScores     AS DECIMAL   NO-UNDO EXTENT 20.
    DEFINE OUTPUT PARAMETER opcScoreTypes AS CHARACTER NO-UNDO EXTENT 20.
    
    DEFINE VARIABLE iIndex         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE hdFormulaProcs AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lRecFound      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cSizeFormat    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReverseGrain  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iEstimateType  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cPanelType     AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-po-ordl   FOR po-ordl.
    
    FIND FIRST bf-po-ordl NO-LOCK
         WHERE bf-po-ordl.company EQ ipcCompany
           AND bf-po-ordl.po-no   EQ ipiPoID
           AND bf-po-ordl.line    EQ ipiPoLine
         NO-ERROR.
    IF NOT AVAILABLE bf-po-ordl THEN
        RETURN.

    RUN system/FormulaProcs.p PERSISTENT SET hdFormulaProcs.
    
    RUN Formula_BuildAndSavePanelDetailsForPO IN hdFormulaProcs (
        INPUT ROWID(bf-po-ordl)
        ).
    
    RUN Formula_GetReverseGrainAndEstimateTypeForPOLine IN hdFormulaProcs (
        INPUT  ROWID(bf-po-ordl),
        OUTPUT cReverseGrain,
        OUTPUT iEstimateType
        ).
    
    cPanelType = IF (cReverseGrain EQ "S" AND iEstimateType GE 5) OR cReverseGrain EQ "B" THEN
                     "L"
                 ELSE
                     "W".
                     
    RUN GetPanelScoreAndTypeForPO IN hdFormulaProcs (
        INPUT  bf-po-ordl.company,
        INPUT  bf-po-ordl.po-no,
        INPUT  bf-po-ordl.line,
        INPUT  cPanelType,
        OUTPUT opdScores,
        OUTPUT opcScoreTypes
        ).

    RUN sys/ref/nk1look.p (
        INPUT ipcCompany,     /* Company Code */ 
        INPUT "CECSCRN",      /* sys-ctrl name */
        INPUT "C",            /* Output return value */
        INPUT NO,             /* Use ship-to */
        INPUT NO,             /* ship-to vendor */
        INPUT "",             /* ship-to vendor value */
        INPUT "",             /* shi-id value */
        OUTPUT cSizeFormat, 
        OUTPUT lRecFound
        ).
    
    IF cSizeFormat NE "Decimal" THEN DO:                   
        DO iIndex = 1 TO EXTENT(opdScores):
            IF opdScores[IIndex] EQ 0 THEN
                NEXT.
            
            IF cSizeFormat EQ "16th's" THEN    
                RUN ConvertDecimalTo16ths IN hdFormulaProcs (
                    INPUT-OUTPUT opdScores[iIndex]
                    ).
            ELSE IF cSizeFormat EQ "32nd's" THEN
                RUN ConvertDecimalTo32nds IN hdFormulaProcs (
                    INPUT-OUTPUT opdScores[iIndex]
                    ).
        END.
    END.
    
    DELETE PROCEDURE hdFormulaProcs.
    
END PROCEDURE.

PROCEDURE PO_ProcessAdder:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER iproPoOrdl       AS ROWID        NO-UNDO.
    DEFINE INPUT PARAMETER iproJobMat       AS ROWID        NO-UNDO.
    DEFINE INPUT PARAMETER ipchCompanyID    AS CHARACTER    NO-UNDO.
    DEFINE INPUT PARAMETER ipdeBasisW       LIKE item.basis-w NO-UNDO.
    DEFINE INPUT PARAMETER ipdeS-Len        LIKE item.s-len   NO-UNDO.
    DEFINE INPUT PARAMETER ipdeS-Wid        LIKE item.s-wid   NO-UNDO.
    DEFINE INPUT PARAMETER ipdeS-Dep        LIKE item.s-dep   NO-UNDO.
    DEFINE INPUT PARAMETER ipdeAdder        AS DECIMAL        NO-UNDO EXTENT 2.

    DEFINE VARIABLE deCost        AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE deAddCost     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE deQtyComp     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE deSetup       LIKE e-item-vend.setup NO-UNDO.
    DEFINE VARIABLE dAdder-setup  AS DECIMAL   NO-UNDO.

    DEFINE VARIABLE dCostTotal         AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCostPerUOM        AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCostSetup         AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cCostUOM           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lError             AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hdPOProcs          AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lNewVendorItemCost AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cReturn            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound             AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE iCount             AS INTEGER   NO-UNDO.
    DEFINE VARIABLE loError            AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE chMessage          AS CHARACTER NO-UNDO.
    DEFINE BUFFER xjob-mat FOR job-mat.

    /* RUN NK1LOOKUP.P instead of using sys/inc/.i */
    RUN sys/ref/nk1look.p (ipchCompanyID, "VendItemCost", "L", NO, NO, "", "", OUTPUT cReturn, OUTPUT lFound).
    IF lFound THEN lNewVendorItemCost = IF cReturn EQ "Yes" THEN YES ELSE NO.

    RUN po/POProcs.p PERSISTENT SET hdPOProcs.

    FIND xjob-mat WHERE ROWID(xjob-mat) EQ iproJobMat NO-LOCK.

    FIND po-ordl WHERE ROWID(po-ordl) EQ iproPoOrdl EXCLUSIVE-LOCK.
    FIND FIRST po-ord WHERE
        po-ord.company EQ po-ordl.company AND
        po-ord.po-no EQ po-ordl.po-no
        NO-LOCK.

    ASSIGN
        ipdeAdder[1] = po-ordl.cost
        ipdeAdder[2] = po-ordl.cons-cost.

    DO WITH FRAME po-ordlf:
        FOR EACH job-mat NO-LOCK
            WHERE job-mat.company  EQ xjob-mat.company
            AND job-mat.job      EQ xjob-mat.job
            AND job-mat.frm      EQ xjob-mat.frm
            AND job-mat.job-no   EQ xjob-mat.job-no
            AND job-mat.job-no2  EQ xjob-mat.job-no2
            USE-INDEX seq-idx,

            FIRST item NO-LOCK
            WHERE item.company  EQ job-mat.company
            AND item.i-no     EQ job-mat.i-no
            AND item.mat-type EQ "A":
                
            ASSIGN 
                deCost  = 0
                deSetup = 0.
                
            IF lNewVendorItemCost THEN 
            DO:     
                RUN GetVendorCost(
                    INPUT  po-ordl.company, 
                    INPUT  item.i-no, 
                    INPUT  "RM", 
                    INPUT  po-ord.vend-no, 
                    INPUT  po-ord.cust-no, 
                    INPUT  "", 
                    INPUT  0, 
                    INPUT  0,
                    INPUT  po-ordl.ord-qty, 
                    INPUT  po-ordl.pr-qty-uom,
                    INPUT  po-ordl.s-len,
                    INPUT  po-ordl.s-wid, 
                    INPUT  po-ordl.s-dep,
                    INPUT  "IN", 
                    INPUT  item.basis-w, 
                    INPUT  "LB/EA", 
                    INPUT  YES,
                    OUTPUT dCostPerUOM, 
                    OUTPUT dCostSetup, 
                    OUTPUT cCostUOM,
                    OUTPUT dCostTotal, 
                    OUTPUT lError, 
                    OUTPUT cMessage).  
            
                ASSIGN 
                    deCost  = dCostPerUOM
                    deSetup = dCostSetup.  
            END.    
            ELSE 
            DO:
                FIND FIRST e-item NO-LOCK
                    WHERE e-item.company EQ po-ordl.company
                    AND e-item.i-no    EQ po-ordl.i-no
                    NO-ERROR.
        
                FIND FIRST e-item-vend NO-LOCK
                    WHERE e-item-vend.company EQ item.company
                    AND e-item-vend.i-no    EQ item.i-no
                    AND e-item-vend.vend-no EQ po-ord.vend-no
                    NO-ERROR.
            END.   
            IF AVAILABLE e-item AND AVAILABLE e-item-vend AND po-ord.vend-no NE "" THEN 
            DO:
                IF po-ordl.pr-qty-uom EQ e-item.std-uom THEN
                    deQtyComp = po-ordl.ord-qty.
                ELSE
                    RUN Conv_QuantityFromUOMToUOM(ipchCompanyID,
                        po-ordl.i-no,
                        po-ordl.item-type,
                        po-ordl.ord-qty,
                        po-ordl.pr-qty-uom, 
                        e-item.std-uom,
                        ipdeBasisW, 
                        ipdeS-Len, 
                        ipdeS-Wid, 
                        ipdeS-Dep,
                        0, 
                        OUTPUT deQtyComp,
                        OUTPUT loError,
                        OUTPUT chMessage).

                deSetup = 0.

                EMPTY TEMP-TABLE tt-eiv.
                CREATE tt-eiv.
                
                DO iCount = 1 TO 10:
                    ASSIGN
                        tt-eiv.run-qty[iCount]  = e-item-vend.run-qty[iCount]
                        tt-eiv.run-cost[iCount] = e-item-vend.run-cost[iCount]
                        tt-eiv.setups[iCount]   = e-item-vend.setups[iCount].
                END.
      
                IF AVAILABLE e-item-vend THEN
                DO:      
                    DO iCount = 1 TO 10:
                        ASSIGN
                            tt-eiv.run-qty[iCount + 10]  = e-item-vend.runQtyXtra[iCount]
                            tt-eiv.run-cost[iCount + 10] = e-item-vend.runCostXtra[iCount]
                            tt-eiv.setups[iCount + 10]   = e-item-vend.setupsXtra[iCount].
                    END.
                END.

                DO iCount = 1 TO 20:
                    IF deQtyComp LE tt-eiv.run-qty[iCount] THEN
                        LEAVE.
                END.
    
                ASSIGN
                    deSetup      = tt-eiv.setups[iCount]
                    deCost       = /*((*/ tt-eiv.run-cost[iCount] /** deQtyComp) + deSetup ) / deQtyComp*/
                    dadder-setup = dadder-setup + deSetup .
                /* This adds the Adder cost in */
                IF e-item.std-uom NE po-ordl.pr-uom THEN
                    RUN Conv_ValueFromUOMtoUOM(ipchCompanyID,
                        po-ordl.i-no,
                        po-ordl.item-type,
                        deCost,
                        e-item.std-uom, 
                        po-ordl.pr-uom, 
                        job-mat.basis-w,
                        job-mat.len, 
                        job-mat.wid, 
                        item.s-dep,
                        0, 
                        OUTPUT deCost,
                        OUTPUT loError,
                        OUTPUT chMessage).
            END.
            FIND FIRST po-ordl-add NO-LOCK 
                WHERE po-ordl-add.company    EQ po-ordl.company
                AND po-ordl-add.po-no      EQ po-ordl.po-no  
                AND po-ordl-add.line       EQ po-ordl.line   
                AND po-ordl-add.adder-i-no EQ job-mat.i-no 
                NO-ERROR. 
            IF AVAILABLE po-ordl-add THEN 
                RUN PO_UpdatePoAdders IN hdPOProcs(
                    INPUT po-ordl.company,
                    INPUT po-ordl.po-no,
                    INPUT po-ordl.line,
                    INPUT job-mat.i-no,
                    INPUT deCost,
                    INPUT deSetup,
                    INPUT po-ordl.pr-uom
                    ). 
            ELSE
                RUN PO_CreatePoAdders IN hdPOProcs(
                    INPUT po-ordl.company,
                    INPUT po-ordl.po-no,
                    INPUT po-ordl.line,
                    INPUT job-mat.i-no,
                    INPUT deCost,
                    INPUT deSetup,
                    INPUT po-ordl.pr-uom).
            ASSIGN 
                deAddCost    = deAddCost + deCost
                dadder-setup = dadder-setup + deSetup.
        END.
        ASSIGN
            po-ordl.cost      = po-ordl.cost + deAddCost
            po-ordl.cons-cost = po-ordl.cost
            po-ordl.setup     = po-ordl.setup + dadder-setup .

        IF po-ordl.pr-uom NE po-ordl.cons-uom THEN
            RUN Conv_ValueFromUOMtoUOM(ipchCompanyID,
                po-ordl.i-no,
                po-ordl.item-type,
                po-ordl.cost,
                po-ordl.pr-uom, 
                po-ordl.cons-uom,
                ipdeBasisW, 
                ipdeS-Len, 
                ipdeS-Wid, 
                ipdeS-Dep,
                0, 
                OUTPUT po-ordl.cons-cost,
                OUTPUT loError,
                OUTPUT chMessage).
    END.

    ASSIGN
        ipdeAdder[1] = po-ordl.cost      - ipdeAdder[1]
        ipdeAdder[2] = po-ordl.cons-cost - ipdeAdder[2].
 
    IF VALID-HANDLE(hdPOProcs) THEN 
        DELETE PROCEDURE hdPOProcs.
        
END PROCEDURE.

PROCEDURE PO_RecalculateCostsPO:
/*------------------------------------------------------------------------------
 Purpose: Public wrapper for pRecalculateCostPO
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriPOOrd AS ROWID NO-UNDO.
    
    RUN pRecalculateCostsPO(ipriPOOrd).
    

END PROCEDURE.



PROCEDURE PO_CalLineTotalCostAndConsQty:
/*------------------------------------------------------------------------------
 Purpose: Public wrapper for pCalLineTotalCostAndConsQty
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriPOOrdl AS ROWID NO-UNDO.
        
    RUN pCalLineTotalCostAndConsQty(ipriPOOrdl).    

END PROCEDURE.

PROCEDURE PO_UpdatePoAdders:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiPoNo       AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiPoLine     AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemID     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdCostPerUom AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipdSetupCost  AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipcPriceUom   AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-Po-ordl-add FOR po-ordl-add.
    
    FIND FIRST bf-Po-ordl-add EXCLUSIVE-LOCK 
         WHERE bf-Po-ordl-add.company    EQ ipcCompany
           AND bf-Po-ordl-add.po-no      EQ ipiPoNo
           AND bf-Po-ordl-add.line       EQ ipiPoLine
           AND bf-Po-ordl-add.adder-i-no EQ ipcItemID
         NO-ERROR.  
    IF AVAILABLE bf-Po-ordl-add THEN 
        ASSIGN 
            bf-Po-ordl-add.cost   = ipdCostPerUom
            bf-po-ordl-add.setup  = ipdSetupCost
            bf-po-ordl-add.pr-uom = ipcPriceUom
            .  
             
    RELEASE bf-Po-ordl-add. 
END PROCEDURE.

PROCEDURE PO_CheckPurchaseLimit:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-po-ord FOR po-ord.
    DEFINE OUTPUT PARAMETER oplCheckHold AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdPurchaseLimit AS DECIMAL NO-UNDO.
    
    DEFINE VARIABLE dPurchaseCost AS DECIMAL NO-UNDO.    
    DEFINE BUFFER bf-po-ordl FOR po-ordl.
    
    FIND FIRST users NO-LOCK
         WHERE users.user_id EQ USERID(LDBNAME(1)) NO-ERROR .
    opdPurchaseLimit = users.purchaseLimit .
         
    FOR EACH bf-po-ordl NO-LOCK
        WHERE bf-po-ordl.company EQ ipbf-po-ord.company
        AND bf-po-ordl.po-no EQ ipbf-po-ord.po-no :
        dPurchaseCost = dPurchaseCost + bf-po-ordl.t-cost .
    END.
    IF dPurchaseCost GT opdPurchaseLimit THEN
    DO:
        oplCheckHold = YES .
    END.
END PROCEDURE.

PROCEDURE pRecalculateCostsPO PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:  Runs pRecalculateCostPOLine for each po line of a given po-ord
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriPOOrd AS ROWID NO-UNDO.
    
    DEFINE BUFFER bf-po-ord FOR po-ord.
    DEFINE BUFFER bf-po-ordl FOR po-ordl.
    
    FIND bf-po-ord NO-LOCK 
        WHERE ROWID(bf-po-ord) EQ ipriPOOrd NO-ERROR.
    IF AVAILABLE bf-po-ord THEN DO:
        FOR EACH bf-po-ordl NO-LOCK 
            WHERE bf-po-ordl.company EQ bf-po-ord.company
            AND bf-po-ordl.po-no EQ bf-po-ord.po-no:
            RUN pRecalculateCostsPOLine(ROWID(bf-po-ordl)).
        END.
    END.

END PROCEDURE.

PROCEDURE pRecalculateCostsPOLine PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:  Given a PO line, recalculate the unit, setup, adder and total costs
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriPOOrdl AS ROWID NO-UNDO.
    
    DEFINE BUFFER bf-po-ordl FOR po-ordl.
    DEFINE BUFFER bf-po-ord FOR po-ord.
    DEFINE BUFFER bf-item FOR ITEM.
    
    DEFINE VARIABLE dCostPerUOM AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dCostSetup AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dCostTotal AS DECIMAL NO-UNDO.
    DEFINE VARIABLE cCostUOM AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lError AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dBasisWeight AS DECIMAL NO-UNDO.
    DEFINE VARIABLE cItemType AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dQtyInCostUOM AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dCostPerUOMAdders AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dCostSetupAdders AS DECIMAL NO-UNDO.
    DEFINE VARIABLE cAdderText AS CHARACTER NO-UNDO.
    
    FOR bf-po-ordl NO-LOCK 
        WHERE ROWID(bf-po-ordl) EQ ipriPOOrdl
        ,
        FIRST bf-po-ord NO-LOCK 
        WHERE bf-po-ord.company EQ bf-po-ordl.company
        AND bf-po-ord.po-no EQ bf-po-ordl.po-no:
        
        IF bf-po-ordl.item-type THEN DO:
            cItemType = 'RM'.
            FIND FIRST bf-item NO-LOCK 
            WHERE bf-item.company EQ bf-po-ordl.company
            AND bf-item.i-no EQ bf-po-ordl.i-no
            NO-ERROR.
            IF AVAILABLE bf-item THEN 
                dBasisWeight = bf-item.basis-w.
            
            RUN pGetPOLineAdderData(bf-po-ordl.company, 
                bf-po-ordl.po-no, 
                bf-po-ordl.line, 
                bf-po-ordl.s-len, 
                bf-po-ordl.s-wid, 
                bf-po-ordl.s-dep, 
                bf-po-ordl.pr-uom,
                OUTPUT dCostPerUOMAdders,
                OUTPUT dCostSetupAdders,
                OUTPUT cAdderText).                    
        END.
        ELSE 
            cItemType = 'FG'.
            
        RUN GetVendorCost(bf-po-ordl.company, 
            bf-po-ordl.i-no, 
            cItemType, 
            bf-po-ord.vend-no, 
            bf-po-ordl.cust-no, 
            "", 0, 0, /*Estimate/F/B*/
            bf-po-ordl.ord-qty, 
            bf-po-ordl.pr-qty-uom,
            bf-po-ordl.s-len, 
            bf-po-ordl.s-wid, 
            bf-po-ordl.s-dep, 
            "IN", 
            IF AVAILABLE bf-item THEN bf-item.basis-w ELSE 0, 
            "LB/EA", 
            NO,
            OUTPUT dCostPerUOM, 
            OUTPUT dCostSetup, 
            OUTPUT cCostUOM,
            OUTPUT dCostTotal, 
            OUTPUT lError, 
            OUTPUT cMessage).  
        
        IF lError THEN RETURN.
        
        
        ASSIGN 
            dCostPerUOM = dCostPerUOM + dCostPerUOMAdders
            dCostSetup = dCostSetup + dCostSetupAdders
            .
        
        IF bf-po-ordl.pr-uom NE bf-po-ordl.pr-qty-uom THEN 
            RUN Conv_QuantityFromUOMToUOM (
                bf-po-ordl.company,
                bf-po-ordl.i-no,
                cItemType,
                bf-po-ordl.ord-qty,
                bf-po-ordl.pr-qty-uom, 
                bf-po-ordl.pr-uom,
                dBasisWeight,
                bf-po-ordl.s-len, 
                bf-po-ordl.s-wid, 
                bf-po-ordl.s-dep,
                0,
                OUTPUT dQtyInCostUOM,
                OUTPUT lError,
                OUTPUT cMessage
                ). 
        ELSE 
            dQtyInCostUOM = bf-po-ordl.ord-qty.
                
        dCostTotal = dQtyInCostUOM * dCostPerUOM + dCostSetup.  
                                  
        IF bf-po-ordl.disc NE 0 THEN
            dCostTotal = dCostTotal * (1 - (bf-po-ordl.disc / 100)).
        IF dCostTotal NE 0 THEN DO:
            FIND CURRENT bf-po-ordl EXCLUSIVE-LOCK. 
            ASSIGN 
                bf-po-ordl.cost = dCostPerUOM
                bf-po-ordl.pr-uom = cCostUOM
                bf-po-ordl.setup = dCostSetup
                bf-po-ordl.t-cost = dCostTotal
                .
            FIND CURRENT bf-po-ordl NO-LOCK.             
        END.
    END.

END PROCEDURE.

