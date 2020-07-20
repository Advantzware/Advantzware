
/*------------------------------------------------------------------------
    File        : TaxProcs.p
    Purpose     : Persistent Procedure with multiple entry points

    Syntax      :

    Description : Holds all procedures and functions for processing AR and AP Tax

    Author(s)   : BV
    Created     : Wed Sep 26 16:46:23 EDT 2018
    Notes       :  Replaces ar\calctax.p and ar\calctext2.p  and custom\shptotax.i
    IMPORTANT NOTE:  This was reduced to a standard proc to simplify the work
    for ticket 35645 - it will eventually be a persistent proc library for all tax calculations
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{system\TaxProcs.i}

DEFINE VARIABLE cRoundMethod     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRoundMethodUp   AS CHARACTER NO-UNDO INITIAL "ROUNDUP".
DEFINE VARIABLE cRoundMethodDown AS CHARACTER NO-UNDO INITIAL "ROUNDDOWN".
DEFINE VARIABLE cCalcMethod      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCalcMethodAPI   AS CHARACTER NO-UNDO INITIAL "API".
DEFINE VARIABLE hdVertexProcs    AS HANDLE    NO-UNDO.

RUN system/VertexProcs.p PERSISTENT SET hdVertexProcs.

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fRoundValue RETURNS DECIMAL PRIVATE
    ( INPUT ipdValue AS DECIMAL, INPUT ipcRoundMethod AS CHARACTER, INPUT ipiDecimals AS INTEGER ) FORWARD.


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

PROCEDURE Tax_GetTaxableMisc:
    /*------------------------------------------------------------------------------
     Purpose: Determines if a given combination of customer, shipto and control file
     should result in taxable misc
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcShipID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcPrepCode AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplTaxable AS LOGICAL NO-UNDO.
    
    DEFINE BUFFER bf-cust   FOR cust.
    DEFINE BUFFER bf-shipto FOR shipto.
    
    RUN pSetCustBuffer(ipcCompany, ipcCustID, BUFFER bf-cust).
    RUN pSetShipToBuffer(ipcCompany, ipcCustID, ipcShipID, BUFFER bf-shipto).
    RUN pGetTaxableMisc(BUFFER bf-cust, BUFFER bf-shipto, ipcPrepCode, OUTPUT oplTaxable).
    
END PROCEDURE.

PROCEDURE Tax_GetTaxGroupAR:
    /*------------------------------------------------------------------------------
     Purpose: Returns Taxable Y/N and Sales Group given customer, shipto, and FG item inputs
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcShipID AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcTaxGroup AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-cust   FOR cust.
    DEFINE BUFFER bf-shipto FOR shipto.
    
    RUN pSetCustBuffer(ipcCompany, ipcCustID, BUFFER bf-cust).
    RUN pSetShipToBuffer(ipcCompany, ipcCustID, ipcShipID, BUFFER bf-shipto).
    RUN pGetTaxGroup(BUFFER bf-cust, BUFFER bf-shipto, OUTPUT opcTaxGroup). 
  
END PROCEDURE.

PROCEDURE Tax_GetTaxableAR:
    /*------------------------------------------------------------------------------
     Purpose: Determines if a given combination of customer, shipto and FG should
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcShipID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFGItemID AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplTaxable AS LOGICAL NO-UNDO.

    DEFINE BUFFER bf-cust   FOR cust.
    DEFINE BUFFER bf-shipto FOR shipto.
    DEFINE BUFFER bf-itemfg FOR itemfg.

    RUN pSetBuffers(ipcCompany, ipcCustID, ipcShipID, ipcFGItemID, BUFFER bf-cust, BUFFER bf-shipto, BUFFER bf-itemfg).
    RUN pGetTaxable(BUFFER bf-cust, BUFFER bf-shipto, BUFFER bf-itemfg, OUTPUT oplTaxable).

END PROCEDURE.

PROCEDURE pCalculate PRIVATE :
    /*------------------------------------------------------------------------------
     Purpose: Calculates tax amount for the given inputs
     Notes: Replaces ar/calctax.p and ar/calctax2.p
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTaxCode       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplIsThisFreight AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdTaxableAmount AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdTax           AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess       AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage       AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE iCount         AS INTEGER NO-UNDO.
    DEFINE VARIABLE iLine          AS INTEGER NO-UNDO.    
    DEFINE VARIABLE dTax           AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dTaxableAmount AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lIsNegative    AS LOGICAL NO-UNDO.

    DEFINE BUFFER bf-stax   FOR stax.
     
    RUN pSetStaxBuffer(
        INPUT  ipcCompany, 
        INPUT  ipcTaxCode, 
        BUFFER bf-stax
        ).
    IF NOT AVAILABLE bf-stax THEN 
    DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "Sales tax does not exist for tax code '" + ipcTaxCode + "'"
            .
        RETURN. 
    END.

    IF iplIsThisFreight AND NOT bf-stax.tax-frt1[1] THEN RETURN.

    /* Fetch Rounding method (Round Up or Round Down) */
    IF cRoundMethod EQ "" THEN
        RUN pGetRoundMethod (
            INPUT  bf-stax.company,
            OUTPUT cRoundMethod
            ).

    ASSIGN 
        dTaxableAmount = ABS(ipdTaxableAmount)
        lIsNegative = ipdTaxableAmount LT 0.
    
    DO iCount = 1 TO EXTENT(bf-stax.tax-rate1):
        /* Skip if tax code is blank. */
        IF bf-stax.tax-code1[iCount] EQ "" THEN
            NEXT.

        /*Apply the Dollar Limit*/
        IF iCount EQ 1 AND bf-stax.taxableLimit GT 0 AND NOT iplIsThisFreight THEN 
            dTaxableAmount = MIN(ABS(bf-stax.taxableLimit), ABS(ipdTaxableAmount)).  /*ABS required for credit processing*/
        ELSE IF iCount NE 1 AND NOT bf-stax.accum-tax THEN
            dTaxableAmount = ABS(ipdTaxableAmount). 
        
        ASSIGN 
            dTax = (dTaxableAmount * bf-stax.tax-rate1[iCount]) / 100
            dTax = fRoundValue (dTax, cRoundMethod, 2)
            .
        
        CREATE ttTaxDetail.
        ASSIGN 
            iLine = iLine + 1
            ttTaxDetail.company =  bf-stax.company
            ttTaxDetail.isFreight = iplIsThisFreight
            ttTaxDetail.isTaxOnFreight = bf-stax.tax-frt1[1]
            ttTaxDetail.isTaxOnTax = bf-stax.accum-tax
            ttTaxDetail.taxCode = bf-stax.tax-code1[iCount]
            ttTaxDetail.taxCodeAccount = bf-stax.tax-acc1[iCount]
            ttTaxDetail.taxCodeDescription = bf-stax.tax-dscr1[iCount]
            ttTaxDetail.taxCodeRate = bf-stax.tax-rate1[iCount]
            ttTaxDetail.taxCodeTaxableAmount = IF lIsNegative THEN - dTaxableAmount ELSE dTaxableAmount
            ttTaxDetail.taxCodeTaxAmount = IF lIsNegative THEN - dTax ELSE dTax
            ttTaxDetail.taxGroup = bf-stax.tax-group
            ttTaxDetail.taxGroupLine = iLine
            ttTaxDetail.taxGroupTaxAmountLimit = bf-stax.taxableLimit
            .

        /* Tax on tax - Build up taxable Amount with last tax calculation*/
        IF bf-stax.accum-tax THEN
            dTaxableAmount = MAX(dTaxableAmount,ABS(ipdTaxableAmount)) + dTax.


        /* Accumulate total tax amount for output */
        opdTax = opdTax + dTax.
    END.

    opdTax = IF lIsNegative THEN - opdTax ELSE opdTax. 
         
END PROCEDURE.

PROCEDURE pGetTaxableMisc PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given record buffers determine taxable flag for misc items
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-cust   FOR cust.
    DEFINE PARAMETER BUFFER ipbf-shipto FOR shipto.
    DEFINE INPUT PARAMETER ipcPrepCode AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplTaxable AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE lTaxableCust   AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lTaxableShipTo AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lTaxablePrep   AS LOGICAL NO-UNDO.
    
    IF NOT AVAILABLE ipbf-cust THEN RETURN.
    ASSIGN 
        lTaxableCust = ipbf-cust.sort EQ "Y"
        .    
    
    IF AVAILABLE ipbf-shipto THEN
        ASSIGN 
            lTaxableShipTo = ipbf-shipto.tax-mandatory
            .
    FIND FIRST prep NO-LOCK 
        WHERE prep.company EQ ipbf-cust.company
        AND prep.code EQ ipcPrepCode
        NO-ERROR.
    IF AVAILABLE prep THEN 
        lTaxablePrep = prep.taxable.
        
    IF AVAILABLE ipbf-shipto THEN 
        oplTaxable = lTaxableShipTo AND lTaxablePrep.
    ELSE 
        oplTaxable = lTaxableCust AND lTaxablePrep.
    

END PROCEDURE.

PROCEDURE pGetTaxGroup PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given record buffers determine taxable flag
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-cust   FOR cust.
    DEFINE PARAMETER BUFFER ipbf-shipto FOR shipto.
    DEFINE OUTPUT PARAMETER opcTaxGroup AS CHARACTER NO-UNDO.
  
    DEFINE VARIABLE cTaxGroupCust   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTaxGroupShipTo AS CHARACTER NO-UNDO.

    IF AVAILABLE ipbf-cust THEN cTaxGroupCust = ipbf-cust.tax-gr.
    IF AVAILABLE ipbf-shipto THEN cTaxGroupShipTo = ipbf-shipto.tax-code.
    
    IF cTaxGroupShipto NE "" 
        THEN 
        opcTaxGroup = cTaxGroupShipTo.
    ELSE 
        opcTaxGroup = cTaxGroupCust.

END PROCEDURE.

PROCEDURE pGetTaxable PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given record buffers determine taxable flag
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-cust   FOR cust.
    DEFINE PARAMETER BUFFER ipbf-shipto FOR shipto.
    DEFINE PARAMETER BUFFER ipbf-itemfg FOR itemfg.
    DEFINE OUTPUT PARAMETER oplTaxable AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE lTaxableCust   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lTaxableShipTo AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lTaxableFGItem AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cTaxGroup      AS CHARACTER NO-UNDO.
    
    IF AVAILABLE ipbf-cust THEN
        ASSIGN 
            lTaxableCust = ipbf-cust.sort EQ "Y"
            .
    
    IF AVAILABLE ipbf-shipto THEN
        ASSIGN 
            lTaxableShipTo = ipbf-shipto.tax-mandatory
            .
            
    IF AVAILABLE ipbf-itemfg THEN 
        ASSIGN 
            lTaxableFGItem = ipbf-itemfg.taxable
            .
    RUN pGetTaxGroup(BUFFER ipbf-cust, BUFFER ipbf-shipto, OUTPUT cTaxGroup).
    
    /*old logic*/    
    /*    oplTaxable = lTaxableCust AND cTaxGroup NE "" AND lTaxableFGItem.*/
    /*    IF NOT oplTaxable THEN                                           */
    /*        oplTaxable = lTaxableShipTo.                                 */
    
    /*new logic - 35645*/
    IF AVAILABLE ipbf-itemfg AND AVAILABLE ipbf-shipto THEN 
        oplTaxable = lTaxableFGItem AND lTaxableShipTo.
    ELSE IF AVAILABLE ipbf-itemfg AND NOT AVAILABLE ipbf-shipto THEN
            oplTaxable = lTaxableFGItem AND lTaxableCust.
        ELSE IF AVAILABLE ipbf-shipto AND NOT AVAILABLE ipbf-itemfg THEN  
                oplTaxable = lTaxableShipto.
            ELSE 
                oplTaxable = lTaxableCust.
                    

END PROCEDURE.

PROCEDURE pSetBuffers PRIVATE:
    /*------------------------------------------------------------------------------
         Purpose: Gets the main buffers given inputs
         Notes:
        ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcShipID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFGItemID AS CHARACTER NO-UNDO.
    DEFINE PARAMETER BUFFER opbf-cust   FOR cust.
    DEFINE PARAMETER BUFFER opbf-shipto FOR shipto.
    DEFINE PARAMETER BUFFER opbf-itemfg FOR itemfg.

    RUN pSetCustBuffer(ipcCompany, ipcCustID, BUFFER opbf-cust).   
    RUN pSetShiptoBuffer(ipcCompany, ipcCustID, ipcShipID, BUFFER opbf-shipto).   
    RUN pSetFGItemBuffer(ipcCompany, ipcFGItemID, BUFFER opbf-itemfg).
          
END PROCEDURE.

PROCEDURE pSetCustBuffer PRIVATE:
    /*------------------------------------------------------------------------------
         Purpose: Gets the cust buffer given company and custID
         Notes:
        ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustID AS CHARACTER NO-UNDO.
    DEFINE PARAMETER BUFFER ipbf-cust FOR cust.

    IF ipcCustID NE "" THEN 
        FIND FIRST ipbf-cust NO-LOCK 
            WHERE ipbf-cust.company EQ ipcCompany
            AND ipbf-cust.cust-no EQ ipcCustID
            NO-ERROR.
        
END PROCEDURE.

PROCEDURE pSetFGItemBuffer PRIVATE:
    /*------------------------------------------------------------------------------
         Purpose: Gets the fgitem buffer given company and FGItemID
         Notes:
        ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFGItemID AS CHARACTER NO-UNDO.
    DEFINE PARAMETER BUFFER ipbf-itemfg FOR itemfg.

    IF ipcFGItemID NE "" THEN 
        FIND FIRST ipbf-itemfg NO-LOCK 
            WHERE ipbf-itemfg.company EQ ipcCompany
            AND ipbf-itemfg.i-no EQ ipcFGItemID
            NO-ERROR.
        
END PROCEDURE.

PROCEDURE pSetShiptoBuffer PRIVATE:
    /*------------------------------------------------------------------------------
         Purpose: Gets the cust buffer given company, custID and shipToID
         Notes:
        ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcShipToID AS CHARACTER NO-UNDO.
    DEFINE PARAMETER BUFFER ipbf-shipto FOR shipto.

    IF ipcShipToID NE "" AND ipcCustID NE "" THEN 
        FIND FIRST ipbf-shipto NO-LOCK 
            WHERE ipbf-shipto.company EQ ipcCompany
            AND ipbf-shipto.cust-no EQ ipcCustID
            AND ipbf-shipto.ship-id EQ ipcShipToID
            NO-ERROR.
        
END PROCEDURE.

PROCEDURE pSetStaxBuffer PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Gets the Stax buffer given company and tax group
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcTaxGroup AS CHARACTER NO-UNDO.
    DEFINE PARAMETER BUFFER ipbf-stax FOR stax.

    FIND FIRST ipbf-stax NO-LOCK 
        WHERE ipbf-stax.company EQ ipcCompany
        AND ipbf-stax.tax-group EQ ipcTaxGroup
        NO-ERROR.

END PROCEDURE.

PROCEDURE pGetRoundMethod PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Returns the rounding method from NK1 setting SalesTaxRoundingMethod
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany     AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcRoundMethod AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO.
    
    IF cRoundMethod NE "" THEN 
    DO:
        opcRoundMethod = cRoundMethod.
        RETURN.
    END.
    
    RUN sys/ref/nk1look.p (
        INPUT  ipcCompany,           /* Company Code */
        INPUT  "SalesTaxRoundingMethod", /* sys-ctrl name */
        INPUT  "C",                  /* Output return value I - int-fld, L - log-flf, C - char-fld, D - dec-fld, DT - date-fld */
        INPUT  FALSE,                /* Use ship-to */
        INPUT  FALSE,                /* ship-to vendor */
        INPUT  "",                   /* ship-to vendor value */
        INPUT  "",                   /* shi-id value */
        OUTPUT cRoundMethod,
        OUTPUT lRecFound
        ). 
    IF NOT lRecFound OR cRoundMethod EQ "" THEN
        ASSIGN
            cRoundMethod   = cRoundMethodUp
            opcRoundMethod = cRoundMethod
            .
    ELSE
        opcRoundMethod = cRoundMethod.  
END PROCEDURE.

PROCEDURE pGetCalcMethod PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Returns the calculation method from NK1 setting SalesTaxCalcMethod
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCalcMethod AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO.
    
    IF cCalcMethod NE "" THEN DO:
        opcCalcMethod = cCalcMethod.
        RETURN.
    END.
    
    RUN sys/ref/nk1look.p (
        INPUT  ipcCompany,           /* Company Code */
        INPUT  "SalesTaxCalcMethod", /* sys-ctrl name */
        INPUT  "C",                  /* Output return value I - int-fld, L - log-flf, C - char-fld, D - dec-fld, DT - date-fld */
        INPUT  FALSE,                /* Use ship-to */
        INPUT  FALSE,                /* ship-to vendor */
        INPUT  "",                   /* ship-to vendor value */
        INPUT  "",                   /* shi-id value */
        OUTPUT cCalcMethod,
        OUTPUT lRecFound
        ). 

    opcCalcMethod = cCalcMethod.  
END PROCEDURE.

PROCEDURE Tax_Calculate:
    /*------------------------------------------------------------------------------
     Purpose: Calculate tax amount for given inputs 
     Notes: This replaces ar/calctax2.p
     Syntax:  RUN Tax_Calculate(ipcCompany, ipcTaxCode, iplIsThisFreight, ipdTaxableAmount, ipcItemID, OUTPUT opdTax).
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTaxCode       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplIsThisFreight AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdTaxableAmount AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItemID        AS CHARACTER NO-UNDO.  /*Deprecate*/
    DEFINE OUTPUT PARAMETER opdTax           AS DECIMAL   NO-UNDO.
    
    DEFINE VARIABLE lSuccess AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    
    EMPTY TEMP-TABLE ttTaxDetail.
    
    RUN pCalculate (
        INPUT  ipcCompany,
        INPUT  ipcTaxCode,
        INPUT  iplIsThisFreight,
        INPUT  ipdTaxableAmount,
        OUTPUT opdTax,
        OUTPUT lSuccess,
        OUTPUT cMessage
        ).
        
END PROCEDURE.

PROCEDURE pAPICalculateForInvHead PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Calculate tax through API for a given ar-inv rowid
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipriInvHead        AS ROWID     NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLocation        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcMessageType     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplPostToJournal   AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTriggerID       AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdTaxTotal        AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdInvoiceTotal    AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdInvoiceSubTotal AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess         AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage         AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-inv-head FOR inv-head.
    
    FIND FIRST bf-inv-head NO-LOCK 
         WHERE ROWID(bf-inv-head) EQ ipriInvHead
         NO-ERROR.
    IF NOT AVAILABLE bf-inv-head THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "Invalid inv-head row id"
            .
        RETURN.
    END.    

    IF cCalcMethod EQ "" THEN
        RUN pGetCalcMethod (
            INPUT  bf-inv-head.company,
            OUTPUT cCalcMethod
            ).
    
    IF cCalcMethod NE cCalcMethodAPI THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "NK1 setting SalesTaxCalcMethod is not set to 'API'"
            .
        RETURN.
    END.

    RUN Vertex_CalculateTaxForInvHead IN hdVertexProcs (
        INPUT  ROWID(bf-inv-head),
        INPUT  ipcLocation,
        INPUT  ipcMessageType,        /*  Message Type "INVOICE" or "QUOTATION" */
        INPUT  iplPostToJournal,      /* Post To journal */
        INPUT  ipcTriggerID,
        OUTPUT opdInvoiceTotal,
        OUTPUT opdInvoiceSubTotal,
        OUTPUT opdTaxTotal,
        OUTPUT oplSuccess,
        OUTPUT opcMessage    
        ).

    IF NOT oplSuccess THEN
        RETURN.
END PROCEDURE.

PROCEDURE pAPICalculateForArInv PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Calculate tax through API for a given ar-inv rowid
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipriArInv          AS ROWID     NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLocation        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcMessageType     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplPostToJournal   AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTriggerID       AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdTaxTotal        AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdInvoiceTotal    AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdInvoiceSubTotal AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess         AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage         AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-ar-inv FOR ar-inv.
    
    FIND FIRST bf-ar-inv NO-LOCK 
         WHERE ROWID(bf-ar-inv) EQ ipriArInv
         NO-ERROR.
    IF NOT AVAILABLE bf-ar-inv THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "Invalid ar-inv row id"
            .
        RETURN.
    END.    

    IF cCalcMethod EQ "" THEN
        RUN pGetCalcMethod (
            INPUT  bf-ar-inv.company,
            OUTPUT cCalcMethod
            ).
                
    IF cCalcMethod NE cCalcMethodAPI THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "NK1 setting SalesTaxCalcMethod is not set to 'API'"
            .
        RETURN.
    END.
    
    RUN Vertex_CalculateTaxForArInv IN hdVertexProcs (
        INPUT  ROWID(bf-ar-inv),
        INPUT  ipcLocation,
        INPUT  ipcMessageType,        /*  Message Type "INVOICE" or "QUOTATION" */
        INPUT  iplPostToJournal,      /* Post To journal */
        INPUT  ipcTriggerID,
        OUTPUT opdInvoiceTotal,
        OUTPUT opdInvoiceSubTotal,
        OUTPUT opdTaxTotal,
        OUTPUT oplSuccess,
        OUTPUT opcMessage    
        ).

    IF NOT oplSuccess THEN
        RETURN.    
END PROCEDURE.

PROCEDURE Tax_CalculateForInvHead:
/*------------------------------------------------------------------------------
 Purpose: Calculates tax for a given ar-inv row id
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipriInvHead        AS ROWID     NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLocation        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcMessageType     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplPostToJournal   AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdTaxTotal        AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdInvoiceTotal    AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdInvoiceSubTotal AS DECIMAL   NO-UNDO.    
    DEFINE OUTPUT PARAMETER oplSuccess         AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage         AS CHARACTER NO-UNDO.

    DEFINE VARIABLE dTax                AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dAPITaxTotal        AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dAPIInvoiceTotal    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dAPIInvoiceSubTotal AS DECIMAL NO-UNDO.    
        
    DEFINE BUFFER bf-inv-head FOR inv-head.
    DEFINE BUFFER bf-inv-line FOR inv-line.
    DEFINE BUFFER bf-inv-misc FOR inv-misc.
    
    FIND FIRST bf-inv-head NO-LOCK 
         WHERE ROWID(bf-inv-head) EQ ipriInvHead
         NO-ERROR.
    IF NOT AVAILABLE bf-inv-head THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "Invalid inv-head row id"
            .
        RETURN.
    END.    

    EMPTY TEMP-TABLE ttTaxDetail.

    FOR EACH bf-inv-line NO-LOCK 
        WHERE bf-inv-line.r-no EQ bf-inv-head.r-no:
        opdInvoiceSubTotal = opdInvoiceSubTotal + bf-inv-line.t-price.

        IF bf-inv-head.tax-gr NE "" AND bf-inv-line.tax THEN DO:
            RUN pCalculate (
                INPUT  bf-inv-head.company,
                INPUT  bf-inv-head.tax-gr,
                INPUT  FALSE,   /* Is this freight */
                INPUT  bf-inv-line.t-price,
                OUTPUT dTax,
                OUTPUT oplSuccess,
                OUTPUT opcMessage
                ).

            opdTaxTotal = opdTaxTotal + dTax.
        END.
    END.

    FOR EACH bf-inv-misc NO-LOCK 
        WHERE bf-inv-misc.company EQ bf-inv-head.company 
          AND bf-inv-misc.r-no    EQ bf-inv-head.r-no 
          AND bf-inv-misc.bill    EQ "Y":     
        opdInvoiceSubTotal = opdInvoiceSubTotal + bf-inv-misc.amt.

        IF bf-inv-head.tax-gr NE "" AND bf-inv-misc.tax THEN DO:
            RUN pCalculate (
                INPUT  bf-inv-head.company,
                INPUT  bf-inv-head.tax-gr,
                INPUT  FALSE,   /* Is this freight */
                INPUT  bf-inv-misc.amt,
                OUTPUT dTax,
                OUTPUT oplSuccess,
                OUTPUT opcMessage
                ).

            opdTaxTotal = opdTaxTotal + dTax.
        END.
    END.

    IF bf-inv-head.f-bill THEN
        opdInvoiceSubTotal = opdInvoiceSubTotal + bf-inv-head.t-inv-freight.

    IF bf-inv-head.tax-gr NE "" AND bf-inv-head.f-bill AND bf-inv-head.t-inv-freight NE 0 THEN DO:        
        RUN pCalculate (
            INPUT  bf-inv-head.company,
            INPUT  bf-inv-head.tax-gr,
            INPUT  TRUE,               /* Is this freight */
            INPUT  bf-inv-head.t-inv-freight,
            OUTPUT dTax,
            OUTPUT oplSuccess,
            OUTPUT opcMessage
            ).
        
        opdTaxTotal = opdTaxTotal + dTax.
    END.      
    
    opdInvoiceTotal = opdInvoiceSubTotal + opdTaxTotal.

    IF cCalcMethod EQ "" THEN
        RUN pGetCalcMethod (
            INPUT  bf-inv-head.company,
            OUTPUT cCalcMethod
            ).
    
    IF cCalcMethod EQ cCalcMethodAPI THEN DO:    
        RUN pAPICalculateForInvHead (
            INPUT  ipriInvHead,
            INPUT  ipcLocation,
            INPUT  ipcMessageType,
            INPUT  iplPostToJournal,
            INPUT  "GetTaxAmount", /* Trigger ID */
            OUTPUT dAPITaxTotal,
            OUTPUT dAPIInvoiceTotal,
            OUTPUT dAPIInvoiceSubTotal,
            OUTPUT oplSuccess,
            OUTPUT opcMessage
            ).
        IF oplSuccess THEN
            ASSIGN
                opdTaxTotal        = dAPITaxTotal
                opdInvoiceTotal    = dAPIInvoiceTotal
                opdInvoiceSubTotal = dAPIInvoiceSubTotal
                .     
    END.   
END PROCEDURE.

PROCEDURE Tax_CalculateForArInv:
/*------------------------------------------------------------------------------
 Purpose: Calculates tax for a given ar-inv row id
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipriArInv          AS ROWID     NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLocation        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcMessageType     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplPostToJournal   AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdTaxTotal        AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdInvoiceTotal    AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdInvoiceSubTotal AS DECIMAL   NO-UNDO.    
    DEFINE OUTPUT PARAMETER oplSuccess         AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage         AS CHARACTER NO-UNDO.

    DEFINE VARIABLE dTax                AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dAPITaxTotal        AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dAPIInvoiceTotal    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dAPIInvoiceSubTotal AS DECIMAL NO-UNDO.    
        
    DEFINE BUFFER bf-ar-inv  FOR ar-inv.
    DEFINE BUFFER bf-ar-invl FOR ar-invl.
    
    FIND FIRST bf-ar-inv NO-LOCK 
         WHERE ROWID(bf-ar-inv) EQ ipriArInv
         NO-ERROR.
    IF NOT AVAILABLE bf-ar-inv THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "Invalid ar-inv row id"
            .
        RETURN.
    END.    

    EMPTY TEMP-TABLE ttTaxDetail.

    FOR EACH bf-ar-invl NO-LOCK 
        WHERE bf-ar-invl.x-no EQ bf-ar-inv.x-no:
        opdInvoiceSubTotal = opdInvoiceSubTotal + bf-ar-invl.amt.

        IF bf-ar-inv.tax-code NE "" AND bf-ar-invl.tax THEN DO:
            RUN pCalculate (
                INPUT  bf-ar-inv.company,
                INPUT  bf-ar-inv.tax-code,
                INPUT  FALSE,   /* Is this freight */
                INPUT  bf-ar-invl.amt,
                OUTPUT dTax,
                OUTPUT oplSuccess,
                OUTPUT opcMessage
                ).

            opdTaxTotal = opdTaxTotal + dTax.
        
            IF bf-ar-inv.f-bill THEN DO:
                RUN pCalculate (
                    INPUT  bf-ar-invl.company,
                    INPUT  bf-ar-inv.tax-code,
                    INPUT  TRUE,   /* Is this freight */
                    INPUT  bf-ar-invl.t-freight,
                    OUTPUT dTax,
                    OUTPUT oplSuccess,
                    OUTPUT opcMessage
                    ). 
        
                opdTaxTotal = opdTaxTotal + dTax.
            END.
        END.        
    END.

    IF cCalcMethod EQ "" THEN
        RUN pGetCalcMethod (
            INPUT  bf-ar-inv.company,
            OUTPUT cCalcMethod
            ).
    
    IF cCalcMethod EQ cCalcMethodAPI THEN DO:    
        RUN pAPICalculateForArInv (
            INPUT  ipriArInv,
            INPUT  ipcLocation,
            INPUT  ipcMessageType,
            INPUT  iplPostToJournal,
            INPUT  "GetTaxAmount", /* Trigger ID */
            OUTPUT dAPITaxTotal,
            OUTPUT dAPIInvoiceTotal,
            OUTPUT dAPIInvoiceSubTotal,
            OUTPUT oplSuccess,
            OUTPUT opcMessage
            ).
        IF oplSuccess THEN
            ASSIGN
                opdTaxTotal        = dAPITaxTotal
                opdInvoiceTotal    = dAPIInvoiceTotal
                opdInvoiceSubTotal = dAPIInvoiceSubTotal
                .     
    END.   
END PROCEDURE.

PROCEDURE Tax_CalculateWithDetail:
    /*------------------------------------------------------------------------------
     Purpose: Calculate tax amount for given inputs 
     Notes: This replaces ar/calctax2.p
     Syntax:  RUN Tax_Calculate(ipcCompany, ipcTaxCode, iplIsThisFreight, ipdTaxableAmount, ipcItemID, OUTPUT opdTax).
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTaxCode       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplIsThisFreight AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdTaxableAmount AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdTax           AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttTaxDetail.
    
    DEFINE VARIABLE lSuccess AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    
    EMPTY TEMP-TABLE ttTaxDetail.
    
    RUN pCalculate (
        INPUT  ipcCompany,
        INPUT  ipcTaxCode,
        INPUT  iplIsThisFreight,
        INPUT  ipdTaxableAmount,
        OUTPUT opdTax,
        OUTPUT lSuccess,
        OUTPUT cMessage
        ).
        
END PROCEDURE.


/* ************************  Function Implementations ***************** */

FUNCTION fRoundValue RETURNS DECIMAL PRIVATE
    ( INPUT ipdValue AS DECIMAL, INPUT ipcRoundMethod AS CHARACTER, INPUT ipiDecimals AS INTEGER ):
    /*------------------------------------------------------------------------------
     Purpose: Rounds/Truncates the given value using the rounding method
     Notes:
    ------------------------------------------------------------------------------*/	

    DEFINE VARIABLE dValue AS DECIMAL NO-UNDO.
    
    IF ipcRoundMethod EQ cRoundMethodDown THEN
        dValue = TRUNCATE( ipdValue, ipiDecimals ).
    ELSE
        dValue = ROUND( ipdValue, ipiDecimals ).
        
    RETURN dValue.
END FUNCTION.

