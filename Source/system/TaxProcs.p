
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
DEFINE VARIABLE cRoundMethod     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRoundMethodUp   AS CHARACTER NO-UNDO INITIAL "ROUNDUP".
DEFINE VARIABLE cRoundMethodDown AS CHARACTER NO-UNDO INITIAL "ROUNDDOWN".
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fRoundValue RETURNS DECIMAL PRIVATE
	( INPUT ipdValue AS DECIMAL, INPUT ipcRoundMethod AS CHARACTER, INPUT ipiDecimals AS INTEGER ) FORWARD.


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

PROCEDURE GetTaxableMisc:
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

PROCEDURE GetTaxGroupAR:
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

PROCEDURE GetTaxableAR:
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

PROCEDURE pCalculate:
/*------------------------------------------------------------------------------
 Purpose: Calculates tax amount for the given inputs
 Notes: Replaces ar/calctax.p and ar/calctax2.p
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTaxCode       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplIsThisFreight AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdTaxableAmount AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItemID        AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdTax           AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess       AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage       AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lTaxVaried             AS LOGICAL NO-UNDO.
    DEFINE VARIABLE iCount                 AS INTEGER NO-UNDO.    
    DEFINE VARIABLE dTax                   AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dTaxableAmount         AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dTaxableLimitExcess    AS DECIMAL NO-UNDO. /* Excess amount over taxable limit */
    DEFINE VARIABLE dTaxableLimitExcessTax AS DECIMAL NO-UNDO. /* Tax on excess amount over taxable limit */

    DEFINE BUFFER bf-stax   FOR stax.
    DEFINE BUFFER bf-itemfg FOR itemfg.
     
    RUN pSetStaxBuffer(
        INPUT  ipcCompany, 
        INPUT  ipcTaxCode, 
        BUFFER bf-stax
        ).
    IF NOT AVAILABLE bf-stax THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "Sales tax does not exist for tax code '" + ipcTaxCode + "'"
            .
        RETURN. 
    END.
    
    IF ipcItemID NE "" THEN DO:
        FIND FIRST bf-itemfg NO-LOCK
             WHERE bf-itemfg.company EQ ipcCompany
               AND bf-itemfg.i-no    EQ ipcItemId
             NO-ERROR.
        IF AVAILABLE bf-itemfg THEN
            lTaxVaried = bf-itemfg.spare-char-2 = "YES".
        ELSE
            lTaxVaried = FALSE.    
    END.
    
    lTaxVaried = lTaxVaried AND (bf-stax.taxableLimit GT 0 AND ipdTaxableAmount GT bf-stax.taxableLimit).

    /* Set to taxable amount to taxable limit amount, if taxable limit amount 
       is greater than 0 and less than taxable amount  */     
    IF lTaxVaried THEN
        ASSIGN
            dTaxableAmount      = bf-stax.taxableLimit
            dTaxableLimitExcess = ipdTaxableAmount - bf-stax.taxableLimit
            .           
     ELSE
        ASSIGN
            dTaxableAmount         = ipdTaxableAmount
            dTaxableLimitExcessTax = 0
            .           
    
    /* Fetch Rounding method (Round Up or Round Down) */
    IF cRoundMethod EQ "" THEN
        RUN pGetRoundMethod (
            INPUT  bf-stax.company,
            OUTPUT cRoundMethod
            ).

    DO iCount = 1 TO EXTENT(bf-stax.tax-rate1):
        /* Skip if tax code is blank. */
        IF bf-stax.tax-code1[iCount] EQ "" THEN
            NEXT.
            
        /* If freight should be taxed */
        IF NOT iplIsThisFreight OR bf-stax.tax-frt1[1] THEN DO:
            /* Calculate the tax amount. */
            dTax = (dTaxableAmount * bf-stax.tax-rate1[iCount]) / 100.

            /* Tax on tax. Apply only if there is a taxable limit */
            IF bf-stax.accum-tax AND NOT lTaxVaried THEN
                dTaxableAmount = dTaxableAmount + dTax.

            /* Accumulate total amount out. */
            opdTax = opdTax + dTax.
        END.
    END.

    /* Tax the excess amount at first rate. */
    dTaxableLimitExcessTax = (dTaxableLimitExcess * bf-stax.tax-rate1[1]) / 100.

    ASSIGN
        opdTax = opdTax + dTaxableLimitExcessTax.    
        opdTax = fRoundValue (opdTax, cRoundMethod, 2)
        .
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
    
    DEFINE VARIABLE lTaxableCust   AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lTaxableShipTo AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lTaxableFGItem AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cTaxGroup AS CHARACTER NO-UNDO.
    
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
    
    IF cRoundMethod NE "" THEN DO:
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

PROCEDURE Tax_Calculate:
/*------------------------------------------------------------------------------
 Purpose: Calculate tax amount for given inputs 
 Notes: This replaces ar/calctax2.p
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTaxCode       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplIsThisFreight AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdTaxableAmount AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItemID        AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdTax           AS DECIMAL   NO-UNDO.
    
    DEFINE VARIABLE lSuccess AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.

    RUN pCalculate (
        INPUT  ipcCompany,
        INPUT  ipcTaxCode,
        INPUT  iplIsThisFreight,
        INPUT  ipdTaxableAmount,
        INPUT  ipcItemID,
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

