
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

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

PROCEDURE CalculateTaxStandard:
    /*------------------------------------------------------------------------------
     Purpose: Calculates Tax based on inputs, standard method
     Notes:  Replaces calctax.p but does not round
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcTaxCode AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplIsThisFreight AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdTaxableAmount AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdTax AS DECIMAL NO-UNDO INIT 0.

    DEFINE BUFFER bf-stax FOR stax.

    DEFINE VARIABLE dTaxTemp AS DECIMAL NO-UNDO.
    DEFINE VARIABLE iCount   AS INTEGER NO-UNDO.

    RUN pSetStaxBuffer(ipcCompany, ipcTaxCode, BUFFER bf-stax).
    IF NOT AVAILABLE bf-stax THEN RETURN.

    RUN pCalculateTaxStandard(BUFFER bf-stax, iplIsThisFreight, ipdTaxableAmount, OUTPUT opdTax).

END PROCEDURE.

PROCEDURE CalculateTaxVaried:
    /*------------------------------------------------------------------------------
     Purpose: Calculates Tax using varied method based on inputs
     Notes: Replaces calctax2.p but does not round
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcTaxCode AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplIsThisFreight AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdTaxableAmount AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdTax AS DECIMAL NO-UNDO INIT 0.

    DEFINE BUFFER bf-stax FOR stax.

    DEFINE VARIABLE dTaxTemp AS DECIMAL NO-UNDO.
    DEFINE VARIABLE iCount   AS INTEGER NO-UNDO.

    RUN pSetStaxBuffer(ipcCompany, ipcTaxCode, BUFFER bf-stax).
    IF NOT AVAILABLE bf-stax THEN RETURN.

    RUN pCalculateTaxVaried(BUFFER bf-stax, iplIsThisFreight, ipdTaxableAmount, OUTPUT opdTax).

END PROCEDURE.

PROCEDURE GetTaxableMisc:
/*------------------------------------------------------------------------------
 Purpose: Determines if a given combination of customer, shipto and control file
 should result in taxable misc
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcShipID AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplTaxable AS LOGICAL NO-UNDO.
    
    DEFINE BUFFER bf-cust   FOR cust.
    DEFINE BUFFER bf-shipto FOR shipto.
    
    RUN pSetCustBuffer(ipcCompany, ipcCustID, BUFFER bf-cust).
    RUN pSetShipToBuffer(ipcCompany, ipcCustID, ipcShipID, BUFFER bf-shipto).
    RUN pGetTaxableMisc(BUFFER bf-cust, BUFFER bf-shipto, OUTPUT oplTaxable).
    
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

PROCEDURE pCalculateTaxStandard PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Calculate Standard Tax based on stax buffer
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-stax FOR stax.
    DEFINE INPUT PARAMETER iplIsFreight AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdTaxableAmount AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdTax AS DECIMAL NO-UNDO.

    DEFINE VARIABLE dTaxTemp AS DECIMAL NO-UNDO.
    DEFINE VARIABLE iCount   AS INTEGER NO-UNDO.



    ASSIGN 
        opdTax = 0.

    DO iCount = 1 TO EXTENT(ipbf-stax.tax-rate1):
        /* Skip if tax code is blank. */
        IF ipbf-stax.tax-code1[iCount] = "" THEN NEXT.
        IF NOT iplIsFreight /*Not Freight*/ 
            OR ipbf-stax.tax-frt1[iCount] /*It freight and the tax freight is checked for this code*/ THEN 
        DO:
        
            dTaxTemp = ipdTaxableAmount * ipbf-stax.tax-rate1[iCount] / 100.  /*Calculate the tax based on taxable amount*/   
        
            /*if ipbf-stax.company eq "yes" then v-in = v-in + v-amt. */
            IF ipbf-stax.accum-tax THEN /*If tax on tax - Accumulate the taxable amount for next tax calculation*/
                ASSIGN ipdTaxableAmount = (ipdTaxableAmount + dTaxTemp).
    
            /* Accumulate total amount out. */
            ASSIGN 
                opdTax = (opdTax + dTaxTemp).
        END.
    END.

END PROCEDURE.

PROCEDURE pCalculateTaxVaried PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Calculate Tax using Varied method based on buffer
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-stax FOR stax.
    DEFINE INPUT PARAMETER iplIsFreight AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdTaxableAmount AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdTax AS DECIMAL NO-UNDO.

    DEFINE VARIABLE dTaxTemp                AS DECIMAL NO-UNDO.
    DEFINE VARIABLE iCount                  AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE dDollarLimit            AS DECIMAL NO-UNDO. /* Dollar limit amount */
    DEFINE VARIABLE dDollarLimitExceeded    AS DECIMAL NO-UNDO. /* Exceeded amount */
    DEFINE VARIABLE dDollarLimitTax         AS DECIMAL NO-UNDO INIT 0. /* Tax amount for dollar limit */
    DEFINE VARIABLE dDollarLimitExceededTax AS DECIMAL NO-UNDO INIT 0. /* Tax amount for exceeded. */
    
    /* Separate the dollar limit amt from the exceeded amount. */
    ASSIGN 
        dDollarLimit         = ipbf-stax.tax-rate1[5] /* Set to dollar limit. */
        dDollarLimitExceeded = (ipdTaxableAmount - dDollarLimit). /* exceeded amt */
        
    IF ipbf-stax.tax-code1[5] EQ "" 
        AND ipbf-stax.tax-dscr1[5] EQ "Dollar Limit"  /* Special configuration in tax group*/
        AND dDollarLimit GT 0 
        AND dDollarLimitExceeded GT 0 THEN 
    DO: /* and the invoice price exceeds the dollar limit... */  
        
        /*Varied Tax Calculation*/
        /* Dollar limit amt taxed with all taxes (first 4). */
        DO iCount = 1 TO 4:
            /* Skip if tax code is blank. */
            IF ipbf-stax.tax-code1[iCount] = "" THEN NEXT.
            
            /* If freight that should be taxed */
            IF NOT iplIsFreight OR ipbf-stax.tax-frt1[iCount] THEN 
            DO:
                /* Calculate the tax amount. */
                ASSIGN 
                    dDollarLimitTax = dDollarLimit * ipbf-stax.tax-rate1[iCount] / 100.

                /* if stax.accum-tax eq "yes" then add tax amount to item total price. */
                IF ipbf-stax.accum-tax THEN 
                    ASSIGN ipdTaxableAmount = (dDollarLimit + dDollarLimitTax).

                /* Accumulate total amount out. */
                ASSIGN 
                    opdTax = opdTax + dDollarLimitTax.
            END.
        END.

        /* Tax the exceeded amount at first rate. */
        ASSIGN 
            dDollarLimitExceededTax = dDollarLimitExceeded * ipbf-stax.tax-rate1[1] / 100.

        /* Accumulate total tax amount out. */
        ASSIGN 
            opdTax = opdTax + dDollarLimitExceededTax.
        
    END.
    ELSE RUN pCalculateTaxStandard(BUFFER ipbf-stax, iplIsFreight, ipdTaxableAmount, OUTPUT opdTax).
  
END PROCEDURE.

PROCEDURE pGetTaxableMisc PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Given record buffers determine taxable flag for misc items
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-cust   FOR cust.
    DEFINE PARAMETER BUFFER ipbf-shipto FOR shipto.
    DEFINE OUTPUT PARAMETER oplTaxable AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE lTaxableCust   AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lTaxableShipTo AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lTaxMiscFromControl AS LOGICAL NO-UNDO.
    
    IF NOT AVAILABLE ipbf-cust THEN RETURN.
    ASSIGN 
        lTaxableCust = ipbf-cust.sort EQ "Y"
        .    
    
    IF AVAILABLE ipbf-shipto THEN
        ASSIGN 
            lTaxableShipTo = ipbf-shipto.tax-mandatory
            .
    FIND FIRST oe-ctrl NO-LOCK 
        WHERE oe-ctrl.company EQ ipbf-cust.company
        NO-ERROR.
    IF AVAILABLE oe-ctrl THEN 
        lTaxMiscFromControl = oe-ctrl.prep-chrg.

    IF AVAILABLE ipbf-shipto THEN 
        oplTaxable = lTaxableShipTo AND lTaxMiscFromControl.
    ELSE 
        oplTaxable = lTaxableCust AND lTaxMiscFromControl.
    

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

