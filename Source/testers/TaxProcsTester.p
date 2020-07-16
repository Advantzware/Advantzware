
/*------------------------------------------------------------------------
    File        : TaxProcsTester.p
    Purpose     : 

    Syntax      :

    Description : Tester fr TaxProcs.p

    Author(s)   : BV
    Created     : Thu Jun 25 20:06:55 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{system\TaxProcs.i}

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

//RUN pTestCalculateTax.
RUN pTestCalculateTaxWithDetail.


/* **********************  Internal Procedures  *********************** */

PROCEDURE pTestCalculateTax PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO INITIAL '001'.
    DEFINE VARIABLE cTaxGroup AS CHARACTER NO-UNDO INITIAL 'TST'.
    DEFINE VARIABLE lIsThisFreight AS LOGICAL NO-UNDO INITIAL NO.
    DEFINE VARIABLE dTaxableAmount AS DECIMAL NO-UNDO INITIAL -10000.
    DEFINE VARIABLE cItemID AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dTax AS DECIMAL NO-UNDO.
    
    RUN Tax_Calculate(cCompany, cTaxGroup, lIsThisFreight, dTaxableAmount, cItemID, OUTPUT dTax).

    MESSAGE "Tax:" dTax
    VIEW-AS ALERT-BOX.
END PROCEDURE.

PROCEDURE pTestCalculateTaxWithDetail PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cCompany       AS CHARACTER NO-UNDO INITIAL '001'.
    DEFINE VARIABLE cTaxGroup      AS CHARACTER NO-UNDO INITIAL 'TST'.
    DEFINE VARIABLE lIsThisFreight AS LOGICAL   NO-UNDO INITIAL NO.
    DEFINE VARIABLE dTaxableAmount AS DECIMAL   NO-UNDO INITIAL -10000.
    DEFINE VARIABLE cItemID        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dTax           AS DECIMAL   NO-UNDO.
    
    RUN Tax_CalculateWithDetail(cCompany, cTaxGroup, lIsThisFreight, dTaxableAmount, OUTPUT dTax, OUTPUT TABLE ttTaxDetail).
    
    FOR EACH ttTaxDetail:
        DISPLAY ttTaxDetail.taxCode ttTaxDetail.taxCodeTaxAmount ttTaxDetail.taxCodeTaxableAmount ttTaxDetail.taxCodeAccount.
    END.
    
    MESSAGE "Tax: " dTax
    VIEW-AS ALERT-BOX.
    
END PROCEDURE.

