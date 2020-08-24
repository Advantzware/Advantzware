
/*------------------------------------------------------------------------
    File        : TaxProcs.i
    Purpose     : Defines temp-tables for tax procs for inclusion when tax details are required by caller

    Syntax      :

    Description : Temp-table Definition File for TaxProcs

    Author(s)   : BV
    Created     : Thu Jun 25 22:59:12 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttTaxDetail NO-UNDO
    FIELD company                AS CHARACTER
    FIELD invoiceNo              AS INTEGER
    FIELD invoiceLineType        AS CHARACTER
    FIELD invoiceLineRecKey      AS CHARACTER
    FIELD taxLine                AS INTEGER
    FIELD taxGroup               AS CHARACTER
    FIELD taxGroupLine           AS INTEGER
    FIELD taxGroupTaxAmountLimit AS DECIMAL
    FIELD isFreight              AS LOGICAL
    FIELD isTaxOnFreight         AS LOGICAL
    FIELD isTaxOnTax             AS LOGICAL
    FIELD taxCode                AS CHARACTER 
    FIELD taxCodeDescription     AS CHARACTER
    FIELD taxCodeRate            AS DECIMAL
    FIELD taxCodeAccount         AS CHARACTER 
    FIELD taxCodeTaxAmount       AS DECIMAL
    FIELD taxCodeTaxableAmount   AS DECIMAL
    .
    

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
