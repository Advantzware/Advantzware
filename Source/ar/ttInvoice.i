
/*------------------------------------------------------------------------
    File        : ttInvoice.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : DEVA$!
    Created     : Wed Jan 27 11:09:52 EST 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE TEMP-TABLE ttInv NO-UNDO 
    FIELD invoiceID                   AS INTEGER
    FIELD invoiceIDString             AS CHARACTER
    FIELD invoiceDate                 AS DATE
    FIELD invoiceDateString           AS CHARACTER
    FIELD customerID                  AS CHARACTER
    FIELD customerName                AS CHARACTER
    FIELD customerAddress1            AS CHARACTER
    FIELD customerAddress2            AS CHARACTER 
    FIELD customerCity                AS CHARACTER 
    FIELD customerState               AS CHARACTER 
    FIELD customerPostalCode          AS CHARACTER
    FIELD company                     AS CHARACTER
    FIELD companyName                 AS CHARACTER
    FIELD companyAddress1             AS CHARACTER
    FIELD companyAddress2             AS CHARACTER 
    FIELD companyCity                 AS CHARACTER 
    FIELD companyState                AS CHARACTER 
    FIELD companyPostalCode           AS CHARACTER 
    FIELD shiptoID                    AS CHARACTER
    FIELD shiptoName                  AS CHARACTER
    FIELD shiptoAddress1              AS CHARACTER
    FIELD shiptoAddress2              AS CHARACTER 
    FIELD shiptoCity                  AS CHARACTER 
    FIELD shiptoState                 AS CHARACTER 
    FIELD shiptoPostalCode            AS CHARACTER 
    FIELD termsDays                   AS INTEGER
    FIELD customerPO                  AS CHARACTER
    FIELD payloadID                   AS CHARACTER
    FIELD amountTotalLines            AS DECIMAL 
    FIELD amountTotalTax              AS DECIMAL 
    FIELD amountTotalTaxable          AS DECIMAL 
    FIELD amountTotalFreight          AS DECIMAL
    FIELD amountTotalTaxableFreight   AS DECIMAL 
    FIELD amountTotalTaxFreight       AS DECIMAL 
    FIELD amountTotalTaxExFreight     AS DECIMAL
    FIELD amountTotalTaxableExFreight AS DECIMAL
    FIELD amountTotal                 AS DECIMAL 
    FIELD taxGroup                    AS CHARACTER
    FIELD billFreight                 AS LOGICAL
    FIELD frtTaxRate                  AS DECIMAL
    .

DEFINE TEMP-TABLE ttInvLine NO-UNDO     
    FIELD invoiceID                AS INTEGER
    FIELD company                  AS CHARACTER
    FIELD lineNo                   AS INTEGER
    FIELD quantity                 AS INTEGER
    FIELD quantityUOM              AS CHARACTER 
    FIELD quantityInvoiced         AS INTEGER
    FIELD quantityInvoicedUOM      AS CHARACTER 
    FIELD quantityOrderOriginal    AS INTEGER
    FIELD quantityOrderOriginalUOM AS CHARACTER
    FIELD pricePerUOM              AS DECIMAL
    FIELD pricePerEach             AS DECIMAL
    FIELD priceUOM                 AS CHARACTER 
    FIELD priceTotal               AS DECIMAL 
    FIELD customerPartID           AS CHARACTER
    FIELD itemID                   AS CHARACTER 
    FIELD itemName                 AS CHARACTER 
    FIELD amountTax                AS DECIMAL 
    FIELD amountTaxExFreightTax    AS DECIMAL
    FIELD amountFreightTax         AS DECIMAL 
    FIELD amountTaxable            AS DECIMAL
    FIELD amountTaxableExFreight   AS DECIMAL 
    FIELD amountTaxableFreight     AS DECIMAL 
    FIELD taxRate                  AS DECIMAL
    FIELD amountFreight            AS DECIMAL 
    FIELD taxable                  AS LOGICAL
    FIELD billable                 AS LOGICAL
    FIELD taxGroup                 AS CHARACTER 
    FIELD orderID                  AS INTEGER 
    FIELD orderLine                AS INTEGER 
    FIELD taxRateFreight           AS DECIMAL 
    FIELD customerPONo             AS CHARACTER
    FIELD isMisc                   AS LOGICAL 
    FIELD charge                   AS CHARACTER
    FIELD chargeDescription        AS CHARACTER
    .

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