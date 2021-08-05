
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


DEFINE TEMP-TABLE ttInv NO-UNDO 
    FIELD invoiceID                   AS INTEGER   LABEL "Invoice ID"
    FIELD invoiceIDString             AS CHARACTER LABEL "Invoice ID String"
    FIELD invoiceDate                 AS DATE      LABEL "Invoice Date"
    FIELD invoiceDueDate              AS DATE      LABEL "Invoice Due Date"
    FIELD customerID                  AS CHARACTER LABEL "Customer ID"
    FIELD customerName                AS CHARACTER LABEL "Customer Name"
    FIELD customerAddress1            AS CHARACTER LABEL "Customer Address1"
    FIELD customerAddress2            AS CHARACTER LABEL "Customer Address2"
    FIELD customerCity                AS CHARACTER LABEL "Customer City"
    FIELD customerState               AS CHARACTER LABEL "Customer State"
    FIELD customerPostalCode          AS CHARACTER LABEL "Customer Postal Code"
    FIELD customerAddressBlock        AS CHARACTER LABEL "Customer Address Block"
    FIELD customerEmail               AS CHARACTER LABEL "Customer Email"
    FIELD company                     AS CHARACTER LABEL "Company"
    FIELD shiptoID                    AS CHARACTER LABEL "Shipto ID"
    FIELD shiptoName                  AS CHARACTER LABEL "Shipto Name"
    FIELD shiptoAddress1              AS CHARACTER LABEL "Shipto Address1"
    FIELD shiptoAddress2              AS CHARACTER LABEL "Shipto Address2"
    FIELD shiptoCity                  AS CHARACTER LABEL "Shipto City"
    FIELD shiptoState                 AS CHARACTER LABEL "Shipto State"
    FIELD shiptoPostalCode            AS CHARACTER LABEL "Shipto Postal Code"
    FIELD shiptoAddressBlock          AS CHARACTER LABEL "Shipto Address Block"
    FIELD siteID                      AS CHARACTER LABEL "SiteID"
    FIELD terms                       AS CHARACTER LABEL "Terms"  
    FIELD termNetDays                 AS INTEGER   LABEL "Term Net Days"
    FIELD termDiscountDays            AS INTEGER   LABEL "Term Discount Days" 
    FIELD termNetDueDate              AS DATE      LABEL "Term Net Due Date"
    FIELD termDiscountDueDate         AS DATE      LABEL "Term Discount Due Date"
    FIELD termDiscountPercent         AS DECIMAL   LABEL "Term Discount Percent"
    FIELD termDiscountAmount          AS DECIMAL   LABEL "Term Discount Amount"
    FIELD customerPONo                AS CHARACTER LABEL "Customer PO No"
    FIELD payloadID                   AS CHARACTER LABEL "Payload ID"
    FIELD amountTotalLines            AS DECIMAL   LABEL "Amount Total Lines"
    FIELD amountTotalTax              AS DECIMAL   LABEL "Amount Total Tax"
    FIELD amountTotalTaxable          AS DECIMAL   LABEL "Amount Total Taxable"
    FIELD amountTotalFreight          AS DECIMAL   LABEL "Amount Total Freight"
    FIELD amountTotalTaxableFreight   AS DECIMAL   LABEL "Amount Total Taxable Freight"
    FIELD amountTotalTaxFreight       AS DECIMAL   LABEL "Amount Total Tax Freight"
    FIELD amountTotalTaxExFreight     AS DECIMAL   LABEL "Amount Total Tax ExFreight"
    FIELD amountTotalTaxableExFreight AS DECIMAL   LABEL "Amount Total Taxable ExFreight"
    FIELD amountTotal                 AS DECIMAL   LABEL "Amount Total"
    FIELD taxGroup                    AS CHARACTER LABEL "Tax Group"
    FIELD billFreight                 AS LOGICAL   LABEL "Bill Freight"
    FIELD frtTaxRate                  AS DECIMAL   LABEL "Frt Tax Rate"
    FIELD invoiceRecKey               AS CHARACTER LABEL "Invoice RecKey" 
    FIELD invoiceNotes                AS CHARACTER LABEL "Invoice Notes"
    FIELD invoiceType                 AS CHARACTER LABEL "Invoice Type"
    FIELD fob                         AS CHARACTER LABEL "FOB"
    FIELD carrier                     AS CHARACTER LABEL "Carrier"
    .
DEFINE TEMP-TABLE ttInvLine NO-UNDO     
    FIELD invoiceID                AS INTEGER   LABEL "Invoice ID"
    FIELD company                  AS CHARACTER LABEL "Company"
    FIELD lineNo                   AS INTEGER   LABEL "Line No"
    FIELD quantity                 AS INTEGER   LABEL "Quantity"
    FIELD quantityUOM              AS CHARACTER LABEL "Quantity UOM"
    FIELD quantityInvoiced         AS INTEGER   LABEL "Quantity Invoiced"
    FIELD quantityInvoicedUOM      AS CHARACTER LABEL "Quantity Invoiced UOM"
    FIELD quantityOrderOriginal    AS INTEGER   LABEL "Quantity Order Original"
    FIELD quantityOrderOriginalUOM AS CHARACTER LABEL "Quantity Order Original UOM"
    FIELD pricePerUOM              AS DECIMAL   LABEL "Price Per UOM"
    FIELD pricePerEach             AS DECIMAL   LABEL "Price Per Each"
    FIELD priceUOM                 AS CHARACTER LABEL "Price UOM"
    FIELD priceTotal               AS DECIMAL   LABEL "Price Total"
    FIELD customerPartID           AS CHARACTER LABEL "Customer Part ID"
    FIELD itemID                   AS CHARACTER LABEL "Item ID"
    FIELD itemName                 AS CHARACTER LABEL "Item Name"
    FIELD itemDescription          AS CHARACTER LABEL "Item Description"
    FIELD amountTax                AS DECIMAL   LABEL "Amount Tax"
    FIELD amountTaxExFreightTax    AS DECIMAL   LABEL "Amount Tax ExFreight Tax"
    FIELD amountFreightTax         AS DECIMAL   LABEL "Amount Freight Tax"
    FIELD amountTaxable            AS DECIMAL   LABEL "Amount Taxable"
    FIELD amountTaxableExFreight   AS DECIMAL   LABEL "Amount Taxable ExFreight"
    FIELD amountTaxableFreight     AS DECIMAL   LABEL "Amount Taxable Freight"
    FIELD taxRate                  AS DECIMAL   LABEL "Tax Rate"
    FIELD amountFreight            AS DECIMAL   LABEL "Amount Freight"
    FIELD taxable                  AS LOGICAL   LABEL "Taxable"
    FIELD billable                 AS LOGICAL   LABEL "Billable"
    FIELD taxGroup                 AS CHARACTER LABEL "Tax Group"
    FIELD bNo                      AS INTEGER   LABEL "bNo"
    FIELD bolID                    AS INTEGER   LABEL "BOL ID"
    FIELD orderID                  AS INTEGER   LABEL "Order ID"
    FIELD orderLine                AS INTEGER   LABEL "Order Line"
    FIELD orderLineOverride        AS INTEGER   LABEL "Order Line Override"
    FIELD orderLineOverridden      AS INTEGER   LABEL "Order Line Overridden"
    FIELD taxRateFreight           AS DECIMAL   LABEL "Tax Rate Freight"
    FIELD customerPONo             AS CHARACTER LABEL "Customer PO No"
    FIELD customerPONoNoBlank      AS CHARACTER LABEL "Customer PO No No Blank"
    FIELD isMisc                   AS LOGICAL   LABEL "Misc"
    FIELD charge                   AS CHARACTER LABEL "Charge"
    FIELD chargeDescription        AS CHARACTER LABEL "Charge Description"
    .
DEFINE TEMP-TABLE ttTaxDetail NO-UNDO
    FIELD company                AS CHARACTER LABEL "Company"
    FIELD invoiceNo              AS INTEGER   LABEL "Invoice"
    FIELD invoiceLineType        AS CHARACTER LABEL "Line Type"
    FIELD invoiceLineRecKey      AS CHARACTER LABEL "Line RecKey"
    FIELD taxLine                AS INTEGER   LABEL "Tax"
    FIELD taxGroup               AS CHARACTER LABEL "Tax Group"
    FIELD taxGroupLine           AS INTEGER   LABEL "Tax Group Line"
    FIELD taxGroupTaxAmountLimit AS DECIMAL   LABEL "Tax Group Amount Limit"
    FIELD isFreight              AS LOGICAL   LABEL "Freight"
    FIELD isTaxOnFreight         AS LOGICAL   LABEL "Tax on Freight"
    FIELD isTaxOnTax             AS LOGICAL   LABEL "Tax on Tax"
    FIELD taxCode                AS CHARACTER LABEL "Tax Code"
    FIELD taxCodeDescription     AS CHARACTER LABEL "Tax Code Description"
    FIELD taxCodeRate            AS DECIMAL   LABEL "Tax Rate"
    FIELD taxCodeAccount         AS CHARACTER LABEL "Tax Code Account"
    FIELD taxCodeTaxAmount       AS DECIMAL   LABEL "Tax Code Amount"
    FIELD taxCodeTaxableAmount   AS DECIMAL   LABEL "Tax Code Taxable Amount"
    .

/* ***************************  Main Block  *************************** */
