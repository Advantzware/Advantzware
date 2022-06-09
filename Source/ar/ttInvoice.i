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
    FIELD invoiceID                   AS INTEGER   FORMAT ">>>>>>>9"   LABEL "Invoice ID"
    FIELD invoiceIDString             AS CHARACTER FORMAT "x(8)"       LABEL "Invoice ID String"
    FIELD invoiceDate                 AS DATE      FORMAT "99/99/9999" LABEL "Invoice Date"
    FIELD invoiceDueDate              AS DATE      FORMAT "99/99/9999" LABEL "Invoice Due Date"
    FIELD customerID                  AS CHARACTER FORMAT "x(8)"       LABEL "Customer ID"
    FIELD customerName                AS CHARACTER FORMAT "x(30)"      LABEL "Customer Name"
    FIELD customerAddress1            AS CHARACTER FORMAT "x(30)"      LABEL "Customer Address1"
    FIELD customerAddress2            AS CHARACTER FORMAT "x(30)"      LABEL "Customer Address2"
    FIELD customerCity                AS CHARACTER FORMAT "x(15)"      LABEL "Customer City"
    FIELD customerState               AS CHARACTER FORMAT "x(2)"       LABEL "Customer State"
    FIELD customerPostalCode          AS CHARACTER FORMAT "x(10)"      LABEL "Customer Postal Code"
    FIELD customerAddressBlock        AS CHARACTER FORMAT "x(90)"      LABEL "Customer Address Block"
    FIELD customerEmail               AS CHARACTER FORMAT "x(30)"      LABEL "Customer Email"
    FIELD company                     AS CHARACTER FORMAT "x(3)"       LABEL "Company"
    FIELD shiptoID                    AS CHARACTER FORMAT "x(8)"       LABEL "Shipto ID"
    FIELD shiptoName                  AS CHARACTER FORMAT "x(30)"      LABEL "Shipto Name"
    FIELD shiptoAddress1              AS CHARACTER FORMAT "x(30)"      LABEL "Shipto Address1"
    FIELD shiptoAddress2              AS CHARACTER FORMAT "x(30)"      LABEL "Shipto Address2"
    FIELD shiptoCity                  AS CHARACTER FORMAT "x(15)"      LABEL "Shipto City"
    FIELD shiptoState                 AS CHARACTER FORMAT "x(2)"       LABEL "Shipto State"
    FIELD shiptoPostalCode            AS CHARACTER FORMAT "x(10)"      LABEL "Shipto Postal Code"
    FIELD shiptoAddressBlock          AS CHARACTER FORMAT "x(90)"      LABEL "Shipto Address Block"
    FIELD siteID                      AS CHARACTER FORMAT "x(8)"       LABEL "SiteID"
    FIELD terms                       AS CHARACTER FORMAT "x(5)"       LABEL "Terms"  
    FIELD termNetDays                 AS INTEGER   FORMAT ">>9"        LABEL "Term Net Days"
    FIELD termDiscountDays            AS INTEGER   FORMAT ">>9"        LABEL "Term Discount Days" 
    FIELD termNetDueDate              AS DATE      FORMAT "99/99/9999" LABEL "Term Net Due Date"
    FIELD termDiscountDueDate         AS DATE      FORMAT "99/99/9999" LABEL "Term Discount Due Date"
    FIELD termDiscountPercent         AS DECIMAL   FORMAT ">>9.99"     LABEL "Term Discount Percent"
    FIELD termDiscountAmount          AS DECIMAL   FORMAT "->>,>>9.99" LABEL "Term Discount Amount"
    FIELD customerPONo                AS CHARACTER FORMAT "x(15)"      LABEL "Customer PO No"
    FIELD payloadID                   AS CHARACTER FORMAT "x(8)"       LABEL "Payload ID"
    FIELD amountTotalLines            AS DECIMAL   FORMAT "->>,>>9.99" LABEL "Amount Total Lines"
    FIELD amountTotalTax              AS DECIMAL   FORMAT "->>,>>9.99" LABEL "Amount Total Tax"
    FIELD amountTotalTaxable          AS DECIMAL   FORMAT "->>,>>9.99" LABEL "Amount Total Taxable"
    FIELD amountTotalFreight          AS DECIMAL   FORMAT "->>,>>9.99" LABEL "Amount Total Freight"
    FIELD amountTotalTaxableFreight   AS DECIMAL   FORMAT "->>,>>9.99" LABEL "Amount Total Taxable Freight"
    FIELD amountTotalTaxFreight       AS DECIMAL   FORMAT "->>,>>9.99" LABEL "Amount Total Tax Freight"
    FIELD amountTotalTaxExFreight     AS DECIMAL   FORMAT "->>,>>9.99" LABEL "Amount Total Tax ExFreight"
    FIELD amountTotalTaxableExFreight AS DECIMAL   FORMAT "->>,>>9.99" LABEL "Amount Total Taxable ExFreight"
    FIELD amountTotal                 AS DECIMAL   FORMAT "->>,>>9.99" LABEL "Amount Total"
    FIELD taxGroup                    AS CHARACTER FORMAT "x(3)"       LABEL "Tax Group"
    FIELD billFreight                 AS LOGICAL   FORMAT "Yes/No"     LABEL "Bill Freight"
    FIELD frtTaxRate                  AS DECIMAL   FORMAT "->>,>>9.99" LABEL "Frt Tax Rate"
    FIELD invoiceRecKey               AS CHARACTER FORMAT "x(26)"      LABEL "Invoice RecKey" 
    FIELD invoiceNotes                AS CHARACTER FORMAT "x(90)"      LABEL "Invoice Notes"
    FIELD invoiceType                 AS CHARACTER FORMAT "x(8)"       LABEL "Invoice Type"
    FIELD fob                         AS CHARACTER FORMAT "x(12)"      LABEL "FOB"
    FIELD carrier                     AS CHARACTER FORMAT "x(30)"      LABEL "Carrier"
    FIELD emailKeyValues              AS CHARACTER FORMAT "x(30)"      LABEL "Email Key Values"
    FIELD phone                       AS CHARACTER FORMAT "X(12)"      LABEL "Phone"
    FIELD areaCode                    AS CHARACTER FORMAT "X(3)"       LABEL "Area Code"
    FIELD fax                         AS CHARACTER FORMAT "X(12)"      LABEL "Fax"
    FIELD country                     AS CHARACTER FORMAT "X(12)"      LABEL "Country"
    FIELD termsDesc                   AS CHARACTER FORMAT "X(30)"      LABEL "Terms Description"
    FIELD frtPay                      AS CHARACTER FORMAT "X(3)"       LABEL "Freight Pay Code"
    FIELD countryName                 AS CHARACTER FORMAT "X(30)"      LABEL "Country Name"
    FIELD currency                    AS CHARACTER FORMAT "X(3)"       LABEL "Currency Code"
    FIELD isEDIOrder                  AS LOGICAL   FORMAT "yes/no"     LABEL "EDI Order"
    .
DEFINE TEMP-TABLE ttInvLine NO-UNDO     
    FIELD invoiceID                AS INTEGER   FORMAT ">>>>>>>9"   LABEL "Invoice ID"
    FIELD company                  AS CHARACTER FORMAT "x(3)"       LABEL "Company"
    FIELD lineNo                   AS INTEGER   FORMAT ">>9"        LABEL "Line No"
    FIELD quantity                 AS INTEGER   FORMAT "->,>>>,>>9" LABEL "Quantity"
    FIELD quantityUOM              AS CHARACTER FORMAT "x(8)"       LABEL "Quantity UOM"
    FIELD quantityInvoiced         AS INTEGER   FORMAT "->,>>>,>>9" LABEL "Quantity Invoiced"
    FIELD quantityInvoicedUOM      AS CHARACTER FORMAT "x(8)"       LABEL "Quantity Invoiced UOM"
    FIELD quantityOrderOriginal    AS INTEGER   FORMAT "->,>>>,>>9" LABEL "Quantity Order Original"
    FIELD quantityOrderOriginalUOM AS CHARACTER FORMAT "x(8)"       LABEL "Quantity Order Original UOM"
    FIELD pricePerUOM              AS DECIMAL   FORMAT "->>,>>9.99" LABEL "Price Per UOM"
    FIELD pricePerEach             AS DECIMAL   FORMAT "->>,>>9.99" LABEL "Price Per Each"
    FIELD priceUOM                 AS CHARACTER FORMAT "x(8)"       LABEL "Price UOM"
    FIELD priceTotal               AS DECIMAL   FORMAT "->>,>>9.99" LABEL "Price Total"
    FIELD ediPrice                 AS DECIMAL   FORMAT "->>,>>9.99" LABEL "EDI Price"
    FIELD ediPriceUOM              AS CHARACTER FORMAT "x(8)"       LABEL "EDI Price UOM"
    FIELD customerPartID           AS CHARACTER FORMAT "x(15)"      LABEL "Customer Part ID"
    FIELD itemID                   AS CHARACTER FORMAT "x(15)"      LABEL "Item ID"
    FIELD itemName                 AS CHARACTER FORMAT "x(30)"      LABEL "Item Name"
    FIELD itemDescription          AS CHARACTER FORMAT "x(30)"      LABEL "Item Description"
    FIELD amountTax                AS DECIMAL   FORMAT "->>,>>9.99" LABEL "Amount Tax"
    FIELD amountTaxExFreightTax    AS DECIMAL   FORMAT "->>,>>9.99" LABEL "Amount Tax ExFreight Tax"
    FIELD amountFreightTax         AS DECIMAL   FORMAT "->>,>>9.99" LABEL "Amount Freight Tax"
    FIELD amountTaxable            AS DECIMAL   FORMAT "->>,>>9.99" LABEL "Amount Taxable"
    FIELD amountTaxableExFreight   AS DECIMAL   FORMAT "->>,>>9.99" LABEL "Amount Taxable ExFreight"
    FIELD amountTaxableFreight     AS DECIMAL   FORMAT "->>,>>9.99" LABEL "Amount Taxable Freight"
    FIELD taxRate                  AS DECIMAL   FORMAT "->>,>>9.99" LABEL "Tax Rate"
    FIELD amountFreight            AS DECIMAL   FORMAT "->>,>>9.99" LABEL "Amount Freight"
    FIELD taxable                  AS LOGICAL   FORMAT "Yes/No"     LABEL "Taxable"
    FIELD billable                 AS LOGICAL   FORMAT "Yes/No"     LABEL "Billable"
    FIELD taxGroup                 AS CHARACTER FORMAT "x(8)"       LABEL "Tax Group"
    FIELD bNo                      AS INTEGER   FORMAT "->,>>>,>>9" LABEL "bNo"
    FIELD bolID                    AS INTEGER   FORMAT "->,>>>,>>9" LABEL "BOL ID"
    FIELD orderID                  AS INTEGER   FORMAT "->,>>>,>>9" LABEL "Order ID"
    FIELD orderLine                AS INTEGER   FORMAT "->,>>>,>>9" LABEL "Order Line"
    FIELD orderLineOverride        AS INTEGER   FORMAT "->,>>>,>>9" LABEL "Order Line Override"
    FIELD orderLineOverridden      AS INTEGER   FORMAT "->,>>>,>>9" LABEL "Order Line Overridden"
    FIELD taxRateFreight           AS DECIMAL   FORMAT "->>,>>9.99" LABEL "Tax Rate Freight"
    FIELD customerPONo             AS CHARACTER FORMAT "x(15)"      LABEL "Customer PO No"
    FIELD customerPONoNoBlank      AS CHARACTER FORMAT "x(15)"      LABEL "Customer PO No No Blank"
    FIELD isMisc                   AS LOGICAL   FORMAT "Yes/No"     LABEL "Misc"
    FIELD charge                   AS CHARACTER FORMAT "x(20)"      LABEL "Charge"
    FIELD chargeDescription        AS CHARACTER FORMAT "x(20)"      LABEL "Charge Description"
    .
DEFINE TEMP-TABLE ttTaxDetail NO-UNDO
    FIELD company                AS CHARACTER FORMAT "x(3)"       LABEL "Company"
    FIELD invoiceNo              AS INTEGER   FORMAT "->,>>>,>>9" LABEL "Invoice"
    FIELD invoiceLineType        AS CHARACTER FORMAT "x(12)"      LABEL "Line Type"
    FIELD invoiceLineRecKey      AS CHARACTER FORMAT "x(26)"      LABEL "Line RecKey"
    FIELD taxLine                AS INTEGER   FORMAT "->,>>>,>>9" LABEL "Tax"
    FIELD taxGroup               AS CHARACTER FORMAT "x(3)"       LABEL "Tax Group"
    FIELD taxGroupLine           AS INTEGER   FORMAT ">>9"        LABEL "Tax Group Line"
    FIELD taxGroupTaxAmountLimit AS DECIMAL   FORMAT "->>,>>9.99" LABEL "Tax Group Amount Limit"
    FIELD isFreight              AS LOGICAL   FORMAT "Yes/No"     LABEL "Freight"
    FIELD isTaxOnFreight         AS LOGICAL   FORMAT "Yes/No"     LABEL "Tax on Freight"
    FIELD isTaxOnTax             AS LOGICAL   FORMAT "Yes/No"     LABEL "Tax on Tax"
    FIELD taxCode                AS CHARACTER FORMAT "x(8)"       LABEL "Tax Group"
    FIELD taxCodeDescription     AS CHARACTER FORMAT "x(25)"      LABEL "Tax Code Description"
    FIELD taxCodeRate            AS DECIMAL   FORMAT "->>,>>9.99" LABEL "Tax Rate"
    FIELD taxCodeAccount         AS CHARACTER FORMAT "x(21)"      LABEL "Tax Code Account"
    FIELD taxCodeTaxAmount       AS DECIMAL   FORMAT "->>,>>9.99" LABEL "Tax Code Amount"
    FIELD taxCodeTaxableAmount   AS DECIMAL   FORMAT "->>,>>9.99" LABEL "Tax Code Taxable Amount"
    .
