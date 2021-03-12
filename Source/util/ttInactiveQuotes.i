
/*------------------------------------------------------------------------
    File        : ttInactiveQuotes.i
    Purpose     : 

    Syntax      :

    Description : Temp Table defination of price matrix, Vend item cust and Quote hd

    Author(s)   : Goutam Sharma
    Created     : Fri Mar 12 05:17:54 EST 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


DEFINE TEMP-TABLE ttOePrmtxCsv
    FIELD company       AS CHARACTER LABEL "Company"
    FIELD custNo        AS CHARACTER LABEL "Customer"
    FIELD custype       AS CHARACTER LABEL "Cust Type"
    FIELD custShipId    AS CHARACTER LABEL "ShipID"
    FIELD itemID        AS CHARACTER LABEL "Item" FORMAT "X(15)"
    FIELD procat        AS CHARACTER LABEL "Category"
    FIELD effectiveDate AS DATE      LABEL "Effective Date"
    FIELD oldExpiryDate AS DATE      LABEL "Old Expiry Date"
    FIELD newExpiryDate AS DATE      LABEL "New Expiry Date"
    FIELD quantity1     AS INTEGER   LABEL "Quantity 1"  FORMAT ">>,>>>,>>9" 
    FIELD price1        AS DECIMAL   LABEL "Price 1"     FORMAT "->>,>>>,>>9.99<<<<"
    FIELD quantity2     AS INTEGER   LABEL "Quantity 2"  FORMAT ">>,>>>,>>9" 
    FIELD price2        AS DECIMAL   LABEL "Price 2"     FORMAT "->>,>>>,>>9.99<<<<"
    FIELD quantity3     AS INTEGER   LABEL "Quantity 3"  FORMAT ">>,>>>,>>9" 
    FIELD price3        AS DECIMAL   LABEL "Price 3"     FORMAT "->>,>>>,>>9.99<<<<"    
    FIELD quantity4     AS INTEGER   LABEL "Quantity 4"  FORMAT ">>,>>>,>>9" 
    FIELD price4        AS DECIMAL   LABEL "Price 4"     FORMAT "->>,>>>,>>9.99<<<<"
    FIELD quantity5     AS INTEGER   LABEL "Quantity 5"  FORMAT ">>,>>>,>>9" 
    FIELD price5        AS DECIMAL   LABEL "Price 5"     FORMAT "->>,>>>,>>9.99<<<<"
    FIELD quantity6     AS INTEGER   LABEL "Quantity 6"  FORMAT ">>,>>>,>>9" 
    FIELD price6        AS DECIMAL   LABEL "Price 6"     FORMAT "->>,>>>,>>9.99<<<<"
    FIELD quantity7     AS INTEGER   LABEL "Quantity 7"  FORMAT ">>,>>>,>>9" 
    FIELD price7        AS DECIMAL   LABEL "Price 7"     FORMAT "->>,>>>,>>9.99<<<<"
    FIELD quantity8     AS INTEGER   LABEL "Quantity 8"  FORMAT ">>,>>>,>>9" 
    FIELD price8        AS DECIMAL   LABEL "Price 8"     FORMAT "->>,>>>,>>9.99<<<<"
    FIELD quantity9     AS INTEGER   LABEL "Quantity 9"  FORMAT ">>,>>>,>>9" 
    FIELD price9        AS DECIMAL   LABEL "Price 9"     FORMAT "->>,>>>,>>9.99<<<<"
    FIELD quantity10    AS INTEGER   LABEL "Quantity 10" FORMAT ">>,>>>,>>9" 
    FIELD price10       AS DECIMAL   LABEL "Price 10"    FORMAT "->>,>>>,>>9.99<<<<"
    .
DEFINE TEMP-TABLE ttQuoteHdCsv
    FIELD company       AS CHARACTER              LABEL "Company"  
    FIELD loc           AS CHARACTER              LABEL "Location"
    FIELD quoteNo       AS INTEGER FORMAT ">>>>9" LABEL "Quote"
    FIELD estimate      AS CHARACTER              LABEL "Estimate"
    FIELD custID        AS CHARACTER              LABEL "Customer"
    FIELD quoteDate     AS DATE                   LABEL "Quote Date"
    FIELD deliveryDate  AS DATE                   LABEL "DeliveryDate"
    FIELD oldExpiryDate AS DATE                   LABEL "Old Expiry Date"
    FIELD newExpiryDate AS DATE                   LABEL "New Expiry Date"
    .
    
DEFINE TEMP-TABLE ttVendItemCostCsv
    FIELD company           AS CHARACTER                LABEL "Company"
    FIELD estimate          AS CHARACTER                LABEL "Estimate"
    FIELD formNo            AS INTEGER   FORMAT ">9"    LABEL "Form"
    FIELD blankNo           AS INTEGER   FORMAT ">9"    LABEL "Blank"
    FIELD itemID            AS CHARACTER FORMAT "X(20)" LABEL "Item"
    FIELD vendorID          AS CHARACTER FORMAT "X(10)" LABEL "Vendor"
    FIELD customerID        AS CHARACTER FORMAT "X(10)" LABEL "Customer"
    FIELD itemType          AS CHARACTER                LABEL "Item Type"
    FIELD UOM               AS CHARACTER FORMAT "X(5)"  LABEL "Vendor UOM"
    FIELD effectiveDate     AS DATE                     LABEL "Effective Date"
    FIELD oldExpirationDate AS DATE                     LABEL "Old Expiry Date"
    FIELD newExpirationDate AS DATE                     LABEL "New Expiry Date"
    .

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
