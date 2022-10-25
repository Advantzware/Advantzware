/* ttOnTimeDeliveries.i */

/* Open Order Report.rpa */ 
DEFINE TEMP-TABLE ttOnTimeDeliveries NO-UNDO
    {aoa/tempTable/ttFields.i}
    FIELD cPartNo          AS CHARACTER LABEL "Customer Part#" FORMAT "x(15)"  
    FIELD cFGItem          AS CHARACTER LABEL "FG Item#" FORMAT "X(15)"
    FIELD iOrdNo           AS INTEGER   LABEL "Order#" FORMAT ">>>>>>>9"
    FIELD dtOrdDate        AS DATE      LABEL "Ord Date" FORMAT 99/99/9999
    FIELD dtDueDate        AS DATE      LABEL "Due Date" FORMAT 99/99/9999
    FIELD dtBolDate        AS DATE      LABEL "Bol Date" FORMAT 99/99/9999
    FIELD cOnTime          AS CHARACTER LABEL "On-Time" FORMAT "X(8)"
    FIELD dtPromDate       AS DATE      LABEL "Prom Date" FORMAT 99/99/9999      
    FIELD cReason          AS CHARACTER LABEL "Reason" FORMAT "X(30)"
    FIELD dMsf             AS DECIMAL   LABEL "MSF" FORMAT "->>>,>>>,>>9.99"
    FIELD dWeight          AS DECIMAL   LABEL "Weight" FORMAT "->>>,>>>,>>9.99"
    FIELD cTrailer         AS CHARACTER LABEL "Trailer" FORMAT "x(25)"
    FIELD cCustGroup       AS CHARACTER LABEL "Customer Group" FORMAT "x(20)"
    FIELD cReceDate        AS CHARACTER LABEL "Last Receipt Date" FORMAT "x(15)"     
    FIELD iOrdQty          AS INTEGER   LABEL "Order Quantity" FORMAT "->>,>>>,>>9"
    FIELD cCustName        AS CHARACTER LABEL "Customer Name" FORMAT "X(30)"
    FIELD dUnitPrice       AS DECIMAL   LABEL "Item Unit Price" FORMAT "->,>>>,>>9.99"
    FIELD cPriceUom        AS CHARACTER LABEL "Item Price UOM" FORMAT "x(3)"          
    FIELD iPalletCount     AS INTEGER   LABEL "Pallet Count" FORMAT ">>>>9"
    FIELD dtManuDate       AS DATE      LABEL "Manufacture Date" FORMAT 99/99/9999 
    FIELD dtCompletionDate AS DATE      LABEL "Completion Date" FORMAT 99/99/9999     
    FIELD cFGCat           AS CHARACTER LABEL "FG Category" FORMAT "x(15)"
    FIELD iBolNo           AS INTEGER   LABEL "BOL Number" FORMAT ">>>>>>>>9"
    FIELD cBolCarrier      AS CHARACTER LABEL "BOL Carrier" FORMAT "x(20)"         
    FIELD iBolShipQty      AS INTEGER   LABEL "BOL Shipped Quantity" FORMAT "->>>,>>>,>>9"
    FIELD dShipmentValue   AS DECIMAL   LABEL "Shipment Value" FORMAT "->>,>>>,>>9.99"    
    FIELD iRelQty          AS INTEGER   LABEL "Release Quantity" FORMAT "->>,>>>,>>9"
    FIELD dtRelDueDate     AS DATE      LABEL "Release Due Date" FORMAT 99/99/9999 
    FIELD dtRelDate        AS DATE      LABEL "Release Date" FORMAT 99/99/9999 
    FIELD iRelNumber       AS INTEGER   LABEL "Release Number" FORMAT ">>>>>>>9" 
    FIELD xxIndex          AS INTEGER   LABEL "Index" FORMAT ">>>>>>9"
    INDEX ttOnTimeDeliveries IS PRIMARY rowType
    .
