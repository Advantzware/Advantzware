


DEFINE TEMP-TABLE ttViewQuote NO-UNDO
    FIELD  vQuote           AS INTEGER FORMAT ">>>>>9"
    FIELD  vDate            AS DATE    FORMAT "99/99/9999"
    FIELD  vCust            AS CHAR    FORMAT "x(8)"
    FIELD  vEstimate        AS CHAR    FORMAT "x(30)"
    FIELD  vRfq             AS CHAR    FORMAT "x(8)"
    FIELD  vDelDate         AS DATE    FORMAT "99/99/9999"
    FIELD  vBill            AS CHAR    FORMAT "x(30)"
    FIELD  vBill2           AS CHAR    FORMAT "x(30)"
    FIELD  vBill3           AS CHAR    FORMAT "x(30)" 
    FIELD  vBill4           AS CHAR    FORMAT "x(30)" 
    FIELD  vShipid          AS CHAR    FORMAT "x(8)" 
    FIELD  vShip            AS CHAR    FORMAT "x(30)" 
    FIELD  vShip2           AS CHAR    FORMAT "x(30)" 
    FIELD  vShip3           AS CHAR    FORMAT "x(30)" 
    FIELD  vShip4           AS CHAR    FORMAT "x(30)" 
    FIELD vContact          AS CHARACTER
    FIELD  vSoldId          AS CHAR    FORMAT "x(8)"   
    FIELD vSold             AS CHAR    FORMAT "x(30)"  
    FIELD vSold2            AS CHAR    FORMAT "x(30)"  
    FIELD vSold3            AS CHAR    FORMAT "x(30)"  
    FIELD vSold4            AS CHAR    FORMAT "x(30)"  
    FIELD vSman             AS CHAR    FORMAT "x(3)"   
    FIELD vTerms            AS CHAR    FORMAT "x(5)"  
    FIELD vCarrier          AS CHAR    FORMAT "x(5)"  
    FIELD vDelZone          AS CHAR    FORMAT "x(5)"  
    FIELD vStat             AS CHAR    FORMAT "x(1)"  
    FIELD vSname            AS CHAR
    FIELD vTermDscr         AS CHAR    FORMAT "x(30)" 
    FIELD vCarrdscr         AS CHAR    FORMAT "x(30)" 
    FIELD  vPart            AS CHAR    FORMAT "x(20)" 
    FIELD  vName            AS CHAR    FORMAT "x(30)" 
    FIELD  Style            AS CHAR    FORMAT "x(6)" 
    FIELD  vStyDscr         AS CHAR    FORMAT "x(6)" 
    FIELD  vUnit            AS DECIMAL
    FIELD  vQty             AS DECIMAL
    FIELD  vUom             AS CHAR
    FIELD  vLen             AS CHAR 
    FIELD  vDim             AS DECIMAL  FORMAT ">>>>>9.99<<<<   "
    FIELD  vBrd             AS DECIMAL  FORMAT ">>>>>9.99<<<<   "
    FIELD  vitem            AS CHARACTER
    FIELD  vZondesc         AS CHARACTER
      . 
DEFINE DATASET dsViewQuote FOR ttViewQuote .
DEFINE QUERY q-ViewQuoteQuery FOR ttViewQuote.
DEFINE DATA-SOURCE src-ViewQuote  FOR QUERY q-ViewQuoteQuery.
BUFFER ttViewQuote :ATTACH-DATA-SOURCE(DATA-SOURCE src-ViewQuote  :HANDLE).


/*AS DECIMAL  FORMAT ">>>>>9.99<<<<   "*/




