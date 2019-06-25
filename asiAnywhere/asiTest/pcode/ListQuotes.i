



DEFINE TEMP-TABLE ttListQuotes NO-UNDO
    FIELD  vQuote           AS INTEGER FORMAT ">>>>>9"
    FIELD  vDate            AS DATE    FORMAT "99/99/9999"
    FIELD  vCust            AS CHAR    FORMAT "x(8)"
    FIELD  vEstimate        AS CHAR    FORMAT "x(30)"
    FIELD  vRfq             AS CHAR    FORMAT "x(8)"
   
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
     FIELD vdscr            AS CHARACTER
     FIELD vdscr1            AS CHARACTER
      . 
DEFINE DATASET dsListQuotes FOR ttListQuotes .
DEFINE QUERY q-ListQuotesQuery FOR ttListQuotes.
DEFINE DATA-SOURCE src-ListQuotes  FOR QUERY q-ListQuotesQuery.
BUFFER ttListQuotes :ATTACH-DATA-SOURCE(DATA-SOURCE src-ListQuotes  :HANDLE).


/*AS DECIMAL  FORMAT ">>>>>9.99<<<<   "*/





