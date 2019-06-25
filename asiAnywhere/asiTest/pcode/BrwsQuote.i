


DEFINE TEMP-TABLE ttBrowseQte NO-UNDO

        FIELD vQuote          AS INTEGER FORMAT ">>>>>9"
        FIELD vDate           AS DATE    FORMAT "99/99/9999"
        FIELD vCust           AS CHAR    FORMAT "x(8)"
        FIELD vContact        AS CHAR    FORMAT "x(30)"
        FIELD vEstimate       AS CHAR    FORMAT "x(8)"
        FIELD vRfq            AS CHAR    FORMAT "x(10)"
        FIELD VPart           AS CHAR    FORMAT "x(20)"
        FIELD vLine           AS INT 
        FIELD vQty            AS INT 
        
        . 
DEFINE DATASET dsBrowseQte FOR ttBrowseQte .
DEFINE QUERY q-BrowseQteQuery FOR ttBrowseQte.
DEFINE DATA-SOURCE src-BrowseQte  FOR QUERY q-BrowseQteQuery.
BUFFER ttBrowseQte :ATTACH-DATA-SOURCE(DATA-SOURCE src-BrowseQte  :HANDLE).







