


DEFINE TEMP-TABLE ttBrwsPo NO-UNDO
BEFORE-TABLE beforeBrwsPo
        FIELD po-no        AS INTEGER FORMAT ">>>>>9"
        FIELD vend-no      AS CHARACTER FORMAT "x(8)"
        FIELD due-date     AS DATE FORMAT "99/99/9999"
        FIELD job-no       AS CHAR FORMAT "x(6)"
        FIELD job-no2      AS INTEGER FORMAT "99"
        FIELD i-no         AS CHAR FORMAT "X(15)"
        FIELD s-num        AS INTEGER FORMAT ">>>"
        FIELD i-name       AS CHAR FORMAT "x(30)"
        FIELD s-wid        AS DEC FORMAT  ">>,>>9.99<<<"
        FIELD s-len        AS DEC FORMAT  ">>,>>9.99<<<"
        FIELD vend-i-no    AS CHAR FORMAT "X(15)"
        FIELD ord-qty      AS DECIMAL FORMAT "->>>,>>>,>>9.9<<<<<"
        FIELD rece-qty     AS DECIMAL FORMAT "->>>,>>>,>>9.9<<"
        FIELD Orduom       AS CHAR FORMAT "x(4)"
        FIELD recqty       AS DECIMAL FORMAT "->>>,>>>,>>9.9<<"
        FIELD cost         AS DECIMAL FORMAT "->,>>>,>>9.99<<<<"
        FIELD uom          AS CHAR FORMAT "x(4)"
        FIELD buyer        AS CHAR FORMAT "x(10)"
        FIELD stat         AS CHAR FORMAT "x"  
        . 
DEFINE DATASET dsBrwsPo FOR ttBrwsPo .
DEFINE QUERY q-BrwsPoQuery FOR ttBrwsPo.
DEFINE DATA-SOURCE src-BrwsPo  FOR QUERY q-BrwsPoQuery.
BUFFER ttBrwsPo :ATTACH-DATA-SOURCE(DATA-SOURCE src-BrwsPo  :HANDLE).







