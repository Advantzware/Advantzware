


DEFINE TEMP-TABLE ttViewInv NO-UNDO
BEFORE-TABLE beforeViewInv
        FIELD cust-no        AS CHAR FORMAT "x(8)"
        FIELD cust-name      AS CHAR FORMAT "x(30)"
        FIELD ship-id        AS CHAR FORMAT "x(8)"
        FIELD inv-no         AS INT  FORMAT ">>>>>9"
        FIELD tax-code       AS CHAR FORMAT "x(3)"
        FIELD terms          AS CHAR FORMAT "x(5)"
        FIELD terms-d        AS CHAR FORMAT "x(30)"
        FIELD inv-date       AS DATE FORMAT "99/99/9999"
        FIELD due-date       AS DATE FORMAT "99/99/9999"
        FIELD ord-no         AS INTEGER FORMAT ">>>>>9"          
        FIELD disc-%         AS DECIMAL FORMAT ">>9.99%" 
        FIELD disc-days      AS INTEGER FORMAT "z9"
        FIELD carrier        AS CHAR FORMAT "x(5)"
        FIELD cost           AS DECIMAL FORMAT "->>>,>>>,>>9.99" 
        FIELD gross          AS DECIMAL FORMAT "->,>>>,>>9.99"
        FIELD freight        AS DECIMAL FORMAT "->>,>>9.99"
        FIELD tax-amt        AS DECIMAL FORMAT "->>,>>9.99"
        FIELD disc-taken     AS DECIMAL FORMAT "->>>,>>9.99"
        FIELD paid           AS DECIMAL FORMAT "->,>>>,>>9.99"
        FIELD due            AS DECIMAL FORMAT "->>,>>>,>>9.99"
        FIELD po-no          AS CHAR FORMAT "x(5)"
        FIELD LINE           AS INT FORMAT "99"
        FIELD actnum         AS CHARACTER FORMAT "x(25)"
        FIELD act-dscr       AS CHARACTER FORMAT "x(45)"
        FIELD i-name         AS CHARACTER FORMAT "x(30)"
        FIELD i-dscr         AS CHARACTER FORMAT "x(30)"
        FIELD inv-qty        AS DECIMAL  FORMAT "->>,>>>,>>9.9<"
        FIELD cons-uom       AS CHAR  FORMAT "x(4)"
        FIELD sf-sht         AS DECIMAL FORMAT "->>,>>9.99<<":U
        FIELD unit-pr        AS DECIMAL FORMAT "->>,>>>,>>9.99<<<<"
        FIELD pr-qty-uom     AS CHARACTER FORMAT "x(4)"
        FIELD amt            AS DECIMAL FORMAT "->>,>>>,>>9.99"
        FIELD amt-msf        AS DECIMAL FORMAT "->>>>,>>9.99"
        FIELD vRowid AS RECID
        . 
DEFINE DATASET dsViewInv FOR ttViewInv .
DEFINE QUERY q-ViewInvQuery FOR ttViewInv.
DEFINE DATA-SOURCE src-ViewInv  FOR QUERY q-ViewInvQuery.
BUFFER ttViewInv :ATTACH-DATA-SOURCE(DATA-SOURCE src-ViewInv  :HANDLE).

/*------------------------------------------------------------------------------*/
FUNCTION get-actdscr RETURNS CHARACTER():
IF AVAIL ar-invl THEN DO:
     FIND FIRST account WHERE account.company = ar-invl.company
                          AND account.actnum = ar-invl.actnum NO-LOCK NO-ERROR.
     IF AVAIL account THEN RETURN account.dscr.
     ELSE RETURN "".
  END.
  ELSE RETURN "".   /* Function return value. */

END FUNCTION.

/*------------------------------------------------------------------------------*/
FUNCTION get-i-dscr RETURNS CHARACTER():
  RETURN IF AVAIL ar-invl AND ar-invl.i-dscr EQ "" THEN
           ar-invl.part-dscr1 ELSE ar-invl.i-dscr.   /* Function return value. */

END FUNCTION.

