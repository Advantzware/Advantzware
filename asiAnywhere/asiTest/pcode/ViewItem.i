
DEFINE TEMP-TABLE ttViewItem NO-UNDO 
BEFORE-TABLE beforeViewItem
        FIELD OrdNo LIKE oe-ordl.ord-no
        FIELD est-no LIKE oe-ordl.est-no
        FIELD job-no LIKE oe-ordl.job-no
        FIELD vLine LIKE oe-ordl.LINE
        FIELD CustPart      AS CHAR FORMAT "x(15)"
        FIELD Item1         AS CHAR FORMAT "x(15)"
        FIELD Name1         AS CHAR FORMAT "x(30)"
        FIELD Dscr          AS CHAR FORMAT "x(30)"
        FIELD quantity      AS DECIMAL FORMAT "->>,>>>,>>99.9<<"
        FIELD price         AS DECIMAL FORMAT "->>,>>>,>>9.99<<"
        FIELD uom           AS CHAR FORMAT "X(4)"
        FIELD counter       AS INTEGER 
        FIELD custpo        AS CHAR FORMAT "x(15)"
        FIELD taxable       AS LOGICAl Initial true 
        FIELD discount      AS DECIMAL FORMAT ">>>,>>9.99" 
        FIELD requested     AS CHAR FORMAT "x(2)"
        FIELD requestdate   AS DATE FORMAT "99/99/9999"
        FIELD extprice      AS DECIMAL FORMAT "->>,>>>,>>9.99"
        FIELD promised      AS CHAR FORMAT ">>>>9"
        FIELD promisdate    AS DATE FORMAT "99/99/9999"
        FIELD shipqty       AS DECIMAL FORMAT "->>,>>>,>>9.99"
        FIELD alloc         AS LOGICAL Initial true 
        FIELD ord-level     AS Decimal FORMAT ">>>,>>>,>>9.999"
        FIELD q-ono         AS Decimal FORMAT  "->>,>>>,>>9.999 "  
        FIELD q-onh         AS Decimal FORMAT "->>,>>>,>>9.999"  
        FIELD q-alloc       AS Decimal FORMAT "->>,>>>,>>9.999"  
        FIELD q-avail       AS Decimal FORMAT "->>,>>>,>>9.999"     
        FIELD q-back        AS Decimal FORMAT "->>,>>>,>>9.999" 
        FIELD vReckey       LIKE oe-ordl.rec_key        
        FIELD over          AS DECIMAL 
        FIELD under         AS DECIMAL. 


DEFINE DATASET dsViewItem FOR ttViewItem .

DEFINE QUERY q-ViewItemQuery FOR ttViewItem.

DEFINE DATA-SOURCE src-ViewItem  FOR QUERY q-ViewItemQuery.

BUFFER ttViewItem :ATTACH-DATA-SOURCE(DATA-SOURCE src-ViewItem  :HANDLE).







