
DEFINE TEMP-TABLE ttViewItem NO-UNDO
BEFORE-TABLE beforeViewItem
        FIELD OrdNo LIKE oe-ordl.ord-no
        FIELD vLine LIKE oe-ordl.LINE
        FIELD CustPart      AS CHAR
        FIELD Item1         AS char
        FIELD Name1         AS char
        FIELD Dscr          AS char
        FIELD quantity      AS DECIMAL
        FIELD price         AS DECIMAL
        FIELD uom           AS char
        FIELD counter       AS INTEGER
        FIELD custpo        AS CHAR
        FIELD taxable       AS LOGICAl Initial true 
        FIELD discount      AS DECIMAL 
        FIELD requested     AS CHAR
        FIELD requestdate   AS DATE
        FIELD extprice      AS DECIMAL
        FIELD promised      AS CHAR
        FIELD promisdate    AS DATE
        FIELD shipqty       AS DECIMAL
        FIELD alloc         AS LOGICAL Initial true 
        FIELD ord-level     AS Decimal 
        FIELD q-ono         AS Decimal   
        FIELD q-onh         AS Decimal   
        FIELD q-alloc       AS Decimal   
        FIELD q-avail       AS Decimal   
        FIELD q-back        AS Decimal   

   . 
DEFINE DATASET dsViewItem FOR ttViewItem .

DEFINE QUERY q-ViewItemQuery FOR ttViewItem.

DEFINE DATA-SOURCE src-ViewItem  FOR QUERY q-ViewItemQuery.

BUFFER ttViewItem :ATTACH-DATA-SOURCE(DATA-SOURCE src-ViewItem  :HANDLE).







