

DEFINE TEMP-TABLE ttViewItems NO-UNDO
BEFORE-TABLE beforeViewItems
        
        FIELD Item1         AS char
        FIELD ord-level     AS Decimal 
        FIELD q-ono         AS Decimal   
        FIELD q-onh         AS Decimal   
        FIELD q-alloc       AS Decimal   
        FIELD q-avail       AS Decimal   
        FIELD q-back        AS Decimal   

   . 
DEFINE DATASET dsViewItems FOR ttViewItems .

DEFINE QUERY q-ViewItemsQuery FOR ttViewItems.

DEFINE DATA-SOURCE src-ViewItems  FOR QUERY q-ViewItemsQuery.

BUFFER ttViewItems :ATTACH-DATA-SOURCE(DATA-SOURCE src-ViewItems  :HANDLE).






