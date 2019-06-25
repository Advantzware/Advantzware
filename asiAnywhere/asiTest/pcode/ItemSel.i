

DEFINE TEMP-TABLE ttItemSel NO-UNDO
BEFORE-TABLE beforeItemSel
        FIELD Item1         AS char
        FIELD ord-level     AS Decimal 
        FIELD q-ono         AS Decimal   
        FIELD q-onh         AS Decimal   
        FIELD q-alloc       AS Decimal   
        FIELD q-avail       AS Decimal   
        FIELD q-back        AS Decimal 
    FIELD Name AS char
        FIELD Order AS Integer
        FIELD Estimate AS char

   . 
DEFINE DATASET dsItemSel FOR ttItemSel .

DEFINE QUERY q-ItemSelQuery FOR ttItemSel.

DEFINE DATA-SOURCE src-ItemSel  FOR QUERY q-ItemSelQuery.

BUFFER ttItemSel :ATTACH-DATA-SOURCE(DATA-SOURCE src-ItemSel  :HANDLE).








