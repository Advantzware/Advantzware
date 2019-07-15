

DEFINE TEMP-TABLE ttBrwsInv NO-UNDO
BEFORE-TABLE beforeBrwsInv
        FIELD inv-no        AS INTEGER
        FIELD bol-no        AS INTEGER
        FIELD cust-no       AS char
        FIELD inv-date      AS DATE
        FIELD act-num       AS CHAR
        FIELD i-no          AS CHAR
        FIELD part-no       AS char
        FIELD ord-no        AS INTEGER
        FIELD po-no         AS CHAR
        FIELD est-no        AS CHAR
        FIELD price         AS DECIMAL 
        FIELD costs         AS DECIMAL
        . 
DEFINE DATASET dsBrwsInv FOR ttBrwsInv .
DEFINE QUERY q-BrwsInvQuery FOR ttBrwsInv.
DEFINE DATA-SOURCE src-BrwsInv  FOR QUERY q-BrwsInvQuery.
BUFFER ttBrwsInv :ATTACH-DATA-SOURCE(DATA-SOURCE src-BrwsInv  :HANDLE).






