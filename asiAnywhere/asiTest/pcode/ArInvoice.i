

DEFINE TEMP-TABLE ttArInvoice NO-UNDO
BEFORE-TABLE beforeArInvoice
        FIELD Arinv-no        AS INTEGER
        FIELD Arbol-no        AS INTEGER
        FIELD Arcust-no       AS char
        FIELD Arinv-date      AS DATE
        FIELD Ari-no          AS CHAR
        FIELD Arpart-no       AS char
        FIELD Arord-no        AS INTEGER
        FIELD Arpo-no         AS CHAR
        FIELD Arest-no        AS CHAR
        FIELD Arname         AS CHAR 
        FIELD Arcosts         AS DECIMAL
        FIELD rec_key_cust    AS CHAR 
        FIELD Aract-num       AS CHAR
        . 
DEFINE DATASET dsArInvoice FOR ttArInvoice .
DEFINE QUERY q-ArInvoiceQuery FOR ttArInvoice.
DEFINE DATA-SOURCE src-ArInvoice  FOR QUERY q-ArInvoiceQuery.
BUFFER ttArInvoice :ATTACH-DATA-SOURCE(DATA-SOURCE src-ArInvoice  :HANDLE).






