DEFINE {1} SHARED TEMP-TABLE tt-bolx
    FIELD print-logo AS LOG
    FIELD print-addr AS LOG
    FIELD logo-file AS CHAR FORMAT "X(256)"
    FIELD NAME AS CHAR FORMAT "X(30)"
    FIELD addr-1 AS CHAR FORMAT "X(30)"
    FIELD addr-2 AS CHAR FORMAT "X(30)"
    FIELD city AS CHAR FORMAT "X(15)"
    FIELD state AS CHAR FORMAT "X(2)"
    FIELD zip AS CHAR FORMAT "X(10)"
    FIELD phone-area AS CHAR FORMAT "(xxx)"
    FIELD phone-num AS CHAR FORMAT "xxx-xxxx"
    FIELD fax-area AS CHAR FORMAT "(xxx)"
    FIELD fax-num AS CHAR FORMAT "xxx-xxxx"
    FIELD note-1 AS CHAR
    FIELD note-2 AS CHAR.
