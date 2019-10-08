DEFINE {1} SHARED TEMP-TABLE tt-1099-m NO-UNDO
    FIELD vend-no AS CHAR
    FIELD vend-name AS CHAR FORMAT "X(30)"
    FIELD vend-tax-id AS CHAR FORMAT "X(15)"
    FIELD vend-add1 AS CHAR FORMAT "X(30)"
    FIELD vend-add2 AS CHAR FORMAT "X(30)"
    FIELD vend-city-line AS CHAR FORMAT "X(30)"
    FIELD vend-total AS DEC
    FIELD vend-box AS CHARACTER 
    INDEX idx vend-name ASC.

DEF {1} SHARED VAR s-copies AS INT NO-UNDO.
