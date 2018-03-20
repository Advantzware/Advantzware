/* cec/tt-eb-set-part.i */

DEF {1} SHARED TEMP-TABLE tt-eb-set-part NO-UNDO
    FIELD company AS CHAR
    FIELD loc AS CHAR
    FIELD est-type AS INT
    FIELD est-no AS CHAR
    FIELD form-no AS INT
    FIELD blank-no AS INT
    FIELD est-int AS INT
    FIELD stock-no AS CHAR
    FIELD part-no AS CHAR
    FIELD part-dscr1 AS CHAR
    FIELD part-dscr2 AS CHAR
    FIELD part-dscr3 AS CHAR
    FIELD part-dscr4 AS CHAR
    FIELD len AS DEC DECIMALS 6
    FIELD wid AS DEC DECIMALS 6
    FIELD dep AS DEC DECIMALS 6
    FIELD procat AS CHAR
    FIELD set-is-assembled AS LOG
    FIELD pur-man AS LOG
    FIELD board AS CHAR
    FIELD style-1 AS CHAR
    FIELD style-2 AS CHAR
    FIELD end-cell-length-1 AS DEC DECIMALS 6
    FIELD end-cell-length-2 AS DEC DECIMALS 6
    FIELD end-cell-width-1 AS DEC DECIMALS 6
    FIELD end-cell-width-2 AS DEC DECIMALS 6
    FIELD in-cell-length AS DEC DECIMALS 6
    FIELD in-cell-width AS DEC DECIMALS 6
    FIELD qty-set-1 AS DECIMAL
    FIELD qty-set-2 AS DECIMAL
    FIELD num-forms AS INT
    FIELD est-qty AS INT
    FIELD header-part-dscr1 AS CHAR
    FIELD header-part-dscr2 AS CHAR
    FIELD header-stock-no AS CHAR
    FIELD grain AS CHAR.
