
DEF {1} SHARED TEMP-TABLE tt-ei NO-UNDO
    FIELD company AS CHAR
    FIELD std-uom AS CHAR
    FIELD i-no AS CHAR
    FIELD run-qty AS DECIMAL DECIMALS 3 EXTENT 20
    FIELD run-cost AS DECIMAL DECIMALS 4 EXTENT 20
    INDEX i-no company i-no.

DEF {1} SHARED TEMP-TABLE tt-eiv NO-UNDO
    FIELD company AS CHAR
    FIELD i-no AS CHAR
    FIELD vend-no AS CHAR
    FIELD item-type AS LOG
    FIELD row-id AS ROWID
    FIELD rec_key AS CHAR
    FIELD SELECTED AS LOG EXTENT 10
    FIELD run-qty AS DECIMAL DECIMALS 3 EXTENT 20
    FIELD run-cost AS DECIMAL DECIMALS 4 EXTENT 20
    FIELD setups AS DECIMAL DECIMALS 2 EXTENT 20
    FIELD roll-w AS DECIMAL DECIMALS 4 EXTENT 30
    INDEX vend-no company i-no vend-no
    INDEX i-no company item-type i-no vend-no
    INDEX row-id row-id.


DEF BUFFER b-setup FOR reftable.
