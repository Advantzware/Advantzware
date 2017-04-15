
DEF {1} SHARED TEMP-TABLE tt-inv NO-UNDO
    FIELD selekt AS LOG LABEL "Selected"
    FIELD row-id AS ROWID
    FIELD qty AS DEC DECIMALS 6
    FIELD sell-price AS DEC DECIMALS 6
    FIELD pr-qty-uom AS CHAR .
