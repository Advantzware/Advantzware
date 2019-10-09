
DEF {1} SHARED TEMP-TABLE tt-bin NO-UNDO LIKE rm-bin
    FIELD cons-uom      LIKE item.cons-uom
    FIELD qty-sht       LIKE rm-bin.qty
    FIELD qty-sel       LIKE rm-bin.qty
    FIELD tt-date       AS   DATE
    FIELD selekt        AS   CHAR FORMAT "x"
    FIELD seq           AS   INT FORMAT ">>>,>>>"
    FIELD selekt-log    AS   LOG FORMAT "yes/no"
    FIELD rec-id        AS   RECID.
