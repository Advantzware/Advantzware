
DEF {1} SHARED TEMP-TABLE w-bin LIKE fg-bin
    FIELD selekt        AS   CHAR FORMAT "x"
    FIELD seq           AS   INT FORMAT ">>>,>>>"
    FIELD selekt-log    AS   LOG FORMAT "yes/no"
    FIELD rec-id        AS   RECID
    FIELD units         AS   INT
    FIELD to-rel        AS   INT
    FIELD to-bol        AS   INT
    FIELD rfid          LIKE rfidtag.rfidtag.
