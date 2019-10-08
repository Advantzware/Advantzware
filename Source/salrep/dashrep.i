DEFINE {1} SHARED TEMP-TABLE tt-raw-salesmen NO-UNDO
    FIELD sman AS CHAR
    FIELD sname AS CHAR
    FIELD DATE AS DATE
    FIELD amt AS DEC EXTENT 12
    FIELD msf AS DEC EXTENT 12
    FIELD date-sf AS DEC
    FIELD date-amt AS DEC
    FIELD date-msf AS DEC
    FIELD date-cost AS DEC
    FIELD date-profit AS DEC
    FIELD mtd-sf AS DEC
    FIELD mtd-amt AS DEC
    FIELD mtd-msf AS DEC
    FIELD mtd-cost AS DEC
    FIELD mtd-profit AS DEC
    FIELD ytd-sf AS DEC
    FIELD ytd-amt AS DEC
    FIELD ytd-msf AS DEC
    FIELD ytd-cost AS DEC
    FIELD ytd-profit AS DEC
    INDEX idx sman ASC.
