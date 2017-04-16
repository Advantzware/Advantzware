DEFINE {1} SHARED TEMP-TABLE tt-raw-sales NO-UNDO
    FIELD DATE AS DATE
    FIELD date-amt AS DEC
    FIELD date-cost AS DEC
    FIELD date-qty AS DEC
    FIELD date-msf AS DEC
    FIELD date-tons AS DEC
    FIELD date-net-profit AS DEC
    INDEX idx DATE ASC.
