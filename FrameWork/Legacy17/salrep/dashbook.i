DEFINE {1} SHARED TEMP-TABLE tt-raw-op NO-UNDO
    FIELD DATE AS DATE
    FIELD oe-dollars AS DEC
    FIELD oe-qty AS DEC
    FIELD oe-qty-msf AS DEC
    FIELD oe-qty-tons AS DEC
    FIELD oe-gp AS DEC
    FIELD rel-dollars AS DEC
    FIELD rel-qty AS DEC
    FIELD rel-qty-msf AS DEC
    FIELD rel-qty-tons AS DEC
    FIELD rel-gp AS DEC
    INDEX tt-raw-op DATE ASC.
