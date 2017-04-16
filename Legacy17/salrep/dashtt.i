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

DEFINE {1} SHARED TEMP-TABLE tt-raw-prod NO-UNDO
    FIELD m-code AS CHAR
    FIELD DATE AS DATE
    FIELD date-qty AS DEC
    FIELD mtd-qty AS DEC
    FIELD ytd-qty AS DEC
    FIELD date-qty-msf AS DEC
    FIELD mtd-qty-msf AS DEC
    FIELD ytd-qty-msf AS DEC
    FIELD date-run-hrs AS DEC
    FIELD mtd-run-hrs AS DEC
    FIELD ytd-run-hrs AS DEC
    FIELD date-mr-hrs AS DEC
    FIELD mtd-mr-hrs AS DEC
    FIELD ytd-mr-hrs AS DEC
    FIELD date-dt-charge AS DEC
    FIELD mtd-dt-charge AS DEC
    FIELD ytd-dt-charge AS DEC
    FIELD date-dt-nc AS DEC
    FIELD mtd-dt-nc AS DEC
    FIELD ytd-dt-nc AS DEC
    FIELD date-std-hrs AS DEC
    FIELD mtd-std-hrs AS DEC
    FIELD ytd-std-hrs AS DEC
    FIELD date-eff AS DEC
    FIELD mtd-eff AS DEC
    FIELD ytd-eff AS DEC
    FIELD date-util AS DEC
    FIELD mtd-util AS DEC
    FIELD ytd-util AS DEC
    FIELD date-dt-perc AS DEC
    FIELD mtd-dt-perc AS DEC
    FIELD ytd-dt-perc AS DEC
    INDEX idx m-code ASC.

DEFINE {1} SHARED TEMP-TABLE tt-sales-prod-cat NO-UNDO
    FIELD prod-cat AS CHAR
    FIELD DATE AS DATE
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
    INDEX idx prod-cat ASC.

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

DEFINE {1} SHARED TEMP-TABLE tt-ar-ap NO-UNDO
    FIELD date-ar-rec-amt AS DEC
    FIELD mtd-ar-rec-amt AS DEC
    FIELD date-ap-paid-amt AS DEC
    FIELD mtd-ap-paid-amt AS DEC.

DEFINE {1} SHARED TEMP-TABLE tt-raw-sales NO-UNDO
    FIELD DATE AS DATE
    FIELD date-amt AS DEC
    FIELD date-cost AS DEC
    FIELD date-qty AS DEC
    FIELD date-msf AS DEC
    FIELD date-tons AS DEC
    FIELD date-net-profit AS DEC
    INDEX idx DATE ASC.

DEFINE {1} SHARED TEMP-TABLE tt-sales-forecast NO-UNDO
    FIELD company AS CHAR
    FIELD invoiced-amt AS DEC
    FIELD backlog-amt AS DEC
    FIELD released-amt AS DEC
    FIELD invoiced-qty AS DEC
    FIELD backlog-qty AS DEC
    FIELD released-qty AS DEC
    FIELD invoiced-msf AS DEC
    FIELD backlog-msf AS DEC
    FIELD released-msf AS DEC
    FIELD invoiced-profit AS DEC
    FIELD backlog-profit AS DEC
    FIELD released-profit AS DEC
    FIELD invoiced-cost AS DEC
    FIELD backlog-cost AS DEC
    FIELD released-cost AS DEC.

DEFINE {1} SHARED TEMP-TABLE tt-ar-dso NO-UNDO
    FIELD actnum AS CHAR.
