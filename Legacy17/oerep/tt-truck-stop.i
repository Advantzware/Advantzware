DEF {1} SHARED TEMP-TABLE tt-report NO-UNDO
    FIELD key-01 AS CHAR
    FIELD key-04 AS INT
    FIELD key-05 AS CHAR
    FIELD key-06 AS CHAR
    FIELD rec_key AS CHAR
    FIELD company AS CHAR
    FIELD pallets AS INT
    FIELD no-units AS INT
    FIELD release-type AS CHAR
    FIELD order-no AS INT FORMAT ">>>>>9"
    FIELD stop-no AS INT FORMAT ">>>>>>9"
    FIELD load-no   AS CHAR FORMAT "X(10)"
    FIELD ship-date AS DATE
    FIELD old-stop-no AS INT FORMAT ">>>>>>9"
    FIELD old-load-no   AS CHAR FORMAT "X(10)"
    FIELD old-ship-date AS DATE
    FIELD truck-code AS CHAR FORMAT "X(20)"
    FIELD old-truck-code AS CHAR FORMAT "X(20)"
    FIELD truck-dscr AS CHAR FORMAT "X(25)"
    FIELD old-truck-dscr AS CHAR FORMAT "X(25)"
    FIELD cust-no AS CHAR FORMAT "X(8)"
    FIELD cust-name AS CHAR FORMAT "X(30)"
    FIELD ship-no AS CHAR FORMAT "X(8)"
    FIELD ship-to-text AS CHAR FORMAT "X(60)"
    FIELD city         AS CHAR FORMAT "X(15)"
    FIELD state        AS CHAR FORMAT "X(2)"
    FIELD zip          AS CHAR FORMAT "X(10)"
    FIELD deliv-zone AS CHAR FORMAT "X(5)"
    FIELD rel-no AS INT
    FIELD bol-no AS INT
    FIELD line-no AS INT
    FIELD item-no AS CHAR FORMAT "X(15)"
    FIELD unique-no AS INT
    FIELD rel-no-internal AS INT
    FIELD b-ord-no AS INT
    FIELD po-no AS CHAR
    FIELD link-no AS INT
    FIELD oe-rel-r-no AS INT
    FIELD msf AS DEC
    FIELD weight AS DEC
    FIELD tot-msf AS DEC
    FIELD tot-weight AS DEC
    FIELD tot-units AS INT
    FIELD loc AS CHAR
    FIELD carrier AS CHAR
    FIELD old-carrier AS CHAR
    FIELD truck-print-key AS CHAR
    FIELD is-orig AS LOG
    INDEX tt-trk-stop-rel IS PRIMARY release-type ASC rel-no ASC
    INDEX tt-trk-stop-bol release-type ASC bol-no ASC
    INDEX tt-trk-stop-2 carrier truck-code ASC load-no ASC ship-date ASC stop-no ASC
    INDEX tt-trk-stop-3 carrier truck-code ASC ship-date ASC load-no ASC stop-no ASC
    INDEX tt-zone deliv-zone ASC cust-no ASC ship-no ASC order-no ASC
    INDEX tt-bol bol-no
    INDEX key-01 key-01 key-04 key-05
    INDEX unique-no unique-no.
