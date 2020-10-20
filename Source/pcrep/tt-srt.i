/* tt-srt.i */

DEFINE TEMP-TABLE tt-srt NO-UNDO LIKE mch-srt
    FIELD act-m-code     LIKE mach.m-code
    FIELD tot-run-hours  AS DECIMAL
    FIELD tot-mr-hours   AS DECIMAL
    FIELD qty-Ton        AS DECIMAL   FORMAT ">>,>>9.99"
    FIELD qty-msf        AS DECIMAL   FORMAT ">>,>>9.99"
    FIELD sqFeet-blank   AS DECIMAL   FORMAT ">>,>>9.9999"
    FIELD sqfeet-rcv     AS INTEGER
    FIELD sheet-rcv      AS INTEGER
    FIELD qty-rcv        AS INTEGER
    FIELD sqfeet-prod    AS INTEGER
    FIELD job-date       AS DATE
    FIELD qty-finished   AS DECIMAL 
    FIELD mr-start-time  LIKE mch-act.start
    FIELD mr-end-time    LIKE mch-act.stopp
    FIELD run-start-time LIKE mch-act.start
    FIELD run-end-time   LIKE mch-act.stopp
    FIELD gotReceipts    AS LOGICAL
    FIELD i-no           LIKE mch-act.i-no
    FIELD qty-lin-ft     AS DECIMAL  
    FIELD mr-eff         AS DECIMAL   FORMAT "->,>>9.9"
    FIELD run-eff        AS DECIMAL   FORMAT "->,>>9.9"
    FIELD fgItemName     AS CHARACTER FORMAT "x(30)"
    FIELD jobNo          AS CHARACTER FORMAT "x(10)"  LABEL "Job"
    FIELD notes          AS CHARACTER FORMAT "x(256)" LABEL "Notes"
        INDEX dept-idx dept m-code job-no job-no2 frm blank-no
        INDEX job-idx job-no job-no2
        .
