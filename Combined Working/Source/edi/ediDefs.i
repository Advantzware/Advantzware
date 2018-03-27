/* edi/ediDefs.i */



DEFINE TEMP-TABLE tt-email NO-UNDO
    FIELD tt-recid   AS RECID
    FIELD job-no   LIKE job-hdr.job-no
    FIELD job-no2  LIKE job-hdr.job-no2
    FIELD i-no     LIKE itemfg.i-no
    FIELD qty        AS INTEGER
    FIELD cust-no    AS CHARACTER
        INDEX tt-cust IS PRIMARY cust-no DESCENDING
    .

