/* aoaJC.p */

/* ** temp-table definitions **************************************** */
DEFINE TEMP-TABLE tt-srt NO-UNDO LIKE mch-srt RCODE-INFORMATION 
    FIELD act-m-code    LIKE mach.m-code
    FIELD tot-run-hours   AS DECIMAL LABEL "Total Run Hrs"
    FIELD tot-mr-hours    AS DECIMAL LABEL "Total MR Hrs"
    FIELD qty-Ton         AS DECIMAL LABEL "Ton Qty"       FORMAT ">>,>>9.99"
    FIELD qty-msf         AS DECIMAL LABEL "MSF Qty"       FORMAT ">>,>>9.99"
    FIELD start-time      AS INTEGER LABEL "Start Time"
    FIELD start-date      AS DATE    LABEL "Start Date"    FORMAT "99/99/9999"
    FIELD i-no          LIKE mch-srt.job-no
        INDEX dept-idx  dept m-code job-no job-no2 frm blank-no
        INDEX job-idx   job-no job-no2
        .

/* ** function declarations ***************************************** */
FUNCTION fProdAnalysis RETURNS HANDLE ():
  EMPTY TEMP-TABLE tt-srt.

  RETURN TEMP-TABLE tt-srt:HANDLE.
END FUNCTION.

/* ** procedure declarations **************************************** */
