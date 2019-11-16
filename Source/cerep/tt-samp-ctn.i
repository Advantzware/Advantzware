/* cerep/tt-samp-ctn.i*/
DEF {1} SHARED TEMP-TABLE tt-sample-ctn NO-UNDO
    FIELD tt-job-no LIKE job-hdr.job-no
    FIELD tt-job-no2 LIKE job-hdr.job-no2
    FIELD tt-frm LIKE job-hdr.frm
    FIELD tt-samp-on-cnt AS LOG.
