/* cerep/jc-pallet.i*/
DEF {1} SHARED TEMP-TABLE tt-pallet FIELD tt-job-no LIKE job-hdr.job-no
                                   field tt-form-no like ef.form-no
                                   field tt-stap-pa as CHAR 
                                   field tt-band-tool as char format "x(20)"
                                   field tt-strech as CHAR
                                   FIELD tt-stak-pa AS CHAR .
