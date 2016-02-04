/* cecrep/jc-prem.i*/
DEF {1} SHARED TEMP-TABLE tt-prem  FIELD tt-job-no LIKE job-hdr.job-no
                                   FIELD tt-job-no2 LIKE job-hdr.job-no2
                                   FIELD tt-frm LIKE job-hdr.frm
                                   FIELD tt-#-bundle AS cha 
                                   FIELD tt-#-unit AS cha
                                   FIELD tt-pattern AS cha
                                   FIELD tt-pallet AS cha
                                   FIELD tt-count AS INT.
