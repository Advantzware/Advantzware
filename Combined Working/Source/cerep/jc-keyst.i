/* cerep/jc-keyst.i*/
DEF {1} SHARED TEMP-TABLE tt-keyst FIELD tt-job-no LIKE job-hdr.job-no
                                   FIELD tt-job-no2 LIKE job-hdr.job-no2
                                   FIELD tt-frm LIKE job-hdr.frm
                                   FIELD tt-mill AS cha 
                                   FIELD tt-negs AS cha
                                   FIELD tt-plate AS cha EXTENT 2
                                   FIELD tt-die AS cha  EXTENT 2
                                   FIELD tt-copy AS cha
                                   FIELD tt-tray-desc AS cha
                                   FIELD tt-tray-qty AS INT
                                   FIELD tt-mb AS cha
                                   FIELD tt-newsize AS cha
                                   FIELD tt-aql AS cha
                                   FIELD tt-glue-cnt AS cha
                                   FIELD tt-plate2 AS cha
                                   FIELD tt-die2 AS cha
                                   FIELD tt-size AS cha
                                   FIELD tt-sheets-req AS INT.
