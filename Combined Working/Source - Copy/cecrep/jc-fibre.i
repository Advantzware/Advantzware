/* cecrep/jc-fibre.i*/
DEF {1} SHARED TEMP-TABLE tt-fibre FIELD tt-job-no LIKE job-hdr.job-no
                                   FIELD tt-job-no2 LIKE job-hdr.job-no2
                                   FIELD tt-frm LIKE job-hdr.frm
                                   FIELD tt-blank LIKE job-hdr.blank-no
                                   FIELD tt-sqty1 AS INT
                                   FIELD tt-sqty2 AS INT
                                   FIELD tt-sqty3 AS INT
                                   FIELD tt-sqty4 AS INT
                                   FIELD tt-oqty1 AS INT
                                   FIELD tt-oqty2 AS INT
                                   FIELD tt-oqty3 AS INT
                                   FIELD tt-oqty4 AS INT
                                   FIELD tt-order1 AS INT
                                   FIELD tt-order2 AS INT
                                   FIELD tt-order3 AS INT
                                   FIELD tt-order4 AS INT
                                   .
