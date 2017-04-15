
DEF {1} {2} TEMP-TABLE tt-report LIKE report
                                 FIELD tt-wid  AS CHAR
                                 FIELD tt-len  AS CHAR
                                 FIELD tt-dep  AS CHAR
                                 FIELD tt-reqs AS DEC
                                 FIELD tt-onhs AS DEC
                                 FIELD tt-onhl AS DEC
                                 FIELD tt-avls AS DEC
                                 FIELD tt-avll AS DEC
                                 FIELD tt-out  AS INT
                                 FIELD tt-msf  AS DEC
                                 FIELD tt-sel  AS LOG.
DEF {1} {2} TEMP-TABLE tt-eb     LIKE eb.
DEF {1} {2} TEMP-TABLE tt-ef     LIKE ef.
