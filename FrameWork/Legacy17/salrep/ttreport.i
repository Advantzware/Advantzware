
DEF {1} SHARED TEMP-TABLE tt-report LIKE report
               FIELD inv-no  LIKE ar-inv.inv-no
               FIELD sman    AS   CHAR
               FIELD freight AS   DEC DECIMALS 10
               FIELD cred    LIKE cust.cr-rating
               INDEX frate-inv-sman key-10 inv-no sman
               INDEX cred cred key-01 key-02 key-03 key-04 key-05
                               key-06 key-07 key-08 key-09 key-10.

DEF BUFFER tt-report2 FOR tt-report.
