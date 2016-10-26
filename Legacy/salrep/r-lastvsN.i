    dFilterStartDate = fdate[2].
    IF INDEX(cSelectedList, "YTD Sales") GT 0 OR index(cSelectedList, "MTD Sales") GT 0 THEN DO:
      IF INDEX(cSelectedList, "YTD Sales") GT 0 AND fdate[4] LT dFilterStartDate THEN
        dFilterStartDate = fdate[4].
       IF INDEX(cSelectedList, "MTD Sales") GT 0 AND fdate[6] LT dFilterStartDate THEN
       dFilterStartDate = fdate[6].
    END.
          
    /* fdate[4] is 1/1 of prior year */
    FOR EACH ar-inv
        WHERE ar-inv.company  EQ cocode
          AND ar-inv.inv-date GE dFilterStartDate
          AND ar-inv.inv-date LE tdate[1]
          AND ar-inv.posted   EQ YES
          AND ar-inv.cust-no  GE fcust
          AND ar-inv.cust-no  LE tcust
          AND (if lselected then can-find(first ttCustList where ttCustList.cust-no eq ar-inv.cust-no
          AND ttCustList.log-fld no-lock) else true)
          AND (ar-inv.type    NE "FC" OR v-inc-fc)
        USE-INDEX inv-date NO-LOCK:

      CREATE tt-report.
      ASSIGN
       tt-report.key-09 = ar-inv.cust-no
       tt-report.key-10 = "ar-inv"
       tt-report.rec-id = RECID(ar-inv).    
 
    END.

    FOR EACH cust
        WHERE cust.company EQ cocode
          AND cust.cust-no GE fcust
          AND cust.cust-no LE tcust
          AND (if lselected then can-find(first ttCustList where ttCustList.cust-no eq cust.cust-no
          AND ttCustList.log-fld no-lock) else true)
        NO-LOCK,
       
        EACH ar-cash
        WHERE ar-cash.company    EQ cocode
          AND ar-cash.cust-no    EQ cust.cust-no
          AND ar-cash.check-date GE dFilterStartDate
          AND ar-cash.check-date LE tdate[1]
          AND ar-cash.posted     EQ YES
        NO-LOCK,

        EACH ar-cashl
        WHERE ar-cashl.c-no    EQ ar-cash.c-no
          AND ar-cashl.posted  EQ YES
          AND ar-cashl.memo    EQ YES
          AND CAN-FIND(FIRST account
                       WHERE account.company EQ ar-cashl.company
                         AND account.actnum  EQ ar-cashl.actnum
                         AND account.type    EQ "R")
        NO-LOCK:

      CREATE tt-report.
      ASSIGN
       tt-report.key-09 = cust.cust-no
       tt-report.key-10 = "ar-cashl"
       tt-report.rec-id = RECID(ar-cashl).
    END.
    
    FOR EACH tt-report
        WHERE tt-report.term-id EQ ""
          AND tt-report.key-01  EQ ""
          AND tt-report.key-02  EQ ""
          AND tt-report.key-03  EQ ""
          AND tt-report.key-04  EQ ""
          AND tt-report.key-05  EQ ""
          AND tt-report.key-06  EQ ""
          AND tt-report.key-07  EQ ""
          /*and tt-report.key-08  eq ""*/,
          
        FIRST cust
        WHERE cust.company EQ cocode
          AND cust.cust-no EQ tt-report.key-09
        NO-LOCK:
        
      IF tt-report.key-10 EQ "ar-inv" THEN DO:
        FIND ar-inv WHERE RECID(ar-inv) EQ tt-report.rec-id NO-LOCK.
        
        FOR EACH ar-invl
            WHERE ar-invl.x-no EQ ar-inv.x-no
              AND ar-invl.i-no GE begin_i-no
              AND ar-invl.i-no LE end_i-no
              AND (ar-invl.billable OR NOT ar-invl.misc)
            USE-INDEX x-no NO-LOCK:
                              /*LOOKUP(rd_show1,"Board,Order,Invoice")*/
          
          
          FIND FIRST itemfg
              WHERE itemfg.company EQ cocode
                AND itemfg.i-no    EQ ar-invl.i-no
              NO-LOCK NO-ERROR.

          DO i = 1 TO 3:
            v-sman-no = IF ar-invl.sman[i] EQ "" AND i EQ 1 THEN cust.sman
                        ELSE ar-invl.sman[i].

            IF v-sman-no  LT fsman                          OR
               v-sman-no  GT tsman                          OR
               (i NE 1 AND
                (v-sman-no EQ "" OR ar-invl.s-pct[i] EQ 0)) THEN NEXT.
       
            ASSIGN
             v-amt = 0             
             v-msf = 0             
             v-pct    = ar-invl.s-pct[i] / 100
             ld-cost = 0
             v-cost = 0.

            IF v-pct EQ 0 THEN
            DO v = 1 TO 3:
              IF v EQ 1 THEN j = 0.
              IF ar-invl.sman[v] NE "" THEN j = j + 1.
              IF v EQ 3 THEN v-pct = 1 / j.
            END.

            IF v-pct LE 0 OR v-pct EQ ? THEN v-pct = 1.
            
            DO v = 1 TO 6:
              IF ar-inv.inv-date GE fdate[v] AND
                 ar-inv.inv-date LE tdate[v] THEN
                ASSIGN
                 v-amt[v] = ar-invl.amt * v-pct
                 v-msf[v] = (IF ar-invl.amt-msf NE 0 THEN
                               ar-invl.amt-msf
                             ELSE
                             IF AVAILABLE itemfg THEN
                               (itemfg.t-sqft * ar-invl.ship-qty / 1000) ELSE 0) * v-pct.
            END.
            
            IF ar-inv.inv-date GE fdate[1] AND
               ar-inv.inv-date LE tdate[1] THEN DO:            
              RUN salrep/salecost.p (3,
                                 ROWID(ar-invl),
                                 ar-invl.job-no,
                                 ar-invl.job-no2,
                                 ar-invl.inv-qty,
                                 OUTPUT ld-cost).              
            END.
            ASSIGN v-cost = ld-cost * v-pct
                   v-costYtd = v-cost
                   v-costMtd = v-cost
                   v-pctYtd = v-pct
                   v-pctMtd = v-pct.
            DEFINE VARIABLE tt-report-key AS RECID.
            CREATE xtt-report.
       
            ASSIGN
             xtt-report.key-01  = "2" 
             xtt-report.key-02  = tt-report.key-09
             xtt-report.key-03  = v-sman-no
             xtt-report.key-04  = STRING(v-amt[1],"-9999999999999.99")
             xtt-report.key-05  = STRING(v-amt[2],"-9999999999999.99")
             xtt-report.key-06  = STRING(v-msf[1],"-9999999999999.99")
             xtt-report.key-07  = STRING(v-msf[2],"-9999999999999.99") 
             xtt-report.key-08  = STRING(v-cost,"-9999999999999.99").
             tt-report-key = RECID(xtt-report)
            .
             /* Handle YTD */
            CREATE xtt-report.
       
            ASSIGN
             xtt-report.key-01  = "2.1" 
             xtt-report.key-02  = tt-report.key-09
             xtt-report.key-03  = v-sman-no
             xtt-report.key-04  = STRING(v-amt[3],"-9999999999999.99")
             xtt-report.key-05  = STRING(v-amt[4],"-9999999999999.99")
             xtt-report.key-06  = STRING(v-msf[3],"-9999999999999.99")
             xtt-report.key-07  = STRING(v-msf[4],"-9999999999999.99") 
             xtt-report.key-08  = STRING(v-costYtd,"-9999999999999.99")
             xtt-report.rec-id   = tt-report-key
             xtt-report.rec_key   = STRING(tt-report-key).             
            
             /* Handle MTD */
            CREATE xtt-report.
       
            ASSIGN
             xtt-report.key-01  = "2.2" 
             xtt-report.key-02  = tt-report.key-09
             xtt-report.key-03  = v-sman-no
             xtt-report.key-04  = STRING(v-amt[5],"-9999999999999.99")
             xtt-report.key-05  = STRING(v-amt[6],"-9999999999999.99")
             xtt-report.key-06  = STRING(v-msf[5],"-9999999999999.99")
             xtt-report.key-07  = STRING(v-msf[6],"-9999999999999.99") 
             xtt-report.key-08  = STRING(v-costMtd,"-9999999999999.99")
             xtt-report.rec-id   = tt-report-key
             xtt-report.rec_key   = STRING(tt-report-key).                 
          END.
        END.  

        DELETE tt-report.
      END.

      ELSE
      IF tt-report.key-10 EQ "ar-cashl" THEN DO:
        FIND ar-cashl WHERE RECID(ar-cashl) EQ tt-report.rec-id NO-LOCK.
        FIND ar-cash  WHERE ar-cash.c-no    EQ ar-cashl.c-no NO-LOCK.

        ASSIGN
         v-amt = 0         
         v-msf = 0         
         lv-i-no  = "".
         
        DO v = 1 TO 6:
          IF ar-cash.check-date GE fdate[v] AND
             ar-cash.check-date LE tdate[v] THEN
            v-amt[v] = ar-cashl.amt-paid - ar-cashl.amt-disc. 
        END.
        
        ASSIGN
         tt-report.key-01 = "2"
         tt-report.key-02 = tt-report.key-09
         tt-report.key-03 = cust.sman
         tt-report.key-04 = STRING(v-amt[1],"-9999999999999.99")
         tt-report.key-05 = STRING(v-amt[2],"-9999999999999.99")
         tt-report.key-06 = STRING(v-msf[1],"-9999999999999.99")
         tt-report.key-07 = STRING(v-msf[2],"-9999999999999.99").
        FIND FIRST xtt-reportYtd 
          WHERE /*xtt-report.term-id EQ "" 
            AND */ xtt-reportYtd.key-01 = "2.1"
            AND xtt-reportYtd.rec_key = string(RECID(tt-report))
            USE-INDEX rec_key
            NO-ERROR.
        IF NOT AVAILABLE xtt-reportYtd THEN DO:
            CREATE xtt-reportYtD.
            ASSIGN xtt-reportYtd.key-01 = "2.1"
                   xtt-reportYtd.rec-id = RECID(tt-report)
                   xtt-reportYtd.rec_key = STRING(RECID(tt-report)).
        END.
        ASSIGN 
             xtt-reportYtd.key-02 = tt-report.key-09
             xtt-reportYtd.key-03 = cust.sman
             xtt-reportYtd.key-04 = STRING(v-amt[3],"-9999999999999.99")
             xtt-reportYtd.key-05 = STRING(v-amt[4],"-9999999999999.99")
             xtt-reportYtd.key-06 = STRING(v-msf[3],"-9999999999999.99")
             xtt-reportYtd.key-07 = STRING(v-msf[4],"-9999999999999.99").
             
        FIND FIRST xtt-reportMtd 
          WHERE /* xtt-reportMtd.term-id EQ ""
           AND */ xtt-reportMtd.key-01 = "2.2"
            AND xtt-reportMtd.rec-id = recid(tt-report)
          AND xtt-reportMtd.rec_key = string(RECID(tt-report))
            USE-INDEX rec_key
            NO-ERROR.        
        IF NOT AVAILABLE xtt-reportMtd THEN DO:
            CREATE xtt-reportMtd.
            ASSIGN xtt-reportMtd.key-01 = "2.2"
                   xtt-reportMtd.rec-id = RECID(tt-report)
                   xtt-reportMtd.rec_key = STRING(RECID(tt-report)).
        END.
        
        ASSIGN 
             xtt-reportMtd.key-02 = tt-report.key-09
             xtt-reportMtd.key-03 = cust.sman
             xtt-reportMtd.key-04 = STRING(v-amt[5],"-9999999999999.99")
             xtt-reportMtd.key-05 = STRING(v-amt[6],"-9999999999999.99")
             xtt-reportMtd.key-06 = STRING(v-msf[5],"-9999999999999.99")
             xtt-reportMtd.key-07 = STRING(v-msf[6],"-9999999999999.99").        
          
        RELEASE ar-inv.

        RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER reftable, BUFFER oe-retl).

        ASSIGN
         lv-r-no = 0
         lv-type = "".
          
        IF AVAILABLE reftable THEN
          ASSIGN
           lv-r-no = reftable.val[1]
           lv-type = reftable.dscr.
        ELSE
        IF ar-cashl.dscr MATCHES "*OE RETURN*" THEN
          ASSIGN
           lv-r-no = INT(SUBSTR(ar-cashl.dscr,INDEX(ar-cashl.dscr,"oe return") + 25,12))
           lv-type = TRIM(SUBSTR(ar-cashl.dscr,INDEX(ar-cashl.dscr,"oe return") + 12,10)).

        IF lv-r-no NE 0 THEN DO:
          FIND FIRST oe-reth
              WHERE oe-reth.company EQ cocode
                AND oe-reth.r-no    EQ lv-r-no
              NO-LOCK NO-ERROR.
          IF AVAILABLE oe-reth THEN
          FIND FIRST ar-inv
               WHERE ar-inv.company EQ cocode
                 AND ar-inv.cust-no EQ oe-reth.cust-no
                 AND ar-inv.inv-no  EQ oe-reth.inv-no
               NO-LOCK NO-ERROR.

          IF lv-type EQ "items" THEN DO:
            RELEASE ar-invl.
            FIND FIRST oe-retl
                WHERE oe-retl.company EQ cocode
                  AND oe-retl.r-no    EQ oe-reth.r-no
                  AND oe-retl.line    EQ ar-cashl.line
                  AND oe-retl.i-no    GE begin_i-no
                  AND oe-retl.i-no    LE end_i-no
                NO-LOCK NO-ERROR.

            IF AVAILABLE oe-retl THEN DO:
              FIND FIRST itemfg
                  WHERE itemfg.company EQ cocode
                    AND itemfg.i-no    EQ oe-retl.i-no
                  NO-LOCK NO-ERROR.

              DO v = 1 TO 6:
                IF ar-cash.check-date GE fdate[v] AND
                   ar-cash.check-date LE tdate[v] THEN
                  v-msf[v] = IF AVAILABLE itemfg THEN
                               (oe-retl.tot-qty-return * itemfg.t-sqft / 1000)
                             ELSE 0.
                IF v-msf[v] EQ ? THEN v-msf[v] = 0.
              END.
            END.

            IF AVAILABLE oe-retl THEN DO:

                /* Search for a matching ar-invl and leave if found so that it is available below */                
                SEARCH-ARINVL:
                FOR EACH ar-invl NO-LOCK
                    WHERE ar-invl.company EQ cocode
                      AND ar-invl.cust-no EQ ar-cash.cust-no
                      AND ar-invl.inv-no  EQ ar-cashl.inv-no
                      AND ar-invl.i-no    EQ oe-retl.i-no
                      AND (ar-invl.billable OR NOT ar-invl.misc)
                    :                                                       
                      
                      DO i = 1 TO 3:
                        v-sman-no = IF ar-invl.sman[i] EQ "" AND i EQ 1 THEN cust.sman
                                ELSE ar-invl.sman[i].
                    
                        IF v-sman-no  LT fsman                          OR
                           v-sman-no  GT tsman                          OR
                           (i NE 1 AND
                            (v-sman-no EQ "" OR ar-invl.s-pct[i] EQ 0)) THEN NEXT.
                            LEAVE SEARCH-ARINVL.
                        
                      END. /* do i = 1 to 3 */
                END. /* each ar-invl */    
                       
            END. /* if avail oe-retl */
            
                
            IF AVAILABLE ar-invl THEN DO:
                
              
              DO i = 1 TO 3:
                v-sman-no = IF ar-invl.sman[i] EQ "" AND i EQ 1 THEN cust.sman
                            ELSE ar-invl.sman[i].

                IF v-sman-no  LT fsman                          OR
                   v-sman-no  GT tsman                          OR
                   (i NE 1 AND
                    (v-sman-no EQ "" OR ar-invl.s-pct[i] EQ 0)) THEN NEXT.

                v-pct = ar-invl.s-pct[i] / 100.

                IF v-pct EQ 0 THEN
                DO v = 1 TO 3:
                  IF v EQ 1 THEN j = 0.
                  IF ar-invl.sman[v] NE "" THEN j = j + 1.
                  IF v EQ 3 THEN v-pct = 1 / j.
                END.
                
                IF v-pct LE 0 OR v-pct EQ ? THEN v-pct = 1.
                RUN salrep/salecost.p (3,
                                   ROWID(ar-invl),
                                   oe-retl.job-no,
                                   oe-retl.job-no2,
                                   oe-retl.tot-qty-return,
                                   OUTPUT ld-cost).
                ld-cost = ld-cost * v-pct.
                CREATE xtt-report.

                ASSIGN
                 xtt-report.key-01  = "2"
                 xtt-report.key-02  = tt-report.key-09
                 xtt-report.key-03  = v-sman-no
                 xtt-report.key-04  = STRING(v-amt[1] * v-pct,"-9999999999999.99")
                 xtt-report.key-05  = STRING(v-amt[2] * v-pct,"-9999999999999.99")
                 xtt-report.key-06  = STRING(v-msf[1] * v-pct,"-9999999999999.99")
                 xtt-report.key-07  = STRING(v-msf[2] * v-pct,"-9999999999999.99")
                 xtt-report.key-08  = STRING(ld-cost * (-1),"-9999999999999.99").
                 tt-report-key = RECID(xtt-report)
                 .
                 /* Handle YTD */
                CREATE xtt-report.           
                ASSIGN
                 xtt-report.key-01  = "2.1" 
                 xtt-report.key-02  = tt-report.key-09
                 xtt-report.key-03  = v-sman-no
                 xtt-report.key-04  = STRING(v-amt[3],"-9999999999999.99")
                 xtt-report.key-05  = STRING(v-amt[4],"-9999999999999.99")
                 xtt-report.key-06  = STRING(v-msf[3],"-9999999999999.99")
                 xtt-report.key-07  = STRING(v-msf[4],"-9999999999999.99") 
                 xtt-report.key-08  = STRING(ld-cost * (-1),"-9999999999999.99")
                 xtt-report.rec-id   = tt-report-key
                 xtt-report.rec_key   = STRING(tt-report-key).             
                
                 /* Handle MTD */
                CREATE xtt-report.
           
                ASSIGN
                 xtt-report.key-01  = "2.2" 
                 xtt-report.key-02  = tt-report.key-09
                 xtt-report.key-03  = v-sman-no
                 xtt-report.key-04  = STRING(v-amt[5],"-9999999999999.99")
                 xtt-report.key-05  = STRING(v-amt[6],"-9999999999999.99")
                 xtt-report.key-06  = STRING(v-msf[5],"-9999999999999.99")
                 xtt-report.key-07  = STRING(v-msf[6],"-9999999999999.99") 
                 xtt-report.key-08  = STRING(ld-cost * (-1),"-9999999999999.99")
                 xtt-report.rec-id   = tt-report-key
                xtt-report.rec_key   = STRING(tt-report-key).                    
                
              END.

              DELETE tt-report.
            END.
          END.

          ELSE
          IF lv-type EQ "freight" OR
             lv-type EQ "tax"     THEN
            lv-i-no = lv-type.
        END.  

        IF AVAILABLE tt-report THEN
          IF tt-report.key-03 LT fsman      OR
             tt-report.key-03 GT tsman      OR
             lv-i-no          LT begin_i-no OR
             lv-i-no          GT end_i-no   THEN DELETE tt-report.
      END.     
    END.

    FOR EACH tt-report
        WHERE tt-report.term-id EQ ""
          AND tt-report.key-01  EQ "2"
        NO-LOCK
        BREAK BY tt-report.key-02:

      IF FIRST-OF(tt-report.key-02) THEN
        ASSIGN
         v-amt = 0
         v-msf = 0
         v-cost = 0
         v-costYtd = 0
         v-costMtd = 0 .
         
      FIND FIRST xtt-reportYtd 
        WHERE  /*xtt-reportYtd.term-id eq ""
          and  */ xtt-reportYtd.key-01  EQ "2.1"
          AND xtt-reportYtd.rec-id = recid(tt-report) 
          AND xtt-reportYtd.rec_key = string(RECID(tt-report))
        USE-INDEX rec_key  NO-ERROR.
      FIND FIRST xtt-reportMtd 
      WHERE  /*xtt-reportMtd.term-id eq "" 
          and  */ xtt-reportMtd.key-01  EQ "2.2"
          AND xtt-reportMtd.rec-id = recid(tt-report)
          AND xtt-reportMtd.rec_key = string(RECID(tt-report))
        USE-INDEX rec_key  NO-ERROR.
      
        
      ASSIGN
       v-amt[1] = v-amt[1] + dec(tt-report.key-04)
       v-amt[2] = v-amt[2] + dec(tt-report.key-05)
       v-msf[1] = v-msf[1] + dec(tt-report.key-06)
       v-msf[2] = v-msf[2] + dec(tt-report.key-07)
       v-cost = v-cost + DEC(tt-report.key-08)
       
       v-amt[3] = v-amt[3] + dec(xtt-reportYtd.key-04)
       v-amt[4] = v-amt[4] + dec(xtt-reportYtd.key-05)
       v-msf[3] = v-msf[3] + dec(xtt-reportYtd.key-06)
       v-msf[4] = v-msf[4] + dec(xtt-reportYtd.key-07)
       v-costYtd = v-costYtd + DEC(xtt-reportYtd.key-08)
       
       v-amt[5] = v-amt[5] + dec(xtt-reportMtd.key-04)
       v-amt[6] = v-amt[6] + dec(xtt-reportMtd.key-05)
       v-msf[5] = v-msf[5] + dec(xtt-reportMtd.key-06)
       v-msf[6] = v-msf[6] + dec(xtt-reportMtd.key-07)
       v-costMtd = v-costMtd + DEC(xtt-reportMtd.key-08)       
       .

      IF LAST-OF(tt-report.key-02) THEN DO:
        CREATE xtt-report.
        ASSIGN
         xtt-report.key-01  = "1"
         xtt-report.key-02  = IF v-sort EQ "C" THEN tt-report.key-02 ELSE "" 
         xtt-report.key-03  = STRING(v-amt[1],"-9999999999999.99")
         xtt-report.key-04  = STRING(v-amt[2],"-9999999999999.99")
         xtt-report.key-05  = tt-report.key-02
         xtt-report.key-06  = STRING(v-msf[1],"-9999999999999.99")
         xtt-report.key-07  = STRING(v-msf[2],"-9999999999999.99")
         xtt-report.key-08  = STRING(v-cost,"-9999999999999.99")    
             .
        CREATE xtt-reportYtd.
        ASSIGN
         xtt-reportYtd.key-01  = "1.1"
         xtt-reportYtd.key-02  = IF v-sort EQ "C" THEN tt-report.key-02 ELSE "" 
         xtt-reportYtd.key-03  = STRING(v-amt[3],"-9999999999999.99")
         xtt-reportYtd.key-04  = STRING(v-amt[4],"-9999999999999.99")
         xtt-reportYtd.key-05  = tt-report.key-02
         xtt-reportYtd.key-06  = STRING(v-msf[3],"-9999999999999.99")
         xtt-reportYtd.key-07  = STRING(v-msf[4],"-9999999999999.99")
         xtt-reportYtd.key-08  = STRING(v-costYtd,"-9999999999999.99")    
         xtt-reportYtd.rec-id  = RECID(xtt-report)
         xtt-reportYtd.rec_key  = STRING(RECID(xtt-report))
             .
             
        CREATE xtt-reportMtd.
        ASSIGN
         xtt-reportMtd.key-01  = "1.2"
         xtt-reportMtd.key-02  = IF v-sort EQ "C" THEN tt-report.key-02 ELSE "" 
         xtt-reportMtd.key-03  = STRING(v-amt[5],"-9999999999999.99")
         xtt-reportMtd.key-04  = STRING(v-amt[6],"-9999999999999.99")
         xtt-reportMtd.key-05  = tt-report.key-02
         xtt-reportMtd.key-06  = STRING(v-msf[5],"-9999999999999.99")
         xtt-reportMtd.key-07  = STRING(v-msf[6],"-9999999999999.99")
         xtt-reportMtd.key-08  = STRING(v-costMtd,"-9999999999999.99")    
         xtt-reportMtd.rec-id  = RECID(xtt-report)
         xtt-reportMtd.rec_key  = STRING(RECID(xtt-report))
             .
      END.
    END.

    PUT SKIP(1).

    FOR EACH xtt-report
        WHERE xtt-report.term-id EQ ""
          AND xtt-report.key-01  EQ "1",

        FIRST cust
        WHERE cust.company EQ cocode
          AND cust.cust-no EQ xtt-report.key-05
        NO-LOCK

        BREAK BY xtt-report.key-02
              BY (IF v-sort EQ "M" THEN dec(xtt-report.key-06)
                                   ELSE dec(xtt-report.key-03)) DESCENDING
              BY (IF v-sort EQ "M" THEN dec(xtt-report.key-07)
                                   ELSE dec(xtt-report.key-04)) DESCENDING
              BY xtt-report.key-05:      

      ASSIGN
       v-one     = YES
       ll-first  = YES
       v-sman-no = "xxxxxx".
      
      FOR EACH tt-report
          WHERE tt-report.term-id EQ ""
            AND tt-report.key-01  EQ "2"
            AND tt-report.key-02  EQ xtt-report.key-05

          BREAK BY tt-report.key-03

          TRANSACTION:
              
        FIND FIRST xtt-reportYtd 
        WHERE /*xtt-reportYtd.term-id eq ""
          and */ xtt-reportYtd.key-01  EQ "2.1"
          AND xtt-reportYtd.rec-id = recid(tt-report) 
          AND xtt-reportYtd.rec_key = string(RECID(tt-report))
          USE-INDEX rec_key NO-ERROR.
          
        FIND FIRST xtt-reportMtd 
        WHERE /* xtt-reportMtd.term-id eq ""
          and */ xtt-reportMtd.key-01  EQ "2.2"
          AND xtt-reportMtd.rec-id = recid(tt-report) 
          AND xtt-reportMtd.rec_key = string(RECID(tt-report))
          USE-INDEX rec_key NO-ERROR.                   
         
        IF FIRST-OF(tt-report.key-03) THEN
          ASSIGN
           v-amt = 0           
           v-msf = 0           
           v-cost = 0
           v-costYtd = 0
           v-costMtd = 0
           .

        ASSIGN
         v-amt[1] = v-amt[1] + dec(tt-report.key-04)
         v-amt[2] = v-amt[2] + dec(tt-report.key-05)
         v-msf[1] = v-msf[1] + dec(tt-report.key-06)
         v-msf[2] = v-msf[2] + dec(tt-report.key-07)
         v-cost = v-cost + dec(tt-report.key-08) 
         v-costTotal = v-costTotal + DEC(tt-report.key-08)  
         .
        ASSIGN
         v-amt[3] = v-amt[3] + dec(xtt-reportYtd.key-04)
         v-amt[4] = v-amt[4] + dec(xtt-reportYtd.key-05)
         v-msf[3] = v-msf[3] + dec(xtt-reportYtd.key-06)
         v-msf[4] = v-msf[4] + dec(xtt-reportYtd.key-07)
         v-costYtd = v-costYtd + dec(xtt-reportYtd.key-08) 
         v-costTotalYtd = v-costTotalYtd + DEC(xtt-reportYtd.key-08)  
         .
        ASSIGN
         v-amt[5] = v-amt[5] + dec(xtt-reportmtd.key-04)
         v-amt[6] = v-amt[6] + dec(xtt-reportMtd.key-05)
         v-msf[5] = v-msf[5] + dec(xtt-reportMtd.key-06)
         v-msf[6] = v-msf[6] + dec(xtt-reportMtd.key-07)
         v-costMtd = v-costMtd + dec(xtt-reportMtd.key-08) 
         v-costTotalMtd = v-costTotalMtd + DEC(xtt-reportMtd.key-08)  
         .        
         
        IF LAST-OF(tt-report.key-03)            AND
           (NOT tb_exc-zero OR
            v-amt[1] NE 0   OR
            v-amt[2] NE 0   /*OR
            (/*tb_msf AND
             (v-msf[1] NE 0 OR v-msf[2] NE 0)*/ )*/  ) THEN DO:

          IF v-sman-no EQ "xxxxxx" THEN v-sman-no = tt-report.key-03.

          IF v-sman-no NE tt-report.key-03 THEN v-one = NO.

          ASSIGN
           v-diff = v-amt[1] - v-amt[2]
           v-pct  = v-amt[1] / v-amt[2] * 100
           v-diffMSF = v-msf[1] - v-msf[2]
           v-pctMSF = v-msf[1] / v-msf[2] * 100  
           v-profit = v-amt[1] - v-cost   
           v-diffYtd = v-amt[3] - v-amt[4]
           v-pctYtd  = v-amt[3] / v-amt[4] * 100
           v-diffMtd = v-amt[5] - v-amt[6]
           v-pctMtd  = v-amt[5] / v-amt[6] * 100
          .

          IF v-pct EQ ? THEN v-pct = 0.
          IF v-pctMSF = ? THEN v-pctMSF = 0.
          IF v-pctYtd = ? THEN v-pctYtd = 0.
          IF v-pctMtd = ? THEN v-pctMtd = 0.
          
          ASSIGN cDisplay = ""
               cTmpField = ""
               cVarValue = ""
               cExcelDisplay = ""
               cExcelVarValue = "".

              BUFFER bcust:FIND-BY-ROWID(ROWID(cust), NO-LOCK) .
              DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                  cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                  IF INDEX(cTmpField,".") > 0 THEN DO:
                            cFieldName = cTmpField.
                            cTmpField = SUBSTRING(cTmpField,INDEX(cTmpField,".") + 1).
                            hField = BUFFER bcust:BUFFER-FIELD(cTmpField).
                            IF hField <> ? THEN DO:                 
                                cTmpField = SUBSTRING(GetFieldValue(hField),1,int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength))).
                                cDisplay = cDisplay + 
                                          IF ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldType) = "C" THEN
                                            (cTmpField + FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cTmpField)))
                                          ELSE IF LENGTH(cTmpField) <  int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) THEN
                                            (FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) - LENGTH(cTmpField)) + cTmpField) + " "
                                          ELSE cTmpField.
                                cExcelDisplay = cExcelDisplay + quoter(GetFieldValue(hField)) + ",".   

                            END.
                            ELSE DO:
                               cTmpField = SUBSTRING(cFieldName,1,int( ENTRY( getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength) ) ).                  
                               cDisplay = cDisplay + FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 ).
                               cExcelDisplay = cExcelDisplay + quoter(" ") + ",".
                            END.
                  END.
                  ELSE DO: 
                       CASE cTmpField:                                          
                            WHEN "v-salesRep" THEN cVarValue = tt-report.key-03.
                            WHEN "v-amt[1]" THEN cVarValue = IF v-amt[1] = 0 THEN "" ELSE STRING(v-amt[1],"->,>>>,>>>,>>>.99").
                            WHEN "v-amt[2]" THEN cVarValue = IF v-amt[2] = 0 THEN "" ELSE STRING(v-amt[2],"->,>>>,>>>,>>>.99").
                            WHEN "v-amt[3]" THEN cVarValue = IF v-amt[3] = 0 THEN "" ELSE STRING(v-amt[3],"->,>>>,>>>,>>>.99").
                            WHEN "v-amt[4]" THEN cVarValue = IF v-amt[4] = 0 THEN "" ELSE STRING(v-amt[4],"->,>>>,>>>,>>>.99").
                            WHEN "v-amt[5]" THEN cVarValue = IF v-amt[5] = 0 THEN "" ELSE STRING(v-amt[5],"->,>>>,>>>,>>>.99").
                            WHEN "v-amt[6]" THEN cVarValue = IF v-amt[6] = 0 THEN "" ELSE STRING(v-amt[6],"->,>>>,>>>,>>>.99").
                            WHEN "v-msf[1]" THEN cVarValue = IF v-msf[1] = 0 THEN "" ELSE STRING(v-msf[1],"->,>>>,>>>,>>>.99").
                            WHEN "v-msf[2]" THEN cVarValue = IF v-msf[2] = 0 THEN "" ELSE STRING(v-msf[2],"->,>>>,>>>,>>>.99").
                            WHEN "v-msf[3]" THEN cVarValue = IF v-msf[3] = 0 THEN "" ELSE STRING(v-msf[3],"->,>>>,>>>,>>>.99").
                            WHEN "v-msf[4]" THEN cVarValue = IF v-msf[4] = 0 THEN "" ELSE STRING(v-msf[4],"->,>>>,>>>,>>>.99").
                            WHEN "v-msf[5]" THEN cVarValue = IF v-msf[5] = 0 THEN "" ELSE STRING(v-msf[5],"->,>>>,>>>,>>>.99").
                            WHEN "v-msf[6]" THEN cVarValue = IF v-msf[6] = 0 THEN "" ELSE STRING(v-msf[6],"->,>>>,>>>,>>>.99").                            
                            WHEN "v-diff" THEN   cVarValue = IF v-diff = 0 THEN "" ELSE STRING(v-diff,"->,>>>,>>>,>>>.99").
                            WHEN "v-diffYtd" THEN   cVarValue = IF v-diffYTD = 0 THEN "" ELSE STRING(v-diffYTD,"->,>>>,>>>,>>>.99").
                            WHEN "v-diffMtd" THEN   cVarValue = IF v-diffMTD = 0 THEN "" ELSE STRING(v-diffMTD,"->,>>>,>>>,>>>.99").                            
                            WHEN "v-diffMSF" THEN cVarValue = IF v-diffMSF = 0 THEN "" ELSE STRING(v-diffMSF,"->,>>>,>>>,>>>.99").
                            WHEN "v-pct" THEN    cVarValue = IF v-pct = 0 THEN "" ELSE STRING(v-pct,"->,>>>,>>>,>>>.99").
                            WHEN "v-pctYtd" THEN    cVarValue = IF v-pctYTD = 0 THEN "" ELSE STRING(v-pctYTD,"->,>>>,>>>,>>>.99").
                            WHEN "v-pctMtd" THEN    cVarValue = IF v-pctMTD = 0 THEN "" ELSE STRING(v-pctMTD,"->,>>>,>>>,>>>.99").
                            WHEN "v-pctMSF" THEN cVarValue = IF v-pctMSF = 0 THEN "" ELSE STRING(v-pctMSF,"->,>>>,>>>,>>>.99").
                            WHEN "v-profit" THEN cVarValue = IF v-profit = 0 THEN "" ELSE STRING(v-profit,"->,>>>,>>>,>>>.99"). 
                            WHEN "v-cost" THEN cVarValue = IF v-cost = 0 THEN "" ELSE STRING(v-cost,"->,>>>,>>>,>>>.99"). 
                       END CASE.
                       cExcelVarValue = cVarValue.  
                       cDisplay = cDisplay + cVarValue +
                                  FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
                       cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",". 
                  END.
              END.
              
              PUT UNFORMATTED cDisplay SKIP.
              IF tb_excel THEN DO:
                    PUT STREAM excel UNFORMATTED  
                          cExcelDisplay SKIP.
              END.
              /*=============================*/

          ll-first = NO.
        END.
      END.
    
      FIND FIRST xtt-reportYtd 
        WHERE /*xtt-reportYtd.term-id eq ""
          and */ xtt-reportYtd.key-01  EQ "1.1"
          AND xtt-reportYtd.rec-id = recid(xtt-report)
          AND xtt-reportYtd.rec_key = string(RECID(xtt-report))
      USE-INDEX rec_key NO-ERROR.
      FIND FIRST xtt-reportMtd 
        WHERE /* xtt-reportMtd.term-id eq ""
          and */ xtt-reportMtd.key-01  EQ "1.2"
          AND xtt-reportMtd.rec-id = recid(xtt-report)
        AND xtt-reportMtd.rec_key = STRING(RECID(xtt-report)) 
        USE-INDEX rec_key NO-ERROR.
       
      ASSIGN
       v-amt[1] = dec(xtt-report.key-03)
       v-amt[2] = dec(xtt-report.key-04)
       v-msf[1] = dec(xtt-report.key-06)
       v-msf[2] = dec(xtt-report.key-07)       
       v-cost = DEC(xtt-report.key-08)
       v-diff   = v-amt[1] - v-amt[2]
       v-pct    = v-amt[1] / v-amt[2] * 100              
       v-diffMSF = v-msf[1] - v-msf[2]
       v-pctMSF = v-msf[1] / v-msf[2] * 100       
       v-profit = v-amt[1] - v-cost   
       .
      ASSIGN
       v-amt[3] = dec(xtt-reportYtd.key-03)
       v-amt[4] = dec(xtt-reportYtd.key-04)
       v-msf[3] = dec(xtt-reportYtd.key-06)
       v-msf[4] = dec(xtt-reportYtd.key-07)       
       v-costYtd = DEC(xtt-reportYtd.key-08)
       v-diffYtd   = v-amt[3] - v-amt[4]
       v-pctYtd    = v-amt[3] / v-amt[4] * 100              
       v-diffMSFYtd = v-msf[3] - v-msf[4]
       v-pctMSFYtd = v-msf[3] / v-msf[4] * 100       
       v-profitYtd = v-amt[3] - v-costYtd   
       .       
      
      ASSIGN
       v-amt[5] = dec(xtt-reportMtd.key-03)
       v-amt[6] = dec(xtt-reportMtd.key-04)
       v-msf[5] = dec(xtt-reportMtd.key-06)
       v-msf[6] = dec(xtt-reportMtd.key-07)       
       v-costMtd = DEC(xtt-reportMtd.key-08)
       v-diffMtd   = v-amt[5] - v-amt[6]
       v-pctMtd    = v-amt[5] / v-amt[6] * 100              
       v-diffMSFMtd = v-msf[5] - v-msf[6]
       v-pctMSFMtd = v-msf[5] / v-msf[6] * 100       
       v-profitMtd = v-amt[5] - v-costMtd   
       .       

              
      IF v-pct EQ ? THEN v-pct = 0.
      IF v-pctYtd EQ ? THEN v-pctYtd = 0.
      IF v-pctMtd EQ ? THEN v-pctMtd = 0.
      IF v-pctMSF = ? THEN v-pctMSF = 0.
      IF v-pctMSFYtd = ? THEN v-pctMSFYtd = 0.
      IF v-pctMSFmTD = ? THEN v-pctMSFMtd = 0.
      
      /* customer total */
      IF NOT v-one                            AND
         (NOT tb_exc-zero OR
          v-amt[1] NE 0   OR
          v-amt[2] NE 0   /*OR
          (/*tb_msf AND
           (v-msf[1] NE 0 OR v-msf[2] NE 0)*/ )*/ ) THEN DO:

          PUT   
            SKIP SPACE(36)  str-line  /*  Task 12041302 */
            /*fill("-",int(entry(4,cFieldLength))) FORMAT "x(17)" " "
            fill("-",int(entry(5,cFieldLength))) FORMAT "x(17)" " "
            fill("-",int(entry(6,cFieldLength))) FORMAT "x(17)" " "
            fill("-",int(entry(7,cFieldLength))) FORMAT "x(17)" " "
            fill("-",int(entry(8,cFieldLength))) FORMAT "x(17)" " "
            fill("-",int(entry(9,cFieldLength))) FORMAT "x(17)" " "
            fill("-",int(entry(10,cFieldLength))) FORMAT "x(17)" " "
            fill("-",int(entry(10,cFieldLength))) FORMAT "x(17)" " "
              fill("-",int(entry(10,cFieldLength))) FORMAT "x(17)" " "
            fill("-",int(entry(10,cFieldLength))) FORMAT "x(17)" " "*/
             SKIP.

          ASSIGN cDisplay = ""
               cTmpField = ""
               cVarValue = ""
               cExcelDisplay = ""
               cExcelVarValue = "".

              BUFFER bcust:FIND-BY-ROWID(ROWID(cust), NO-LOCK) .
              DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                  cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                  IF INDEX(cTmpField,".") > 0 THEN DO:
                            cFieldName = cTmpField.
                            cTmpField = SUBSTRING(cTmpField,INDEX(cTmpField,".") + 1).
                            hField = BUFFER bcust:BUFFER-FIELD(cTmpField).
                            IF hField <> ? THEN DO:                 
                                cTmpField = SUBSTRING(GetFieldValue(hField),1,int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength))).
                                cDisplay = cDisplay + 
                                          IF ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldType) = "C" THEN
                                            (cTmpField + FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cTmpField)))
                                          ELSE IF LENGTH(cTmpField) <  int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) THEN
                                            (FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) - LENGTH(cTmpField)) + cTmpField) + " "
                                          ELSE cTmpField.
                                cExcelDisplay = cExcelDisplay + quoter(GetFieldValue(hField)) + ",".   

                            END.
                            ELSE DO:
                               cTmpField = SUBSTRING(cFieldName,1,int( ENTRY( getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength) ) ).                  
                               cDisplay = cDisplay + FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 ).
                               cExcelDisplay = cExcelDisplay + quoter(" ") + ",".
                            END.
                  END.
                  ELSE DO:      
                       CASE cTmpField:                                          
                            WHEN "v-salesRep" THEN cVarValue = "" /*tt-report.key-03*/.
                            WHEN "v-amt[1]" THEN cVarValue = IF v-amt[1] = 0 THEN "" ELSE STRING(v-amt[1],"->,>>>,>>>,>>>.99").
                            WHEN "v-amt[2]" THEN cVarValue = IF v-amt[2] = 0 THEN "" ELSE STRING(v-amt[2],"->,>>>,>>>,>>>.99").
                            WHEN "v-msf[1]" THEN cVarValue = IF v-msf[1] = 0 THEN "" ELSE STRING(v-msf[1],"->,>>>,>>>,>>>.99").
                            WHEN "v-msf[2]" THEN cVarValue = IF v-msf[2] = 0 THEN "" ELSE STRING(v-msf[2],"->,>>>,>>>,>>>.99").
                            WHEN "v-diff" THEN   cVarValue = IF v-diff = 0 THEN "" ELSE STRING(v-diff,"->,>>>,>>>,>>>.99").
                            WHEN "v-diffMSF" THEN cVarValue = IF v-diffMSF = 0 THEN "" ELSE STRING(v-diffMSF,"->,>>>,>>>,>>>.99").
                            WHEN "v-pct" THEN    cVarValue = IF v-pct = 0 THEN "" ELSE STRING(v-pct,"->,>>>,>>>,>>>.99").
                            WHEN "v-pctMSF" THEN cVarValue = IF v-pctMSF = 0 THEN "" ELSE STRING(v-pctMSF,"->,>>>,>>>,>>>.99").
                            
                            WHEN "v-amt[3]" THEN cVarValue = IF v-amt[3] = 0 THEN "" ELSE STRING(v-amt[3],"->,>>>,>>>,>>>.99").
                            WHEN "v-amt[4]" THEN cVarValue = IF v-amt[4] = 0 THEN "" ELSE STRING(v-amt[4],"->,>>>,>>>,>>>.99").
                            WHEN "v-msf[3]" THEN cVarValue = IF v-msf[3] = 0 THEN "" ELSE STRING(v-msf[3],"->,>>>,>>>,>>>.99").
                            WHEN "v-msf[4]" THEN cVarValue = IF v-msf[4] = 0 THEN "" ELSE STRING(v-msf[4],"->,>>>,>>>,>>>.99").
                            WHEN "v-diffYtd" THEN   cVarValue = IF v-diffYtd = 0 THEN "" ELSE STRING(v-diffYtd,"->,>>>,>>>,>>>.99").
                            WHEN "v-diffMSFYtd" THEN cVarValue = IF v-diffMSFYtd = 0 THEN "" ELSE STRING(v-diffMSFYtd,"->,>>>,>>>,>>>.99").
                            WHEN "v-pctYtd" THEN    cVarValue = IF v-pctYtd = 0 THEN "" ELSE STRING(v-pctYtd,"->,>>>,>>>,>>>.99").
                            WHEN "v-pctMSFYtd" THEN cVarValue = IF v-pctMSFYtd = 0 THEN "" ELSE STRING(v-pctMSFYtd,"->,>>>,>>>,>>>.99").
                            
                            WHEN "v-amt[5]" THEN cVarValue = IF v-amt[5] = 0 THEN "" ELSE STRING(v-amt[5],"->,>>>,>>>,>>>.99").
                            WHEN "v-amt[6]" THEN cVarValue = IF v-amt[6] = 0 THEN "" ELSE STRING(v-amt[6],"->,>>>,>>>,>>>.99").
                            WHEN "v-msf[5]" THEN cVarValue = IF v-msf[5] = 0 THEN "" ELSE STRING(v-msf[5],"->,>>>,>>>,>>>.99").
                            WHEN "v-msf[6]" THEN cVarValue = IF v-msf[6] = 0 THEN "" ELSE STRING(v-msf[6],"->,>>>,>>>,>>>.99").
                            WHEN "v-diffMtd" THEN   cVarValue = IF v-diffMtd = 0 THEN "" ELSE STRING(v-diffMtd,"->,>>>,>>>,>>>.99").
                            WHEN "v-diffMSF" THEN cVarValue = IF v-diffMSFMtd = 0 THEN "" ELSE STRING(v-diffMSFMtd,"->,>>>,>>>,>>>.99").
                            WHEN "v-pctMtd" THEN    cVarValue = IF v-pctMtd = 0 THEN "" ELSE STRING(v-pctMtd,"->,>>>,>>>,>>>.99").
                            WHEN "v-pctMSFMtd" THEN cVarValue = IF v-pctMSFMtd = 0 THEN "" ELSE STRING(v-pctMSFMtd,"->,>>>,>>>,>>>.99").
                            
                            WHEN "v-profit" THEN cVarValue = IF v-profit = 0 THEN "" ELSE STRING(v-profit,"->,>>>,>>>,>>>.99"). 
                            WHEN "v-cost" THEN cVarValue = IF v-cost = 0 THEN "" ELSE STRING(v-cost,"->,>>>,>>>,>>>.99"). 
                       END CASE.
                       cExcelVarValue = cVarValue.  
                       cDisplay = cDisplay + cVarValue +
                                  FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
                       cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",". 
                  END.
              END.
              
              PUT UNFORMATTED cDisplay SKIP.
/*               IF tb_excel THEN DO:               */
/*                     PUT STREAM excel UNFORMATTED */
/*                           cExcelDisplay SKIP.    */
/*               END.                               */

      END.  
      
      DO v = 1 TO 6:
    
        ASSIGN
         v-t-amt[v] = v-t-amt[v] + v-amt[v]
         v-t-msf[v] = v-t-msf[v] + v-msf[v].
      END.
              
      /* display cust totals */
      IF LAST(xtt-report.key-02) THEN DO:
         
        ASSIGN
         v-diff = v-t-amt[1] - v-t-amt[2]
         v-pct  = v-t-amt[1] / v-t-amt[2] * 100
         v-diffMSF = v-t-msf[1] - v-t-msf[2]
         v-pctMSF = v-t-msf[1] / v-t-msf[2] * 100   
         
         v-diffYtd = v-t-amt[3] - v-t-amt[4]         
         v-pctYtd  = v-t-amt[3] / v-t-amt[4] * 100
         v-diffMSFYTD = v-t-msf[3] - v-t-msf[4]
         v-pctMSFYTD = v-t-msf[3] / v-t-msf[4] * 100 
         
         v-diffMtd = v-t-amt[5] - v-t-amt[6]
         v-pctMtd  = v-t-amt[5] / v-t-amt[6] * 100
         v-diffMSFMTD = v-t-msf[5] - v-t-msf[6]
         v-pctMSFMTD = v-t-msf[5] / v-t-msf[6] * 100 
         .

        IF v-pct EQ ? THEN v-pct = 0.
        IF v-pctMSF EQ ? THEN v-pctMSF = 0.
        IF v-pctYtd EQ ? THEN v-pctYtd = 0.
        IF v-pctMSFYtd EQ ? THEN v-pctMSFYtd = 0.
        IF v-pctMtd EQ ? THEN v-pctMtd = 0.
        IF v-pctMSFMtd EQ ? THEN v-pctMSFMtd = 0.
        
        PUT   
            SKIP SPACE(36) str-line SKIP   /*  Task 12041302 */
            /*fill("-",int(entry(4,cFieldLength))) FORMAT "x(17)" " "
            fill("-",int(entry(5,cFieldLength))) FORMAT "x(17)" " "
            fill("-",int(entry(6,cFieldLength))) FORMAT "x(17)" " "
            fill("-",int(entry(7,cFieldLength))) FORMAT "x(17)" " "
            fill("-",int(entry(8,cFieldLength))) FORMAT "x(17)" " "
            fill("-",int(entry(9,cFieldLength))) FORMAT "x(17)" " "
            fill("-",int(entry(10,cFieldLength))) FORMAT "x(17)" " "
            fill("-",int(entry(10,cFieldLength))) FORMAT "x(17)" " "
            fill("-",int(entry(10,cFieldLength))) FORMAT "x(17)" " "
            fill("-",int(entry(10,cFieldLength))) FORMAT "x(17)" " " 
            skip*/
            /*"Grand Totals" 
            SPACE(28)              
                v-amt[3] FORM "->>>>>>>>>>>>>.99" " "
                v-amt[4] FORM "->>>>>>>>>>>>>.99" " "
                v-diff   FORM "->>>>>>>>>>>>>.99" " "
                v-pct  FORM "->>>>>>>>>>>>>.99" " "
                v-msf[3] FORM "->>>>>>>>>>>>>.99" " "                
                v-msf[4] FORM "->>>>>>>>>>>>>.99" " "
                v-diffMSF  FORM "->>>>>>>>>>>>>.99" " "
                v-pctmsf  FORM "->>>>>>>>>>>>>.99" " "
                v-costTotal FORM "->>>>>>>>>>>>>.99" " "
                v-amt[3] - v-costTotal FORM "->>>>>>>>>>>>>.99"  
            SKIP*/
            .
       /*  IF tb_excel THEN 
            PUT STREAM excel UNFORMATTED 
                "Grand Totals," ",,"
                v-amt[3] ","
                v-amt[4] ","
                v-diff   ","
                v-pct   ","
                v-msf[3] ","                
                v-msf[4] ","
                v-diffMSF  ","
                v-pctmsf   ","
                v-costTotal ","
                v-amt[3] - v-costTotal.*/
        /*  Task 12041302 .. */
         ASSIGN cDisplay = ""
               cTmpField = ""
               cVarValue = ""
               cExcelDisplay = ""
               cExcelVarValue = "".

              BUFFER bcust:FIND-BY-ROWID(ROWID(cust), NO-LOCK) .
              DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                  cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                   CASE cTmpField:                                          
                            WHEN "cust.cust-no" THEN cVarValue = "" /*tt-report.key-03*/.
                            WHEN "cust.NAME" THEN cVarValue = "Grand Totals" .
                            WHEN "v-salesRep" THEN cVarValue = "" .
                            WHEN "v-amt[1]" THEN cVarValue = IF v-t-amt[1] = 0 THEN "" ELSE STRING(v-t-amt[1],"->,>>>,>>>,>>>.99").
                            WHEN "v-amt[2]" THEN cVarValue = IF v-t-amt[2] = 0 THEN "" ELSE STRING(v-t-amt[2],"->,>>>,>>>,>>>.99").
                            WHEN "v-msf[1]" THEN cVarValue = IF v-t-msf[1] = 0 THEN "" ELSE STRING(v-t-msf[1],"->,>>>,>>>,>>>.99").
                            WHEN "v-msf[2]" THEN cVarValue = IF v-t-msf[2] = 0 THEN "" ELSE STRING(v-t-msf[2],"->,>>>,>>>,>>>.99").
                            WHEN "v-amt[3]" THEN cVarValue = IF v-t-amt[3] = 0 THEN "" ELSE STRING(v-t-amt[3],"->,>>>,>>>,>>>.99").
                            WHEN "v-amt[4]" THEN cVarValue = IF v-t-amt[4] = 0 THEN "" ELSE STRING(v-t-amt[4],"->,>>>,>>>,>>>.99").
                            WHEN "v-msf[3]" THEN cVarValue = IF v-t-msf[3] = 0 THEN "" ELSE STRING(v-t-msf[3],"->,>>>,>>>,>>>.99").
                            WHEN "v-msf[4]" THEN cVarValue = IF v-t-msf[4] = 0 THEN "" ELSE STRING(v-t-msf[4],"->,>>>,>>>,>>>.99").
                            WHEN "v-amt[5]" THEN cVarValue = IF v-t-amt[5] = 0 THEN "" ELSE STRING(v-t-amt[5],"->,>>>,>>>,>>>.99").
                            WHEN "v-amt[6]" THEN cVarValue = IF v-t-amt[6] = 0 THEN "" ELSE STRING(v-t-amt[6],"->,>>>,>>>,>>>.99").
                            WHEN "v-msf[5]" THEN cVarValue = IF v-t-msf[5] = 0 THEN "" ELSE STRING(v-t-msf[5],"->,>>>,>>>,>>>.99").
                            WHEN "v-msf[6]" THEN cVarValue = IF v-t-msf[6] = 0 THEN "" ELSE STRING(v-t-msf[6],"->,>>>,>>>,>>>.99").
                                    
                            WHEN "v-diff" THEN   cVarValue = IF v-diff = 0 THEN "" ELSE STRING(v-diff,"->,>>>,>>>,>>>.99").
                            WHEN "v-diffMSF" THEN cVarValue = IF v-diffMSF = 0 THEN "" ELSE STRING(v-diffMSF,"->,>>>,>>>,>>>.99").
                            WHEN "v-diffYTD" THEN   cVarValue = IF v-diffYTD = 0 THEN "" ELSE STRING(v-diffYTD,"->,>>>,>>>,>>>.99").
                            WHEN "v-diffMTD" THEN   cVarValue = IF v-diffMTD = 0 THEN "" ELSE STRING(v-diffMTD,"->,>>>,>>>,>>>.99").
                            WHEN "v-pct" THEN    cVarValue = IF v-pct = 0 THEN "" ELSE STRING(v-pct,"->,>>>,>>>,>>>.99").
                            WHEN "v-pctYTD" THEN    cVarValue = IF v-pctYTD = 0 THEN "" ELSE STRING(v-pctYTD,"->,>>>,>>>,>>>.99").
                            WHEN "v-pctMTD" THEN    cVarValue = IF v-pctMTD = 0 THEN "" ELSE STRING(v-pctMTD,"->,>>>,>>>,>>>.99").
                            WHEN "v-pctMSF" THEN cVarValue = IF v-pctmsf = 0 THEN "" ELSE STRING(v-pctmsf,"->,>>>,>>>,>>>.99").
                            WHEN "v-pctMSFYTD" THEN cVarValue = IF v-pctmsfYTD = 0 THEN "" ELSE STRING(v-pctmsfYTD,"->,>>>,>>>,>>>.99").
                            WHEN "v-pctMSFMTD" THEN cVarValue = IF v-pctmsfMTD = 0 THEN "" ELSE STRING(v-pctmsfMTD,"->,>>>,>>>,>>>.99").
                            WHEN "v-profit" THEN cVarValue = IF v-t-amt[1] = 0 AND v-costTotal = 0  THEN "" ELSE STRING(v-t-amt[1] - v-costTotal,"->,>>>,>>>,>>>.99"). 
                            WHEN "v-cost" THEN cVarValue = IF v-costTotal = 0 THEN "" ELSE STRING(v-costTotal,"->,>>>,>>>,>>>.99"). 
                       END CASE.
           
                       cExcelVarValue = cVarValue.  
                       cDisplay = cDisplay + cVarValue +
                                  FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
                       cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",". 
                 
              END.
              
              PUT UNFORMATTED cDisplay SKIP.
              IF tb_excel THEN DO:
                    PUT STREAM excel UNFORMATTED  
                          cExcelDisplay SKIP.
              END.    /*  Task 12041302 */


/*         if tb_msf then do with frame detail-msf: */
/*           underline v-amt[1]                     */
/*                     v-msf[1]                     */
/*                     v-amt[2]                     */
/*                     v-msf[2]                     */
/*                     v-diff                       */
/*                     v-pct.                       */
/*           down.                                  */
/*           underline v-amt[1]                     */
/*                     v-msf[1]                     */
/*                     v-amt[2]                     */
/*                     v-msf[2]                     */
/*                     v-diff                       */
/*                     v-pct.                       */
/*                                                  */
/*           display "Grand Totals"                 */
/*                            @ cust.name           */
/*                   v-amt[3] @ v-amt[1]            */
/*                   v-msf[3] @ v-msf[1]            */
/*                   v-amt[4] @ v-amt[2]            */
/*                   v-msf[4] @ v-msf[2]            */
/*                   v-diff                         */
/*                   v-pct.                         */
/*           down.                                  */
/*         end.                                     */
/*                                                  */
/*         else                                     */
/*         do with frame detail:                    */
/*           underline v-amt[1]                     */
/*                     v-amt[2]                     */
/*                     v-diff                       */
/*                     v-pct.                       */
/*           down.                                  */
/*           underline v-amt[1]                     */
/*                     v-amt[2]                     */
/*                     v-diff                       */
/*                     v-pct.                       */
/*                                                  */
/*           display "Grand Totals"                 */
/*                            @ cust.name           */
/*                   v-amt[3] @ v-amt[1]            */
/*                   v-amt[4] @ v-amt[2]            */
/*                   v-diff                         */
/*                   v-pct.                         */
/*           down.                                  */
/*         end.                                     */
      END.

/*       else                                                      */
/*       IF (NOT tb_exc-zero OR                                    */
/*           v-amt[1] NE 0   OR                                    */
/*           v-amt[2] NE 0   OR                                    */
/*           (/*tb_msf AND*/                                       */
/*            (v-msf[1] NE 0 OR v-msf[2] NE 0))) THEN PUT SKIP(1). */
    END.
   
