    
FOR EACH ar-inv
    WHERE ar-inv.company  EQ cocode
    AND ar-inv.inv-date GE fdate[2]
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
    AND ar-cash.check-date GE fdate[2]
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
    AND tt-report.key-08  EQ "",
          
    FIRST cust
    WHERE cust.company EQ cocode
    AND cust.cust-no EQ tt-report.key-09
    NO-LOCK:

    IF tt-report.key-10 EQ "ar-inv" THEN 
    DO:
        FIND ar-inv WHERE RECID(ar-inv) EQ tt-report.rec-id NO-LOCK.

        FOR EACH ar-invl
            WHERE ar-invl.x-no EQ ar-inv.x-no
            AND ar-invl.i-no GE begin_i-no
            AND ar-invl.i-no LE end_i-no
            AND (ar-invl.billable OR NOT ar-invl.misc)
            USE-INDEX x-no NO-LOCK:

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
                    v-amt[1] = 0
                    v-amt[2] = 0
                    v-msf[1] = 0
                    v-msf[2] = 0
                    v-pct    = ar-invl.s-pct[i] / 100.

                IF v-pct EQ 0 THEN
                DO v = 1 TO 3:
                    IF v EQ 1 THEN j = 0.
                    IF ar-invl.sman[v] NE "" THEN j = j + 1.
                    IF v EQ 3 THEN v-pct = 1 / j.
                END.

                IF v-pct LE 0 OR v-pct EQ ? THEN v-pct = 1.
            
                DO v = 1 TO 2:
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

                CREATE xtt-report.
       
                ASSIGN
                    xtt-report.key-01 = "2" 
                    xtt-report.key-02 = tt-report.key-09
                    xtt-report.key-03 = v-sman-no
                    xtt-report.key-04 = STRING(v-amt[1],"-9999999999999.99")
                    xtt-report.key-05 = STRING(v-amt[2],"-9999999999999.99")
                    xtt-report.key-06 = STRING(v-msf[1],"-9999999999999.99")
                    xtt-report.key-07 = STRING(v-msf[2],"-9999999999999.99").
            END.
        END.  

        DELETE tt-report.
    END.

    ELSE
        IF tt-report.key-10 EQ "ar-cashl" THEN 
        DO:
            FIND ar-cashl WHERE RECID(ar-cashl) EQ tt-report.rec-id NO-LOCK.
            FIND ar-cash  WHERE ar-cash.c-no    EQ ar-cashl.c-no NO-LOCK.

            ASSIGN
                v-amt[1] = 0
                v-amt[2] = 0
                v-msf[1] = 0
                v-msf[2] = 0
                lv-i-no  = "".
         
            DO v = 1 TO 2:
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

            RELEASE ar-inv.

            RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER oe-retl).

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

            IF lv-r-no NE 0 THEN 
            DO:
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

                IF lv-type EQ "items" THEN 
                DO:
                    RELEASE ar-invl.
                    FIND FIRST oe-retl
                        WHERE oe-retl.company EQ cocode
                        AND oe-retl.r-no    EQ oe-reth.r-no
                        AND oe-retl.line    EQ ar-cashl.line
                        AND oe-retl.i-no    GE begin_i-no
                        AND oe-retl.i-no    LE end_i-no
                        NO-LOCK NO-ERROR.

                    IF AVAILABLE oe-retl THEN 
                    DO:
                        FIND FIRST itemfg
                            WHERE itemfg.company EQ cocode
                            AND itemfg.i-no    EQ oe-retl.i-no
                            NO-LOCK NO-ERROR.

                        DO v = 1 TO 2:
                            IF ar-cash.check-date GE fdate[v] AND
                                ar-cash.check-date LE tdate[v] THEN
                                v-msf[v] = IF AVAILABLE itemfg THEN
                                    (oe-retl.tot-qty-return * itemfg.t-sqft / 1000)
                                    ELSE 0.
                            IF v-msf[v] EQ ? THEN v-msf[v] = 0.
                        END.
                    END.

                    IF AVAILABLE oe-retl THEN 
                    DO:
                
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
                
                    END. /* avail oe-retl */
            
                    IF AVAILABLE ar-invl THEN 
                    DO:
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
                
                            CREATE xtt-report.

                            ASSIGN
                                xtt-report.key-01 = "2"
                                xtt-report.key-02 = tt-report.key-09
                                xtt-report.key-03 = v-sman-no
                                xtt-report.key-04 = STRING(v-amt[1] * v-pct,"-9999999999999.99")
                                xtt-report.key-05 = STRING(v-amt[2] * v-pct,"-9999999999999.99")
                                xtt-report.key-06 = STRING(v-msf[1] * v-pct,"-9999999999999.99")
                                xtt-report.key-07 = STRING(v-msf[2] * v-pct,"-9999999999999.99").
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
            v-msf = 0.

    ASSIGN
        v-amt[1] = v-amt[1] + dec(tt-report.key-04)
        v-amt[2] = v-amt[2] + dec(tt-report.key-05)
        v-msf[1] = v-msf[1] + dec(tt-report.key-06)
        v-msf[2] = v-msf[2] + dec(tt-report.key-07).

    IF LAST-OF(tt-report.key-02) THEN 
    DO:
        CREATE xtt-report.
        ASSIGN
            xtt-report.key-01 = "1"
            xtt-report.key-02 = IF v-sort EQ "C" THEN tt-report.key-02 ELSE "" 
            xtt-report.key-03 = STRING(v-amt[1],"-9999999999999.99")
            xtt-report.key-04 = STRING(v-amt[2],"-9999999999999.99")
            xtt-report.key-05 = tt-report.key-02
            xtt-report.key-06 = STRING(v-msf[1],"-9999999999999.99")
            xtt-report.key-07 = STRING(v-msf[2],"-9999999999999.99").
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

        IF FIRST-OF(tt-report.key-03) THEN
            ASSIGN
                v-amt[1] = 0
                v-amt[2] = 0
                v-msf[1] = 0
                v-msf[2] = 0.

        ASSIGN
            v-amt[1] = v-amt[1] + dec(tt-report.key-04)
            v-amt[2] = v-amt[2] + dec(tt-report.key-05)
            v-msf[1] = v-msf[1] + dec(tt-report.key-06)
            v-msf[2] = v-msf[2] + dec(tt-report.key-07).

        IF LAST-OF(tt-report.key-03)            AND
            (NOT tb_exc-zero OR
            v-amt[1] NE 0   OR
            v-amt[2] NE 0   OR
            (tb_msf AND
            (v-msf[1] NE 0 OR v-msf[2] NE 0))) THEN 
        DO:

            IF v-sman-no EQ "xxxxxx" THEN v-sman-no = tt-report.key-03.

            IF v-sman-no NE tt-report.key-03 THEN v-one = NO.

            ASSIGN
                v-diff = v-amt[1] - v-amt[2]
                v-pct  = v-amt[1] / v-amt[2] * 100.

            IF v-pct EQ ? THEN v-pct = 0.

            IF tb_msf THEN 
            DO WITH FRAME detail-msf:   
                DISPLAY cust.cust-no     
                    WHEN ll-first
                    cust.NAME        
                    WHEN ll-first
                    tt-report.key-03 @ cust.sman
                    v-amt[1]
                    v-msf[1]
                    v-amt[2]
                    v-msf[2]
                    v-diff
                    v-pct.
                DOWN.
            END.

            ELSE
            DO WITH FRAME detail:
                DISPLAY cust.cust-no     
                    WHEN ll-first
                    cust.NAME        
                    WHEN ll-first
                    tt-report.key-03 @ cust.sman
                    v-amt[1]
                    v-amt[2]
                    v-diff
                    v-pct.
                DOWN.
            END.
          
            IF tb_excel THEN  
                PUT STREAM excel UNFORMATTED
                    '"' cust.cust-no '",' 
                    '"' cust.name '",' 
                    '"' tt-report.key-03 '",'
                    '"' v-amt[1] '",'
                    '"' v-msf[1] '",'
                    '"' v-amt [2] '",' 
                    '"' v-msf[2] '",'
                    '"' v-diff '",' 
                    '"' v-pct '",'  
                    SKIP. 

            ll-first = NO.
        END.
    END.
      
    ASSIGN
        v-amt[1] = dec(xtt-report.key-03)
        v-amt[2] = dec(xtt-report.key-04)
        v-msf[1] = dec(xtt-report.key-06)
        v-msf[2] = dec(xtt-report.key-07)
        v-diff   = v-amt[1] - v-amt[2]
        v-pct    = v-amt[1] / v-amt[2] * 100.

    IF v-pct EQ ? THEN v-pct = 0.

    IF NOT v-one                            AND
        (NOT tb_exc-zero OR
        v-amt[1] NE 0   OR
        v-amt[2] NE 0   OR
        (tb_msf AND
        (v-msf[1] NE 0 OR v-msf[2] NE 0))) THEN 
    DO:

        IF tb_msf THEN 
        DO WITH FRAME detail-msf:
            UNDERLINE v-amt[1]
                v-msf[1]
                v-amt[2]
                v-msf[2]
                v-diff
                v-pct.
                  
            DISPLAY cust.cust-no
                cust.name
                v-amt[1]
                v-msf[1]
                v-amt[2]
                v-msf[2]
                v-diff
                v-pct.
            DOWN.
        END.

        ELSE
        DO WITH FRAME detail:
            UNDERLINE v-amt[1]
                v-amt[2]
                v-diff
                v-pct.
                  
            DISPLAY cust.cust-no
                cust.name
                v-amt[1]
                v-amt[2]
                v-diff
                v-pct.
            DOWN.
        END.
          
    /*         IF tb_excel THEN                 */
    /*           PUT STREAM excel UNFORMATTED   */
    /*                '"' cust.cust-no '",'     */
    /*                '"' cust.name '",'        */
    /*                '"' tt-report.key-03 '",' */
    /*                '"' v-amt[1] '",'         */
    /*                '"' v-msf[1] '",'         */
    /*                '"' v-amt [2] '",'        */
    /*                '"' v-msf[2] '",'         */
    /*                '"' v-diff '",'           */
    /*                '"' v-pct '",'            */
    /*                SKIP.                     */
    END.  
      
    DO v = 3 TO 4:
        ASSIGN
            v-amt[v] = v-amt[v] + v-amt[v - 2]
            v-msf[v] = v-msf[v] + v-msf[v - 2].
    END.
              
    /* display cust totals */
    IF LAST(xtt-report.key-02) THEN 
    DO:
        ASSIGN
            v-diff = v-amt[3] - v-amt[4]
            v-pct  = v-amt[3] / v-amt[4] * 100.

        IF v-pct EQ ? THEN v-pct = 0.

        IF tb_msf THEN 
        DO WITH FRAME detail-msf:
            UNDERLINE v-amt[1]
                v-msf[1]
                v-amt[2]
                v-msf[2]
                v-diff
                v-pct.
            DOWN.          
            UNDERLINE v-amt[1]
                v-msf[1]
                v-amt[2]
                v-msf[2]
                v-diff
                v-pct.

            DISPLAY "Grand Totals"
                @ cust.name
                v-amt[3] @ v-amt[1]
                v-msf[3] @ v-msf[1]
                v-amt[4] @ v-amt[2]
                v-msf[4] @ v-msf[2]
                v-diff
                v-pct.
            DOWN.
        END.

        ELSE
        DO WITH FRAME detail:
            UNDERLINE v-amt[1]
                v-amt[2]
                v-diff
                v-pct.
            DOWN.          
            UNDERLINE v-amt[1]
                v-amt[2]
                v-diff
                v-pct.

            DISPLAY "Grand Totals"
                @ cust.name
                v-amt[3] @ v-amt[1]
                v-amt[4] @ v-amt[2]
                v-diff
                v-pct.
            DOWN.
        END.
    END.

    ELSE
        IF (NOT tb_exc-zero OR
            v-amt[1] NE 0   OR
            v-amt[2] NE 0   OR
            (tb_msf AND
            (v-msf[1] NE 0 OR v-msf[2] NE 0))) THEN PUT SKIP(1).
END.
   
