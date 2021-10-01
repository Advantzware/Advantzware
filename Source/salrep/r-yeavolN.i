   
EMPTY TEMP-TABLE tt-report.
EMPTY TEMP-TABLE tt-custsort.

FOR EACH ttCustList 
    WHERE ttCustList.log-fld
    NO-LOCK,
    EACH cust
    WHERE cust.company EQ cocode
    AND cust.cust-no EQ ttCustList.cust-no /*fcust*/
    /*    and cust.cust-no le tcust*/
    NO-LOCK:

    STATUS DEFAULT "Processing Customer#: " + TRIM(cust.cust-no).

    {sa/sa-sls03.i "fdate[4]" "tdate[2]"}
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
    AND tt-report.key-08  EQ ""

    BREAK BY tt-report.key-09:

    STATUS DEFAULT "Sorting Customer#: " + TRIM(tt-report.key-09).

    IF tt-report.key-10 EQ "ar-inv" THEN 
    DO:
        FIND ar-inv WHERE RECID(ar-inv) EQ tt-report.rec-id NO-LOCK.

        FOR EACH ar-invl
            WHERE ar-invl.x-no    EQ ar-inv.x-no
            AND (ar-invl.billable OR NOT ar-invl.misc)
            NO-LOCK:

            v-prodc = "MISC".

            FIND FIRST itemfg
                WHERE itemfg.company EQ cocode
                AND itemfg.i-no    EQ ar-invl.i-no
                NO-LOCK NO-ERROR.
            IF AVAILABLE itemfg AND itemfg.procat NE "" THEN v-prodc = itemfg.procat.

            ELSE 
            DO:
                FIND FIRST fgcat
                    WHERE fgcat.company EQ cocode
                    AND fgcat.glacc   EQ ar-invl.actnum
                    NO-LOCK NO-ERROR.
                IF AVAILABLE fgcat THEN v-prodc = fgcat.procat.
            END.

            IF v-prodc GE begin_fg-cat AND
                v-prodc LE end_fg-cat   THEN
            DO v = 1 TO 4:
                IF ar-inv.inv-date GE fdate[v] AND
                    ar-inv.inv-date LE tdate[v] THEN
                    ASSIGN
                        v-tot[5] = v-tot[5] + (IF v EQ 1 THEN ar-invl.amt ELSE 0)
                        v-tot[6] = v-tot[6] + (IF v EQ 2 THEN ar-invl.amt ELSE 0)
                        v-tot[v] = v-tot[v] + ar-invl.amt.
            END.
        END.
    END.

    ELSE
        IF tt-report.key-10 EQ "ar-cashl" THEN 
        DO:
            FIND ar-cashl WHERE RECID(ar-cashl) EQ tt-report.rec-id NO-LOCK.
            FIND ar-cash  WHERE ar-cash.c-no    EQ ar-cashl.c-no NO-LOCK.

            v-prodc = "MEMO".

            RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER oe-retl).

            IF AVAILABLE oe-retl THEN 
            DO:
                FIND FIRST ar-invl
                    WHERE ar-invl.company EQ cocode
                    AND ar-invl.cust-no EQ ar-cash.cust-no
                    AND ar-invl.inv-no  EQ ar-cashl.inv-no
                    AND ar-invl.i-no    EQ oe-retl.i-no
                    AND (ar-invl.billable OR NOT ar-invl.misc)
                    NO-LOCK NO-ERROR.

                IF AVAILABLE ar-invl THEN 
                DO:
                    v-prodc = "MISC".

                    FIND FIRST itemfg
                        WHERE itemfg.company EQ cocode
                        AND itemfg.i-no    EQ ar-invl.i-no
                        NO-LOCK NO-ERROR.
                    IF AVAILABLE itemfg AND itemfg.procat NE "" THEN v-prodc = itemfg.procat.

                    ELSE 
                    DO:
                        FIND FIRST fgcat
                            WHERE fgcat.company EQ cocode
                            AND fgcat.glacc   EQ ar-invl.actnum
                            NO-LOCK NO-ERROR.
                        IF AVAILABLE fgcat THEN v-prodc = fgcat.procat.
                    END.
                END.
            END.

            IF v-prodc GE begin_fg-cat AND
                v-prodc LE end_fg-cat   THEN
            DO v = 1 TO 4:
                IF ar-cash.check-date GE fdate[v] AND
                    ar-cash.check-date LE tdate[v] THEN
                    ASSIGN
                        v-tot[5] = v-tot[5] + (IF v EQ 1 THEN
                                      (ar-cashl.amt-paid - ar-cashl.amt-disc)
                                    ELSE 0)
                        v-tot[6] = v-tot[6] + (IF v EQ 2 THEN
                                      (ar-cashl.amt-paid - ar-cashl.amt-disc)
                                    ELSE 0)
                        v-tot[v] = v-tot[v] + (ar-cashl.amt-paid - ar-cashl.amt-disc).
            END.
        END.

    IF LAST-OF(tt-report.key-09) THEN 
    DO:
        FIND FIRST cust
            WHERE cust.company EQ cocode
            AND cust.cust-no EQ tt-report.key-09
            NO-LOCK NO-ERROR.
        CREATE tt-custsort.
        ASSIGN
            tt-custsort.tot-01  = v-tot[5]
            tt-custsort.tot-02  = v-tot[6]
            tt-custsort.cust-no = tt-report.key-09
            v-tot[5]            = 0
            v-tot[6]            = 0.
        IF v-sort-by-name EQ "N" THEN
            tt-custsort.sorter = IF AVAILABLE cust THEN cust.name ELSE tt-custsort.cust-no.
        ELSE
            IF v-sort-by-name EQ "C" THEN
                tt-custsort.sorter = IF AVAILABLE cust THEN cust.cust-no ELSE tt-custsort.cust-no.
    END.
END.

PUT SKIP(1).

FOR EACH tt-custsort USE-INDEX sorter,

    EACH tt-report
    WHERE tt-report.term-id EQ ""
    AND tt-report.key-01  EQ ""
    AND tt-report.key-02  EQ ""
    AND tt-report.key-03  EQ ""
    AND tt-report.key-04  EQ ""
    AND tt-report.key-05  EQ ""
    AND tt-report.key-06  EQ ""
    AND tt-report.key-07  EQ ""
    AND tt-report.key-08  EQ ""
    AND tt-report.key-09  EQ tt-custsort.cust-no

    BREAK BY tt-custsort.sorter
    BY tt-custsort.tot-01 DESCENDING
    BY tt-custsort.tot-02 DESCENDING
    BY tt-custsort.cust-no

    WITH FRAME custx:

    IF tt-report.key-10 EQ "ar-inv" THEN 
    DO:
        FIND ar-inv WHERE RECID(ar-inv) EQ tt-report.rec-id NO-LOCK.

        STATUS DEFAULT "Printing Customer#/Inv#: " +
            TRIM(ar-inv.cust-no) + "/"  +
            TRIM(STRING(ar-inv.inv-no,">>>>>>>>>>")).

        FOR EACH ar-invl
            WHERE ar-invl.x-no    EQ ar-inv.x-no
            NO-LOCK:

            v-prodc = "MISC".

            FIND FIRST itemfg
                WHERE itemfg.company EQ cocode
                AND itemfg.i-no    EQ ar-invl.i-no
                NO-LOCK NO-ERROR.
            IF AVAILABLE itemfg AND itemfg.procat NE "" THEN v-prodc = itemfg.procat.

            ELSE 
            DO:
                FIND FIRST fgcat
                    WHERE fgcat.company EQ cocode
                    AND fgcat.glacc   EQ ar-invl.actnum
                    NO-LOCK NO-ERROR.
                IF AVAILABLE fgcat THEN v-prodc = fgcat.procat.
            END.

            RUN salrep/salecost.p (LOOKUP(rd_show1,"Board,Order,Invoice"),
                ROWID(ar-invl),
                ar-invl.job-no,
                ar-invl.job-no2,
                ar-invl.ship-qty,
                OUTPUT ld-cost).

            IF v-prodc GE begin_fg-cat AND
                v-prodc LE end_fg-cat   THEN
            DO v = 1 TO 4:
                IF ar-inv.inv-date GE fdate[v] AND
                    ar-inv.inv-date LE tdate[v] THEN
                    ASSIGN
                        v-msf[v] = v-msf[v] +
                          (IF ar-invl.amt-msf NE 0 THEN ar-invl.amt-msf
                           ELSE
                           IF AVAILABLE itemfg THEN
                             (ar-invl.ship-qty * itemfg.t-sqft / 1000)
                           ELSE 0)
                        v-cst[v] = v-cst[v] + ld-cost
                        v-amt[v] = v-amt[v] + ar-invl.amt
                        v-ton[v] = v-ton[v] +
                          ((IF ar-invl.t-weight NE 0 THEN ar-invl.t-weight
                            ELSE
                            IF AVAILABLE itemfg THEN
                              (ar-invl.ship-qty * itemfg.weight-100 / 100)
                            ELSE 0) / 2000).
            END.
        END.
    END.

    ELSE
        IF tt-report.key-10 EQ "ar-cashl" THEN 
        DO:
            FIND ar-cashl WHERE RECID(ar-cashl) EQ tt-report.rec-id NO-LOCK.
            FIND ar-cash  WHERE ar-cash.c-no    EQ ar-cashl.c-no NO-LOCK.

            STATUS DEFAULT "Printing Customer#/Check#: " +
                TRIM(ar-cash.cust-no) + "/"  +
                TRIM(STRING(ar-cash.check-no,">>>>>>>>>>>>")).

            v-prodc = "MEMO".

            RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER oe-retl).

            IF AVAILABLE oe-retl THEN 
            DO:
                FIND FIRST ar-invl
                    WHERE ar-invl.company EQ cocode
                    AND ar-invl.cust-no EQ ar-cash.cust-no
                    AND ar-invl.inv-no  EQ ar-cashl.inv-no
                    AND ar-invl.i-no    EQ oe-retl.i-no
                    AND (ar-invl.billable OR NOT ar-invl.misc)
                    NO-LOCK NO-ERROR.

                IF AVAILABLE ar-invl THEN 
                DO:
                    v-prodc = "MISC".

                    FIND FIRST itemfg
                        WHERE itemfg.company EQ cocode
                        AND itemfg.i-no    EQ ar-invl.i-no
                        NO-LOCK NO-ERROR.
                    IF AVAILABLE itemfg AND itemfg.procat NE "" THEN v-prodc = itemfg.procat.

                    ELSE 
                    DO:
                        FIND FIRST fgcat
                            WHERE fgcat.company EQ cocode
                            AND fgcat.glacc   EQ ar-invl.actnum
                            NO-LOCK NO-ERROR.
                        IF AVAILABLE fgcat THEN v-prodc = fgcat.procat.
                    END.

                    RUN salrep/salecost.p (LOOKUP(rd_show1,"Board,Order,Invoice"),
                        ROWID(ar-invl),
                        oe-retl.job-no,
                        oe-retl.job-no2,
                        oe-retl.tot-qty-return,
                        OUTPUT ld-cost).

                    IF v-prodc GE begin_fg-cat AND
                        v-prodc LE end_fg-cat   THEN
                    DO v = 1 TO 4:
                        IF ar-cash.check-date GE fdate[v] AND
                            ar-cash.check-date LE tdate[v] THEN
                            ASSIGN
                                v-msf[v] = v-msf[v] -
                            (IF AVAILABLE itemfg THEN
                               (oe-retl.tot-qty-return * itemfg.t-sqft / 1000)
                             ELSE 0)
                                v-cst[v] = v-cst[v] - ld-cost
                                v-ton[v] = v-ton[v] +
                            ((IF AVAILABLE itemfg THEN
                                (oe-retl.tot-qty-return * itemfg.weight-100 / 100)
                              ELSE 0) / 2000).
                    END.
                END.
            END.

            IF v-prodc GE begin_fg-cat AND
                v-prodc LE end_fg-cat   THEN
            DO v = 1 TO 4:
                IF ar-cash.check-date GE fdate[v] AND
                    ar-cash.check-date LE tdate[v] THEN
                    v-amt[v] = v-amt[v] + (ar-cashl.amt-paid - ar-cashl.amt-disc).
            END.
        END.

    IF LAST-OF(tt-custsort.cust-no) THEN 
    DO:
        FIND FIRST cust
            WHERE cust.company EQ cocode
            AND cust.cust-no EQ tt-report.key-09
            NO-LOCK.

        IF v-msf[1] NE 0 OR v-cst[1] NE 0 OR v-amt[1] NE 0 OR
            v-msf[2] NE 0 OR v-cst[2] NE 0 OR v-amt[2] NE 0 OR
            v-msf[3] NE 0 OR v-cst[3] NE 0 OR v-amt[3] NE 0 OR
            v-msf[4] NE 0 OR v-cst[4] NE 0 OR v-amt[4] NE 0 THEN 
        DO:

            IF LINE-COUNTER + 5 GT PAGE-SIZE THEN PAGE.

            ASSIGN
                v-a-m = v-amt[1] / v-msf[1]
                v-pc1 = v-amt[1] / v-tot[1] * 100
                v-ret = v-amt[1] - v-cst[1]
                v-pc2 = v-ret    / v-amt[1] * 100
                v-a-t = v-amt[1] / v-ton[1].

            IF v-a-m EQ ? THEN v-a-m = 0.
            IF v-pc1 EQ ? THEN v-pc1 = 0.
            IF v-ret EQ ? THEN v-ret = 0.
            IF v-pc2 EQ ? THEN v-pc2 = 0.
            IF v-a-t EQ ? THEN v-a-t = 0.

            /* display cust.cust-no
                     cust.name
                     cust.sman
                     "  PTD:" @ v-lab
                     v-a-m
                     v-msf[1]
                     v-cst[1]
                     v-amt[1]
                     v-pc1
                     v-ret
                     v-pc2
                     v-a-t     WHEN tb_ton
                     v-ton[1]  WHEN tb_ton.
             down.*/

            ASSIGN 
                cDisplay       = ""
                cTmpField      = ""
                cVarValue      = ""
                cExcelDisplay  = ""
                cExcelVarValue = "".
          
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                CASE cTmpField:             
                    WHEN "cust"    THEN 
                        cVarValue = STRING(cust.cust-no,"x(8)") .
                    WHEN "name-city"   THEN 
                        cVarValue =  STRING(cust.NAME,"x(30)").
                    WHEN "rep"   THEN 
                        cVarValue = STRING(cust.sman,"x(3)").
                    WHEN "comp"  THEN 
                        cVarValue = STRING("  PTD:","x(6)") .
                    WHEN "$msf"   THEN 
                        cVarValue = STRING(v-a-m,"->>>9.99") .
                    WHEN "msf"  THEN 
                        cVarValue = STRING(v-msf[1],"->>>>9.9") .
                    WHEN "cost-sale"   THEN 
                        cVarValue = STRING(v-cst[1],"->>,>>>,>>9.99") .
                    WHEN "sale"  THEN 
                        cVarValue = STRING(v-amt[1],"->>,>>>,>>9.99") .
                    WHEN "%sale"    THEN 
                        cVarValue = STRING(v-pc1,"->>>9.9") .
                    WHEN "return"   THEN 
                        cVarValue = STRING(v-ret,"->>,>>>,>>9.99").
                    WHEN "%ofret"   THEN 
                        cVarValue = STRING(v-pc2,"->>>9.9").
                    WHEN "$ton"  THEN 
                        cVarValue = STRING(v-a-t,"->>>>>>9.99") .
                    WHEN "ton"   THEN 
                        cVarValue = STRING(v-ton[1],"->>>9.9") .
                        
                END CASE.
                      
                cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED cDisplay SKIP.
            IF rd-dest = 3 THEN 
            DO:
                PUT STREAM excel UNFORMATTED  
                    cExcelDisplay SKIP.
            END.

            IF rd-dest = 3 THEN 
            DO:

                ASSIGN
                    ptd-v-a-m = v-a-m
                    ptd-v-pc1 = v-pc1
                    ptd-v-ret = v-ret
                    ptd-v-pc2 = v-pc2.

                /*IF tb_ton THEN*/
                ptd-v-a-t = v-a-t.
            END.

            ASSIGN
                v-a-m = v-amt[2] / v-msf[2]
                v-pc1 = v-amt[2] / v-tot[2] * 100
                v-ret = v-amt[2] - v-cst[2]
                v-pc2 = v-ret    / v-amt[2] * 100
                v-a-t = v-amt[2] / v-ton[2].

            IF v-a-m EQ ? THEN v-a-m = 0.
            IF v-pc1 EQ ? THEN v-pc1 = 0.
            IF v-ret EQ ? THEN v-ret = 0.
            IF v-pc2 EQ ? THEN v-pc2 = 0.
            IF v-a-t EQ ? THEN v-a-t = 0.

            /*display string(cust.city + ", " + cust.state,"x(30)")
                             @ cust.name
                    "  YTD:" @ v-lab
                    v-a-m
                    v-msf[2] @ v-msf[1]
                    v-cst[2] @ v-cst[1]
                    v-amt[2] @ v-amt[1]
                    v-pc1
                    v-ret
                    v-pc2
                    v-a-t     WHEN tb_ton
                    v-ton[2]  WHEN tb_ton @ v-ton[1].
            down.*/

            ASSIGN 
                cDisplay       = ""
                cTmpField      = ""
                cVarValue      = ""
                cExcelDisplay  = ""
                cExcelVarValue = "".
          
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                CASE cTmpField:             
                    WHEN "cust"    THEN 
                        cVarValue = "" .
                    WHEN "name-city"   THEN 
                        cVarValue =  STRING(cust.city + ", " + cust.state,"x(30)").
                    WHEN "rep"   THEN 
                        cVarValue = "".
                    WHEN "comp"  THEN 
                        cVarValue = STRING("  YTD:","x(6)") .
                    WHEN "$msf"   THEN 
                        cVarValue = STRING(v-a-m,"->>>9.99") .
                    WHEN "msf"  THEN 
                        cVarValue = STRING(v-msf[2],"->>>>9.9") .
                    WHEN "cost-sale"   THEN 
                        cVarValue = STRING(v-cst[2],"->>,>>>,>>9.99") .
                    WHEN "sale"  THEN 
                        cVarValue = STRING(v-amt[2],"->>,>>>,>>9.99") .
                    WHEN "%sale"    THEN 
                        cVarValue = STRING(v-pc1,"->>>9.9") .
                    WHEN "return"   THEN 
                        cVarValue = STRING(v-ret,"->>,>>>,>>9.99").
                    WHEN "%ofret"   THEN 
                        cVarValue = STRING(v-pc2,"->>>9.9").
                    WHEN "$ton"  THEN 
                        cVarValue = STRING(v-a-t,"->>>>>>9.99") .
                    WHEN "ton"   THEN 
                        cVarValue = STRING(v-ton[2],"->>>9.9") .
                        
                END CASE.
                      
                cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED cDisplay SKIP.
            IF rd-dest = 3 THEN 
            DO:
                PUT STREAM excel UNFORMATTED  
                    cExcelDisplay SKIP.
            END.

            IF rd-dest = 3 THEN 
            DO:

                ASSIGN
                    ytd-v-a-m = v-a-m
                    ytd-v-pc1 = v-pc1
                    ytd-v-ret = v-ret
                    ytd-v-pc2 = v-pc2.

                /*IF tb_ton THEN*/
                ytd-v-a-t = v-a-t.
            END.

            ASSIGN
                v-a-m = v-amt[3] / v-msf[3]
                v-pc1 = v-amt[3] / v-tot[3] * 100
                v-ret = v-amt[3] - v-cst[3]
                v-pc2 = v-ret    / v-amt[3] * 100
                v-dif = v-amt[1] - v-amt[3]
                v-a-t = v-amt[3] / v-ton[3].

            IF v-a-m EQ ? THEN v-a-m = 0.
            IF v-pc1 EQ ? THEN v-pc1 = 0.
            IF v-ret EQ ? THEN v-ret = 0.
            IF v-pc2 EQ ? THEN v-pc2 = 0.
            IF v-dif EQ ? THEN v-dif = 0.
            IF v-a-t EQ ? THEN v-a-t = 0.

            /*display string("PTD Sales Diff: " +
                    string(v-dif,">>>>>>>>9.99-"),"x(30)")
                           @ cust.name
                    "PTDLY:" @ v-lab
                    v-a-m
                    v-msf[3] @ v-msf[1]
                    v-cst[3] @ v-cst[1]
                    v-amt[3] @ v-amt[1]
                    v-pc1
                    v-ret
                    v-pc2
                    v-a-t     WHEN tb_ton
                    v-ton[3]  WHEN tb_ton @ v-ton[1].
            down.*/

            ASSIGN 
                cDisplay       = ""
                cTmpField      = ""
                cVarValue      = ""
                cExcelDisplay  = ""
                cExcelVarValue = "".
          
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                CASE cTmpField:             
                    WHEN "cust"    THEN 
                        cVarValue = "" .
                    WHEN "name-city"   THEN 
                        cVarValue = STRING("PTD Sales Diff: " + string(v-dif,">>>>>>>>9.99-"),"x(30)") .
                    WHEN "rep"   THEN 
                        cVarValue = "".
                    WHEN "comp"  THEN 
                        cVarValue = STRING("PTDLY:","x(6)") .
                    WHEN "$msf"   THEN 
                        cVarValue = STRING(v-a-m,"->>>9.99") .
                    WHEN "msf"  THEN 
                        cVarValue = STRING(v-msf[3],"->>>>9.9") .
                    WHEN "cost-sale"   THEN 
                        cVarValue = STRING(v-cst[3],"->>,>>>,>>9.99") .
                    WHEN "sale"  THEN 
                        cVarValue = STRING(v-amt[3],"->>,>>>,>>9.99") .
                    WHEN "%sale"    THEN 
                        cVarValue = STRING(v-pc1,"->>>9.9") .
                    WHEN "return"   THEN 
                        cVarValue = STRING(v-ret,"->>,>>>,>>9.99").
                    WHEN "%ofret"   THEN 
                        cVarValue = STRING(v-pc2,"->>>9.9").
                    WHEN "$ton"  THEN 
                        cVarValue = STRING(v-a-t,"->>>>>>9.99") .
                    WHEN "ton"   THEN 
                        cVarValue = STRING(v-ton[3],"->>>9.9") .
                        
                END CASE.
                      
                cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED cDisplay SKIP.
            IF rd-dest = 3 THEN 
            DO:
                PUT STREAM excel UNFORMATTED  
                    cExcelDisplay SKIP.
            END.

            IF rd-dest = 3 THEN 
            DO:

                ASSIGN
                    ptdly-v-a-m = v-a-m
                    ptdly-v-pc1 = v-pc1
                    ptdly-v-ret = v-ret
                    ptdly-v-pc2 = v-pc2
                    ptd-v-dif   = v-dif.

                /*IF tb_ton THEN*/
                ptdly-v-a-t = v-a-t.
            END.

            ASSIGN
                v-a-m = v-amt[4] / v-msf[4]
                v-pc1 = v-amt[4] / v-tot[4] * 100
                v-ret = v-amt[4] - v-cst[4]
                v-pc2 = v-ret    / v-amt[4] * 100
                v-dif = v-amt[2] - v-amt[4]
                v-a-t = v-amt[4] / v-ton[4].

            IF v-a-m EQ ? THEN v-a-m = 0.
            IF v-pc1 EQ ? THEN v-pc1 = 0.
            IF v-ret EQ ? THEN v-ret = 0.
            IF v-pc2 EQ ? THEN v-pc2 = 0.
            IF v-dif EQ ? THEN v-dif = 0.
            IF v-a-t EQ ? THEN v-a-t = 0.

            /*display string("YTD Sales Diff: " +
                    string(v-dif,">>>>>>>>9.99-"),"x(30)")
                           @ cust.name
                    "YTDLY:" @ v-lab
                    v-a-m
                    v-msf[4] @ v-msf[1]
                    v-cst[4] @ v-cst[1]
                    v-amt[4] @ v-amt[1]
                    v-pc1
                    v-ret
                    v-pc2
                    v-a-t     WHEN tb_ton
                    v-ton[4]  WHEN tb_ton @ v-ton[1].
            down.*/
            ASSIGN 
                cDisplay       = ""
                cTmpField      = ""
                cVarValue      = ""
                cExcelDisplay  = ""
                cExcelVarValue = "".
          
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                CASE cTmpField:             
                    WHEN "cust"    THEN 
                        cVarValue = "" .
                    WHEN "name-city"   THEN 
                        cVarValue = STRING("YTD Sales Diff: " + string(v-dif,">>>>>>>>9.99-"),"x(30)") .
                    WHEN "rep"   THEN 
                        cVarValue = "".
                    WHEN "comp"  THEN 
                        cVarValue = STRING("YTDLY:","x(6)") .
                    WHEN "$msf"   THEN 
                        cVarValue = STRING(v-a-m,"->>>9.99") .
                    WHEN "msf"  THEN 
                        cVarValue = STRING(v-msf[4],"->>>>9.9") .
                    WHEN "cost-sale"   THEN 
                        cVarValue = STRING(v-cst[4],"->>,>>>,>>9.99") .
                    WHEN "sale"  THEN 
                        cVarValue = STRING(v-amt[4],"->>,>>>,>>9.99") .
                    WHEN "%sale"    THEN 
                        cVarValue = STRING(v-pc1,"->>>9.9") .
                    WHEN "return"   THEN 
                        cVarValue = STRING(v-ret,"->>,>>>,>>9.99").
                    WHEN "%ofret"   THEN 
                        cVarValue = STRING(v-pc2,"->>>9.9").
                    WHEN "$ton"  THEN 
                        cVarValue = STRING(v-a-t,"->>>>>>9.99") .
                    WHEN "ton"   THEN 
                        cVarValue = STRING(v-ton[4],"->>>9.9") .
                        
                END CASE.
                      
                cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED cDisplay SKIP.
            IF rd-dest = 3 THEN 
            DO:
                PUT STREAM excel UNFORMATTED  
                    cExcelDisplay SKIP(1).
            END.
            PUT SKIP(1).
        END.

        DO v = 5 TO 8:
            ASSIGN
                v-msf[v]     = v-msf[v] + v-msf[v - 4]
                v-cst[v]     = v-cst[v] + v-cst[v - 4]
                v-amt[v]     = v-amt[v] + v-amt[v - 4]
                v-ton[v]     = v-ton[v] + v-ton[v - 4]

                v-msf[v - 4] = 0
                v-cst[v - 4] = 0
                v-amt[v - 4] = 0
                v-ton[v - 4] = 0.
        END.
    END.

    /* display final totals */
    IF LAST(tt-custsort.sorter) THEN 
    DO:
        /*underline cust.cust-no
                  cust.name
                  cust.sman
                  v-lab
                  v-a-m
                  v-msf[1]
                  v-cst[1]
                  v-amt[1]
                  v-pc1
                  v-ret
                  v-pc2
                  v-a-t     WHEN tb_ton
                  v-ton[1]  WHEN tb_ton.*/
      
        PUT str-tit5 SKIP.
        PUT SKIP(1).

        IF LINE-COUNTER + 5 GT PAGE-SIZE THEN PAGE.

        ASSIGN
            v-a-m = v-amt[5] / v-msf[5]
            v-pc1 = v-amt[5] / v-tot[1] * 100
            v-ret = v-amt[5] - v-cst[5]
            v-pc2 = v-ret    / v-amt[5] * 100
            v-a-t = v-amt[5] / v-ton[5].

        IF v-a-m EQ ? THEN v-a-m = 0.
        IF v-pc1 EQ ? THEN v-pc1 = 0.
        IF v-ret EQ ? THEN v-ret = 0.
        IF v-pc2 EQ ? THEN v-pc2 = 0.
        IF v-a-t EQ ? THEN v-a-t = 0.

        IF rd-dest = 3 THEN 
        DO:

            ASSIGN
                ptd-v-a-m = v-a-m
                ptd-v-pc1 = v-pc1
                ptd-v-ret = v-ret
                ptd-v-pc2 = v-pc2.

            /*IF tb_ton THEN*/
            ptd-v-a-t = v-a-t.
        END.

        /*display "Grand Totals"
                         @ cust.name
                "  PTD:" @ v-lab
                v-a-m
                v-msf[5] @ v-msf[1]
                v-cst[5] @ v-cst[1]
                v-amt[5] @ v-amt[1]
                v-pc1
                v-ret
                v-pc2
                v-a-t     WHEN tb_ton
                v-ton[5]  WHEN tb_ton @ v-ton[1].
        down.*/

        ASSIGN 
            cDisplay       = ""
            cTmpField      = ""
            cVarValue      = ""
            cExcelDisplay  = ""
            cExcelVarValue = "".
          
        DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
            cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
            CASE cTmpField:             
                WHEN "cust"    THEN 
                    cVarValue = "" .
                WHEN "name-city"   THEN 
                    cVarValue = "".
                WHEN "rep"   THEN 
                    cVarValue = "".
                WHEN "comp"  THEN 
                    cVarValue = STRING("  PTD:","x(6)") .
                WHEN "$msf"   THEN 
                    cVarValue = STRING(v-a-m,"->>>9.99") .
                WHEN "msf"  THEN 
                    cVarValue = STRING(v-msf[5],"->>>>9.9") .
                WHEN "cost-sale"   THEN 
                    cVarValue = STRING(v-cst[5],"->>,>>>,>>9.99") .
                WHEN "sale"  THEN 
                    cVarValue = STRING(v-amt[5],"->>,>>>,>>9.99") .
                WHEN "%sale"    THEN 
                    cVarValue = STRING(v-pc1,"->>>9.9") .
                WHEN "return"   THEN 
                    cVarValue = STRING(v-ret,"->>,>>>,>>9.99").
                WHEN "%ofret"   THEN 
                    cVarValue = STRING(v-pc2,"->>>9.9").
                WHEN "$ton"  THEN 
                    cVarValue = STRING(v-a-t,"->>>>>>9.99") .
                WHEN "ton"   THEN 
                    cVarValue = STRING(v-ton[5],"->>>9.9") .
                        
            END CASE.
                      
            cExcelVarValue = cVarValue.
            cDisplay = cDisplay + cVarValue +
                FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
        END.
          
        PUT UNFORMATTED  
            "        Grand Totals" SUBSTRING(cDisplay,21,350) SKIP.
        IF rd-dest = 3 THEN 
        DO:
            PUT STREAM excel UNFORMATTED 
                ' Grand Totals ,' 
                SUBSTRING(cExcelDisplay,4,350) SKIP.
        END.

        ASSIGN
            v-a-m = v-amt[6] / v-msf[6]
            v-pc1 = v-amt[6] / v-tot[2] * 100
            v-ret = v-amt[6] - v-cst[6]
            v-pc2 = v-ret    / v-amt[6] * 100
            v-a-t = v-amt[6] / v-ton[6].

        IF v-a-m EQ ? THEN v-a-m = 0.
        IF v-pc1 EQ ? THEN v-pc1 = 0.
        IF v-ret EQ ? THEN v-ret = 0.
        IF v-pc2 EQ ? THEN v-pc2 = 0.
        IF v-a-t EQ ? THEN v-a-t = 0.

        IF rd-dest = 3 THEN 
        DO:

            ASSIGN
                ytd-v-a-m = v-a-m
                ytd-v-pc1 = v-pc1
                ytd-v-ret = v-ret
                ytd-v-pc2 = v-pc2.

            /*IF tb_ton THEN*/
            ytd-v-a-t = v-a-t.
        END.

        /* display "  YTD:" @ v-lab
                 v-a-m
                 v-msf[6] @ v-msf[1]
                 v-cst[6] @ v-cst[1]
                 v-amt[6] @ v-amt[1]
                 v-pc1
                 v-ret
                 v-pc2
                 v-a-t     WHEN tb_ton
                 v-ton[6]  WHEN tb_ton @ v-ton[1].
         down.*/

        ASSIGN 
            cDisplay       = ""
            cTmpField      = ""
            cVarValue      = ""
            cExcelDisplay  = ""
            cExcelVarValue = "".
          
        DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
            cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
            CASE cTmpField:             
                WHEN "cust"    THEN 
                    cVarValue = "" .
                WHEN "name-city"   THEN 
                    cVarValue = "".
                WHEN "rep"   THEN 
                    cVarValue = "".
                WHEN "comp"  THEN 
                    cVarValue = STRING("  YTD:","x(6)") .
                WHEN "$msf"   THEN 
                    cVarValue = STRING(v-a-m,"->>>9.99") .
                WHEN "msf"  THEN 
                    cVarValue = STRING(v-msf[6],"->>>>9.9") .
                WHEN "cost-sale"   THEN 
                    cVarValue = STRING(v-cst[6],"->>,>>>,>>9.99") .
                WHEN "sale"  THEN 
                    cVarValue = STRING(v-amt[6],"->>,>>>,>>9.99") .
                WHEN "%sale"    THEN 
                    cVarValue = STRING(v-pc1,"->>>9.9") .
                WHEN "return"   THEN 
                    cVarValue = STRING(v-ret,"->>,>>>,>>9.99").
                WHEN "%ofret"   THEN 
                    cVarValue = STRING(v-pc2,"->>>9.9").
                WHEN "$ton"  THEN 
                    cVarValue = STRING(v-a-t,"->>>>>>9.99") .
                WHEN "ton"   THEN 
                    cVarValue = STRING(v-ton[6],"->>>9.9") .
                        
            END CASE.
                      
            cExcelVarValue = cVarValue.
            cDisplay = cDisplay + cVarValue +
                FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
        END.
          
        PUT UNFORMATTED cDisplay SKIP.
        IF rd-dest = 3 THEN 
        DO:
            PUT STREAM excel UNFORMATTED 
                cExcelDisplay SKIP.
        END.
        
        ASSIGN
            v-a-m = v-amt[7] / v-msf[7]
            v-pc1 = v-amt[7] / v-tot[3] * 100
            v-ret = v-amt[7] - v-cst[7]
            v-pc2 = v-ret    / v-amt[7] * 100
            v-dif = v-amt[5] - v-amt[7]
            v-a-t = v-amt[7] / v-ton[7].

        IF v-a-m EQ ? THEN v-a-m = 0.
        IF v-pc1 EQ ? THEN v-pc1 = 0.
        IF v-ret EQ ? THEN v-ret = 0.
        IF v-pc2 EQ ? THEN v-pc2 = 0.
        IF v-dif EQ ? THEN v-dif = 0.
        IF v-a-t EQ ? THEN v-a-t = 0.

        IF rd-dest = 3 THEN 
        DO:

            ASSIGN
                ptdly-v-a-m = v-a-m
                ptdly-v-pc1 = v-pc1
                ptdly-v-ret = v-ret
                ptdly-v-pc2 = v-pc2
                ptd-v-dif   = v-dif.

            /*IF tb_ton THEN*/
            ptdly-v-a-t = v-a-t.
        END.

        /*display string("PTD Sales Diff: " +
                  string(v-dif,">>>>>>>>9.99-"),"x(30)")
                         @ cust.name
                "PTDLY:" @ v-lab
                v-a-m
                v-msf[7] @ v-msf[1]
                v-cst[7] @ v-cst[1]
                v-amt[7] @ v-amt[1]
                v-pc1
                v-ret
                v-pc2
                v-a-t     WHEN tb_ton
                v-ton[7]  WHEN tb_ton @ v-ton[1].
        down.*/

        ASSIGN 
            cDisplay       = ""
            cTmpField      = ""
            cVarValue      = ""
            cExcelDisplay  = ""
            cExcelVarValue = "".
          
        DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
            cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
            CASE cTmpField:             
                WHEN "cust"    THEN 
                    cVarValue = "" .
                WHEN "name-city"   THEN 
                    cVarValue = STRING("PTD Sales Diff: " + string(v-dif,">>>>>>>>9.99-"),"x(30)") .
                WHEN "rep"   THEN 
                    cVarValue = "".
                WHEN "comp"  THEN 
                    cVarValue = STRING("PTDLY:","x(6)") .
                WHEN "$msf"   THEN 
                    cVarValue = STRING(v-a-m,"->>>9.99") .
                WHEN "msf"  THEN 
                    cVarValue = STRING(v-msf[7],"->>>>9.9") .
                WHEN "cost-sale"   THEN 
                    cVarValue = STRING(v-cst[7],"->>,>>>,>>9.99") .
                WHEN "sale"  THEN 
                    cVarValue = STRING(v-amt[7],"->>,>>>,>>9.99") .
                WHEN "%sale"    THEN 
                    cVarValue = STRING(v-pc1,"->>>9.9") .
                WHEN "return"   THEN 
                    cVarValue = STRING(v-ret,"->>,>>>,>>9.99").
                WHEN "%ofret"   THEN 
                    cVarValue = STRING(v-pc2,"->>>9.9").
                WHEN "$ton"  THEN 
                    cVarValue = STRING(v-a-t,"->>>>>>9.99") .
                WHEN "ton"   THEN 
                    cVarValue = STRING(v-ton[7],"->>>9.9") .
                        
            END CASE.
                      
            cExcelVarValue = cVarValue.
            cDisplay = cDisplay + cVarValue +
                FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
        END.
          
        PUT UNFORMATTED cDisplay SKIP.
        IF rd-dest = 3 THEN 
        DO:
            PUT STREAM excel UNFORMATTED 
                cExcelDisplay SKIP.
        END.

        ASSIGN
            v-a-m = v-amt[8] / v-msf[8]
            v-pc1 = v-amt[8] / v-tot[4] * 100
            v-ret = v-amt[8] - v-cst[8]
            v-pc2 = v-ret    / v-amt[8] * 100
            v-dif = v-amt[6] - v-amt[8]
            v-a-t = v-amt[8] / v-ton[8].

        IF v-a-m EQ ? THEN v-a-m = 0.
        IF v-pc1 EQ ? THEN v-pc1 = 0.
        IF v-ret EQ ? THEN v-ret = 0.
        IF v-pc2 EQ ? THEN v-pc2 = 0.
        IF v-dif EQ ? THEN v-dif = 0.
        IF v-a-m EQ ? THEN v-a-m = 0.

        /* display string("YTD Sales Diff: " +
                 string(v-dif,">>>>>>>>9.99-"),"x(30)")
                          @ cust.name
                 "YTDLY:" @ v-lab
                 v-a-m
                 v-msf[8] @ v-msf[1]
                 v-cst[8] @ v-cst[1]
                 v-amt[8] @ v-amt[1]
                 v-pc1
                 v-ret
                 v-pc2
                 v-a-t     WHEN tb_ton
                 v-ton[8]  WHEN tb_ton @ v-ton[1].
         down.*/

        ASSIGN 
            cDisplay       = ""
            cTmpField      = ""
            cVarValue      = ""
            cExcelDisplay  = ""
            cExcelVarValue = "".
          
        DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
            cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
            CASE cTmpField:             
                WHEN "cust"    THEN 
                    cVarValue = "" .
                WHEN "name-city"   THEN 
                    cVarValue = STRING("YTD Sales Diff: " + string(v-dif,">>>>>>>>9.99-"),"x(30)") .
                WHEN "rep"   THEN 
                    cVarValue = "".
                WHEN "comp"  THEN 
                    cVarValue = STRING("YTDLY:","x(6)") .
                WHEN "$msf"   THEN 
                    cVarValue = STRING(v-a-m,"->>>9.99") .
                WHEN "msf"  THEN 
                    cVarValue = STRING(v-msf[8],"->>>>9.9") .
                WHEN "cost-sale"   THEN 
                    cVarValue = STRING(v-cst[8],"->>,>>>,>>9.99") .
                WHEN "sale"  THEN 
                    cVarValue = STRING(v-amt[8],"->>,>>>,>>9.99") .
                WHEN "%sale"    THEN 
                    cVarValue = STRING(v-pc1,"->>>9.9") .
                WHEN "return"   THEN 
                    cVarValue = STRING(v-ret,"->>,>>>,>>9.99").
                WHEN "%ofret"   THEN 
                    cVarValue = STRING(v-pc2,"->>>9.9").
                WHEN "$ton"  THEN 
                    cVarValue = STRING(v-a-t,"->>>>>>9.99") .
                WHEN "ton"   THEN 
                    cVarValue = STRING(v-ton[8],"->>>9.9") .
                        
            END CASE.
                      
            cExcelVarValue = cVarValue.
            cDisplay = cDisplay + cVarValue +
                FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
        END.
          
        PUT UNFORMATTED cDisplay SKIP.
        IF rd-dest = 3 THEN 
        DO:
            PUT STREAM excel UNFORMATTED 
                cExcelDisplay SKIP.
        END.
    END.
END.

STATUS DEFAULT.
