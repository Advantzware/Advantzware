
      FOR EACH ttCustList 
          WHERE ttCustList.log-fld
          NO-LOCK,
          EACH ar-inv
          WHERE ar-inv.company  EQ cocode
            AND ar-inv.inv-date GE fdate[1]
            AND ar-inv.inv-date LE as-of-date
            AND ar-inv.posted   EQ YES
          USE-INDEX inv-date NO-LOCK,
          
          FIRST cust
          WHERE cust.company EQ ar-inv.company
            AND cust.cust-no EQ ttCustList.cust-no /*ar-inv.cust-no*/
          NO-LOCK:
        
          {custom/statusMsg.i "'Processing Customer # ' + string(cust.cust-no)"}

        FOR EACH ar-invl
            WHERE ar-invl.x-no EQ ar-inv.x-no
              AND (ar-invl.billable OR NOT ar-invl.misc)
            NO-LOCK:
            
          FIND FIRST itemfg
              WHERE itemfg.company EQ ar-inv.company
                AND itemfg.i-no    EQ ar-invl.i-no
                AND itemfg.procat  GE begin_fg-cat
                AND itemfg.procat  LE end_fg-cat
              NO-LOCK NO-ERROR.

          IF AVAIL itemfg OR ("" GE begin_fg-cat AND "" LE end_fg-cat) THEN
          DO i = 1 TO 3:
            ASSIGN
             v-amt     = 0
             v-slsm[1] = IF ar-invl.sman[i] EQ "" AND i EQ 1 THEN
                           cust.sman ELSE ar-invl.sman[i].

            IF v-slsm[1]   LT fsman                         OR
               v-slsm[1]   GT tsman                         OR
               (i NE 1 AND
                (v-slsm[1] EQ "" OR ar-invl.s-pct[i] EQ 0)) THEN NEXT.

            ASSIGN
             v-slsp[1] = IF ar-invl.sman[i] EQ ""              OR
                            (ar-invl.s-pct[i] EQ 0 AND i EQ 1) THEN 100
                         ELSE ar-invl.s-pct[i]
             v-amt1    = ar-invl.amt * v-slsp[1] / 100.

            IF v-amt1 EQ ? THEN v-amt1 = 0.

            IF ar-inv.inv-date GE fdate[v-per-2] AND
               ar-inv.inv-date LE tdate[v-per-2] THEN
              v-amt[1] = v-amt[1] + v-amt1.

            IF ar-inv.inv-date GE fdate[14] THEN
              v-amt[2] = v-amt[2] + v-amt1.

            CREATE tt-report.
            ASSIGN
             tt-report.key-01  = v-slsm[1]
             tt-report.key-02  = cust.cust-no
             tt-report.dec1    = v-amt[1]
             tt-report.dec2    = v-amt[2]
             tt-report.dec3    = v-amt1
             tt-report.date1   = ar-inv.inv-date
             tt-report.key-10  = "ar-invl"
             tt-report.rec-id  = RECID(ar-invl).
          END.
        END.
      END.

      FOR EACH ttCustList 
          WHERE ttCustList.log-fld
          NO-LOCK,
         EACH cust WHERE cust.company EQ cocode
          AND cust.cust-no EQ ttCustList.cust-no NO-LOCK,

          EACH ar-cash
          WHERE ar-cash.company    EQ cocode
            AND ar-cash.cust-no    EQ cust.cust-no
            AND ar-cash.check-date GE fdate[1]
            AND ar-cash.check-date LE as-of-date
            AND ar-cash.posted     EQ YES
          USE-INDEX ar-cash NO-LOCK,

          EACH ar-cashl
          WHERE ar-cashl.c-no    EQ ar-cash.c-no
            AND ar-cashl.posted  EQ YES
            AND ar-cashl.memo    EQ YES
            AND CAN-FIND(FIRST account
                         WHERE account.company EQ ar-cashl.company
                           AND account.actnum  EQ ar-cashl.actnum
                           AND account.type    EQ "R")
          NO-LOCK:

          {custom/statusMsg.i "'Processing Customer # ' + string(cust.cust-no)"}

        RELEASE ar-invl.

        RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER reftable, BUFFER oe-retl).

        IF AVAIL oe-retl THEN
        FIND FIRST ar-invl
            WHERE ar-invl.company EQ cocode
              AND ar-invl.cust-no EQ cust.cust-no
              AND ar-invl.inv-no  EQ ar-cashl.inv-no
              AND ar-invl.i-no    EQ oe-retl.i-no
              AND (ar-invl.billable OR NOT ar-invl.misc)
            NO-LOCK NO-ERROR.

        IF AVAIL ar-invl THEN
        FIND FIRST itemfg
            WHERE itemfg.company EQ ar-inv.company
              AND itemfg.i-no    EQ ar-invl.i-no
              AND itemfg.procat  GE begin_fg-cat
              AND itemfg.procat  LE end_fg-cat
            NO-LOCK NO-ERROR.

        IF AVAIL itemfg OR ("" GE begin_fg-cat AND "" LE end_fg-cat) THEN
        DO i = 1 TO 3:
          ASSIGN
           v-amt     = 0
           v-slsm[1] = IF (NOT AVAIL ar-invl)                OR
                          (ar-invl.sman[i] EQ "" AND i EQ 1) THEN
                         cust.sman ELSE ar-invl.sman[i].

          IF v-slsm[1]   LT fsman                         OR
             v-slsm[1]   GT tsman                         OR
             (i NE 1 AND
              (v-slsm[1] EQ "" OR ar-invl.s-pct[i] EQ 0)) THEN NEXT.

          ASSIGN
           v-slsp[1] = IF (NOT AVAIL ar-invl)                OR
                          ar-invl.sman[i] EQ ""              OR
                          (ar-invl.s-pct[i] EQ 0 AND i EQ 1) THEN 100
                       ELSE ar-invl.s-pct[i]
           v-amt1    = (ar-cashl.amt-paid - ar-cashl.amt-disc) *
                       v-slsp[1] / 100.

          IF v-amt1 EQ ? THEN v-amt1 = 0.

          IF ar-cash.check-date GE fdate[v-per-2] AND
             ar-cash.check-date LE tdate[v-per-2] THEN
            v-amt[1] = v-amt[1] + v-amt1.

          IF ar-cash.check-date GE fdate[14] THEN
            v-amt[2] = v-amt[2] + v-amt1.

          CREATE tt-report.
          ASSIGN
           tt-report.key-01  = v-slsm[1]
           tt-report.key-02  = cust.cust-no
           tt-report.dec1    = v-amt[1]
           tt-report.dec2    = v-amt[2]
           tt-report.dec3    = v-amt1
           tt-report.date1   = ar-cash.check-date
           tt-report.key-10  = "ar-cashl"
           tt-report.rec-id  = RECID(ar-cashl).

          IF NOT AVAIL ar-invl THEN LEAVE.
        END.
      END.

    v-amt = 0.

    FOR EACH tt-report NO-LOCK

        BREAK BY tt-report.key-01
              BY tt-report.key-02:
        
      ASSIGN
       v-amt[1] = v-amt[1] + tt-report.dec1
       v-amt[2] = v-amt[2] + tt-report.dec2.

      IF LAST-OF(tt-report.key-02) THEN DO:
        IF v-amt[1] NE 0 OR v-inc OR v-ytd THEN DO:
          CREATE tt-report2.
          ASSIGN
           tt-report2.key-01 = tt-report.key-01
           tt-report2.key-02 = tt-report.key-02
           tt-report2.dec1   = IF v-ytd THEN 0 ELSE v-amt[1]
           tt-report2.dec2   = v-amt[2].
        END.

        v-amt = 0.
      END.
    END.

    FOR EACH tt-report2,

        FIRST cust
        WHERE cust.company EQ cocode
          AND cust.cust-no EQ tt-report2.key-02
        NO-LOCK

        BREAK BY tt-report2.key-01
              BY tt-report2.dec1 DESC
              BY tt-report2.dec2 DESC

        WITH FRAME custx:

        {custom/statusMsg.i "'Processing Customer # ' + string(cust.cust-no)"}

      IF FIRST-OF(tt-report2.key-01) THEN DO:
        ASSIGN
         v-prt   = 0
         ll-tots = YES.

        IF FIRST(tt-report2.key-01) THEN VIEW FRAME r-top.

        FIND FIRST sman NO-LOCK
            WHERE sman.company EQ cocode
              AND sman.sman    EQ tt-report2.key-01
            NO-ERROR.
        lv-sman = TRIM(tt-report2.key-01) + " " +
                  TRIM(IF AVAIL sman THEN sman.sname ELSE "Not on file").
          
     /*   IF FIRST(tt-report2.key-01) THEN VIEW FRAME r-top2.*/

        IF tb_excel THEN
           PUT STREAM excel UNFORMATTED
               "SalesRep: " lv-sman SKIP(1)
              '"' REPLACE(excelheader,',','","') '" ' SKIP . 
        PAGE.
      END. 

      ASSIGN
       v-amt = 0
       v-prt = v-prt + 1.

      FOR EACH tt-report
          WHERE tt-report.key-01 EQ tt-report2.key-01
            AND tt-report.key-02 EQ tt-report2.key-02:

        DO v = 1 TO 26:
          IF tt-report.date1 GE fdate[v] AND
             tt-report.date1 LE tdate[v] THEN
            ASSIGN
             v-amt[v] = v-amt[v] + tt-report.dec3
             v-tot[v] = v-tot[v] + tt-report.dec3.
        END.
      END.

      IF v-prt LE v-custs THEN DO:
        DO i = 1 TO 13:
          v-pct[i] = v-amt[i + 13] / v-amt[i] * 100.
          IF v-pct[i] EQ ? THEN v-pct[i] = 0.
        END.

        ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
          
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "cust"     THEN cVarValue = string(cust.cust-no)  .
                         WHEN "name"   THEN cVarValue =  STRING(cust.NAME,"x(30)") .
                        WHEN "hding"   THEN cVarValue =  STRING(lv-ltype[1]) .
                         WHEN "p1"        THEN cVarValue = IF v1 GE 1 THEN STRING(v-amt[14],"->>>,>>>,>>9")  ELSE " " .
                         WHEN "p2"        THEN cVarValue = IF v1 GE 2 THEN STRING(v-amt[15],"->>>,>>>,>>9")  ELSE " ".
                         WHEN "p3"        THEN cVarValue = IF v1 GE 3 THEN STRING(v-amt[16],"->>>,>>>,>>9")  ELSE " ".
                         WHEN "p4"        THEN cVarValue = IF v1 GE 4 THEN STRING(v-amt[17],"->>>,>>>,>>9")  ELSE " ".
                         WHEN "p5"        THEN cVarValue = IF v1 GE 5 THEN STRING(v-amt[18],"->>>,>>>,>>9") ELSE " ".
                         WHEN "p6"        THEN cVarValue = IF v1 GE 6  THEN STRING(v-amt[19],"->>>,>>>,>>9") ELSE " ".
                         WHEN "p7"        THEN cVarValue = IF v1 GE 7  THEN STRING(v-amt[20],"->>>,>>>,>>9") ELSE " ".
                         WHEN "p8"        THEN cVarValue = IF v1 GE 8  THEN STRING(v-amt[21],"->>>,>>>,>>9") ELSE " ".
                         WHEN "p9"        THEN cVarValue = IF v1 GE 9  THEN STRING(v-amt[22],"->>>,>>>,>>9") ELSE " ".
                         WHEN "p10"      THEN cVarValue = IF v1 GE 10  THEN STRING(v-amt[23],"->>>,>>>,>>9") ELSE " ".
                         WHEN "p11"      THEN cVarValue = IF v1 GE 11 THEN STRING(v-amt[24],"->>>,>>>,>>9") ELSE " ".
                         WHEN "p12"      THEN cVarValue = IF v1 GE 12 THEN STRING(v-amt[25],"->>>,>>>,>>9") ELSE " ".
                         WHEN "p13"      THEN cVarValue = IF v1 GE 13  THEN STRING(v-amt[26],"->>>,>>>,>>9") ELSE " ".
                        
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED cDisplay SKIP.
            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED  
                       cExcelDisplay SKIP.
             END.

             ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
          
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "cust"     THEN cVarValue = " "  .
                         WHEN "name"   THEN cVarValue =  " " .
                        WHEN "hding"   THEN cVarValue =  STRING(lv-ltype[2]) .
                         WHEN "p1"        THEN cVarValue = IF v1 GE 1 THEN STRING(v-amt[01],"->>>,>>>,>>9")  ELSE " " .
                         WHEN "p2"        THEN cVarValue = IF v1 GE 2 THEN STRING(v-amt[02],"->>>,>>>,>>9")  ELSE " ". 
                         WHEN "p3"        THEN cVarValue = IF v1 GE 3 THEN STRING(v-amt[03],"->>>,>>>,>>9")  ELSE " ". 
                         WHEN "p4"        THEN cVarValue = IF v1 GE 4 THEN STRING(v-amt[04],"->>>,>>>,>>9")  ELSE " ". 
                         WHEN "p5"        THEN cVarValue = IF v1 GE 5 THEN STRING(v-amt[05],"->>>,>>>,>>9") ELSE " ".  
                         WHEN "p6"        THEN cVarValue = IF v1 GE 6  THEN STRING(v-amt[06],"->>>,>>>,>>9") ELSE " ". 
                         WHEN "p7"        THEN cVarValue = IF v1 GE 7  THEN STRING(v-amt[07],"->>>,>>>,>>9") ELSE " ". 
                         WHEN "p8"        THEN cVarValue = IF v1 GE 8  THEN STRING(v-amt[08],"->>>,>>>,>>9") ELSE " ". 
                         WHEN "p9"        THEN cVarValue = IF v1 GE 9  THEN STRING(v-amt[09],"->>>,>>>,>>9") ELSE " ". 
                         WHEN "p10"      THEN cVarValue = IF v1 GE 10  THEN STRING(v-amt[10],"->>>,>>>,>>9") ELSE " ". 
                         WHEN "p11"      THEN cVarValue = IF v1 GE 11 THEN STRING(v-amt[11],"->>>,>>>,>>9") ELSE " ".  
                         WHEN "p12"      THEN cVarValue = IF v1 GE 12 THEN STRING(v-amt[12],"->>>,>>>,>>9") ELSE " ".  
                         WHEN "p13"      THEN cVarValue = IF v1 GE 13  THEN STRING(v-amt[13],"->>>,>>>,>>9") ELSE " ". 
                        
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED cDisplay SKIP.
            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED  
                       cExcelDisplay SKIP.
             END.

              ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
          
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "cust"     THEN cVarValue = "" .
                         WHEN "name"   THEN cVarValue =  " " .
                        WHEN "hding"   THEN cVarValue =  STRING(lv-ltype[3]) .
                         WHEN "p1"        THEN cVarValue = IF v1 GE 1 THEN STRING(v-pct[01],"->>,>>>,>>9%")  ELSE " " .
                         WHEN "p2"        THEN cVarValue = IF v1 GE 2  THEN STRING(v-pct[02],"->>,>>>,>>9%")  ELSE " ".
                         WHEN "p3"        THEN cVarValue = IF v1 GE 3 THEN  STRING(v-pct[03],"->>,>>>,>>9%")  ELSE " ".
                         WHEN "p4"        THEN cVarValue = IF v1 GE 4  THEN STRING(v-pct[04],"->>,>>>,>>9%")  ELSE " ".
                         WHEN "p5"        THEN cVarValue = IF v1 GE 5  THEN STRING(v-pct[05],"->>,>>>,>>9%") ELSE " ".
                         WHEN "p6"        THEN cVarValue = IF v1 GE 6  THEN STRING(v-pct[06],"->>,>>>,>>9%") ELSE " ".
                         WHEN "p7"        THEN cVarValue = IF v1 GE 7  THEN STRING(v-pct[07],"->>,>>>,>>9%") ELSE " ".
                         WHEN "p8"        THEN cVarValue = IF v1 GE 8  THEN STRING(v-pct[08],"->>,>>>,>>9%") ELSE " ".
                         WHEN "p9"        THEN cVarValue = IF v1 GE 9  THEN STRING(v-pct[09],"->>,>>>,>>9%") ELSE " ".
                         WHEN "p10"      THEN cVarValue = IF v1 GE 10  THEN STRING(v-pct[10],"->>,>>>,>>9%") ELSE " ".
                         WHEN "p11"      THEN cVarValue = IF v1 GE 11 THEN STRING(v-pct[11],"->>,>>>,>>9%") ELSE " ".
                         WHEN "p12"      THEN cVarValue = IF v1 GE 12  THEN STRING(v-pct[12],"->>,>>>,>>9%") ELSE " ".
                         WHEN "p13"      THEN cVarValue = IF v1 GE 13  THEN STRING(v-pct[13],"->>,>>>,>>9%") ELSE " ".
                        
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED cDisplay SKIP.
            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED  
                       cExcelDisplay SKIP.
             END.

    

        PUT SKIP(2).

        DO i = 13 TO 2 BY -1:
          DO j = i - 1 TO 1 BY -1:
            ASSIGN
             v-amt[i]      = v-amt[i]      + v-amt[j]
             v-amt[i + 13] = v-amt[i + 13] + v-amt[j + 13].
          END.
        END.

        DO i = 1 TO 13:
          v-pct[i] = v-amt[i + 13] / v-amt[i] * 100.
          IF v-pct[i] EQ ? THEN v-pct[i] = 0.
        END.

        ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
          
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "cust"     THEN cVarValue = " "  .
                         WHEN "name"   THEN cVarValue =  " " .
                        WHEN "hding"   THEN cVarValue =  STRING(lv-ltype[4]) .
                         WHEN "p1"        THEN cVarValue = IF v1 GE 1 THEN STRING(v-amt[14],"->>>,>>>,>>9")  ELSE " " .
                         WHEN "p2"        THEN cVarValue = IF v1 GE 2  THEN STRING(v-amt[15],"->>>,>>>,>>9")  ELSE " ".
                         WHEN "p3"        THEN cVarValue = IF v1 GE 3  THEN STRING(v-amt[16],"->>>,>>>,>>9")  ELSE " ".
                         WHEN "p4"        THEN cVarValue = IF v1 GE 4  THEN STRING(v-amt[17],"->>>,>>>,>>9")  ELSE " ".
                         WHEN "p5"        THEN cVarValue = IF v1 GE 5  THEN STRING(v-amt[18],"->>>,>>>,>>9") ELSE " ".
                         WHEN "p6"        THEN cVarValue = IF v1 GE 6  THEN STRING(v-amt[19],"->>>,>>>,>>9") ELSE " ".
                         WHEN "p7"        THEN cVarValue = IF v1 GE 7  THEN STRING(v-amt[20],"->>>,>>>,>>9") ELSE " ".
                         WHEN "p8"        THEN cVarValue = IF v1 GE 8  THEN STRING(v-amt[21],"->>>,>>>,>>9") ELSE " ".
                         WHEN "p9"        THEN cVarValue = IF v1 GE 9  THEN STRING(v-amt[22],"->>>,>>>,>>9") ELSE " ".
                         WHEN "p10"      THEN cVarValue = IF v1 GE 10  THEN STRING(v-amt[23],"->>>,>>>,>>9") ELSE " ".
                         WHEN "p11"      THEN cVarValue = IF v1 GE 11 THEN STRING(v-amt[24],"->>>,>>>,>>9") ELSE " ".
                         WHEN "p12"      THEN cVarValue = IF v1 GE 12  THEN STRING(v-amt[25],"->>>,>>>,>>9") ELSE " ".
                         WHEN "p13"      THEN cVarValue = IF v1 GE 13  THEN STRING(v-amt[26],"->>>,>>>,>>9") ELSE " ".
                        
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED cDisplay SKIP.
            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED  
                       cExcelDisplay SKIP.
             END.

             ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
          
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "cust"     THEN cVarValue = " "  .
                         WHEN "name"   THEN cVarValue =  " " .
                        WHEN "hding"   THEN cVarValue =  STRING(lv-ltype[5]) .
                         WHEN "p1"        THEN cVarValue = IF v1 GE 1 THEN STRING(v-amt[01],"->>>,>>>,>>9")  ELSE " " . 
                         WHEN "p2"        THEN cVarValue = IF v1 GE 2  THEN STRING(v-amt[02],"->>>,>>>,>>9")  ELSE " ".  
                         WHEN "p3"        THEN cVarValue = IF v1 GE 3  THEN STRING(v-amt[03],"->>>,>>>,>>9")  ELSE " ".  
                         WHEN "p4"        THEN cVarValue = IF v1 GE 4  THEN STRING(v-amt[04],"->>>,>>>,>>9")  ELSE " ".  
                         WHEN "p5"        THEN cVarValue = IF v1 GE 5  THEN STRING(v-amt[05],"->>>,>>>,>>9") ELSE " ".   
                         WHEN "p6"        THEN cVarValue = IF v1 GE 6  THEN STRING(v-amt[06],"->>>,>>>,>>9") ELSE " ".   
                         WHEN "p7"        THEN cVarValue = IF v1 GE 7  THEN STRING(v-amt[07],"->>>,>>>,>>9") ELSE " ".   
                         WHEN "p8"        THEN cVarValue = IF v1 GE 8  THEN STRING(v-amt[08],"->>>,>>>,>>9") ELSE " ".   
                         WHEN "p9"        THEN cVarValue = IF v1 GE 9  THEN STRING(v-amt[09],"->>>,>>>,>>9") ELSE " ".   
                         WHEN "p10"      THEN cVarValue = IF v1 GE 10  THEN STRING(v-amt[10],"->>>,>>>,>>9") ELSE " ".  
                         WHEN "p11"      THEN cVarValue = IF v1 GE 11 THEN STRING(v-amt[11],"->>>,>>>,>>9") ELSE " ".   
                         WHEN "p12"      THEN cVarValue = IF v1 GE 12  THEN STRING(v-amt[12],"->>>,>>>,>>9") ELSE " ".  
                         WHEN "p13"      THEN cVarValue = IF v1 GE 13  THEN STRING(v-amt[13],"->>>,>>>,>>9") ELSE " ".  
                        
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED cDisplay SKIP.
            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED  
                       cExcelDisplay SKIP.
             END.

              ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
          
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "cust"     THEN cVarValue = " " .
                         WHEN "name"   THEN cVarValue =  " " .
                        WHEN "hding"   THEN cVarValue =  STRING(lv-ltype[6]) .
                         WHEN "p1"        THEN cVarValue = IF v1 GE 1 THEN STRING(v-pct[01],"->>,>>>,>>9%")  ELSE " " .
                         WHEN "p2"        THEN cVarValue = IF v1 GE 2  THEN STRING(v-pct[02],"->>,>>>,>>9%")  ELSE " ".
                         WHEN "p3"        THEN cVarValue = IF v1 GE 3  THEN STRING(v-pct[03],"->>,>>>,>>9%")  ELSE " ".
                         WHEN "p4"        THEN cVarValue = IF v1 GE 4  THEN STRING(v-pct[04],"->>,>>>,>>9%")  ELSE " ".
                         WHEN "p5"        THEN cVarValue = IF v1 GE 5  THEN STRING(v-pct[05],"->>,>>>,>>9%") ELSE " ".
                         WHEN "p6"        THEN cVarValue = IF v1 GE 6  THEN STRING(v-pct[06],"->>,>>>,>>9%") ELSE " ".
                         WHEN "p7"        THEN cVarValue = IF v1 GE 7  THEN STRING(v-pct[07],"->>,>>>,>>9%") ELSE " ".
                         WHEN "p8"        THEN cVarValue = IF v1 GE 8  THEN STRING(v-pct[08],"->>,>>>,>>9%") ELSE " ".
                         WHEN "p9"        THEN cVarValue = IF v1 GE 9 THEN  STRING(v-pct[09],"->>,>>>,>>9%") ELSE " ".
                         WHEN "p10"      THEN cVarValue = IF v1 GE 10  THEN STRING(v-pct[10],"->>,>>>,>>9%") ELSE " ".
                         WHEN "p11"      THEN cVarValue = IF v1 GE 11  THEN STRING(v-pct[11],"->>,>>>,>>9%") ELSE " ".
                         WHEN "p12"      THEN cVarValue = IF v1 GE 12  THEN STRING(v-pct[12],"->>,>>>,>>9%") ELSE " ".
                         WHEN "p13"      THEN cVarValue = IF v1 GE 13  THEN STRING(v-pct[13],"->>,>>>,>>9%") ELSE " ".
                        
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED cDisplay SKIP.
            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED  
                       cExcelDisplay SKIP.
             END.

        PUT SKIP(3).
      END.

      IF ll-tots AND (LAST-OF(tt-report2.key-01) OR v-prt EQ v-custs) THEN DO:
        ll-tots = NO.

        DO i = 1 TO EXTENT(v-amt):
          v-amt[i] = v-tot[i].
        END.

        DO i = 1 TO 13:
          v-pct[i] = v-amt[i] / v-amt[i + 13] * 100.
          IF v-pct[i] EQ ? THEN v-pct[i] = 0.
        END.

        ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
          
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "cust"     THEN cVarValue = " "  .
                         WHEN "name"   THEN cVarValue =  " " .
                        WHEN "hding"   THEN cVarValue =  STRING(lv-ltype[1]) .
                         WHEN "p1"        THEN cVarValue = IF v1 GE 1 THEN STRING(v-amt[14],"->>>,>>>,>>9")  ELSE " " .
                         WHEN "p2"        THEN cVarValue = IF v1 GE 2  THEN STRING(v-amt[15],"->>>,>>>,>>9")  ELSE " ".
                         WHEN "p3"        THEN cVarValue = IF v1 GE 3  THEN STRING(v-amt[16],"->>>,>>>,>>9")  ELSE " ".
                         WHEN "p4"        THEN cVarValue = IF v1 GE 4  THEN STRING(v-amt[17],"->>>,>>>,>>9")  ELSE " ".
                         WHEN "p5"        THEN cVarValue = IF v1 GE 5  THEN STRING(v-amt[18],"->>>,>>>,>>9") ELSE " ".
                         WHEN "p6"        THEN cVarValue = IF v1 GE 6  THEN STRING(v-amt[19],"->>>,>>>,>>9") ELSE " ".
                         WHEN "p7"        THEN cVarValue = IF v1 GE 7  THEN STRING(v-amt[20],"->>>,>>>,>>9") ELSE " ".
                         WHEN "p8"        THEN cVarValue = IF v1 GE 8  THEN STRING(v-amt[21],"->>>,>>>,>>9") ELSE " ".
                         WHEN "p9"        THEN cVarValue = IF v1 GE 9  THEN STRING(v-amt[22],"->>>,>>>,>>9") ELSE " ".
                         WHEN "p10"      THEN cVarValue = IF v1 GE 10  THEN STRING(v-amt[23],"->>>,>>>,>>9") ELSE " ".
                         WHEN "p11"      THEN cVarValue = IF v1 GE 11  THEN STRING(v-amt[24],"->>>,>>>,>>9") ELSE " ".
                         WHEN "p12"      THEN cVarValue = IF v1 GE 12  THEN STRING(v-amt[25],"->>>,>>>,>>9") ELSE " ".
                         WHEN "p13"      THEN cVarValue = IF v1 GE 13  THEN STRING(v-amt[26],"->>>,>>>,>>9") ELSE " ".
                         
                        
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED  "        Totals" SUBSTRING(cDisplay,15,350) SKIP.
            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED ' Totals ' 
                       substring(cExcelDisplay,4,350) SKIP.
             END.

             ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
          
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "cust"     THEN cVarValue = " "  .
                         WHEN "name"   THEN cVarValue =  " " .
                        WHEN "hding"   THEN cVarValue =  STRING(lv-ltype[2]) .
                         WHEN "p1"        THEN cVarValue = IF v1 GE 1 THEN STRING(v-amt[01],"->>>,>>>,>>9")  ELSE " ".  
                         WHEN "p2"        THEN cVarValue = IF v1 GE 2 THEN STRING(v-amt[02],"->>>,>>>,>>9")  ELSE " ".  
                         WHEN "p3"        THEN cVarValue = IF v1 GE 3  THEN STRING(v-amt[03],"->>>,>>>,>>9")  ELSE " ".  
                         WHEN "p4"        THEN cVarValue = IF v1 GE 4  THEN STRING(v-amt[04],"->>>,>>>,>>9")  ELSE " ".  
                         WHEN "p5"        THEN cVarValue = IF v1 GE 5  THEN STRING(v-amt[05],"->>>,>>>,>>9")  ELSE " ".  
                         WHEN "p6"        THEN cVarValue = IF v1 GE 6  THEN STRING(v-amt[06],"->>>,>>>,>>9")  ELSE " ".  
                         WHEN "p7"        THEN cVarValue = IF v1 GE 7  THEN STRING(v-amt[07],"->>>,>>>,>>9")  ELSE " ".  
                         WHEN "p8"        THEN cVarValue = IF v1 GE 8 THEN  STRING(v-amt[08],"->>>,>>>,>>9")  ELSE " ".  
                         WHEN "p9"        THEN cVarValue = IF v1 GE 9  THEN STRING(v-amt[09],"->>>,>>>,>>9")  ELSE " ".  
                         WHEN "p10"      THEN cVarValue = IF v1 GE 10  THEN STRING(v-amt[10],"->>>,>>>,>>9") ELSE " ".  
                         WHEN "p11"      THEN cVarValue = IF v1 GE 11  THEN STRING(v-amt[11],"->>>,>>>,>>9") ELSE " ".  
                         WHEN "p12"      THEN cVarValue = IF v1 GE 12  THEN STRING(v-amt[12],"->>>,>>>,>>9") ELSE " ".  
                         WHEN "p13"      THEN cVarValue = IF v1 GE 13 THEN  STRING(v-amt[13],"->>>,>>>,>>9") ELSE " " .
                        
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED  "        Totals" SUBSTRING(cDisplay,15,350) SKIP.
            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED ' Totals' 
                       substring(cExcelDisplay,4,350) SKIP.
             END.

              ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
          
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "cust"     THEN cVarValue = " " .
                         WHEN "name"   THEN cVarValue =  " " .
                        WHEN "hding"   THEN cVarValue =  STRING(lv-ltype[3]) .
                         WHEN "p1"        THEN cVarValue = IF v1 GE 1 THEN STRING(v-pct[01],"->>,>>>,>>9%")  ELSE " " .
                         WHEN "p2"        THEN cVarValue = IF v1 GE 2  THEN STRING(v-pct[02],"->>,>>>,>>9%")  ELSE " ".
                         WHEN "p3"        THEN cVarValue = IF v1 GE 3  THEN STRING(v-pct[03],"->>,>>>,>>9%")  ELSE " ".
                         WHEN "p4"        THEN cVarValue = IF v1 GE 4 THEN STRING(v-pct[04],"->>,>>>,>>9%")  ELSE " ".
                         WHEN "p5"        THEN cVarValue = IF v1 GE 5  THEN STRING(v-pct[05],"->>,>>>,>>9%") ELSE " ".
                         WHEN "p6"        THEN cVarValue = IF v1 GE 6  THEN STRING(v-pct[06],"->>,>>>,>>9%") ELSE " ".
                         WHEN "p7"        THEN cVarValue = IF v1 GE 7  THEN STRING(v-pct[07],"->>,>>>,>>9%") ELSE " ".
                         WHEN "p8"        THEN cVarValue = IF v1 GE 8 THEN  STRING(v-pct[08],"->>,>>>,>>9%") ELSE " ".
                         WHEN "p9"        THEN cVarValue = IF v1 GE 9  THEN STRING(v-pct[09],"->>,>>>,>>9%") ELSE " ".
                         WHEN "p10"      THEN cVarValue = IF v1 GE 10  THEN STRING(v-pct[10],"->>,>>>,>>9%") ELSE " ".
                         WHEN "p11"      THEN cVarValue = IF v1 GE 11  THEN STRING(v-pct[11],"->>,>>>,>>9%") ELSE " ".
                         WHEN "p12"      THEN cVarValue = IF v1 GE 12  THEN STRING(v-pct[12],"->>,>>>,>>9%") ELSE " ".
                         WHEN "p13"      THEN cVarValue = IF v1 GE 13  THEN STRING(v-pct[13],"->>,>>>,>>9%") ELSE " ".
                        
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED  "        Totals" SUBSTRING(cDisplay,15,350) SKIP.
            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED ' Totals ' 
                       substring(cExcelDisplay,4,350) SKIP.
             END.

        PUT SKIP(2).

     
        DO i = 13 TO 2 BY -1:
          DO j = i - 1 TO 1 BY -1:
            ASSIGN
             v-amt[i]      = v-amt[i]      + v-amt[j]
             v-amt[i + 13] = v-amt[i + 13] + v-amt[j + 13].
          END.
        END.

        DO i = 1 TO 13:
          v-pct[i] = v-amt[i + 13] / v-amt[i] * 100.
          IF v-pct[i] EQ ? THEN v-pct[i] = 0.
        END.

        ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
          
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "cust"     THEN cVarValue = " "  .
                         WHEN "name"   THEN cVarValue =  " " .
                        WHEN "hding"   THEN cVarValue =  STRING(lv-ltype[4]) .
                         WHEN "p1"        THEN cVarValue = IF v1 GE 1 THEN STRING(v-amt[14],"->>>,>>>,>>9")  ELSE " ".  
                         WHEN "p2"        THEN cVarValue = IF v1 GE 2  THEN STRING(v-amt[15],"->>>,>>>,>>9")  ELSE " ".  
                         WHEN "p3"        THEN cVarValue = IF v1 GE 3 THEN  STRING(v-amt[16],"->>>,>>>,>>9")  ELSE " ".  
                         WHEN "p4"        THEN cVarValue = IF v1 GE 4  THEN STRING(v-amt[17],"->>>,>>>,>>9")  ELSE " ".  
                         WHEN "p5"        THEN cVarValue = IF v1 GE 5  THEN STRING(v-amt[18],"->>>,>>>,>>9")  ELSE " ".  
                         WHEN "p6"        THEN cVarValue = IF v1 GE 6  THEN STRING(v-amt[19],"->>>,>>>,>>9")  ELSE " ".  
                         WHEN "p7"        THEN cVarValue = IF v1 GE 7  THEN STRING(v-amt[20],"->>>,>>>,>>9")  ELSE " ".  
                         WHEN "p8"        THEN cVarValue = IF v1 GE 8  THEN STRING(v-amt[21],"->>>,>>>,>>9")  ELSE " ".  
                         WHEN "p9"        THEN cVarValue = IF v1 GE 9  THEN STRING(v-amt[22],"->>>,>>>,>>9")  ELSE " ".  
                         WHEN "p10"      THEN cVarValue = IF v1 GE 10  THEN STRING(v-amt[23],"->>>,>>>,>>9") ELSE " ".  
                         WHEN "p11"      THEN cVarValue = IF v1 GE 11 THEN  STRING(v-amt[24],"->>>,>>>,>>9") ELSE " ".  
                         WHEN "p12"      THEN cVarValue = IF v1 GE 12  THEN STRING(v-amt[25],"->>>,>>>,>>9") ELSE " ".  
                         WHEN "p13"      THEN cVarValue = IF v1 GE 13  THEN STRING(v-amt[26],"->>>,>>>,>>9") ELSE " " .
                         
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED  "        Totals" SUBSTRING(cDisplay,15,350) SKIP.
            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED ' Totals ' 
                       substring(cExcelDisplay,4,350) SKIP.
             END.

             ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
          
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "cust"     THEN cVarValue = " "  .
                         WHEN "name"   THEN cVarValue =  " " .
                        WHEN "hding"   THEN cVarValue =  STRING(lv-ltype[5]) .
                         WHEN "p1"        THEN cVarValue = IF v1 GE 1 THEN STRING(v-amt[01],"->>>,>>>,>>9")  ELSE " ".  
                         WHEN "p2"        THEN cVarValue = IF v1 GE 2  THEN STRING(v-amt[02],"->>>,>>>,>>9")  ELSE " ".  
                         WHEN "p3"        THEN cVarValue = IF v1 GE 3 THEN  STRING(v-amt[03],"->>>,>>>,>>9")  ELSE " ".  
                         WHEN "p4"        THEN cVarValue = IF v1 GE 4  THEN STRING(v-amt[04],"->>>,>>>,>>9")  ELSE " ".  
                         WHEN "p5"        THEN cVarValue = IF v1 GE 5  THEN STRING(v-amt[05],"->>>,>>>,>>9")  ELSE " ".  
                         WHEN "p6"        THEN cVarValue = IF v1 GE 6  THEN STRING(v-amt[06],"->>>,>>>,>>9")  ELSE " ".  
                         WHEN "p7"        THEN cVarValue = IF v1 GE 7  THEN STRING(v-amt[07],"->>>,>>>,>>9")  ELSE " ".  
                         WHEN "p8"        THEN cVarValue = IF v1 GE 8  THEN STRING(v-amt[08],"->>>,>>>,>>9")  ELSE " ".  
                         WHEN "p9"        THEN cVarValue = IF v1 GE 9  THEN STRING(v-amt[09],"->>>,>>>,>>9")  ELSE " ".  
                         WHEN "p10"      THEN cVarValue = IF v1 GE 10  THEN STRING(v-amt[10],"->>>,>>>,>>9") ELSE " ".  
                         WHEN "p11"      THEN cVarValue = IF v1 GE 11 THEN  STRING(v-amt[11],"->>>,>>>,>>9") ELSE " ".  
                         WHEN "p12"      THEN cVarValue = IF v1 GE 12  THEN STRING(v-amt[12],"->>>,>>>,>>9") ELSE " ".  
                         WHEN "p13"      THEN cVarValue = IF v1 GE 13  THEN STRING(v-amt[13],"->>>,>>>,>>9") ELSE " " .
                         
                        
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED  "        Totals" SUBSTRING(cDisplay,15,350) SKIP.
            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED ' Totals ' 
                       substring(cExcelDisplay,4,350) SKIP.
             END.

              ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
          
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "cust"     THEN cVarValue = " ".
                         WHEN "name"   THEN cVarValue =  " " .
                        WHEN "hding"   THEN cVarValue =  STRING(lv-ltype[6]) .
                         WHEN "p1"        THEN cVarValue = IF v1 GE 1 THEN STRING(v-pct[01],"->>,>>>,>>9%")  ELSE " " .
                         WHEN "p2"        THEN cVarValue = IF v1 GE 2  THEN STRING(v-pct[02],"->>,>>>,>>9%")  ELSE " ".
                         WHEN "p3"        THEN cVarValue = IF v1 GE 3  THEN STRING(v-pct[03],"->>,>>>,>>9%")  ELSE " ".
                         WHEN "p4"        THEN cVarValue = IF v1 GE 4  THEN STRING(v-pct[04],"->>,>>>,>>9%")  ELSE " ".
                         WHEN "p5"        THEN cVarValue = IF v1 GE 5  THEN STRING(v-pct[05],"->>,>>>,>>9%") ELSE " ".
                         WHEN "p6"        THEN cVarValue = IF v1 GE 6  THEN STRING(v-pct[06],"->>,>>>,>>9%") ELSE " ".
                         WHEN "p7"        THEN cVarValue = IF v1 GE 7  THEN STRING(v-pct[07],"->>,>>>,>>9%") ELSE " ".
                         WHEN "p8"        THEN cVarValue = IF v1 GE 8  THEN STRING(v-pct[08],"->>,>>>,>>9%") ELSE " ".
                         WHEN "p9"        THEN cVarValue = IF v1 GE 9  THEN STRING(v-pct[09],"->>,>>>,>>9%") ELSE " ".
                         WHEN "p10"      THEN cVarValue = IF v1 GE 10  THEN STRING(v-pct[10],"->>,>>>,>>9%") ELSE " ".
                         WHEN "p11"      THEN cVarValue = IF v1 GE 11 THEN STRING(v-pct[11],"->>,>>>,>>9%") ELSE " ".
                         WHEN "p12"      THEN cVarValue = IF v1 GE 12  THEN STRING(v-pct[12],"->>,>>>,>>9%") ELSE " ".
                         WHEN "p13"      THEN cVarValue = IF v1 GE 13  THEN STRING(v-pct[13],"->>,>>>,>>9%") ELSE " ".
                        
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED  "        Totals" SUBSTRING(cDisplay,15,350) SKIP.
            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED ' Totals ' 
                       substring(cExcelDisplay,4,350) SKIP.
             END.
      END.

      IF LAST-OF(tt-report2.key-01) THEN v-tot = 0.
    END.
