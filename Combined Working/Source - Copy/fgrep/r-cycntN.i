     CREATE tt-report.
     ASSIGN
        tt-report.rec-id = RECID(fg-bin)
        tt-report.key-01 = IF v-item-bin EQ "B" OR v-item-bin EQ "C"
                             THEN 
                              STRING(fg-bin.loc,"x(10)") +
                              STRING(fg-bin.loc-bin,"x(10)") +
                              STRING(fg-bin.tag,"x(30)") +
                              STRING(fg-bin.cust-no,"x(10)")
                             ELSE IF v-item-bin EQ "N" THEN itemfg.i-name  ELSE ""
        tt-report.key-02 = fg-bin.i-no
       /* gdm - 10160901 */
        tt-report.key-03 = itemfg.cust-no
         /* this should always come from itemfg
                           IF fg-bin.cust-no NE ""
                            THEN fg-bin.cust-no 
                             ELSE itemfg.cust-no
         */
        tt-report.key-04 = TRIM(STRING(fg-bin.loc,"x(10)"))
        tt-report.key-05 = TRIM(STRING(fg-bin.loc-bin,"x(10)"))
        .
