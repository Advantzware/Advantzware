
    FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode NO-LOCK NO-ERROR.
    
    FOR EACH oe-ord
        WHERE oe-ord.company  EQ cocode
          AND oe-ord.cust-no  GE v-cust[1]
          AND oe-ord.cust-no  LE v-cust[2]
          AND oe-ord.ord-date GE v-date[1]
          AND oe-ord.ord-date LE v-date[2]
          AND oe-ord.user-id  GE begin_userid
          AND oe-ord.user-id  LE end_userid
          AND (v-ostat EQ "A"                           OR
               (oe-ord.opened AND v-ostat EQ "O")       OR
               (NOT oe-ord.opened AND v-ostat EQ "C"))
        USE-INDEX ordate NO-LOCK,

        EACH oe-ordl OF oe-ord
        WHERE oe-ordl.i-no      GE v-item[1]
          AND oe-ordl.i-no      LE v-item[2]
          AND FILL(" ",6 - LENGTH(TRIM(oe-ordl.job-no))) +
              TRIM(oe-ordl.job-no) + STRING(oe-ordl.job-no2,"99") GE v-job[1]
          AND FILL(" ",6 - LENGTH(TRIM(oe-ordl.job-no))) +
              TRIM(oe-ordl.job-no) + STRING(oe-ordl.job-no2,"99") LE v-job[2]
          AND oe-ordl.po-no     GE v-po[1]
          AND oe-ordl.po-no     LE v-po[2]
          AND oe-ordl.s-man[1]  GE begin_slsmn
          AND oe-ordl.s-man[1]  LE end_slsmn
          AND (v-ostat EQ "A"                           OR
               (oe-ordl.stat NE "C" AND v-ostat EQ "O") OR
               (oe-ordl.stat EQ "C" AND v-ostat EQ "C"))
        NO-LOCK:

          {custom/statusMsg.i "'Processing Order # ' + string(oe-ordl.ord-no)"} 

        FIND FIRST itemfg WHERE itemfg.company EQ cocode 
                          AND itemfg.i-no = oe-ordl.i-no NO-LOCK NO-ERROR.
        
        IF itemfg.stat NE "" THEN
            IF AVAIL itemfg AND (itemfg.stat NE "A") AND NOT tb_itm-act THEN NEXT .

      {oerep/r-ordo1N.i}
    END.

    FOR EACH tt-report WHERE tt-report.term-id EQ "",
        FIRST oe-ordl WHERE ROWID(oe-ordl) EQ tt-report.row-id NO-LOCK,
        FIRST oe-ord OF oe-ordl NO-LOCK
        BREAK BY tt-report.row-id
              BY tt-report.key-07:
        
        {custom/statusMsg.i "'Processing Order # ' + string(oe-ordl.ord-no)"} 

      ASSIGN
       v-q-shp = v-q-shp + tt-report.q-shp
       v-q-rel = v-q-rel + tt-report.q-rel.

      IF LAST-OF(tt-report.row-id) THEN DO:
        IF NOT CAN-FIND(FIRST tt-fg-bin
                        WHERE tt-fg-bin.company EQ cocode
                          AND tt-fg-bin.i-no    EQ oe-ordl.i-no) THEN
          RUN calc-qoh.

        FOR EACH tt-fg-bin
            WHERE tt-fg-bin.company EQ oe-ordl.company
              AND tt-fg-bin.i-no    EQ oe-ordl.i-no
              AND tt-fg-bin.job-no  EQ oe-ordl.job-no
              AND tt-fg-bin.job-no2 EQ oe-ordl.job-no2:
          ASSIGN
           tt-report.q-onh  = tt-report.q-onh + tt-fg-bin.qty
           tt-fg-bin.ord-no = oe-ord.ord-no.
        END.

        IF tb_job-qoh THEN
        FOR EACH job-hdr NO-LOCK
            WHERE job-hdr.company  EQ oe-ordl.company
              AND job-hdr.ord-no   EQ oe-ordl.ord-no
              AND job-hdr.i-no     EQ oe-ordl.i-no
              AND (job-hdr.job-no  NE oe-ordl.job-no OR
                   job-hdr.job-no2 NE oe-ordl.job-no2)
            BREAK BY job-hdr.job-no
                  BY job-hdr.job-no2
                  BY job-hdr.i-no:

          IF FIRST-OF(job-hdr.i-no) THEN
          FOR EACH tt-fg-bin
              WHERE tt-fg-bin.company EQ job-hdr.company
                AND tt-fg-bin.i-no    EQ job-hdr.i-no
                AND tt-fg-bin.job-no  EQ job-hdr.job-no
                AND tt-fg-bin.job-no2 EQ job-hdr.job-no2:
            ASSIGN
             tt-report.q-onh  = tt-report.q-onh + tt-fg-bin.qty
             tt-fg-bin.ord-no = oe-ord.ord-no.
          END.
        END.

        ASSIGN
         tt-report.q-shp = v-q-shp
         tt-report.q-rel = v-q-rel.

        IF rd_wip-qty EQ "1" THEN
           tt-report.q-wip = oe-ordl.qty - (tt-report.q-onh + tt-report.q-shp).
        ELSE 
        DO:
           ASSIGN
              lv-job-qty = 0
              lv-rec-qty = 0.

           FIND FIRST job WHERE
                job.company EQ cocode AND
                job.job-no  EQ oe-ordl.job-no AND
                job.job-no2 EQ oe-ordl.job-no2
                NO-LOCK NO-ERROR.

           IF AVAIL job THEN
           DO:
              IF NOT job.opened THEN
                 tt-report.q-wip = 0.
              ELSE
              DO:
                 FOR EACH job-hdr FIELDS(qty) WHERE
                     job-hdr.company  EQ oe-ordl.company AND
                     job-hdr.ord-no   EQ oe-ordl.ord-no AND
                     job-hdr.i-no     EQ oe-ordl.i-no AND
                     job-hdr.job-no   EQ oe-ordl.job-no AND
                     job-hdr.job-no2  EQ oe-ordl.job-no2
                     NO-LOCK:

                     lv-job-qty = lv-job-qty + job-hdr.qty.
                 END.

                 FIND FIRST itemfg WHERE
                      itemfg.company EQ job.company AND
                      itemfg.i-no    EQ oe-ordl.i-no
                      NO-LOCK NO-ERROR.

                 IF AVAIL itemfg THEN
                 DO:
                    IF itemfg.isaset AND itemfg.alloc THEN
                       FOR EACH fg-act FIELDS(qty) WHERE
                           fg-act.company eq job.company AND
                           fg-act.job-no  eq oe-ordl.job-no AND
                           fg-act.job-no2 eq oe-ordl.job-no2 AND
                           fg-act.i-no    eq oe-ordl.i-no
                           NO-LOCK:
                           lv-rec-qty = lv-rec-qty + fg-act.qty.
                       END.
                      
                    FOR EACH fg-rcpth FIELDS(r-no rita-code company) WHERE
                        fg-rcpth.company   EQ job.company AND
                        fg-rcpth.i-no      EQ oe-ordl.i-no AND
                        fg-rcpth.job-no    EQ oe-ordl.job-no AND
                        fg-rcpth.job-no2   EQ oe-ordl.job-no2 AND
                        fg-rcpth.rita-code EQ "R"
                        NO-LOCK,
                        EACH fg-rdtlh FIELDS(qty) WHERE
                             fg-rdtlh.r-no EQ fg-rcpth.r-no AND
                             fg-rdtlh.rita-code EQ fg-rcpth.rita-code
                             NO-LOCK
                             BREAK BY fg-rcpth.company:
                    
                        IF FIRST(fg-rcpth.company) THEN lv-rec-qty = 0.
                    
                        lv-rec-qty = lv-rec-qty + fg-rdtlh.qty.
                    END.
                    
                    RELEASE itemfg.
                 END.
              END.

              RELEASE job.

              tt-report.q-wip = lv-job-qty - lv-rec-qty.

           END.
        END.

        IF tt-report.q-wip LT 0                                     OR
           tt-report.q-wip LT oe-ordl.qty * oe-ordl.under-pct / 100 THEN
           tt-report.q-wip = 0.

        tt-report.q-avl = tt-report.q-onh + tt-report.q-wip - tt-report.q-rel.

        IF tt-report.q-avl LT 0 THEN tt-report.q-avl = 0.

        ASSIGN
         v-q-shp = 0
         v-q-rel = 0.
      END.

      ELSE DELETE tt-report.
    END.

    FOR EACH tt-report
        WHERE tt-report.term-id EQ ""
          AND tt-report.cad-no  GE begin_cad-no
          AND tt-report.cad-no  LE end_cad-no
          AND (tb_0-wip OR tt-report.q-wip GT 0)
          AND (tb_0-avl OR tt-report.q-avl GT 0 OR tt-report.q-rel GT 0),

        FIRST itemfg
        WHERE itemfg.company eq cocode
          AND itemfg.i-no    eq tt-report.key-06
        NO-LOCK,

        FIRST cust
        WHERE cust.company eq cocode
          AND cust.cust-no eq tt-report.key-02
        NO-LOCK,

        FIRST oe-ordl WHERE ROWID(oe-ordl) EQ tt-report.row-id NO-LOCK,

        FIRST oe-ord OF oe-ordl NO-LOCK
            
        BREAK BY tt-report.key-01
              BY tt-report.key-02
              BY tt-report.key-03
              BY tt-report.key-04
              BY tt-report.key-05
              BY tt-report.key-06
              BY tt-report.row-id
              BY tt-report.key-07:

        {custom/statusMsg.i "'Processing Order # ' + string(oe-ordl.ord-no)"} 

    /*  IF FIRST-OF(tt-report.key-01) THEN DO:
        lv-slsmn = tt-report.key-01.

        IF rd_sort-1 EQ "Salesman" THEN DO:
          VIEW FRAME r-top2.
          IF tb_excel THEN
            PUT STREAM st-excel
                       "Sales Rep: "
                       lv-slsmn
                       SKIP.
          IF NOT FIRST(tt-report.key-01) THEN PAGE.
        END.

        IF FIRST(tt-report.key-01) THEN DO:
          VIEW FRAME r-top3.
          PAGE.
        END.
      END.

      DISPLAY cust.cust-no       FORMAT "x(8)"
              tt-report.due-date FORMAT "99/99/9999"
              oe-ordl.part-no
              oe-ordl.i-name
              oe-ordl.i-no
              oe-ordl.ord-no
              tt-report.cad-no   FORMAT "x(13)"
              tt-report.po-no    FORMAT "x(10)"
              oe-ordl.qty        FORMAT "->,>>>,>>9"
              tt-report.q-onh    FORMAT "->,>>>,>>9"
              tt-report.q-shp    FORMAT "->,>>>,>>9"
              tt-report.q-rel    FORMAT "->,>>>,>>9"
              tt-report.q-wip    FORMAT "->,>>>,>>9"
              tt-report.q-avl    FORMAT "->,>>>,>>9"                
          WITH FRAME summ-re NO-BOX STREAM-IO WIDTH 180
          NO-ATTR-SPACE NO-UNDERLINE NO-LABELS.

      DOWN WITH FRAME summ-re.

      IF tb_excel THEN
      DO:
         PUT STREAM st-excel
             cust.cust-no       FORMAT "x(8)" v-comma
             tt-report.due-date FORMAT "99/99/9999" v-comma
             oe-ordl.part-no v-comma
             ReplaceCommas(oe-ordl.i-name) FORMAT "x(30)" v-comma
             ReplaceCommas(oe-ordl.i-no) FORMAT "x(20)" v-comma
             oe-ordl.ord-no v-comma
             tt-report.cad-no   FORMAT "x(13)" v-comma
             tt-report.po-no    FORMAT "x(15)" v-comma  /* 07181307 JAD */
             oe-ordl.qty        FORMAT "->>>>>>9" v-comma
             tt-report.q-onh    FORMAT "->>>>>>9" v-comma
             tt-report.q-shp    FORMAT "->>>>>>9" v-comma
             tt-report.q-rel    FORMAT "->>>>>>9" v-comma
             tt-report.q-wip    FORMAT "->>>>>>9" v-comma
             tt-report.q-avl    FORMAT "->>>>>>9".

        /* IF tb_est-count THEN
            PUT STREAM st-excel v-comma tt-report.unit-count FORMAT ">>>>9".

         IF tb_est-pallets THEN
            PUT STREAM st-excel v-comma tt-report.units-pallet FORMAT ">>9". */

         PUT STREAM st-excel SKIP.
      END.*/


     ASSIGN lv-due-date2 = ? .
      FOR EACH oe-rel
          WHERE oe-rel.company EQ oe-ordl.company
            AND oe-rel.ord-no  EQ oe-ordl.ord-no
            AND oe-rel.i-no    EQ oe-ordl.i-no
            AND oe-rel.line    EQ oe-ordl.line
          NO-LOCK BY oe-rel.rel-date DESC:
      
         {oe/rel-stat.i lv-stat} 
        
        /*IF INDEX("ALSBI",lv-stat) GT 0 THEN DO:*/
           lv-due-date2 = IF AVAIL oe-relh THEN oe-relh.rel-date ELSE oe-rel.rel-date.
           LEAVE.
        /*END.*/
      END.
      dOrdVal = 0.

      if oe-ordl.pr-uom begins "L" AND oe-ordl.pr-uom NE "LB" then
          assign dOrdVal = oe-ordl.price * IF oe-ordl.qty LT 0 THEN -1 ELSE 1.
                             
        else
        if oe-ordl.pr-uom eq "CS" then
        do:
          
          dOrdVal = oe-ordl.qty /
                     (if oe-ordl.cas-cnt ne 0 then oe-ordl.cas-cnt else
                      if avail itemfg and itemfg.case-count ne 0
                      then itemfg.case-count else 1) *
                     oe-ordl.price.
        end.
        else
        if oe-ordl.pr-uom eq "C" then
          dOrdVal = oe-ordl.qty / 100 * oe-ordl.price.

        else
        if oe-ordl.pr-uom eq "M" then
          dOrdVal = oe-ordl.qty / 1000 * oe-ordl.price.

        else /** DEFAULT TO EACH **/
          dOrdVal = oe-ordl.qty * oe-ordl.price.

        dOrdVal = ROUND(dOrdVal,2).

        IF oe-ordl.disc NE 0 THEN
           dOrdVal = ROUND(dOrdVal * (1 - (oe-ordl.disc / 100)),2).

             

            ASSIGN cDisplay = ""
             cTmpField = ""
             cVarValue = ""
             cExcelDisplay = ""
             cExcelVarValue = "".
      
      DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
         cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
              CASE cTmpField:             
                   WHEN "rep"      THEN cVarValue = IF AVAIL oe-ordl THEN STRING(oe-ordl.s-man[1],"x(3)") ELSE "".
                   WHEN "cust"     THEN cVarValue = STRING(cust.cust-no,"x(8)") .
                   WHEN "l-due-dt" THEN cVarValue = IF oe-ordl.req-date NE ? THEN STRING(oe-ordl.req-date) ELSE "".
                   WHEN "r-due-dt" THEN cVarValue = IF lv-due-date2 NE ? THEN STRING(lv-due-date2) ELSE "" .
                   WHEN "cust-prt" THEN cVarValue = IF AVAIL oe-ordl THEN STRING(oe-ordl.part-no) ELSE "" .
                   WHEN "itm-dscr" THEN cVarValue = IF AVAIL oe-ordl THEN STRING(oe-ordl.i-name) ELSE "" .
                   WHEN "fg-itm"   THEN cVarValue = IF AVAIL oe-ordl THEN string(oe-ordl.i-no) ELSE "" .
                   WHEN "ord"      THEN cVarValue = IF AVAIL oe-ordl THEN STRING(oe-ordl.ord-no) ELSE "" .
                   WHEN "cad"      THEN cVarValue = STRING(tt-report.cad-no,"x(13)") .
                   WHEN "po"       THEN cVarValue = STRING(tt-report.po-no,"x(10)") .
                   WHEN "ord-qty"  THEN cVarValue = IF AVAIL oe-ordl THEN STRING(oe-ordl.qty,"->,>>>,>>9") ELSE "" .
                   WHEN "qty-oh "  THEN cVarValue = STRING(tt-report.q-onh,"->,>>>,>>9") .
                   WHEN "qty-shp"  THEN cVarValue = STRING(tt-report.q-shp,"->,>>>,>>9") .
                   WHEN "qty-act"  THEN cVarValue = STRING(tt-report.q-rel,"->,>>>,>>9") .
                   WHEN "qty-wip"  THEN cVarValue = STRING(tt-report.q-wip,"->,>>>,>>9") .
                   WHEN "qty-avl"  THEN cVarValue = STRING(tt-report.q-avl,"->,>>>,>>9") .
                   WHEN "est-unt"  THEN cVarValue = STRING(tt-report.unit-count,">>>>9") .
                   WHEN "est-palt" THEN cVarValue = STRING(tt-report.units-pallet,">>9") .
                   WHEN "ord-value"  THEN cVarValue =  STRING(dOrdVal,"->>>,>>>,>>9.99") .
                   WHEN "ack-date" THEN cVarValue = IF oe-ord.ack-prnt-date NE ? THEN STRING(oe-ord.ack-prnt-date) ELSE "".
                  /* WHEN "bin-job"  THEN cVarValue = STRING(v-bin-job)              .
                   WHEN "whs"      THEN cVarValue = STRING(v-whs)              .
                   WHEN "bin"      THEN cVarValue = STRING(v-bin)              .
                   WHEN "tag"      THEN cVarValue = STRING(v-tab,"x(8)")              .
                   WHEN "qty"      THEN cVarValue = STRING(v-qty,">>>,>>>,>>9")              . */
               END CASE.                                                                 
                
              cExcelVarValue = cVarValue.
              cDisplay = cDisplay + cVarValue +
                         FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
              cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
      END.
      
      PUT UNFORMATTED cDisplay SKIP.
      IF tb_excel THEN DO:
           PUT STREAM st-excel UNFORMATTED  
                 cExcelDisplay SKIP.
       END.  

      IF v-jobq AND tt-report.q-onh GT 0 THEN
      FOR EACH tt-fg-bin
          WHERE tt-fg-bin.company EQ cocode
            AND tt-fg-bin.i-no    EQ tt-report.key-06
            AND tt-fg-bin.qty     GT 0
            AND (tt-fg-bin.ord-no EQ oe-ord.ord-no OR
                 SUBSTR(tt-report.key-04,1,6) EQ "")
          NO-LOCK
          BREAK BY tt-fg-bin.job-no
                BY tt-fg-bin.job-no2
                BY tt-fg-bin.loc
                BY tt-fg-bin.loc-bin
                BY tt-fg-bin.tag:
        
        IF FIRST(tt-fg-bin.job-no2) THEN
          PUT SKIP(1)
              SPACE(34)
              "Bins: Job "
              "Whs   "
              "Bin      "
              "Tag      "
              "        Qty"
              SKIP
              SPACE(34)
              "--------- "
              "----- "
              "-------- "
              "-------- "
              "-----------"
              SKIP.
        
        PUT SPACE(34)
            FILL(" ",6 - LENGTH(TRIM(tt-fg-bin.job-no))) +
            TRIM(tt-fg-bin.job-no) + 
                 (IF tt-fg-bin.job-no NE ""
                  THEN ("-" + STRING(tt-fg-bin.job-no2,"99"))
                  ELSE "")              FORMAT "x(9)"
            SPACE(1)
            tt-fg-bin.loc
            SPACE(1)
            tt-fg-bin.loc-bin
            SPACE(1).

        IF SUBSTR(tt-fg-bin.tag,1,15) EQ tt-fg-bin.i-no THEN
          PUT SUBSTR(tt-fg-bin.tag,16,8) FORMAT "x(8)".
        ELSE
          PUT tt-fg-bin.tag.

        PUT SPACE(1)
            tt-fg-bin.qty               FORMAT ">>>,>>>,>>9"
            SKIP.
      END.            

      PUT SKIP(1). 
      DELETE tt-report.
    END. /* each tt-report */
