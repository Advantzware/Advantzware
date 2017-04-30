    
    FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode NO-LOCK NO-ERROR.
    
    FOR EACH ttCustList NO-LOCK,
        EACH oe-ord
        WHERE oe-ord.company  EQ cocode
          AND oe-ord.cust-no EQ ttCustList.cust-no
/*           AND oe-ord.cust-no  GE v-cust[1] */
/*           AND oe-ord.cust-no  LE v-cust[2] */
          AND oe-ord.user-id  GE begin_userid
          AND oe-ord.user-id  LE end_userid
          AND (v-ostat EQ "A"                           OR
               (oe-ord.opened AND v-ostat EQ "O")       OR
               (NOT oe-ord.opened AND v-ostat EQ "C"))
/*         USE-INDEX ordate */
        NO-LOCK,

        EACH oe-ordl OF oe-ord
        WHERE 
           FILL(" ",6 - LENGTH(TRIM(oe-ordl.job-no))) +
              TRIM(oe-ordl.job-no) + STRING(oe-ordl.job-no2,"99") GE v-job[1]
          AND FILL(" ",6 - LENGTH(TRIM(oe-ordl.job-no))) +
              TRIM(oe-ordl.job-no) + STRING(oe-ordl.job-no2,"99") LE v-job[2]
          AND oe-ordl.s-man[1]  GE begin_slsmn
          AND oe-ordl.s-man[1]  LE end_slsmn
          AND (v-ostat EQ "A"                           OR
               (oe-ordl.stat NE "C" AND v-ostat EQ "O") OR
               (oe-ordl.stat EQ "C" AND v-ostat EQ "C"))
        NO-LOCK:

       {custom/statusMsg.i "'Processing Customer # ' + string(oe-ord.cust-no)"} 

      {oerep/r-hotsOp1.i}
    END.

    FOR EACH tt-report WHERE tt-report.term-id EQ "",
        FIRST oe-ordl WHERE ROWID(oe-ordl) EQ tt-report.row-id NO-LOCK,
        FIRST oe-ord OF oe-ordl NO-LOCK
        BREAK BY tt-report.row-id
              BY tt-report.key-07:
        
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

        ASSIGN
         tt-report.q-shp = v-q-shp
         tt-report.q-rel = v-q-rel.

        FIND FIRST itemfg WHERE
             itemfg.company eq cocode AND
             itemfg.i-no    eq tt-report.key-06
             NO-LOCK NO-ERROR.

        IF AVAIL itemfg THEN
        DO:
           IF scr-msf EQ "Rel" THEN
              tt-report.msf = tt-report.q-rel * itemfg.t-sqft / 1000.
           ELSE
              tt-report.msf = oe-ordl.qty * itemfg.t-sqft / 1000.
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
          AND tt-report.cad-no  LE end_cad-no,

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

        {custom/statusMsg.i "'Processing Customer # ' + string(cust.cust-no)"} 

      IF FIRST-OF(tt-report.key-01) THEN DO:
         lv-date-msf = 0.
        
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

        IF FIRST(tt-report.key-01) THEN
           VIEW FRAME r-top3.
        
        put "Due Date: " substring(tt-report.key-01,5,2) + "/" + substring(tt-report.key-01,7,2) +
                              "/" +  substring(tt-report.key-01,1,4)  form "x(10)" skip.
        UNDERLINE cust.cust-no
          oe-ordl.part-no tt-report.routing tt-report.rm-no
          v-job# tt-report.ship-to oe-ordl.qty tt-report.q-rel
          tt-report.numofunit tt-report.msf
          WITH FRAME summ-re.
        DOWN WITH FRAME summ-re.
      END.

      assign lv-date-msf = lv-date-msf + tt-report.msf
                lv-tot-msf = lv-tot-msf + tt-report.msf.
                
      DISPLAY cust.cust-no       FORMAT "x(8)"
              oe-ordl.part-no
              tt-report.routing form "x(20)"
              tt-report.rm-no form "x(20)"
              trim(oe-ordl.job-no) + "-" + STRING(oe-ordl.job-no2,"99") WHEN oe-ordl.job-no <> "" @ v-job# FORM "x(10)" 
              
              tt-report.ship-to
              oe-ordl.qty        FORMAT "->,>>>,>>9"
              tt-report.q-rel    FORMAT "->,>>>,>>9"
              tt-report.NumOfUnit
              tt-report.msf              
          WITH FRAME summ-re NO-BOX STREAM-IO WIDTH 180
                NO-ATTR-SPACE NO-LABELS DOWN. 

      DOWN WITH FRAME summ-re.
      UNDERLINE cust.cust-no
          oe-ordl.part-no tt-report.routing tt-report.rm-no
          v-job# tt-report.ship-to oe-ordl.qty tt-report.q-rel
          tt-report.numofunit tt-report.msf
          WITH FRAME summ-re.
      DOWN WITH FRAME summ-re.

      IF tb_excel THEN
      DO:
         PUT STREAM st-excel
             tt-report.due-date FORMAT "99/99/9999" v-comma
             cust.cust-no       FORMAT "x(8)" v-comma             
             oe-ordl.part-no FORMAT "X(15)" v-comma
             REPLACE(tt-report.routing,","," ") FORMAT "X(20)" v-comma
             tt-report.rm-no form "x(20)" v-comma
             oe-ordl.job-no + "-" + STRING(oe-ordl.job-no2,"99") v-comma
             tt-report.ship-to    v-comma
             oe-ordl.qty        FORMAT "->>>>>>9" v-comma
             tt-report.q-rel    FORMAT "->>>>>>9" v-comma
             tt-report.NumOfUnit  FORMAT "->>>>>>9" v-comma
             tt-report.msf    FORMAT "->>>>>>9".

         PUT STREAM st-excel SKIP.
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

      v-set-count = 0.

      FOR EACH tt-fg-set WHERE
          tt-fg-set.ord-no EQ oe-ordl.ord-no AND
          tt-fg-set.LINE EQ oe-ordl.LINE:

          v-set-count = v-set-count + 1.

          IF v-set-count GT 1 THEN
             LEAVE.
      END.

      IF v-set-count GT 1 THEN
      FOR EACH tt-fg-set WHERE
          tt-fg-set.ord-no EQ oe-ordl.ord-no AND
          tt-fg-set.LINE EQ oe-ordl.LINE:

          ASSIGN s-b-line = "S/B: " + STRING(tt-fg-set.part-qty) + "/"
                          + STRING(tt-fg-set.part-qty-dec).

          DISPLAY 
             s-b-line
             tt-fg-set.part-no 
             tt-fg-set.routing
             tt-fg-set.rm-no  
          WITH FRAME tt-fg-set-frame.
 
          DOWN WITH FRAME tt-fg-set-frame.

          IF tb_excel THEN
             PUT STREAM st-excel
                 s-b-line v-comma
                 v-comma
                 tt-fg-set.part-no v-comma
                 REPLACE(tt-fg-set.routing,","," ") v-comma
                 tt-fg-set.rm-no SKIP.
      END.

      if last-of (tt-report.key-01) then do:
         
         disp  "Total: " @ tt-report.q-rel 
                 lv-date-msf @ tt-report.msf with frame summ-re.
         down with frame summ-re.
         UNDERLINE tt-report.q-rel tt-report.msf WITH FRAME summ-re.
         DOWN WITH FRAME summ-re.
      end.

      if last (tt-report.key-01) then do:
         
         disp  "Grand Total: " @ tt-report.q-rel 
                 lv-tot-msf @ tt-report.msf with frame summ-re.
         down with frame summ-re.
         underline tt-report.q-rel tt-report.msf with frame summ-re.
         down with frame summ-re.
         
      end.

      DELETE tt-report.
    END. /* each tt-report */
