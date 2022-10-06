/* Mod: Ticket - 103137 (Format Change for Order No. and Job No.    */

DEFINE VARIABLE dNetprct LIKE probe.net-profit.
DEFINE VARIABLE cUsers-id AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMachine AS CHARACTER NO-UNDO .
DEFINE VARIABLE cInks AS CHARACTER NO-UNDO .
DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
DEFINE BUFFER bf-oe-rel FOR oe-rel.
         FORMAT oe-ord.due-date COLUMN-LABEL " !Due!Date"
                                FORMAT "99/99/99"
                w-data.ord-no
                cust.NAME       FORMAT "x(20)"
                w-data.comm
                w-data.item-n
                w-data.qty
                w-data.t-sqft
                v-price-per-m
                v-revenue
                v-profit
                w-data.t-tons
                v-price-per-t

             WITH NO-BOX FRAME det1000 DOWN STREAM-IO WIDTH 180.

         FORMAT oe-ord.due-date COLUMN-LABEL " !Due!Date"
                                FORMAT "99/99/99"
                w-data.ord-no
                cust.name       FORMAT "x(20)"
                w-data.comm
                w-data.item-n
                w-data.qty
                w-data.t-sqft
                v-price-per-m
                v-revenue
                v-margin
                w-data.t-tons
                v-price-per-t

             WITH NO-BOX FRAME det1000m DOWN STREAM-IO WIDTH 180.

         FORMAT oe-ord.due-date COLUMN-LABEL " !Due!Date"
                                FORMAT "99/99/99"
                w-data.ord-no
                cust.name       FORMAT "x(20)"
                w-data.comm
                w-data.item-n
                w-data.qty
                w-data.t-sqft
                v-price-per-m
                v-revenue
                v-profit
             WITH NO-BOX FRAME det1500 DOWN STREAM-IO WIDTH 180.

         FORMAT oe-ord.due-date COLUMN-LABEL " !Due!Date"
                                FORMAT "99/99/99"
                w-data.ord-no
                cust.name       FORMAT "x(20)"
                w-data.comm
                w-data.item-n
                w-data.qty
                w-data.t-sqft
                v-price-per-m
                v-revenue
                v-margin
             WITH NO-BOX FRAME det1500m DOWN STREAM-IO WIDTH 180.

         FORMAT oe-ord.due-date COLUMN-LABEL " !Due!Date"
                                FORMAT "99/99/99"
                w-data.ord-no
                cust.name       FORMAT "x(20)"
                w-data.comm
                w-data.procat
                w-data.qty
                w-data.sqft
                w-data.t-sqft
                v-price-per-m
                w-data.price
                v-revenue
                v-profit
                w-data.t-tons
                v-price-per-m
             WITH NO-BOX FRAME det2000 DOWN STREAM-IO WIDTH 180.

         FORMAT oe-ord.due-date COLUMN-LABEL " !Due!Date"
                                FORMAT "99/99/99"
                w-data.ord-no
                cust.name       FORMAT "x(20)"
                w-data.comm
                w-data.procat
                w-data.qty
                w-data.sqft
                w-data.t-sqft
                v-price-per-m
                w-data.price
                v-revenue
                v-margin
                w-data.t-tons
                v-price-per-m
             WITH NO-BOX FRAME det2000m DOWN STREAM-IO WIDTH 180.

         FORMAT oe-ord.due-date COLUMN-LABEL " !Due!Date"
                                FORMAT "99/99/99"
                w-data.ord-no
                cust.name       FORMAT "x(20)"
                w-data.comm
                w-data.procat
                w-data.qty
                w-data.sqft
                w-data.t-sqft
                v-price-per-m
                w-data.price
                v-revenue
                v-profit
             WITH NO-BOX FRAME det2500 DOWN STREAM-IO WIDTH 180.

         FORMAT oe-ord.due-date COLUMN-LABEL " !Due!Date"
                                FORMAT "99/99/99"
                w-data.ord-no
                cust.name       FORMAT "x(20)"
                w-data.comm
                w-data.procat
                w-data.qty
                w-data.sqft
                w-data.t-sqft
                v-price-per-m
                w-data.price
                v-revenue
                v-margin
             WITH NO-BOX FRAME det2500m DOWN STREAM-IO WIDTH 180.

         FORMAT oe-ord.due-date COLUMN-LABEL " !Due!Date"
                                FORMAT "99/99/99"
                w-data.ord-no
                cust.name       FORMAT "x(20)"
                w-data.comm
                w-data.procat
                w-data.qty
                oe-ordl.part-no COLUMN-LABEL " !Customer!Part Number"
                w-data.item-n
                v-price-per-m
                w-data.price
                v-revenue
                v-profit
                w-data.t-tons
                v-price-per-m
             WITH NO-BOX FRAME det3000 DOWN STREAM-IO WIDTH 180.

         FORMAT oe-ord.due-date COLUMN-LABEL " !Due!Date"
                                FORMAT "99/99/99"
                w-data.ord-no
                cust.name       FORMAT "x(20)"
                w-data.comm
                w-data.procat
                w-data.qty
                oe-ordl.part-no COLUMN-LABEL " !Customer!Part Number"
                w-data.item-n
                v-price-per-m
                w-data.price
                v-revenue
                v-margin
                w-data.t-tons
                v-price-per-m
             WITH NO-BOX FRAME det3000m DOWN STREAM-IO WIDTH 180.

         FORMAT oe-ord.due-date COLUMN-LABEL " !Due!Date"
                                FORMAT "99/99/99"
                w-data.ord-no
                cust.name       FORMAT "x(20)"
                w-data.comm
                w-data.procat
                w-data.item-n
                w-data.qty
                oe-ordl.part-no COLUMN-LABEL " !Customer!Part Number"
                v-price-per-m
                w-data.price
                v-revenue
                v-profit
             WITH NO-BOX FRAME det3500 DOWN STREAM-IO WIDTH 180.

         FORMAT oe-ord.due-date COLUMN-LABEL " !Due!Date"
                                FORMAT "99/99/99"
                w-data.ord-no
                cust.name       FORMAT "x(20)"
                w-data.comm
                w-data.procat
                w-data.item-n
                w-data.qty
                oe-ordl.part-no COLUMN-LABEL " !Customer!Part Number"
                v-price-per-m
                w-data.price
                v-revenue
                v-margin
             WITH NO-BOX FRAME det3500m DOWN STREAM-IO WIDTH 180.

FORMAT wkrecap.procat
       fgcat.dscr COLUMN-LABEL "Category Description" 
       wkrecap.num-of-ord
       wkrecap.revenue[1]
       wkrecap.t-sqft[1]
       wkrecap.price-per-m[1]
       wkrecap.revenue[2]
       wkrecap.t-sqft[2]
       wkrecap.price-per-m[2]
    HEADER "                                      ----------- Dates Selected" +
           " --------- ---------- Period to Date ----------"
    WITH FRAME f-recap DOWN NO-BOX STREAM-IO WIDTH 180.

FORMAT wkrecap.procat
       fgcat.dscr COLUMN-LABEL "Category Description" 
       wkrecap.num-of-ord
       wkrecap.revenue[1]
       wkrecap.t-sqft[1]
       wkrecap.price-per-m[1]
       wkrecap.t-tons[1]
       wkrecap.price-per-t[1]
       wkrecap.revenue[2]
       wkrecap.t-sqft[2]
       wkrecap.price-per-m[2]
       wkrecap.t-tons[2]
       wkrecap.price-per-t[2]
    HEADER "                                      --------------------- Dates Selected" +
           " ------------------- -------------------- Period to Date --------------------"
    WITH FRAME f-recap-t DOWN NO-BOX STREAM-IO WIDTH 180.


  FIND FIRST ce-ctrl {sys/look/ce-ctrlW.i} NO-LOCK.

  FIND FIRST period NO-LOCK
      WHERE period.company EQ cocode
        AND period.pst     LE tdate
        AND period.pend    GE tdate
         NO-ERROR.

  lo_trandate = IF AVAILABLE period THEN MINIMUM(fdate,period.pst) ELSE fdate.
 FOR EACH oe-ord NO-LOCK
      WHERE oe-ord.company  EQ cocode
        AND oe-ord.cust-no  GE begin_cust-no
        AND oe-ord.cust-no  LE end_cust-no
        AND (if lselected then can-find(first ttCustList where ttCustList.cust-no eq oe-ord.cust-no
        AND ttCustList.log-fld no-lock) else true)
        AND oe-ord.ord-date GE lo_trANDate
        AND oe-ord.ord-date LE tdate
        AND oe-ord.due-date GE dSDueDate
        AND oe-ord.due-date LE dEDueDate
        AND oe-ord.stat     NE "D"
      BY oe-ord.company BY oe-ord.ord-date BY oe-ord.ord-no:

     IF tb_exclude-transfer THEN
      DO:

       IF oe-ord.TYPE EQ "T" THEN
          NEXT.
       
       v-code = "".
        
       FOR EACH oe-rel FIELDS(r-no) NO-LOCK WHERE
           oe-rel.company = oe-ord.company AND 
           oe-rel.ord-no  = oe-ord.ord-no AND 
             oe-rel.s-code  = "T"
                     :
      
                 v-code = "T".
                 LEAVE.
        END.
       
       IF v-code = "T" THEN
          NEXT.
    END.  

    {custom/statusMsg.i "'Processing Order # ' + string(oe-ord.ord-no)"} 
    MAIN:
    FOR EACH oe-ordl NO-LOCK
        WHERE oe-ordl.company EQ cocode
          AND oe-ordl.ord-no  EQ oe-ord.ord-no
          AND oe-ordl.part-no GE fcpart
          AND oe-ordl.part-no LE tcpart
          AND (oe-ordl.is-a-component EQ NO OR tb_exclude-set-comps = NO),
          
        FIRST itemfg NO-LOCK
        WHERE itemfg.company EQ cocode
          AND itemfg.i-no    EQ oe-ordl.i-no
          AND itemfg.procat  GE begin_fg-cat
          AND itemfg.procat  LE end_fg-cat
          
        BREAK BY oe-ordl.line:

        FIND FIRST oe-rel NO-LOCK
            where oe-rel.company   eq cocode
            and oe-rel.ord-no    eq oe-ordl.ord-no
            and oe-rel.i-no      eq oe-ordl.i-no
            and oe-rel.line      eq oe-ordl.LINE NO-ERROR.

        IF NOT AVAIL oe-rel THEN DO:
            FIND FIRST bf-oe-ordl NO-LOCK
                WHERE bf-oe-ordl.company EQ oe-ordl.company
                AND bf-oe-ordl.ord-no  EQ oe-ordl.ord-no
                AND bf-oe-ordl.is-a-component EQ YES NO-ERROR.
            
            IF AVAIL bf-oe-ordl THEN
                FIND FIRST bf-oe-rel NO-LOCK
                   where bf-oe-rel.company   eq bf-oe-ordl.company
                   and bf-oe-rel.ord-no    eq bf-oe-ordl.ord-no
                   and bf-oe-rel.i-no      eq bf-oe-ordl.i-no
                   and bf-oe-rel.line      eq bf-oe-ordl.LINE NO-ERROR.

            IF AVAIL bf-oe-rel AND NOT(bf-oe-rel.spare-char-1   ge begin_shipfrom
              and bf-oe-rel.spare-char-1   le end_shipfrom) THEN NEXT.
            ELSE IF NOT AVAIL bf-oe-rel AND NOT lOrdWithNoRel THEN NEXT.
            
        END.
        ELSE DO:   
            IF AVAIL oe-rel AND NOT(oe-rel.spare-char-1   ge begin_shipfrom
            and oe-rel.spare-char-1   le end_shipfrom) THEN NEXT.
            ELSE IF NOT AVAIL oe-rel AND NOT lOrdWithNoRel THEN NEXT.

        END.
      
      v-exclude = YES.
      DO i = 1 TO 3:
        IF v-exclude                 AND
           oe-ordl.s-man[i] GE fsman AND
           oe-ordl.s-man[i] LE tsman THEN v-exclude = NO.
      END. /* do i.. */

      IF v-exclude THEN NEXT.

      /* At this point we have either 1, 2 or 3 valid salesman, in any  */
      /* combination of the array. */
      
      v-misc = FALSE.
      DO i = 1 TO 3:
         
        IF v-misc THEN LEAVE.

        IF oe-ordl.s-man[i] LT fsman OR
           oe-ordl.s-man[i] GT tsman THEN NEXT.

        /* if no salesman number then assign to misc, ie, blank no */
        IF i EQ 1               AND
           oe-ordl.s-man[1] EQ "" AND
           oe-ordl.s-man[2] EQ "" AND
           oe-ordl.s-man[3] EQ "" THEN v-sman = "MISC".

        ELSE    /* if blank salesman # then ignore */
        IF oe-ordl.s-man[i] EQ "" THEN NEXT.

        /* There must be at least 1 salesman in either pos'n 1, 2 or 3 */
        ELSE v-sman = oe-ordl.s-man[i].

        IF oe-ord.ord-date GE fdate AND
           oe-ord.ord-date LE tdate THEN do:
             
          CREATE tt-report.
          ASSIGN 
           tt-report.term-id = ""
           tt-report.key-01  = v-sman
           tt-report.key-02 = IF tb_sortby THEN
                                 STRING(oe-ord.ord-no,">>>>>>>>>>") ELSE ""
           tt-report.key-03  = STRING(i,"9")
           tt-report.key-04  = IF AVAIL oe-rel THEN STRING(oe-rel.spare-char-1) ELSE IF AVAIL bf-oe-rel THEN STRING(bf-oe-rel.spare-char-1) ELSE ""
           tt-report.rec-id  = RECID(oe-ordl)
                                     .           
        END.    /* date in selected period */
        
        RUN fg/GetFGArea.p (ROWID(itemfg), "SF", OUTPUT v-sqft).
  
        ASSIGN 
         v-pct  = oe-ordl.s-pct[i] / 100
         v-qty  = oe-ordl.qty * v-pct
         v-sqft = v-sqft * v-qty / 1000
         v-tons = itemfg.weight-100 * v-qty / 100 / 2000
         v-amt  = oe-ordl.t-price * v-pct.
        
        FIND FIRST wkrecap
            WHERE wkrecap.procat EQ IF AVAILABLE itemfg THEN itemfg.procat ELSE ?
            NO-ERROR.
        IF NOT AVAILABLE wkrecap THEN DO:
          CREATE wkrecap.
          ASSIGN 
           wkrecap.procat     = IF AVAILABLE itemfg THEN itemfg.procat ELSE ?
           wkrecap.num-of-ord = wkrecap.num-of-ord + 1.
        END.
        
        ELSE wkrecap.num-of-ord = wkrecap.num-of-ord + 1.

        j = IF oe-ord.ord-date GE fdate AND
               oe-ord.ord-date LE tdate THEN 1 ELSE 2.

        k = IF AVAILABLE period AND oe-ord.ord-date GE period.pst  AND
               oe-ord.ord-date LE period.pend THEN 2 ELSE 1.

        IF j LE k THEN
        DO ii = j TO k:
          ASSIGN
           wkrecap.t-sqft[ii]  = wkrecap.t-sqft[ii] + v-sqft
           wkrecap.t-tons[ii]  = wkrecap.t-tons[ii] + v-tons
           wkrecap.revenue[ii] = wkrecap.revenue[ii] + v-amt.
        END.
      END. /* do i = 1 to 3... */

      IF oe-ord.ord-date NE mdate THEN DO:
        mdate = oe-ord.ord-date.
         
        IF oe-ord.ord-date GE fdate AND
           oe-ord.ord-date LE tdate THEN
          v-per-days[1] = v-per-days[1] + 1.
          
        IF AVAILABLE period AND oe-ord.ord-date GE period.pst  AND
           oe-ord.ord-date LE period.pend THEN
          v-per-days[2] = v-per-days[2] + 1.
      END.
    END.

    IF p-m-chg THEN 
    FOR EACH oe-ordm NO-LOCK
        WHERE oe-ordm.company EQ cocode
          AND oe-ordm.ord-no  EQ oe-ord.ord-no :

      v-exclude = YES.
      DO i = 1 TO 3:
        IF v-exclude                 AND
           oe-ordm.s-man[i] GE fsman AND
           oe-ordm.s-man[i] LE tsman THEN v-exclude = NO.
      END. /* do i.. */

      IF v-exclude THEN NEXT.

      /* At this point we have either 1, 2 or 3 valid salesman, in any  */
      /* combination of the array. */
      
      v-misc = FALSE.
      DO i = 1 TO 3:
        IF v-misc THEN LEAVE.

        IF oe-ordm.s-man[i] LT fsman OR
           oe-ordm.s-man[i] GT tsman THEN NEXT.

        /* if no salesman number THEN assign to misc, ie, blank no */
        IF i EQ 1                 AND
           oe-ordm.s-man[1] EQ "" AND
           oe-ordm.s-man[2] EQ "" AND
           oe-ordm.s-man[3] EQ "" THEN v-sman = "MISC".

        ELSE   /* if blank salesman # then ignore */
        IF oe-ordm.s-man[i] EQ "" THEN NEXT.

        /* There must be at least 1 salesman in either pos'n 1, 2 or 3 */
        ELSE v-sman = oe-ordm.s-man[i].

        IF oe-ord.ord-date GE fdate AND
           oe-ord.ord-date LE tdate THEN DO:
               
          CREATE tt-report.
          ASSIGN 
           tt-report.term-id = ""
           tt-report.key-01  = v-sman
           tt-report.key-02  = IF tb_sortby THEN
                                 STRING(oe-ord.ord-no,">>>>>>>>>>") ELSE ""
           tt-report.key-03  = STRING(i,"9")
           tt-report.key-04  = IF AVAIL oe-rel THEN STRING(oe-rel.spare-char-1) ELSE IF AVAIL bf-oe-rel THEN STRING(bf-oe-rel.spare-char-1) ELSE ""
           tt-report.rec-id  = RECID(oe-ordm).
        END.
        
        ASSIGN 
         v-pct = oe-ordm.s-pct[i] / 100
         v-amt = oe-ordm.amt * v-pct.

        FIND FIRST wkrecap WHERE wkrecap.procat EQ "P/M" NO-ERROR.
        IF not AVAILABLE wkrecap THEN DO:
          CREATE wkrecap.
          ASSIGN
           wkrecap.procat     = "P/M"
           wkrecap.num-of-ord = wkrecap.num-of-ord + 1.
        END.
            
        ELSE wkrecap.num-of-ord = wkrecap.num-of-ord + 1.

        j = IF oe-ord.ord-date GE fdate AND
               oe-ord.ord-date LE tdate THEN 1 ELSE 2.

        k = IF AVAILABLE period AND oe-ord.ord-date GE period.pst  AND
               oe-ord.ord-date LE period.pend THEN 2 ELSE 1.

        /* We cannot disturb loop variable i from within loop,
           so use ii: */
        IF j LE k THEN
        DO ii = j TO k:
          wkrecap.revenue[ii] = wkrecap.revenue[ii] + v-amt.
        END.
      END.
    END.



  END.  /* for each oe-ord */

   FOR EACH tt-report WHERE tt-report.term-id EQ ""
      BREAK BY tt-report.key-01 BY tt-report.key-02:

        FIND FIRST oe-ordm NO-LOCK
          WHERE RECID(oe-ordm) EQ tt-report.rec-id
           NO-ERROR.
      IF AVAILABLE oe-ordm THEN do:
        FIND FIRST oe-ord OF oe-ordm NO-LOCK.
        ASSIGN 
         i     = INTEGER(tt-report.key-03)
         v-pct = oe-ordm.s-pct[i] / 100
         v-amt = oe-ordm.amt * v-pct.

         ASSIGN 
             v-revenue     = v-amt
             v-profit      = (v-revenue - (oe-ordm.cost * v-pct)) / v-revenue * 100
             .

      END.
      ELSE DO:
        FIND FIRST oe-ordl
            WHERE RECID(oe-ordl) EQ tt-report.rec-id
            NO-LOCK NO-ERROR.
        ASSIGN 
           i      = INTEGER(tt-report.key-03)
           v-pct  = oe-ordl.s-pct[i] / 100
           v-qty  = oe-ordl.qty * v-pct
           v-amt  = oe-ordl.t-price * v-pct .
             qm              = oe-ordl.qty / 1000 .
        ASSIGN 
             v-revenue     = v-amt
             v-profit      = (v-revenue - (oe-ordl.cost * qm)) / v-revenue * 100
             .

      END.

      IF v-profit      EQ ? THEN v-profit      = 0.
      
      IF prt-sqft THEN DO:       
   
       /*==== new with selectable columns ====*/
        IF tb_under% AND tb_over% THEN DO:
            IF v-profit GE fUnder% AND v-profit LE fOver% THEN DELETE tt-report NO-ERROR .
        END.
        ELSE IF tb_under% AND NOT tb_over% THEN DO:
            IF v-profit GE fUnder% THEN DELETE tt-report NO-ERROR.
        END.
        ELSE IF tb_over% AND NOT tb_under% THEN DO:
            IF v-profit GE fOver% THEN DELETE tt-report NO-ERROR.
        END.
      END.  /* prt-sqft then do */

      ELSE  DO:
          IF tb_under% AND v-profit GT fUnder% THEN DELETE tt-report NO-ERROR.
          IF tb_over% AND v-profit LT fOver% THEN DELETE tt-report NO-ERROR.

      END. /* end of else do prt-sqft */

      
  END.

  FOR EACH tt-report WHERE tt-report.term-id EQ ""
      BREAK BY tt-report.key-01 BY tt-report.key-02:
      
    {oe/rep/oe-sman2.i}
    
    FIND FIRST oe-ordl NO-LOCK WHERE RECID(oe-ordl) EQ tt-report.rec-id NO-ERROR.

    IF AVAILABLE oe-ord THEN
        {custom/statusMsg.i "'Processing Order # ' + string(oe-ord.ord-no)"}

   IF FIRST (tt-report.key-01) THEN VIEW FRAME r-top. 


    IF FIRST-OF(tt-report.key-01) THEN DO:
      FIND FIRST sman NO-LOCK
          WHERE sman.company EQ cocode
            AND sman.sman    EQ w-data.sman
           NO-ERROR.
            
      v-sname = IF AVAILABLE sman THEN sman.sname
                ELSE "* NOT IN SALES REP FILE *".
        PUT SKIP(1)
            "Sales Rep: "
            w-data.sman
            " - "
            v-sname
            SKIP(1).
    END.
    

    FIND FIRST oe-ord NO-LOCK
        WHERE oe-ord.company EQ cocode
          AND oe-ord.ord-no  EQ w-data.ord-no
         NO-ERROR.
    FIND cust OF oe-ord NO-LOCK NO-ERROR.

        ASSIGN  dNetprct = 0.
         IF AVAIL oe-ordl THEN
         ASSIGN dNetprct = w-data.comm .
        
    ASSIGN 
     v-revenue     = w-data.revenue
     v-price-per-m = v-revenue / w-data.t-sqft
     v-price-per-t = v-revenue / w-data.t-tons
     v-profit      = (v-revenue - w-data.cost) / v-revenue * 100
     v-margin      = w-data.margin.

    IF v-price-per-m EQ ? THEN v-price-per-m = 0.
    IF v-price-per-t EQ ? THEN v-price-per-t = 0.
    IF v-profit      EQ ? THEN v-profit      = 0.
    IF v-margin      EQ ? THEN v-margin      = 0.
    
    ACCUMULATE 
     w-data.t-sqft (TOTAL BY tt-report.key-01)
     w-data.t-tons (TOTAL BY tt-report.key-01)
     v-revenue     (TOTAL BY tt-report.key-01)
     w-data.cost   (TOTAL BY tt-report.key-01).

    cUsers-id = "".
    cMachine = "" .
    cInks = "" .
    IF AVAIL oe-ord THEN  
     FIND FIRST job NO-LOCK
          WHERE job.company EQ cocode
            AND job.job-no EQ oe-ord.job-no
            AND job.job-no2 EQ oe-ord.job-no2 NO-ERROR.
   
     cUsers-id = IF AVAIL job AND job.csrUser_id NE "" THEN job.csrUser_id ELSE IF AVAIL oe-ord THEN oe-ord.csrUser_id ELSE "".
     IF AVAILABLE oe-ordl THEN do:
        FIND FIRST itemfg NO-LOCK
            WHERE itemfg.company EQ cocode
            AND itemfg.i-no    EQ oe-ordl.i-no NO-ERROR.

        FIND FIRST eb NO-LOCK WHERE eb.company EQ cocode
            AND eb.est-no  EQ oe-ordl.est-no
            AND eb.stock-no EQ oe-ordl.i-no NO-ERROR .
    END. /* avail oe-ordl  */
    
    IF AVAIL oe-ord AND AVAIL job AND job.job-no NE "" THEN 
    cMachine = fGetRoutingForJob(INPUT job.job, INPUT job.job-no,INPUT job.job-no2).
    
    IF AVAIL oe-ord AND AVAIL job AND job.job-no NE "" AND AVAIL eb THEN
    cInks    = fGetInksForJob(INPUT job.job,INPUT ROWID(eb)).     

    IF AVAIL oe-ord THEN DO:
        IF oe-ord.TYPE = "T" AND oe-ord.pord-no GT 0 THEN
            cPrevOrder = STRING(oe-ord.pord-no).
        cPrevOrder = IF oe-ord.po-no2 NE "" THEN oe-ord.po-no2
            ELSE STRING(oe-ord.pord-no).
    END.

    IF prt-sqft THEN do:       
       /*==== new with selectable columns ====*/
        IF tb_under% AND tb_over% THEN DO:
            IF v-profit GE fUnder% AND v-profit LE fOver% THEN NEXT.
        END.
        ELSE IF tb_under% AND NOT tb_over% THEN DO:
            IF v-profit GE fUnder% THEN NEXT.
        END.
        ELSE IF tb_over% AND NOT tb_under% THEN DO:
            IF v-profit LE fOver% THEN NEXT.
        END.

        c-result = oe-ord.stat .
        RUN oe/getStatusDesc.p( INPUT oe-ord.stat, OUTPUT cResult) .
        IF cResult NE "" THEN
            c-result  = cResult .

       IF AVAILABLE oe-ord THEN
       BUFFER boe-ord:FIND-BY-ROWID(ROWID(oe-ord), NO-LOCK) .
       IF AVAILABLE oe-ordl THEN
       BUFFER boe-ordl:FIND-BY-ROWID(ROWID(oe-ordl), NO-LOCK) .
       IF AVAILABLE cust THEN
       BUFFER bcust:FIND-BY-ROWID(ROWID(cust), NO-LOCK) .
       BUFFER bw-data:FIND-BY-ROWID(ROWID(w-data), NO-LOCK) .
       ASSIGN cDisplay = ""
           cTmpField = ""
           cVarValue = ""
           cExcelDisplay = ""
           cExcelVarValue = "".    

       DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
         cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
         
         IF INDEX(cTmpField,".") GT 0 AND LOOKUP(cTmpField, "oe-ord.due-date,oe-ord.ord-date") EQ 0 THEN DO:
                 cFieldName = cTmpField.
                 cTmpField = SUBSTRING(cTmpField,INDEX(cTmpField,".") + 1).
                 IF cFieldName BEGINS "oe-ordl" THEN hField = IF AVAILABLE boe-ordl THEN BUFFER boe-ordl:BUFFER-FIELD(cTmpField) ELSE ?.
                 ELSE IF cFieldName BEGINS "oe-ord" THEN hField = BUFFER boe-ord:BUFFER-FIELD(cTmpField).
                 ELSE IF cFieldName BEGINS "cust" THEN hField = IF AVAILABLE bcust THEN BUFFER bcust:BUFFER-FIELD(cTmpField) ELSE ?.
                 ELSE hField = BUFFER bw-data:BUFFER-FIELD(cTmpField).
                 IF hField <> ? THEN DO:                 
                     cTmpField = SUBSTRING(GetFieldValue(hField),1,INTEGER(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength))).
                     IF ENTRY(i,cSelectedList) = "Job#" THEN
                        cTmpField = IF cTmpField NE "" THEN TRIM(STRING(DYNAMIC-FUNCTION('sfFormat_JobFormatWithHyphen', cTmpField, fg-rcpth.job-no2))) ELSE "".                  
                     
                     cDisplay = cDisplay + cTmpField + 
                               FILL(" ",INTEGER(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cTmpField))
                               .
                     cExcelDisplay = cExcelDisplay + QUOTER(DYNAMIC-FUNCTION("FormatForCSV" IN hdOutputProcs, GetFieldValue(hField))) + ",".
                 END.
                 ELSE DO:
                    cTmpField = substring(cFieldName,1,INTEGER(ENTRY( getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength) ) ).                  
                    cDisplay = cDisplay + FILL(" ",INTEGER(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 ).
                    cExcelDisplay = cExcelDisplay + QUOTER(" ")  /*GetFieldValue(hField))*/ + ",".
                 END.
         END.
         ELSE DO:       
            CASE cTmpField: 
                 WHEN "price"  THEN cVarValue = STRING(w-data.price,"->>,>>>.99") .
                 WHEN "t-sqft" THEN cVarValue = STRING(w-data.t-sqft,"->,>>>.999"). 
                 WHEN "v-price-per-m" THEN cVarValue = STRING(v-price-per-m,"->>,>>9.99").
                 WHEN "v-revenue" THEN cVarValue = IF prt-profit THEN STRING(v-revenue,"->,>>>,>>9.99") ELSE "".
                 WHEN "v-margin" THEN cVarValue = STRING(v-margin,"->>,>>9.9").
                 WHEN "t-tons" THEN cVarValue = STRING(w-data.t-tons,"->,>>>.9").
                 WHEN "v-profit" THEN cVarValue = IF prt-profit THEN STRING(v-profit,"->>,>>9.9") ELSE "".
                 WHEN "v-price-per-t" THEN cVarValue = STRING(v-price-per-t,"->>>,>>>,>>9.99").
                 WHEN "cust-po" THEN cVarValue = IF AVAILABLE oe-ordl AND oe-ordl.cust-no NE "" THEN STRING(oe-ordl.po-no,"x(15)") ELSE STRING(oe-ord.po-no,"x(15)") . /* ticket 14966*/
                 WHEN "die-no" THEN cVarValue = IF AVAILABLE itemfg AND itemfg.die-no NE "" THEN STRING(itemfg.die-no,"x(15)") ELSE IF AVAILABLE eb THEN STRING(eb.die-no,"x(15)") ELSE "" . /* ticket 16188*/
                 WHEN "v-net-prct" THEN cVarValue = STRING(dNetprct,"->>9.99"). 
                 WHEN "csrUser_id" THEN cVarValue = STRING(cUsers-id,"X(8)").
                 WHEN "ack-date" THEN cVarValue = IF AVAIL oe-ord AND oe-ord.ack-prnt-date NE ? THEN STRING(oe-ord.ack-prnt-date) ELSE "".
                 WHEN "ship-from" THEN cVarValue = STRING(w-data.cShip-from,"X(9)").
                 WHEN "v-mach" THEN cVarValue = STRING(cMachine,"X(30)").
                 WHEN "v-ink" THEN cVarValue = STRING(cInks,"X(40)").
                 WHEN "print-sheet" THEN cVarValue =  IF AVAIL itemfg THEN STRING(itemfg.plate-no,"X(20)") ELSE "".
                 WHEN "full-cost" THEN cVarValue = IF prt-profit THEN STRING(oe-ordl.spare-dec-1,"->>>>>>>9.99") ELSE "" .                                                                                               
                 WHEN "v-cost" THEN cVarValue = IF prt-profit AND oe-ordl.cost NE ? THEN STRING(oe-ordl.cost,"->>>,>>>,>>9.99") ELSE "".
                 WHEN "v-t-cost" THEN cVarValue = IF prt-profit AND oe-ordl.t-cost NE ? THEN STRING(oe-ordl.t-cost,"->>,>>>,>>9.99") ELSE "".
                 WHEN "status" THEN cVarValue = STRING(c-result,"x(20)") . 
                 WHEN "po-recvdt" THEN cVarValue = IF AVAIL oe-ord AND oe-ord.poReceivedDate NE ? THEN STRING(oe-ord.poReceivedDate) ELSE "".
                 WHEN "prev-order" THEN cVarValue = STRING(cPrevOrder,"x(8)") .
		         WHEN "approved-date" THEN cVarValue = IF AVAIL oe-ord AND oe-ord.approved-date NE ? THEN STRING(oe-ord.approved-date) ELSE "".
		         WHEN "oe-ord.due-date" THEN cVarValue = IF AVAIL oe-ord AND oe-ord.due-date NE ? THEN STRING(oe-ord.due-date) ELSE "".
		         WHEN "oe-ord.ord-date" THEN cVarValue = IF AVAIL oe-ord AND oe-ord.ord-date NE ? THEN STRING(oe-ord.ord-date) ELSE "".
            END CASE.
            IF lookup(cTmpField,"v-profit,v-revenue,full-cost,v-cost,v-t-cost") GT 0 AND NOT prt-profit THEN NEXT.
            
            IF  cTmpField = "oe-ord.due-date" THEN
                 cExcelVarValue = IF AVAIL oe-ord AND oe-ord.due-date NE ? THEN DYNAMIC-FUNCTION("sfFormat_Date",oe-ord.due-date) ELSE "".
            ELSE IF  cTmpField = "oe-ord.ord-date" THEN
                 cExcelVarValue = IF AVAIL oe-ord AND oe-ord.ord-date NE ? THEN DYNAMIC-FUNCTION("sfFormat_Date",oe-ord.ord-date) ELSE "".
            ELSE IF  cTmpField = "approved-date" THEN
                 cExcelVarValue = IF AVAIL oe-ord AND oe-ord.approved-date NE ? THEN DYNAMIC-FUNCTION("sfFormat_Date",oe-ord.approved-date) ELSE "".
            ELSE IF  cTmpField = "ack-date" THEN
                 cExcelVarValue = IF AVAIL oe-ord AND oe-ord.ack-prnt-date NE ? THEN DYNAMIC-FUNCTION("sfFormat_Date",oe-ord.ack-prnt-date) ELSE "".
            ELSE cExcelVarValue = cVarValue.
            cDisplay = cDisplay + cVarValue +
                       FILL(" ",INTEGER(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
            cExcelDisplay = cExcelDisplay + quoter(DYNAMIC-FUNCTION("FormatForCSV" IN hdOutputProcs,cExcelVarValue)) + ",".            
         END.
      END.
      PUT UNFORMATTED cDisplay SKIP.
      IF rd-dest = 3 THEN DO:
       cExcelDisplay = '"' + w-data.sman + '",' +
                       '"' + v-sname    + '",' +
                      cExcelDisplay.
       PUT STREAM excel UNFORMATTED  
               cExcelDisplay SKIP.
      END.
      /*===== end of new ===== */
      
      IF LAST-OF(tt-report.key-01) THEN DO:
          
        ASSIGN 
         v-price-per-m = (ACCUM TOTAL BY tt-report.key-01 v-revenue) /
                         (ACCUM TOTAL BY tt-report.key-01 w-data.t-sqft)
         v-price-per-t = (ACCUM TOTAL BY tt-report.key-01 v-revenue) /
                         (ACCUM TOTAL BY tt-report.key-01 w-data.t-tons)
         v-profit      = ((ACCUM TOTAL BY tt-report.key-01 v-revenue) -
                          (ACCUM TOTAL BY tt-report.key-01 w-data.cost)) /
                          (ACCUM TOTAL BY tt-report.key-01 v-revenue) * 100
         tot-sqft        = (ACCUM TOTAL BY tt-report.key-01 w-data.t-sqft) 
         tot-renv        = (ACCUM TOTAL BY tt-report.key-01 v-revenue)
         tot-ton         = (ACCUM TOTAL BY tt-report.key-01 w-data.t-tons) .

        IF v-price-per-m EQ ? THEN v-price-per-m = 0.
        IF v-price-per-t EQ ? THEN v-price-per-t = 0.
        IF v-profit      EQ ? THEN v-profit      = 0.
        IF tot-sqft      EQ ? THEN tot-sqft      = 0.
        IF tot-renv      EQ ? THEN tot-renv      = 0.
        IF tot-ton       EQ ? THEN tot-ton       = 0.
      
      
        IF tb_rep-tot THEN DO:
         PUT    SKIP  str-line SKIP .
          ASSIGN cDisplay = ""
           cTmpField = ""
           cVarValue = ""
           cExcelDisplay = ""
           cExcelVarValue = "".    

         DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
           cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
           
              CASE cTmpField:
                   WHEN "oe-ord.due-date" THEN cVarValue = "" .
                   WHEN "w-data.ord-no" THEN cVarValue = "" .
                   WHEN "cust.cust-no" THEN cVarValue = "" .
                   WHEN "cust.name" THEN cVarValue = "" .
                   WHEN "w-data.comm" THEN cVarValue = "" .
                   WHEN "w-data.procat" THEN cVarValue = "" .
                   WHEN "w-data.item-n" THEN cVarValue = "" .
                   WHEN "w-data.qty" THEN cVarValue = "" .
                   WHEN "w-data.sqft" THEN cVarValue = "" .
                   WHEN "price" THEN cVarValue = "" .
                   WHEN "oe-ordl.i-no" THEN cVarValue = "" .
                   WHEN "oe-ordl.ord-date" THEN cVarValue = "" .
                   WHEN "cust-po" THEN cVarValue = "" . /* ticket 14966*/
                   WHEN "die-no" THEN cVarValue =  "" . /* ticket 16188*/
                   WHEN "t-sqft" THEN cVarValue = STRING(tot-sqft,"->,>>>.999").
                   WHEN "v-price-per-m" THEN cVarValue = STRING(v-price-per-m,"->>,>>9.99").
                   WHEN "v-revenue" THEN cVarValue = IF prt-profit THEN STRING(tot-renv,"->,>>>,>>9.99") ELSE "".
                   WHEN "v-margin" THEN cVarValue = "".
                   WHEN "t-tons" THEN cVarValue = STRING(tot-ton,"->,>>>.9").
                   WHEN "v-profit" THEN cVarValue = IF prt-profit THEN STRING(v-profit,"->>,>>9.9") ELSE "".
                   WHEN "v-price-per-t" THEN cVarValue = STRING(v-price-per-t,"->>,>>9.99").
                   WHEN "v-net-prct" THEN cVarValue = "". 
                   WHEN "w-data.shp-qty" THEN cVarValue = "" .
                   WHEN "csrUser_id" THEN cVarValue = "" .
                   WHEN "ack-date" THEN cVarValue = "".
                   WHEN "oe-ordl.pr-uom" THEN cVarValue = "".
                   WHEN "Ship-from" THEN cVarValue = "".
                   WHEN "v-mach" THEN cVarValue = "".
                   WHEN "v-ink" THEN cVarValue = "" .
                   WHEN "print-sheet" THEN cVarValue =  "".
                   WHEN "v-cost" THEN cVarValue = "" .
                   WHEN "v-t-cost" THEN cVarValue = "".
                   WHEN "full-cost" THEN cVarValue = "" .
                   WHEN "status" THEN cVarValue = "" .
                   WHEN "po-recvdt" THEN cVarValue = "".
                   WHEN "prev-order" THEN cVarValue = "". 
		           WHEN "approved-date" THEN cVarValue = "" .
              END CASE.
              IF lookup(cTmpField,"v-profit,v-revenue,full-cost,v-cost,v-t-cost") GT 0 AND NOT prt-profit THEN NEXT.
              cExcelVarValue = cVarValue.
              cDisplay = cDisplay + cVarValue +
                         FILL(" ",INTEGER(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
              cExcelDisplay = cExcelDisplay + QUOTER(cExcelVarValue) + ",".            
          
         END.
         PUT UNFORMATTED "       * REP TOTALS *" SUBSTRING(cDisplay,22,300) SKIP.
         IF rd-dest = 3 THEN DO:
             cExcelDisplay = '"' + "" + '",' +
                 '"' + ""    + '",' +
                 cExcelDisplay.
             PUT STREAM excel UNFORMATTED  
                 "REP TOTALS " + SUBSTRING(cExcelDisplay,3,300) SKIP.
         END.
        END. /*if tb_rep-tot then */
      END.    /* last-of sman */ 

      IF LAST(tt-report.key-01) THEN DO:
          
        ASSIGN 
         v-price-per-m = (ACCUM TOTAL v-revenue) /
                         (ACCUM TOTAL w-data.t-sqft)
         v-price-per-t = (ACCUM TOTAL v-revenue) /
                         (ACCUM TOTAL w-data.t-tons)
         v-profit      = ((ACCUM TOTAL v-revenue) -
                          (ACCUM TOTAL w-data.cost)) /
                         (ACCUM TOTAL v-revenue) * 100.

        IF v-price-per-m EQ ? THEN v-price-per-m = 0.
        IF v-price-per-t EQ ? THEN v-price-per-t = 0.
        IF v-profit      EQ ? THEN v-profit      = 0.

        PUT    SKIP  str-line SKIP .
          ASSIGN cDisplay = ""
           cTmpField = ""
           cVarValue = ""
           cExcelDisplay = ""
           cExcelVarValue = "".    

          DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
           cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
           
              CASE cTmpField:
                   WHEN "oe-ord.due-date" THEN cVarValue = "" .
                   WHEN "w-data.ord-no" THEN cVarValue = "" .
                   WHEN "cust.cust-no" THEN cVarValue = "" .
                   WHEN "cust.name" THEN cVarValue = "" .
                   WHEN "w-data.comm" THEN cVarValue = "" .
                   WHEN "w-data.procat" THEN cVarValue = "" .
                   WHEN "w-data.item-n" THEN cVarValue = "" .
                   WHEN "w-data.qty" THEN cVarValue = "" .
                   WHEN "w-data.sqft" THEN cVarValue = "" .
                   WHEN "price" THEN cVarValue = "" .
                   WHEN "oe-ordl.i-no" THEN cVarValue = "" .
                   WHEN "oe-ordl.ord-date" THEN cVarValue = "" .
                   WHEN "cust-po" THEN cVarValue = "" . /* ticket 14966*/
                   WHEN "die-no" THEN cVarValue =  "" . /* ticket 16188*/
                   WHEN "t-sqft" THEN cVarValue = STRING((ACCUM TOTAL w-data.t-sqft),"->,>>>.999"). 
                   WHEN "v-price-per-m" THEN cVarValue = STRING(v-price-per-m,"->>,>>9.99"). 
                   WHEN "v-revenue" THEN cVarValue = IF prt-profit THEN STRING((ACCUM TOTAL v-revenue),"->,>>>,>>9.99") ELSE "".
                   WHEN "v-margin" THEN cVarValue = "".
                   WHEN "t-tons" THEN cVarValue = STRING((ACCUM TOTAL w-data.t-tons),"->,>>>.9").
                   WHEN "v-profit" THEN cVarValue = IF prt-profit THEN STRING(v-profit,"->>,>>9.9") ELSE "".
                   WHEN "v-price-per-t" THEN cVarValue = STRING(v-price-per-t,"->>,>>9.99").
                   WHEN "v-net-prct" THEN cVarValue = "". 
                   WHEN "w-data.shp-qty" THEN cVarValue = "" .
                   WHEN "csrUser_id" THEN cVarValue = "" .
                   WHEN "ack-date" THEN cVarValue = "".
	               WHEN "oe-ordl.pr-uom" THEN cVarValue = "".
                   WHEN "Ship-from" THEN cVarValue = "".
                   WHEN "v-mach" THEN cVarValue = "".
                   WHEN "v-ink" THEN cVarValue = "" .
                   WHEN "print-sheet" THEN cVarValue =  "".
		           WHEN "v-cost" THEN cVarValue = "" .
                   WHEN "v-t-cost" THEN cVarValue = "".
                   WHEN "full-cost" THEN cVarValue = "" .
                   WHEN "status" THEN cVarValue = "" .
                   WHEN "po-recvdt" THEN cVarValue = "".
                   WHEN "prev-order" THEN cVarValue = "". 
		           WHEN "approved-date" THEN cVarValue = "" .
              END CASE.
              IF lookup(cTmpField,"v-profit,v-revenue,full-cost,v-cost,v-t-cost") GT 0 AND NOT prt-profit THEN NEXT.
              cExcelVarValue = cVarValue.
              cDisplay = cDisplay + cVarValue +
                         FILL(" ",INTEGER(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
              cExcelDisplay = cExcelDisplay + QUOTER(cExcelVarValue) + ",".            
          
         END.
         PUT UNFORMATTED "       ** COMPANY TOTALS **   " STRING(v-n-lines,">>>>9") " Line Items" SUBSTRING(cDisplay,47,300) SKIP.
      END.
      
    END.  /* end of if prt-sqft */      
    ELSE DO:         
      
         /*==== new with selectable columns ====*/
       IF tb_under% AND v-profit > fUnder% THEN NEXT.
       IF tb_over% AND v-profit < fOver% THEN NEXT.

        c-result = oe-ord.stat .
        RUN oe/getStatusDesc.p( INPUT oe-ord.stat, OUTPUT cResult) .
        IF cResult NE "" THEN
            c-result  = cResult .

       IF AVAILABLE oe-ord THEN
       BUFFER boe-ord:FIND-BY-ROWID(ROWID(oe-ord), NO-LOCK) .
       IF AVAILABLE oe-ordl THEN
       BUFFER boe-ordl:FIND-BY-ROWID(ROWID(oe-ordl), NO-LOCK) .
       IF AVAILABLE cust THEN
       BUFFER bcust:FIND-BY-ROWID(ROWID(cust), NO-LOCK) .
       BUFFER bw-data:FIND-BY-ROWID(ROWID(w-data), NO-LOCK) .
       ASSIGN cDisplay = ""
           cTmpField = ""
           cVarValue = ""
           cExcelDisplay = ""
           cExcelVarValue = "".    

       DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
         cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
         IF INDEX(cTmpField,".") > 0 AND LOOKUP(cTmpField, "oe-ord.due-date,oe-ord.ord-date") EQ 0 THEN DO:
                 cFieldName = cTmpField.
                 cTmpField = SUBSTRING(cTmpField,INDEX(cTmpField,".") + 1).
                 IF cFieldName BEGINS "oe-ordl" THEN hField = IF AVAILABLE boe-ordl THEN BUFFER boe-ordl:BUFFER-FIELD(cTmpField) ELSE ?.
                 ELSE IF cFieldName BEGINS "oe-ord" THEN hField = BUFFER boe-ord:BUFFER-FIELD(cTmpField).
                 ELSE IF cFieldName BEGINS "cust" THEN hField = IF AVAILABLE bcust THEN BUFFER bcust:BUFFER-FIELD(cTmpField) ELSE ?.
                 ELSE hField = BUFFER bw-data:BUFFER-FIELD(cTmpField).
                 IF hField NE ? THEN DO:                 
                     cTmpField = SUBSTRING(GetFieldValue(hField),1,INTEGER(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength))).
                     IF ENTRY(i,cSelectedList) = "Job#" THEN
                        cTmpField = IF cTmpField <> "" THEN TRIM(STRING(DYNAMIC-FUNCTION('sfFormat_JobFormatWithHyphen', cTmpField, fg-rcpth.job-no2))) ELSE "".                  
                     
                     cDisplay = cDisplay + cTmpField + 
                               FILL(" ",INTEGER(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cTmpField))
                               .
                     cExcelDisplay = cExcelDisplay + QUOTER(DYNAMIC-FUNCTION("FormatForCSV" IN hdOutputProcs, GetFieldValue(hField))) + ",".
                 END.
                 ELSE DO:
                    cTmpField = SUBSTRING(cFieldName,1,INTEGER(ENTRY( getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength) ) ).                  
                    cDisplay = cDisplay + FILL(" ",INTEGER(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 ).
                    cExcelDisplay = cExcelDisplay + QUOTER(" ")  /*GetFieldValue(hField))*/ + ",".
                 END.
         END.
         ELSE DO:       
            CASE cTmpField: 
                WHEN "price"  THEN cVarValue = STRING(w-data.price,"->>,>>>.99") .
                WHEN "t-sqft" THEN cVarValue = STRING(w-data.t-sqft,"->,>>>.999").
                WHEN "v-price-per-m" THEN cVarValue = STRING(v-price-per-m,"->>,>>9.99").
                WHEN "v-revenue" THEN cVarValue = IF prt-profit THEN STRING(v-revenue,"->,>>>,>>9.99") ELSE "".
                WHEN "v-margin" THEN cVarValue = STRING(v-margin,"->>,>>9.9").
                WHEN "t-tons" THEN cVarValue = STRING(w-data.t-tons,"->,>>>.9").
                WHEN "v-profit" THEN cVarValue = IF prt-profit THEN STRING(v-profit,"->>,>>9.9") ELSE "".
                WHEN "v-price-per-t" THEN cVarValue = STRING(v-price-per-t,"->>,>>9.99").
                WHEN "cust-po" THEN cVarValue = IF AVAILABLE oe-ordl AND oe-ordl.cust-no NE "" THEN STRING(oe-ordl.po-no,"x(15)") ELSE STRING(oe-ord.po-no,"x(15)") . /* ticket 14966*/
                WHEN "die-no" THEN cVarValue = IF AVAILABLE itemfg AND itemfg.die-no NE "" THEN STRING(itemfg.die-no,"x(15)") ELSE IF AVAILABLE eb THEN STRING(eb.die-no,"x(15)") ELSE "" . /* ticket 16188*/
                WHEN "v-net-prct" THEN cVarValue = STRING(dNetprct,"->>9.99").
                WHEN "csrUser_id" THEN cVarValue = STRING(cUsers-id,"X(8)").
                WHEN "ack-date" THEN cVarValue = IF AVAIL oe-ord AND oe-ord.ack-prnt-date NE ? THEN STRING(oe-ord.ack-prnt-date) ELSE "".
                WHEN "Ship-from" THEN cVarValue = STRING(w-data.cShip-from,"X(9)").
                WHEN "v-mach" THEN cVarValue = STRING(cMachine,"X(30)").
                WHEN "v-ink" THEN cVarValue = STRING(cInks,"X(40)").
                WHEN "print-sheet" THEN cVarValue =  IF AVAIL itemfg THEN STRING(itemfg.plate-no,"X(20)") ELSE "".
                WHEN "full-cost" THEN cVarValue = STRING(oe-ordl.spare-dec-1,"->>>>>>>9.99").   
                WHEN "v-cost" THEN cVarValue = IF prt-profit AND oe-ordl.cost NE ? THEN STRING(oe-ordl.cost,"->>>,>>>,>>9.99") ELSE "".
                WHEN "v-t-cost" THEN cVarValue = IF prt-profit AND oe-ordl.t-cost NE ? THEN STRING(oe-ordl.t-cost,"->>,>>>,>>9.99") ELSE "".
                WHEN "status" THEN cVarValue = STRING(c-result,"x(20)") .
                WHEN "po-recvdt" THEN cVarValue = IF AVAIL oe-ord AND oe-ord.poReceivedDate NE ? THEN STRING(oe-ord.poReceivedDate) ELSE "".
                WHEN "prev-order" THEN cVarValue = STRING(cPrevOrder,"X(8)") .
		        WHEN "approved-date" THEN cVarValue = IF AVAIL oe-ord AND oe-ord.approved-date NE ? THEN STRING(oe-ord.approved-date) ELSE "".
		        WHEN "oe-ord.due-date" THEN cVarValue = IF AVAIL oe-ord AND oe-ord.due-date NE ? THEN STRING(oe-ord.due-date) ELSE "".
		        WHEN "oe-ord.ord-date" THEN cVarValue = IF AVAIL oe-ord AND oe-ord.ord-date NE ? THEN STRING(oe-ord.ord-date) ELSE "".
            END CASE.
            IF lookup(cTmpField,"v-profit,v-revenue,full-cost,v-cost,v-t-cost") GT 0 AND NOT prt-profit THEN NEXT.
            
            IF  cTmpField = "oe-ord.due-date" THEN
                 cExcelVarValue = IF AVAIL oe-ord AND oe-ord.due-date NE ? THEN DYNAMIC-FUNCTION("sfFormat_Date",oe-ord.due-date) ELSE "".
            ELSE IF  cTmpField = "oe-ord.ord-date" THEN
                 cExcelVarValue = IF AVAIL oe-ord AND oe-ord.ord-date NE ? THEN DYNAMIC-FUNCTION("sfFormat_Date",oe-ord.ord-date) ELSE "".
            ELSE IF  cTmpField = "approved-date" THEN
                 cExcelVarValue = IF AVAIL oe-ord AND oe-ord.approved-date NE ? THEN DYNAMIC-FUNCTION("sfFormat_Date",oe-ord.approved-date) ELSE "".
            ELSE IF  cTmpField = "ack-date" THEN
                 cExcelVarValue = IF AVAIL oe-ord AND oe-ord.ack-prnt-date NE ? THEN DYNAMIC-FUNCTION("sfFormat_Date",oe-ord.ack-prnt-date) ELSE "".
            ELSE cExcelVarValue = cVarValue.
            cDisplay = cDisplay + cVarValue +
                       FILL(" ",INTEGER(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
            cExcelDisplay = cExcelDisplay + quoter(DYNAMIC-FUNCTION("FormatForCSV" IN hdOutputProcs,cExcelVarValue)) + ",".            
         END.
      END.
      PUT UNFORMATTED cDisplay SKIP.
      IF rd-dest = 3 THEN DO:
       cExcelDisplay = '"' + w-data.sman + '",' +
                       '"' + v-sname    + '",' +
                       cExcelDisplay.
       PUT STREAM excel UNFORMATTED  
               cExcelDisplay SKIP.
      END.
      /* ========== end of new selectable culumns =======*/

      IF last-of(tt-report.key-01) then do:
        ASSIGN 
         v-price-per-m = (ACCUM TOTAL BY tt-report.key-01 v-revenue) /
                         (ACCUM TOTAL BY tt-report.key-01 w-data.t-sqft)
         v-price-per-t = (ACCUM TOTAL BY tt-report.key-01 v-revenue) /
                         (ACCUM TOTAL BY tt-report.key-01 w-data.t-tons)
         v-profit      = ((ACCUM TOTAL BY tt-report.key-01 v-revenue) -
                          (ACCUM TOTAL BY tt-report.key-01 w-data.cost)) /
                         (ACCUM TOTAL BY tt-report.key-01 v-revenue) * 100
         tot-sqft        = (ACCUM TOTAL BY tt-report.key-01 w-data.t-sqft) 
         tot-renv        = (ACCUM TOTAL BY tt-report.key-01 v-revenue)
         tot-ton         = (ACCUM TOTAL BY tt-report.key-01 w-data.t-tons) .

        IF v-price-per-m EQ ? THEN v-price-per-m = 0.
        IF v-price-per-t EQ ? THEN v-price-per-t = 0.
        IF v-profit      EQ ? THEN v-profit      = 0.
        IF tot-sqft      EQ ? THEN tot-sqft      = 0.
        IF tot-renv      EQ ? THEN tot-renv      = 0.
        IF tot-ton       EQ ? THEN tot-ton       = 0.
        
        IF tb_rep-tot THEN DO:
         PUT    SKIP  str-line SKIP .
          ASSIGN cDisplay = ""
           cTmpField = ""
           cVarValue = ""
           cExcelDisplay = ""
           cExcelVarValue = "".    

          DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
           cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
           
              CASE cTmpField:
                   WHEN "oe-ord.due-date" THEN cVarValue = "" .
                   WHEN "w-data.ord-no" THEN cVarValue = "" .
                   WHEN "cust.cust-no" THEN cVarValue = "" .
                   WHEN "cust.name" THEN cVarValue = "" .
                   WHEN "w-data.comm" THEN cVarValue = "" .
                   WHEN "w-data.procat" THEN cVarValue = "" .
                   WHEN "w-data.item-n" THEN cVarValue = "" .
                   WHEN "w-data.qty" THEN cVarValue = "" .
                   WHEN "w-data.sqft" THEN cVarValue = "" .
                   WHEN "price" THEN cVarValue = "" .
                   WHEN "oe-ordl.i-no" THEN cVarValue = "" .
                   WHEN "oe-ordl.ord-date" THEN cVarValue = "" .
                   WHEN "cust-po" THEN cVarValue = "". /* ticket 14966*/
                   WHEN "t-sqft" THEN cVarValue = STRING(tot-sqft,"->,>>>.999").
                   WHEN "v-price-per-m" THEN cVarValue = STRING(v-price-per-m,"->>,>>9.99").
                   WHEN "v-revenue" THEN cVarValue = IF prt-profit THEN STRING(tot-renv,"->,>>>,>>9.99") ELSE "".
                   WHEN "v-margin" THEN cVarValue = "".
                   WHEN "t-tons" THEN cVarValue = STRING(tot-ton,"->,>>>.9").
                   WHEN "v-profit" THEN cVarValue = IF prt-profit THEN STRING(v-profit,"->>,>>9.9") ELSE "".
                   WHEN "v-price-per-t" THEN cVarValue = STRING(v-price-per-t,"->>,>>9.99").
                   WHEN "v-net-prct" THEN cVarValue = "" .
                   WHEN "w-data.shp-qty" THEN cVarValue = "" .
                   WHEN "csrUser_id" THEN cVarValue = "" .
                   WHEN "ack-date" THEN cVarValue = "".
                   WHEN "oe-ordl.pr-uom" THEN cVarValue = "".
                   WHEN "Ship-from" THEN cVarValue = "".
                   WHEN "v-mach" THEN cVarValue = "".
                   WHEN "v-ink" THEN cVarValue = "" .
                   WHEN "print-sheet" THEN cVarValue =  "".
                   WHEN "v-cost" THEN cVarValue = "" .
                   WHEN "v-t-cost" THEN cVarValue = "".
                   WHEN "full-cost" THEN cVarValue = "" .
                   WHEN "status" THEN cVarValue = "" .
                   WHEN "po-recvdt" THEN cVarValue = "".
                   WHEN "prev-order" THEN cVarValue = "" . 
		           WHEN "approved-date" THEN cVarValue = "" .	
              END CASE.
              IF lookup(cTmpField,"v-profit,v-revenue,full-cost,v-cost,v-t-cost") GT 0 AND NOT prt-profit THEN NEXT.
              cExcelVarValue = cVarValue.
              cDisplay = cDisplay + cVarValue +
                         FILL(" ",INTEGER(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
              cExcelDisplay = cExcelDisplay + QUOTER(cExcelVarValue) + ",".            
          
         END.
         PUT UNFORMATTED "       * REP TOTALS *" SUBSTRING(cDisplay,22,300) SKIP.
         IF rd-dest = 3 THEN DO:
             cExcelDisplay = '"' + "" + '",' +
                 '"' + ""    + '",' +
                 cExcelDisplay.
             PUT STREAM excel UNFORMATTED  
                 "REP TOTALS " + SUBSTRING(cExcelDisplay,3,300) SKIP.
         END.
        END. /*if tb_rep-tot then */
      END.    /* last-of sman */

      IF LAST(tt-report.key-01) THEN DO:
        ASSIGN 
         v-price-per-m = (ACCUM TOTAL v-revenue) /
                         (ACCUM TOTAL w-data.t-sqft)
         v-price-per-t = (ACCUM TOTAL v-revenue) /
                         (ACCUM TOTAL w-data.t-tons)
         v-profit      = ((ACCUM TOTAL v-revenue) -
                          (ACCUM TOTAL w-data.cost)) /
                         (ACCUM TOTAL v-revenue) * 100.

        IF v-price-per-m EQ ? THEN v-price-per-m = 0.
        IF v-price-per-t EQ ? THEN v-price-per-t = 0.
        IF v-profit      EQ ? THEN v-profit      = 0.
      

          PUT    SKIP  str-line SKIP .
          ASSIGN cDisplay = ""
           cTmpField = ""
           cVarValue = ""
           cExcelDisplay = ""
           cExcelVarValue = "".    

          DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
           cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
           
              CASE cTmpField:
                   WHEN "oe-ord.due-date" THEN cVarValue = "" .
                   WHEN "w-data.ord-no" THEN cVarValue = "" .
                   WHEN "cust.cust-no" THEN cVarValue = "" .
                   WHEN "cust.name" THEN cVarValue = "" .
                   WHEN "w-data.comm" THEN cVarValue = "" .
                   WHEN "w-data.procat" THEN cVarValue = "" .
                   WHEN "w-data.item-n" THEN cVarValue = "" .
                   WHEN "w-data.qty" THEN cVarValue = "" .
                   WHEN "w-data.sqft" THEN cVarValue = "" .
                   WHEN "price" THEN cVarValue = "" .
                   WHEN "oe-ordl.i-no" THEN cVarValue = "" .
                   WHEN "oe-ordl.ord-date" THEN cVarValue = "" .
                   WHEN "cust-po" THEN cVarValue = "". /* ticket 14966*/
                   WHEN "t-sqft" THEN cVarValue = STRING(tot-sqft,"->,>>>.999"). 
                   WHEN "v-price-per-m" THEN cVarValue = STRING(v-price-per-m,"->>,>>9.99"). 
                   WHEN "v-revenue" THEN cVarValue = IF prt-profit THEN STRING((ACCUM TOTAL v-revenue),"->,>>>,>>9.99") ELSE "".
                   WHEN "v-margin" THEN cVarValue = "".
                   WHEN "t-tons" THEN cVarValue = STRING((ACCUM TOTAL w-data.t-tons),"->,>>>.9").
                   WHEN "v-profit" THEN cVarValue = IF prt-profit THEN STRING(v-profit,"->>,>>9.9") ELSE "".
                   WHEN "v-price-per-t" THEN cVarValue = STRING(v-price-per-t,"->>,>>9.99").
                   WHEN "v-net-prct" THEN cVarValue = "" .
                   WHEN "w-data.shp-qty" THEN cVarValue = "" .
                   WHEN "csrUser_id" THEN cVarValue = "" .
                   WHEN "ack-date" THEN cVarValue = "".
                   WHEN "oe-ordl.pr-uom" THEN cVarValue = "".
                   WHEN "Ship-from" THEN cVarValue = "".
                   WHEN "v-mach" THEN cVarValue = "".
                   WHEN "v-ink" THEN cVarValue = "" .
                   WHEN "print-sheet" THEN cVarValue =  "".
                   WHEN "v-cost" THEN cVarValue = "" .
                   WHEN "v-t-cost" THEN cVarValue = "".
                   WHEN "full-cost" THEN cVarValue = "" .
                   WHEN "status" THEN cVarValue = "" .
                   WHEN "po-recvdt" THEN cVarValue = "".
                   WHEN "prev-order" THEN cVarValue = "". 
		           WHEN "approved-date" THEN cVarValue = "" .
              END CASE.
              IF lookup(cTmpField,"v-profit,v-revenue,full-cost,v-cost,v-t-cost") GT 0 AND NOT prt-profit THEN NEXT.
              cExcelVarValue = cVarValue.
              cDisplay = cDisplay + cVarValue +
                         FILL(" ",INTEGER(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
              cExcelDisplay = cExcelDisplay + QUOTER(cExcelVarValue) + ",".            
          
         END.
         PUT UNFORMATTED "       ** COMPANY TOTALS **   " STRING(v-n-lines,">>>>9") " Line Items" SUBSTRING(cDisplay,47,300) SKIP.
      END.
    END.
    
    DELETE w-data.
    DELETE tt-report.
  END.
  
  IF v-break THEN HIDE FRAME r-top1 NO-PAUSE.
  /*
  page.
  /*HIDE FRAME r-top NO-PAUSE.*/
  */
  PUT SKIP(1).
  FOR EACH wkrecap BREAK BY wkrecap.procat:
    DO i = 1 TO 2:
      ASSIGN 
       wkrecap.price-per-m[i] = wkrecap.revenue[i] / wkrecap.t-sqft[i]
       wkrecap.price-per-t[i] = wkrecap.revenue[i] / wkrecap.t-tons[i].

      IF wkrecap.price-per-m[i] EQ ? THEN wkrecap.price-per-m[i] = 0.
      IF wkrecap.price-per-t[i] EQ ? THEN wkrecap.price-per-t[i] = 0.
    END.

    FIND FIRST fgcat NO-LOCK
        WHERE fgcat.company EQ cocode
          AND fgcat.procat  EQ wkrecap.procat
         NO-ERROR.
          
    IF tb_ton THEN DO WITH FRAME f-recap-t:
      DISPLAY wkrecap.procat
              fgcat.dscr WHEN AVAILABLE fgcat
                "Prep/Misc" WHEN wkrecap.procat EQ "P/M" @ procat.dscr
              wkrecap.num-of-ord
              wkrecap.revenue
              wkrecap.t-sqft
              wkrecap.price-per-m
              wkrecap.t-tons
              wkrecap.price-per-t.
      DOWN.
    END.

    ELSE
    DO WITH FRAME f-recap:
      DISPLAY wkrecap.procat
              fgcat.dscr WHEN AVAILABLE fgcat
                "Prep/Misc" WHEN wkrecap.procat EQ "P/M" @ procat.dscr
              wkrecap.num-of-ord
              wkrecap.revenue
              wkrecap.t-sqft
              wkrecap.price-per-m.
      DOWN.
    END.

    ACCUMULATE 
     wkrecap.revenue[1] (TOTAL)
     wkrecap.t-sqft[1]  (TOTAL)
     wkrecap.t-tons[1]  (TOTAL)
     wkrecap.revenue[2] (TOTAL)
     wkrecap.t-sqft[2]  (TOTAL)
     wkrecap.t-tons[2]  (TOTAL).

    IF LAST(wkrecap.procat) THEN DO:
      ASSIGN 
       v-msf[1] = (ACCUM TOTAL wkrecap.revenue[1]) /
                  (ACCUM TOTAL wkrecap.t-sqft[1])
       v-msf[2] = (ACCUM TOTAL wkrecap.revenue[2]) /
                  (ACCUM TOTAL wkrecap.t-sqft[2])
       v-ton[1] = (ACCUM TOTAL wkrecap.revenue[1]) /
                  (ACCUM TOTAL wkrecap.t-tons[1])
       v-ton[2] = (ACCUM TOTAL wkrecap.revenue[2]) /
                  (ACCUM TOTAL wkrecap.t-tons[2]).

      IF v-msf[1] EQ ? THEN v-msf[1] = 0.
      IF v-msf[2] EQ ? THEN v-msf[2] = 0.
      IF v-ton[1] EQ ? THEN v-ton[1] = 0.
      IF v-ton[2] EQ ? THEN v-ton[2] = 0.

      IF tb_ton THEN DO WITH FRAME f-recap-t:
        UNDERLINE wkrecap.revenue
                  wkrecap.t-sqft
                  wkrecap.price-per-m
                  wkrecap.t-tons
                  wkrecap.price-per-t.
        DOWN.

        DISPLAY (ACCUM TOTAL wkrecap.revenue[1]) @ wkrecap.revenue[1]
                (ACCUM TOTAL wkrecap.t-sqft[1])  @ wkrecap.t-sqft[1]
                v-msf[1] @ wkrecap.price-per-m[1]
                (ACCUM TOTAL wkrecap.t-tons[1])  @ wkrecap.t-tons[1]
                v-ton[1] @ wkrecap.price-per-t[1]
                (ACCUM TOTAL wkrecap.revenue[2]) @ wkrecap.revenue[2]
                (ACCUM TOTAL wkrecap.t-sqft[2])  @ wkrecap.t-sqft[2]
                v-msf[2] @ wkrecap.price-per-m[2]
                (ACCUM TOTAL wkrecap.t-tons[2])  @ wkrecap.t-tons[2]
                v-ton[2] @ wkrecap.price-per-t[2].
        DOWN.

        DISPLAY "Number of Days" @ procat.dscr
                v-per-days[1] FORMAT ">>>9" @ wkrecap.revenue[1]
                v-per-days[2] FORMAT ">>>9" @ wkrecap.revenue[2].
        DOWN.

        UNDERLINE wkrecap.revenue[1]
                  wkrecap.revenue[2].
        DOWN.
      
        DISPLAY "Average" @ procat.dscr
                ((ACCUM TOTAL wkrecap.revenue[1]) / v-per-days[1])
                                              @ wkrecap.revenue[1]
                ((ACCUM TOTAL wkrecap.revenue[2]) / v-per-days[2])
                                              @ wkrecap.revenue[2].
        DOWN.
      END.

      ELSE DO WITH FRAME f-recap:
        UNDERLINE wkrecap.revenue
                  wkrecap.t-sqft
                  wkrecap.price-per-m.
        DOWN.

        DISPLAY (ACCUM TOTAL wkrecap.revenue[1]) @ wkrecap.revenue[1]
                (ACCUM TOTAL wkrecap.t-sqft[1])  @ wkrecap.t-sqft[1]
                v-msf[1] @ wkrecap.price-per-m[1]
                (ACCUM TOTAL wkrecap.revenue[2]) @ wkrecap.revenue[2]
                (ACCUM TOTAL wkrecap.t-sqft[2])  @ wkrecap.t-sqft[2]
                v-msf[2] @ wkrecap.price-per-m[2].
        DOWN.

        DISPLAY "Number of Days" @ procat.dscr
                v-per-days[1] FORMAT ">>>9" @ wkrecap.revenue[1]
                v-per-days[2] FORMAT ">>>9" @ wkrecap.revenue[2].
        DOWN.

        UNDERLINE wkrecap.revenue[1]
                  wkrecap.revenue[2].
        DOWN.
      
        DISPLAY "Average" @ procat.dscr
                ((ACCUM TOTAL wkrecap.revenue[1]) / v-per-days[1])
                                              @ wkrecap.revenue[1]
                ((ACCUM TOTAL wkrecap.revenue[2]) / v-per-days[2])
                                              @ wkrecap.revenue[2].
        DOWN.
      END.
    END.
  END.
