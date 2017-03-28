   /* oe\rep\oehots.i from oe/rep/schdrel2.i*/
   DEF BUFFER b-oe-ordl FOR oe-ordl.
   DEF BUFFER b-oe-ordl2 FOR oe-ordl.

   DEF VAR lv-qty             LIKE oe-rell.qty        NO-UNDO.
   DEF VAR lv-subt            AS CHAR                 NO-UNDO.
   DEF VAR lv-cr-rating       LIKE cust.cr-rating     NO-UNDO.
   DEF VAR ll-show-top-only   AS LOG                  NO-UNDO.
   DEF VAR lv-board-po-no     LIKE po-ord.po-no       NO-UNDO.
   DEF VAR lv-vend-no         LIKE vend.vend-no       NO-UNDO.
   DEF VAR lv-routing         AS CHAR FORM "x(10)"    NO-UNDO.
   DEF VAR lv-prev-cust-name  AS CHAR                 NO-UNDO.
   DEF VAR v-first            AS LOG INIT YES         NO-UNDO.
   DEF VAR v-ship-city        LIKE shipto.ship-city   NO-UNDO.
   DEF VAR lv-board-qty       AS INT                  NO-UNDO.
   DEF VAR lv-first-last-set  AS LOG                  NO-UNDO.
   DEF VAR v-comp-qty         AS INT                  NO-UNDO.
   DEF VAR v-line             AS CHAR FORM "x(50)"    NO-UNDO.
   DEF VAR v-line2            AS CHAR FORMAT "x(13)"  NO-UNDO INITIAL "Comments</B>". 
   DEF VAR lc-style-xl        LIKE eb.style           NO-UNDO.
   DEF VAR lc-test-xl         LIKE eb.test            NO-UNDO.

   ASSIGN 
      v-line  = "<C1><FROM><C107><LINE><||3>"
      v-line2 = IF rd_lComments THEN "Comments</B>" ELSE "Style Test</B>".
  
   FORM HEADER
      "<FCourier New><B><C5><P26>Hots <P8>" skip(1)
      STRING(TODAY) AT 3 FORM "x(5)"  "Begin Date:" AT 30 STRING(begin_date) FORM "x(5)"  "  End date:" STRING(end_date) FORM "x(5)" 
      "Page: " AT 145 PAGE-NUM FORM ">>9" SKIP
      " Date Due Ordered Item       Cust PO#        Order#  R# Ven    PO#   Brd Rcpt  Routing      Q-Order " +
      " Q-Comp Q-Onhand  Ship City         " v-line2 SKIP
      v-line SKIP
     WITH FRAME pg-head NO-BOX PAGE-TOP NO-LABEL STREAM-IO WIDTH 200.

   IF tb_excel THEN DO:
      OUTPUT STREAM st-excel TO VALUE(fi_file).
      PUT STREAM st-excel UNFORMATTED
         "Date,Due,Ordered,Item,Cust PO#,Order#,R#,Ven,PO#,Brd Rcpt,Routing,Q-Order,Q-Comp,Q-Onhand,Ship City,"
         (IF rd_lComments THEN "Comment,Customer" ELSE "Style,Test,Customer")
         SKIP.
   END.
   
   ASSIGN 
      {sys/inc/ctrtext.i str-tit2 112}
      v-tot-qty    = 0
      v-tot-msf    = 0
      v-tot-val    = 0
      str-tit2     = c-win:TITLE
      v-fcust[1]   = begin_cust-no
      v-fcust[2]   = end_cust-no
      v-fsman[1]   = begin_slsmn
      v-fsman[2]   = end_slsmn
      v-ford-no[1] = begin_ord-no
      v-ford-no[2] = end_ord-no
      v-fitem[1]   = begin_i-no
      v-fitem[2]   = end_i-no
      v-floc[1]    = begin_loc
      v-floc[2]    = end_loc
      v-fdate[1]   = begin_date
      v-fdate[2]   = end_date
      v-fcarr[1]   = begin_carr
      v-fcarr[2]   = end_carr
      v-ponum      = tb_po-no
      v-sort       = IF rd_sort EQ "Customer#"     THEN "C"  ELSE
                     IF rd_sort EQ "Release Date"  THEN "R"  ELSE
                     IF rd_sort EQ "Item#"         THEN "I"  ELSE
                     IF rd_sort EQ "Item Name"     THEN "N"  ELSE
                     IF rd_sort EQ "Territory"     THEN "T"  ELSE
                     IF rd_sort EQ "Credit Rating" THEN "CR" ELSE "A"
      v-print      = SUBSTRING(rd_print,1,1)
      v-by-job     = tb_qoh-job
      v-types      = STRING(tb_posted,"P/")      + STRING(tb_actual,"A/")      +
                     STRING(tb_late,"L/")        + STRING(tb_scheduled,"S/")   +
                     STRING(tb_backordered,"B/") + STRING(tb_invoiceable,"I/") +
                     STRING(tb_completed,"C/")   + STRING(tb_invoice,"Z/")

      str-tit3 = IF v-sort EQ "C"  THEN "By Customer By Date"      ELSE
                 IF v-sort EQ "R"  THEN "By Date By Customer"      ELSE
                 IF v-sort EQ "I"  THEN "By Item By Date"          ELSE
                 IF v-sort EQ "N"  THEN "By Item Name By Date"     ELSE
                 IF v-sort EQ "A"  THEN "By Carrier By Date"       ELSE
                 IF v-sort EQ "CR" THEN "By Credit Rating By Date" ELSE
                                     "By Territory By Date"      .
   SESSION:SET-WAIT-STATE ("general").
   {sys/inc/ctrtext.i str-tit3 132}.
   
   {sys/inc/print1.i}

   {sys/inc/outprint.i VALUE(lines-per-page)}

   IF rd-dest EQ 2 THEN DO:
      IF NOT lBussFormModle THEN
        PUT UNFORMATTED "<PREVIEW><MODAL=NO><OLANDSCAPE><P9></PROGRESS>".   
      ELSE
        PUT UNFORMATTED "<PREVIEW><OLANDSCAPE><P9></PROGRESS>".
   END.
   ELSE 
   IF rd-dest EQ 1 THEN PUT UNFORMATTED "<PRINTER?><OLANDSCAPE><P9></PROGRESS>". ELSE 
   IF rd-dest EQ  4 THEN DO:
      ls-fax-file = "c:\tmp\fax" + STRING(TIME) + ".tif".
      PUT UNFORMATTED "<PRINTER?><EXPORT=" Ls-fax-file ",BW>".
   END.
   ELSE 
      IF rd-dest = 5 THEN 
         PUT UNFORMATTED "<PRINT=NO><OLANDSCAPE><PDF-LEFT=4mm><PDF-TOP=1mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf></PROGRESS>" FORM "x(180)".

   VIEW FRAME pg-head.

   EMPTY TEMP-TABLE tt-report.

   FOR EACH w-ord:
      DELETE w-ord.
   END.
   
   EMPTY TEMP-TABLE tt-ord.
  
   main-loop:
   FOR EACH oe-ordl WHERE 
            oe-ordl.company EQ cocode
        AND oe-ordl.opened  EQ YES
        AND oe-ordl.ord-no  GE v-ford-no[1]
        AND oe-ordl.ord-no  LE v-ford-no[2]
        AND oe-ordl.i-no    GE v-fitem[1]
        AND oe-ordl.i-no    LE v-fitem[2]
        AND ((oe-ordl.s-man[1] GE v-fsman[1] 
        AND   oe-ordl.s-man[1] LE v-fsman[2]) 
         OR  (oe-ordl.s-man[2] GE v-fsman[1] 
        AND   oe-ordl.s-man[2] LE v-fsman[2]) 
         OR  (oe-ordl.s-man[3] GE v-fsman[1] 
        AND   oe-ordl.s-man[3] LE v-fsman[2]))
        AND NOT CAN-FIND(FIRST b-oe-ordl {sys/inc/ordlcomp.i b-oe-ordl oe-ordl})
        USE-INDEX opened NO-LOCK,
      FIRST oe-ord WHERE 
            oe-ord.company EQ oe-ordl.company
        AND oe-ord.ord-no  EQ oe-ordl.ord-no
        AND oe-ord.cust-no GE v-fcust[1]
        AND oe-ord.cust-no LE v-fcust[2] 
        AND oe-ord.user-id GE begin_userid
        AND oe-ord.user-id LE end_userid NO-LOCK,
      FIRST cust WHERE 
            cust.company EQ oe-ord.company
        AND cust.cust-no EQ oe-ord.cust-no NO-LOCK:

      STATUS DEFAULT "Processing Order#/FG#: " +
                     TRIM(STRING(oe-ordl.ord-no,">>>>>>>>")) + "/" +
                     TRIM(oe-ordl.i-no).

      RUN oe/cleanrel.p (ROWID(oe-ordl)).
            
      FOR EACH oe-rel NO-LOCK WHERE 
              oe-rel.company   EQ oe-ordl.company
          AND oe-rel.ord-no    EQ oe-ordl.ord-no
          AND oe-rel.i-no      EQ oe-ordl.i-no
          AND oe-rel.line      EQ oe-ordl.line
          AND oe-rel.rel-date  GE v-fdate[1]
          AND oe-rel.rel-date  LE v-fdate[2]
          AND oe-rel.carrier   GE v-fcarr[1]
          AND oe-rel.carrier   LE v-fcarr[2]:
      
         RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT v-type).

         IF INDEX("AB",v-type) GT 0 THEN NEXT.
    
         IF INDEX(v-types,v-type) GT 0 THEN DO:
            CREATE tt-report.
            ASSIGN
               tt-report.term-id = ""
               tt-report.key-01  = IF v-sort EQ "R" THEN
                                    (STRING(YEAR(oe-rel.rel-date),"9999") +
                                     STRING(MONTH(oe-rel.rel-date),"99")  +
                                     STRING(DAY(oe-rel.rel-date),"99")) ELSE
                                   IF v-sort EQ "N" THEN oe-ordl.i-name ELSE ""
               tt-report.key-02  = IF v-sort EQ "I" OR v-sort EQ "D" THEN oe-rel.i-no ELSE
                                   IF v-sort EQ "T" THEN cust.terr ELSE
                                   IF v-sort EQ "A" THEN oe-rel.carrier ELSE
                                   IF v-sort EQ "CR" THEN cust.cr-rating ELSE oe-rel.cust-no
               tt-report.key-03  = IF v-sort ne "R" THEN
                                    (STRING(year(oe-rel.rel-date),"9999") +
                                     STRING(month(oe-rel.rel-date),"99")  +
                                     STRING(day(oe-rel.rel-date),"99")) ELSE ""
               tt-report.key-04  = STRING(IF v-sort EQ "A" THEN oe-rel.cust-no ELSE " ","x(10)") + STRING(oe-ord.ord-no,"9999999999")
               tt-report.key-05  = STRING(INDEX(v-types,v-type),"99")
               tt-report.key-06  = v-type
               tt-report.rec-id  = RECID(oe-rel).
         END.
      END.
  
      FOR EACH oe-rell NO-LOCK WHERE 
               oe-rell.company EQ oe-ordl.company
          AND oe-rell.ord-no  EQ oe-ordl.ord-no
          AND oe-rell.i-no    EQ oe-ordl.i-no
          AND oe-rell.line    EQ oe-ordl.line
          AND ((oe-rell.b-ord-no NE 0 AND INDEX(v-types,"B") GT 0) OR
               (oe-rell.b-ord-no EQ 0 AND INDEX(v-types,"A") GT 0))
        USE-INDEX ord-no,

        FIRST oe-relh NO-LOCK WHERE 
              oe-relh.r-no     EQ oe-rell.r-no
          AND oe-relh.posted   EQ NO
          AND oe-relh.deleted  EQ NO
          AND oe-relh.rel-date GE v-fdate[1]
          AND oe-relh.rel-date LE v-fdate[2]
          AND oe-relh.carrier  GE v-fcarr[1]
          AND oe-relh.carrier  LE v-fcarr[2]

         BREAK BY oe-rell.r-no
               BY oe-rell.ord-no
               BY oe-rell.i-no
               BY oe-rell.line
               BY oe-rell.rel-no
               BY oe-rell.b-ord-no
               BY oe-rell.po-no:

         IF FIRST-OF(oe-rell.po-no) THEN lv-qty = 0.

         lv-qty = lv-qty + oe-rell.qty.

         IF LAST-OF(oe-rell.po-no) THEN DO:
            CREATE tt-report.
            ASSIGN
               tt-report.term-id = ""
               tt-report.key-01  = IF v-sort EQ "R" THEN
                                       (STRING(year(oe-relh.rel-date),"9999") +
                                        STRING(month(oe-relh.rel-date),"99")  +
                                        STRING(day(oe-relh.rel-date),"99")) ELSE
                                   IF v-sort EQ "N" THEN oe-ordl.i-name ELSE ""
               tt-report.key-02  = IF v-sort EQ "I" OR v-sort EQ "D" THEN oe-rell.i-no ELSE
                                   IF v-sort EQ "T" THEN cust.terr ELSE
                                   IF v-sort EQ "A" THEN oe-relh.carrier ELSE
                                   IF v-sort EQ "CR" THEN cust.cr-rating ELSE oe-relh.cust-no
               tt-report.key-03  = IF v-sort ne "R" THEN
                                       (STRING(year(oe-relh.rel-date),"9999") +
                                        STRING(month(oe-relh.rel-date),"99")  +
                                        STRING(day(oe-relh.rel-date),"99")) ELSE ""
               tt-report.key-04  = STRING(IF v-sort EQ "A" THEN oe-relh.cust-no ELSE " ","x(10)") + STRING(oe-ord.ord-no,"9999999999")
               tt-report.key-05  = STRING(INDEX(v-types,v-type),"99")
               tt-report.key-06  = IF oe-rell.b-ord-no EQ 0 THEN "A" ELSE "B"
               tt-report.qty     = lv-qty
               tt-report.rec-id  = recid(oe-rell).
         END.

      /* new start */
      END.
      /* new end */
   END.

   IF scr-jobs-no-order THEN
   FOR EACH job WHERE
       job.company EQ cocode AND
       job.opened EQ YES AND
       job.USER-ID GE begin_userid AND
       job.USER-ID LE END_userid AND
       ((job.start-date GE begin_date AND
         job.start-date LE END_date) OR job.start-date EQ ?)
       NO-LOCK,
       EACH job-hdr WHERE
            job-hdr.company EQ cocode AND
            job-hdr.job EQ job.job AND
            job-hdr.job-no EQ job.job-no AND
            job-hdr.job-no2 EQ job.job-no2 AND
            job-hdr.ord-no EQ 0 AND
            job-hdr.cust-no GE begin_cust-no AND
            job-hdr.cust-no LE end_cust-no AND
            job-hdr.i-no GE begin_i-no AND
            job-hdr.i-no LE end_i-no
            NO-LOCK:

       STATUS DEFAULT "Processing Job# " +
                      TRIM(job-hdr.job-no) + "-" +
                      STRING(job-hdr.job-no2,"99").

       CREATE tt-report.
       ASSIGN
          tt-report.term-id = ""
          tt-report.key-01  = ""
          tt-report.key-02  = job-hdr.cust-no
          tt-report.key-03  = ""
          tt-report.key-04  = ""
          tt-report.key-05  = ""
          tt-report.key-06  = ""
          tt-report.rec-id  = RECID(job-hdr)
          tt-report.open-job-no-ord = YES.

   END. /*scr-job-no-order*/

   STATUS DEFAULT "Printing...".

   IF NOT CAN-FIND(FIRST tt-report WHERE tt-report.term-id EQ "") THEN DO:
      CREATE tt-report.
      ASSIGN
         tt-report.term-id = ""
         ll-show-top-only  = YES.
   END.

   RELEASE tt-report.

   FOR EACH tt-report WHERE tt-report.term-id EQ ""
      BREAK BY tt-report.key-01
            BY tt-report.key-02
            BY tt-report.key-03
            by tt-report.key-04:

      RELEASE oe-rel.
      RELEASE oe-rell.
      RELEASE oe-relh.
      RELEASE oe-ord.
      RELEASE oe-ordl.

      v-rel-no = 0.

      IF NOT tt-report.open-job-no-ord THEN
      DO:
         FIND FIRST oe-rel WHERE RECID(oe-rel) EQ tt-report.rec-id NO-LOCK NO-ERROR.
        
         IF AVAIL oe-rel THEN DO:
            FOR EACH oe-rell WHERE 
                   oe-rell.company  EQ cocode
               AND oe-rell.ord-no   EQ oe-rel.ord-no
               AND oe-rell.rel-no   EQ oe-rel.rel-no
               AND oe-rell.b-ord-no EQ oe-rel.b-ord-no
               AND oe-rell.i-no     EQ oe-rel.i-no
               AND oe-rell.line     EQ oe-rel.line
               USE-INDEX ord-no NO-LOCK,
               FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no NO-LOCK:
               
               v-rel-no = oe-relh.release#.
        
               IF oe-relh.posted EQ NO AND oe-relh.deleted EQ NO THEN
                  tt-report.rec-id = recid(oe-rell).
               ELSE 
                  RELEASE oe-relh.
        
               LEAVE.
            END.
        
            FIND FIRST oe-ordl WHERE 
                       oe-ordl.company EQ cocode
                   AND oe-ordl.ord-no  EQ oe-rel.ord-no
                   AND oe-ordl.i-no    EQ oe-rel.i-no
                   AND oe-ordl.line    EQ oe-rel.LINE NO-LOCK NO-ERROR.
         END.
        
         FIND FIRST oe-rell WHERE RECID(oe-rell) EQ tt-report.rec-id NO-LOCK NO-ERROR.
        
         IF avail oe-rell THEN DO:    
            IF INDEX("SLI",tt-report.key-06) GT 0 THEN
               tt-report.key-06 = IF oe-rell.b-ord-no EQ 0 THEN "A" ELSE "B" .
        
            FIND FIRST oe-relh WHERE 
                       oe-relh.company EQ cocode
                   AND oe-relh.r-no    EQ oe-rell.r-no NO-LOCK NO-ERROR.
         
            v-rel-no = IF AVAIL oe-relh THEN oe-relh.release# ELSE v-rel-no.
        
            FIND FIRST oe-ordl WHERE 
                       oe-ordl.company EQ cocode
                   AND oe-ordl.ord-no  EQ oe-rell.ord-no
                   AND oe-ordl.i-no    EQ oe-rell.i-no
                   AND oe-ordl.line    EQ oe-rell.LINE NO-LOCK NO-ERROR.
         END.
        
         FIND FIRST oe-ord OF oe-ordl NO-LOCK NO-ERROR.
      
         IF AVAIL oe-ord THEN
            FIND FIRST cust WHERE 
                       cust.company EQ cocode
                   AND cust.cust-no EQ oe-ord.cust-no NO-LOCK NO-ERROR.
        
         IF AVAIL oe-relh THEN
            ASSIGN
               v-qty     = IF tt-report.qty NE 0 THEN tt-report.qty ELSE oe-rell.qty
               v-date    = oe-relh.rel-date 
               v-po-no   = oe-rell.po-no
               v-rel-no  = IF rd_rel BEGINS "N" OR v-rel-no EQ 0 THEN oe-rell.rel-no ELSE v-rel-no
               v-ship-id = oe-relh.ship-id
               v-carrier = oe-relh.carrier.
         ELSE
            IF AVAIL oe-rel THEN
               ASSIGN
                  v-qty     = if oe-rel.link-no eq 0 then oe-rel.tot-qty else oe-rel.qty 
                  v-date    = oe-rel.rel-date
                  v-po-no   = oe-rel.po-no
                  v-rel-no  = IF rd_rel BEGINS "N" OR v-rel-no EQ 0 THEN oe-rel.rel-no ELSE v-rel-no
                  v-ship-id = oe-rel.ship-id
                  v-carrier = oe-rel.carrier.
      END.
      ELSE /*job without order*/
      DO:
         FIND FIRST job-hdr WHERE RECID(job-hdr) EQ tt-report.rec-id NO-LOCK.

         ASSIGN
            v-qty     = job-hdr.qty
            v-date    = job-hdr.due-date 
            v-po-no   = ""
            v-ship-id = ""
            v-carrier = "".
      END.

      CREATE w-ord.

      IF AVAIL oe-ordl THEN DO:
         FIND FIRST itemfg WHERE 
                    itemfg.company EQ cocode
                AND itemfg.i-no    EQ oe-ordl.i-no NO-LOCK NO-ERROR.
         ASSIGN
            w-ord.ord-no    = oe-ord.ord-no
            w-ord.cust-no   = oe-ord.cust-no
            w-ord.cust-name = oe-ord.cust-name
            w-ord.part-no   = oe-ordl.part-no
            w-ord.i-no      = oe-ordl.i-no
            w-ord.i-name    = oe-ordl.i-name
            w-ord.qty       = oe-ordl.qty
            w-ord.cost      = oe-ordl.cost
            w-ord.price     = oe-ordl.t-price / oe-ordl.qty
            w-ord.rel-qty   = v-qty
            w-ord.t-price   = w-ord.price * w-ord.rel-qty
            w-ord.rel-date  = STRING(v-date)
            w-ord.rel-no    = v-rel-no
            w-ord.ship-id   = v-ship-id
            w-ord.job-no    = oe-ordl.job-no
            w-ord.job-no2   = oe-ordl.job-no2
            w-ord.job       = IF w-ord.job-no EQ "" THEN "" ELSE (TRIM(w-ord.job-no) + "-" + STRING(w-ord.job-no2,"99"))
            w-ord.po-num    = v-po-no
            w-ord.ord-qty   = v-qty
            w-ord.shp-qty   = oe-ordl.ship-qty
            w-ord.msf       = w-ord.rel-qty * itemfg.t-sqft / 1000 
            w-ord.prom-code = oe-ordl.prom-code
            w-ord.last-date = oe-ord.ord-date
            w-ord.carrier   = v-carrier
            w-ord.is-a-component = oe-ordl.is-a-component.
         
         IF v-comps AND itemfg.isaset THEN DO:

            RUN fg/fullset.p (ROWID(itemfg)).

            FOR EACH tt-fg-set,
               FIRST itemfg WHERE 
                     itemfg.company EQ cocode
                 AND itemfg.i-no    EQ tt-fg-set.part-no NO-LOCK:

            CREATE b-w-ord.
            BUFFER-COPY w-ord TO b-w-ord
            ASSIGN
               b-w-ord.component = 1
               b-w-ord.cust-name = w-ord.cust-name
               b-w-ord.part-no   = itemfg.part-no 
               b-w-ord.i-no      = tt-fg-set.part-no
               b-w-ord.i-name    = itemfg.i-name 
               b-w-ord.price     = 0
               b-w-ord.cost      = 0
               b-w-ord.t-price   = 0
               b-w-ord.job       = ""
               b-w-ord.po-num    = ""
               b-w-ord.qty       = w-ord.qty     * tt-fg-set.part-qty-dec
               b-w-ord.rel-qty   = w-ord.rel-qty * tt-fg-set.part-qty-dec
               b-w-ord.ord-qty   = w-ord.ord-qty * tt-fg-set.part-qty-dec
               b-w-ord.shp-qty   = w-ord.shp-qty * tt-fg-set.part-qty-dec
               b-w-ord.msf       = b-w-ord.rel-qty * itemfg.t-sqft / 1000.
            END.
         END.
      
         CREATE tt-ord.
         ASSIGN 
            tt-ord.tt-recid = RECID(w-ord)
            tt-ord.tt-po-no = oe-ordl.po-no-po.
      END.
      ELSE
         IF tt-report.open-job-no-ord THEN
         DO:
            FIND FIRST itemfg WHERE 
                 itemfg.company EQ cocode AND
                 itemfg.i-no    EQ job-hdr.i-no
                 NO-LOCK NO-ERROR.

            FIND FIRST cust WHERE
                 cust.company EQ cocode AND
                 cust.cust-no EQ job-hdr.cust-no
                 NO-LOCK NO-ERROR.

            FIND FIRST job OF job-hdr NO-LOCK.

            FIND FIRST eb WHERE
                 eb.company EQ cocode AND
                 eb.est-no EQ job.est-no AND
                 eb.form-no EQ job-hdr.frm AND
                 eb.blank-no EQ job-hdr.blank-no
                 NO-LOCK NO-ERROR.

            ASSIGN
               w-ord.ord-no    = 0
               w-ord.cust-no   = job-hdr.cust-no
               w-ord.cust-name = cust.name
               w-ord.part-no   = IF AVAIL eb THEN eb.part-no ELSE ""
               w-ord.i-no      = job-hdr.i-no
               w-ord.i-name    = itemfg.i-name
               w-ord.qty       = v-qty
               w-ord.cost      = 0
               w-ord.price     = 0
               w-ord.rel-qty   = v-qty
               w-ord.t-price   = 0
               w-ord.rel-date  = STRING(v-date)
               w-ord.rel-no    = v-rel-no
               w-ord.ship-id   = v-ship-id
               w-ord.job-no    = job-hdr.job-no
               w-ord.job-no2   = job-hdr.job-no2
               w-ord.job       = IF w-ord.job-no EQ "" THEN "" ELSE (TRIM(w-ord.job-no) + "-" + STRING(w-ord.job-no2,"99"))
               w-ord.po-num    = job-hdr.po-no
               w-ord.ord-qty   = v-qty
               w-ord.shp-qty   = 0
               w-ord.msf       = v-qty * itemfg.t-sqft / 1000 
               w-ord.prom-code = ""
               w-ord.last-date = job-hdr.start-date
               w-ord.carrier   = v-carrier
               w-ord.is-a-component = NO.
            
            IF v-comps AND itemfg.isaset THEN DO:
           
               RUN fg/fullset.p (ROWID(itemfg)).
           
               FOR EACH tt-fg-set,
                  FIRST itemfg WHERE 
                        itemfg.company EQ cocode
                    AND itemfg.i-no    EQ tt-fg-set.part-no NO-LOCK:
           
               CREATE b-w-ord.
               BUFFER-COPY w-ord TO b-w-ord
               ASSIGN
                  b-w-ord.component = 1
                  b-w-ord.cust-name = w-ord.cust-name
                  b-w-ord.part-no   = itemfg.part-no 
                  b-w-ord.i-no      = tt-fg-set.part-no
                  b-w-ord.i-name    = itemfg.i-name 
                  b-w-ord.price     = 0
                  b-w-ord.cost      = 0
                  b-w-ord.t-price   = 0
                  b-w-ord.job       = ""
                  b-w-ord.po-num    = ""
                  b-w-ord.qty       = w-ord.qty     * tt-fg-set.part-qty-dec
                  b-w-ord.rel-qty   = w-ord.rel-qty * tt-fg-set.part-qty-dec
                  b-w-ord.ord-qty   = w-ord.ord-qty * tt-fg-set.part-qty-dec
                  b-w-ord.shp-qty   = w-ord.shp-qty * tt-fg-set.part-qty-dec
                  b-w-ord.msf       = b-w-ord.rel-qty * itemfg.t-sqft / 1000.
               END.
            END.
           
            CREATE tt-ord.
            ASSIGN 
               tt-ord.tt-recid = RECID(w-ord)
               tt-ord.tt-po-no = 0.
         END.

   END. /* each tt-report */

   STATUS DEFAULT "".

   IF v-sort = "C" THEN
      FOR EACH w-ord
         BREAK BY w-ord.cust-name
               BY DATE(w-ord.rel-date):
         FIND FIRST itemfg WHERE 
                    itemfg.company EQ cocode
                AND itemfg.i-no    EQ w-ord.i-no NO-LOCK NO-ERROR.

         IF v-by-job THEN
            FOR EACH fg-bin WHERE 
                     fg-bin.company EQ cocode
                 AND fg-bin.i-no    EQ w-ord.i-no
                 AND fg-bin.job-no  EQ w-ord.job-no
                 AND fg-bin.job-no2 EQ w-ord.job-no2
                 AND fg-bin.loc     GE v-floc[1]
                 AND fg-bin.loc     LE v-floc[2]
               USE-INDEX job NO-LOCK:
               
            w-ord.onh-qty = w-ord.onh-qty + fg-bin.qty.
            END.
         ELSE /* w-ord.onh-qty = itemfg.q-onh. */
            FOR EACH fg-bin FIELDS(qty) WHERE 
                     fg-bin.company EQ cocode
                 AND fg-bin.i-no    EQ w-ord.i-no
                 AND fg-bin.loc     GE v-floc[1]
                 AND fg-bin.loc     LE v-floc[2]
               USE-INDEX i-no NO-LOCK:
               
               w-ord.onh-qty = w-ord.onh-qty + fg-bin.qty.
            END.
         
         v-comp-qty = 0.
        
         IF w-ord.job-no <> "" THEN DO:
            FIND FIRST tt-ord WHERE tt-ord.tt-recid = RECID(w-ord) NO-LOCK NO-ERROR.
            FOR EACH fg-rcpth NO-LOCK WHERE fg-rcpth.company = cocode
                                AND fg-rcpth.i-no = w-ord.i-no
                                AND fg-rcpth.job-no  EQ w-ord.job-no
                                AND fg-rcpth.job-no2 EQ w-ord.job-no2
                                AND fg-rcpth.rita-code = "R"
                                AND ((AVAIL tt-ord AND int(fg-rcpth.po-no) = tt-ord.tt-po-no) OR 
                                     (AVAIL tt-ord AND tt-ord.tt-po-no = 0) OR NOT AVAIL tt-ord OR w-ord.job-no <> ""),
                EACH fg-rdtlh FIELDS(qty) WHERE fg-rdtlh.r-no = fg-rcpth.r-no
                                AND fg-rdtlh.rita-code = fg-rcpth.rita-code
                                AND fg-rdtlh.loc     GE v-floc[1]
                                AND fg-rdtlh.loc     LE v-floc[2]
                                NO-LOCK:
                v-comp-qty = v-comp-qty + fg-rdtlh.qty.
            END.
        END.
        
        {oe/rep/oehots2.i}
    END.
    

ELSE IF v-sort = "R" THEN
    FOR EACH w-ord
            BREAK BY date(w-ord.rel-date)
                      BY w-ord.cust-name:
          FIND FIRST itemfg
              WHERE itemfg.company EQ cocode
                AND itemfg.i-no    EQ w-ord.i-no
              NO-LOCK NO-ERROR.

          /*IF AVAIL itemfg OR ll-show-top-only THEN DO:*/
            IF v-by-job /*AND w-ord.component EQ 0*/ THEN
            FOR EACH fg-bin
                WHERE fg-bin.company EQ cocode
                  AND fg-bin.i-no    EQ w-ord.i-no
                  AND fg-bin.job-no  EQ w-ord.job-no
                  AND fg-bin.job-no2 EQ w-ord.job-no2
                  AND fg-bin.loc     GE v-floc[1]
                  AND fg-bin.loc     LE v-floc[2]
                USE-INDEX job NO-LOCK:
              w-ord.onh-qty = w-ord.onh-qty + fg-bin.qty.
            end.
            ELSE /* w-ord.onh-qty = itemfg.q-onh. */
            FOR EACH fg-bin FIELDS(qty)
                WHERE fg-bin.company EQ cocode
                  AND fg-bin.i-no    EQ w-ord.i-no
                  AND fg-bin.loc     GE v-floc[1]
                  AND fg-bin.loc     LE v-floc[2]
                USE-INDEX i-no NO-LOCK:
              w-ord.onh-qty = w-ord.onh-qty + fg-bin.qty.
            end.
            v-comp-qty = 0.
            IF w-ord.job-no <> "" THEN DO:
                FIND FIRST tt-ord WHERE tt-ord.tt-recid = RECID(w-ord) NO-LOCK NO-ERROR.
                FOR EACH fg-rcpth NO-LOCK WHERE fg-rcpth.company = cocode
                                    AND fg-rcpth.i-no = w-ord.i-no
                                    AND fg-rcpth.job-no  EQ w-ord.job-no
                                    AND fg-rcpth.job-no2 EQ w-ord.job-no2
                                    AND fg-rcpth.rita-code = "R"
                                    AND ((AVAIL tt-ord AND int(fg-rcpth.po-no) = tt-ord.tt-po-no) OR 
                                         (AVAIL tt-ord AND tt-ord.tt-po-no = 0) OR NOT AVAIL tt-ord OR w-ord.job-no <> ""),
                    EACH fg-rdtlh FIELDS(qty) WHERE
                         fg-rdtlh.r-no = fg-rcpth.r-no AND
                         fg-rdtlh.rita-code = fg-rcpth.rita-code AND
                         fg-rdtlh.loc     GE v-floc[1] AND
                         fg-rdtlh.loc     LE v-floc[2]
                         NO-LOCK:
                    v-comp-qty = v-comp-qty + fg-rdtlh.qty.

                END.
            END.
            
            {oe/rep/oehots2.i "2" }
        END. /* ELSE v-sort = "R" */

 FOR EACH w-ord:
     DELETE w-ord.
 END.
 
  SESSION:SET-WAIT-STATE ("").
