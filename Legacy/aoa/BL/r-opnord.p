/* r-opnord.p */

/* temp-table definitions */
{aoa/tempTable/ttOpenOrderReport.i}

{sys/ref/CustList.i NEW}

DEFINE TEMP-TABLE tt-report NO-UNDO LIKE report 
    FIELD qty  LIKE oe-rell.qty
    FIELD qty2 LIKE oe-rel.qty
    .

DEFINE TEMP-TABLE tt-ord NO-UNDO
    FIELD tt-recid AS RECID
    FIELD tt-po-no LIKE po-ord.po-no
    .

/* Parameters Definitions ---                                           */
DEFINE OUTPUT PARAMETER TABLE FOR ttOpenOrderReport.
{aoa/includes/pOpenOrderReport.i}

/* subject business logic */
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
               tt-report.rec-id  = RECID(oe-rel)
               tt-report.qty2    = if oe-rel.link-no eq 0 then oe-rel.tot-qty else oe-rel.qty.
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

      FIND FIRST oe-rel WHERE RECID(oe-rel) EQ tt-report.rec-id NO-LOCK NO-ERROR.

      v-rel-no = 0.

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
            /*w-ord.qty       = oe-ordl.qty*/
            w-ord.qty       = tt-report.qty2
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
            w-ord.is-a-component = oe-ordl.is-a-component
            w-ord.xls-status = tt-report.key-06.
         
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
               b-w-ord.msf       = b-w-ord.rel-qty * itemfg.t-sqft / 1000
               b-w-ord.xls-status = tt-report.key-06.
            END.
         END.
      
         CREATE tt-ord.
         ASSIGN 
            tt-ord.tt-recid = RECID(w-ord)
            tt-ord.tt-po-no = oe-ordl.po-no-po.
      END.
   END. /* each tt-report */

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
        
        {oe/rep/oeopnord2N.i}
      /*END.*/
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
            FOR EACH fg-bin
                WHERE fg-bin.company EQ cocode
                  AND fg-bin.i-no    EQ w-ord.i-no
                  AND fg-bin.loc     GE v-floc[1]
                  AND fg-bin.loc     LE v-floc[2]
                USE-INDEX i-no NO-LOCK:
              w-ord.onh-qty = w-ord.onh-qty + fg-bin.qty.
              /*IF fg-bin.job-no  EQ w-ord.job-no 
                 AND fg-bin.job-no2 EQ w-ord.job-no2
                 THEN v-comp-qty = v-comp-qty + fg-bin.qty.*/
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
            
            {oe/rep/oeopnord2N.i "2" }
        END. /* ELSE v-sort = "R" */

 FOR EACH w-ord:
     DELETE w-ord.
 END.

PROCEDURE oeopnord2N:
IF v-first THEN 
   /*IF tb_exp-po THEN
      PUT UNFORMATTED
         "<FCourier New><B><C5><P26>Hots <P15>(0-Z-7)<P8>" skip(1)
         string(TODAY) AT 3 FORM "x(5)"  "Begin Date:" AT 30 string(begin_date) FORM "x(5)" "  End date:" string(end_date) FORM "x(5)" 
         "Page: " AT 145 PAGE-NUM FORM ">>9"SKIP
         "Rel Date Ord Date  Order# Cust PO#       Item Name                 Routing         Ven PO#     Brd Rcpt Qty Order  Qty Comp Qty Onhnd Ship City     Status"
       SKIP v-line SKIP.                                                                                                                        
   ELSE*/
      PUT UNFORMATTED
         "<FCourier New><B><C5><P26>Hots <P15>(0-Z-7)<P8>" skip(1)
         STRING(TODAY) AT 3 FORM "x(5)"  "Begin Date:" AT 30 string(begin_date) FORM "x(5)" "  End date:" string(end_date) FORM "x(5)" 
         "Page: " AT 145 PAGE-NUM FORM ">>9"SKIP
         /*"Rel Date Ord Date  Order# Cust PO#       Item Name                 Routing         Ven PO#     Brd Rcpt Qty Order  Qty Comp Qty Onhnd Ship City     Status"         */
          str-tit4
          SKIP str-tit5 SKIP.
         

v-first = NO.
IF AVAIL itemfg THEN DO:
   /* ROUTING,PO and Vend */
   ASSIGN 
      lv-board-po-no = 0
      lv-vend-no = ""
      ld-qty-rec = 0
      lv-routing = "".
   
   FOR EACH tt-fg-set:
      DELETE tt-fg-set.
   END.
   
   RELEASE job-hdr.
   RELEASE job.
   RELEASE reftable.
   
   IF TRIM(w-ord.job-no) EQ "" THEN
      FOR EACH job-hdr NO-LOCK WHERE 
               job-hdr.company EQ cocode
           AND job-hdr.ord-no  EQ w-ord.ord-no
           AND job-hdr.cust-no EQ w-ord.cust-no
           AND job-hdr.i-no    EQ w-ord.i-no
           AND job-hdr.opened  EQ YES
         BY ROWID(job-hdr) DESC:
         LEAVE.
      END.
   ELSE DO:
      FIND FIRST job-hdr WHERE 
                 job-hdr.company EQ cocode
             AND job-hdr.job-no  EQ w-ord.job-no
             AND job-hdr.job-no2 EQ w-ord.job-no2
             AND job-hdr.ord-no  EQ w-ord.ord-no
             AND job-hdr.i-no    EQ w-ord.i-no NO-LOCK NO-ERROR.
      IF NOT AVAIL job-hdr THEN
         FIND FIRST job-hdr WHERE 
                    job-hdr.company EQ cocode
                AND job-hdr.job-no  EQ w-ord.job-no
                AND job-hdr.job-no2 EQ w-ord.job-no2
                AND job-hdr.ord-no  EQ w-ord.ord-no NO-LOCK NO-ERROR.
   END.
   
   IF AVAIL job-hdr THEN
      FIND FIRST job WHERE 
                 job.company EQ job-hdr.company
             AND job.job     EQ job-hdr.job
             AND job.job-no  EQ job-hdr.job-no
             AND job.job-no2 EQ job-hdr.job-no2 NO-LOCK NO-ERROR.
      IF AVAIL job THEN DO: 
         IF (itemfg.isaset OR w-ord.is-a-component) AND
            CAN-FIND(FIRST reftable WHERE 
                           reftable.reftable EQ "jc/jc-calc.p"
                       AND reftable.company  EQ job.company
                       AND reftable.loc      EQ ""
                       AND reftable.code     EQ STRING(job.job,"999999999")) THEN
            FOR EACH reftable NO-LOCK WHERE 
                     reftable.reftable EQ "jc/jc-calc.p"
                 AND reftable.company  EQ job-hdr.company
                 AND reftable.loc      EQ ""
                 AND reftable.code     EQ STRING(job-hdr.job,"999999999")
                 AND ((reftable.code2  EQ w-ord.i-no AND w-ord.is-a-component) OR
                     (job-hdr.i-no    EQ w-ord.i-no AND NOT w-ord.is-a-component)):
               CREATE tt-fg-set.
               ASSIGN 
                  tt-fg-set.part-no      = reftable.code2
                  tt-fg-set.part-qty     = reftable.val[12]
                  tt-fg-set.part-qty-dec = reftable.val[13].
            END.
         ELSE DO:
            CREATE tt-fg-set.
            ASSIGN 
               tt-fg-set.part-no      = job-hdr.i-no
               tt-fg-set.part-qty     = job-hdr.frm
               tt-fg-set.part-qty-dec = job-hdr.blank-no.
         END.
         
         FOR EACH tt-fg-set
            BREAK BY tt-fg-set.part-qty
                  BY tt-fg-set.part-qty-dec:

            ll-po = NO.
            IF LAST-OF(tt-fg-set.part-qty) THEN
               FOR EACH po-ordl WHERE 
                        po-ordl.company   EQ job.company
                    AND po-ordl.job-no    EQ job.job-no
                    AND po-ordl.job-no2   EQ job.job-no2
                    AND po-ordl.s-num     EQ tt-fg-set.part-qty
                    AND po-ordl.item-type EQ YES USE-INDEX job-no NO-LOCK,
                  FIRST po-ord WHERE 
                        po-ord.company EQ po-ordl.company
                    AND po-ord.po-no   EQ po-ordl.po-no NO-LOCK,
                  FIRST ITEM WHERE 
                        item.company EQ po-ordl.company
                    AND item.i-no    EQ po-ordl.i-no
                    AND INDEX("1234BPR",item.mat-type) GT 0 NO-LOCK
                  BREAK BY po-ordl.po-no
                        BY po-ordl.i-no
                        BY po-ordl.rec_key:

                     ll-po = YES.
                     ASSIGN 
                        lv-board-po-no = po-ordl.po-no
                        lv-vend-no = po-ord.vend-no.
                     IF po-ordl.cons-uom EQ "EA" THEN 
                        ld-qty-rec = po-ordl.t-rec-qty.
                     ELSE 
                        RUN sys/ref/convquom.p(po-ordl.cons-uom,"EA",item.basis-w,po-ordl.s-len,po-ordl.s-wid,item.s-dep,po-ordl.t-rec-qty, output ld-qty-rec).
                     {sys/inc/roundup.i ld-qty-rec}
                     LEAVE.
               END.
               lv-routing = "".
               IF FIRST(tt-fg-set.part-qty) THEN
                  FOR EACH job-mch WHERE 
                           job-mch.company EQ job.company
                       AND job-mch.job     EQ job.job
                       AND job-mch.job-no  EQ job.job-no
                       AND job-mch.job-no2 EQ job.job-no2
                       AND job-mch.frm     EQ tt-fg-set.part-qty NO-LOCK
                     BREAK BY job-mch.line:

                     lv-routing = lv-routing + job-mch.m-code + ",".
                  END.
         END. /* each tt-fg-set*/
      END.  /* job*/
   
      IF lv-routing = "" AND itemfg.est-no <> "" THEN 
         FOR EACH est-op NO-LOCK WHERE 
                  est-op.company = itemfg.company
              AND est-op.est-no = itemfg.est-no
              AND est-op.line LT 500 :
              /*((ASI.est-op.qty eq est-qty.eqty and est.est-type ne 8) or
                 (ASI.est-op.qty eq lv-eqty and est.est-type ge 7)) NO-LOCK*/
            lv-routing = lv-routing + est-op.m-code + ",".
         END.

      /* spec notes */
      lv-text = "".
      IF tb_notes AND rd_lComments THEN 
         FOR EACH notes WHERE 
                  notes.rec_key   EQ itemfg.rec_key
              AND notes.note_type EQ "S"
              AND notes.note_code GE begin_spec
              AND notes.note_code LE end_spec NO-LOCK
            BREAK BY notes.note_code:
            
            IF FIRST(notes.note_code) THEN
               lv-text = TRIM(notes.note_code) + ":" + TRIM(notes.note_text).
            LEAVE.
         END.
      ELSE IF NOT rd_lComments THEN DO: 

         FOR EACH eb fields(test style) WHERE 
                  eb.company  EQ cocode
              AND eb.est-no   EQ oe-ordl.est-no
              AND eb.stock-no EQ oe-ordl.i-no NO-LOCK:
            ASSIGN 
               lv-text = eb.test.
            LEAVE.
         END.

         IF NOT AVAIL eb THEN
            FOR EACH eb fields(test style) WHERE 
                eb.company  EQ cocode AND
                eb.est-no   EQ itemfg.est-no AND
                eb.stock-no EQ itemfg.i-no
                NO-LOCK:
                ASSIGN 
                   lv-text = eb.test.
                LEAVE.
            END.

         ASSIGN 
            lv-text = (IF avail eb and itemfg.style eq "" THEN eb.style
                       ELSE itemfg.style)
                    + "  " + lv-text.  /* style + test */
      END.
  
   IF v-sort = "C" THEN DO:
      IF lv-prev-cust-name <> "" AND lv-prev-cust-name <> w-ord.cust-name THEN 
         PUT SKIP(1).
      IF lv-prev-cust-name <> w-ord.cust-name THEN 
         PUT "<B>" w-ord.cust-name FORM "x(30)" "</B>" SKIP.
   END.
  
   FIND FIRST shipto WHERE 
              shipto.company = cocode
          AND shipto.cust-no = w-ord.cust-no  
          AND shipto.ship-id = w-ord.ship-id NO-LOCK NO-ERROR.
  
   v-ship-city = IF AVAIL shipto AND shipto.ship-city <> "" THEN shipto.ship-city ELSE w-ord.ship-id.
   IF substring(lv-routing,15,1) = "," THEN 
      lv-routing = SUBSTRING(lv-routing,1,14).
  
   IF length(lv-routing) > 1 AND substring(lv-routing,LENGTH(lv-routing),1) = "," THEN  
      lv-routing = SUBSTRING(lv-routing,1,LENGTH(lv-routing) - 1).
   
   FIND FIRST tt-ord WHERE tt-ord.tt-recid = RECID(w-ord) NO-LOCK NO-ERROR.

   IF lv-vend-no = "" AND AVAIL tt-ord AND tt-ord.tt-po-no <> 0 THEN DO:
      FIND FIRST po-ord WHERE 
                 po-ord.company = cocode
             AND po-ord.po-no = tt-ord.tt-po-no NO-LOCK NO-ERROR.
      lv-vend-no = po-ord.vend-no.
   END.

   /*IF tb_exp-po THEN
      DISPLAY 
          w-ord.rel-date  
          w-ord.last-date FORMAT "99/99/99"    AT 10 
          w-ord.ord-no    FORMAT "999999"      AT 20  
          w-ord.po-num    FORMAT "x(15)"       AT 27 
          itemfg.i-name   FORMAT "x(25)"       AT 42
          lv-routing      FORMAT "x(15)"       AT 68           
          lv-vend-no      FORMAT "x(8)"        AT 85                                   
          ld-qty-rec      FORMAT "->>>>>>>>9"  AT 94 
          w-ord.ord-qty   FORMAT "->>>>>>>>9"  AT 104
          v-comp-qty      FORMAT "->>>>>>>>9"  AT 114
          w-ord.onh-qty   FORMAT "->>>>>>>>9"  AT 124
          v-ship-city     FORMAT "x(15)"       AT 135
          w-ord.xls-status                     AT 151
         WITH DOWN FRAME hots-po{1} NO-BOX STREAM-IO NO-LABEL WIDTH 250.
   ELSE*/
     /* DISPLAY 
          w-ord.rel-date  
          w-ord.last-date FORMAT "99/99/99"   AT 10  
          w-ord.ord-no    FORMAT "999999"     AT 20  
          w-ord.po-num    FORMAT "x(15)"      AT 27  
          itemfg.i-name   FORMAT "x(25)"      AT 42
          lv-routing      FORMAT "x(15)"      AT 68            
          lv-vend-no      FORMAT "x(8)"       AT 85  
          ld-qty-rec      FORMAT "->>>>>>>>9" AT 94 
          w-ord.ord-qty   FORMAT "->>>>>>>>9" AT 104
          v-comp-qty      FORMAT "->>>>>>>>9" AT 114
          w-ord.onh-qty   FORMAT "->>>>>>>>9" AT 124
          v-ship-city     FORMAT "x(15)"      AT 135
          w-ord.xls-status                    AT 151 
         WITH DOWN FRAME hots{1} NO-BOX STREAM-IO NO-LABEL WIDTH 250.*/

      ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
      
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:       
                         WHEN "item"    THEN cVarValue = IF AVAIL itemfg THEN  string(itemfg.i-no,"x(15)") ELSE "" .
                         WHEN "rel-date"    THEN cVarValue = IF w-ord.rel-date NE ? THEN string(date(w-ord.rel-date),"99/99/99") ELSE "" .
                         WHEN "ord-date"   THEN cVarValue = IF w-ord.last-date NE ? THEN string(w-ord.last-date,"99/99/99")  ELSE "".
                         WHEN "ord-no"   THEN cVarValue = STRING(w-ord.ord-no,"99999999").
                         WHEN "cust-po"  THEN cVarValue = STRING(w-ord.po-num,"x(15)") .
                         WHEN "item-name"   THEN cVarValue = IF AVAIL itemfg THEN STRING(itemfg.i-name,"x(25)") ELSE "" .
                         WHEN "rout"  THEN cVarValue = STRING(lv-routing,"x(15)") .
                         WHEN "vend-po"   THEN cVarValue = STRING(lv-vend-no,"x(8)") .
                         WHEN "brd-rc"  THEN cVarValue = STRING(ld-qty-rec,"->>>>>>>>9") .

                         WHEN "qty-ord"  THEN cVarValue = STRING(w-ord.ord-qty,"->>>>>>>>9") .
                         WHEN "qty-comp"   THEN cVarValue = STRING(v-comp-qty,"->>>>>>>>9") .
                         WHEN "qty-hand"  THEN cVarValue = STRING(w-ord.onh-qty,"->>>>>>>>9") .
                         WHEN "ship-city"   THEN cVarValue = STRING(v-ship-city,"x(15)") .
                         WHEN "stat"  THEN cVarValue = STRING(w-ord.xls-status,"x(6)") .
                         
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED cDisplay SKIP.
            IF tb_excel THEN DO:
                 /* last time we use lv-text here, put a ',' in pos 5 for excel */
                IF trim(lv-text) <> "" and NOT rd_lComments THEN 
                    ASSIGN lv-text = substring(lv-text,1,4) + '","' + substring(lv-text,6).
                PUT STREAM st-excel UNFORMATTED  
                       cExcelDisplay SKIP.
             END.

   /*IF tb_excel THEN DO:      
      /* last time we use lv-text here, put a ',' in pos 5 for excel */
      IF trim(lv-text) <> "" and NOT rd_lComments THEN 
         ASSIGN lv-text = substring(lv-text,1,4) + '","' + substring(lv-text,6).
      PUT STREAM st-excel UNFORMATTED
         '"' w-ord.rel-date       '",'              
         '"' w-ord.last-date      '",'
         '"' w-ord.ord-no         '",'
         '"' w-ord.po-num         '",'
         '"' itemfg.i-name          '",'              
         '"' lv-routing           '",'
         '"' w-ord.rel-no         '",'
         '"' lv-vend-no           '",'
         '"' ld-qty-rec           '",'
         '"' w-ord.ord-qty        '",'
         '"' v-comp-qty           '",'
         '"' w-ord.onh-qty        '",'
         '"'  v-ship-city         '",'                                               
         '"' tt-report.key-06    '",' 
         SKIP.
   END.*/
   
 
   /*IF LAST-OF(w-ord.cust-name) THEN PUT SKIP(1).*/
   ASSIGN
      v-comp-qty = 0
      lv-prev-cust-name = w-ord.cust-name     
      lv-first-last-set = NO.
   
   FOR EACH tt-fg-set 
      BREAK BY tt-fg-set.part-qty
            BY tt-fg-set.part-qty-dec:
      
      lv-first-last-set = FIRST(tt-fg-set.part-qty) AND LAST(tt-fg-set.part-qty).
      
      IF NOT lv-first-last-set THEN DO:
         PUT SPACE(5)
            "S/B: "
            TRIM(STRING(tt-fg-set.part-qty,">>")) + "/" + TRIM(STRING(tt-fg-set.part-qty-dec,">>"))   FORMAT "x(5)"
            SPACE(1).
         ll-po = NO.
         IF LAST-OF(tt-fg-set.part-qty) THEN
            FOR EACH po-ordl WHERE 
                     po-ordl.company   EQ job.company
                 AND po-ordl.job-no    EQ job.job-no
                 AND po-ordl.job-no2   EQ job.job-no2
                 AND po-ordl.s-num     EQ tt-fg-set.part-qty
                 AND po-ordl.item-type EQ YES
               USE-INDEX job-no NO-LOCK,
               FIRST po-ord WHERE 
                     po-ord.company EQ po-ordl.company
                 AND po-ord.po-no   EQ po-ordl.po-no NO-LOCK,
               FIRST item WHERE 
                     item.company EQ po-ordl.company
                 AND item.i-no    EQ po-ordl.i-no
                 AND INDEX("1234BPR",item.mat-type) GT 0 NO-LOCK
               BREAK BY po-ordl.po-no
                     BY po-ordl.i-no
                     BY po-ordl.rec_key:
               
               ll-po = YES.
               IF po-ordl.pr-qty-uom EQ "EA" THEN
                  ld-qty-ord = po-ordl.ord-qty.
               ELSE
                  RUN sys/ref/convquom.p(po-ordl.pr-qty-uom, "EA",item.basis-w,po-ordl.s-len,po-ordl.s-wid,item.s-dep,po-ordl.ord-qty, output ld-qty-ord).
               {sys/inc/roundup.i ld-qty-ord}
    
               IF po-ordl.cons-uom EQ "EA" THEN
                  ld-qty-rec = po-ordl.t-rec-qty.
               ELSE
                  RUN sys/ref/convquom.p(po-ordl.cons-uom, "EA",item.basis-w,po-ordl.s-len,po-ordl.s-wid,item.s-dep,po-ordl.t-rec-qty, output ld-qty-rec).
               {sys/inc/roundup.i ld-qty-rec}
    
               /*IF NOT LAST(po-ordl.po-no) THEN PUT SPACE(16).*/
    
               PUT 
                  "Brd PO#: " 
                  TRIM(STRING(po-ordl.po-no,">>>>>>>>")) FORMAT "x(8)"
                  SPACE(1)
                  "Vendor: "
                  po-ord.vend-no                         FORMAT "x(8)"
                  SPACE(1)
                  "Qty Rec'd: "
                  TRIM(STRING(ld-qty-rec,">>>,>>>,>>9")) FORMAT "x(11)"
                  SPACE(1).
    
               IF NOT LAST(po-ordl.po-no) THEN PUT SKIP.
            END.
       
         IF NOT ll-po THEN PUT SPACE(58).

         FOR EACH job-mch NO-LOCK WHERE 
                  job-mch.company EQ job.company
              AND job-mch.job     EQ job.job
              AND job-mch.job-no  EQ job.job-no
              AND job-mch.job-no2 EQ job.job-no2
              AND job-mch.frm     EQ tt-fg-set.part-qty
            BREAK BY job-mch.line:
          
            IF FIRST(job-mch.line) THEN 
               PUT "Routing: ".
            PUT UNFORMATTED job-mch.m-code.
            
            IF NOT LAST(job-mch.line) THEN 
               PUT ", ".
         END.
         PUT SKIP.
       END. /* not first-last */
   END. /* for each tt-fg-set*/

   FOR EACH tt-formtext:
      DELETE tt-formtext.
   END.
END.
END PROCEDURE.
    
{aoa/BL/pBuildCustList.i}
