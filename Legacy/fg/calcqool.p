DEF INPUT  PARAM ip-rowid AS ROWID NO-UNDO.
DEF INPUT  PARAM ip-loc   LIKE itemfg-loc.loc NO-UNDO.
DEF OUTPUT PARAM op-q-ono LIKE itemfg.q-ono NO-UNDO.

{sys/inc/var.i NEW SHARED}

DEF BUFFER b-itemfg FOR itemfg.
DEF BUFFER b-po-ordl FOR po-ordl.

DEF VAR ld-qty LIKE fg-act.qty NO-UNDO.
DEF VAR ldrcv-qty LIKE fg-act.qty NO-UNDO.
DEF VAR li-loop AS INT NO-UNDO.
DEF VAR v-set-job-qty AS INT NO-UNDO.
DEF VAR v-set-rcv-qty AS INT NO-UNDO.
DEF VAR v-set-item    LIKE itemfg.i-no NO-UNDO.
DEF VAR v-part-qty-dec AS DEC NO-UNDO.
DEF VAR v-part-qty AS INT NO-UNDO.

{fg/fullset.i NEW}

DEF BUFFER bf-itemfg FOR itemfg.
DEF BUFFER bf-est    FOR est.
DEF BUFFER bf-eb     FOR eb.
DEF BUFFER bf-job-hdr FOR job-hdr.

FIND itemfg WHERE ROWID(itemfg) EQ ip-rowid NO-LOCK NO-ERROR.
IF AVAIL itemfg THEN DO:

   FIND FIRST job-hdr WHERE
        job-hdr.company EQ itemfg.company AND
        job-hdr.i-no    EQ itemfg.i-no AND
        job-hdr.loc     EQ ip-loc AND
        job-hdr.opened  EQ YES
        NO-LOCK NO-ERROR.

   IF AVAIL job-hdr THEN DO:
     
      FOR EACH job-hdr WHERE
          job-hdr.company EQ itemfg.company AND
          job-hdr.i-no    EQ itemfg.i-no AND
          job-hdr.loc     EQ ip-loc AND
          job-hdr.opened  EQ YES AND
          CAN-FIND(FIRST job WHERE
                         job.company EQ job-hdr.company AND
                         job.job     EQ job-hdr.job AND                         
                         job.job-no  EQ job-hdr.job-no AND
                         job.job-no2 EQ job-hdr.job-no2)
        USE-INDEX i-no
        NO-LOCK:
      
        RELEASE eb.

        IF NOT itemfg.isaset THEN
           FIND FIRST eb WHERE 
                eb.company  EQ job-hdr.company AND
                eb.est-no   EQ job-hdr.est-no AND
                eb.form-no  EQ job-hdr.frm AND
                eb.blank-no EQ job-hdr.blank-no
                NO-LOCK NO-ERROR.
     
        IF NOT(itemfg.isaset OR (AVAIL eb AND NOT eb.pur-man) OR
               (NOT AVAIL eb AND NOT itemfg.pur-man)) THEN
           NEXT.

        ld-qty = 0.
        FOR EACH fg-act fields(qty) WHERE
            fg-act.company EQ job-hdr.company AND
            fg-act.job     EQ job-hdr.job AND
            fg-act.job-no  EQ job-hdr.job-no AND
            fg-act.job-no2 EQ job-hdr.job-no2 AND           
            fg-act.i-no    EQ job-hdr.i-no AND
            (IF itemfg.alloc = ? THEN fg-act.qty GT 0
             ELSE TRUE)
            NO-LOCK:
            ld-qty = ld-qty + fg-act.qty.
        END.
      
        IF ld-qty LT job-hdr.qty THEN
           op-q-ono = op-q-ono + (job-hdr.qty - ld-qty).
        
      END. /* FOR EACH job-hdr */      
      
      FOR EACH reftable NO-LOCK
           WHERE reftable.reftable EQ "jc/jc-calc.p"
             AND reftable.company  EQ itemfg.company
             AND reftable.loc      EQ ""
             AND reftable.code2    EQ itemfg.i-no
           USE-INDEX code2,
       EACH job NO-LOCK
          WHERE job.company EQ itemfg.company
            AND job.job EQ INTEGER(reftable.CODE)
            AND job.opened  EQ YES 
          USE-INDEX job,
       FIRST job-hdr NO-LOCK
          WHERE job-hdr.company EQ job.company
            AND job-hdr.job     EQ job.job
            AND job-hdr.job-no  EQ job.job-no
            AND job-hdr.job-no2 EQ job.job-no2
          USE-INDEX job-no,
       FIRST b-itemfg NO-LOCK
          WHERE b-itemfg.company EQ job-hdr.company
            AND b-itemfg.i-no    EQ job-hdr.i-no
            AND b-itemfg.isaset  EQ YES:
      
          RUN fg/fullset.p (ROWID(b-itemfg)).
          
          FOR EACH tt-fg-set WHERE tt-fg-set.part-no EQ reftable.code2:
              ld-qty = 0.
              FOR EACH fg-act FIELDS(qty) NO-LOCK
                  WHERE fg-act.company EQ job-hdr.company
                    AND fg-act.job     EQ job-hdr.job
                    AND fg-act.job-no  EQ job-hdr.job-no
                    AND fg-act.job-no2 EQ job-hdr.job-no2
                    AND fg-act.i-no    EQ tt-fg-set.part-no:
                    ld-qty = ld-qty + fg-act.qty.
              END. /* FOR EACH fg-act */
              IF ld-qty LT job-hdr.qty * tt-fg-set.part-qty-dec THEN
                 op-q-ono = op-q-ono + ((job-hdr.qty * tt-fg-set.part-qty-dec) - 
                                         ld-qty).
              
          END. /* FOR EACH tt-fg-set*/
      END. /* FOR EACH job */
      
   END. /* AVAIL job-hdr */
   ELSE 
   IF NOT AVAIL job-hdr THEN DO:
                                 
      FOR EACH bf-eb FIELDS(est-no) WHERE
          bf-eb.company  EQ itemfg.company AND
          bf-eb.stock-no EQ itemfg.i-no   AND
          bf-eb.loc      EQ ip-loc
          NO-LOCK:

          FOR EACH job-hdr FIELDS(company i-no) WHERE
              job-hdr.company EQ itemfg.company AND
              job-hdr.est-no  EQ bf-eb.est-no AND
              job-hdr.loc     EQ ip-loc AND
              job-hdr.opened  EQ YES
              NO-LOCK:
          
              FIND FIRST bf-itemfg WHERE
                   bf-itemfg.company EQ job-hdr.company AND
                   bf-itemfg.i-no    EQ job-hdr.i-no AND
                   bf-itemfg.isaset
                   NO-LOCK NO-ERROR.
          
              IF AVAIL bf-itemfg THEN LEAVE.
          END.
      END.
     
      IF AVAIL bf-itemfg THEN DO:
          
        IF bf-itemfg.alloc EQ YES THEN DO:
         FOR EACH job-hdr WHERE
             job-hdr.company EQ itemfg.company AND
             job-hdr.i-no    EQ bf-itemfg.i-no AND
             job-hdr.loc     EQ ip-loc AND
             job-hdr.opened  EQ YES AND
             CAN-FIND(FIRST job WHERE
             job.company EQ job-hdr.company AND
             job.job     EQ job-hdr.job AND
             job.job-no  EQ job-hdr.job-no AND
             job.job-no2 EQ job-hdr.job-no2)
             NO-LOCK
             USE-INDEX i-no:
         
             FIND FIRST bf-est WHERE
                  bf-est.company EQ job-hdr.company AND
                  bf-est.est-no EQ job-hdr.est-no
                  NO-LOCK NO-ERROR.

             IF NOT AVAIL bf-est THEN NEXT.

             IF bf-est.est-type NE 6 AND bf-est.est-type NE 2 THEN
                FIND FIRST eb WHERE
                     eb.company  EQ job-hdr.company AND
                     eb.est-no   EQ job-hdr.est-no AND
                     eb.form-no  EQ job-hdr.frm AND
                     eb.blank-no EQ job-hdr.blank-no
                     NO-LOCK NO-ERROR.
             ELSE
             DO:
                FIND FIRST eb WHERE
                     eb.company  EQ job-hdr.company AND
                     eb.est-no   EQ job-hdr.est-no AND
                     eb.form-no  EQ job-hdr.frm AND
                     eb.stock-no EQ itemfg.i-no
                     NO-LOCK NO-ERROR.

                IF NOT AVAIL eb THEN
                   FIND FIRST eb WHERE
                     eb.company  EQ job-hdr.company AND
                     eb.est-no   EQ job-hdr.est-no AND
                     eb.stock-no EQ itemfg.i-no
                     NO-LOCK NO-ERROR.
             END.

             IF NOT((AVAIL eb AND NOT eb.pur-man) OR
                    (NOT AVAIL eb AND NOT itemfg.pur-man)) THEN
                NEXT.

             v-part-qty = 1.
             FIND FIRST fg-set
                 where fg-set.company eq itemfg.company
                   and fg-set.part-no eq itemfg.i-no
                 NO-LOCK NO-ERROR.
             IF AVAIL(fg-set) THEN DO:
                  {sys/inc/part-qty.i v-part-qty fg-set}
                   v-part-qty = 
                     (IF AVAIL eb AND eb.spare-char-2 = "Y" THEN 1.0 ELSE v-part-qty).
   
             END.
    
             ld-qty = 0.
             
             FOR EACH fg-act fields(qty) NO-LOCK
               WHERE fg-act.company EQ job-hdr.company
                 AND fg-act.job     EQ job-hdr.job
                 AND fg-act.job-no  EQ job-hdr.job-no
                 AND fg-act.job-no2 EQ job-hdr.job-no2
                 AND fg-act.i-no    EQ job-hdr.i-no:
               ld-qty = ld-qty + fg-act.qty.
             END.
             IF ld-qty LT job-hdr.qty THEN
                op-q-ono = op-q-ono + ((job-hdr.qty - ld-qty) * v-part-qty).
             
         END. /* FOR EACH job-hdr */
             
         FOR EACH job NO-LOCK
           WHERE job.company EQ itemfg.company
             AND job.opened  EQ YES,
          FIRST job-hdr NO-LOCK
           WHERE job-hdr.company EQ job.company
             AND job-hdr.job     EQ job.job
             AND job-hdr.job-no  EQ job.job-no
             AND job-hdr.job-no2 EQ job.job-no2
             AND job-hdr.loc     EQ ip-loc,
          FIRST b-itemfg NO-LOCK
           WHERE b-itemfg.company EQ job-hdr.company
             AND b-itemfg.i-no    EQ job-hdr.i-no
             AND b-itemfg.isaset  EQ YES,
             EACH reftable NO-LOCK
           WHERE reftable.reftable EQ "jc/jc-calc.p"
             AND reftable.company  EQ job-hdr.company
             AND reftable.loc      EQ ""
             AND reftable.code     EQ STRING(job-hdr.job,"999999999")
             AND reftable.code2    EQ itemfg.i-no:
         
           RUN fg/fullset.p (ROWID(b-itemfg)).
         
           FOR EACH tt-fg-set WHERE tt-fg-set.part-no EQ reftable.code2:
               ld-qty = 0.
               FOR EACH fg-act FIELDS(qty) WHERE
                   fg-act.company EQ job-hdr.company AND
                   fg-act.job     EQ job-hdr.job AND
                   fg-act.job-no  EQ job-hdr.job-no AND
                   fg-act.job-no2 EQ job-hdr.job-no2 AND
                   fg-act.i-no    EQ tt-fg-set.part-no 
                                      NO-LOCK:
                   ld-qty = ld-qty + fg-act.qty.
               END.  /* FOR EACH fg-act */
               IF ld-qty LT job-hdr.qty * tt-fg-set.part-qty-dec THEN
                  op-q-ono = op-q-ono + ((job-hdr.qty * tt-fg-set.part-qty-dec) - 
                                         ld-qty).
               
           END. /* FOR EACH tt-fg-set*/
         END. /* FOR EACH job */
        END.  /* if not assembled set */
        ELSE DO:
            /* assembled set logic */
               
          v-set-item = bf-itemfg.i-no.
          FOR EACH job-hdr WHERE
             job-hdr.company EQ itemfg.company AND
             job-hdr.i-no    EQ bf-itemfg.i-no AND
             job-hdr.loc     EQ ip-loc AND
             job-hdr.opened  EQ YES AND
             CAN-FIND(FIRST job WHERE
             job.company EQ job-hdr.company AND
             job.job     EQ job-hdr.job AND
             job.job-no  EQ job-hdr.job-no AND
             job.job-no2 EQ job-hdr.job-no2)
             NO-LOCK
             USE-INDEX i-no:
         
             FIND FIRST bf-est WHERE
                  bf-est.company EQ job-hdr.company AND
                  bf-est.est-no EQ job-hdr.est-no
                  NO-LOCK NO-ERROR.

             IF NOT AVAIL bf-est THEN NEXT.

             IF bf-est.est-type NE 6 AND bf-est.est-type NE 2 THEN
                FIND FIRST eb WHERE
                     eb.company  EQ job-hdr.company AND
                     eb.est-no   EQ job-hdr.est-no AND
                     eb.form-no  EQ job-hdr.frm AND
                     eb.blank-no EQ job-hdr.blank-no
                     NO-LOCK NO-ERROR.
             ELSE
             DO:
                FIND FIRST eb WHERE
                     eb.company  EQ job-hdr.company AND
                     eb.est-no   EQ job-hdr.est-no AND
                     eb.form-no  EQ job-hdr.frm AND
                     eb.stock-no EQ itemfg.i-no
                     NO-LOCK NO-ERROR.

                IF NOT AVAIL eb THEN
                   FIND FIRST eb WHERE
                     eb.company  EQ job-hdr.company AND
                     eb.est-no   EQ job-hdr.est-no AND
                     eb.stock-no EQ itemfg.i-no
                     NO-LOCK NO-ERROR.
             END.

             IF NOT((AVAIL eb AND NOT eb.pur-man) OR
                    (NOT AVAIL eb AND NOT itemfg.pur-man)) THEN
                NEXT.

             ld-qty = 0.
             
             FOR EACH fg-act fields(qty) NO-LOCK
               WHERE fg-act.company EQ job-hdr.company
                 AND fg-act.job     EQ job-hdr.job
                 AND fg-act.job-no  EQ job-hdr.job-no
                 AND fg-act.job-no2 EQ job-hdr.job-no2
                 AND fg-act.i-no    EQ job-hdr.i-no                 
                 AND fg-act.qty     GT 0:
                 /* Note: For assembled items, fg-act has both a negative
                          and positive record */
               ld-qty = ld-qty + fg-act.qty.
             END.
             IF ld-qty LT job-hdr.qty THEN
                ASSIGN v-set-job-qty = job-hdr.qty
                       v-set-rcv-qty =  ld-qty.
         END. /* FOR EACH job-hdr */
         FOR EACH job NO-LOCK
           WHERE job.company EQ itemfg.company
             AND job.opened  EQ YES,
          FIRST job-hdr NO-LOCK
           WHERE job-hdr.company EQ job.company
             AND job-hdr.i-no    EQ v-set-item
             AND job-hdr.job     EQ job.job
             AND job-hdr.loc     EQ ip-loc
             AND job-hdr.job-no  EQ job.job-no
             AND job-hdr.job-no2 EQ job.job-no2,
          FIRST b-itemfg NO-LOCK
           WHERE b-itemfg.company EQ job-hdr.company
             AND b-itemfg.i-no    EQ job-hdr.i-no
             AND b-itemfg.isaset  EQ YES:
           RUN fg/fullset.p (ROWID(b-itemfg)).
           
           FOR EACH fg-set WHERE fg-set.part-no EQ itemfg.i-no:
                {sys/inc/part-qty.i v-part-qty-dec fg-set}
               IF v-set-rcv-qty LT v-set-job-qty THEN
                  op-q-ono = op-q-ono + ((v-set-job-qty - v-set-rcv-qty) * v-part-qty-dec).               
           END. /* FOR EACH tt-fg-set*/
         END. /* FOR EACH job */           
        END.
      END. /* IF AVAIL bf-itemfg */   
   END. /* NOT AVAIL job-hdr */
  
   /*** itemfg.q-ono from purchase orders ***/
   FOR EACH po-ordl NO-LOCK
       WHERE po-ordl.company   EQ itemfg.company
         AND po-ordl.i-no      EQ itemfg.i-no
         AND po-ordl.job-no    EQ ""
         AND po-ordl.item-type EQ NO
         AND po-ordl.opened    EQ YES
         AND po-ordl.stat      NE "C",
       FIRST po-ord WHERE
             po-ord.company EQ po-ordl.company AND
             po-ord.po-no   EQ po-ordl.po-no AND
             po-ord.loc     EQ ip-loc
             NO-LOCK:
  
     li-loop = 0.
     DO WHILE li-loop LT 1000:
       li-loop = li-loop + 1.
  
       FIND b-po-ordl WHERE ROWID(b-po-ordl) EQ ROWID(po-ordl)
           EXCLUSIVE NO-ERROR NO-WAIT.
  
       IF AVAIL b-po-ordl THEN DO TRANSACTION:
         IF b-po-ordl.cons-uom EQ b-po-ordl.pr-qty-uom THEN
           b-po-ordl.cons-qty = b-po-ordl.ord-qty.
         ELSE
           RUN sys/ref/convquom.p(b-po-ordl.pr-qty-uom, b-po-ordl.cons-uom,
                                  0, b-po-ordl.s-len, b-po-ordl.s-wid, 0,
                                  b-po-ordl.ord-qty, OUTPUT b-po-ordl.cons-qty).
         li-loop = 1000.
       END.
     END.
   
     FIND b-po-ordl WHERE ROWID(b-po-ordl) EQ ROWID(po-ordl) NO-LOCK NO-ERROR.
  
     ldrcv-qty = 0.
     FOR EACH fg-rcpth 
         WHERE fg-rcpth.company   EQ po-ordl.company
           AND fg-rcpth.i-no      EQ po-ordl.i-no
           AND fg-rcpth.rita-code EQ "R"
           AND fg-rcpth.po-no     EQ STRING(po-ordl.po-no)
         NO-LOCK,
           FIRST fg-rctd 
             WHERE fg-rctd.r-no EQ fg-rcpth.r-no
               AND fg-rctd.i-no EQ fg-rcpth.i-no
           NO-LOCK.

         ldrcv-qty = ldrcv-qty + fg-rctd.qty.
     END.

     IF b-po-ordl.cons-uom EQ "EA" THEN
        ld-qty = b-po-ordl.cons-qty.
     ELSE
        RUN sys/ref/convquom.p(b-po-ordl.cons-uom, "EA",
                               0, b-po-ordl.s-len, b-po-ordl.s-wid, 0,
                               b-po-ordl.cons-qty, OUTPUT ld-qty).
   
     IF ld-qty - ldrcv-qty GT 0 THEN
        op-q-ono = op-q-ono + (ld-qty - ldrcv-qty).
   
     IF op-q-ono LT 0 THEN op-q-ono = 0.
   END.

END. /* if avail itemfg */
