/* fg/fg-post.i   in GUI  copied and changed */ 

  RELEASE prod.
  FIND FIRST prodl
      WHERE prodl.company EQ cocode
         AND prodl.procat  EQ itemfg.procat
         AND CAN-FIND(FIRST prod
                      WHERE prod.company EQ cocode
                        AND prod.prolin  EQ prodl.prolin)
      NO-LOCK NO-ERROR.

  IF AVAIL prodl THEN
  FIND FIRST prod
      WHERE prod.company EQ cocode
        AND prod.prolin  EQ prodl.prolin
      NO-LOCK NO-ERROR.

  v-newhdr = NO. 
  RELEASE po-ord.
  IF {1}.rita-code NE "E" AND {1}.rita-code NE "I" AND int({1}.po-no) NE 0 THEN
      FIND FIRST po-ord   WHERE po-ord.company EQ cocode
                            AND po-ord.po-no   EQ int({1}.po-no)
          NO-LOCK NO-ERROR.
          
 /*     for each {2} where {2}.r-no eq {1}.r-no:  */
  RELEASE po-ordl.
  IF AVAIL po-ord THEN
        FIND FIRST po-ordl WHERE po-ordl.company   EQ cocode
                             AND po-ordl.po-no     EQ po-ord.po-no
                             AND po-ordl.i-no      EQ {1}.i-no
                             AND po-ordl.deleted   EQ NO
                             AND po-ordl.item-type EQ NO
                             AND po-ordl.stat NE "C"
             USE-INDEX po-no NO-ERROR.
        IF NOT AVAIL po-ordl THEN
        FIND FIRST po-ordl  WHERE po-ordl.company   EQ cocode
                              AND po-ordl.po-no     EQ po-ord.po-no
                              AND po-ordl.i-no      EQ {1}.i-no
                              AND po-ordl.deleted   EQ NO
                              AND po-ordl.item-type EQ NO
                        NO-ERROR.
  
  IF {1}.inv-no GT 0 AND "{1}" = "w-fg-rctd" THEN
    ASSIGN
    {1}.t-qty = {1}.inv-no
    {1}.inv-no = 0.

  IF {1}.rita-code EQ "I" THEN DO:
    IF {1}.job-no = "" THEN  /* for return without job#*/
    FOR EACH loadtag WHERE loadtag.company = g_company
                       AND loadtag.ITEM-type = NO
                       AND loadtag.tag-no = {1}.tag NO-LOCK,
        EACH fg-bin
      WHERE fg-bin.company EQ cocode
        AND fg-bin.i-no    EQ {1}.i-no
        AND fg-bin.tag     EQ ""
        AND fg-bin.qty     GE {2}.t-qty
      NO-LOCK,

      EACH fg-rcpth
      WHERE fg-rcpth.company EQ cocode
        AND fg-rcpth.i-no    EQ fg-bin.i-no
        AND fg-rcpth.job-no  EQ fg-bin.job-no
        AND fg-rcpth.job-no2 EQ fg-bin.job-no2
      USE-INDEX i-no NO-LOCK,

      FIRST fg-rdtlh
      WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
        AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
        AND fg-rdtlh.loc       EQ fg-bin.loc
        AND fg-rdtlh.loc-bin   EQ fg-bin.loc-bin
        AND fg-rdtlh.tag       EQ fg-bin.tag
      USE-INDEX rm-rdtl

      BY fg-rcpth.trans-date
      BY fg-rcpth.r-no:
    
    ASSIGN
     {1}.rita-code = "T"
     {2}.loc2      = {2}.loc
     {2}.loc-bin2  = {2}.loc-bin
     {2}.tag2      = {2}.tag
     {2}.loc       = fg-bin.loc
     {2}.loc-bin   = fg-bin.loc-bin
     {2}.tag       = fg-bin.tag
     {2}.cust-no   = fg-bin.cust-no
     {1}.job-no = fg-bin.job-no
     {1}.job-no2 = fg-bin.job-no2  .
     LEAVE.
    END. /* {1}.job-no = ""*/
    ELSE 
    FOR EACH fg-bin
      WHERE fg-bin.company EQ cocode
        AND fg-bin.i-no    EQ {1}.i-no
        AND fg-bin.job-no  EQ {1}.job-no
        AND fg-bin.job-no2 EQ {1}.job-no2
        AND fg-bin.tag     EQ ""
        AND fg-bin.qty     GE {2}.t-qty
      NO-LOCK,

      EACH fg-rcpth
      WHERE fg-rcpth.company EQ cocode
        AND fg-rcpth.i-no    EQ fg-bin.i-no
        AND fg-rcpth.job-no  EQ fg-bin.job-no
        AND fg-rcpth.job-no2 EQ fg-bin.job-no2
      USE-INDEX i-no NO-LOCK,

      FIRST fg-rdtlh
      WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
        AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
        AND fg-rdtlh.loc       EQ fg-bin.loc
        AND fg-rdtlh.loc-bin   EQ fg-bin.loc-bin
        AND fg-rdtlh.tag       EQ fg-bin.tag
      USE-INDEX rm-rdtl

      BY fg-rcpth.trans-date
      BY fg-rcpth.r-no:

    ASSIGN
     {1}.rita-code = "T"
     {2}.loc2      = {2}.loc
     {2}.loc-bin2  = {2}.loc-bin
     {2}.tag2      = {2}.tag
     {2}.loc       = fg-bin.loc
     {2}.loc-bin   = fg-bin.loc-bin
     {2}.tag       = fg-bin.tag
     {2}.cust-no   = fg-bin.cust-no.

     LEAVE.
    END.
  END.
  
  IF {1}.rita-code NE "I" THEN
  DO:

  /** Adjusting the Finish Good item quantitys **/
  IF {1}.rita-code EQ "R" THEN DO:           /** RECEIPTS **/
          v-reduce-qty = 0.

          RELEASE job.

          IF AVAIL po-ordl THEN DO:
            IF LOOKUP(po-ordl.cons-uom,fg-uom-list) GT 0 THEN
              v-reduce-qty = po-ordl.cons-qty.
            ELSE
            IF po-ordl.cons-qty NE 0 THEN
              RUN sys/ref/convquom.p(INPUT po-ordl.cons-uom,
                                     INPUT "EA", INPUT 0,
                                     INPUT po-ordl.s-len,
                                     INPUT po-ordl.s-wid,
                                     INPUT 0,
                                     INPUT po-ordl.cons-qty,
                                     OUTPUT v-reduce-qty).

            ASSIGN v-overrun-qty     = v-reduce-qty * (1 - (po-ordl.over-pct / 100))
                   v-underrun-qty    = v-reduce-qty * (1 - (po-ordl.under-pct / 100))
                   po-ordl.t-rec-qty = po-ordl.t-rec-qty + {1}.t-qty.

            IF po-ordl.t-rec-qty GE v-underrun-qty THEN DO:
               ASSIGN  v-reduce-qty = min(v-reduce-qty,v-reduce-qty - po-ordl.t-rec-qty + {1}.t-qty)
                       po-ordl.stat = "C".       
               /*find first b-po-ordl  where b-po-ordl.company eq po-ord.company
                                       and b-po-ordl.po-no   eq po-ord.po-no
                                        and b-po-ordl.stat    ne "C"
                                        and b-po-ordl.deleted eq no
                                        and recid(b-po-ordl)  ne recid(po-ordl)
                              no-lock no-error.
               if not avail b-po-ordl then po-ord.stat = "C".*/
            END.
            ELSE po-ordl.stat = "P".
            IF v-reduce-qty LT 0 THEN v-reduce-qty = 0.
            IF po-ordl.stat NE "C" THEN  v-reduce-qty = min({1}.t-qty,v-reduce-qty). 

            {1}.invoiced = YES.
          END.   /* avail po-ordl */

          ELSE  
          IF {1}.job-no NE "" THEN
          FIND FIRST job NO-LOCK
              WHERE job.company EQ cocode
                AND job.job-no  EQ {1}.job-no
                AND job.job-no2 EQ {1}.job-no2
                AND CAN-FIND(FIRST job-hdr
                             WHERE job-hdr.company EQ job.company
                               AND job-hdr.job     EQ job.job
                               AND job-hdr.job-no  EQ job.job-no
                               AND job-hdr.job-no2 EQ job.job-no2)
              NO-ERROR.

          IF AVAIL job THEN DO:
            v-est-no = job.est-no.
                      
            IF job.opened THEN
            FOR EACH job-hdr NO-LOCK
                WHERE job-hdr.company EQ job.company
                  AND job-hdr.job     EQ job.job
                  AND job-hdr.job-no  EQ job.job-no
                  AND job-hdr.job-no2 EQ job.job-no2
                  AND job-hdr.i-no    EQ {1}.i-no:

              ACCUMULATE job-hdr.qty (TOTAL).
            END.

            IF (ACCUM TOTAL job-hdr.qty) GT 0 THEN DO:
              FOR EACH fg-act NO-LOCK
                  WHERE fg-act.company EQ job.company
                    AND fg-act.job-no  EQ job.job-no
                    AND fg-act.job-no2 EQ job.job-no2
                    AND fg-act.i-no    EQ {1}.i-no:
                ACCUMULATE fg-act.qty (TOTAL).
              END.
            
              v-reduce-qty = (ACCUM TOTAL fg-act.qty) + {2}.t-qty.
            
              IF v-reduce-qty GT (ACCUM TOTAL job-hdr.qty) THEN
                v-reduce-qty = {2}.t-qty -
                               (v-reduce-qty - (ACCUM TOTAL job-hdr.qty)).
              ELSE             
                v-reduce-qty = v-reduce-qty - (ACCUM TOTAL fg-act.qty).
            END.
 
            FIND FIRST sys-ctrl NO-LOCK
                WHERE sys-ctrl.company EQ job.company
                  AND sys-ctrl.name    EQ "AUTOISSU"
                NO-ERROR.
                
            IF AVAIL sys-ctrl AND sys-ctrl.char-fld EQ "FGPost" THEN DO:
              v-one-item = TRIM(job.est-no) NE "" AND
                           CAN-FIND(FIRST est
                                    WHERE est.company   EQ job.company
                                      AND est.est-no    EQ job.est-no
                                      AND (est.est-type EQ 2 OR
                                           est.est-type EQ 6)).

              FOR EACH job-hdr NO-LOCK
                  WHERE job-hdr.company EQ job.company
                    AND job-hdr.job     EQ job.job
                    AND job-hdr.job-no  EQ job.job-no
                    AND job-hdr.job-no2 EQ job.job-no2
                    AND job-hdr.i-no    EQ {1}.i-no:
                ACCUMULATE job-hdr.qty (TOTAL).
              END.

              IF (ACCUM TOTAL job-hdr.qty) NE 0 THEN
              FOR EACH job-hdr NO-LOCK
                  WHERE job-hdr.company EQ job.company
                    AND job-hdr.job-no  EQ job.job-no
                    AND job-hdr.job-no2 EQ job.job-no2
                    AND job-hdr.i-no    EQ {1}.i-no,
                
                  EACH job-mat NO-LOCK
                  WHERE job-mat.company     EQ job-hdr.company
                    AND job-mat.job         EQ job-hdr.job
                    AND job-mat.job-no      EQ job-hdr.job-no
                    AND job-mat.job-no2     EQ job-hdr.job-no2
                    AND ((job-mat.frm       EQ job-hdr.frm AND
                          (job-mat.blank-no EQ job-hdr.blank-no OR
                           job-mat.blank-no EQ 0)) OR
                         v-one-item)
                  USE-INDEX seq-idx,

                  FIRST item NO-LOCK
                  WHERE item.company EQ job-mat.company
                    AND item.i-no    EQ job-mat.rm-i-no
                    AND (NOT CAN-DO("C,D,5,6",item.mat-type) OR
                         NOT v-one-item)
    
                  BREAK BY job-mat.frm
                        BY job-mat.blank-no
                        BY job-mat.i-no
                        BY ROWID(job-mat):

                IF FIRST-OF(job-mat.i-no) THEN
                  IF /*item.industry EQ "1"        AND*/  /*09151415*/
                     CAN-DO("C,D,5,6",item.mat-type) THEN DO:
                    v-dec = {1}.cases.
                    FIND FIRST eb 
                        WHERE eb.company EQ job-mat.company
                          AND eb.est-no EQ job.est-no
                          AND eb.form-no EQ job-mat.frm
                          AND eb.blank-no EQ job-mat.blank-no
                        NO-LOCK NO-ERROR.
                    IF AVAIL eb THEN DO:
                        IF ITEM.mat-type EQ "C" THEN DO:
                            IF eb.spare-int-3 GT 0 THEN 
                                v-dec = v-dec * eb.spare-int-3.
                        END.
                        IF ITEM.mat-type EQ "5" THEN DO:
                            IF eb.spare-char-3 = "P" THEN
                                v-dec = v-dec / eb.cas-pal * eb.lp-up .
                            ELSE
                                v-dec = v-dec * eb.lp-up .
                        END.
                        IF ITEM.mat-type EQ "6" THEN DO:
                            IF eb.spare-char-4 = "P" THEN
                                v-dec = v-dec / eb.cas-pal * eb.div-up .
                            ELSE
                                v-dec = v-dec * eb.div-up .
                        END.
                    END.
                    IF item.mat-type EQ "D" THEN
                      ASSIGN
                       v-dec = v-dec / (IF {1}.cases-unit EQ 0 THEN 1
                                        ELSE {1}.cases-unit)
                       v-dec = TRUNC(v-dec,0)
                       v-dec = v-dec + INT({1}.partial GT 0)
                       v-dec = v-dec - INT({1}.partial LT 0).
                                                  
                    RUN jc/jc-autop.p (ROWID(job-mat), 0, v-dec).
                  END.

                  ELSE DO:
                    v-dec = {2}.t-qty / (ACCUM TOTAL job-hdr.qty) *
                            (IF job-mat.blank-no EQ 0 THEN (job-hdr.sq-in / 100)
                                                      ELSE 1). 
                                                  
                    RUN jc/jc-autop.p (ROWID(job-mat), v-dec, 0).
                  END.
              END.

              /* Case & Pallet Auto Issue for sets */
              IF v-one-item THEN
              FOR EACH reftable NO-LOCK
                  WHERE reftable.reftable EQ "jc/jc-calc.p"
                    AND reftable.company  EQ job.company
                    AND reftable.loc      EQ ""
                    AND reftable.code     EQ STRING(job.job,"999999999")
                    AND reftable.code2    EQ {1}.i-no,

                  EACH job-mat NO-LOCK
                  WHERE job-mat.company  EQ job.company
                    AND job-mat.job      EQ job.job
                    AND job-mat.job-no   EQ job.job-no
                    AND job-mat.job-no2  EQ job.job-no2
                    AND job-mat.frm      EQ INT(reftable.val[12])
                    AND job-mat.blank-no EQ INT(reftable.val[13])
                  USE-INDEX seq-idx,

                  FIRST item NO-LOCK
                  WHERE item.company  EQ job.company
                    AND item.i-no     EQ job-mat.rm-i-no
                    /*AND item.industry EQ "1"*/ /*09151415*/
                    AND CAN-DO("C,D,5,6",item.mat-type)
    
                  BREAK BY job-mat.frm
                        BY job-mat.blank-no
                        BY job-mat.i-no
                        BY ROWID(job-mat):

                IF FIRST-OF(job-mat.i-no) THEN DO:
                  v-dec = {1}.cases.
                    IF item.mat-type EQ "5" OR ITEM.mat-type = "6"  THEN DO:
                        FIND FIRST eb 
                            WHERE eb.company EQ job-mat.company
                              AND eb.est-no EQ job.est-no
                              AND eb.form-no EQ job-mat.frm
                              AND eb.blank-no EQ job-mat.blank-no
                            NO-LOCK NO-ERROR.
                       IF AVAIL eb THEN DO:
                            IF ITEM.mat-type EQ "5" THEN DO:
                                IF eb.spare-char-3 = "P" THEN
                                    v-dec = v-dec / eb.cas-pal * eb.lp-up .
                                ELSE
                                    v-dec = v-dec * eb.lp-up .
                            END.
                            ELSE DO:
                                IF eb.spare-char-4 = "P" THEN
                                    v-dec = v-dec / eb.cas-pal * eb.div-up .
                                ELSE
                                    v-dec = v-dec * eb.div-up .
                            END.
                        END.
                    END.
                  IF item.mat-type EQ "D" THEN
                    ASSIGN
                     v-dec = v-dec / (IF {1}.cases-unit EQ 0 THEN 1
                                      ELSE {1}.cases-unit)
                     v-dec = TRUNC(v-dec,0)
                     v-dec = v-dec + INT({1}.partial GT 0)
                     v-dec = v-dec - INT({1}.partial LT 0).
                                                  
                  RUN jc/jc-autop.p (ROWID(job-mat), 0, v-dec).
                END.
              END.
            END.
          END.   /* else */

          ASSIGN
           itemfg.q-prod     = itemfg.q-prod     + {2}.t-qty
           itemfg.q-prod-ptd = itemfg.q-prod-ptd + {2}.t-qty
           itemfg.q-prod-ytd = itemfg.q-prod-ytd + {2}.t-qty
           itemfg.q-onh      = itemfg.q-onh      + {2}.t-qty
           itemfg.q-ono      = itemfg.q-ono      - v-reduce-qty. 
          RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT {2}.loc).
          IF itemfg.def-loc EQ "" AND itemfg.def-loc-bin EQ "" THEN
            ASSIGN itemfg.def-loc     = {2}.loc
                   itemfg.def-loc-bin = {2}.loc-bin.          
          IF itemfg.q-ono LT 0 THEN
              itemfg.q-ono = 0.             

          FIND FIRST itemfg-loc 
              WHERE itemfg-loc.company EQ itemfg.company
                AND itemfg-loc.i-no    EQ itemfg.i-no
                AND itemfg-loc.loc     EQ {2}.loc
              EXCLUSIVE-LOCK NO-ERROR.
          IF AVAIL itemfg-loc THEN DO:
              IF AVAIL job THEN
                  FIND FIRST job-hdr NO-LOCK
                    WHERE job-hdr.company EQ job.company
                      AND job-hdr.job     EQ job.job
                      AND job-hdr.job-no  EQ job.job-no
                      AND job-hdr.job-no2 EQ job.job-no2
                      AND job-hdr.i-no    EQ {1}.i-no
                      NO-ERROR.
              IF {2}.po-no GT "" THEN
                  FIND FIRST po-ordl WHERE po-ordl.company EQ {2}.company
                                       AND po-ordl.po-no EQ INTEGER({2}.po-no)
                                       AND po-ordl.i-no  EQ {2}.i-no
                                     NO-LOCK NO-ERROR.
              IF AVAIL po-ordl THEN
                  FIND po-ord WHERE po-ord.company = po-ordl.company
                    AND po-ord.po-no = po-ordl.po-no 
                  NO-LOCK NO-ERROR.
              ASSIGN
               itemfg-loc.q-prod     = itemfg-loc.q-prod     + {2}.t-qty
               itemfg-loc.q-prod-ptd = itemfg-loc.q-prod-ptd + {2}.t-qty
               itemfg-loc.q-prod-ytd = itemfg-loc.q-prod-ytd + {2}.t-qty
               itemfg-loc.q-onh      = itemfg-loc.q-onh      + {2}.t-qty.

             IF (NOT AVAIL job-hdr OR AVAIL(job-hdr) AND job-hdr.loc EQ {2}.loc) 
                 AND (NOT AVAIL po-ord OR avail(po-ord) AND po-ord.loc EQ {2}.loc) THEN
               itemfg-loc.q-ono      = itemfg-loc.q-ono      - v-reduce-qty. 
             ELSE DO:     
                 
                 /* If job or po location was different, have to reduce q-ono */
                 /* for that location                                         */
                 IF AVAIL po-ord AND po-ord.loc NE {2}.loc AND AVAIL itemfg THEN DO:
                   FIND FIRST itemfg-loc 
                     WHERE itemfg-loc.company EQ itemfg.company
                       AND itemfg-loc.i-no    EQ itemfg.i-no
                       AND itemfg-loc.loc     EQ po-ord.loc
                     EXCLUSIVE-LOCK NO-ERROR.
                 END.
                 ELSE IF AVAIL job-hdr AND job-hdr.loc NE {2}.loc AND AVAIL itemfg THEN DO:
                     FIND FIRST itemfg-loc 
                       WHERE itemfg-loc.company EQ itemfg.company
                         AND itemfg-loc.i-no    EQ itemfg.i-no
                         AND itemfg-loc.loc     EQ job-hdr.loc
                      EXCLUSIVE-LOCK NO-ERROR.
                 END.
   
                IF AVAIL itemfg-loc THEN
                    itemfg-loc.q-ono      = itemfg-loc.q-ono      - v-reduce-qty.
               
             END.
             IF AVAIL itemfg-loc AND itemfg-loc.q-ono LT 0 THEN
                      itemfg-loc.q-ono = 0.
              
              FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
          END.

          RUN fg/comp-upd.p (RECID(itemfg), v-reduce-qty * -1, "q-ono",v-est-no).

          IF {2}.pur-uom NE itemfg.prod-uom              AND
             (LOOKUP({2}.pur-uom,fg-uom-list)     EQ 0 OR
              LOOKUP(itemfg.prod-uom,fg-uom-list) EQ 0)  THEN
          RUN sys/ref/convcuom.p({2}.pur-uom, itemfg.prod-uom, 0, 0, 0, 0,
                                 itemfg.last-cost, OUTPUT itemfg.last-cost).

          IF AVAIL job THEN DO:
              FIND FIRST job-hdr NO-LOCK
                WHERE job-hdr.company EQ job.company
                  AND job-hdr.job     EQ job.job
                  AND job-hdr.job-no  EQ job.job-no
                  AND job-hdr.job-no2 EQ job.job-no2
                  AND job-hdr.i-no    EQ {1}.i-no
                  NO-ERROR.
                    
              IF AVAIL job-hdr AND (job-hdr.std-tot-cost GT 0
                                   OR job-hdr.std-mat-cost GT 0
                                   OR job-hdr.std-lab-cost GT 0
                                   OR job-hdr.std-fix-cost GT 0) THEN DO:
                  ASSIGN itemfg.std-tot-cost = job-hdr.std-tot-cost
                         itemfg.std-mat-cost = job-hdr.std-mat-cost
                         itemfg.std-lab-cost = job-hdr.std-lab-cost
                         itemfg.std-fix-cost = job-hdr.std-fix-cost
                         itemfg.std-var-cost = job-hdr.std-var-cost
                         itemfg.total-std-cost = itemfg.std-tot-cost                         
                         itemfg.last-cost    = itemfg.std-tot-cost.
                  
                 itemfg.avg-cost = /*new-total-cost*/ (/* original-total-cost */ ((itemfg.q-onh - {2}.t-qty) * itemfg.avg-cost) + (job-hdr.std-tot-cost * {2}.t-qty))  / itemfg.q-onh.
                 FIND FIRST fg-ctrl WHERE fg-ctrl.company EQ cocode NO-LOCK NO-ERROR.
                 IF AVAIL fg-ctrl AND fg-ctrl.inv-meth = "A" THEN
                     itemfg.total-std-cost = itemfg.avg-cost.
              END.

          END.

                               
  END.  /* rita-code = "r" */
  ELSE IF {1}.rita-code EQ "S" THEN DO:      /** SHIPPMENTS **/
  
       ASSIGN
           itemfg.q-ytd      = itemfg.q-ytd - {2}.t-qty
           itemfg.q-ship     = itemfg.q-ship + {2}.t-qty
           itemfg.q-ship-ptd = itemfg.q-ship-ptd + {2}.t-qty
           itemfg.q-ship-ytd = itemfg.q-ship-ytd + {2}.t-qty
           itemfg.q-onh      = itemfg.q-onh - {2}.t-qty.

       RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT {2}.loc).
       FIND FIRST itemfg-loc 
           WHERE itemfg-loc.company EQ itemfg.company
             AND itemfg-loc.i-no    EQ itemfg.i-no
             AND itemfg-loc.loc     EQ {2}.loc
           EXCLUSIVE-LOCK NO-ERROR.
       ASSIGN
           itemfg-loc.q-ytd      = itemfg-loc.q-ytd - {2}.t-qty
           itemfg-loc.q-ship     = itemfg-loc.q-ship + {2}.t-qty
           itemfg-loc.q-ship-ptd = itemfg-loc.q-ship-ptd + {2}.t-qty
           itemfg-loc.q-ship-ytd = itemfg-loc.q-ship-ytd + {2}.t-qty
           itemfg-loc.q-onh      = itemfg-loc.q-onh - {2}.t-qty. 
       FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
 
  END.
  ELSE IF {1}.rita-code EQ "T" THEN DO:        /** TRANSFERS **/
       ASSIGN
           itemfg.q-tran     = itemfg.q-tran + {2}.t-qty
           itemfg.q-tran-ptd = itemfg.q-tran-ptd + {2}.t-qty.

       /* Move the inventory between the two itemfg-loc records */
       RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT {2}.loc).
       FIND FIRST itemfg-loc 
           WHERE itemfg-loc.company EQ itemfg.company
             AND itemfg-loc.i-no    EQ itemfg.i-no
             AND itemfg-loc.loc     EQ {2}.loc
           EXCLUSIVE-LOCK NO-ERROR.
       IF AVAIL itemfg-loc THEN DO:
         ASSIGN
           itemfg-loc.q-onh = itemfg-loc.q-onh - {2}.t-qty
           itemfg-loc.q-avail = itemfg-loc.q-onh + itemfg-loc.q-ono - itemfg-loc.q-alloc.
         FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
       END.

       RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT {2}.loc2).
       FIND FIRST itemfg-loc 
           WHERE itemfg-loc.company EQ itemfg.company
             AND itemfg-loc.i-no    EQ itemfg.i-no
             AND itemfg-loc.loc     EQ {2}.loc2
           EXCLUSIVE-LOCK NO-ERROR.
       IF AVAIL itemfg-loc THEN DO:
         ASSIGN
           itemfg-loc.q-onh = itemfg-loc.q-onh + {2}.t-qty
           itemfg-loc.q-avail = itemfg-loc.q-onh + itemfg-loc.q-ono - itemfg-loc.q-alloc.
         FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
       END.

       /* Close transfer orders */
       IF {2}.bol-no GT 0 THEN DO:
           RUN oe/clsorditm.p (INPUT {2}.company, INPUT {2}.bol-no, INPUT {2}.i-no).
       END.

  END.
  ELSE IF {1}.rita-code EQ "A" THEN DO:       /** ADJUSTMENTS **/
  
       ASSIGN
           itemfg.q-adj     = itemfg.q-adj  + {2}.t-qty
           itemfg.q-adj-ytd = itemfg.q-adj-ytd  + {2}.t-qty
           itemfg.q-adj-ptd = itemfg.q-adj-ptd  + {2}.t-qty
           itemfg.q-onh     = itemfg.q-onh + {2}.t-qty.
       RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT {2}.loc).
       FIND FIRST itemfg-loc 
           WHERE itemfg-loc.company EQ itemfg.company
             AND itemfg-loc.i-no    EQ itemfg.i-no
             AND itemfg-loc.loc     EQ {2}.loc
           EXCLUSIVE-LOCK NO-ERROR.
       IF AVAIL itemfg-loc THEN DO:
         ASSIGN
           itemfg-loc.q-adj     = itemfg-loc.q-adj  + {2}.t-qty
           itemfg-loc.q-adj-ytd = itemfg-loc.q-adj-ytd  + {2}.t-qty
           itemfg-loc.q-adj-ptd = itemfg-loc.q-adj-ptd  + {2}.t-qty
           itemfg-loc.q-onh     = itemfg-loc.q-onh + {2}.t-qty.
         FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
       END.

  END.
  ELSE IF {1}.rita-code EQ "E" THEN DO:      /** CREDIT RETURNS **/
  
       ASSIGN
           itemfg.q-ytd      = itemfg.q-ytd + {2}.t-qty
           itemfg.q-ship     = itemfg.q-ship - {2}.t-qty
           itemfg.q-ship-ptd = itemfg.q-ship-ptd - {2}.t-qty
           itemfg.q-ship-ytd = itemfg.q-ship-ytd - {2}.t-qty
           itemfg.q-onh      = itemfg.q-onh + {2}.t-qty.
       RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT {2}.loc).
       FIND FIRST itemfg-loc 
           WHERE itemfg-loc.company EQ itemfg.company
             AND itemfg-loc.i-no    EQ itemfg.i-no
             AND itemfg-loc.loc     EQ {2}.loc
           EXCLUSIVE-LOCK NO-ERROR.
       IF AVAIL itemfg-loc THEN DO:
         ASSIGN
           itemfg-loc.q-ytd      = itemfg-loc.q-ytd + {2}.t-qty
           itemfg-loc.q-ship     = itemfg-loc.q-ship - {2}.t-qty
           itemfg-loc.q-ship-ptd = itemfg-loc.q-ship-ptd - {2}.t-qty
           itemfg-loc.q-ship-ytd = itemfg-loc.q-ship-ytd - {2}.t-qty
           itemfg-loc.q-onh      = itemfg-loc.q-onh + {2}.t-qty.
         FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
       END.

  END.
                                         /** Re-Calculate Qty Available **/
  itemfg.q-avail = itemfg.q-onh + itemfg.q-ono - itemfg.q-alloc.
  RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT {2}.loc).
  FIND FIRST itemfg-loc 
      WHERE itemfg-loc.company EQ itemfg.company
        AND itemfg-loc.i-no    EQ itemfg.i-no
        AND itemfg-loc.loc     EQ {2}.loc
      EXCLUSIVE-LOCK NO-ERROR.
  IF AVAIL itemfg-loc THEN DO:
      itemfg-loc.q-avail = itemfg-loc.q-onh + itemfg-loc.q-ono - itemfg-loc.q-alloc.
  
      IF itemfg.q-ship-ptd LT 0 THEN DO:
          itemfg.q-ship-ptd = 0.
          IF AVAIL itemfg-loc THEN
            itemfg-loc.q-ship-ptd = 0.
      END.
          
      IF itemfg.q-ship-ytd LT 0 THEN DO: 
          itemfg.q-ship-ytd = 0.
          IF AVAIL itemfg-loc THEN
              itemfg-loc.q-ship-ytd = 0.
      END.
      FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
  END.

  FIND FIRST job-hdr WHERE job-hdr.company EQ cocode
                          AND job-hdr.job-no  EQ {1}.job-no
                          AND job-hdr.job-no2 EQ {1}.job-no2
                          AND job-hdr.i-no    EQ {1}.i-no
                NO-LOCK NO-ERROR.
                
  RUN fg/searchBin (INPUT cocode, INPUT {1}.job-no, INPUT {1}.job-no2,
      INPUT 0 /* OrdNo */, INPUT {1}.i-no, INPUT ABSOLUTE({1}.qty),
      INPUT {2}.loc, INPUT {2}.loc-bin, INPUT "" /* bolWhse */, INPUT {2}.tag, 
      OUTPUT rFgBinRow).

  IF rFgBinRow NE ? THEN 
      FIND FIRST fg-bin EXCLUSIVE-LOCK WHERE ROWID(fg-bin) EQ rFgBinRow NO-ERROR.
         
  IF NOT AVAILABLE fg-bin THEN       
    FIND FIRST fg-bin WHERE fg-bin.company EQ cocode
                        AND fg-bin.i-no    EQ {1}.i-no
                        AND fg-bin.job-no  EQ {1}.job-no
                        AND fg-bin.job-no2 EQ {1}.job-no2
                        AND fg-bin.loc     EQ {2}.loc
                        AND fg-bin.loc-bin EQ {2}.loc-bin
                        AND fg-bin.tag     EQ {2}.tag
                        AND fg-bin.cust-no EQ {2}.cust-no
              USE-INDEX co-ino NO-ERROR.
              
  IF NOT AVAIL fg-bin THEN
         IF INDEX("REA",{1}.rita-code) GT 0 THEN DO:
            CREATE fg-bin.
            ASSIGN
             fg-bin.company    = cocode
             fg-bin.i-no       = {1}.i-no
             fg-bin.job-no     = {1}.job-no
             fg-bin.job-no2    = {1}.job-no2
             fg-bin.loc        = {2}.loc
             fg-bin.loc-bin    = {2}.loc-bin
             fg-bin.tag        = {2}.tag
             fg-bin.cust-no    = {2}.cust-no.
         END.
         ELSE RETURN ERROR.  /*undo {3}, next {3}. */

  /* Transfer don't have PO # populated, so take from the "from" bin */
  IF NOT {1}.rita-code EQ "T" AND NOT ({1}.rita-code EQ "A" AND {1}.qty LT 0) THEN
         fg-bin.po-no      = {2}.po-no. /*01161406*/
  ASSIGN  
      
      fg-bin.tot-wt     = {2}.tot-wt .

  IF fg-bin.case-count   LE 0 OR
     (fg-bin.case-count  LT {2}.qty-case AND fg-bin.case-count EQ fg-bin.qty)
                              THEN fg-bin.case-count   = {2}.qty-case.
  IF fg-bin.units-pallet LE 0 THEN fg-bin.units-pallet = {2}.units-pallet.
  IF fg-bin.cases-unit   LE 0 THEN fg-bin.cases-unit   = {2}.cases-unit.

  /* For Transfers from & Shipments decrease the quantity in the BIN */
  IF INDEX("TS",{1}.rita-code) NE 0 THEN
    ASSIGN
     fg-bin.qty           = fg-bin.qty - {2}.t-qty
     fg-bin.partial-count = fg-bin.partial-count - {2}.partial.

  /* Store the PO-no to put into new bin */
  IF {1}.rita-code = "T" THEN
      {2}.po-no = fg-bin.po-no.

  /* For Receipts increase the quantity in the BIN */
  ELSE 
  IF {1}.rita-code EQ "R" THEN DO:
    ld-cvt-cost = {2}.ext-cost / {2}.t-qty * 1000.
    IF {2}.cost-uom NE "M" THEN
      RUN sys/ref/convcuom.p("M", {2}.cost-uom, 0,
                             IF AVAIL po-ordl THEN po-ordl.s-len ELSE 0,
                             IF AVAIL po-ordl THEN po-ordl.s-wid ELSE 0,
                             0, ld-cvt-cost, OUTPUT ld-cvt-cost).
    {fg/upd-bin.i "fg-bin" "{2}.cost-uom" "ld-cvt-cost" {2}}
  END.
  ELSE 
  IF {1}.rita-code EQ "A" OR {1}.rita-code EQ "E" THEN
    ASSIGN
     fg-bin.qty           = fg-bin.qty + {2}.t-qty
     fg-bin.partial-count = fg-bin.partial-count + {2}.partial.

  /* This code is to handle the Transfer quantity to increase the BIN */
  IF {1}.rita-code EQ "T" THEN DO:
    FIND FIRST b-fg-bin
        WHERE b-fg-bin.company EQ {1}.company
          AND b-fg-bin.job-no  EQ {1}.job-no
          AND b-fg-bin.job-no2 EQ {1}.job-no2
          AND b-fg-bin.i-no    EQ {1}.i-no
          AND b-fg-bin.loc     EQ {2}.loc2
          AND b-fg-bin.loc-bin EQ {2}.loc-bin2
          AND b-fg-bin.tag     EQ {2}.tag2
          AND b-fg-bin.cust-no EQ {2}.cust-no
        USE-INDEX co-ino NO-ERROR.

    IF NOT AVAIL b-fg-bin THEN DO:
      CREATE b-fg-bin.
      ASSIGN
       b-fg-bin.company      = {1}.company
       b-fg-bin.i-no         = {1}.i-no
       b-fg-bin.job-no       = {1}.job-no
       b-fg-bin.job-no2      = {1}.job-no2
       b-fg-bin.loc          = {2}.loc2
       b-fg-bin.loc-bin      = {2}.loc-bin2
       b-fg-bin.tag          = {2}.tag2
       b-fg-bin.cust-no      = {2}.cust-no
       b-fg-bin.case-count   = {2}.qty-case
       b-fg-bin.units-pallet = fg-bin.units-pallet
       b-fg-bin.cases-unit   = fg-bin.cases-unit
       b-fg-bin.po-no        = {2}.po-no.
    END.
    IF {2}.po-no GT "" THEN
        b-fg-bin.po-no = {2}.po-no.
    IF b-fg-bin.case-count   LE 0 THEN b-fg-bin.case-count   = fg-bin.case-count.
    IF b-fg-bin.units-pallet LE 0 THEN b-fg-bin.units-pallet = fg-bin.units-pallet.
    IF b-fg-bin.cases-unit   LE 0 THEN b-fg-bin.cases-unit   = fg-bin.cases-unit.

    {fg/upd-bin.i "b-fg-bin" "fg-bin.pur-uom" "fg-bin.std-tot-cost" {2}}

    IF fg-bin.pur-uom EQ "M" THEN
       {2}.ext-cost = fg-bin.std-tot-cost * {2}.t-qty / 1000.
    ELSE
       {2}.ext-cost = fg-bin.std-tot-cost * {2}.t-qty.
       
       ASSIGN b-fg-bin.tot-wt   = {2}.tot-wt .

    FIND CURRENT b-fg-bin NO-LOCK NO-ERROR.
  END. /* if rita-code eq "T" */
 
  /* Find first to prevent duplicates */
  FIND FIRST fg-rdtlh 
    WHERE fg-rdtlh.r-no EQ {2}.r-no
      AND fg-rdtlh.i-no EQ {2}.i-no
      AND fg-rdtlh.cases EQ {2}.cases
      AND fg-rdtlh.tag EQ {2}.tag
      AND fg-rdtlh.qty EQ {2}.qty
      AND fg-rdtlh.job-no EQ {2}.job-no
      AND fg-rdtlh.job-no2 EQ {2}.job-no2
      AND fg-rdtlh.partial EQ {2}.partial
    NO-LOCK NO-ERROR.
  IF NOT AVAIL fg-rdtlh THEN DO:
  

    CREATE fg-rdtlh.
    {fg/fg-rdtl.i fg-rdtlh {2}} /* Create Detail History Records */
    {fg/fg-fgact.i {1} {2}}         /* Create Job Costing F/G WIP Record */
    ASSIGN fg-rdtlh.user-id = USERID('nosweat')
           fg-rdtlh.upd-date = TODAY
           fg-rdtlh.upd-time = TIME.
  END.

  CREATE fg-rcpth.
  {fg/fg-rcpts.i fg-rcpth {1}}  /* Create Header History Records */
  ASSIGN fg-rcpth.user-id = USERID('nosweat')
         fg-rcpth.upd-date = TODAY
         fg-rcpth.upd-time = TIME.

  FIND CURRENT fg-rdtlh NO-LOCK NO-ERROR.
  FIND CURRENT fg-rcpth NO-LOCK NO-ERROR.

  END. /*end IF {1}.rita-code NE "I"*/

