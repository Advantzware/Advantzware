/* fg/fg-post.i   in GUI  copied and changed */ 

  release prod.
  find first prodl
      where prodl.company eq cocode
         and prodl.procat  eq itemfg.procat
         and can-find(first prod
                      where prod.company eq cocode
                        and prod.prolin  eq prodl.prolin)
      no-lock no-error.

  if avail prodl then
  find first prod
      where prod.company eq cocode
        and prod.prolin  eq prodl.prolin
      no-lock no-error.

  v-newhdr = no. 
  release po-ord.
  if {1}.rita-code ne "E" AND {1}.rita-code ne "I" and int({1}.po-no) ne 0 then
      find first po-ord   where po-ord.company eq cocode
                            and po-ord.po-no   eq int({1}.po-no)
          NO-LOCK no-error.
          
 /*     for each {2} where {2}.r-no eq {1}.r-no:  */
  release po-ordl.
  if avail po-ord then
        find first po-ordl where po-ordl.company   eq cocode
                             and po-ordl.po-no     eq po-ord.po-no
                             and po-ordl.i-no      eq {1}.i-no
                             and po-ordl.deleted   eq no
                             and po-ordl.item-type eq no
                             AND po-ordl.stat NE "C"
             USE-INDEX po-no no-error.
        IF NOT AVAIL po-ordl THEN
        find first po-ordl  where po-ordl.company   eq cocode
                              and po-ordl.po-no     eq po-ord.po-no
                              and po-ordl.i-no      eq {1}.i-no
                              and po-ordl.deleted   eq no
                              and po-ordl.item-type eq no
                        no-error.
  
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
  if {1}.rita-code eq "R" then do:           /** RECEIPTS **/
          v-reduce-qty = 0.

          RELEASE job.

          if avail po-ordl then do:
            IF LOOKUP(po-ordl.cons-uom,fg-uom-list) GT 0 THEN
              v-reduce-qty = po-ordl.cons-qty.
            ELSE
            if po-ordl.cons-qty ne 0 then
              run sys/ref/convquom.p(input po-ordl.cons-uom,
                                     input "EA", input 0,
                                     input po-ordl.s-len,
                                     input po-ordl.s-wid,
                                     input 0,
                                     input po-ordl.cons-qty,
                                     output v-reduce-qty).

            assign v-overrun-qty     = v-reduce-qty * (1 - (po-ordl.over-pct / 100))
                   v-underrun-qty    = v-reduce-qty * (1 - (po-ordl.under-pct / 100))
                   po-ordl.t-rec-qty = po-ordl.t-rec-qty + {1}.t-qty.

            if po-ordl.t-rec-qty ge v-underrun-qty then do:
               assign  v-reduce-qty = min(v-reduce-qty,v-reduce-qty - po-ordl.t-rec-qty + {1}.t-qty)
                       po-ordl.stat = "C".       
               /*find first b-po-ordl  where b-po-ordl.company eq po-ord.company
                                       and b-po-ordl.po-no   eq po-ord.po-no
                                        and b-po-ordl.stat    ne "C"
                                        and b-po-ordl.deleted eq no
                                        and recid(b-po-ordl)  ne recid(po-ordl)
                              no-lock no-error.
               if not avail b-po-ordl then po-ord.stat = "C".*/
            end.
            else po-ordl.stat = "P".
            if v-reduce-qty lt 0 then v-reduce-qty = 0.
            if po-ordl.stat ne "C" then  v-reduce-qty = min({1}.t-qty,v-reduce-qty). 

            {1}.invoiced = YES.
          end.   /* avail po-ordl */

          ELSE  
          IF {1}.job-no ne "" THEN
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
 
            find first sys-ctrl no-lock
                where sys-ctrl.company eq job.company
                  and sys-ctrl.name    eq "AUTOISSU"
                no-error.
                
            if avail sys-ctrl and sys-ctrl.char-fld eq "FGPost" then do:
              v-one-item = TRIM(job.est-no) NE "" AND
                           CAN-FIND(FIRST est
                                    WHERE est.company   EQ job.company
                                      AND est.est-no    EQ job.est-no
                                      AND (est.est-type EQ 2 OR
                                           est.est-type EQ 6)).

              for each job-hdr no-lock
                  where job-hdr.company eq job.company
                    and job-hdr.job     eq job.job
                    and job-hdr.job-no  eq job.job-no
                    and job-hdr.job-no2 eq job.job-no2
                    and job-hdr.i-no    eq {1}.i-no:
                accumulate job-hdr.qty (total).
              end.

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
            end.
          end.   /* else */

          assign
           itemfg.q-prod     = itemfg.q-prod     + {2}.t-qty
           itemfg.q-prod-ptd = itemfg.q-prod-ptd + {2}.t-qty
           itemfg.q-prod-ytd = itemfg.q-prod-ytd + {2}.t-qty
           itemfg.q-onh      = itemfg.q-onh      + {2}.t-qty
           itemfg.q-ono      = itemfg.q-ono      - v-reduce-qty. 
          RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT {2}.loc).
          if itemfg.def-loc eq "" and itemfg.def-loc-bin eq "" then
            assign itemfg.def-loc     = {2}.loc
                   itemfg.def-loc-bin = {2}.loc-bin.          
          if itemfg.q-ono lt 0 THEN
              itemfg.q-ono = 0.             

          FIND FIRST itemfg-loc 
              WHERE itemfg-loc.company EQ itemfg.company
                AND itemfg-loc.i-no    EQ itemfg.i-no
                AND itemfg-loc.loc     EQ {2}.loc
              EXCLUSIVE-LOCK NO-ERROR.
          IF AVAIL itemfg-loc THEN DO:
              IF AVAIL job THEN
                  FIND FIRST job-hdr no-lock
                    where job-hdr.company eq job.company
                      and job-hdr.job     eq job.job
                      and job-hdr.job-no  eq job.job-no
                      and job-hdr.job-no2 eq job.job-no2
                      and job-hdr.i-no    eq {1}.i-no
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
              assign
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
             IF AVAIL itemfg-loc AND itemfg-loc.q-ono lt 0 THEN
                      itemfg-loc.q-ono = 0.
              
              FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
          END.

          run fg/comp-upd.p (recid(itemfg), v-reduce-qty * -1, "q-ono",v-est-no).

          IF {2}.pur-uom NE itemfg.prod-uom              AND
             (LOOKUP({2}.pur-uom,fg-uom-list)     EQ 0 OR
              LOOKUP(itemfg.prod-uom,fg-uom-list) EQ 0)  THEN
          run sys/ref/convcuom.p({2}.pur-uom, itemfg.prod-uom, 0, 0, 0, 0,
                                 itemfg.last-cost, output itemfg.last-cost).

          IF AVAIL job THEN DO:
              FIND FIRST job-hdr no-lock
                where job-hdr.company eq job.company
                  and job-hdr.job     eq job.job
                  and job-hdr.job-no  eq job.job-no
                  and job-hdr.job-no2 eq job.job-no2
                  and job-hdr.i-no    eq {1}.i-no
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
                         .
              END.
          END.
          ELSE DO:
              ASSIGN
                  itemfg.std-mat-cost = {2}.ext-cost / {2}.t-qty.
              IF itemfg.prod-uom EQ "M" THEN ASSIGN
                  itemfg.std-mat-cost =  itemfg.std-mat-cost * 1000.
              ASSIGN 
                  itemfg.std-tot-cost = itemfg.std-mat-cost.
          END.
          ASSIGN 
             itemfg.total-std-cost = itemfg.std-tot-cost                         
             itemfg.last-cost    = itemfg.std-tot-cost.
             itemfg.avg-cost =  (((itemfg.q-onh - {2}.t-qty) * itemfg.avg-cost) + (itemfg.std-tot-cost * {2}.t-qty))  / itemfg.q-onh.
         FIND FIRST fg-ctrl NO-LOCK  
             WHERE fg-ctrl.company EQ itemfg.company
             NO-ERROR.
         IF AVAIL fg-ctrl AND fg-ctrl.inv-meth = "A" THEN
             itemfg.total-std-cost = itemfg.avg-cost.                     
  end.  /* rita-code = "r" */
  else if {1}.rita-code eq "S" then DO:      /** SHIPPMENTS **/
  
       assign
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
       assign
           itemfg-loc.q-ytd      = itemfg-loc.q-ytd - {2}.t-qty
           itemfg-loc.q-ship     = itemfg-loc.q-ship + {2}.t-qty
           itemfg-loc.q-ship-ptd = itemfg-loc.q-ship-ptd + {2}.t-qty
           itemfg-loc.q-ship-ytd = itemfg-loc.q-ship-ytd + {2}.t-qty
           itemfg-loc.q-onh      = itemfg-loc.q-onh - {2}.t-qty. 
       FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
 
  END.
  else if {1}.rita-code eq "T" then DO:        /** TRANSFERS **/
       assign
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
  else if {1}.rita-code eq "A" then DO:       /** ADJUSTMENTS **/
  
       assign
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
         assign
           itemfg-loc.q-adj     = itemfg-loc.q-adj  + {2}.t-qty
           itemfg-loc.q-adj-ytd = itemfg-loc.q-adj-ytd  + {2}.t-qty
           itemfg-loc.q-adj-ptd = itemfg-loc.q-adj-ptd  + {2}.t-qty
           itemfg-loc.q-onh     = itemfg-loc.q-onh + {2}.t-qty.
         FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
       END.

  END.
  else if {1}.rita-code eq "E" then DO:      /** CREDIT RETURNS **/
  
       assign
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
  
      if itemfg.q-ship-ptd lt 0 then DO:
          itemfg.q-ship-ptd = 0.
          IF AVAIL itemfg-loc THEN
            itemfg-loc.q-ship-ptd = 0.
      END.
          
      if itemfg.q-ship-ytd lt 0 then do: 
          itemfg.q-ship-ytd = 0.
          IF AVAIL itemfg-loc THEN
              itemfg-loc.q-ship-ytd = 0.
      END.
      FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
  END.

  find first job-hdr where job-hdr.company eq cocode
                          and job-hdr.job-no  eq {1}.job-no
                          and job-hdr.job-no2 eq {1}.job-no2
                          and job-hdr.i-no    eq {1}.i-no
                no-lock no-error.
  find first fg-bin where fg-bin.company eq cocode
                      and fg-bin.i-no    eq {1}.i-no
                      and fg-bin.job-no  eq {1}.job-no
                      and fg-bin.job-no2 eq {1}.job-no2
                      and fg-bin.loc     eq {2}.loc
                      and fg-bin.loc-bin eq {2}.loc-bin
                      and fg-bin.tag     eq {2}.tag
                      and fg-bin.cust-no eq {2}.cust-no
            USE-INDEX co-ino no-error.
  if not avail fg-bin then
         if INDEX("REA",{1}.rita-code) GT 0 then do:
            create fg-bin.
            assign
             fg-bin.company    = cocode
             fg-bin.i-no       = {1}.i-no
             fg-bin.job-no     = {1}.job-no
             fg-bin.job-no2    = {1}.job-no2
             fg-bin.loc        = {2}.loc
             fg-bin.loc-bin    = {2}.loc-bin
             fg-bin.tag        = {2}.tag
             fg-bin.cust-no    = {2}.cust-no.
         end.
         else return error.  /*undo {3}, next {3}. */

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
  if index("TS",{1}.rita-code) ne 0 then
    assign
     fg-bin.qty           = fg-bin.qty - {2}.t-qty
     fg-bin.partial-count = fg-bin.partial-count - {2}.partial.

  /* Store the PO-no to put into new bin */
  IF {1}.rita-code = "T" THEN
      {2}.po-no = fg-bin.po-no.

  /* For Receipts increase the quantity in the BIN */
  else 
  if {1}.rita-code eq "R" then do:
    ld-cvt-cost = {2}.ext-cost / {2}.t-qty * 1000.
    IF {2}.cost-uom NE "M" THEN
      RUN sys/ref/convcuom.p("M", {2}.cost-uom, 0,
                             IF AVAIL po-ordl THEN po-ordl.s-len ELSE 0,
                             IF AVAIL po-ordl THEN po-ordl.s-wid ELSE 0,
                             0, ld-cvt-cost, OUTPUT ld-cvt-cost).
    {fg/upd-bin.i "fg-bin" "{2}.cost-uom" "ld-cvt-cost" {2}}
  end.
  else 
  if {1}.rita-code eq "A" or {1}.rita-code eq "E" then
    assign
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
  

    create fg-rdtlh.
    {fg/fg-rdtl.i fg-rdtlh {2}} /* Create Detail History Records */
    {fg/fg-fgact.i {1} {2}}         /* Create Job Costing F/G WIP Record */
    ASSIGN fg-rdtlh.user-id = USERID('nosweat')
           fg-rdtlh.upd-date = TODAY
           fg-rdtlh.upd-time = TIME.
  END.

  create fg-rcpth.
  {fg/fg-rcpts.i fg-rcpth {1}}  /* Create Header History Records */
  ASSIGN fg-rcpth.user-id = USERID('nosweat')
         fg-rcpth.upd-date = TODAY
         fg-rcpth.upd-time = TIME.

  FIND CURRENT fg-rdtlh NO-LOCK NO-ERROR.
  FIND CURRENT fg-rcpth NO-LOCK NO-ERROR.

  END. /*end IF {1}.rita-code NE "I"*/

