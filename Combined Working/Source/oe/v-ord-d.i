
def buffer xfg-set for fg-set.
def buffer xitemfg for itemfg.
DEF BUFFER b-oe-ordl FOR oe-ordl.
DEF BUFFER del-job-hdr FOR job-hdr.

def var v-totord like oe-ord.t-revenue no-undo.
def var v-tot-tax like oe-ord.tax no-undo.
def var v-tot-freight like oe-ord.t-freight no-undo.
def var v-qty-lft like oe-ordl.qty no-undo.
def var v-new-ord as logical initial NO no-undo.
def var v-ext-price like oe-ordl.t-price no-undo.
def var v-tax-rate as dec format "->>>.99" no-undo.
def var v-frt-tax-rate like v-tax-rate no-undo.
def var v-period as INT no-undo.
def var tmp-ordm-amt like oe-ordm.amt no-undo.
def var tmp-tax like oe-ord.tax no-undo.
def var v-continue as LOG no-undo.
def var v-blank-fg-on-est as INT no-undo.
def var char-hdl as cha no-undo.
DEF VAR loop-limit AS INT NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  IF CAN-FIND(FIRST ar-invl
              WHERE ar-invl.company EQ oe-ord.company
                AND ar-invl.ord-no  EQ oe-ord.ord-no) THEN DO:
    MESSAGE "Order has been Invoice, no deletion allowed..."
        VIEW-AS ALERT-BOX ERROR.
    RETURN NO-APPLY.
  END.

  IF (oe-ord.posted AND oe-ord.stat NE "H" ) OR 
     INDEX("CZ",oe-ord.stat) NE 0            THEN DO:
    IF oe-ord.stat = "H" THEN MESSAGE "Order already posted. No deletion allowed." VIEW-AS ALERT-BOX ERROR.
    ELSE MESSAGE "Order has been closed. No changes allowed"
                 VIEW-AS ALERT-BOX ERROR.

    RETURN NO-APPLY.
  END.

  if oe-ord.job-no ne "" then do:
    find first job
        where job.company eq oe-ord.company
          and job.job-no  eq oe-ord.job-no
          and job.job-no2 eq oe-ord.job-no2
          and (job.stat eq "C" or job.stat eq "W" or job.stat eq "Z")
        use-index job-no no-lock no-error.
    if avail job then do:
      MESSAGE "Order cannot be Deleted, Job has been Processed or Closed."
              "You Must Close the Order."
              VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
    END.
  END.

  find first oe-ordl of oe-ord
        where oe-ordl.rel-stat
           or oe-ordl.t-inv-qty ne 0
           or oe-ordl.t-ship-qty ne 0
        no-lock no-error.
  if avail oe-ordl then do:
    MESSAGE "Previous Quantities have been released for this Order." skip
            VIEW-AS ALERT-BOX ERROR.
    RETURN NO-APPLY.
  end.

  FIND FIRST oe-ordl OF oe-ord WHERE oe-ordl.po-no-po <> 0 NO-LOCK NO-ERROR.
  IF AVAIL oe-ordl  THEN DO:
      MESSAGE "Cannot delete, purchase order for board exists." VIEW-AS ALERT-BOX ERROR.
      RETURN.
  END.

  for each oe-ordl of oe-ord no-lock:
    release fg-rcpth.
    find first fg-rcpts use-index cust-no
        where fg-rcpts.company eq oe-ord.company
          and fg-rcpts.cust-no eq oe-ord.cust-no
          and fg-rcpts.job-no  EQ fill(" ",6 - length(trim(oe-ordl.job-no))) +
                                  trim(oe-ordl.job-no)
          and fg-rcpts.job-no2 eq oe-ordl.job-no2
          and fg-rcpts.i-no    eq oe-ordl.i-no
        no-lock no-error.
    if not avail fg-rcpts then
    find first fg-rcpth use-index job
        where fg-rcpth.company eq oe-ord.company
          and fg-rcpth.job-no  EQ fill(" ",6 - length(trim(oe-ordl.job-no))) +
                                  trim(oe-ordl.job-no)
          and fg-rcpth.job-no2 eq oe-ordl.job-no2
          and fg-rcpth.i-no    eq oe-ordl.i-no
        no-lock no-error.
    if (avail fg-rcpts or avail fg-rcpth) and
       oe-ordl.job-no ne ""               then do:  
      MESSAGE "Finished Goods Transactions Exists For This Order. "
              "Deleting Is Not Permitted!  You Must Close The Order."
              VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
    end.
  END.

  IF NOT adm-new-record THEN DO:
    message "Delete Order" STRING(oe-ord.ord-no) "for" STRING(oe-ord.cust-no) "?" view-as alert-box question
          button yes-no update ll-ans as log.

    if not ll-ans then return error.

    RUN check-use-1 NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN "ADM-ERROR".

    RUN check-use-2 NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN "ADM-ERROR".
  END.

  DO TRANSACTION:
  find first period
      where period.company eq oe-ord.company
        and period.pst     le oe-ord.ord-date
        and period.pend    ge oe-ord.ord-date
      no-lock no-error.
  v-period = if avail period and period.pstat then period.pnum else 1.

  if (oe-ord.est-no ne "" or int(oe-ord.est-no) ne 0) then do:
    find first est
        where est.company eq oe-ord.company
          and est.est-no  eq fill(" ",8 - length(trim(oe-ord.est-no))) +
                             trim(oe-ord.est-no)
        no-lock no-error.
    if avail est then do:

      FIND CURRENT est EXCLUSIVE.
      find last xoe-ord where xoe-ord.company = oe-ord.company and
                              xoe-ord.est-no = oe-ord.est-no and
                              RECID(xoe-ord) NE RECID(oe-ord)
                              no-lock no-error.
      if avail xoe-ord then
        assign
         est.ord-date = xoe-ord.ord-date
         est.ord-no   = xoe-ord.ord-no.
      else do:
        if est.ord-date eq oe-ord.ord-date then est.ord-date = ?.
        if est.ord-no eq oe-ord.ord-no then est.ord-no = 0.
      end.

      FIND CURRENT est NO-LOCK.
    end.
  end.

  for each oe-ordl OF oe-ord:
    run oe/oe-ordd.p(recid(oe-ordl),output v-continue).
    if not v-continue then RETURN NO-APPLY.
      
    for each oe-rel
        where oe-rel.company = oe-ordl.company
          and oe-rel.ord-no = oe-ordl.ord-no
          and oe-rel.i-no = oe-ordl.i-no
          and oe-rel.line = oe-ordl.line:
      delete oe-rel.
    end. /* each oe-rel */
      
    find first itemfg
        where itemfg.company eq oe-ordl.company
          and itemfg.i-no    eq oe-ordl.i-no
        no-error.
    if avail itemfg then do:
      
      IF AVAIL(itemfg) AND AVAIL(oe-ord) THEN DO:          
          RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT oe-ord.loc).
          FIND FIRST itemfg-loc 
              WHERE itemfg-loc.company EQ itemfg.company
                AND itemfg-loc.i-no    EQ itemfg.i-no
                AND itemfg-loc.loc     EQ oe-ord.loc
              EXCLUSIVE-LOCK NO-ERROR.
      END.

      IF oe-ord.type NE "T" THEN DO: 
          itemfg.q-alloc = itemfg.q-alloc - oe-ordl.qty.
          IF AVAIL(itemfg-loc) THEN
            itemfg-loc.q-alloc = itemfg-loc.q-alloc - oe-ordl.qty.
      END.

      IF itemfg.q-alloc LT 0 THEN DO: 
          itemfg.q-alloc = 0.
          IF AVAIL(itemfg-loc) THEN
              itemfg-loc.q-alloc = 0.
      END.
            
      assign
       itemfg.q-avail   = itemfg.q-onh + itemfg.q-ono - itemfg.q-alloc
       itemfg.q-ptd     = itemfg.q-ptd - oe-ordl.qty
       itemfg.q-ord-ytd = itemfg.q-ord-ytd - oe-ordl.qty.
      IF AVAIL(itemfg-loc) THEN
          assign
           itemfg-loc.q-avail   = itemfg-loc.q-onh + itemfg-loc.q-ono - itemfg-loc.q-alloc
           itemfg-loc.q-ptd     = itemfg-loc.q-ptd - oe-ordl.qty
           itemfg-loc.q-ord-ytd = itemfg-loc.q-ord-ytd - oe-ordl.qty.

      IF oe-ord.type NE "T"                                                   AND
         NOT CAN-FIND(FIRST b-oe-ordl {sys/inc/ordlcomp.i b-oe-ordl oe-ordl}) THEN
        RUN fg/comp-upd.p (RECID(itemfg), oe-ordl.qty * -1, "q-alloc", 0).
    end. /* avail itemfg */
   
    FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
    run ar/cctaxrt.p (input oe-ord.company, input oe-ord.tax-gr,
                      output v-tax-rate, output v-frt-tax-rate).

    assign
     v-qty-lft   = oe-ordl.qty - oe-ordl.inv-qty
     v-ext-price = 0.
       
    if v-qty-lft gt 0 then do:
      if oe-ordl.pr-uom begins "L" AND oe-ordl.pr-uom NE "LB" then
        v-ext-price = oe-ordl.price - round( (oe-ordl.price * oe-ordl.disc) / 100, 2).
      else
      if oe-ordl.pr-uom = "CS" then do:
        find first itemfg {sys/look/itemfgrlW.i}
             and itemfg.i-no = oe-ordl.i-no no-lock no-error.
        if avail itemfg and itemfg.case-count ne 0 then
          v-ext-price = ((v-qty-lft / itemfg.case-count) * oe-ordl.price) -
                        round((((v-qty-lft / itemfg.case-count) *
                                oe-ordl.price) * oe-ordl.disc) / 100, 2).
        else
          v-ext-price = (v-qty-lft * oe-ordl.price) - 
                        round(((v-qty-lft * oe-ordl.price) * oe-ordl.disc) / 100, 2).
      end.
      else
      if oe-ordl.pr-uom = "C" then
        v-ext-price = ((v-qty-lft / 100) *
                       oe-ordl.price) - round((((v-qty-lft / 100) *
                       oe-ordl.price) * oe-ordl.disc) / 100, 2).
      else
      if oe-ordl.pr-uom = "M" then
        v-ext-price = ((v-qty-lft / 1000) *
                      oe-ordl.price) - round(( ((v-qty-lft / 1000) *
                      oe-ordl.price) * oe-ordl.disc) / 100, 2).
      else /** default per thousand **/
        v-ext-price = ((v-qty-lft) *
                     oe-ordl.price) - round(( ((v-qty-lft) *
                     oe-ordl.price) * oe-ordl.disc) / 100, 2).

                                           /** calculate freight charges **/
      assign v-tot-freight = v-tot-freight +
                            (round(oe-ordl.t-freight / oe-ordl.qty, 2) *
                             v-qty-lft).
                                           /** calculate tax charges **/
      if oe-ordl.tax and v-tax-rate gt 0 then assign
        v-tot-tax = v-tot-tax + round((v-ext-price * v-tax-rate) / 100,2).
    end. /* inv-qty ne 0 */

    if oe-ordl.tax then
      assign v-totord = (v-totord + v-ext-price +
                         ROUND((v-ext-price * v-tax-rate) / 100,2)).
    else
      assign v-totord = v-totord + v-ext-price.

    for each oe-ordm
        where oe-ordm.company eq oe-ordl.company
          and oe-ordm.ord-no  eq oe-ordl.ord-no
          and oe-ordm.est-no  eq oe-ordl.est-no:
      if oe-ordm.bill eq "Y" then do:
        assign  v-totord = v-totord + oe-ordm.amt.
        if oe-ordm.tax and v-tax-rate gt 0 then
          assign v-tot-tax = (v-tot-tax +
                             round((oe-ordm.amt * v-tax-rate) / 100,2))
                 v-totord = (v-totord +
                             round((oe-ordm.amt * v-tax-rate) / 100,2)).
      end.
        
      if oe-ord.stat = "N" or oe-ord.stat = "A" or oe-ord.stat = "H" then
        delete oe-ordm.
    end. /* each oe-ordm */

    if oe-ordl.job-no ne "" then do:
      find first job
          where job.company eq oe-ordl.company
            and job.job-no  eq oe-ordl.job-no
            and job.job-no2 eq oe-ordl.job-no2
          use-index job-no no-lock no-error.

      for each job-hdr no-lock
          where job-hdr.company  eq oe-ordl.company
            and job-hdr.est-no   eq oe-ordl.est-no
            and job-hdr.job-no   eq oe-ordl.job-no
            and job-hdr.job-no2  eq oe-ordl.job-no2
            and ((job-hdr.ord-no eq oe-ordl.ord-no and
                  job-hdr.i-no   eq oe-ordl.i-no) or
                 job-hdr.ord-no  eq 0) 
          use-index enum:
          
        if job-hdr.ord-no ne 0 then do:
          /*if avail job and job.stat ne "P" then do:
            find first itemfg where itemfg.company = oe-ord.company and
                                    itemfg.i-no = oe-ordl.i-no no-error.
            if avail itemfg then do:
              itemfg.q-ono = itemfg.q-ono - job-hdr.qty.
              if itemfg.q-ono lt 0 then itemfg.q-ono = 0.
              itemfg.q-avail = itemfg.q-onh + itemfg.q-ono - itemfg.q-alloc.
                  
              run fg/comp-upd.p (recid(itemfg), job-hdr.qty * -1,
                                 "q-ono", job-hdr.est-no).
            end.
          end.*/
          
          if avail job then do:
            {util/dljobkey.i}
          end.
        end.
          
        DO loop-limit = 1 TO 1000:
          FIND del-job-hdr WHERE ROWID(del-job-hdr) EQ ROWID(job-hdr)
              EXCLUSIVE NO-WAIT NO-ERROR.
          IF AVAIL del-job-hdr THEN DO:
            DELETE del-job-hdr.
            LEAVE.
          END.
        END.
      end.

      find first job
          where job.company eq oe-ordl.company
            and job.job-no  eq oe-ordl.job-no
            and job.job-no2 eq oe-ordl.job-no2
          use-index job-no exclusive no-error.

      if avail job                                          and
         not can-find(first job-hdr
                      where job-hdr.company eq job.company
                        and job-hdr.job     eq job.job
                        and job-hdr.job-no  eq job.job-no
                        and job-hdr.job-no2 eq job.job-no2) then do:

        run jc/jc-dall.p (recid(job)).
                      
        for each job-mat
            where job-mat.company  eq job.company
              and job-mat.job      eq job.job
              and job-mat.job-no   eq job.job-no
              and job-mat.job-no2  eq job.job-no2
            use-index seq-idx:
          delete job-mat.
        end.

        for each mat-act
            where mat-act.company  eq job.company
              and mat-act.job      eq job.job
              and mat-act.job-no   eq job.job-no
              and mat-act.job-no2  eq job.job-no2
            use-index job:
          delete mat-act.
        end.

        for each job-mch
            where job-mch.company  eq job.company
              and job-mch.job      eq job.job
              and job-mch.job-no   eq job.job-no
              and job-mch.job-no2  eq job.job-no2
            use-index seq-idx:
          delete job-mch.
        end.

        for each mch-act
            where mch-act.company  eq job.company
              and mch-act.job      eq job.job
              and mch-act.job-no   eq job.job-no
              and mch-act.job-no2  eq job.job-no2
            use-index job:
          delete mch-act.
        end.

        for each job-prep
            where job-prep.company  eq job.company
              and job-prep.job      eq job.job
              and job-prep.job-no   eq job.job-no
              and job-prep.job-no2  eq job.job-no2:
          delete job-prep.
        end.

        for each misc-act
            where misc-act.company  eq job.company
              and misc-act.job      eq job.job
              and misc-act.job-no   eq job.job-no
              and misc-act.job-no2  eq job.job-no2
            use-index misc-idx:
          delete misc-act.
        end.
            
        if job.exported then DO:              
          job.stat = "X".    
          run jc/kiwiexp2.p (recid(job)).
        end.

        delete job.
      end.
    end.
      
    find first itemfg
        where itemfg.company eq oe-ordl.company
          and itemfg.i-no    eq oe-ordl.i-no
        no-error.

    if avail itemfg then do:
      ll-ans = NO.

      if itemfg.avg-cost       eq 0 and itemfg.last-cost eq 0 and
         itemfg.total-std-cost eq 0 and itemfg.beg-bal   eq 0 and
         itemfg.q-onh          eq 0 and itemfg.q-ono     eq 0 and
         itemfg.q-alloc        eq 0 and itemfg.q-back    eq 0 and
         itemfg.q-avail        eq 0 then
      for first est
          where est.company eq oe-ord.company
            and est.est-no  eq oe-ord.est-no
          no-lock,
            
          each eb
          where eb.company    eq est.company
            and eb.est-no     eq est.est-no
            and eb.stock-no   ne ""
            and (eb.stock-no  eq oe-ordl.i-no or
                 est.est-type eq 2 or est.est-type eq 6):
          
        IF NOT ll-ans THEN DO:
          ll-ans = YES.
          MESSAGE "Remove FG Item#" +
                  (IF est.est-type EQ 2 OR est.est-type eq 6 THEN "s" ELSE "")
                  "from the Estimate?"
                  VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                  UPDATE ll-ans.
        END.
            
        IF ll-ans THEN eb.stock-no = "".
        ELSE LEAVE.

        RELEASE eb.
      end.
    end.
      
    if oe-ordl.t-inv-qty = 0 then assign v-new-ord = yes.
    /*if index("NAH",oe-ord.stat) gt 0 THEN delete oe-ordl.*/
  end. /* each oe-ordl */

  /*task 07221004*/
  if index("NAH",oe-ord.stat) gt 0 THEN
     FOR EACH oe-ordl OF oe-ord:
         DELETE oe-ordl.
     END.

  FIND CURRENT itemfg NO-LOCK NO-ERROR.

  RUN ar/cctaxrt.p (INPUT oe-ord.company, INPUT oe-ord.tax-gr,
                    OUTPUT v-tax-rate, OUTPUT v-frt-tax-rate).

  for each oe-ordm
      where oe-ordm.company eq oe-ord.company
        and oe-ordm.ord-no  eq oe-ord.ord-no:
    if oe-ordm.bill eq "Y" then do:
      v-totord = v-totord + oe-ordm.amt.
      if oe-ordm.tax and v-tax-rate gt 0 then
        assign
         v-tot-tax = v-tot-tax +
                     round((oe-ordm.amt * v-tax-rate) / 100,2)
         v-totord  = v-totord +
                     round((oe-ordm.amt * v-tax-rate) / 100,2).
    end.
         
    if index("NAH",oe-ord.stat) gt 0 then delete oe-ordm.
  end. /* each oe-ordm */
    
  FIND CURRENT oe-ordm NO-LOCK NO-ERROR.

  if oe-ord.f-bill then do:
    if v-fr-tax then
      v-totord = (v-totord + oe-ord.t-freight +
                  round((oe-ord.t-freight * v-frt-tax-rate) / 100,2)).
    else
      v-totord = v-totord + oe-ord.t-freight.
  end.

  if v-fr-tax then
    v-tot-tax = v-tot-tax + round((v-tot-freight * v-frt-tax-rate) / 100,2).

  find first cust
      where cust.company eq oe-ord.company
        and cust.cust-no eq oe-ord.cust-no
      no-error.
  if avail cust then do:
      cust.ord-bal  = cust.ord-bal - v-totord.
    if cust.ord-bal lt 0 then assign cust.ord-bal = 0.
    if v-new-ord then
      assign
       cust.n-sales[13]      = cust.n-sales[13]  - 1
       cust.n-sales[v-period] = cust.n-sales[v-period] - 1.
    if cust.n-sales[13]  lt 0 then assign cust.n-sales[13] = 0.
    if cust.n-sales[v-period] lt 0 then assign cust.n-sales[v-period] = 0.
    FIND CURRENT cust NO-LOCK.
  end.
       
  for each work-ordl:
    delete work-ordl.
  end.
  
  FOR EACH job-hdr NO-LOCK
      WHERE job-hdr.company EQ oe-ord.company
        AND job-hdr.ord-no  EQ oe-ord.ord-no
        AND job-hdr.est-no  EQ oe-ord.est-no:
          
    DO loop-limit = 1 TO 1000:
      FIND del-job-hdr WHERE ROWID(del-job-hdr) EQ ROWID(job-hdr)
          EXCLUSIVE NO-WAIT NO-ERROR.
      IF AVAIL del-job-hdr THEN DO:
        DELETE del-job-hdr.
        LEAVE.
      END. 
    END.
  END.


  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ).

    
  END. /* transaction */

  RUN release-shared-buffers.
         
  /* Code placed here will execute AFTER standard behavior.    */
  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
  RUN reopen-query IN WIDGET-HANDLE(char-hdl).
