/* --------------------------------------------------- oe/oe-ordel 3/94 RM    */
/* delete statement - order entry                                             */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/s-top.f}

def shared buffer xoe-ord for oe-ord.
def shared buffer xoe-ordl2 for oe-ordl.
def buffer xfg-set for fg-set.
def buffer xitemfg for itemfg.
def buffer xoe-ord1 for oe-ord.

def var v-totord like oe-ord.t-revenue.
def var v-tot-tax like oe-ord.tax.
def var v-tot-freight like oe-ord.t-freight.
def var v-qty-lft like oe-ordl.qty.
def var v-new-ord as logical initial no.
def var v-ext-price like oe-ordl.t-price.
def var v-tax-rate as dec format ">,>>9.99<<<".
def var v-frt-tax-rate like v-tax-rate.
def var v-period as int.
def var tmp-ordm-amt like oe-ordm.amt.
def var tmp-tax like oe-ord.tax.
def var v-continue as log.
def var v-blank-fg-on-est as int.
DEF VAR choice AS LOG NO-UNDO.
DEF VAR v-abortord AS LOG NO-UNDO.
DEF VAR fil_id AS RECID NO-UNDO.
DEF VAR call_id AS RECID NO-UNDO.

def shared workfile work-ordl like oe-ordl.

if xoe-ord.posted and xoe-ord.stat ne "N" and xoe-ord.stat ne "A" and
   xoe-ord.stat ne "H" then do:
  pause 0.
  display skip(1) "  Record already posted. No deletion allowed!  " skip(1)
          with no-labels no-box overlay centered frame nwj.
  hide frame nwj.
  return.
end.

find first period
    where period.company eq cocode
      and period.pst     le xoe-ord.ord-date
      and period.pend    ge xoe-ord.ord-date
    no-lock no-error.
v-period = if avail period and period.pstat then period.pnum else 1.

if avail xoe-ord then do:
  choice = no.
  pause 0.
  
  if v-abortord then choice = yes.
  
  else do:
    status input off.
    repeat:
      update "  DELETE this Record ? " choice auto-return
          with frame del overlay no-labels no-box.
      leave.
    end.
    hide frame del no-pause.
  end.
  
  if choice then do:
    assign
     fil_id = recid(xoe-ord)
     call_id = ?.
    find next xoe-ord WHERE xoe-ord.company EQ cocode no-lock no-error.
    if not avail xoe-ord then
    find prev xoe-ord WHERE xoe-ord.company EQ cocode no-lock no-error.
    if avail xoe-ord then call_id eq recid(xoe-ord).
    find xoe-ord where recid(xoe-ord) eq fil_id no-error no-wait.
    if not avail xoe-ord and locked xoe-ord then do:
      bell.
      pause 3 message " This Order Is Inuse!  Unable to Delete. ".
      return.
    end.
    
    if xoe-ord.job-no ne "" then do:
      find first job
          where job.company eq cocode
            and job.job-no  eq xoe-ord.job-no
            and job.job-no2 eq xoe-ord.job-no2
            and (job.stat eq "C" or job.stat eq "W" or job.stat eq "Z")
          use-index job-no no-lock no-error.
      if avail job then do:
        repeat:
          bell.
          pause 0.
          display
                " Order cannot be Deleted, Job has been Processed or Closed. "
                " You Must Close the Order. "
              with frame jobopen row 17 no-labels overlay centered width 62.
          pause 6 no-message.
          leave.
        end.  /* repeat */
        hide frame jobopen no-pause.
        return.
      end.
    end.
    
    if not v-abortord and
       (xoe-ord.est-no ne "" or int(xoe-ord.est-no) ne 0) then do:
      find first est
          where est.company eq xoe-ord.company
            and est.est-no  eq fill(" ",8 - length(trim(xoe-ord.est-no))) +
                               trim(xoe-ord.est-no)
          no-error.
      if avail est then do:
        find last xoe-ord1 where xoe-ord1.company = cocode and
                                 xoe-ord1.est-no = xoe-ord.est-no and
                                 RECID(xoe-ord1) NE fil_id
                                 no-lock no-error.
        if avail xoe-ord1 then
          assign
           est.ord-date = xoe-ord1.ord-date
           est.ord-no   = xoe-ord1.ord-no.
        else do:
          if est.ord-date eq xoe-ord.ord-date then est.ord-date = ?.
          if est.ord-no eq xoe-ord.ord-no then est.ord-no = 0.
        end.
      end.
    end. /* not abortord */
    
    v-abortord = no.
    
    find first oe-ordl of xoe-ord
        where oe-ordl.rel-stat
           or oe-ordl.t-inv-qty ne 0
           or oe-ordl.t-ship-qty ne 0
        no-lock no-error.
    if avail oe-ordl then do:
      repeat:
        bell.
        pause 0.
        display
           " Previous Quantities have been released for this Order. " skip
            with frame nodelt row 18 no-labels  overlay
                centered.
        pause 4 no-message.
        leave.
      end.  /* repeat */
      hide frame del no-pause.
      hide frame nodelt no-pause.
      return.
    end.
    
    for each xoe-ordl2 of xoe-ord:
      release fg-rcpth.
      find first fg-rcpts use-index cust-no
          where fg-rcpts.company eq cocode
            and fg-rcpts.cust-no eq xoe-ord.cust-no
            and fg-rcpts.job-no  eq
                                 fill(" ",6 - length(trim(xoe-ordl2.job-no))) +
                                 trim(xoe-ordl2.job-no)
            and fg-rcpts.job-no2 eq xoe-ordl2.job-no2
            and fg-rcpts.i-no    eq xoe-ordl2.i-no
          no-lock no-error.
      if not avail fg-rcpts then
      find first fg-rcpth use-index job
          where fg-rcpth.company eq cocode
            and fg-rcpth.job-no  eq
                                 fill(" ",6 - length(trim(xoe-ordl2.job-no))) +
                                 trim(xoe-ordl2.job-no)
            and fg-rcpth.job-no2 eq xoe-ordl2.job-no2
            and fg-rcpth.i-no    eq xoe-ordl2.i-no
          no-lock no-error.
      if ((avail fg-rcpts or avail fg-rcpth) and
          xoe-ordl2.job-no ne "") then do:
        repeat:
          bell.
          display " Finish Goods Transactions Exists For This Order. " skip
                  " Deleting Is Not Permitted!  You Must Close The Order. "
              with frame nodel2 row 18 no-labels
                   overlay centered.
          pause 4 no-message.
          leave.
        end.  /* repeat */
        hide frame nodel2 no-pause.
        return.
      end.
    end.
    
    for each oe-ordl of xoe-ord:
      run oe/oe-ordd.p(recid(oe-ordl),output v-continue).
      if not v-continue then return.
      
      for each oe-rel
          where oe-rel.company = oe-ordl.company
            and oe-rel.ord-no = oe-ordl.ord-no
            and oe-rel.i-no = oe-ordl.i-no
            and oe-rel.line = oe-ordl.line:
          FIND FIRST itemfg-loc 
              WHERE itemfg-loc.company EQ oe-rel.company
                AND itemfg-loc.i-no    EQ oe-rel.i-no
                AND itemfg-loc.loc     EQ oe-rel.spare-char-1
              EXCLUSIVE-LOCK NO-ERROR.      
          IF AVAIL itemfg-loc AND oe-rel.spare-dec-1 GT 0 THEN
              itemfg-loc.q-alloc = itemfg-loc.q-alloc - oe-rel.spare-dec-1.
          FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
          DELETE oe-rel.
      end. /* each oe-rel */
      
      find first itemfg
          where itemfg.company eq cocode
            and itemfg.i-no    eq oe-ordl.i-no
          no-error.
      if avail itemfg then do:
        RUN fg/chkfgloc.p (INPUT oe-ordl.i-no, INPUT xoe-ord.loc).
        FIND FIRST itemfg-loc 
            WHERE itemfg-loc.company EQ cocode
              AND itemfg-loc.i-no    EQ oe-ordl.i-no
              AND itemfg-loc.loc     EQ xoe-ord.loc
            EXCLUSIVE-LOCK NO-ERROR.

        IF xoe-ord.stat NE "T" THEN
          itemfg.q-alloc = itemfg.q-alloc - oe-ordl.qty.
        IF xoe-ord.stat NE "T" AND avail(itemfg-loc) THEN
          itemfg-loc.q-alloc = itemfg-loc.q-alloc - oe-ordl.qty.

        if itemfg.q-alloc lt 0 then do:
            
            assign itemfg.q-alloc = 0.
            IF AVAIL itemfg-loc THEN
                assign itemfg.q-alloc = 0.
        END.
            
        assign
         itemfg.q-avail   = itemfg.q-onh + itemfg.q-ono - itemfg.q-alloc
         itemfg.q-ptd     = itemfg.q-ptd - oe-ordl.qty
         itemfg.q-ord-ytd = itemfg.q-ord-ytd - oe-ordl.qty.

        IF AVAIL itemfg-loc THEN
        assign
         itemfg-loc.q-avail   = itemfg-loc.q-onh + itemfg-loc.q-ono - itemfg-loc.q-alloc
         itemfg-loc.q-ptd     = itemfg-loc.q-ptd - oe-ordl.qty
         itemfg-loc.q-ord-ytd = itemfg-loc.q-ord-ytd - oe-ordl.qty.       

        IF xoe-ord.stat NE "T" THEN
          RUN fg/comp-upd.p (RECID(itemfg), oe-ordl.qty * -1, "q-alloc", 0).
        FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
      end. /* avail itemfg */
      
      run ar/cctaxrt.p (input xoe-ord.company, input xoe-ord.tax-gr,
                        output v-tax-rate, output v-frt-tax-rate).

      assign
       v-qty-lft   = oe-ordl.qty - oe-ordl.inv-qty
       v-ext-price = 0.
       
      if v-qty-lft gt 0 then do:
        if oe-ordl.pr-uom begins "L" AND oe-ordl.pr-uom NE "LB" then
          assign v-ext-price = oe-ordl.price -
                round( (oe-ordl.price * oe-ordl.disc) / 100, 2).
        else
        if oe-ordl.pr-uom = "CS" then do:
          find first itemfg {sys/look/itemfgrl.w}
               and itemfg.i-no = oe-ordl.i-no no-lock no-error.
          if avail itemfg and itemfg.case-count ne 0 then
            assign v-ext-price = ((v-qty-lft /
                     itemfg.case-count) * oe-ordl.price) -
                     round((((v-qty-lft / itemfg.case-count) *
                     oe-ordl.price) * oe-ordl.disc) / 100, 2).
          else
            assign v-ext-price = (v-qty-lft *
                     oe-ordl.price) - round(((v-qty-lft *
                     oe-ordl.price) * oe-ordl.disc) / 100, 2).
        end.
        else if oe-ordl.pr-uom = "C" then
          assign v-ext-price = ((v-qty-lft / 100) *
                     oe-ordl.price) - round((((v-qty-lft / 100) *
                     oe-ordl.price) * oe-ordl.disc) / 100, 2).
        else if oe-ordl.pr-uom = "M" then
          assign v-ext-price = ((v-qty-lft / 1000) *
                     oe-ordl.price) - round(( ((v-qty-lft / 1000) *
                     oe-ordl.price) * oe-ordl.disc) / 100, 2).
        else /** default per thousand **/
          assign v-ext-price = ((v-qty-lft) *
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
        
        if xoe-ord.stat = "N" or xoe-ord.stat = "A" or xoe-ord.stat = "H" then
          delete oe-ordm.
      end. /* each oe-ordm */
      
      if oe-ordl.job-no ne "" then do:
        find first job
            where job.company eq cocode
              and job.job-no  eq oe-ordl.job-no
              and job.job-no2 eq oe-ordl.job-no2
            use-index job-no no-lock no-error.

        for each job-hdr
            where job-hdr.company  eq cocode
              and job-hdr.est-no   eq oe-ordl.est-no
              and job-hdr.job-no   eq oe-ordl.job-no
              and job-hdr.job-no2  eq oe-ordl.job-no2
              and ((job-hdr.ord-no eq oe-ordl.ord-no and
                    job-hdr.i-no   eq oe-ordl.i-no) or
                   job-hdr.ord-no  eq 0) 
            use-index enum:
          
          if job-hdr.ord-no ne 0 then do:
            /*if avail job and job.stat ne "P" then do:
              find first itemfg where itemfg.company = cocode and
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
          
          delete job-hdr.
        end.
        
        find first job
            where job.company eq cocode
              and job.job-no  eq oe-ordl.job-no
              and job.job-no2 eq oe-ordl.job-no2
            use-index job-no exclusive no-error.

        if avail job then do:
        
        
          find first job-hdr
              where job-hdr.company eq cocode
                and job-hdr.job     eq job.job
                and job-hdr.job-no  eq job.job-no
                and job-hdr.job-no2 eq job.job-no2
              no-lock no-error.
          if not avail job-hdr then do:
            run jc/jc-dall.p (recid(job)).
                      
            for each job-mat
                where job-mat.company  eq cocode
                  and job-mat.job      eq job.job
                  and job-mat.job-no   eq job.job-no
                  and job-mat.job-no2  eq job.job-no2
                use-index seq-idx:
              delete job-mat.
            end.

            for each mat-act
                where mat-act.company  eq cocode
                  and mat-act.job      eq job.job
                  and mat-act.job-no   eq job.job-no
                  and mat-act.job-no2  eq job.job-no2
                use-index job:
              delete mat-act.
            end.

            for each job-mch
                where job-mch.company  eq cocode
                  and job-mch.job      eq job.job
                  and job-mch.job-no   eq job.job-no
                  and job-mch.job-no2  eq job.job-no2
                use-index seq-idx:
              delete job-mch.
            end.

            for each mch-act
                where mch-act.company  eq cocode
                  and mch-act.job      eq job.job
                  and mch-act.job-no   eq job.job-no
                  and mch-act.job-no2  eq job.job-no2
                use-index job:
              delete mch-act.
            end.

            for each job-prep
                where job-prep.company  eq cocode
                  and job-prep.job      eq job.job
                  and job-prep.job-no   eq job.job-no
                  and job-prep.job-no2  eq job.job-no2:
              delete job-prep.
            end.

            for each misc-act
                where misc-act.company  eq cocode
                  and misc-act.job      eq job.job
                  and misc-act.job-no   eq job.job-no
                  and misc-act.job-no2  eq job.job-no2
                use-index misc-idx:
              delete misc-act.
            end.

            FOR EACH job-farm
                WHERE job-farm.company EQ job.company
                  AND job-farm.job-no  EQ job.job-no
                  AND job-farm.job-no2 EQ job.job-no2
                EXCLUSIVE:
              DELETE job-farm.
            END.

            FOR EACH job-farm-rctd
                WHERE job-farm-rctd.company EQ job.company
                  AND job-farm-rctd.job-no  EQ job.job-no
                  AND job-farm-rctd.job-no2 EQ job.job-no2
                EXCLUSIVE:
              DELETE job-farm-rctd.
            END.
            if job.exported then do:
              job.stat = "X".
              run jc/kiwiexp2.p (recid(job)).
            end.

            delete job.
          end.
        end.
      end.
      
      find first itemfg
          where itemfg.company eq cocode
            and itemfg.i-no    eq oe-ordl.i-no
          no-error.
      if avail itemfg then do:
        choice = NO.

        if itemfg.avg-cost       eq 0 and itemfg.last-cost eq 0 and
           itemfg.total-std-cost eq 0 and itemfg.beg-bal   eq 0 and
           itemfg.q-onh          eq 0 and itemfg.q-ono     eq 0 and
           itemfg.q-alloc        eq 0 and itemfg.q-back    eq 0 and
           itemfg.q-avail        eq 0 then
        for first est
            where est.company eq xoe-ord.company
              and est.est-no  eq fill(" ",8 - length(trim(xoe-ord.est-no))) +
                                 trim(xoe-ord.est-no)
            no-lock,
            
            each eb
            where eb.company    eq est.company
              and eb.est-no     eq est.est-no
              and eb.stock-no   ne ""
              and (eb.stock-no  eq oe-ordl.i-no or
                   est.est-type eq 2 or est.est-type eq 6):
          
          IF NOT choice THEN DO:
            choice = YES.
            MESSAGE "Remove FG Item#" +
                    (IF est.est-type EQ 2 OR est.est-type eq 6 THEN "s" ELSE "")
                    "from the Estimate?"
                    VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                    UPDATE choice.
          END.
            
          IF choice THEN eb.stock-no = "".
          ELSE LEAVE.
        end.
      end.
      
      if oe-ordl.t-inv-qty = 0 then assign v-new-ord = yes.
      if index("NAH",xoe-ord.stat) gt 0 then delete oe-ordl.
    end. /* each oe-ordl */
    
    RUN ar/cctaxrt.p (INPUT xoe-ord.company, INPUT xoe-ord.tax-gr,
                      OUTPUT v-tax-rate, OUTPUT v-frt-tax-rate).

    for each oe-ordm
        where oe-ordm.company eq xoe-ord.company
          and oe-ordm.ord-no  eq xoe-ord.ord-no:
      if oe-ordm.bill eq "Y" then do:
        v-totord = v-totord + oe-ordm.amt.
        if oe-ordm.tax and v-tax-rate gt 0 then
          assign
           v-tot-tax = v-tot-tax +
                       round((oe-ordm.amt * v-tax-rate) / 100,2)
           v-totord  = v-totord +
                       round((oe-ordm.amt * v-tax-rate) / 100,2).
      end.
         
      if index("NAH",xoe-ord.stat) gt 0 then delete oe-ordm.
    end. /* each oe-ordm */
    
    if xoe-ord.f-bill then do:
      /*if v-fr-tax then
        v-totord = (v-totord + xoe-ord.t-freight +
                    round((xoe-ord.t-freight * v-frt-tax-rate) / 100,2)).
      else*/
        v-totord = v-totord + xoe-ord.t-freight.
    end.

    /*if v-fr-tax then
       v-tot-tax = v-tot-tax +
                  round((v-tot-freight * v-frt-tax-rate) / 100,2).*/
    find first cust
         {sys/ref/cust.w}
           and cust.cust-no eq xoe-ord.cust-no
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
      release cust.
    end.
       
    for each work-ordl:
        /*{sys/inc/debug.i "Deleting work-ordl in oe/ordel.p. ord-no is "
        "work-ordl.ord-no"
        "job-no part-no cust-no are "
        "work-ordl.job-no work-ordl.part-no work-ordl.cust-no"}*/
      delete work-ordl.
    end.
       
    for each job-hdr
        where job-hdr.company eq cocode
          and job-hdr.loc     eq locode
          and job-hdr.ord-no  eq xoe-ord.ord-no
          and job-hdr.est-no  eq xoe-ord.est-no:
      delete job-hdr.
    end.
              
    if index("NAH",xoe-ord.stat) gt 0 then do:
/*  06211305 - Now using a sequence instead of n-ord        */
/*       find first oe-ctrl WHERE oe-ctrl.company EQ cocode */
/*            exclusive-lock no-error.                      */
/*       if xoe-ord.ord-no = oe-ctrl.n-ord - 1 then         */
/*         oe-ctrl.n-ord = oe-ctrl.n-ord - 1.               */
      delete xoe-ord.
    end.
    
    else xoe-ord.stat = "D".
  END.
END.

/* end ---------------------------------- copr. 1993  advanced software, inc. */
