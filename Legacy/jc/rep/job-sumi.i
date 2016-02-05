
DEF VAR li-qty AS INT NO-UNDO.
DEF VAR v-qty-set AS INT NO-UNDO.

for each work-item:
  delete work-item.
end.

for each work-mch:
   delete work-mch.
end.

EMPTY TEMP-TABLE tt-ordm-amt.

for each job-hdr
    where job-hdr.company eq cocode
      and job-hdr.job     eq job.job
      and job-hdr.job-no  eq job.job-no
      and job-hdr.job-no2 eq job.job-no2
    use-index job no-lock
    break by job-hdr.i-no
          by job-hdr.frm:

  find first cust
      where cust.company eq cocode
        and cust.cust-no eq job-hdr.cust-no
      no-lock no-error.
  find first itemfg
      where itemfg.company eq cocode
        and itemfg.i-no    eq job-hdr.i-no
      no-lock.
  find first oe-ord
      where oe-ord.company eq cocode
        and oe-ord.ord-no  eq job-hdr.ord-no
      no-lock no-error.
  find first oe-ordl
      where oe-ordl.company eq cocode
        and oe-ordl.ord-no  eq job-hdr.ord-no
        and oe-ordl.i-no    eq job-hdr.i-no
      use-index ord-no no-lock no-error.
  v-over-pct = IF AVAIL oe-ordl THEN oe-ordl.over-pct ELSE
               IF AVAIL oe-ord  THEN oe-ord.over-pct  ELSE
               IF AVAIL cust    THEN cust.over-pct    ELSE 0.

  if avail oe-ord then do:
    if first(job-hdr.i-no) and job-hdr.ord-no ne 0 THEN DO:

    for each ar-inv FIELDS(t-comm f-bill freight)
        where ar-inv.company eq cocode
          and ar-inv.ord-no  eq oe-ord.ord-no
        no-lock:
      v-comm = v-comm + ar-inv.t-comm.
      if not ar-inv.f-bill then v-frate = v-frate + ar-inv.freight.
    end.

    IF v-charge-prep THEN
       FOR EACH oe-ordm FIELDS(oe-ordm.company oe-ordm.ord-no oe-ordm.LINE oe-ordm.charge oe-ordm.bill oe-ordm.amt) 
                               OF oe-ord NO-LOCK WHERE oe-ordm.bill = "I":
          FIND FIRST tt-ordm-amt WHERE tt-ordm-amt.company = oe-ordm.company
                                   AND tt-ordm-amt.ord-no  = oe-ordm.ord-no
                                   AND tt-ordm-amt.LINE    = oe-ordm.LINE
                                   AND tt-ordm-amt.charge  = oe-ordm.charge 
               NO-LOCK NO-ERROR.
          IF NOT AVAIL tt-ordm-amt THEN DO:
              CREATE tt-ordm-amt.
              ASSIGN tt-ordm-amt.company = oe-ordm.company
                     tt-ordm-amt.ord-no  = oe-ordm.ord-no
                     tt-ordm-amt.LINE    = oe-ordm.LINE
                     tt-ordm-amt.charge  = oe-ordm.charge
                     tt-ordm-amt.bill    = oe-ordm.bill
                     tt-ordm-amt.amt     = oe-ordm.amt.
          END. /* not avail tt-ordm-amt */                                        
       END. /* each oe-ordm */
    END. /* first-of job-hdr.i-no */

  end.
  
  if last-of(job-hdr.frm) then do:
    create work-item.
    assign
     work-item.i-no    = job-hdr.i-no
     work-item.form-no = job-hdr.frm
     work-item.qty-job = job-hdr.qty.

    if avail oe-ordl THEN DO:
      assign
       work-item.qty-ord = oe-ordl.qty
       work-item.price   = oe-ordl.price
       v-uom             = oe-ordl.pr-uom.
    END.
    ELSE DO:
         assign
         work-item.qty-ord = job-hdr.qty
         work-item.price   = itemfg.sell-price
         v-uom             = itemfg.sell-uom.

    END.
       
    if v-uom ne "M" THEN DO:


            IF v-uom begins "L" THEN
                ASSIGN work-item.price = (work-item.price / work-item.qty-ord *
                                          (if work-item.qty-ord lt 0 then -1 else 1000)).

            ELSE
            IF v-uom eq "CS" then
                ASSIGN work-item.price = (work-item.price / (IF avail oe-ordl AND oe-ordl.cas-cnt ne 0 then
                                                             oe-ordl.cas-cnt
                                                           ELSE IF itemfg.case-count ne 0 then
                                                             itemfg.case-count ELSE 1))
                                      * 1000.
            ELSE
            IF v-uom eq "C" then
                ASSIGN work-item.price = (work-item.price / 100).

            ELSE
                ASSIGN work-item.price = (work-item.price * 1000).

    END.

    ASSIGN
       work-item.qty-all = work-item.qty-ord * (1 + (v-over-pct / 100))
       li-qty = 0.

    IF itemfg.isaset AND itemfg.alloc THEN
    FOR EACH fg-act FIELDS(qty)
        WHERE fg-act.company eq job-hdr.company
          AND fg-act.job-no  eq job-hdr.job-no
          AND fg-act.job-no2 eq job-hdr.job-no2
          AND fg-act.i-no    eq job-hdr.i-no
        NO-LOCK:
      li-qty = li-qty + fg-act.qty.
    END.

    FIND FIRST eb WHERE
         eb.company EQ job.company AND
         eb.est-no EQ job.est-no AND
         eb.form-no EQ work-item.form-no AND
         eb.stock-no EQ work-item.i-no
         NO-LOCK NO-ERROR.

    FIND FIRST est WHERE
         est.company EQ job.company AND
         est.est-no EQ job.est-no
         NO-LOCK NO-ERROR.

/*Task: 05071402 - calculating the received qty * the yield qty is incorrect*/
/*     IF AVAIL eb AND AVAIL est THEN */
/*     DO:                            */
/*        IF est.est-type LE 4 THEN   */
/*           v-qty-set = eb.cust-%.   */
/*        ELSE                        */
/*           v-qty-set = eb.yld-qty.  */
/*     END.                           */
/*     ELSE                           */
/*        v-qty-set = 1.              */

    IF v-qty-set EQ 0 THEN
       v-qty-set = 1.

    FOR EACH fg-rcpth NO-LOCK
        WHERE fg-rcpth.company   EQ job-hdr.company
          AND fg-rcpth.i-no      EQ job-hdr.i-no
          AND fg-rcpth.job-no    EQ job-hdr.job-no
          AND fg-rcpth.job-no2   EQ job-hdr.job-no2
          AND fg-rcpth.rita-code EQ "R",

        EACH fg-rdtlh NO-LOCK
        WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
          AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
        
        BREAK BY fg-rcpth.company:

      IF FIRST(fg-rcpth.company) THEN li-qty = 0.

      li-qty = li-qty + fg-rdtlh.qty.
    END.

    ASSIGN
     work-item.qty-prod = work-item.qty-prod + (li-qty /* * v-qty-set */ )
     work-item.press-1  = work-item.press-1  + li-qty.
  end.
end.

v-t-bord = 0.
FOR EACH work-item BREAK BY work-item.i-no:
  v-t-bord = v-t-bord + work-item.qty-job.

  IF LAST-OF(work-item.i-no) THEN DO:
    v-qty = work-item.qty-ord.

    FOR EACH b-wi WHERE b-wi.i-no EQ work-item.i-no:
      b-wi.qty-ord = b-wi.qty-ord * (b-wi.qty-job / v-t-bord).
    END.

    v-t-bord = 0.
    FOR EACH b-wi WHERE b-wi.i-no EQ work-item.i-no BREAK BY b-wi.i-no:
      v-t-bord = v-t-bord + b-wi.qty-ord.
      IF LAST(b-wi.i-no) AND v-t-bord NE v-qty THEN
        b-wi.qty-ord = b-wi.qty-ord + (v-qty - v-t-bord).
    END.

    v-t-bord = 0.
  END.
END.

v-t-bord = 0.
for each work-item:
  v-t-bord = v-t-bord + work-item.qty-ord.
end.

for each work-item,
    each job-hdr
    where job-hdr.company eq cocode
      and job-hdr.job     eq job.job
      and job-hdr.job-no  eq job.job-no
      and job-hdr.job-no2 eq job.job-no2
      and job-hdr.i-no    eq work-item.i-no
      AND job-hdr.frm     EQ work-item.form-no
    no-lock,

    first ef
    where ef.company  eq job-hdr.company
       AND ef.est-no  EQ job-hdr.est-no
       and ef.form-no eq job-hdr.frm
    no-lock,

    first eb of ef
    where eb.blank-no eq job-hdr.blank-no
    no-lock:

  run sys/inc/numup.p (est.company, est.est-no, ef.form-no, output v-num-up).

  for each mch-act
      where mch-act.company   eq cocode
        and mch-act.job       eq job-hdr.job
        and mch-act.job-no    eq job-hdr.job-no
        and mch-act.job-no2   eq job-hdr.job-no2
        and mch-act.frm       eq job-hdr.frm
        and (mch-act.blank-no eq job-hdr.blank-no or mch-act.blank-no eq 0)
      no-lock,
      first mach
      where mach.company eq cocode
        and mach.m-code  eq mch-act.m-code
      no-lock:

    FIND FIRST job-mch NO-LOCK
        WHERE job-mch.company   EQ mch-act.company
          AND job-mch.m-code    EQ mch-act.m-code
          AND job-mch.job       EQ mch-act.job
          AND job-mch.job-no    EQ mch-act.job-no
          AND job-mch.job-no2   EQ mch-act.job-no2
          AND job-mch.frm       EQ mch-act.frm
          AND (job-mch.blank-no EQ mch-act.blank-no OR
               mch-act.blank-no EQ 0)
          AND job-mch.pass      EQ mch-act.pass
        NO-ERROR.

    if index("APB",mach.p-type) gt 0 then v-blk-pct = 1.

    else do:
      v-blk-pct = if est.est-type eq 3 then
                    if mch-act.blank-no ne 0 then 1
                    else (work-item.qty-ord / v-t-bord)
                  else (eb.num-up / v-num-up).
      if mch-act.dept eq "RC" or
         mch-act.dept eq "RS" then v-num-up =
        v-num-up * (IF AVAIL job-mch AND job-mch.n-out NE 0 THEN job-mch.n-out ELSE 1).
      v-blk-pct = v-blk-pct * v-num-up.
    end.

    work-item.act-spo = work-item.act-spo + ROUND(mch-act.waste * v-blk-pct,0).
  end.

  run sys/inc/numup.p (est.company, est.est-no, ef.form-no, output v-num-up).

  for each job-mch
      where job-mch.company   eq cocode
        and job-mch.job       eq job-hdr.job
        and job-mch.job-no    eq job-hdr.job-no
        and job-mch.job-no2   eq job-hdr.job-no2
        and job-mch.frm       eq job-hdr.frm
        and (job-mch.blank-no eq job-hdr.blank-no or job-mch.blank-no eq 0)
      no-lock,
      first mach
      where mach.company eq cocode
        and mach.m-code  eq job-mch.m-code
      no-lock
      break by job-mch.line desc:

    /*find first work-mch where work-mch.m-code eq job-mch.m-code no-error.
    if not avail work-mch then do:
      create work-mch.
      assign
       work-mch.m-code = job-mch.m-code
       work-mch.d-seq  = mach.d-seq.
    end.*/
    FIND FIRST work-mch
       WHERE work-mch.m-code    EQ job-mch.m-code
         AND work-mch.frm       EQ job-mch.frm
         AND (work-mch.blk      EQ job-mch.blank-no OR
              (work-mch.blk     EQ 1 AND
               mach.p-type      EQ "B" AND
               job-mch.blank-no EQ 0))
          AND work-mch.pass     EQ job-mch.pass
        NO-ERROR.
    IF NOT AVAIL work-mch THEN DO:
      CREATE work-mch.
      ASSIGN work-mch.m-code    = job-mch.m-code
             work-mch.frm       = job-mch.frm
             work-mch.blk       = IF mach.p-type EQ "B"    AND
                                     job-mch.blank-no EQ 0 THEN 1
                                                           ELSE job-mch.blank-no
             work-mch.pass      = job-mch.pass
             work-mch.d-seq     = mach.d-seq
             work-mch.est-speed = job-mch.speed
             work-mch.est-qty   = job-mch.run-qty.
    END.

    if first(job-mch.line) then v-t-ebspo = work-item.qty-prod.

    if index("APB",mach.p-type) gt 0 then v-blk-pct = 1.

    else do:
      v-blk-pct = if est.est-type eq 3 then
                    if job-mch.blank-no ne 0 then 1
                    else (work-item.qty-ord / v-t-bord)
                  else (eb.num-up / v-num-up).
      if job-mch.dept eq "RC" or
         job-mch.dept eq "RS" then v-num-up =
        v-num-up * (IF job-mch.n-out NE 0 THEN job-mch.n-out ELSE 1).
      v-blk-pct = v-blk-pct * v-num-up.
    end.

    v-t-ebspo = (v-t-ebspo / (1 - (job-mch.wst-prct / 100))) +
                (job-mch.mr-waste * v-blk-pct) - v-t-ebspo.
    {sys/inc/roundup.i v-t-ebspo}

    if v-t-ebspo / v-num-up ne trunc(v-t-ebspo / v-num-up,0) then
      v-t-ebspo = (trunc(v-t-ebspo / v-num-up,0) * v-num-up) + v-num-up.

    assign
     work-mch.run-waste = work-mch.run-waste +
                          (v-t-ebspo / v-blk-pct) - job-mch.mr-waste
     work-item.est-spo  = work-item.est-spo + v-t-ebspo
     v-t-ebspo          = work-item.est-spo + work-item.qty-prod.
  end.
end.

IF v-charge-prep THEN 
   FOR EACH tt-ordm-amt NO-LOCK:
       ASSIGN v-misc-prep = v-misc-prep + tt-ordm-amt.amt.
   END.

