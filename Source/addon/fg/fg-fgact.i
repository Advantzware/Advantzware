/* --------------------------------------------------- fg/fg-fgact.i 10/94 gb */
/* Finished Goods - Create Job Costing F/G WIP Record */
/* -------------------------------------------------------------------------- */

release job.
release fg-set.

if {1}.job-no    ne ""  and
   {2}.rita-code ne "T" then
find first job
    where job.company eq cocode
      and job.job-no  eq {1}.job-no
      and job.job-no2 eq {1}.job-no2
    no-lock no-error.

find first fg-set
    where fg-set.company eq cocode
      and fg-set.set-no  eq {1}.i-no
    no-lock no-error.
    
if avail job and job.opened then do:
  find first job-hdr
      where job-hdr.company eq cocode
        and job-hdr.job     eq job.job
        and job-hdr.job-no  eq job.job-no
        and job-hdr.job-no2 eq job.job-no2
        and job-hdr.i-no    eq {1}.i-no
      no-lock no-error.
  if avail job-hdr then do:
    v-fin-qty = 0.
    for each fg-act
        where fg-act.company eq cocode
          and fg-act.job     eq job.job
          and fg-act.job-no  eq job.job-no
          and fg-act.job-no2 eq job.job-no2
          and fg-act.i-no    eq {1}.i-no
        no-lock:
      v-fin-qty = v-fin-qty + fg-act.qty.
    end.

    if v-close-job gt 0                          and
       (job.stat eq "W" or v-close-job eq 2)     and
       v-fin-qty + {2}.t-qty ge job-hdr.qty * .9 and
       {2}.rita-code eq "R"                      and
       job.opened                                then do:

      choice = YES.

      for each job-hdr
          where job-hdr.company eq cocode
            and job-hdr.job     eq job.job
            and job-hdr.job-no  eq job.job-no
            and job-hdr.job-no2 eq job.job-no2
            and job-hdr.i-no    ne {1}.i-no
          no-lock:

        v-fin-qty = 0.
        for each fg-act
            where fg-act.company eq cocode
              and fg-act.job     eq job.job
              and fg-act.job-no  eq job.job-no
              and fg-act.job-no2 eq job.job-no2
              and fg-act.i-no    eq job-hdr.i-no
            no-lock:
          v-fin-qty = v-fin-qty + fg-act.qty.
        end.

        if v-fin-qty lt job-hdr.qty * .9 then do:
          choice = no.
          leave.
        end.
      end.

      if choice then do:
        create w-job.
        assign
         w-job.job-no = fill(" ",6 - length(trim(job.job-no))) +
                        trim(job.job-no) +
                        string(job.job-no2,"99")
         w-job.rec-id = recid(job).
      end.
    end.

    RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT job-hdr.loc).

    FIND FIRST itemfg-loc 
        WHERE itemfg-loc.company EQ itemfg.company
          AND itemfg-loc.i-no    EQ itemfg.i-no
          AND itemfg-loc.loc     EQ job-hdr.loc
        EXCLUSIVE-LOCK NO-ERROR.

    if itemfg.q-ono lt 0 THEN DO:  
        itemfg.q-ono = 0.
        IF AVAIL itemfg-loc THEN
          itemfg-loc.q-ono = 0.
    END.
    itemfg.q-avail = itemfg.q-onh + itemfg.q-ono - itemfg.q-alloc.

    IF AVAIL(itemfg-loc) THEN
      itemfg-loc.q-avail = itemfg-loc.q-onh + itemfg-loc.q-ono - itemfg-loc.q-alloc.

    find first fg-act
        where fg-act.company eq cocode
          and fg-act.fg-date eq {1}.rct-date
          and fg-act.job-no  eq {1}.job-no
          and fg-act.job-no2 eq {1}.job-no2
          and fg-act.i-no    eq {1}.i-no
          and fg-act.opn     eq yes
        exclusive-lock no-error.
    if not avail fg-act then do:
      create fg-act.
      assign
       fg-act.company = cocode
       fg-act.fg-date = {1}.rct-date
       fg-act.job     = job.job
       fg-act.job-no  = {1}.job-no
       fg-act.job-no2 = {1}.job-no2
       fg-act.cust-no = IF AVAIL po-ordl THEN po-ordl.cust-no ELSE ""
       fg-act.i-no    = {1}.i-no
       fg-act.i-name  = {1}.i-name
       fg-act.qty-uom = {1}.pur-uom
       fg-act.tag     = {2}.tag
       fg-act.loc     = {2}.loc
       fg-act.loc-bin = {2}.loc-bin
       fg-act.cost    = {2}.std-cost
       fg-act.opn     = yes
       fg-act.fg-time = time.
    end.

    if fg-act.cost eq 0 then fg-act.cost = {2}.std-cost.

    fg-act.qty = fg-act.qty + {2}.t-qty.
  end.
end.

/* end ---------------------------------- copr. 1994  advanced software, inc. */

