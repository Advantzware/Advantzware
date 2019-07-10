/* ---------------------------------------------------- oe/ordlup.p 03/98 JLF */
/* order lines update 2 - o/e module                                          */
/* UPDATES COSTS FOR ITEM                                                     */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
/*{sys/form/s-top.f}
*/

def shared buffer xoe-ord for oe-ord.
/*def shared var v-new-item as log no-undo. */
def shared var fil_id as recid no-undo.
def shared var nufile as log no-undo.

def var v-save-id as recid.
def var v-job-job like job-hdr.job.
def var v-qty-ord like oe-ordl.qty.
def var v-part-qty as dec.
DEF VAR v-q-back AS INT NO-UNDO.
DEF VAR K_FRAC AS DEC INIT 6.25 NO-UNDO.

def buffer xitemfg for itemfg.

{ce/print4a.i shared}

{fg/fullset.i NEW}


{ce/msfcalc.i}
    
{sys/inc/f16to32.i}

DISABLE TRIGGERS FOR LOAD OF itemfg.

find oe-ordl where recid(oe-ordl) eq fil_id no-lock no-error.

if not avail job-hdr then
find first job-hdr
    where job-hdr.company eq cocode
      and job-hdr.i-no    eq oe-ordl.i-no
      and job-hdr.job-no  eq oe-ordl.job-no
      and job-hdr.job-no2 eq oe-ordl.job-no2
      and job-hdr.ord-no  eq oe-ordl.ord-no
    use-index job-no no-error.

/**** create job-hdr --- xjob header file ****/
if oe-ordl.est-no ne "" or
   (oe-ordl.est-no eq "" and oe-ordl.job-no ne "") then do:

  if not avail job-hdr then do:
     find first job where job.company eq cocode
                      and job.job-no  eq oe-ordl.job-no
                      and job.job-no2 eq oe-ordl.job-no2
                      no-lock no-error.
    if avail job then v-job-job = job.job.
    else do:
       find last job-hdr where job-hdr.company eq cocode
                         use-index job no-lock no-error.
       if not avail job-hdr then do:
          create job-hdr.
          assign job-hdr.company = cocode
                 job-hdr.job-no  = "FIRST".
       end.
       find last job where job.company eq cocode
                   use-index job no-lock no-error.
       if not avail job then do:
          create job.
          assign job.company = cocode
                 job.job-no  = "FIRST".
       end.
       if job-hdr.job gt job.job then v-job-job = job-hdr.job + 1.
       if job.job ge job-hdr.job then v-job-job = job.job + 1.

       create job.
       assign job.job        = v-job-job
              job.company    = cocode
              job.loc        = locode
              job.est-no     = xoe-ord.est-no
              job.job-no     = oe-ordl.job-no
              job.job-no2    = oe-ordl.job-no2
             job.stat       = "P".
    end.

    create job-hdr.
    assign
     job-hdr.company    = cocode
     job-hdr.loc        = locode
     job-hdr.est-no     = oe-ordl.est-no
     job-hdr.i-no       = oe-ordl.i-no
     job-hdr.qty        = oe-ordl.qty   /**/
     job-hdr.job-no     = oe-ordl.job-no
     job-hdr.job-no2    = oe-ordl.job-no2
     job-hdr.job        = v-job-job
     job-hdr.cust-no    = oe-ordl.cust-no
     job-hdr.ord-no     = oe-ordl.ord-no
     job-hdr.po-no      = oe-ordl.po-no
     job-hdr.frm        = oe-ordl.form-no.
     
    {util/mkjobkey.i}
  end. /* not avail job-hdr */

  /** if this is a new line item then write job number to order line **/
  if nufile then do:
    find oe-ordl where recid(oe-ordl) eq fil_id.
    oe-ordl.j-no = job-hdr.j-no.
    find oe-ordl where recid(oe-ordl) eq fil_id no-lock.
  end. /* nufile */

  find first est where est.company = oe-ordl.company
                   and est.est-no eq oe-ordl.est-no no-lock no-error.

  find first xjob where xjob.i-no eq oe-ordl.i-no no-error.

  IF AVAIL xjob THEN DO:
    if avail est and (est.est-type eq 2 or est.est-type eq 6) then do:
      assign
       job-hdr.std-mat-cost = 0
       job-hdr.std-lab-cost = 0
       job-hdr.std-fix-cost = 0
       job-hdr.std-var-cost = 0.
      for each xjob:
        assign
         job-hdr.std-mat-cost = xjob.mat + job-hdr.std-mat-cost
         job-hdr.std-lab-cost = xjob.lab + job-hdr.std-lab-cost
         job-hdr.std-fix-cost = xjob.foh + job-hdr.std-fix-cost
         job-hdr.std-var-cost = xjob.voh + job-hdr.std-var-cost.
      end.
    end.

    else do:
      assign
       job-hdr.std-mat-cost = xjob.mat
       job-hdr.std-lab-cost = xjob.lab
       job-hdr.std-fix-cost = xjob.foh
       job-hdr.std-var-cost = xjob.voh.
    end.
  end.

  job-hdr.std-tot-cost = job-hdr.std-mat-cost +
                         job-hdr.std-lab-cost +
                         job-hdr.std-fix-cost +
                         job-hdr.std-var-cost.
end.   /* if oe-ordl.est-no ne "" */   /*DAR*/

MESSAGE "ordlup" oe-ordl.i-no .
find first itemfg
    where itemfg.company eq cocode
      and itemfg.i-no    eq oe-ordl.i-no
    use-index i-no no-error.

if avail itemfg then do:
  itemfg.cust-po-no = oe-ordl.po-no.

  IF xoe-ord.type NE "T" THEN /*itemfg.q-alloc = itemfg.q-alloc + oe-ordl.qty.*/
      RUN fg/calcqa&b.p (ROWID(itemfg), OUTPUT itemfg.q-alloc, OUTPUT v-q-back).
  IF itemfg.q-alloc LT 0 THEN itemfg.q-alloc = 0.
MESSAGE "ordlup" itemfg.q-alloc itemfg.q-onh oe-ordl.i-no .
  assign
   itemfg.q-avail   = itemfg.q-onh + itemfg.q-ono - itemfg.q-alloc
   itemfg.q-ptd     = itemfg.q-ptd + oe-ordl.qty
   itemfg.q-ord-ytd = itemfg.q-ord-ytd + oe-ordl.qty.

  IF itemfg.isaset THEN DO:                 /** Update Set Parts */
   IF CAN-FIND(FIRST fg-set WHERE fg-set.company EQ itemfg.company
                              AND fg-set.set-no  EQ itemfg.i-no
                              AND fg-set.part-no NE fg-set.set-no) THEN
     RUN fg/fullset.p (ROWID(itemfg)).

   FOR EACH tt-fg-set BREAK BY tt-fg-set.set-no:
    if first(tt-fg-set.set-no) then
      assign
       itemfg.t-len      = 0
       itemfg.t-wid      = 0
       itemfg.t-sqin     = 0
       itemfg.t-sqft     = 0
       itemfg.weight-100 = 0.

    find first xitemfg
        where xitemfg.company eq cocode
          and xitemfg.i-no    eq tt-fg-set.part-no
        use-index i-no no-error.

    if not avail xitemfg then next.
    
    find first eb
        where eb.company eq oe-ordl.company 
          and eb.est-no  eq oe-ordl.est-no
          and eb.form-no eq 0
        no-lock no-error.

    if avail eb then
      assign
       itemfg.w-score[50] = eb.wid
       itemfg.l-score[50] = eb.len
       itemfg.d-score[50] = eb.dep.
     
    assign
     itemfg.t-wid       = itemfg.t-wid      + (xitemfg.t-wid      * tt-fg-set.part-qty-dec)
     itemfg.t-len       = itemfg.t-len      + (xitemfg.t-len      * tt-fg-set.part-qty-dec)
     itemfg.t-sqin      = itemfg.t-sqin     + (xitemfg.t-sqin     * tt-fg-set.part-qty-dec)
     itemfg.t-sqft      = itemfg.t-sqft     + (xitemfg.t-sqft     * tt-fg-set.part-qty-dec)
     itemfg.weight-100  = itemfg.weight-100 + (xitemfg.weight-100 * tt-fg-set.part-qty-dec).

    IF xoe-ord.type NE "T" THEN
      /*xitemfg.q-alloc = xitemfg.q-alloc + (oe-ordl.qty * tt-fg-set.part-qty-dec).*/
        RUN fg/calcqa&b.p (ROWID(itemfg), OUTPUT itemfg.q-alloc, OUTPUT v-q-back).
    IF xitemfg.q-alloc LT 0 THEN xitemfg.q-alloc = 0.

    MESSAGE "itemfg.q-alloc" itemfg.q-alloc  v-q-back  xitemfg.q-ono .
    assign
     xitemfg.q-avail   = xitemfg.q-onh + xitemfg.q-ono - xitemfg.q-alloc
     xitemfg.q-ptd     = xitemfg.q-ptd     + (oe-ordl.qty * tt-fg-set.part-qty-dec)
     xitemfg.q-ord-ytd = xitemfg.q-ord-ytd + (oe-ordl.qty * tt-fg-set.part-qty-dec).
   END.
  END. /* isaset */

  else do:
    find first eb
        where eb.company = oe-ordl.company
          and eb.est-no    eq oe-ordl.est-no
          and eb.form-no  eq oe-ordl.form-no
          and eb.blank-no eq oe-ordl.blank-no
        no-lock no-error.
    {sys/inc/updfgdim.i "eb"}
  end.
end.

/* end ---------------------------------- copr. 1998  advanced software, inc. */
