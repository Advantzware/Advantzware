/* --------------------------------------------- sys/inc/bordcost.p 01/02 JLF */
/* Calculate the total cost of the board received for a job/item              */
/* -------------------------------------------------------------------------- */

def input  parameter v-job-no   like job.job-no.
def input  parameter v-job-no2  like job.job-no2.
def input  parameter v-i-no     like itemfg.i-no.
DEF INPUT PARAMETER  v-bol-no   LIKE oe-boll.bol-no.
def input  parameter v-qty      as   int.
def input  parameter v-act-cost as   log.
def output parameter v-tot-cost as   dec.

{sys/inc/var.i shared}
{sys/form/s-top.f}

def var v-job-qty               like job-hdr.qty.
def var v-cost                  as   dec.
DEF VAR lv-est-type LIKE est.est-type.


if v-job-no eq "" then do:

    /* Find BOL for item. */
    FIND FIRST oe-boll NO-LOCK WHERE
        oe-boll.company = cocode AND
        oe-boll.bol-no = v-bol-no AND
        oe-boll.i-no = v-i-no NO-ERROR.

    IF AVAILABLE oe-boll THEN
        ASSIGN v-job-no = oe-boll.job-no
               v-job-no2 = oe-boll.job-no2.


/*   find first itemfg                                                        */
/*       where itemfg.company eq cocode                                       */
/*         and itemfg.i-no    eq v-i-no                                       */
/*       no-lock no-error.                                                    */
/*   if avail itemfg then do:                                                 */
/*     /* If item has estimate, then find job of estimate. */                 */
/*     IF TRIM(itemfg.est-no) NE "" THEN                                      */
/*     FOR EACH job                                                           */
/*         WHERE job.company EQ cocode /*itemfg.company*/                     */
/*           AND job.est-no  EQ itemfg.est-no /* v-est-no*/                   */
/*           AND CAN-FIND(FIRST job-hdr                                       */
/*                        WHERE job-hdr.company EQ job.company                */
/*                          AND job-hdr.job     EQ job.job                    */
/*                          AND job-hdr.job-no  EQ job.job-no                 */
/*                          AND job-hdr.job-no2 EQ job.job-no2                */
/*                          AND job-hdr.i-no    EQ itemfg.i-no /* v-i-no*/ )  */
/*         BY job.job DESC:                                                   */
/*       LEAVE.                                                               */
/*     END. /* FOR EACH job of itemfg estimate */                             */
/*                                                                            */
/*     /* If job not found, then set cost from item. */                       */
/*     IF NOT AVAIL job THEN DO:                                              */
/*         v-tot-cost = itemfg.std-mat-cost * (v-qty / 1000).                 */
/*         /* stacey */                                                       */
/*         MESSAGE "set cost to itemfg: " v-tot-cost                          */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK.                             */
/*     END. /* IF NOT AVAIL job */                                            */
/*                                                                            */
/*   end. /* if avail itemfg */                                               */
end. /* if v-job-no eq "" */

/* else */
find first job
    where job.company eq cocode
      and job.job-no  eq v-job-no
      and job.job-no2 eq v-job-no2
    no-lock no-error.
            
if avail job then
find first job-hdr
    where job-hdr.company eq cocode
      and job-hdr.job     eq job.job
      and job-hdr.job-no  eq job.job-no
      and job-hdr.job-no2 eq job.job-no2
      and job-hdr.i-no    eq v-i-no
    no-lock no-error.

if avail job-hdr then do:
  IF job.est-no NE "" THEN
  FIND FIRST est
      WHERE est.company EQ job.company
        AND est.est-no  EQ job.est-no
      NO-LOCK NO-ERROR.
  lv-est-type = IF AVAIL est THEN est.est-type ELSE 1.
  IF lv-est-type GT 4 THEN lv-est-type = lv-est-type - 4.

  for each fg-rcpth
      where fg-rcpth.company   eq cocode
        and fg-rcpth.job-no    eq job-hdr.job-no
        and fg-rcpth.job-no2   eq job-hdr.job-no2
        and fg-rcpth.i-no      eq job-hdr.i-no
        and fg-rcpth.rita-code eq "R"
      no-lock,

      each fg-rdtlh
      where fg-rdtlh.r-no eq fg-rcpth.r-no
      no-lock:

    v-job-qty = v-job-qty + fg-rdtlh.qty.
  end.

  if v-act-cost then do:
    for each mat-act
        where mat-act.company eq cocode
          and mat-act.job     eq job-hdr.job
          and mat-act.job-no  eq job-hdr.job-no
          and mat-act.job-no2 eq job-hdr.job-no2
          and (mat-act.s-num  eq job-hdr.frm OR lv-est-type EQ 2)
        no-lock,

        first job-mat
        where job-mat.company  eq cocode
          and job-mat.job      eq mat-act.job
          and job-mat.frm      eq mat-act.s-num
          and job-mat.blank-no eq mat-act.b-num
          and job-mat.i-no     eq mat-act.i-no
        no-lock,

        first item
        where item.company eq cocode
          and item.i-no    eq mat-act.i-no
          and index("BA",item.mat-type) gt 0
        no-lock:
      
      if item.r-wid eq 0 then
        run sys/ref/convcuom.p(job-mat.sc-uom, mat-act.qty-uom,
                               (if job-mat.basis-w  ne 0 then
                                  job-mat.basis-w else item.basis-w),
                               (if job-mat.len      ne 0 then
                                  job-mat.len else item.s-len),
                               (if job-mat.wid      ne 0 then
                                  job-mat.wid else item.s-wid),
                               item.s-dep,   
                               mat-act.cost, output v-cost).

      else
        run sys/ref/convcuom.p(job-mat.sc-uom, mat-act.qty-uom,
                               (if job-mat.basis-w  ne 0 then
                                  job-mat.basis-w else item.basis-w),
                               job-mat.len,
                               (if job-mat.wid      ne 0 then
                                  job-mat.wid else item.r-wid),
                               item.s-dep,   
                               mat-act.cost, output v-cost).

      IF mat-act.ext-cost EQ 0 OR mat-act.ext-cost EQ ? THEN
        v-tot-cost = v-tot-cost + (v-cost * mat-act.qty).
      ELSE
        v-tot-cost = v-tot-cost + mat-act.ext-cost.
    end.
    
    assign
     v-tot-cost = v-tot-cost * (job-hdr.qty / v-job-qty)
     v-tot-cost = v-tot-cost / job-hdr.qty * v-qty.
  end. /* if v-act-cost */
  
  else
  for each job-mat
      where job-mat.company eq cocode
        and job-mat.job     eq job-hdr.job
        and job-mat.job-no  eq job-hdr.job-no
        and job-mat.job-no2 eq job-hdr.job-no2
        and (job-mat.frm    eq job-hdr.frm OR lv-est-type EQ 2)
      no-lock,

      first item
      where item.company eq cocode
        and item.i-no    eq job-mat.i-no
        and index("BA",item.mat-type) gt 0
      no-lock
      break by job-mat.frm 
            by item.mat-type:
      
    IF item.mat-type NE "B"    OR
       FIRST-OF(item.mat-type) THEN
        v-tot-cost = v-tot-cost + (job-mat.cost-m * v-qty / 1000).
      
  end.
end.

