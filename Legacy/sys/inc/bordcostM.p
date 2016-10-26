/* --------------------------------------------- sys/inc/bordcost.p 01/02 JLF */
/* Calculate the total cost of the board received for a job/item              */
/* -------------------------------------------------------------------------- */

def input  parameter v-job-no   like job.job-no.
def input  parameter v-job-no2  like job.job-no2.
def input  parameter v-i-no     like itemfg.i-no.
DEF INPUT PARAMETER  v-bol-no   LIKE oe-boll.bol-no.
def input  parameter v-qty      as   int.
def input  parameter v-act-cost as   log.
def output parameter v-tot-cost as   DECIMAL.

{sys/inc/var.i shared}
{sys/form/s-top.f}

def var v-fgrec-qty               LIKE job-hdr.qty.
def var v-cost                  as   dec.
DEF VAR lv-est-type LIKE est.est-type.
DEFINE VAR v-po-cost AS DECIMAL NO-UNDO .
DEFINE VAR v-issued-qty AS INTEGER NO-UNDO .
DEFINE VAR v-rec-qty AS INTEGER NO-UNDO .
DEFINE VAR v-sub-tot AS DECIMAL NO-UNDO .
DEFINE VAR v-sub-qty AS DECIMAL NO-UNDO .
DEFINE VARIABLE v-req-qty AS INT NO-UNDO .


/* Find BOL for item. */
FOR EACH oe-boll NO-LOCK WHERE
    oe-boll.company = cocode AND
    oe-boll.bol-no = v-bol-no AND
    oe-boll.i-no = v-i-no :

    ASSIGN v-job-no = oe-boll.job-no
           v-job-no2 = oe-boll.job-no2
           v-fgrec-qty = 0
           v-sub-tot   = 0 
           v-req-qty   = 0
           v-sub-qty   = 0.


    find first job no-lock
        where job.company eq cocode
        and job.job-no  eq v-job-no
        and job.job-no2 eq v-job-no2
        no-error.
    
    if avail job then
        find first job-hdr no-lock
        where job-hdr.company eq cocode
        and job-hdr.job     eq job.job
        and job-hdr.job-no  eq job.job-no
        and job-hdr.job-no2 eq job.job-no2
        and job-hdr.i-no    eq v-i-no
        no-error.


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

      v-fgrec-qty = v-fgrec-qty + fg-rdtlh.qty.
      end.

     
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
         NO-LOCK BREAK BY job-mat.i-no:

          FIND FIRST job-mch NO-LOCK WHERE  job-mch.company = job.company 
              AND job-mch.job = job.job 
              AND job-mch.job-no = job.job-no 
              AND job-mch.job-no2 = job.job-no2 
              use-index line-idx NO-ERROR  .

          IF AVAIL job-mch  THEN
              v-req-qty = job-mch.run-qty .
          
          for each po-ordl no-lock
              where po-ordl.company   eq cocode
              and po-ordl.i-no      eq job-mat.i-no
              and po-ordl.job-no    eq v-job-no
              and po-ordl.job-no2   eq v-job-no2
              use-index item 
              by po-ordl.po-no desc:
              leave.
          end.
        
          IF AVAILABLE po-ordl THEN do:
              ASSIGN
                  v-po-cost =  po-ordl.cons-cost * 1000  /* po-ordl.t-cost */
                  v-issued-qty = po-ordl.t-rec-qty.
             
              v-sub-tot = v-sub-tot + (v-issued-qty / 1000 * v-po-cost ) .
              v-sub-qty = ( v-sub-tot / ( MIN( v-issued-qty / v-req-qty , 1) * job-hdr.qty ) * v-fgrec-qty ) .
              v-tot-cost = v-tot-cost + ( v-sub-qty / 1000 * oe-boll.qty ).
          END.
          ELSE DO:
               IF FIRST-OF(job-mat.i-no) THEN
                   ASSIGN v-tot-cost = v-tot-cost + oe-boll.qty / 1000 * job-mat.cost-m .
          END.
    END.  /* job-mat */
  END. /* avail job-hdr */
END.  /* for each oe-boll */



