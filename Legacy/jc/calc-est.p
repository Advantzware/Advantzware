
def input parameter v-recid as recid no-undo.

{sys/inc/var.i shared}
{sys/form/s-top.f}

def shared buffer xest for est.
def shared buffer xef for ef.
def shared buffer xeb for eb.

{ce/print4.i "shared" "shared"}
{ce/print42.i "shared"}

def var v-est-qty as dec no-undo.
def shared var qty as int NO-UNDO.
DEF SHARED VAR v-shared-rel AS INT NO-UNDO.
DEF NEW SHARED VAR gEstSummaryOnly AS LOG NO-UNDO.
DEFINE NEW SHARED VARIABLE v-prep-mat LIKE tprep-mat NO-UNDO.  /* for probemk cost */
DEFINE NEW SHARED VARIABLE v-prep-lab LIKE tprep-lab NO-UNDO.

def TEMP-TABLE work-eb NO-UNDO
   field form-no  like job-hdr.frm
   field cust-seq like xeb.cust-seq
   field blank-no like job-hdr.blank-no
   field bl-qty   like xeb.bl-qty
   field yld-qty  like xeb.yld-qty.

def var chcs as char extent 8 init ["ce/print4.p",
                                    "ce/box/print42.p",
                                    "ce/tan/print4.p",
                                    "ce/com/print4.p",
                                    "cec/print4.p",
                                    "cec/box/print42.p",
                                    "cec/tan/print4.p",
                                    "cec/com/print4.p"] no-undo.
                                    
                                    
for first job where recid(job) eq v-recid no-lock: 
  find first xest
      where xest.company eq job.company
        and xest.est-no  eq job.est-no.
 
  assign
   v-est-qty       = xest.est-qty[1]
   xest.est-qty[1] = 0.

  for each job-hdr
      where job-hdr.company eq cocode
        and job-hdr.job     eq job.job
        and job-hdr.job-no  eq job.job-no
        and job-hdr.job-no2 eq job.job-no2
      break by job-hdr.frm:
      
    if xest.est-type eq 3 or
       xest.est-type eq 4 or
       xest.est-type eq 7 or
       xest.est-type eq 8 then do:
      find first xeb
          where xeb.company   eq job.company
            and xeb.est-no    eq job.est-no
            and xeb.form-no   eq job-hdr.frm
            and (xeb.blank-no eq job-hdr.blank-no or
                 xeb.stock-no eq job-hdr.i-no)
          no-error.
          
      if avail xeb then do:
        create work-eb.
        assign
         work-eb.form-no  = xeb.form-no
         work-eb.cust-seq = xeb.cust-seq
         work-eb.blank-no = xeb.blank-no
         work-eb.bl-qty   = xeb.bl-qty
         work-eb.yld-qty  = xeb.yld-qty
         xeb.yld-qty      = job-hdr.qty
         xeb.bl-qty       = job-hdr.qty.

        IF work-eb.yld-qty GT work-eb.bl-qty THEN DO:
          xeb.yld-qty = xeb.yld-qty * (work-eb.yld-qty / work-eb.bl-qty).
          {sys/inc/roundup.i xeb.yld-qty}
        END.
      end.
      xest.est-qty[1] = xest.est-qty[1] + job-hdr.qty.
    end.
    else xest.est-qty[1] = job-hdr.qty.
  end.
  
  if xest.est-qty[1] eq 0 then xest.est-qty[1] = v-est-qty.

  find first xef
      where xef.company eq job.company
        and xef.est-no  eq job.est-no
      no-lock.
  xcal = xef.cal.

  find first xeb
      where xeb.company eq job.company
        and xeb.est-no  eq job.est-no
        and xeb.form-no eq 1
      no-lock.

  qty = if xest.est-type eq 3 then xeb.bl-qty  else
        if xest.est-type eq 4 then xeb.yld-qty else
        if xest.est-type eq 8 then xeb.yld-qty else xest.est-qty[1].
  
  ASSIGN  /*Initialize Shared Variables for CostHeader calc*/
    cJobNo = job.job-no
    iJobNo2 = job.job-no2
    riJob = ROWID(job)
    .
  RUN est\CostResetHeaders.p(ROWID(xest),riJob).
  run value(chcs[xest.est-type]).
  RUN est\CostExportHeaders.p(job.company,TRIM(xest.est-no) + "JC").
  
  find first xest
      where xest.company eq job.company
        and xest.est-no  eq job.est-no.
  
  for each work-eb:
    find first xeb
        where xeb.company  eq job.company
          and xeb.est-no   eq job.est-no
          and xeb.form-no  eq work-eb.form-no
          and xeb.cust-seq eq work-eb.cust-seq
          and xeb.blank-no eq work-eb.blank-no
        no-error.
    if avail xeb then
      assign
       xeb.bl-qty  = work-eb.bl-qty
       xeb.yld-qty = work-eb.yld-qty.
  end.
  
  xest.est-qty[1] = v-est-qty.
end.

