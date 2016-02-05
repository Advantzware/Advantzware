/* -------------------------------------------------- rm/mkjobmat.p 12/00 JLF */
/* Find/Create Job Material record for RM issued                              */
/* -------------------------------------------------------------------------- */

def input  parameter v-recid1 as recid.
def input param ip-cocode as cha.
def output parameter v-recid2 as recid. 
/*
{sys/inc/var.i shared}
{sys/form/s-top.f}
*/
def buffer b-item    for item.
def buffer b-job-mat for job-mat.

def var v-int as int.


/*find rm-rdtl where recid(rm-rdtl) eq v-recid1 no-lock.*/
find rm-rctd where recid(rm-rctd) = v-recid1 no-lock.
/*
find first rm-rcpt where rm-rcpt.r-no eq rm-rdtl.r-no no-lock.
*/
find first item
    where item.company eq ip-cocode
      and item.i-no    eq rm-rctd.i-no
    no-lock.  

find first job
    where job.company eq ip-cocode
      and job.job-no  eq fill(" ",6 - length(trim(rm-rctd.job-no))) +
                         trim(rm-rctd.job-no)
      and job.job-no2 eq rm-rctd.job-no2
    no-lock.
    
find first est where est.e-num eq job.e-num no-lock no-error.
     
v-recid2 = ?.
            
for each job-mat
    where job-mat.company  eq ip-cocode
      and job-mat.job      eq job.job
      and job-mat.job-no   eq job.job-no
      and job-mat.job-no2  eq job.job-no2
      and job-mat.frm      eq rm-rctd.s-num
      and job-mat.i-no     eq rm-rctd.i-no
    no-lock
                
    by job-mat.blank-no desc:
                
  v-recid2 = recid(job-mat).
  
  if job-mat.blank-no eq rm-rctd.b-num then leave.
end.
find job-mat where recid(job-mat) eq v-recid2 no-error.
            
if not avail job-mat then do:
  if INDEX("MOXY789@",ITEM.mat-type) EQ 0 then
  for each b-job-mat
      where b-job-mat.company  eq ip-cocode
        and b-job-mat.job      eq job.job
        and b-job-mat.job-no   eq job.job-no
        and b-job-mat.job-no2  eq job.job-no2
        and b-job-mat.frm      eq rm-rctd.s-num
      no-lock,
                  
      first b-item
      where b-item.company  eq ip-cocode
        and b-item.i-no     eq b-job-mat.i-no
        and b-item.mat-type eq item.mat-type
      no-lock
              
      by b-job-mat.blank-no desc:

    v-recid2 = recid(b-job-mat).
  
    if b-job-mat.blank-no eq rm-rctd.b-num then leave.
  end.
  find b-job-mat where recid(b-job-mat) eq v-recid2 no-error.
              
  if avail b-job-mat then do:
    create job-mat.
    
    buffer-copy b-job-mat to job-mat
      assign
       job-mat.i-no    = rm-rctd.i-no
       job-mat.rm-i-no = rm-rctd.i-no
       job-mat.qty     = 0
       job-mat.basis-w = item.basis-w.
   
    if item.i-code eq "R" then
      if b-item.r-wid ne 0 then job-mat.wid = item.r-wid.
                  
      else
        assign
         job-mat.len = item.s-len
         job-mat.wid = item.s-wid.
     
    v-recid2 = recid(job-mat).   
  end.
  
  else
  if INDEX("MOXY789",ITEM.mat-type) GT 0 then do:
    v-int = 0.
    
    for each job-mat
        where job-mat.company eq ip-cocode
          and job-mat.job     eq job.job
          and job-mat.job-no  eq job.job-no
          and job-mat.job-no2 eq job.job-no2
        no-lock
        by job-mat.line desc:
        
      v-int = job-mat.line.
      leave.
    end.
    
    create job-mat.
    assign
     job-mat.company  = ip-cocode
     job-mat.job      = job.job
     job-mat.job-no   = job.job-no
     job-mat.job-no2  = job.job-no2
     job-mat.line     = v-int + 1
     job-mat.j-no     = 1.
     
    v-int = 0.
     
    for each job-hdr
        where job-hdr.company eq ip-cocode
          and job-hdr.job     eq job.job
          and job-hdr.job-no  eq job.job-no
          and job-hdr.job-no2 eq job.job-no2
          and job-hdr.frm     eq rm-rctd.s-num
        no-lock:
        
      v-int = v-int + job-hdr.qty.
    end.
     
    assign 
     job-mat.cost-m   = (rm-rctd.cost * rm-rctd.qty) / (v-int / 1000)
     job-mat.frm      = rm-rctd.s-num
     job-mat.blank-no = if avail est                             and
                        (est.est-type eq 1 or est.est-type eq 5) then 1 else 0
     job-mat.i-no     = rm-rctd.i-no
     job-mat.rm-i-no  = rm-rctd.i-no
     job-mat.qty      = rm-rctd.qty
     job-mat.qty-uom  = rm-rctd.pur-uom
     job-mat.sc-uom   = rm-rctd.pur-uom
     job-mat.std-cost = rm-rctd.cost
     job-mat.post     = no
     v-recid2         = recid(job-mat). 
  end.
end.  
            
