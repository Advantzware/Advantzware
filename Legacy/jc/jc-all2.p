/* jc-all2.p  copied from jc-all.p but no conversion.
                         conversion is done from windows jc/b-jobmat.w */
                         
def input param ip-rowid as rowid no-undo.
def input param ip-factor as int no-undo.

{sys/inc/var.i shared}

def buffer b-job-mat for job-mat.
def buffer b-item    for item.

def var v-comm      as   dec                                            no-undo.
def var v-bwt       like item.basis-w                                   no-undo.
def var v-len       like item.s-len                                     no-undo.
def var v-wid       like item.s-wid                                     no-undo.

DEF VAR li AS INT NO-UNDO.


for each job-mat
    where rowid(job-mat)  eq ip-rowid
      and job-mat.qty-all gt 0
      and job-mat.all-flg,

    first b-item
    where b-item.company eq job-mat.company
      and b-item.i-no    eq job-mat.rm-i-no
      and b-item.i-code  eq "R"
    no-lock,

    first job
    where job.company eq job-mat.company
      and job.job     eq job-mat.job
      and job.job-no  eq job-mat.job-no
      and job.job-no2 eq job-mat.job-no2:

  li = 0.
  DO WHILE li LT 1000:
    li = li + 1.
    FIND item WHERE ROWID(item) EQ ROWID(b-item) EXCLUSIVE NO-WAIT NO-ERROR.
    IF AVAIL item THEN LEAVE.
  END.
  IF NOT AVAIL item THEN NEXT.

  if job-mat.qty-uom eq item.cons-uom then
    v-comm = job-mat.qty-all.

  else do:
    assign
     v-bwt = job-mat.basis-w
     v-len = job-mat.len
     v-wid = job-mat.wid.

    if v-len eq 0 then v-len = item.s-len.

    if v-wid eq 0 then
      v-wid = if item.r-wid ne 0 then item.r-wid else item.s-wid.

    if v-bwt eq 0 then v-bwt = item.basis-w.
    run sys/ref/convquom.p(job-mat.qty-uom, item.cons-uom,
                           v-bwt, v-len, v-wid, item.s-dep,
                           job-mat.qty-all, output v-comm).
  end.
 
  assign
   item.q-comm     = item.q-comm + (v-comm * ip-factor)
   item.q-comm     = if item.q-comm lt 0 then 0 else item.q-comm
   item.q-avail    = item.q-onh + item.q-ono - item.q-comm.

  if ip-factor eq 1 and job.stat eq "W" then job.stat = "A".
  else
  if ip-factor eq -1 then do:
    assign
     job-mat.qty-all = 0
     job-mat.all-flg = no.

    find first b-job-mat
        where b-job-mat.company eq job.company
          and b-job-mat.job     eq job.job
          and b-job-mat.job-no  eq job.job-no
          and b-job-mat.job-no2 eq job.job-no2
          and rowid(b-job-mat)  ne rowid(job-mat)
          and b-job-mat.all-flg
        no-lock no-error.
    if not avail b-job-mat and job.stat eq "A" then job.stat = "L".
  end.

  FIND CURRENT item NO-LOCK NO-ERROR.
end.

