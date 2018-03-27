/* ----------------------------------------------- oe/rep/ticket1.p 05/97 JLF */
/* factory ticket                                                             */
/* -------------------------------------------------------------------------- */

def input parameter v-recid  as recid.
def input parameter v-recid1 as recid.

{sys/inc/var.i shared}
{sys/form/s-top.f}

def shared var save_id as recid.
def shared var v-job-no like oe-ordl.job-no.
def shared var v-job-no2 like oe-ordl.job-no2.
def shared var v-up like eb.num-up.
def shared var v-tandem as log.


def var v-dept like job-mch.dept.

def shared workfile wrk-sheet
  field gsh-qty like ef.gsh-qty
  field cal like ef.cal
  FIELD i-no LIKE ef.board
  field brd-dscr like ef.brd-dscr
  field form-no like ef.form-no
  field sh-wid like ef.gsh-len
  field sh-len like ef.gsh-wid.

def shared workfile wrk-film
  field form-no like ef.form-no
  field snum as int format "99"
  field bnum as int format "99"
  field leaf as char format "x(10)"
  field leaf-l as dec format ">9.9999"
  field leaf-w as dec format ">9.9999".

def shared workfile wrk-spec
  field form-no like ef.form-no
  field spec-no as char format "x(10)"
  field dscr as char format "x(20)"
  field qty as dec format ">>>,>>9.9<<<<"
  field uom as char format "x(3)".

def shared workfile wrk-misc
  field form-no like ef.form-no
  field snum as int format "99"
  field bnum as int format "99"
  field cost as char format "x(20)".


find ef where recid(ef) eq v-recid no-lock.
find job-hdr where recid(job-hdr) eq v-recid1 no-lock.
find first est
    where est.company eq ef.company
      AND est.est-no  EQ ef.est-no
      no-lock.


/** BUILD SHEET **/
create wrk-sheet.
wrk-sheet.form-no = ef.form-no.
 
for each job-mat
    where job-mat.company eq cocode
      and job-mat.job     eq job-hdr.job
      and job-mat.frm     eq wrk-sheet.form-no
      no-lock,

    first item
    where item.company eq cocode
      and item.i-no    eq job-mat.i-no
      and index("BPR",item.mat-type) gt 0
    no-lock:
    
  assign
   wrk-sheet.cal      = item.cal
   wrk-sheet.i-no     = ITEM.i-no
   wrk-sheet.brd-dscr = item.est-dscr
   wrk-sheet.sh-wid   = job-mat.wid
   wrk-sheet.sh-len   = job-mat.len.
   
  leave.
end.

v-up = 0.
for each eb
    where eb.company eq ef.company
      AND eb.est-no  EQ ef.est-no
      and eb.form-no eq ef.form-no
    no-lock:
  v-up = v-up + eb.num-up.
  if avail est and est.est-type eq 3 then leave.
end.

if avail est and (est.est-type eq 2 or est.est-type eq 6) and
   est.form-qty eq 1                                      then v-up = v-up / 2.

assign
 save_id  = recid(wrk-sheet)
 v-tandem = avail est and est.est-type eq 3.

find first job
    where job.company eq cocode
      and job.job-no  eq v-job-no
      and job.job-no2 eq v-job-no2
    no-lock no-error.

if v-tandem then do:
  if avail job then 
  for each job-mch
      where job-mch.company eq cocode
        and job-mch.job     eq job.job
        and job-mch.frm     eq ef.form-no
      no-lock,

      first mach
      where mach.company    eq cocode
        and mach.m-code     eq job-mch.m-code
      no-lock

      by mach.d-seq
      by job-mch.pass:

    v-dept = job-mch.dept.
    leave.
  end.

  for each job-mch
      where job-mch.company eq cocode
        and job-mch.job     eq job.job
        and job-mch.frm     eq ef.form-no
        and job-mch.dept    eq v-dept
      use-index seq-idx no-lock,

      first mach
      where mach.company    eq cocode
        and mach.m-code     eq job-mch.m-code
      no-lock

      by mach.d-seq
      by job-mch.pass:

    wrk-sheet.gsh-qty = wrk-sheet.gsh-qty +
                     (job-mch.run-qty / if mach.p-type eq "B" then v-up else 1).

    if v-dept ne "PR" then leave.
  end.
end.

else
for each job-mch
    where job-mch.company eq cocode
      and job-mch.job     eq job.job
      and job-mch.frm     eq ef.form-no
    use-index seq-idx no-lock,

    first mach
    where mach.company    eq cocode
      and mach.m-code     eq job-mch.m-code
    no-lock

    by mach.d-seq
    by job-mch.pass:

  wrk-sheet.gsh-qty = wrk-sheet.gsh-qty +
                     (job-mch.run-qty / if mach.p-type eq "B" then v-up else 1).
  leave.
end.

/** BUILD FILM **/
for each job-mat
    where job-mat.company eq cocode
      and job-mat.job     eq job-hdr.job
      and job-mat.frm     eq job-hdr.frm
    no-lock,

    first item
    where item.company eq cocode
      and item.i-no    eq job-mat.i-no
      and index("WF",item.mat-type) gt 0
    no-lock:

  create wrk-film.
  assign
   wrk-film.form-no = job-hdr.frm
   wrk-film.snum    = job-hdr.frm
   wrk-film.bnum    = job-hdr.blank-no
   wrk-film.leaf    = job-mat.i-no.

  do i = 1 to 4:
    if ef.leaf[i] eq wrk-film.leaf then do:
      if ef.leaf-bnum[i] eq 0             or
         ef.leaf-bnum[i] eq wrk-film.bnum then leave.
    end.
    if i eq 4 then do:
      i = 0.
      leave.
    end.
  end.

  if i lt 1 or i gt 4 then
    assign
     wrk-film.leaf-l = item.s-len
     wrk-film.leaf-w = item.s-wid.
  else
    assign
     wrk-film.leaf-l = ef.leaf-l[i]
     wrk-film.leaf-w = ef.leaf-w[i].
end.

/** BUILD SPECIAL MATERIAL **/
do i = 1 to 8:
  if ef.spec-no[i] ne "" then do:
    create wrk-spec.
    assign
     wrk-spec.form-no = ef.form-no
     wrk-spec.spec-no = ef.spec-no[i]
     wrk-spec.dscr = ef.spec-dscr[i]
     /*wrk-spec.qty = ef.spec-qty[i]*/
     wrk-spec.uom = ef.spec-uom[i].

    RUN custom/extradec.p (.0001, ef.spec-qty[i],
                           OUTPUT wrk-spec.qty).
  end.
end.

/** BUILD MISC/SUBCONTRACT **/
do i = 1 to 6:
  if ef.mis-cost[i] ne "" then do:
    create wrk-misc.
    assign
     wrk-misc.form-no = ef.form-no
     wrk-misc.snum = ef.mis-snum[i]
     wrk-misc.bnum = ef.mis-bnum[i]
     wrk-misc.cost = ef.mis-cost[i].
  end.
end.

/* end ---------------------------------- copr. 1997  advanced software, inc. */

