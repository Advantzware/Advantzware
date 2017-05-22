
{sys/inc/var.i shared}
def var qty as int no-undo.

def shared buffer xest    for est.
def shared buffer xef     for ef.
def shared buffer xeb     for eb.
def shared buffer xoe-ord for oe-ord.

{ce/print4a.i shared}

def var v-recid as recid.
def var v-qty as int.

def TEMP-TABLE work-eb NO-UNDO
   field part-no like oe-ordl.i-no
   field qty     like eb.yld-qty.

v-recid = recid(xest).

find xest where recid(xest) eq v-recid.

assign
 v-qty           = xest.est-qty[1]
 xest.est-qty[1] = 0.

for each oe-ordl
    where oe-ordl.company eq cocode
      and oe-ordl.ord-no  eq xoe-ord.ord-no
    no-lock,
    first eb
    where eb.company eq xest.company
      and eb.est-no  eq xest.est-no
      and eb.part-no eq oe-ordl.part-no:

  xest.est-qty[1] = xest.est-qty[1] + oe-ordl.qty.

  create work-eb.
  work-eb.part-no = oe-ordl.part-no.
  assign
   work-eb.qty = eb.bl-qty
   eb.bl-qty   = oe-ordl.qty.
end.

find first xef
    where xef.company eq xest.company
      and xef.est-no  eq xest.est-no
    no-lock.
find first xeb
    where xeb.company eq xest.company
      and xeb.est-no  eq xest.est-no
      and xeb.form-no eq 1
    no-lock.

qty = if xest.est-type eq 3 then xeb.bl-qty  else
      if xest.est-type eq 4 then xeb.yld-qty else
      if xest.est-type eq 8 then xeb.yld-qty else xest.est-qty[1].

if xest.est-type eq 3 then run ce/tan/print4.p.
else
if xest.est-type eq 4 then run ce/com/print4.p.
else                       run cec/com/print4.p.

for each oe-ordl
    where oe-ordl.company eq cocode
      and oe-ordl.ord-no  eq xoe-ord.ord-no,
    first eb
    where eb.company eq xest.company
      and eb.est-no  eq xest.est-no
      and eb.part-no eq oe-ordl.part-no,
    first work-eb
    where work-eb.part-no eq eb.part-no,
    first blk
    where blk.id eq eb.part-no
    no-lock:

  eb.bl-qty = work-eb.qty.

  assign
   oe-ordl.form-no  = eb.form-no
   oe-ordl.blank-no = eb.blank-no
   oe-ordl.cost     = blk.fact - (((blk.fg-wt / 100) * blk.fg-wt$) *
                                   (blk.qyld / xest.est-qty[1])).
  if xest.est-type eq 4 or xest.est-type eq 8 then
    oe-ordl.cost = blk.fact / ((if blk.yr$ then blk.qyld else blk.qreq) / 1000).

  find first job-hdr
      where job-hdr.company eq cocode
        and job-hdr.job-no  eq oe-ordl.job-no
        and job-hdr.job-no2 eq oe-ordl.job-no2
        and job-hdr.frm     eq oe-ordl.form-no
        and job-hdr.i-no    eq oe-ordl.i-no
      no-error.
  if avail job-hdr then
    assign
     job-hdr.frm      = oe-ordl.form-no
     job-hdr.blank-no = oe-ordl.blank-no.
end.

xest.est-qty[1] = v-qty.

find xest where recid(xest) eq v-recid no-lock.

