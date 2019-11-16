/* oe/ordlest.p */
def input parameter v-recid as recid.

{sys/inc/var.i shared}
def shared buffer xest for est.
def shared buffer xeb  for eb.

def var b-wt like item.basis-w.

{ce/msfcalc.i}
{sys/ref/oecount.i}

find oe-ordl where recid(oe-ordl) eq v-recid.
find xest where xest.company = xeb.company and
                xest.est-no eq xeb.est-no no-lock.
find first ef where ef.company = xest.company
                and ef.est-no eq xest.est-no
                and ef.form-no eq xeb.form-no
                no-lock no-error.
if not avail ef then
   find first ef where ef.company = xest.company and ef.est-no eq xest.est-no no-lock.

find first itemfg where itemfg.company eq cocode
                    and itemfg.i-no    eq xeb.stock-no
                    no-lock no-error.

do transaction:
  assign
   oe-ordl.est-no     = xeb.est-no
   oe-ordl.i-name     = xeb.part-dscr1
   oe-ordl.part-no    = xeb.part-no
   oe-ordl.part-dscr1 = xeb.part-dscr2
   oe-ordl.part-dscr2 = if avail itemfg then itemfg.part-dscr2 else ""
   oe-ordl.i-no       = xeb.stock-no
   oe-ordl.pr-uom     = "M"
   oe-ordl.cas-cnt    = if v-oecount then xeb.cas-cnt
                        else if xeb.tr-cnt ne 0 then xeb.tr-cnt
                        else (xeb.cas-cnt * xeb.cas-pal)  
   oe-ordl.qty = xeb.bl-qty
   oe-ordl.est-type   = xeb.est-type
   oe-ordl.job-no     = string(integer(xeb.est-no),">>>>>9").

  for each job-hdr
      where job-hdr.company eq cocode
        and job-hdr.job-no  eq oe-ordl.job-no
      use-index job-no no-lock
      by job-hdr.job-no desc by job-hdr.job-no2 desc:
    leave.
  end.
  oe-ordl.job-no2 = if avail job-hdr then job-hdr.job-no2 + 1 else 0.
end.

