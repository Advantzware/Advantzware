/* -------------------------------------------------- fg/cre-pchr.p 08/00 JLF */
/* Create a History Record for this bin/qty                                   */
/* -------------------------------------------------------------------------- */

def input parameter ip-rowid as rowid.
def input parameter ip-rcode like fg-rcpth.rita-code.
def input parameter ip-qty   as dec.
def input parameter ip-part  as dec.

{sys/inc/var.i shared}


IF ip-part EQ ? THEN ip-part = 0.

find fg-bin where rowid(fg-bin) eq ip-rowid no-lock.

find first itemfg
    where itemfg.company eq cocode
      and itemfg.i-no    eq fg-bin.i-no
    no-lock.
      
x = 0.
FOR EACH fg-rctd NO-LOCK BY fg-rctd.r-no DESC:
  x = fg-rctd.r-no.
  LEAVE.
END.

FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
IF AVAIL fg-rcpth AND fg-rcpth.r-no GT x THEN x = fg-rcpth.r-no.

create fg-rcpth.
assign
 fg-rcpth.r-no       = x + 1
 fg-rcpth.trans-date = today
 fg-rcpth.company    = cocode
 fg-rcpth.loc        = locode
 fg-rcpth.rita-code  = ip-rcode
 fg-rcpth.i-no       = fg-bin.i-no
 fg-rcpth.job-no     = fg-bin.job-no
 fg-rcpth.job-no2    = fg-bin.job-no2
 fg-rcpth.post-date  = today
 fg-rcpth.i-name     = itemfg.i-name
 fg-rcpth.pur-uom    = if fg-bin.pur-uom ne "" then fg-bin.pur-uom
                                               else itemfg.pur-uom.
create fg-rdtlh.
assign
 fg-rdtlh.r-no         = fg-rcpth.r-no
 fg-rdtlh.company      = fg-rcpth.company
 fg-rdtlh.loc          = fg-bin.loc
 fg-rdtlh.rita-code    = ip-rcode
 fg-rdtlh.loc-bin      = fg-bin.loc-bin
 fg-rdtlh.tag          = fg-bin.tag
 fg-rdtlh.qty          = ip-qty
 fg-rdtlh.qty-case     = fg-bin.case-count
 fg-rdtlh.stacks-unit  = fg-bin.cases-unit
 fg-rdtlh.units-pallet = fg-bin.units-pallet
 fg-rdtlh.cust-no      = fg-bin.cust-no
 fg-rdtlh.partial      = ip-part
 fg-rdtlh.cases        = TRUNC((ip-qty - ip-part) / fg-bin.case-count,0)
 fg-rdtlh.cost         = fg-bin.std-tot-cost
 fg-rdtlh.trans-time   = TIME.

IF fg-rdtlh.cases EQ ? THEN fg-rdtlh.cases = 0.

CREATE fg-rctd.
BUFFER-COPY fg-rcpth EXCEPT rec_key TO fg-rctd
ASSIGN
 fg-rctd.rct-date = fg-rcpth.trans-date
 fg-rctd.trans-time = fg-rdtlh.trans-time.

BUFFER-COPY fg-rdtlh EXCEPT rec_key TO fg-rctd
ASSIGN
 fg-rctd.rita-code  = "P"
 fg-rctd.post-date  = TODAY
 fg-rctd.t-qty      = fg-rdtlh.qty
 fg-rctd.ext-cost   = fg-rdtlh.cost *
                      (fg-rdtlh.qty / IF fg-rcpth.pur-uom EQ "M" THEN 1000 ELSE 1)
 fg-rctd.cases-unit = fg-rdtlh.stacks-unit
 fg-rctd.partial    = ip-part.
