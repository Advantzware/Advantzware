
{oe/boll-po.i}

create report.

assign
 report.term-id = v-term
 report.rec-id  = recid(oe-boll)
 report.key-01  = oe-bolh.cust-no
 report.key-04  = string(oe-boll.ord-no,"9999999999")
 report.key-05  = string(oe-boll.rel-no,"9999999999")
 report.key-06  = string(oe-boll.b-ord-no,"9999999999")
 report.key-07  = v-po-no.
 
if avail cust and cust.inv-meth and v-po-no gt "" then
  assign
   report.key-02 = "1"
   report.key-03 = v-po-no.

else
if oe-ctrl.p-sep then
  assign
   report.key-02 = "2"
   report.key-03 = string(oe-bolh.b-no,"9999999999").

else
  assign
   report.key-02 = "3"
   report.key-03 = string(oe-bolh.bol-no,"9999999999").

