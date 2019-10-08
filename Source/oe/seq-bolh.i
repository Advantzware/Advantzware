
/*{oe/boll-po.i}*/

CREATE report.

ASSIGN
 report.term-id = v-term
 report.rec-id  = RECID(oe-boll)
 report.key-01  = oe-bolh.cust-no
 report.key-04  = STRING(oe-boll.ord-no,"9999999999")
 report.key-05  = STRING(oe-boll.rel-no,"9999999999")
 report.key-06  = STRING(oe-boll.b-ord-no,"9999999999")
 report.key-07  = oe-boll.po-no.
 
IF AVAIL cust AND cust.inv-meth EQ YES AND TRIM(oe-boll.po-no) GT "" THEN
  ASSIGN
   report.key-02 = "1"
   report.key-03 = oe-boll.po-no.

ELSE
IF oe-ctrl.p-sep THEN
  ASSIGN
   report.key-02 = "2"
   report.key-03 = STRING(oe-bolh.b-no,"9999999999").

ELSE
  ASSIGN
   report.key-02 = "3"
   report.key-03 = STRING(oe-bolh.bol-no,"9999999999").

