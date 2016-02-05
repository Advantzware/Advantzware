def var ld-date as DATE NO-UNDO.

{sa/sa-sls01.i}

{oe/rel-stat.i lv-stat}
  
ld-date = if avail oe-relh then oe-relh.rel-date else oe-rel.rel-date.
  
assign
 report.term-id = v-term
 report.rec-id  = recid(oe-rel)
 report.key-01  = string(year(ld-date),"9999") +
                  string(month(ld-date),"99")  +
                  string(day(ld-date),"99")
 report.key-02  = STRING(ld-date,"99/99/9999").
 
if oeinq then 
  report.key-01 = string(9999999999 - int(report.key-01),"9999999999").
  
