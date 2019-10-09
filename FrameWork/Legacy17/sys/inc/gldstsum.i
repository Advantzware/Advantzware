
release wkdistrib.

create wkdistrib.

assign
 wkdistrib.actnum  = {1}
 wkdistrib.tr-dscr = cust.name + " Inv# " + STRING(ar-inv.inv-no,"99999999") +
                     " " + "{4}"
 wkdistrib.amount  = wkdistrib.amount + {2}
 wkdistrib.debit   = {3}
 wkdistrib.recs    = wkdistrib.recs + 1.
