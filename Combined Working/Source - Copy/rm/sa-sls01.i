
def buffer xreport for report.

def var v-term   like report.term-id format "x(20)".
def var v-term-2 like v-term.

assign
 v-term   = string(year(today),"9999") +
	    string(month(today),"99")  +
	    string(day(today),"99")

 v-term   = v-term + string(time,"99999")

 v-term-2 = string(year(today - 2),"9999") +
	    string(month(today - 2),"99")  +
	    string(day(today - 2),"99").

for each report where report.term-id lt v-term-2 transaction:
  if avail report then delete report.
end.

