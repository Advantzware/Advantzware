
def buffer xreport for report.

def var v-term   like report.term-id format "x(20)".
def var v-term-2 like v-term.

assign
 v-term   = string(year(today),"9999")      +
            string(month(today),"99")       +
            string(day(today),"99")         +
            string(time,"99999")            +
            string(program-name(1),"x(40)") +
            string(userid("nosweat"),"x(40)")

 v-term-2 = string(year(today - 2),"9999")  +
            string(month(today - 2),"99")   +
            string(day(today - 2),"99").

for each report where report.term-id lt v-term-2 no-lock TRANSACTION:
  find first xreport where recid(xreport) eq recid(report)
      exclusive-lock no-wait no-error.
  if avail xreport then delete xreport.
end.
