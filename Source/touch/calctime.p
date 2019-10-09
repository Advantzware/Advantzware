/* touch/calctime.p  calculate total time */
def input parameter ip-total-time as int no-undo.
def output parameter op-time-string as cha no-undo.
def var ll as int no-undo.
def var dd as int no-undo.
def var tt as int no-undo.
def var mm as int no-undo.

ll = ip-total-time.
tt = truncate(ll / 3600,0).
dd = ll mod 3600.
mm = dd / 60.
op-time-string = string(tt,">99") + ":" + string(mm,"99")

/*message string(86400,"hh:MM")
          ll tt dd skip
          string(tt) + ":" + string(mm)
          view-as alert-box.
*/          
