/* sys/inc/print1.i */

if tmp-dir = "" then tmp-dir = "C:\tmp".
assign list-name = tmp-dir + "\tmp" + string(time)
       init-dir = tmp-dir.


