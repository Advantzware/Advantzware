/*
07.30.98 by CAH on \\ricky\robj8\ Log#0000:
1.  Added date and time to output. 
*/
def input param p_text as char no-undo.
output to rpro_dbg.txt append.
if p_text = "" then p_text = "Called by " + program-name(3).
put unformatted 
    today
    " " string(time,"HH:MM:SS")
    " " userid("dictdb") 
    " "
    program-name(2) /* caller */
    " : "
    p_text skip.
output close.
