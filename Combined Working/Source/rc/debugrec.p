def input param p_text as char no-undo.
def input param p_rec as recid no-undo.
/* We're not using run-time compile 
find {1} where recid({1}) = p_rec no-lock no-error.
run rc/debugmsg.p (p_text + (if avail {1} then "" else "(Unavailable)" ) ).
if avail ({1}) then do:
output to rpro_dbg.txt append.
display {1} 
with frame f-{1} 1 column title "{1} Record".
output close.
end.
*/
