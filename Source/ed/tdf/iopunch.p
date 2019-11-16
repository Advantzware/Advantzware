def input param p_in as char no-undo.
def output param p_out as char no-undo.
def var n as int        no-undo.
def var c as char       no-undo.
p_out = p_in.
c = substring(p_in,length(p_in),1).
if  asc(c) >= asc("p")
and asc(c) <= asc("y")
then assign
    n = asc(c) - asc("p")
    substring(p_out,1,1) = "-"
    substring(p_out,length(p_in),1) = string(n,"9").
    
    
