/*  rc/runifok.p - conditional run based on search for source */
def input param p_fid as char no-undo.
def var r_fid as char no-undo.
def var fid as char no-undo.
fid = search(p_fid).
if fid = ? and r-index(p_fid,'.p') > 0 then do:
    assign
	r_fid = p_fid.
	substring(r_fid,length(r_fid),1) = 'r'.
    fid = search(r_fid).
end.
if fid <> ? then run value(p_fid).
