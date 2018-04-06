/*  rc/chkifok.p - see if source or object is available on propath */
def input param p_fid       as char no-undo.    /* proc to look for */
def output param p_ok       as logical no-undo. /* true if search <> ? */
def output param p_found    as char no-undo.    /* full file name */
def var r_fid as char no-undo.
def var fid as char no-undo.
fid = search(p_fid).
if fid = ? and r-index(p_fid,'.p') > 0 then do:
    assign
        r_fid = p_fid.
        substring(r_fid,length(r_fid),1) = 'r'.
    fid = search(r_fid).
end.
if fid <> ? then assign p_ok = true p_found= fid.
else assign p_ok = false p_found = ?.
