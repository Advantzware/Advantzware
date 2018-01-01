/* ed/login.p - initialize shared variables for EDI use
*/
{ed/sharedv.i "NEW GLOBAL"}
IF ws_userid <= " " THEN ws_userid = substring(userid("dictdb"),1,3).
ws_app = "ED".
if ws_company = "" or connected("rpro")
then do:
    ws_company = string(ws_co,"99").
end.
find first edco
where edco.company >= ws_company
and (if connected("rpro") then true else edco.system <> "rpro")
no-lock no-error.
if not avail edco
then find first edco no-lock no-error.
assign ws_company = if avail edco then edco.company else "1".
find edco where edco.company = ws_company no-lock no-error.
if avail edco then do:
    if edco.system <> "rpro"
    then assign printer_sel_proc = "ed"
        + dirsep + edco.system + dirsep + "getprint.p".
    else assign printer_sel_proc = "rc/getprint.p".
    if search(printer_sel_proc) = ?
    then printer_sel_proc = "rc/getprint.p".
    /* run a program which sets global shared variables based on back end */
    next_program = "ed" + dirsep + edco.system + dirsep + "setvars.p".
    if search(next_program) <> ? then run value(next_program).
    assign ws_856_from_invoice = edco.asn-on-inv.
end.
called = false.
