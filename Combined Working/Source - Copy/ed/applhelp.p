{ed/sharedv.i}
if frame-field matches "*partner*" then do:
    run ed/mastpick.p.
    if lib_recid_ret = ? then leave.
    if ws_partner = ? then leave.
    frame-value = ws_partner.
end.
