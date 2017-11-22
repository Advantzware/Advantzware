do with frame f-view:
form
    ws_partner  help "Enter partner ID, press f5 to lookup" 
    "" to 80
with frame f-view side-labels color value(c_wrn) row 2 no-box overlay.

update ws_partner
editing:
    readkey.
    if keylabel(lastkey) = "f5" then do:
        run ed/mastpick.p.
        if lib_recid_ret <> ? 
        then display ws_partner with frame f-view.
        next.
    end.    
    apply lastkey.
end.    
ws_partner = caps(ws_partner).

find edmast where edmast.partner = ws_partner no-lock no-error.
if not avail edmast then do:
    bell.
    message color value(c_err) "Invalid partner code, setup first".
    undo, retry.
end.

assign ws_partner = edmast.partner.

display ws_partner with frame f-view.

end.
