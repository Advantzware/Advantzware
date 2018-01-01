if ws_shipvia = "" then true
else can-find(carrier where carrier.company = ws_company
                        and carrier.carrier = ws_shipvia)
