if carrier = "" then true
else can-find(asi.carrier where carrier.company = ws_company
                          and carrier.carrier = carrier)
