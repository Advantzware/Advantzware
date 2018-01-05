if sf-code = "" then true
else can-find(loc
where loc.company = ws_company
  and loc.loc = sf-code)
