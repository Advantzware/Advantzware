if vendor = "" then true
else can-find(vend
where vend.company = ws_company
  and vend.vend-no = vendor)
