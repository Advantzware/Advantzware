if item-no = "" then true
else can-find(itemfg
where itemfg.company = ws_company
  and itemfg.i-no = item-no)
