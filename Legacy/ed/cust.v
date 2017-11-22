if cust = "" then true
else can-find(cust
where cust.company = ws_company
  and cust.cust-no = cust)
