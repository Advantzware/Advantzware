if cust > '' and ship-to > ''
then can-find(first asi.shipto
where shipto.company  = ws_company
  and shipto.cust-no  = cust
  and shipto.ship-id  = ship-to) else true
