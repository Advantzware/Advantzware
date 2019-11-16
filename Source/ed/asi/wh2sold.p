def var i as int.
{ed/sharedv.i}
{rc/viewline.i}
{ed/getpart.i}
update ws_company with frame f-view.
for each edshipto
where edshipto.partner = ws_partner
and edshipto.ref-type = "ST":
find cust where cust.company = ws_company
and cust.cust-no = edmast.cust.
edshipto.ship-to = edshipto.by-code.
edshipto.st-code = edshipto.by-code.
find soldto where soldto.company = cust.company
and soldto.cust-no = cust.cust-no
and soldto.sold-id = edshipto.ship-to
no-error.
if not avail soldto then do:
/* ------------------------------------------------ sys/ref/soldto.a 2/92 cd  */
/*                                                                            */
/*                                                                            */
/*                                                                            */
/* -------------------------------------------------------------------------- */
find last soldto where soldto.company = cust.company and
                       soldto.cust-no = cust.cust-no
                      use-index sold-no no-lock no-error.
if avail soldto then assign i = soldto.sold-no + 1.
else assign i = 1.
create soldto.
assign
   soldto.company   = cust.company
   soldto.cust-no   = cust.cust-no
   soldto.sold-name = cust.name
   soldto.sold-no   = i
   soldto.sold-id   = edshipto.ship-to.
/* end ---------------------------------- copr. 1992  advanced software, inc. */
end.
ASSIGN
  soldto.sold-name    = edshipto.name
  soldto.sold-addr[1] = edshipto.addr1
  soldto.sold-addr[2] = edshipto.addr2
  soldto.sold-city    = edshipto.city
  soldto.sold-state   = edshipto.state
  soldto.sold-zip     = edshipto.zip
  soldto.country      = edshipto.country
  .
end.
