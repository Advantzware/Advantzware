def var i as int.
{ed/sharedv.i}
find cust where cust.company = ws_company
and cust.cust-no = "1".
for each shipto of cust:
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
   soldto.sold-id   = string(i, ">>>>>>>9").
/* end ---------------------------------- copr. 1992  advanced software, inc. */
ASSIGN
  soldto.company      = shipto.company
  soldto.cust-no      = shipto.cust-no
  soldto.sold-name    = shipto.ship-name
  soldto.sold-addr[1] = shipto.ship-addr[1]
  soldto.sold-addr[2] = shipto.ship-addr[2]
  soldto.sold-city    = shipto.ship-city
  soldto.sold-state   = shipto.ship-state
  soldto.sold-zip     = shipto.ship-zip
  soldto.Postal       = shipto.Postal
  soldto.country      = shipto.country
  soldto.sold-id      = shipto.ship-id
  .
end.
