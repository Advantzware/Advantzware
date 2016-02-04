/* cust.i */

&IF '{&CUSTOMER-TOTALS}' = '' &THEN
IF adm-new-record and adm-adding-record THEN
DO:
  CREATE shipto.
  ASSIGN
    shipto.company = cust.company
    shipto.cust-no = cust.cust-no
    shipto.ship-addr[1] = cust.addr[1]
    shipto.ship-addr[2] = cust.addr[2]
    shipto.ship-city = cust.city
    shipto.ship-id = cust.cust-no
    shipto.ship-name = cust.name
    shipto.ship-no = 1
    shipto.ship-state = cust.state
    shipto.ship-zip = cust.zip
    shipto.carrier = cust.carrier
    shipto.dest-code = cust.del-zone
    shipto.loc = cust.loc
    shipto.tax-code = cust.tax-gr.
  CREATE soldto.
  ASSIGN
    soldto.company = cust.company
    soldto.cust-no = cust.cust-no
    soldto.sold-addr[1] = cust.addr[1]
    soldto.sold-addr[2] = cust.addr[2]
    soldto.sold-city = cust.city
    soldto.sold-id = cust.cust-no
    soldto.sold-name = cust.name
    soldto.sold-no = 1
    soldto.sold-state = cust.state
    soldto.sold-zip = cust.zip.
END.
DISABLE {&faxFields} WITH FRAME {&FRAME-NAME}.
cust.fax = faxAreaCode + faxNumber.
&ELSE
DISABLE ptd-sales total-msf WITH FRAME {&FRAME-NAME}.
ASSIGN
  cust.sales[gperiod] = ptd-sales
  cust.ptd-msf[gperiod] = total-msf.
&ENDIF
