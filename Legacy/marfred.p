/* marfred.p */

def var okpressed as log no-undo.
def var file-name as char format 'x(40)' label 'File Name' no-undo.
def var init-dir as char no-undo.

def temp-table arfold no-undo
  field cust# as char
  field custtype as char
  field cust-typedesc as char
  field billto-name as char
  field billto-address1 as char
  field billto-city as char
  field billto-state as char
  field billto-zip as char
  field shipto-address1 as char
  field shipto-city as char
  field shipto-state as char
  field shipto-zip as char
  field resaleyn as char
  field billto-tel as char
  field dateacctopened as char
  field regioncode as char
  field termcode as char
  field termcodedesc as char
  field statement as char
  field servicecharge as char
  field creditlimit as char
  field lastpaydate as char
  field lastckamt as char
  field openaramt as char
  field lastsaledate as char
  field salesmancode as char
  field salesmanname as char
  field billto-address2 as char
  field billto-address3 as char
  field billto-contact as char
  field shipto-companyname as char
  field shipto-address2 as char
  field shipto-address3 as char
  field shipto-telephone as char
  field shipto-contact as char
  field salestaxcode as char
  field salestaxdesc as char
  field shipcode as char
  field shipcodedesc as char
  field backorders as char
  field openordersamt as char
  field ytdshippedamt as char
  field mtdshippedamt as char
  field lastinvoiceddate as char
  field specialaging as char
  field performdays as char
  field mapcoordinates as char
  field lclcode as char
  field billto-fax as char
  field lastyrytd-ship as char
  field onhold as char
  field redistribution as char
  field mcvisanumber as char
  field mcvisaexpdate as char
  field billto-country as char
  field shipto-country as char
  field emailaddress as char
    index arfold is primary unique
          cust#.

def temp-table stfold no-undo
  field customer# as char
  field shiptonumber as char
  field customername as char
  field address1 as char
  field address2 as char
  field address3 as char
  field city as char
  field zip as char
  field contact as char
  field state as char
  field telephone as char
  field mapcoordinates as char
  field shipcode as char
  field shipcodedesc as char
  field salesmancode as char
  field salesmanname as char
  field regioncode as char
  field regiondesc as char
  field salestaxcode as char
  field salestaxdesc as char
  field enduser as char
  field nation as char
    index stfold is primary unique
          customer#
          shiptonumber.

do while true:
  &if '{&window-system}' ne 'tty' &then
  system-dialog get-file file-name
    title 'Choose Text File to Process ...'
    filters 'Text File (*.txt)' '*.txt',
            'All Files (*.*)' '*.*'
    must-exist
    update okpressed.
  if not okpressed then
  leave.
  &else
  update file-name help 'Enter File Name to Process'
      with centered side-labels row 10.
  if search(file-name) = ? then
  do:
    message 'File' file-name 'does not exist, please re-enter File Name'
        view-as alert-box error.
    next.
  end.
  &endif
  okpressed = if index(file-name,'arfold') ne 0 then yes
         else if index(file-name,'stfold') ne 0 then no
         else ?.
  message 'Process File' file-name 'As:' skip(1)
      '"YES" = Accounts Receivable type' skip
      '"NO" = Ship To type' skip
    view-as alert-box question buttons yes-no-cancel update okpressed.
  if okpressed = ? then
  leave.
  input from value(file-name) no-echo.
  import ^.
  if okpressed then
  run process-arfold.
  else
  run process-stfold.
  message 'Processing of File' file-name 'Complete.' view-as alert-box.
end. /* do while true */

procedure process-stfold:
  repeat transaction:
    create stfold.
    import delimiter '~t' stfold.
  end. /* repeat trans */
  find first stfold exclusive.
  if stfold.customer# = '' then
  delete stfold.
  for each stfold no-lock:
    if not can-find(cust where cust.company = '001'
                and cust.cust-no = stfold.customer#) then
    next.
    /*
    if not can-find(sman where sman.company = '001'
                and sman.sman = stfold.salesmancode) then
    do:
      create sman.
      assign
        sman.company = '001'
        sman.sman-no = int(stfold.salesmancode)
        sman.sman = stfold.salesmancode
        sman.sname = stfold.salesmanname
        sman.commbasis = 'S'.
    end.
    */
    if not can-find(stax where stax.tax-group = stfold.salestaxcode) then
    do:
      create stax.
      assign
        stax.company = ''
        stax.tax-group = stfold.salestaxcode
        stax.tax-code[1] = stfold.salestaxcode
        stax.tax-dscr[1] = stfold.salestaxdesc.
    end.
    if not can-find(terr where terr.company = '001'
                and terr.terr = stfold.regioncode) then
    do:
      create terr.
      assign
        terr.company = '001'
        terr.terr = stfold.regioncode
        terr.dscr = stfold.regiondesc.
    end.
    if can-find(shipto where shipto.company = '001'
            and shipto.cust-no = stfold.customer#
            and shipto.ship-no = int(stfold.shiptonumber) + 1) then
    next.
    create shipto.
    assign
      shipto.company = '001'
      shipto.cust-no = stfold.customer#
      shipto.ship-no = int(stfold.shiptonumber) + 1
      shipto.ship-id = if shipto.ship-no = 1 then stfold.customer#
                       else stfold.shiptonumber
      shipto.ship-name = stfold.customername
      shipto.ship-addr[1] = stfold.address1
      shipto.ship-addr[2] = stfold.address2
      shipto.ship-city = stfold.city
      shipto.ship-zip = stfold.zip
      shipto.ship-state = stfold.state
      /*
      shipto. = stfold.shipcode
      shipto. = stfold.shipcodedesc
      */
      shipto.tax-code = stfold.salestaxcode
      shipto.country = stfold.nation.
  end. /* for each stfold */
end procedure.

procedure process-arfold:
  repeat transaction:
    create arfold.
    import delimiter '~t' arfold.
  end. /* repeat trans */
  find first arfold exclusive.
  if arfold.cust# = '' then
  delete arfold.
  for each arfold no-lock:
    if not can-find(custype where custype.company = '001'
                and custype.custype = trim(arfold.custtype)) then
    do:
      create custype.
      assign
        custype.company = '001'
        custype.custype = trim(arfold.custtype)
        custype.dscr = trim(arfold.cust-typedesc).
    end.
    if not can-find(terms where custype.company = '001'
                and terms.dscr = trim(arfold.termcodedesc)
                and terms.t-code = trim(arfold.termcode)) then
    do:
      create terms.
      assign
        terms.company = '001'
        terms.t-code = trim(arfold.termcode)
        terms.dscr = trim(arfold.termcodedesc).
    end.
    /*
    if not can-find(sman where sman.company = '001'
                and sman.sman = arfold.salesmancode) then
    do:
      create sman.
      assign
        sman.company = '001'
        sman.sman-no = int(arfold.salesmancode)
        sman.sman = arfold.salesmancode
        sman.sname = arfold.salesmanname
        sman.commbasis = 'S'.
    end.
    */
    if not can-find(stax where stax.tax-group = trim(arfold.salestaxcode)) then
    do:
      create stax.
      assign
        stax.company = ''
        stax.tax-group = trim(arfold.salestaxcode)
        stax.tax-code[1] = trim(arfold.salestaxcode)
        stax.tax-dscr[1] = trim(arfold.salestaxdesc).
    end.
    if can-find(cust where cust.company = '001'
            and cust.cust-no = arfold.cust#) then
    next.
    create cust.
    assign
      cust.company = '001'
      cust.cust-no = arfold.cust#
      cust.type = arfold.custtype
      cust.name = arfold.billto-name
      cust.addr[1] = arfold.billto-address1
      cust.city = arfold.billto-city
      cust.state = arfold.billto-state
      cust.zip = arfold.billto-zip
      cust.area-code = substr(arfold.billto-tel,1,3)
      cust.phone = replace(substr(arfold.billto-tel,4),'-','')
      cust.terr = arfold.regioncode
      cust.terms = arfold.termcode
      cust.cr-lim = dec(arfold.creditlimit)
      cust.lpay-date = date(arfold.lastpaydate)
      cust.lpay = dec(arfold.lastckamt)
      cust.acc-bal = dec(arfold.openaramt)
      cust.sman = arfold.salesmancode
      cust.addr[2] = arfold.billto-address2
      cust.contact = arfold.billto-contact
      cust.ord-bal = dec(arfold.openordersamt)
      cust.ytd-msf = dec(arfold.ytdshippedamt)
      cust.ptd-msf = dec(arfold.mtdshippedamt)
      cust.fax = replace(arfold.billto-fax,'-','')
      cust.lyr-sales = dec(arfold.lastyrytd-ship)
      cust.cr-hold = if arfold.onhold = 'oh' then yes else no
      cust.country = arfold.billto-country
      cust.email = arfold.emailaddress
      cust.fob-code = 'DEST'
      cust.loc = 'MAIN'
      cust.over-pct = 10
      cust.under-pct = 10.
  end. /* for each arfold */
end procedure.
