

  /* SKB- 1/24/07 EXCEL AUTOMATION */
  
  /* Quote# */
  
  ASSIGN v-cell = "R1C5".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = v-first-q-no .

  /* Customer ID */
  
  ASSIGN v-cell = "R4C3".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = xquo.cust-no .

  /* Contact */
  
  ASSIGN v-cell = "R4C5".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = xquo.contact .

  /* Telephone */
  
  ASSIGN v-cell = "R6C3".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = cust.area-code + cust.phone .
    
  /* Fax */
  
  ASSIGN v-cell = "R6C5".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = cust.fax .

  /* Sold To */
  
  ASSIGN v-cell = "R9C1".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = bill[1] .

  ASSIGN v-cell = "R10C1".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = bill[2] .

  ASSIGN v-cell = "R11C1".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = bill[3] .

  ASSIGN v-cell = "R12C1".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = bill[4] .

  /* Ship To */
  
  ASSIGN v-cell = "R9C3".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = shipto[1] .

  ASSIGN v-cell = "R10C3".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = shipto[2] .

  ASSIGN v-cell = "R11C3".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = shipto[3] .

  ASSIGN v-cell = "R12C3".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = shipto[4] .

  /* Quote Date */
  
  ASSIGN v-cell = "R15C1".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value =  xquo.quo-date.

  /* SalesPerson */
  
  ASSIGN v-cell = "R15C2".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value =  sman.sname.

  /* Terms */
  
  ASSIGN v-cell = "R15C3".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value =  terms.dscr .

  /* Ship Via */
  
  ASSIGN v-cell = "R15C4".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value =  carrier.dscr .

  /* FOB */
  
  ASSIGN v-cell = "R15C5".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value =  cust.fob-code.

  /* Over-Under */
  
  ASSIGN v-cell = "R15C6".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = v-over-under .
