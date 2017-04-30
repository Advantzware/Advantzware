  /* /* Quote# */
  
  ASSIGN v-cell = "R1C5".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = v-first-q-no */

  /* Customer ID */
  
  v-cell = "R4C3".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = xquo.cust-no

  /* Contact */
  
  v-cell = "R4C6".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = xquo.contact

  /* Telephone */
  
  v-cell = "R6C3".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = cust.area-code + cust.phone
    
  /* Fax */
  
  v-cell = "R6C6".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = cust.fax

  /* Sold To */
  
  v-cell = "R9C1".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = bill[1]

  v-cell = "R10C1".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = bill[2]

  v-cell = "R11C1".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = bill[3]

  v-cell = "R12C1".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = bill[4]

  /* Ship To */
  
  v-cell = "R9C3".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = shipto[1]

  v-cell = "R10C3".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = shipto[2]

  v-cell = "R11C3".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = shipto[3]

  v-cell = "R12C3".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = shipto[4]

  /* Quote Date */
  
  v-cell = "R15C1".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value =  xquo.quo-date

  /* SalesPerson */
  
  v-cell = "R15C2".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value =  sman.sname

  /* Terms */
  
  v-cell = "R15C3".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value =  terms.dscr

  /* FOB */
  
  v-cell = "R15C6".
  chExcelApplication:Goto(v-cell) NO-ERROR.
 /* IF AVAIL est THEN
        FIND FIRST eb 
             WHERE eb.company EQ est.company
               AND eb.est-no  EQ est.est-no
               AND eb.part-no EQ xquo.part-no
               AND eb.form-no NE 0
             NO-LOCK NO-ERROR.
   
     IF NOT AVAIL eb AND xquo.est-no <> "" THEN  */
        FIND FIRST eb
             WHERE eb.company EQ est.company
               AND eb.est-no  EQ est.est-no
               and eb.form-no NE 0
             NO-LOCK NO-ERROR.
     IF AVAIL eb THEN
   IF eb.carrier = "CPU"  THEN 
  ASSIGN chExcelApplication:ActiveCell:Value =  "Our Dock" .
      ELSE 
 ASSIGN chExcelApplication:ActiveCell:Value =  cust.fob-code .
/*  ASSIGN chExcelApplication:ActiveCell:Value =  cust.fob-code .*/

  /* Over-Under */
  
  v-cell = "R15C7".
  chExcelApplication:Goto(v-cell) NO-ERROR.
 
  ASSIGN chExcelApplication:ActiveCell:Value = v-over-under.
