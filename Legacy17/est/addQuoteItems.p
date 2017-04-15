DEF INPUT PARAMETER iprQuote AS ROWID NO-UNDO.
DEF INPUT PARAMETER ipcPriceType AS CHAR NO-UNDO.

{custom/globdefs.i}
{custom/gcompany.i}
{sys/inc/var.i NEW SHARED}
{sys/inc/varasgn.i}

DEF VAR cCustNo AS CHAR NO-UNDO. 
 DEF VAR li AS INT NO-UNDO.
  DEF VAR char-hdl AS CHAR NO-UNDO.
  DEF VAR cPriceType AS CHAR NO-UNDO.
  DEF VAR io-rowid-list AS CHAR NO-UNDO .
  DEF VAR lv-multi-select  AS LOG NO-UNDO.
  DEF VAR lv-qty AS DECIMAL NO-UNDO.
  DEF VAR lv-price AS DECIMAL NO-UNDO.
  DEF VAR lv-uom AS CHAR NO-UNDO.
  DEF VAR io-qty-list AS CHAR NO-UNDO.
  DEF VAR io-price-list AS CHAR NO-UNDO.
  DEF VAR io-uom-list AS CHAR NO-UNDO.
  DEF VAR hHand AS HANDLE NO-UNDO.
  DEF VAR lv-rowid AS ROWID NO-UNDO.
  DEF VAR lv-part-no AS CHAR NO-UNDO.
  cPriceType = ipcPriceType.
  hHand = THIS-PROCEDURE.
  DEF BUFFER bfCust FOR cust.

{fg/d-invprc.i NEW}
  SESSION:SET-WAIT-STATE("general").

  FIND quotehd WHERE ROWID(quotehd) EQ iprQuote NO-LOCK NO-ERROR.
  IF NOT AVAIL quotehd THEN
    RETURN.
/*   /* Prompt for what items to list */         */
/*   RUN est/d-qpriceType.w (OUTPUT cPriceType). */

  IF cPriceType EQ "Invoice" THEN
     RUN fg/d-invprc.w (quotehd.cust-no).
  ELSE DO: 
    FIND FIRST itemfg WHERE itemfg.company = quotehd.company
       AND itemfg.cust-no = quotehd.cust NO-LOCK NO-ERROR.
    IF NOT AVAIL itemfg THEN DO:
        FOR EACH bfCust WHERE bfCust.company EQ quotehd.company
          AND bfCust.active = "X" NO-LOCK:
          /* If itemfg of cust not avail, try with customer "X" */
          FIND FIRST itemfg WHERE itemfg.company EQ quotehd.company
            AND itemfg.cust-no = bfCust.cust-no
            NO-LOCK NO-ERROR.
        END.        
    END.
    FIND FIRST bfCust WHERE bfCust.company EQ quotehd.company
       AND bfCust.cust-no EQ itemfg.cust-no NO-LOCK NO-ERROR.
    IF AVAIL itemfg  THEN DO: 
      EMPTY TEMP-TABLE tt-inv.
      io-Rowid-list = STRING(ROWID(bfCust)).
      
      RUN est/d-qprice.w (INPUT-OUTPUT io-rowid-list, INPUT hHand, OUTPUT io-qty-list, OUTPUT io-price-list, OUTPUT io-uom-list).
      
      IF NUM-ENTRIES(io-rowid-list) GT 1 THEN
        lv-multi-select = TRUE.
      ELSE
        lv-multi-select = FALSE.
  
      EACH-SELECTED:
      DO li = 1 TO NUM-ENTRIES(io-rowid-list):
        IF io-rowid-list = "" THEN
            LEAVE.
        ASSIGN 
           lv-rowid = ?
           lv-qty   = 0
           lv-price = 0
           lv-uom   = "".    
  
        lv-rowid = TO-ROWID(ENTRY(li, io-rowid-list)) NO-ERROR.
        lv-qty   = DECIMAL(ENTRY(li, io-qty-list)) NO-ERROR.
        lv-price = DECIMAL(ENTRY(li, io-price-list)) NO-ERROR.
        lv-uom   = ENTRY(li, io-uom-list) NO-ERROR.        
        
        CREATE tt-inv.
        ASSIGN tt-inv.row-id = lv-rowid
               tt-inv.sell-price = lv-price
               tt-inv.qty        = lv-qty
               tt-inv.pr-qty-uom = lv-uom
               tt-inv.SELEkT    = TRUE.
      END.
    END.

  END.

  SESSION:SET-WAIT-STATE("").
  IF cPriceType EQ "Invoice" THEN DO:
  
    FOR EACH tt-inv WHERE tt-inv.selekt,
        FIRST ar-invl WHERE ROWID(ar-invl) EQ tt-inv.row-id NO-LOCK,
        FIRST itemfg
        WHERE itemfg.company EQ ar-invl.company
          AND itemfg.i-no    EQ ar-invl.i-no
        NO-LOCK:
    
      FIND LAST quoteitm
          WHERE quoteitm.company EQ quotehd.company
            AND quoteitm.loc     EQ quotehd.loc
            AND quoteitm.q-no    EQ quotehd.q-no
          USE-INDEX q-line NO-LOCK NO-ERROR.
      li = (IF AVAIL quoteitm THEN quoteitm.line else 0) + 1.
  
      ASSIGN
       lv-part-no = ""
       lv-rowid   = ROWID(itemfg).
      RUN custom/getcpart.p (quotehd.company, quotehd.cust-no,
                             INPUT-OUTPUT lv-part-no, INPUT-OUTPUT lv-rowid).
      FIND FIRST quoteitm 
        WHERE quoteitm.company EQ quotehd.company
          AND quoteitm.loc EQ quotehd.loc
          AND quoteitm.q-no EQ quotehd.q-no
          AND quoteitm.i-no EQ itemfg.i-no          
        NO-LOCK NO-ERROR.

      IF NOT AVAIL quoteitm THEN DO:      
        CREATE quoteitm.
        ASSIGN
         quoteitm.company    = quotehd.company
         quoteitm.loc        = quotehd.loc
         quoteitm.q-no       = quotehd.q-no
         quoteitm.line       = li
         quoteitm.upd-date   = TODAY
         quoteitm.upd-user   = USERID("nosweat")
         quoteitm.part-no    = IF lv-part-no EQ "" THEN itemfg.part-no
                                                   ELSE lv-part-no
         quoteitm.part-dscr1 = itemfg.i-name
         quoteitm.style      = itemfg.style
         quoteitm.price      = ar-invl.unit-pr
         quoteitm.qty        = ar-invl.inv-qty
         quoteitm.uom        = ar-invl.pr-qty-uom
         quoteitm.size       = STRING(itemfg.l-score[50]) + " x " +
                               STRING(itemfg.w-score[50]) + " x " +
                               STRING(itemfg.d-score[50])
         /*RCO400*/
         quoteitm.i-no = itemfg.i-no.
      END.

      CREATE quoteqty.
      ASSIGN
       quoteqty.company    = quoteitm.company
       quoteqty.loc        = quoteitm.loc
       quoteqty.q-no       = quoteitm.q-no
       quoteqty.line       = quoteitm.line
       quoteqty.qty        = quoteitm.qty
       quoteqty.price      = quoteitm.price
       quoteqty.uom        = quoteitm.uom
       quoteqty.quote-date = TODAY
       quoteqty.quote-user = USERID("nosweat").
  
      RUN sys/ref/convcuom.p(itemfg.prod-uom, "M", 0, 0, 0, 0,
                             itemfg.std-mat-cost,
                             OUTPUT quoteqty.mat-cost).
  
      RUN sys/ref/convcuom.p(itemfg.prod-uom, "M", 0, 0, 0, 0,
                             itemfg.std-lab-cost,
                             OUTPUT quoteqty.lab-cost).
  
      RUN sys/ref/convcuom.p(itemfg.prod-uom, "M", 0, 0, 0, 0,
                             itemfg.std-fix-cost,
                             OUTPUT quoteqty.fo-cost).
  
      RUN sys/ref/convcuom.p(itemfg.prod-uom, "M", 0, 0, 0, 0,
                             itemfg.std-var-cost,
                             OUTPUT quoteqty.vo-cost).
    END.
  END.
  ELSE DO:
    FOR EACH tt-inv WHERE tt-inv.selekt,    
        FIRST itemfg
        WHERE ROWID(itemfg) EQ tt-inv.row-id
        NO-LOCK:
    
      FIND LAST quoteitm
          WHERE quoteitm.company EQ quotehd.company
            AND quoteitm.loc     EQ quotehd.loc
            AND quoteitm.q-no    EQ quotehd.q-no
          USE-INDEX q-line NO-LOCK NO-ERROR.
      li = (IF AVAIL quoteitm THEN quoteitm.line else 0) + 1.
  
      ASSIGN
       lv-part-no = ""
       lv-rowid   = ROWID(itemfg).
      RUN custom/getcpart.p (quotehd.company, quotehd.cust-no,
                             INPUT-OUTPUT lv-part-no, INPUT-OUTPUT lv-rowid).
        FIND FIRST quoteitm 
        WHERE quoteitm.company EQ quotehd.company
          AND quoteitm.loc EQ quotehd.loc
          AND quoteitm.q-no EQ quotehd.q-no
          AND quoteitm.i-no EQ itemfg.i-no          
        NO-LOCK NO-ERROR.

      IF NOT AVAIL quoteitm THEN DO:   
        CREATE quoteitm.
        ASSIGN
         quoteitm.company    = quotehd.company
         quoteitm.loc        = quotehd.loc
         quoteitm.q-no       = quotehd.q-no
         quoteitm.line       = li
         quoteitm.upd-date   = TODAY
         quoteitm.upd-user   = USERID("nosweat")
         quoteitm.part-no    = IF lv-part-no EQ "" THEN itemfg.part-no
                                                   ELSE lv-part-no
         quoteitm.part-dscr1 = itemfg.i-name
         quoteitm.style      = itemfg.style
         quoteitm.price      = tt-inv.sell-price
         quoteitm.qty        = tt-inv.qty
         quoteitm.uom        = tt-inv.pr-qty-uom
         quoteitm.size       = STRING(itemfg.l-score[50]) + " x " +
                               STRING(itemfg.w-score[50]) + " x " +
                               STRING(itemfg.d-score[50])
         /*RCO400*/
         quoteitm.i-no = itemfg.i-no.
      END.
      CREATE quoteqty.
      ASSIGN
       quoteqty.company    = quoteitm.company
       quoteqty.loc        = quoteitm.loc
       quoteqty.q-no       = quoteitm.q-no
       quoteqty.line       = quoteitm.line
       quoteqty.qty        = tt-inv.qty
       quoteqty.price      = tt-inv.sell-pric
       quoteqty.uom        = tt-inv.pr-qty-uom
       quoteqty.quote-date = TODAY
       quoteqty.quote-user = USERID("nosweat").
  
      RUN sys/ref/convcuom.p(itemfg.prod-uom, "M", 0, 0, 0, 0,
                             itemfg.std-mat-cost,
                             OUTPUT quoteqty.mat-cost).
  
      RUN sys/ref/convcuom.p(itemfg.prod-uom, "M", 0, 0, 0, 0,
                             itemfg.std-lab-cost,
                             OUTPUT quoteqty.lab-cost).
  
      RUN sys/ref/convcuom.p(itemfg.prod-uom, "M", 0, 0, 0, 0,
                             itemfg.std-fix-cost,
                             OUTPUT quoteqty.fo-cost).
  
      RUN sys/ref/convcuom.p(itemfg.prod-uom, "M", 0, 0, 0, 0,
                             itemfg.std-var-cost,
                             OUTPUT quoteqty.vo-cost).
    END.
  END.
