/* ------------------------------------------------- oe/invhpost.i 11/94 gb   */
/* O/E Invoicing History Assigment of Fields                                 */
/* -------------------------------------------------------------------------- */

assign
  ar-inv.x-no           = v-xno
  ar-inv.company        = inv-head.company
  ar-inv.ord-no         = v-ord-no
  ar-inv.ord-date       = v-ord-date
  ar-inv.inv-no         = inv-head.inv-no
  ar-inv.sold-name      = inv-head.sold-name
  ar-inv.bill-to        = inv-head.bill-to
  ar-inv.sold-city      = inv-head.sold-city
  ar-inv.sold-zip       = inv-head.sold-zip
  ar-inv.contact        = inv-head.contact
  ar-inv.terms          = inv-head.terms
  ar-inv.frt-pay        = inv-head.frt-pay
  ar-inv.fob-code       = inv-head.fob-code
  ar-inv.carrier        = inv-head.carrier
  ar-inv.cust-no        = inv-head.cust-no
  ar-inv.inv-date       = inv-head.inv-date
  ar-inv.sold-id        = inv-head.sold-no
  ar-inv.ship-id        = inv-head.sold-no /* RLL */
  ar-inv.addr[1]        = inv-head.addr[1]
  ar-inv.addr[2]        = inv-head.addr[2]
  ar-inv.state          = inv-head.state
  ar-inv.zip            = inv-head.zip
  ar-inv.city           = inv-head.city
  ar-inv.sold-state     = inv-head.sold-state
  ar-inv.cust-name      = inv-head.cust-name
  ar-inv.terms-d        = inv-head.terms-d
  ar-inv.sold-addr[1]   = inv-head.sold-addr[1]
  ar-inv.sold-addr[2]   = inv-head.sold-addr[2]
  ar-inv.bill-i[1]      = inv-head.bill-i[1]
  ar-inv.bill-i[2]      = inv-head.bill-i[2]
  ar-inv.bill-i[3]      = inv-head.bill-i[3]
  ar-inv.bill-i[4]      = inv-head.bill-i[4]
  ar-inv.f-bill         = inv-head.f-bill
  ar-inv.ship-i[1]      = inv-head.ship-i[1]
  ar-inv.ship-i[2]      = inv-head.ship-i[2]
  ar-inv.ship-i[3]      = inv-head.ship-i[3]
  ar-inv.ship-i[4]      = inv-head.ship-i[4]
  ar-inv.STAT           = inv-head.STAT
  ar-inv.TAX-code       = inv-head.TAX-GR
  ar-inv.t-comm         = ar-inv.t-comm + inv-head.t-comm
  ar-inv.t-weight       = inv-head.t-inv-weight   /* total weight shipped */
  ar-inv.freight        = inv-head.t-inv-freight  /* total freight Invoiced */
  ar-inv.tax-amt        = inv-head.t-inv-tax      /* total tax Invoiced */
  ar-inv.t-cost         = inv-head.t-inv-cost     /* total cost invoiced */
  ar-inv.due   = if inv-head.terms eq "CASH" then 0 else inv-head.t-inv-rev
						  /* total invoiced amount */
  ar-inv.gross = inv-head.t-inv-rev /*+ v-inv-disc   total invoiced + disc */
  ar-inv.posted         = yes
  ar-inv.printed        = yes
  ar-inv.period         = tran-period
  ar-inv.disc-taken     = 0
  ar-inv.paid           = 0
				    /* total invoiced - fright - misc - tax */
  ar-inv.t-sales  = inv-head.t-inv-rev - inv-head.t-inv-tax
  ar-inv.net   = inv-head.t-inv-rev - inv-head.t-inv-tax.

  if inv-head.f-bill then
  assign
    ar-inv.t-sales = ar-inv.t-sales - inv-head.t-inv-freight .
    /* ar-inv.net   = ar-inv.net - inv-head.t-inv-freight. */

  find first terms where terms.company = cocode and
			 terms.t-code  = inv-head.terms
			 no-lock no-error.
  if available terms then
     assign ar-inv.due-date  = ar-inv.inv-date + terms.net-days
	    ar-inv.disc-%    = terms.disc-rate
	    ar-inv.disc-days = terms.disc-days.

/***
assign oe-ord.stat = "P"
  oe-ord.inv-no = 0
  oe-ord.inv-date = ?
  oe-ord.posted = no
  oe-ord.t-inv-weight = 0
  oe-ord.t-inv-tax = 0
  oe-ord.t-inv-freight = 0
  oe-ord.t-inv-rev = 0
  oe-ord.t-inv-cost = 0.
***/


   /* multiple currency mods */
   FIND FIRST cust WHERE cust.company = inv-head.company
                     AND cust.cust-no = inv-head.cust-no NO-LOCK NO-ERROR.
   IF AVAIL cust THEN ASSIGN ar-inv.curr-code[1] = cust.curr-code.
   IF cust.curr-code = "" THEN DO:
      FIND company WHERE company.company = inv-head.company NO-LOCK NO-ERROR.
      IF AVAIL company THEN ar-inv.curr-code[1] = company.curr-code.
   END.            
   FIND currency WHERE currency.company = inv-head.company
                   AND currency.c-code = ar-inv.curr-code[1] NO-LOCK NO-ERROR.
   IF AVAIL currency THEN ar-inv.ex-rate = currency.ex-rate .
   /* copy notes */
   DEF BUFFER new-notes FOR notes.
   FOR EACH notes WHERE notes.rec_key = inv-head.rec_key NO-LOCK:
       CREATE new-notes.
       BUFFER-COPY notes EXCEPT notes.rec_key TO new-notes. 
       new-notes.rec_key = ar-inv.rec_key.
   END.
   /*copy notes for Group By Date (multi-invoice)*/
   IF inv-head.multi-invoice THEN DO:
       DEF BUFFER bf-inv-head FOR inv-head.
       FOR EACH bf-inv-head 
           WHERE bf-inv-head.company EQ inv-head.company
             AND bf-inv-head.inv-no EQ inv-head.inv-no
             AND bf-inv-head.cust-no EQ inv-head.cust-no
             AND NOT bf-inv-head.multi-invoice 
           NO-LOCK:
           FOR EACH notes WHERE notes.rec_key EQ bf-inv-head.rec_key NO-LOCK:
               CREATE new-notes.
               BUFFER-COPY notes EXCEPT notes.rec_key TO new-notes.
               new-notes.rec_key = ar-inv.rec_key.
            END.
       END.
   END.
