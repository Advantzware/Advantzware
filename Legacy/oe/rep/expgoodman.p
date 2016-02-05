/* ---------------------------------------------- oe/rep/expgoodman.p   */
/* Export INVOICE   for Goodman          */
/* -------------------------------------------------------------------- */

{sys/inc/var.i shared}
{oe/invwork2.i}

DEF VAR v-due-date AS DATE NO-UNDO.
DEF VAR v-inv-freight LIKE inv-head.t-inv-freight NO-UNDO.
DEF VAR v-tax-amt AS DEC NO-UNDO.
DEF VAR v-discount-amt AS DEC NO-UNDO.
DEF VAR v-discount-date AS DATE NO-UNDO.
DEF VAR v-item-total AS DEC NO-UNDO.
DEF VAR v-inv-total AS DEC NO-UNDO.
DEF VAR v-ref-no AS CHAR NO-UNDO.
DEF VAR v-misc-amt AS DEC NO-UNDO.
DEF VAR v-misc-desc AS CHAR FORMAT "X(40)" NO-UNDO.
DEF VAR v-tax AS DEC NO-UNDO.
DEF VAR v-tax-tot AS DEC NO-UNDO.
DEF VAR v-misc-tax-tot AS DEC NO-UNDO.

for each tt-report NO-LOCK where tt-report.term-id eq "",
    first inv-head where recid(inv-head) eq tt-report.rec-id no-lock,
    FIRST cust WHERE cust.company = inv-head.company
                 AND cust.cust-no = inv-head.cust-no 
                 AND cust.an-edi-cust
                 AND CAN-FIND(FIRST sys-ctrl-shipto WHERE
                     sys-ctrl-shipto.company EQ cust.company AND
                     sys-ctrl-shipto.NAME EQ "INEXPORT" AND
                     sys-ctrl-shipto.cust-vend EQ YES AND
                     sys-ctrl-shipto.cust-vend-no EQ cust.cust-no AND
                     sys-ctrl-shipto.char-fld EQ "GOODMAN")
    NO-LOCK:

    ASSIGN
       v-misc-amt = 0
       v-item-total = 0
       v-misc-desc = ""
       v-tax-tot = 0
       v-misc-tax-tot = 0
       v-inv-freight = 0.

    for each inv-line FIELDS(t-price) where
        inv-line.r-no = inv-head.r-no
        NO-LOCK:
        v-item-total = v-item-total + inv-line.t-price.
    END.
    
    for each inv-misc FIELDS(amt dscr tax) NO-LOCK WHERE
        inv-misc.company = inv-head.company and
        inv-misc.r-no = inv-head.r-no and
        inv-misc.bill = "Y" AND
        inv-misc.dscr NE "Freight":

        IF inv-misc.tax THEN DO:
           RUN ar/calctax2.p (inv-head.tax-gr, 
                             NO,
                             inv-misc.amt,
                             inv-misc.company,
                             inv-misc.inv-i-no,
                             OUTPUT v-tax).
           v-misc-tax-tot = v-misc-tax-tot + v-tax.
        END.

        ASSIGN
           v-misc-amt = v-misc-amt + inv-misc.amt.
           v-misc-desc = v-misc-desc + " " + inv-misc.dscr.
    END.

    for each inv-misc FIELDS(amt) NO-LOCK WHERE
        inv-misc.company = inv-head.company and
        inv-misc.r-no = inv-head.r-no and
        inv-misc.bill = "Y" AND
        inv-misc.dscr EQ "Freight":

        v-inv-freight = v-inv-freight + inv-misc.amt.
    END.

    v-misc-desc = TRIM(v-misc-desc).

    find first terms where
         terms.company = inv-head.company and
		 terms.t-code  = inv-head.terms
		 no-lock no-error.

    if available terms then
       assign v-due-date  = inv-head.inv-date + terms.net-days
              v-discount-amt = round( (v-misc-amt + v-item-total) * (terms.disc-rate / 100), 2)
              v-discount-date = IF v-discount-amt EQ 0 THEN ? ELSE inv-head.inv-date + terms.disc-days.
    ELSE
       ASSIGN v-due-date = inv-head.inv-date
              v-discount-amt = 0
              v-discount-date = ?.

    ASSIGN
       v-tax-amt = IF inv-head.tax-gr <> "" THEN inv-head.t-inv-tax ELSE 0
       v-inv-total = v-item-total + v-inv-freight + v-tax-amt + v-misc-amt.

    FIND FIRST oe-bolh WHERE
         oe-bolh.company EQ inv-head.company AND
         oe-bolh.bol-no EQ inv-head.bol-no
         NO-LOCK NO-ERROR.

    IF AVAIL oe-bolh THEN
       v-ref-no = oe-bolh.trailer.
    ELSE
       v-ref-no = "01-NO ASP".

    /*line item*/
    for each inv-line no-lock where
        inv-line.r-no = inv-head.r-no
        BREAK BY inv-line.r-no:
        
        /*header*/
       
        PUT "041624|" /*Vendor*/
            STRING(inv-head.inv-no) FORMAT "X(26)" "|" /*invoice #*/
            "INV|" /*document type*/
            STRING(YEAR(inv-head.inv-date),"9999") +
            STRING(MONTH(inv-head.inv-date),"99") +
            STRING(DAY(inv-head.inv-date),"99") "|" /*inv date*/
            STRING(YEAR(v-due-date),"9999") +
            STRING(MONTH(v-due-date),"99") +
            STRING(DAY(v-due-date),"99") "|" /*due date*/
            v-item-total FORM  "->>>>>>>>>>>>>9.9999" "|" /*item total amount*/
            v-inv-freight FORM "->>>>>>>>>>>>>9.9999" "|" /*total freight*/
            v-misc-amt FORM "->>>>>>>>>>>>>9.9999" "|" /*other charges*/
            v-misc-desc FORM "X(40)" "|" /*other charges description*/
            v-tax-amt  FORM "->>>>>>>>>>>>>9.9999" "|" /*total tax amount*/
            v-inv-total FORM "->>>>>>>>>>>>>9.9999" "|" /*other charges*/
            v-discount-amt FORM "->>>>>>>>>>>>>9.9999" "|" /*discount amount*/
            0 FORM ">>>>9.99" "|" /*discount pct*/.
       
        IF v-discount-date NE ? THEN
           PUT STRING(YEAR(v-due-date),"9999") +
               STRING(MONTH(v-due-date),"99") +
               STRING(DAY(v-due-date),"99") "|".
        ELSE
            PUT FILL(" ",8) "|" . /*discount date*/.
       
        PUT inv-line.po-no FORMAT "X(7)" "|" /*purchase order*/
            v-ref-no FORMAT "X(40)" "|" /*reference asp #*/
            inv-line.part-no FORMAT "X(15)" "|" /*goodman line item #*/
            inv-line.ship-qty FORM "->>>>>>>>>>>>>9.9999" "|" /*quantity shipped*/
            inv-line.price FORM "->>>>>>>>>>>>>9.9999" "|" /*unit price*/
            inv-line.t-price FORM "->>>>>>>>>>>>>9.9999" "|" /*extended price*/ .
       
        v-tax-tot = 0.

        IF FIRST-OF(inv-line.r-no) THEN
        DO:
           v-tax-tot = v-misc-tax-tot.
           PUT v-inv-freight FORM "->>>>>>>>>>>>>9.9999" "|" /*freight amount*/
               v-misc-amt FORM "->>>>>>>>>>>>>9.9999" "|" /*other charges*/ 
               v-misc-desc FORMAT "X(40)" /*other charges description*/ "|".
        END.

        ELSE
           PUT 0 FORM "->>>>>>>>>>>>>9.9999" "|"
               0 FORM "->>>>>>>>>>>>>9.9999" "|" /*other charges*/ 
                 FILL(" ",40) /*other charges description*/ "|".
       
        IF inv-line.tax THEN DO:
           RUN ar/calctax2.p (inv-head.tax-gr, 
                              NO,
                              inv-line.t-price, 
                              inv-line.company,
                              inv-line.i-no,
                              OUTPUT v-tax).
           
           v-tax-tot = v-tax-tot + v-tax.
       
           PUT v-tax-tot FORM "->>>>>>>>>>>>>9.9999" "|" SKIP.
        END.
        ELSE
           PUT v-tax-tot FORM "->>>>>>>>>>>>>9.9999" "|" SKIP.
    END.
end. /* each tt-report */
