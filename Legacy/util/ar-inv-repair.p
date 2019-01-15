DEF VAR v-invno AS INT                       NO-UNDO.
DEF VAR v-pos  AS INT                        NO-UNDO.
DEF VAR v-reccnt AS INT                      NO-UNDO.
DEF VAR ls AS CHAR                           NO-UNDO.
DEF VAR v-line-count AS INT                  NO-UNDO.
DEF VAR v-start-pos AS INT INIT 1            NO-UNDO.
DEF VAR li AS INT                            NO-UNDO.
DEF VAR fg-uom-list AS cha                   NO-UNDO.
DEF VAR ll-calc-disc-FIRST AS LOG            NO-UNDO.
DEF VAR v-format LIKE sys-ctrl.char-fld      NO-UNDO.
DEF VAR v-cost AS DEC EXTENT 4               NO-UNDO.
DEF VAR v-basis LIKE sman.commbasis INIT ""  NO-UNDO.
DEF VAR v-ref-ar AS INT                      NO-UNDO.
DEF VAR v-ref-inv AS INT                     NO-UNDO.
DEF VAR v-ref-arl AS INT                     NO-UNDO.
DEF VAR v-tax AS DEC                         NO-UNDO.
DEF VAR xx-amt LIKE inv-head.t-inv-rev       NO-UNDO.
DEF VAR xx-cost1 AS DEC                      NO-UNDO.
DEF VAR xx-cost2 AS DEC                      NO-UNDO.
DEF VAR v-tot-inv-rev AS DEC                 NO-UNDO.


DEFINE INPUT  PARAMETER ip-beg-date AS DATE       NO-UNDO.
DEFINE INPUT  PARAMETER ip-end-date AS DATE       NO-UNDO.
DEFINE INPUT  PARAMETER ip-st-invno AS INT        NO-UNDO.
DEFINE INPUT  PARAMETER ip-end-invno AS INT       NO-UNDO.


/* DEFINE VAR  ip-beg-date  AS DATE INITIAL 01/01/2000             NO-UNDO. */
/* DEFINE VAR  ip-end-date  AS DATE INITIAL 12/31/2008             NO-UNDO. */
/* DEFINE VAR  ip-st-invno  AS INT  INITIAL 1466 NO-UNDO.                   */
/* DEFINE VAR  ip-end-invno AS INT  INITIAL 1466 NO-UNDO.                   */

DEFINE VARIABLE v-inv-no AS INT  NO-UNDO.

DEF BUFFER b-inv-line FOR inv-line.
DEF BUFFER b-oe-ordl  FOR oe-ordl.
DEF BUFFER b-ar-invl  FOR ar-invl.

DEF TEMP-TABLE t-invoice NO-UNDO
  FIELD tt-invoice LIKE v-invno
  FIELD tt-amt     LIKE gltrans.tr-amt
  FIELD tt-date    LIKE gltrans.tr-date.

 

{sys/inc/var.i NEW shared}
/*
{sys/form/s-top.f}
*/



RUN sys/ref/uom-ea.p (OUTPUT fg-uom-list).

FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "INVPRINT"
    NO-LOCK NO-ERROR.


ASSIGN
 v-format           = sys-ctrl.char-fld
 ll-calc-disc-FIRST = v-format EQ "Dayton".

FIND LAST ar-inv NO-LOCK USE-INDEX x-no.
v-ref-ar = ar-inv.x-no + 1.

FIND LAST inv-head NO-LOCK USE-INDEX r-no NO-ERROR.
v-ref-inv = next-value(inv_r_no_seq).

FIND LAST ar-invl NO-LOCK USE-INDEX x-no.
v-ref-arl = ar-invl.x-no + 1.


FOR EACH gltrans WHERE
         gltrans.jrnl = "OEINV" AND
        (gltrans.tr-date >= ip-beg-date AND 
         gltrans.tr-date <= ip-end-date)  NO-LOCK:

   v-pos = R-INDEX(gltrans.tr-dscr,"Inv#").

   IF v-pos = 0 THEN NEXT.

   ASSIGN
    v-pos = v-pos + 4
    v-invno = INT(SUBSTRING(gltrans.tr-dscr,v-pos,10)).

    IF ( v-invno < ip-st-invno  OR 
         v-invno > ip-end-invno) THEN  NEXT . 
    

   FIND t-invoice WHERE
        t-invoice.tt-invoice = v-invno
       NO-LOCK NO-ERROR.


   FIND ar-inv WHERE
        ar-inv.inv-no = v-invno  
       NO-LOCK NO-ERROR.

   IF NOT AVAILABLE ar-inv THEN
   DO:
      FIND t-invoice WHERE
           t-invoice.tt-invoice = v-invno
           NO-ERROR.

      IF NOT AVAIL t-invoice THEN 
      DO:
         CREATE t-invoice.
         ASSIGN t-invoice.tt-invoice = v-invno
                t-invoice.tt-date    = gltrans.tr-date
                t-invoice.tt-amt     = gltrans.tr-amt.
      END.
      ELSE
        t-invoice.tt-amt = t-invoice.tt-amt + gltrans.tr-amt.
   END.

END.

FOR EACH t-invoice NO-LOCK:

    
      v-invno = t-invoice.tt-invoice.

      FIND FIRST oe-boll  WHERE oe-boll.inv-no = v-invno NO-LOCK NO-ERROR.

      FIND oe-bolh OF oe-boll NO-LOCK NO-ERROR.

      FIND oe-ord WHERE oe-ord.company EQ oe-boll.company
        AND oe-ord.ord-no  EQ oe-boll.ord-no
       USE-INDEX ord-no  NO-LOCK NO-ERROR .

      FIND FIRST cust
           WHERE cust.company EQ oe-bolh.company
             AND cust.cust-no EQ oe-bolh.cust-no
            NO-LOCK NO-ERROR.

      v-tot-inv-rev = 0.

      IF AVAILABLE oe-bolh THEN
      DO:
         RUN create-inv-head.        
         RUN CREATE-inv-line.
         RUN create-ar-inv.      
         FIND FIRST oe-boll  WHERE oe-boll.inv-no = v-invno NO-LOCK NO-ERROR.
         RUN create-misc-charges.

         /* Assign the net after all totals */
         ASSIGN ar-inv.net  = ar-inv.gross  + (ar-inv.tax-amt + ar-inv.freight)
                ar-inv.gross = ar-inv.gross +  (ar-inv.tax-amt + ar-inv.freight)
                ar-inv.due   = ar-inv.gross.
         FIND inv-head WHERE inv-head.inv-no = v-invno
              EXCLUSIVE-LOCK NO-ERROR.

         FOR EACH inv-line WHERE
                  inv-line.inv-no = v-invno EXCLUSIVE-LOCK:
             DELETE inv-line.
         END.

         FOR EACH inv-misc WHERE
             inv-misc.r-no = v-ref-inv EXCLUSIVE-LOCK:
             DELETE inv-misc.
         END.

      END. /* inv-line */
END.

PROCEDURE CREATE-ar-inv.

   
    DO WHILE TRUE :
        FIND ar-inv WHERE ar-inv.x-no = v-ref-ar NO-LOCK NO-ERROR.
        IF AVAILABLE ar-inv THEN 
            v-ref-ar = v-ref-ar + 1.
        ELSE LEAVE.
    END.


    CREATE ar-inv .
    ASSIGN ar-inv.x-no           = v-ref-ar
           ar-inv.company        = inv-head.company
           ar-inv.ord-no         = oe-bolh.b-ord-no
           ar-inv.ord-date       = oe-bolh.bol-date
           ar-inv.inv-no         = v-invno
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
           ar-inv.f-bill         = IF inv-head.t-inv-freight GT 0 THEN YES
                                   ELSE NO
           ar-inv.ship-i[1]      = inv-head.ship-i[1]
           ar-inv.ship-i[2]      = inv-head.ship-i[2]
           ar-inv.ship-i[3]      = inv-head.ship-i[3]
           ar-inv.ship-i[4]      = inv-head.ship-i[4]
           ar-inv.STAT           = inv-head.STAT
           ar-inv.TAX-code       = inv-head.TAX-GR
           ar-inv.t-comm         =  ar-inv.t-comm + inv-head.t-comm
           ar-inv.t-weight       = inv-head.t-inv-weight         
           ar-inv.t-cost         = inv-head.t-inv-cost     
           ar-inv.due            = v-tot-inv-rev        
           ar-inv.gross          = v-tot-inv-rev 
           ar-inv.tax-amt        = inv-head.t-inv-tax
           ar-inv.t-cost         = inv-head.t-inv-cost
           ar-inv.posted         = yes
           ar-inv.printed        = yes
           ar-inv.period         = 0 /* tran-period */
           ar-inv.disc-taken     = 0
           ar-inv.paid           = 0 
           ar-inv.t-sales        = inv-head.t-inv-rev - inv-head.t-inv-tax
           ar-inv.net            = inv-head.t-inv-rev - inv-head.t-inv-tax
           ar-inv.freight        = inv-head.t-inv-freight.
           
           RUN CopyShipNote (inv-head.rec_key, ar-inv.rec_key).
                             
           if inv-head.f-bill then
              ASSIGN ar-inv.t-sales = ar-inv.t-sales - inv-head.t-inv-freight .

           find first terms where terms.company = inv-head.company and
                      terms.t-code  = inv-head.terms
                      no-lock no-error.

           if available terms then
              assign ar-inv.due-date  = ar-inv.inv-date + terms.net-days
                     ar-inv.disc-%    = terms.disc-rate
                     ar-inv.disc-days = terms.disc-days.

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
           

END.

PROCEDURE CREATE-ar-invl.
    DO WHILE TRUE :
        FIND ar-invl WHERE ar-invl.x-no = v-ref-arl NO-LOCK NO-ERROR.
        IF AVAILABLE ar-invl THEN 
            v-ref-arl = v-ref-arl + 1.
        ELSE LEAVE.
    END.

              FIND FIRST oe-bolh WHERE oe-bolh.b-no EQ inv-line.b-no NO-LOCK NO-ERROR.
              FIND FIRST inv-misc WHERE inv-misc.r-no =  inv-head.r-no NO-LOCK NO-ERROR. 
        

              CREATE ar-invl.
              assign
               ar-invl.x-no       = v-ref-arl
               ar-invl.actnum     = if avail inv-misc THEN inv-misc.actnum else ""
               ar-invl.inv-no     = v-invno
               ar-invl.bol-no     = IF AVAIL oe-bolh THEN oe-bolh.bol-no ELSE inv-head.bol-no
               ar-invl.b-no       = inv-line.b-no
               ar-invl.company    = inv-line.company
               ar-invl.ord-no     = inv-line.ord-no
               ar-invl.cust-no    = inv-line.cust-no
               ar-invl.line       = inv-line.LINE
               ar-invl.est-no     = inv-line.est-no
               ar-invl.est-type   = inv-line.est-type
               ar-invl.form-no    = inv-line.form-no
               ar-invl.blank-no   = inv-line.blank-no
               ar-invl.job-no     = inv-line.job-no
               ar-invl.job-no2    = inv-line.job-no2
               ar-invl.part-no    = inv-line.part-no
               ar-invl.i-no       = inv-line.i-no
               ar-invl.i-name     = inv-line.i-name
               ar-invl.i-dscr     = inv-line.i-dscr
               ar-invl.po-no      = inv-line.po-no
               ar-invl.req-code   = inv-line.req-code
               ar-invl.req-date   = inv-line.req-date
               ar-invl.prom-code  = inv-line.prom-code
               ar-invl.prom-date  = inv-line.prom-date
               ar-invl.part-dscr1 = inv-line.part-dscr1
               ar-invl.part-dscr2 = inv-line.part-dscr2
               ar-invl.po-no-po   = inv-line.po-no-po
               ar-invl.cas-cnt    = inv-line.cas-cnt
               ar-invl.pr-uom     = inv-line.pr-uom
               ar-invl.unit-pr    = inv-line.price
               ar-invl.tax        = inv-line.tax
               ar-invl.disc       = inv-line.disc
               ar-invl.amt        = inv-line.t-price   /* total price of invoiced item */
               ar-invl.t-weight   = inv-line.t-weight  /* total weight of invoiced item */
               ar-invl.t-freight  = inv-line.t-freight /* total freight of invoiced item */
               ar-invl.ship-qty   = inv-line.ship-qty
               ar-invl.inv-qty    = inv-line.inv-qty
               ar-invl.qty        = inv-line.qty
               ar-invl.sman[1]    = inv-line.sman[1]
               ar-invl.sman[2]    = inv-line.sman[2]
               ar-invl.sman[3]    = inv-line.sman[3]
               ar-invl.s-pct[1]   = inv-line.s-pct[1]
               ar-invl.s-pct[2]   = inv-line.s-pct[2]
               ar-invl.s-pct[3]   = inv-line.s-pct[3]
               ar-invl.s-comm[1]  = inv-line.s-comm[1]
               ar-invl.s-comm[2]  = inv-line.s-comm[2]
               ar-invl.s-comm[3]  = inv-line.s-comm[3]
               ar-invl.sname[1]   = inv-line.sname[1]
               ar-invl.sname[2]   = inv-line.sname[2]
               ar-invl.sname[3]   = inv-line.sname[3]
               ar-invl.s-commbasis[1] = inv-line.s-commbasis[1]
               ar-invl.s-commbasis[2] = inv-line.s-commbasis[2]
               ar-invl.s-commbasis[3] = inv-line.s-commbasis[3]
               ar-invl.misc       = no
               ar-invl.posted     = yes
               ar-invl.pr-qty-uom = inv-line.pr-uom
               ar-invl.cost       = inv-line.cost
               ar-invl.dscr[1]    = "M"
               ar-invl.t-cost     = inv-line.inv-qty * inv-line.cost /* not sure */

               ar-invl.std-tot-cost = inv-line.cost
               ar-invl.std-lab-cost = itemfg.std-lab-cost
               ar-invl.std-fix-cost = itemfg.std-fix-cost
               ar-invl.std-var-cost = itemfg.std-var-cost 
               ar-invl.std-mat-cost = itemfg.std-mat-cost.

               if ar-invl.ord-no eq 0 then ar-invl.s-pct[1] = 100. 

END.

PROCEDURE create-inv-head.

  FIND FIRST shipto NO-LOCK
       WHERE shipto.company EQ oe-bolh.company
         AND shipto.ship-id EQ oe-bolh.ship-id
         AND shipto.cust-no EQ oe-bolh.cust-no
         AND shipto.ship-no NE 1
         USE-INDEX ship-id NO-ERROR.

  IF NOT AVAIL shipto THEN
  FIND FIRST shipto NO-LOCK
      WHERE shipto.company EQ oe-bolh.company
        AND shipto.cust-no EQ oe-bolh.cust-no
      USE-INDEX ship-no NO-ERROR.

   
  CREATE inv-head.
  ASSIGN inv-head.sold-no    = shipto.ship-id
       inv-head.sold-name    = shipto.ship-name
       inv-head.sold-addr[1] = shipto.ship-addr[1]
       inv-head.sold-addr[2] = shipto.ship-addr[2]
       inv-head.sold-state   = shipto.ship-state
       inv-head.sold-city    = shipto.ship-city
       inv-head.sold-zip     = shipto.ship-zip
       inv-head.r-no         = v-ref-inv
       inv-head.company      = oe-bolh.company
       inv-head.bol-no       = oe-bolh.bol-no
       inv-head.bill-to      = oe-bolh.cust-no
       inv-head.cust-no      = oe-bolh.cust-no
       inv-head.frt-pay      = oe-bolh.frt-pay
       inv-head.carrier      = oe-bolh.carrier
       inv-head.ship-i[1]    = oe-bolh.ship-i[1]
       inv-head.ship-i[2]    = oe-bolh.ship-i[2]
       inv-head.ship-i[3]    = oe-bolh.ship-i[3]
       inv-head.ship-i[4]    = oe-bolh.ship-i[4]
       inv-head.fob-code     = oe-ord.fob-code
       inv-head.contact      = oe-ord.contact
       inv-head.terms        = oe-ord.terms
       inv-head.terms-d      = oe-ord.terms-d
       inv-head.f-bill       = NO
       inv-head.tax-gr       = IF AVAIL shipto AND shipto.tax-code NE ""
                               THEN shipto.tax-code ELSE oe-ord.tax-gr
       inv-head.tot-ord      = 0
       inv-head.inv-no       = 0
       inv-head.stat         = ""
       inv-head.deleted      = NO
       inv-head.posted       = NO
       inv-head.inv-date     = t-invoice.tt-date
       inv-head.inv-no       = v-invno
       inv-head.cust-name    = cust.name
       inv-head.addr[1]      = cust.addr[1]
       inv-head.addr[2]      = cust.addr[2]
       inv-head.city         = cust.city
       inv-head.state        = cust.state
       inv-head.zip          = cust.zip
       inv-head.curr-code[1] = cust.curr-code.
      
      RUN CopyShipNote (oe-bolh.rec_key, inv-head.rec_key).    
      
      FIND FIRST usergrps WHERE
           usergrps.usergrps = "IN"
           NO-LOCK NO-ERROR.

      IF AVAIL usergrps AND TRIM(usergrps.users) NE "" THEN
      DO:
         ASSIGN
          v-line-count = 0
          v-start-pos  = 1.

         DO li = 1 TO LENGTH(usergrps.users):
            ls = SUBSTR(usergrps.users,li,1).

            IF v-line-count < 5 AND ls EQ CHR(10) OR ls EQ CHR(13) THEN
               ASSIGN
                  v-line-count = v-line-count + 1
                  inv-head.bill-i[v-line-count] = SUBSTR(usergrps.users,v-start-pos,li - v-start-pos)
                  v-start-pos = li + 1.

            IF v-line-count < 5 AND li = LENGTH(usergrps.users) AND
               NOT(ls EQ CHR(10) OR ls EQ CHR(13)) THEN
               ASSIGN
                  v-line-count = v-line-count + 1
                  inv-head.bill-i[v-line-count] = SUBSTR(usergrps.users,v-start-pos,li - v-start-pos + 1).
         END.

         RELEASE usergrps.
      END.

      DO li = 1 TO 4:
         IF inv-head.bill-i[li] = "" THEN
            inv-head.bill-i[li] = oe-ord.bill-i[li].
      END.


END.

PROCEDURE create-inv-line.
  
FOR EACH oe-boll WHERE oe-boll.b-no = oe-bolh.b-no NO-LOCK :

   FIND FIRST itemfg WHERE
             itemfg.company = inv-head.company
         AND itemfg.i-no    = oe-boll.i-no
         NO-LOCK NO-ERROR.

  FIND FIRST oe-ordl
      WHERE oe-ordl.company EQ oe-boll.company
        AND oe-ordl.ord-no  EQ oe-boll.ord-no
        AND oe-ordl.line    EQ oe-boll.line
        AND oe-ordl.i-no    EQ oe-boll.i-no
       USE-INDEX ord-no  NO-LOCK NO-ERROR .

  IF AVAIL oe-ordl THEN
    find first inv-line
        where inv-line.r-no   eq inv-head.r-no
          and inv-line.ord-no eq oe-boll.ord-no
          and inv-line.b-no   eq oe-bolh.b-no
          and inv-line.i-no   eq oe-boll.i-no
          and inv-line.line   eq oe-boll.line
          and inv-line.po-no  eq oe-boll.po-no
          use-index r-no no-error.

  IF NOT AVAIL inv-line THEN 
  DO:
      
      CREATE inv-line.
      ASSIGN 
       inv-line.r-no       = v-ref-inv
       inv-line.company    = oe-bolh.company
       inv-line.ord-no     = oe-boll.ord-no
       inv-line.inv-no     = v-invno
       inv-line.b-no       = oe-bolh.b-no
       inv-line.line       = oe-boll.line
       inv-line.i-no       = oe-boll.i-no
       inv-line.stat       = oe-boll.s-code
       inv-line.est-no     = oe-ordl.est-no
       inv-line.est-type   = oe-ord.est-type
       inv-line.ord-date   = oe-ord.ord-date
       inv-line.part-no    = oe-ordl.part-no
       inv-line.i-name     = oe-ordl.i-name
       inv-line.i-dscr     = oe-ordl.i-dscr
       inv-line.pr-uom     = oe-ordl.pr-uom
       inv-line.price      = oe-ordl.price
       inv-line.cas-cnt    = IF oe-ordl.pr-uom EQ "CS" THEN oe-ordl.cas-cnt
                                                       ELSE oe-boll.qty-case
       inv-line.req-code   = oe-ordl.req-code
       inv-line.req-date   = oe-ordl.req-date
       inv-line.prom-code  = oe-ordl.prom-code
       inv-line.prom-date  = oe-ordl.prom-date
       inv-line.part-dscr1 = oe-ordl.part-dscr1
       inv-line.part-dscr2 = oe-ordl.part-dscr2
       inv-line.po-no-po   = oe-ordl.po-no-po
       inv-line.e-num      = oe-ordl.e-num
       inv-line.form-no    = oe-ordl.form-no
       inv-line.blank-no   = oe-ordl.blank-no
       inv-line.j-no       = oe-ordl.j-no
       inv-line.job-no     = oe-ordl.job-no
       inv-line.job-no2    = oe-ordl.job-no2
       inv-line.tax        = oe-ordl.tax
       inv-line.disc       = oe-ordl.disc
       inv-line.qty        = oe-ordl.qty
       inv-line.p-c        = oe-boll.p-c
       inv-line.po-no      = oe-boll.po-no.
 
          IF oe-boll.zeroPrice EQ 1 THEN
             inv-line.price = 0.
          ELSE
             IF oe-boll.sell-price NE 0 THEN
                inv-line.price = oe-boll.sell-price.
          
  END.

  ASSIGN inv-line.t-weight      = inv-line.t-weight + oe-boll.weight
         inv-head.t-inv-weight  = inv-head.t-inv-weight + oe-boll.weight
         inv-line.t-freight     = inv-line.t-freight + oe-boll.freight
         inv-head.t-inv-freight = inv-head.t-inv-freight + oe-boll.freight.
 
  IF oe-boll.s-code ne "S" and not oe-ordl.is-a-component then
     inv-line.inv-qty = inv-line.inv-qty + oe-boll.qty.
  
  /** Increase ship Qty when ship or invoice & ship **/
  if oe-boll.s-code ne "I"                                            or
     can-find(first b-oe-ordl {sys/inc/ordlcomp.i b-oe-ordl oe-ordl}) then
    inv-line.ship-qty = inv-line.ship-qty + oe-boll.qty.

  IF inv-line.pr-uom BEGINS "L" AND inv-line.pr-uom NE "LB" THEN
     inv-line.t-price = inv-line.price *
                           IF inv-line.inv-qty LT 0 THEN -1 ELSE 1.
  ELSE IF inv-line.pr-uom EQ "CS" THEN
     inv-line.t-price = inv-line.inv-qty /
                           (IF inv-line.cas-cnt NE 0 THEN
                             inv-line.cas-cnt
                            ELSE
                            IF itemfg.case-count NE 0 THEN
                              itemfg.case-count ELSE 1) *
                           inv-line.price.
  ELSE IF LOOKUP(inv-line.pr-uom,fg-uom-list) GT 0 THEN
       inv-line.t-price = inv-line.inv-qty * inv-line.price.
  ELSE
    FOR EACH uom
       WHERE uom.uom  EQ inv-line.pr-uom
         AND uom.mult NE 0 NO-LOCK:
      inv-line.t-price = inv-line.inv-qty / uom.mult * inv-line.price.
      LEAVE.
    END.

  inv-line.t-price = ROUND(inv-line.t-price,2).

  IF inv-line.disc NE 0 THEN
     inv-line.t-price = 
        IF ll-calc-disc-first THEN 
          (inv-line.t-price - ROUND(inv-line.t-price * inv-line.disc / 100,2))
        ELSE
          ROUND(inv-line.t-price * (1 - (inv-line.disc / 100)),2).


  /** Calculations **/
  ASSIGN v-tot-inv-rev     = v-tot-inv-rev + inv-line.t-price.

  FIND FIRST stax WHERE stax.company = inv-head.company AND
                      stax.tax-group = inv-head.tax-gr NO-LOCK NO-ERROR.
  if avail stax THEN
      v-tax = ROUND(inv-line.t-price * stax.tax-rate[1] / 100,2).
    
  ASSIGN
  xx-amt = inv-head.t-inv-rev
  inv-head.t-inv-rev = xx-amt
  inv-head.t-inv-tax =  inv-head.t-inv-tax + v-tax.

  do i = 1 to 3:          /** Calculate Commission Amount **/
    ASSIGN inv-line.sname[i]   = oe-ord.sname[i]
           inv-line.s-comm[i]  = oe-ordl.s-comm[i]
           inv-line.s-pct[i]   = oe-ordl.s-pct[i]
           inv-line.sman[i]    = oe-ordl.s-man[i].
  end.

  DO i = 1 TO EXTENT(inv-line.sman):    /** Calculate Commission Amount **/
     RUN custom/combasis.p (oe-boll.company, inv-line.sman[i], cust.type, itemfg.procat, 0,
                           cust.cust-no,
                           OUTPUT v-basis).

     IF v-basis EQ "G" THEN
      inv-line.comm-amt[i] = ROUND(((inv-line.t-price - inv-line.t-cost)
                                       * inv-line.s-comm[i]) / 100,2).

     ELSE
      inv-line.comm-amt[i] = ROUND((((inv-line.t-price
                                       * inv-line.s-pct[i]) / 100)
                                       * inv-line.s-comm[i]) / 100,2).        
      
  END.

  RUN create-ar-invl.


END.

 
END PROCEDURE.



PROCEDURE create-misc-charges.


  for each oe-ordm
    where oe-ordm.company eq oe-boll.company
      AND oe-ordm.ord-no  EQ oe-boll.ord-no
      and oe-ordm.bill    eq "I":
  

      create inv-misc.
      BUFFER-COPY oe-ordm EXCEPT rec_key TO inv-misc
      ASSIGN inv-misc.r-no           = v-ref-inv
             inv-misc.posted         = no
             inv-misc.deleted        = no
             inv-misc.inv-i-no       = oe-ordm.ord-i-no
             inv-misc.inv-line       = oe-ordm.ord-line
             inv-misc.s-commbasis[1] = oe-ordm.commbasis[1].

   
      find LAST b-ar-invl  WHERE
           b-ar-invl.x-no = v-ref-arl
           no-lock NO-ERROR.

         
      create ar-invl.
      ASSIGN ar-invl.x-no           = v-ref-arl
             ar-invl.company        = inv-misc.company
             ar-invl.INV-NO         = inv-head.inv-no
             ar-invl.ord-no         = inv-misc.ord-no
             ar-invl.cust-no        = inv-head.cust-no
             ar-invl.line           = IF AVAIL b-ar-invl THEN b-ar-invl.LINE + 1
                                      ELSE 1
             ar-invl.est-no         = inv-misc.est-no
             ar-invl.tax            = inv-misc.tax
             ar-invl.actnum         = inv-misc.actnum
             ar-invl.prep-amt       = inv-misc.amt
             ar-invl.qty            = 1
             ar-invl.unit-pr        = inv-misc.amt
             ar-invl.amt            = inv-misc.amt
             ar-invl.t-cost         = inv-misc.cost
             ar-invl.cost           = ar-invl.t-cost / 1000
             ar-invl.dscr[1]        = "M"
             ar-invl.prep-charge    = inv-misc.charge
             ar-invl.prep-cost      = inv-misc.cost
             ar-invl.prep-dscr      = inv-misc.dscr
             ar-invl.i-name         = inv-misc.charge
             ar-invl.i-dscr         = inv-misc.dscr
             ar-invl.po-no          = inv-misc.po-no
             ar-invl.po-no-po       = inv-misc.po-no-po
             ar-invl.sman[1]        = inv-misc.s-man[1]
             ar-invl.sman[2]        = inv-misc.s-man[2]
             ar-invl.sman[3]        = inv-misc.s-man[3]
             ar-invl.s-pct[1]       = inv-misc.s-pct[1]
             ar-invl.s-pct[2]       = inv-misc.s-pct[2]
             ar-invl.s-pct[3]       = inv-misc.s-pct[3]
             ar-invl.s-comm[1]      = inv-misc.s-comm[1]
             ar-invl.s-comm[2]      = inv-misc.s-comm[2]
             ar-invl.s-comm[3]      = inv-misc.s-comm[3]
             ar-invl.s-commbasis[1] = inv-misc.s-commbasis[1]
             ar-invl.s-commbasis[2] = inv-misc.s-commbasis[2]
             ar-invl.s-commbasis[3] = inv-misc.s-commbasis[3]
             ar-invl.inv-i-no       = inv-misc.inv-i-no
             ar-invl.inv-line       = inv-misc.inv-line
             ar-invl.misc           = YES
             ar-invl.billable       = IF inv-misc.bill EQ "I" THEN TRUE
                                      ELSE FALSE
             ar-invl.posted         = YES.

             IF NOT ar-invl.billable THEN ar-invl.amt = 0.
              

      FIND FIRST stax WHERE stax.company = inv-head.company AND
                      stax.tax-group = inv-head.tax-gr NO-LOCK NO-ERROR.
      
   
      if avail stax THEN
         v-tax = ROUND(inv-misc.amt * stax.tax-rate[1] / 100,2).

 
      FIND ar-inv WHERE
           ar-inv.company = inv-misc.company 
       AND ar-inv.inv-no = inv-head.inv-no
           EXCLUSIVE-LOCK NO-ERROR.

      DEF VAR tt AS DEC.


      ASSIGN ar-inv.gross = ar-inv.gross 
             ar-inv.tax-amt = ar-inv.tax-amt + v-tax.
             tt = ar-inv.gross.
             tt = tt + inv-misc.amt.

      ASSIGN ar-inv.gross = tt.



end.



END PROCEDURE.

PROCEDURE CopyShipNote PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Copies Ship Note from rec_key to rec_key
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipcRecKeyFrom AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcRecKeyTo AS CHARACTER NO-UNDO.

DEFINE VARIABLE hNotesProcs AS HANDLE NO-UNDO.

    RUN "sys/NotesProcs.p" PERSISTENT SET hNotesProcs.  

    RUN CopyShipNote IN hNotesProcs (ipcRecKeyFrom, ipcRecKeyTo).

    DELETE OBJECT hNotesProcs.   

END PROCEDURE.
    