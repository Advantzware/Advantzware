/* -------------------------------------------------- oe/oe-rpric.i 11/96 JLF */
/*                                                                            */
/* order entry - ITEM PRICING FROM PRICE MATRIX                               */
/*                   FOR STOCK BOXES ONLY                                     */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

define shared buffer x{2} for {2}. /* BUFFER WITH ORDER HEADER */

DEF BUFFER tmp-{1}    FOR {1}.
DEF BUFFER b-oe-ord   FOR oe-ord.
DEF BUFFER b-oe-ordl  FOR oe-ordl.
DEF BUFFER b-ar-inv   FOR ar-inv.
DEF BUFFER b-ar-invl  FOR ar-invl.
DEF BUFFER b-inv-head FOR inv-head.
DEF BUFFER b-inv-line FOR inv-line.

define shared var price-ent as log initial false no-undo.

define var v-i-item like {1}.i-no. /* INPUT ITEM */
define var v-i-qty like {1}.qty. /* INPUT QUANTITY */
DEF VAR ldt AS DATE NO-UNDO.
define var class-qty as int extent 13.
DEFINE VAR matrixExists AS LOG NO-UNDO.
DEF VAR lv-date AS CHAR NO-UNDO.

{sys/inc/sellpric.i}


find first cust
    {sys/ref/custW.i}
      and cust.cust-no eq x{2}.cust-no
    use-index cust no-lock.

find first {1} of x{2} {3} no-error.
if not avail {1} then return.

DISABLE TRIGGERS FOR LOAD OF {1}.
DISABLE TRIGGERS FOR LOAD OF tmp-{1}.

ASSIGN
 lv-date  = STRING(YEAR(TODAY),"9999") +
            STRING(MONTH(TODAY),"99")  +
            STRING(DAY(TODAY),"99")
 v-i-item = {1}.i-no
 v-i-qty  = {1}.{4}.

{oe/oe-pric1.i "{1}" "{2}" "{3}" "{4}"}

for each tmp-{1} of x{2} {3}:
  assign
   v-i-item = tmp-{1}.i-no
   v-i-qty  = tmp-{1}.qty.

  {oe/oe-pric2.i "tmp-{1}" "{2}" "{4}"}
end.

/* end ---------------------------------- copr. 1996  advanced software, inc. */

