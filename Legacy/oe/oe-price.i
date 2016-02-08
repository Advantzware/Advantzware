/* --------------------------------------------------- oe/oe-price.i 5/93 rd  */
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

define shared var save_id as recid no-undo.  /* RECORD ID FOR ORDER LINE */
define shared var v-i-item like itemfg.i-no no-undo. /* INPUT ITEM */
define shared var v-i-qty like {1}.qty no-undo. /* INPUT QUANTITY */
define shared var price-ent as log NO-UNDO.
DEFINE SHARED VAR matrixExists AS LOG NO-UNDO.

define var class-qty as int extent 13 no-undo.
DEF VAR ldt AS DATE NO-UNDO.
DEF VAR lv-i-no LIKE v-i-item NO-UNDO.
DEF VAR lv-date AS CHAR NO-UNDO.

{sys/inc/sellpric.i}

ASSIGN
 lv-date = STRING(YEAR(TODAY),"9999") +
           STRING(MONTH(TODAY),"99")  +
           STRING(DAY(TODAY),"99")
 lv-i-no = v-i-item.

DISABLE TRIGGERS FOR LOAD OF {1}.
DISABLE TRIGGERS FOR LOAD OF tmp-{1}.

if save_id ne ? then
  find {1} where recid({1}) = save_id no-error.

find first cust
    {sys/ref/custW.i}
      and cust.cust-no eq x{2}.cust-no
   use-index cust no-lock.

{oe/oe-pric1.i "{1}" "{2}" "{3}" "{4}"}

if cust.auto-reprice then do:
  for each tmp-{1} of x{2}
      where RECID(tmp-{1}) <> RECID({1})
      {3}:
    v-i-item = tmp-{1}.i-no.
    {oe/oe-pric2.i "tmp-{1}" "{2}" "{4}"}
  end.
end.

v-i-item = lv-i-no.

IF v-i-item EQ "" THEN v-i-item = {1}.i-no.

{oe/oe-pric2.i "{1}" "{2}" "{4}"}

/* end ---------------------------------- copr. 1992  advanced software, inc. */
