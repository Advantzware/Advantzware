DEF INPUT PARAMETER ipCustNo AS CHAR NO-UNDO.
DEF INPUT PARAMETER ipiNo AS CHAR NO-UNDO.
DEF INPUT PARAMETER ipQty AS DEC NO-UNDO.
DEFINE OUTPUT PARAMETER opPrice AS DECIMAL     NO-UNDO.
DEFINE OUTPUT PARAMETER opUOM AS CHARACTER   NO-UNDO.

DEF TEMP-TABLE tt-oe-ord LIKE oe-ord.
DEF TEMP-TABLE tt-oe-ordl LIKE oe-ordl.
{custom/globdefs.i}
{custom/gcompany.i}
{sys/inc/var.i NEW SHARED}
{sys/inc/varasgn.i}

CREATE tt-oe-ord.
ASSIGN tt-oe-ord.cust-no = ipCustNo
       tt-oe-ord.company = cocode
       tt-oe-ord.ord-no  = 1.

CREATE tt-oe-ordl.
ASSIGN tt-oe-ordl.company = cocode
       tt-oe-ordl.ord-no  = tt-oe-ord.ord-no
       tt-oe-ordl.cust-no = ipCustNo
       tt-oe-ordl.i-no = ipiNo
       tt-oe-ordl.qty  = ipQty.
/* {oe/oe-price.i tt-oe-ordl tt-oe-ord "use-index ord-no" "qty"} */

/* --------------------------------------------------- oe/oe-price.i 5/93 rd  */
/*                                                                            */
/* order entry - ITEM PRICING FROM PRICE MATRIX                               */
/*                   FOR STOCK BOXES ONLY                                     */
/* -------------------------------------------------------------------------- */



DEFINE buffer xtt-oe-ord for tt-oe-ord. /* BUFFER WITH ORDER HEADER */
FIND FIRST xtt-oe-ord NO-ERROR.

IF NOT AVAIL xtt-oe-ord THEN DO:
  CREATE xtt-oe-ord.
ASSIGN xtt-oe-ord.cust-no = ipCustNo
       xtt-oe-ord.company = cocode
       xtt-oe-ord.ord-no  = 2.
CREATE tt-oe-ordl.
ASSIGN tt-oe-ordl.company = cocode
       tt-oe-ordl.ord-no  = xtt-oe-ord.ord-no
       tt-oe-ordl.cust-no = ipCustNo
       tt-oe-ordl.i-no = ipiNo
       tt-oe-ordl.qty  = ipQty.
END.

DEF BUFFER tmp-tt-oe-ordl    FOR tt-oe-ordl.
DEF BUFFER b-oe-ord   FOR oe-ord.
DEF BUFFER b-oe-ordl  FOR oe-ordl.
DEF BUFFER b-ar-inv   FOR ar-inv.
DEF BUFFER b-ar-invl  FOR ar-invl.
DEF BUFFER b-inv-head FOR inv-head.
DEF BUFFER b-inv-line FOR inv-line.

define /* shared */ var save_id as recid no-undo.  /* RECORD ID FOR ORDER LINE */
define /* shared */ var v-i-item like itemfg.i-no no-undo. /* INPUT ITEM */
define /* shared */ var v-i-qty like tt-oe-ordl.qty no-undo. /* INPUT QUANTITY */
define /* shared */ var price-ent as log NO-UNDO.
DEFINE /*SHARED */ VAR matrixExists AS LOG NO-UNDO.
SAVE_id = ?.
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



if save_id ne ? then
  find tt-oe-ordl where recid(tt-oe-ordl) = save_id no-error.

find first cust
    {sys/ref/custW.i}
      and cust.cust-no eq xtt-oe-ord.cust-no
   use-index cust no-lock.

{oe/oe-pric1.i "tt-oe-ordl" "tt-oe-ord" "use-index ord-no" "qty"}
    
/* if cust.auto-reprice then do:                          */
/*   for each tmp-tt-oe-ordl of xtt-oe-ord                */
/*       where RECID(tmp-tt-oe-ordl) <> RECID(tt-oe-ordl) */
/*       use-index ord-no:                                */
/*     v-i-item = tmp-tt-oe-ordl.i-no.                    */
/*     {oe/oe-pric2.i "tmp-tt-oe-ordl" "tt-oe-ord" "qty"} */
/*   end.                                                 */
/* end.                                                   */

v-i-item = lv-i-no.

IF v-i-item EQ "" THEN v-i-item = tt-oe-ordl.i-no.
FIND FIRST itemfg WHERE itemfg.company EQ tt-oe-ord.company
   AND itemfg.i-no = tt-oe-ordl.i-no NO-LOCK NO-ERROR.
{oe/oe-pric2.i "tt-oe-ordl" "tt-oe-ord" "qty"}
  
IF AVAIL tt-oe-ordl THEN
  ASSIGN opPrice = tt-oe-ordl.price
         opUom   = tt-oe-ordl.pr-uom.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
