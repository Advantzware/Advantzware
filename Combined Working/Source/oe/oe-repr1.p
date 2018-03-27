/* -------------------------------------------------- oe/oe-repr1.p 3/96 fwk  */
/*                                                                            */
/* reprice option - ITEM REPRICING FROM PRICE MATRIX                          */
/*                   FOR STOCK BOXES ONLY                                     */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

define buffer bfx-ordl for oe-ordl.
DEFINE BUFFER bf-oe-prmtx FOR oe-prmtx.

define shared buffer xoe-ord for oe-ord. /* BUFFER WITH ORDER HEADER */

define shared var head as ch format "x(80)" extent 2 no-undo.
define shared var v-unline as ch format "x(78)" no-undo.
define shared var v-price-lev as int no-undo.
/*define shared var save_id as recid.  /* RECORD ID FOR ORDER LINE */ */
define shared var v-procat like oe-prmtx.procat no-undo. /* ITEM CATEGORY */

/* define var v-custype like oe-prmtx.custype no-undo. /* CUSTOMER TYPE */ */
/* define shared var v-i-item like oe-ordl.i-no no-undo. /* INPUT ITEM */  
define shared var v-i-qty like oe-ordl.qty no-undo. /* INPUT QUANTITY */   */
define var v-tot-ord as dec format "->>>,>>>,>>9.99" no-undo.
define var change-tot-ord as dec format "->>>,>>>,>>9.99" no-undo.
DEFINE VARIABLE lMatrixExists AS LOG NO-UNDO.
/* DEF VAR lv-date AS CHAR NO-UNDO.       */
/*                                        */
/*                                        */
/* lv-date = STRING(YEAR(TODAY),"9999") + */
/*           STRING(MONTH(TODAY),"99")  + */
/*           STRING(DAY(TODAY),"99").     */

find first cust {sys/ref/custW.i} and cust.cust-no = xoe-ord.cust-no
	   use-index cust no-lock no-error.
/* if avail cust then              */
/*   assign v-custype = cust.type. */
/* else                            */
/*   assign v-custype = "".        */

if not cust.auto-reprice then return.

assign change-tot-ord = 0
       change-tot-ord = (change-tot-ord - v-tot-ord)
       v-tot-ord = 0.

for each bfx-ordl where bfx-ordl.ord-no = xoe-ord.ord-no and
			   bfx-ordl.company = xoe-ord.company:

  find first itemfg where itemfg.company  = bfx-ordl.company and
                           itemfg.i-no = bfx-ordl.i-no no-lock no-error.
  if avail itemfg then assign v-procat = itemfg.procat.
  else next.

  /*for first oe-prmtx no-lock where oe-prmtx.company = bfx-ordl.company and
                                (oe-prmtx.cust-no = xoe-ord.cust-no and oe-prmtx.custype = v-custype and
                                 oe-prmtx.i-no = bfx-ordl.i-no and oe-prmtx.procat = v-procat) or
                                (oe-prmtx.cust-no = xoe-ord.cust-no and oe-prmtx.custype = v-custype and
                                 oe-prmtx.i-no = "" and oe-prmtx.procat = "") or
                                (oe-prmtx.cust-no = "" and oe-prmtx.custype = v-custype and
                                 oe-prmtx.i-no = "" and oe-prmtx.procat = v-procat) or
                                (oe-prmtx.cust-no = "" and oe-prmtx.custype = v-custype and
                                 oe-prmtx.i-no = "" and oe-prmtx.procat = "") or
                                (oe-prmtx.cust-no = "" and oe-prmtx.custype = "" and
                                 oe-prmtx.i-no = bfx-ordl.i-no and oe-prmtx.procat = v-procat) or
                                (oe-prmtx.cust-no = "" and oe-prmtx.custype = "" and
                                 oe-prmtx.i-no = "" and oe-prmtx.procat = v-procat):*/
  RELEASE bf-oe-prmtx.
  RUN oe/GetPriceMatrix.p(
      BUFFER bf-oe-prmtx,
      INPUT ROWID(itemfg),
      INPUT ROWID(cust),
      INPUT NO,
      OUTPUT lMatrixExists
      ).

/*   FOR EACH oe-prmtx NO-LOCK                                  */
/*       WHERE oe-prmtx.company            EQ bfx-ordl.company  */
/*         AND oe-prmtx.custype            EQ v-custype         */
/*         AND oe-prmtx.cust-no            EQ xoe-ord.cust-no   */
/*         AND oe-prmtx.procat             EQ v-procat          */
/*         AND oe-prmtx.i-no               BEGINS bfx-ordl.i-no */
/*         AND SUBSTR(oe-prmtx.i-no,1,100) EQ bfx-ordl.i-no     */
/*         AND SUBSTR(oe-prmtx.i-no,101,8) LE lv-date           */
/*       BY SUBSTR(oe-prmtx.i-no,101,8) DESC:                   */
/*     LEAVE.                                                   */
/*   END.                                                       */
/*                                                              */
/*   IF NOT AVAIL oe-prmtx THEN                                 */
/*   FOR EACH oe-prmtx NO-LOCK                                  */
/*       WHERE oe-prmtx.company            EQ bfx-ordl.company  */
/*         AND oe-prmtx.custype            EQ v-custype         */
/*         AND oe-prmtx.cust-no            EQ ""                */
/*         AND oe-prmtx.procat             EQ v-procat          */
/*         AND oe-prmtx.i-no               BEGINS bfx-ordl.i-no */
/*         AND SUBSTR(oe-prmtx.i-no,1,100) EQ bfx-ordl.i-no     */
/*         AND SUBSTR(oe-prmtx.i-no,101,8) LE lv-date           */
/*       BY SUBSTR(oe-prmtx.i-no,101,8) DESC:                   */
/*     LEAVE.                                                   */
/*   END.                                                       */
/*                                                              */
/*   IF NOT AVAIL oe-prmtx THEN                                 */
/*   FOR EACH oe-prmtx NO-LOCK                                  */
/*       WHERE oe-prmtx.company            EQ bfx-ordl.company  */
/*         AND oe-prmtx.custype            EQ v-custype         */
/*         AND oe-prmtx.cust-no            EQ ""                */
/*         AND oe-prmtx.procat             EQ v-procat          */
/*         AND SUBSTR(oe-prmtx.i-no,1,100) EQ ""                */
/*         AND SUBSTR(oe-prmtx.i-no,101,8) LE lv-date           */
/*       BY SUBSTR(oe-prmtx.i-no,101,8) DESC:                   */
/*     LEAVE.                                                   */
/*   END.                                                       */
/*                                                              */
/*   IF NOT AVAIL oe-prmtx THEN                                 */
/*   FOR EACH oe-prmtx NO-LOCK                                  */
/*       WHERE oe-prmtx.company            EQ bfx-ordl.company  */
/*         AND oe-prmtx.custype            EQ ""                */
/*         AND oe-prmtx.cust-no            EQ ""                */
/*         AND oe-prmtx.procat             EQ v-procat          */
/*         AND oe-prmtx.i-no               BEGINS bfx-ordl.i-no */
/*         AND SUBSTR(oe-prmtx.i-no,1,100) EQ bfx-ordl.i-no     */
/*         AND SUBSTR(oe-prmtx.i-no,101,8) LE lv-date           */
/*       BY SUBSTR(oe-prmtx.i-no,101,8) DESC:                   */
/*     LEAVE.                                                   */
/*   END.                                                       */
/*                                                              */
/*   IF NOT AVAIL oe-prmtx THEN                                 */
/*   FOR EACH oe-prmtx NO-LOCK                                  */
/*       WHERE oe-prmtx.company            EQ bfx-ordl.company  */
/*         AND oe-prmtx.custype            EQ ""                */
/*         AND oe-prmtx.cust-no            EQ ""                */
/*         AND oe-prmtx.procat             EQ ""                */
/*         AND oe-prmtx.i-no               BEGINS bfx-ordl.i-no */
/*         AND SUBSTR(oe-prmtx.i-no,1,100) EQ bfx-ordl.i-no     */
/*         AND SUBSTR(oe-prmtx.i-no,101,8) LE lv-date           */
/*       BY SUBSTR(oe-prmtx.i-no,101,8) DESC:                   */
/*     LEAVE.                                                   */
/*   END.                                                       */
/*                                                              */
/*   IF NOT AVAIL oe-prmtx THEN                                 */
/*   FOR EACH oe-prmtx NO-LOCK                                  */
/*       WHERE oe-prmtx.company            EQ bfx-ordl.company  */
/*         AND oe-prmtx.custype            EQ ""                */
/*         AND oe-prmtx.cust-no            EQ ""                */
/*         AND oe-prmtx.procat             EQ v-procat          */
/*         AND SUBSTR(oe-prmtx.i-no,1,100) EQ ""                */
/*         AND SUBSTR(oe-prmtx.i-no,101,8) LE lv-date           */
/*       BY SUBSTR(oe-prmtx.i-no,101,8) DESC:                   */
/*     LEAVE.                                                   */
/*   END.                                                       */
/*                                                              */
  IF lMatrixExists AND AVAIL bf-oe-prmtx THEN DO:
      RUN oe/GetPriceMatrixPrice.p(
        BUFFER bf-oe-prmtx,
        INPUT 0,
        INPUT v-price-lev,
        INPUT cust.cust-level,
        INPUT itemfg.sell-price,
        INPUT itemfg.sell-uom,
        OUTPUT bfx-ordl.price,
        OUTPUT bfx-ordl.pr-uom).
/*       if v-price-lev < cust.cust-level then assign i = cust.cust-level. */
/*       else assign i = v-price-lev.                                      */
/*          if oe-prmtx.meth then                                          */
/*       do:                                                               */
/*             assign bfx-ordl.price = oe-prmtx.price[i]                   */
/*                 bfx-ordl.pr-uom = oe-prmtx.uom[i].                      */
      RUN  oe/GetPriceTotal.p(
          INPUT bfx-ordl.qty,
          INPUT bfx-ordl.price,
          INPUT bfx-ordl.pr-uom,
          INPUT itemfg.case-count,
          INPUT bfx-ordl.disc,
          OUTPUT bfx-ordl.t-price
          ).

/*             if bfx-ordl.pr-uom Begins "L" AND bfx-ordl.pr-uom NE "LB" then                  */
/*                assign bfx-ordl.t-price = bfx-ordl.price -                                   */
/*                            round( (bfx-ordl.price * bfx-ordl.disc) / 100, 2).               */
/*                                                                                             */
/*             else if bfx-ordl.pr-uom = "CS" and avail itemfg and                             */
/*                  itemfg.case-count ne 0 then                                                */
/*                assign bfx-ordl.t-price = ((bfx-ordl.qty / itemfg.case-count) *              */
/*                                           bfx-ordl.price) - round((((bfx-ordl.qty /         */
/*                                           itemfg.case-count) *  bfx-ordl.price) *           */
/*                                           bfx-ordl.disc) / 100, 2).                         */
/*             else if  bfx-ordl.pr-uom = "C" then                                             */
/*              assign bfx-ordl.t-price = (( bfx-ordl.qty / 100) *                             */
/*                                       bfx-ordl.price) - round(((( bfx-ordl.qty / 100) *     */
/*                                          bfx-ordl.price) *  bfx-ordl.disc) / 100, 2).       */
/*             else if  bfx-ordl.pr-uom = "M" then                                             */
/*              assign bfx-ordl.t-price = (( bfx-ordl.qty / 1000) *                            */
/*                                      bfx-ordl.price) - round(( (( bfx-ordl.qty / 1000) *    */
/*                                      bfx-ordl.price) *  bfx-ordl.disc) / 100, 2).           */
/*             else /** DEFAULT TO EACH **/                                                    */
/*              assign bfx-ordl.t-price = (( bfx-ordl.qty /** / 1000 **/ ) * bfx-ordl.price) - */
/*                                     round(( (( bfx-ordl.qty /** / 1000 **/ ) *              */
/*                                          bfx-ordl.price) *  bfx-ordl.disc) / 100, 2).       */
/*       end.         */
/*       /* leave. */ */
  END. /* avail oe-prmtx */

  assign v-tot-ord = v-tot-ord + bfx-ordl.t-price.
end. 

assign change-tot-ord = change-tot-ord + v-tot-ord.

do:
  find first cust {sys/ref/custW.i} and cust.cust-no = xoe-ord.cust-no
       use-index cust no-error.
  if avail cust then
    assign cust.ord-bal = cust.ord-bal + change-tot-ord.
end.

/* end ---------------------------------- copr. 1992  advanced software, inc. */

