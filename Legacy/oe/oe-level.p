/* -------------------------------------------------- oe/oe-level.p 3/96 fwk  */
/*                                                                            */
/* cust level - what level is pricing at                                      */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

define new shared buffer tmp-oe-ordl for oe-ordl.
DEFINE BUFFER bf-oe-prmtx FOR oe-prmtx.

define shared buffer xoe-ord for oe-ord. /* BUFFER WITH ORDER HEADER */
define shared var v-i-qty like oe-ordl.qty no-undo. /* INPUT QUANTITY */
define shared var v-price-lev as int no-undo.
define shared var v-procat like oe-prmtx.procat no-undo. /* ITEM CATEGORY */

define var v-custype like oe-prmtx.custype no-undo. /* CUSTOMER TYPE */
DEF VAR lv-date AS CHAR NO-UNDO.
DEFINE VARIABLE lMatrixExists AS LOGICAL     NO-UNDO.


lv-date = STRING(YEAR(TODAY),"9999") +
          STRING(MONTH(TODAY),"99")  +
          STRING(DAY(TODAY),"99").

for each tmp-oe-ordl where tmp-oe-ordl.ord-no = xoe-ord.ord-no and
			   tmp-oe-ordl.company = xoe-ord.company no-lock:
       assign v-i-qty = v-i-qty + tmp-oe-ordl.qty.
end. 
find first cust {sys/ref/custW.i} and cust.cust-no = xoe-ord.cust-no
	   use-index cust no-lock no-error.
if avail cust then v-custype = cust.type. else next.

find first tmp-oe-ordl where tmp-oe-ordl.ord-no = xoe-ord.ord-no and
			  tmp-oe-ordl.company = xoe-ord.company 
		 	  no-lock no-error.

find first itemfg where itemfg.company = tmp-oe-ordl.company and
			itemfg.i-no = tmp-oe-ordl.i-no no-lock no-error.
if avail itemfg then v-procat = itemfg.procat. else next.

/*for first oe-prmtx no-lock
       where oe-prmtx.company = tmp-oe-ordl.company
         and (oe-prmtx.cust-no = xoe-ord.cust-no and oe-prmtx.custype = v-custype and
       oe-prmtx.i-no = tmp-oe-ordl.i-no and oe-prmtx.procat = v-procat) or
     (oe-prmtx.cust-no = xoe-ord.cust-no and oe-prmtx.custype = v-custype and
       oe-prmtx.i-no = "" and oe-prmtx.procat = "") or
     (oe-prmtx.cust-no = "" and oe-prmtx.custype = v-custype and
       oe-prmtx.i-no = "" and oe-prmtx.procat = v-procat) or
     (oe-prmtx.cust-no = "" and oe-prmtx.custype = v-custype and
       oe-prmtx.i-no = "" and oe-prmtx.procat = "") or
     (oe-prmtx.cust-no = "" and oe-prmtx.custype = "" and
       oe-prmtx.i-no = tmp-oe-ordl.i-no and oe-prmtx.procat = v-procat) or
     (oe-prmtx.cust-no = "" and oe-prmtx.custype = "" and
       oe-prmtx.i-no = "" and oe-prmtx.procat = v-procat):*/

/* RELEASE oe-prmtx.                                             */
/*                                                               */
/* FOR EACH oe-prmtx NO-LOCK                                     */
/*     WHERE oe-prmtx.company            EQ tmp-oe-ordl.company  */
/*       AND oe-prmtx.custype            EQ v-custype            */
/*       AND oe-prmtx.cust-no            EQ xoe-ord.cust-no      */
/*       AND oe-prmtx.procat             EQ v-procat             */
/*       AND oe-prmtx.i-no               BEGINS tmp-oe-ordl.i-no */
/*       AND SUBSTR(oe-prmtx.i-no,1,100) EQ tmp-oe-ordl.i-no     */
/*       AND SUBSTR(oe-prmtx.i-no,101,8) LE lv-date              */
/*     BY SUBSTR(oe-prmtx.i-no,101,8) DESC:                      */
/*   LEAVE.                                                      */
/* END.                                                          */
/*                                                               */
/* IF NOT AVAIL oe-prmtx THEN                                    */
/* FOR EACH oe-prmtx NO-LOCK                                     */
/*     WHERE oe-prmtx.company            EQ tmp-oe-ordl.company  */
/*       AND oe-prmtx.custype            EQ v-custype            */
/*       AND oe-prmtx.cust-no            EQ ""                   */
/*       AND oe-prmtx.procat             EQ v-procat             */
/*       AND oe-prmtx.i-no               BEGINS tmp-oe-ordl.i-no */
/*       AND SUBSTR(oe-prmtx.i-no,1,100) EQ tmp-oe-ordl.i-no     */
/*       AND SUBSTR(oe-prmtx.i-no,101,8) LE lv-date              */
/*     BY SUBSTR(oe-prmtx.i-no,101,8) DESC:                      */
/*   LEAVE.                                                      */
/* END.                                                          */
/*                                                               */
/* IF NOT AVAIL oe-prmtx THEN                                    */
/* FOR EACH oe-prmtx NO-LOCK                                     */
/*     WHERE oe-prmtx.company            EQ tmp-oe-ordl.company  */
/*       AND oe-prmtx.custype            EQ v-custype            */
/*       AND oe-prmtx.cust-no            EQ ""                   */
/*       AND oe-prmtx.procat             EQ v-procat             */
/*       AND SUBSTR(oe-prmtx.i-no,1,100) EQ ""                   */
/*       AND SUBSTR(oe-prmtx.i-no,101,8) LE lv-date              */
/*     BY SUBSTR(oe-prmtx.i-no,101,8) DESC:                      */
/*   LEAVE.                                                      */
/* END.                                                          */
/*                                                               */
/* IF NOT AVAIL oe-prmtx THEN                                    */
/* FOR EACH oe-prmtx NO-LOCK                                     */
/*     WHERE oe-prmtx.company            EQ tmp-oe-ordl.company  */
/*       AND oe-prmtx.custype            EQ ""                   */
/*       AND oe-prmtx.cust-no            EQ ""                   */
/*       AND oe-prmtx.procat             EQ v-procat             */
/*       AND oe-prmtx.i-no               BEGINS tmp-oe-ordl.i-no */
/*       AND SUBSTR(oe-prmtx.i-no,1,100) EQ tmp-oe-ordl.i-no     */
/*       AND SUBSTR(oe-prmtx.i-no,101,8) LE lv-date              */
/*     BY SUBSTR(oe-prmtx.i-no,101,8) DESC:                      */
/*   LEAVE.                                                      */
/* END.                                                          */
/*                                                               */
/* IF NOT AVAIL oe-prmtx THEN                                    */
/* FOR EACH oe-prmtx NO-LOCK                                     */
/*     WHERE oe-prmtx.company            EQ tmp-oe-ordl.company  */
/*       AND oe-prmtx.custype            EQ ""                   */
/*       AND oe-prmtx.cust-no            EQ ""                   */
/*       AND oe-prmtx.procat             EQ ""                   */
/*       AND oe-prmtx.i-no               BEGINS tmp-oe-ordl.i-no */
/*       AND SUBSTR(oe-prmtx.i-no,1,100) EQ tmp-oe-ordl.i-no     */
/*       AND SUBSTR(oe-prmtx.i-no,101,8) LE lv-date              */
/*     BY SUBSTR(oe-prmtx.i-no,101,8) DESC:                      */
/*   LEAVE.                                                      */
/* END.                                                          */
/*                                                               */
/* IF NOT AVAIL oe-prmtx THEN                                    */
/* FOR EACH oe-prmtx NO-LOCK                                     */
/*     WHERE oe-prmtx.company            EQ tmp-oe-ordl.company  */
/*       AND oe-prmtx.custype            EQ ""                   */
/*       AND oe-prmtx.cust-no            EQ ""                   */
/*       AND oe-prmtx.procat             EQ v-procat             */
/*       AND SUBSTR(oe-prmtx.i-no,1,100) EQ ""                   */
/*       AND SUBSTR(oe-prmtx.i-no,101,8) LE lv-date              */
/*     BY SUBSTR(oe-prmtx.i-no,101,8) DESC:                      */
/*   LEAVE.                                                      */
/* END.                                                          */
RUN oe/GetPriceMatrix.p (BUFFER bf-oe-prmtx,
                         INPUT ROWID(itemfg),
                         INPUT ROWID(cust),
                         INPUT NO,
                         OUTPUT lMatrixExists).

IF AVAIL bf-oe-prmtx THEN
DO i = 1 TO 10:
  IF v-i-qty LE bf-oe-prmtx.qty[i] OR bf-oe-prmtx.qty[i] = 0 THEN DO:
      IF bf-oe-prmtx.qty[i] EQ 0 THEN v-price-lev = i - 1.
                                ELSE v-price-lev = i.
    LEAVE.
  END. /* do */
  
END. /* 1 to 10 */

/* end ---------------------------------- copr. 1992  advanced software, inc. */

