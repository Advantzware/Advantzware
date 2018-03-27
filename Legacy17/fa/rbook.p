/* fa/rbook.p - Asset List With Book Value */
/**************** Copyright Statement Follows ***************************
* (C) Copyright, 1996 - 1997 Foresight Software, Inc.  All Rights       *
* Reserved.  This is unpublished material and contains trade secrets    *
* and other confidential information and is subject to licensing and    *
* a confidentiality agreement.  The unauthorized possession, use,       *
* reproduction, reverse engineering, distribution, display, or          *
* disclosure of this material or of any information contained herein    *
* or any information derived from this material is strictly prohibited. *
************************************************************************/

define            variable wpage-number as integer.
define            variable msg as character format "x(25)".
define            stream   crt.
DEFINE VARIABLE BOOK-VAL-BOOK AS DECIMAL FORMAT "->>,>>>,>>9.99".
DEFINE VARIABLE BOOK-VAL-TX1 AS DECIMAL FORMAT "->>,>>>,>>9.99".
DEFINE VARIABLE BOOK-VAL-TX2 AS DECIMAL FORMAT "->>,>>>,>>9.99".

define variable acc-book like book-val-book.
define variable acc-tax1 like book-val-book.
define variable acc-tax2 like book-val-book.

define variable t-book like book-val-book.
define variable t-tax1 like book-val-book.
define variable t-tax2 like book-val-book.

define variable t-ac-book like book-val-book.
define variable t-ac-tax1 like book-val-book.
define variable t-ac-tax2 like book-val-book.
define variable ok as logical.


define variable cst-book like book-val-book.

Define buffer fa-mast# for fa-mast.
define variable liste# as character format "x(5)".

{fa/rbook.y}                /* YOUR PARAMETERS INCLUDE FILE */
{fa/shared}                /* YOU MAY WANT TO CHANGE IT TO GL/STD */


if by-list = "P":U then liste# = "no":U.
if by-list = "C":U then liste# = "yes":U.
if by-list = "B":U then liste# = "yesno":U.
if by-list = "R":U then liste# = "no":U.
if by-list = "O":U then liste# = "no":U.

if terminal > "" then input stream crt from terminal.

form header systemname
	    "Page:"                     to 117 space(4)
	    page-number  format ">>>>9"

	    functdesc                   at 1
	    "As-of date:"               to 117
	    r-date
	    string(time,"hh:mm") format "x(5)"

	    fill("-",132) format "x(132)"

"Asset"                 AT 1
"Asset Description"     AT 10
"Acquired"              AT 42
"New Use"              AT 54 /* YR2000 */
/*"SALVAGE"               AT 67*/
"Sort Codes 1"          at 63
"Life-Bk"               AT 77
"Cost Book"             AT 91
"Acc. Dep Bk"           AT 104
"Book Val -Bk"        AT 118
SKIP
"Ch/Par"                  AT 1
"Group GL"              at 10
"Location"              AT 25
"Service"               AT 42
"Status"                AT 54 /* YR2000 */
/*"SEC 179"               AT 67*/
"Sort Codes 2"          at 63
"Life-T1"               AT 77
"ITC Amt"               AT 91    /* rpm 01/95 */
"Acc. Dep T1"           AT 104
"Book Val -Tx1"       AT 118

SKIP
"Parent"                at 1
"Dep.Method Bk T1 T2"   AT 10
"P.O. No."              AT 42
"Entity"                AT 54 /* YR2000 */   /* mod-rg 4/92 */
"Life-T2"               AT 77
"Salvage"               at 91   /* rpm 01/95 */
"Acc. Dep T2"           AT 104
"Book Val -Tx2"         AT 118
SKIP
"Adjustment"            at 91   /* rpm 01/95 */

	    fill("-",132) format "x(132)" skip(1)

     with {&UGUI-RPT} page-top no-box width 132 frame top.

			   /* PAGE FOOTER */

form header skip(1) "Continued on page:" page-number + 1 format ">>>>9"
     with {&UGUI-RPT} page-bottom no-box no-labels frame footer width 132.

view frame top.
if page-size ne 0 then view frame footer.
page.
MAIN-LOOP:
FOR EACH FA-MAST WHERE fa-mast.ASSET-CODE GE BEG-CODE
    AND fa-mast.ASSET-CODE LE END-CODE
    and fa-mast.fa-entity = fa-control.fa-entity    /* rpm 01/95 */
    AND FA-MAST.GL-CODE GE BEG-GL
    AND FA-MAST.GL-CODE LE END-GL
    AND FA-MAST.LOCATION GE BEG-LOC
    AND FA-MAST.LOCATION LE END-LOC
    AND (IF SORT1 NE "" THEN CAN-DO(SORT1,FA-MAST.SORT-CODE1) ELSE TRUE)
    AND (IF SORT2 NE "" THEN CAN-DO(SORT2,FA-MAST.SORT-CODE2) ELSE TRUE)
    AND (INDEX(t-status,FA-MAST.ASSET-STATUS) NE 0)
    and (index(liste#,string(fa-mast.child-par)) ne 0) no-lock
    BY IF t-order = "l":U THEN fa-mast.location
    ELSE IF t-order = "1":U THEN fa-mast.sort-code1
    ELSE IF t-order = "2":U THEN fa-mast.sort-code2
    ELSE IF t-order = "g":U THEN fa-mast.gl-code
    ELSE fa-mast.asset-code
    ON ERROR UNDO, LEAVE MAIN-LOOP WITH {&UGUI-RPT} FRAME BODY
    NO-ATTR-SPACE NO-LABELS NO-BOX WIDTH 255:  /* mod-sh 11/91 */


  if by-list = "O":U then do:
     find first fa-mast# where fa-mast#.par-asset = fa-mast.asset-code
			   and fa-mast#.fa-entity = logincontrol
			   no-lock no-error.
     if not available fa-mast# then next.
  end.

  {pt/newpage main-loop string(fa-mast.asset-code)}

  find location of fa-mast.
BOOK-VAL-BOOK = 0.
BOOK-VAL-TX1 = 0.
BOOK-VAL-TX2 = 0.

if fa-mast.asset-status = "R":U then acc-book = fa-mast.acc-dep-book.
   else acc-book = fa-mast.acc-dep-book + fa-mast.cy-dep-book.
if fa-mast.asset-status = "R":U then acc-tax1 = fa-mast.acc-dep-tax1.
   else acc-tax1 = fa-mast.acc-dep-tax1 + fa-mast.cy-dep-tax-1.
if fa-mast.asset-status = "R":U then acc-tax2 = fa-mast.acc-dep-tax2.
   else acc-tax2 = fa-mast.acc-dep-tax2 + fa-mast.cy-dep-tax-2.

BOOK-VAL-BOOK = fa-mast.COST-BOOK  -  ACC-BOOK .
if fa-mast.method-tax-1 ne " ":U then
   BOOK-VAL-TX1 = fa-mast.COST-BOOK  -  ACC-TAX1.
if fa-mast.method-tax-2 ne " ":U then
   BOOK-VAL-TX2 = fa-mast.COST-BOOK  -  ACC-TAX2.

if fa-mast.life-book eq 0 then book-val-book = 0.
if fa-mast.life-tax-1 eq 0 then book-val-tx1 = 0.
if fa-mast.life-tax-2 eq 0 then book-val-tx2 = 0.

t-book = book-val-book.
t-tax1 = book-val-tx1.
t-tax2 = book-val-tx2.

t-ac-book = acc-book.
t-ac-tax1 = acc-tax1.
t-ac-tax2 = acc-tax2.

DISPLAY
SKIP
fa-mast.ASSET-CODE              AT 1
fa-mast.ASSET-DESC              AT 10
fa-mast.DATE-AQUIRED            AT 42
fa-mast.NEW-USED                AT 54
fa-mast.sort-code1              at 63
fa-mast.LIFE-BOOK               AT 78
fa-mast.mth-year format "M/Y"
fa-mast.COST-BOOK        AT 86
ACC-BOOK     AT 101
BOOK-VAL-BOOK    AT 116
SKIP
fa-mast.child-par               at 1
fa-mast.GL-CODE                 AT 10
FA-MAST.LOCATION        AT 25
fa-mast.DATE-SERVICE            AT 42
fa-mast.ASSET-STATUS            AT 54
fa-mast.Sort-code2              at 63
fa-mast.LIFE-TAX-1              AT 80
fa-mast.ITC-AMT                 AT 86   /* rpm 01/95 */
ACC-TAX1         AT 101
BOOK-VAL-TX1     AT 116
SKIP
fa-mast.par-asset  when fa-mast.child-par = yes at 1
FA-MAST.METHOD-BOOK       AT 10
FA-MAST.METHOD-TAX-1      AT 18
FA-MAST.METHOD-TAX-2      AT 25
fa-mast.PURCH-ORDER#      AT 42
fa-mast.entity-code       at 54
fa-mast.LIFE-TAX-2        AT 80
fa-mast.salvage           at 86          /* rpm 01/95 */
ACC-TAX2                  AT 101
BOOK-VAL-TX2              AT 116
SKIP
fa-mast.sec-179           at 86          /* rpm 01/95 */

SKIP(1) with {&UGUI-RPT}.
  ACCUM 1 (COUNT).

  accum acc-book (total).
  accum acc-tax1 (total).
  accum acc-tax2 (total).
  accum book-val-book (total).
  accum book-val-tx1 (total).
  accum book-val-tx2 (total).
  cst-book = fa-mast.cost-book.
  accum cst-book (total).


if by-list = "R":U or by-list = "O":U then do:
ok = yes.

FOR EACH FA-MAST# WHERE fa-mast#.par-asset =  fa-mast.ASSET-CODE
    and fa-mast#.fa-entity = fa-control.fa-entity   /* rpm 01/95 */
    AND FA-MAST#.GL-CODE GE BEG-GL
    AND FA-MAST#.GL-CODE LE END-GL
    AND FA-MAST#.LOCATION GE BEG-LOC
    AND FA-MAST#.LOCATION LE END-LOC
    AND (IF SORT1 NE "" THEN CAN-DO(SORT1,FA-MAST#.SORT-CODE1) ELSE TRUE)
    AND (IF SORT2 NE "" THEN CAN-DO(SORT2,FA-MAST#.SORT-CODE2) ELSE TRUE)
    AND (INDEX(t-status,FA-MAST#.ASSET-STATUS) NE 0) no-lock
    BY IF t-order = "l":U THEN fa-mast#.location
    ELSE IF t-order = "1":U THEN fa-mast#.sort-code1
    ELSE IF t-order = "2":U THEN fa-mast#.sort-code2
    ELSE IF t-order = "g":U THEN fa-mast#.gl-code
    ELSE fa-mast#.asset-code
    ON ERROR UNDO, LEAVE MAIN-LOOP WITH {&UGUI-RPT} FRAME BODY11
    NO-ATTR-SPACE NO-LABELS NO-BOX WIDTH 255:



  find location of fa-mast#.
BOOK-VAL-BOOK = 0.
BOOK-VAL-TX1 = 0.
BOOK-VAL-TX2 = 0.

if fa-mast#.asset-status = "R":U then acc-book = fa-mast#.acc-dep-book.
   else acc-book = fa-mast#.acc-dep-book + fa-mast#.cy-dep-book.
if fa-mast#.asset-status = "R":U then acc-tax1 = fa-mast#.acc-dep-tax1.
   else acc-tax1 = fa-mast#.acc-dep-tax1 + fa-mast#.cy-dep-tax-1.
if fa-mast#.asset-status = "R":U then acc-tax2 = fa-mast#.acc-dep-tax2.
   else acc-tax2 = fa-mast#.acc-dep-tax2 + fa-mast#.cy-dep-tax-2.

BOOK-VAL-BOOK = fa-mast#.COST-BOOK  -  ACC-BOOK .
if fa-mast#.method-tax-1 ne " ":U then
   BOOK-VAL-TX1 = fa-mast#.COST-BOOK  -  ACC-TAX1.
if fa-mast#.method-tax-2 ne " ":U then
   BOOK-VAL-TX2 = fa-mast#.COST-BOOK  -  ACC-TAX2.

if fa-mast#.life-book eq 0 then book-val-book = 0.
if fa-mast#.life-tax-1 eq 0 then book-val-tx1 = 0.
if fa-mast#.life-tax-2 eq 0 then book-val-tx2 = 0.


t-book = t-book + book-val-book.
t-tax1 = t-tax1 + book-val-tx1.
t-tax2 = t-tax2 + book-val-tx2.

t-ac-book = t-ac-book + acc-book.
t-ac-tax1 = t-ac-tax1 + acc-tax1.
t-ac-tax2 = t-ac-tax2 + acc-tax2.

DISPLAY
SKIP
fa-mast#.ASSET-CODE              AT 1
fa-mast#.ASSET-DESC              AT 10
fa-mast#.DATE-AQUIRED            AT 42
fa-mast#.NEW-USED                AT 54
/*SALVAGE                 AT 60*/
fa-mast#.sort-code1              at 63
fa-mast#.LIFE-BOOK               AT 78
fa-mast#.mth-year format "M/Y"
fa-mast#.COST-BOOK        AT 86
ACC-BOOK    AT 101
BOOK-VAL-BOOK    AT 116
SKIP
fa-mast#.child-par               at 1
fa-mast#.GL-CODE                 AT 10
FA-MAST#.LOCATION        AT 25
fa-mast#.DATE-SERVICE            AT 42
fa-mast#.ASSET-STATUS            AT 54
fa-mast#.Sort-code2              at 63
fa-mast#.LIFE-TAX-1              AT 80
ACC-TAX1         AT 101
BOOK-VAL-TX1     AT 116
SKIP

fa-mast#.par-asset when fa-mast#.child-par = yes at 1
FA-MAST#.METHOD-BOOK       AT 10
FA-MAST#.METHOD-TAX-1      AT 18
FA-MAST#.METHOD-TAX-2      AT 25
fa-mast#.PURCH-ORDER#      AT 42
fa-mast#.entity-code       at 54       /* rpm 01/95 */
fa-mast#.ITC-AMT           AT 62
fa-mast#.LIFE-TAX-2        AT 80
ACC-TAX2                   AT 101
BOOK-VAL-TX2               AT 116
SKIP
fa-mast#.salvage           at 10       /* rpm 01/95 */
fa-mast#.sec-179           at 42       /* rpm 01/95 */

SKIP(1) with {&UGUI-RPT} no-box no-labels frame eeee width 255.
  ACCUM 1 (COUNT).

  accum acc-book (total).
  accum acc-tax1 (total).
  accum acc-tax2 (total).
  accum book-val-book (total).
  accum book-val-tx1 (total).
  accum book-val-tx2 (total).
  cst-book = fa-mast#.cost-book.
  accum cst-book (total).
ok = yes.
end.  /* of for each fa-mast# */

if ok then
display fill("-",30) format "x(30)" at 101 skip
"Total Parent & Children :" to 95
t-ac-book at 101 t-book at 116 skip
t-ac-tax1 at 101 t-tax1 at 116 skip
t-ac-tax2 at 101 t-tax2 at 116 skip
fill("-",30) format "x(30)" at 101 skip(1)
	with {&UGUI-RPT} no-box no-labels width 230 frame tot-book.
end.  /* of if "R" or "O" */

END.                       /* END OF MAIN-LOOP */

			   /* PRINT REPORT TOTALS IF APPLICABLE */
display skip(1)
	fill("-",45) format "x(45)" at 86 skip
	 accum total cst-book format ">>>,>>>,>>9.99"  at 86
	accum total acc-book format ">>>,>>>,>>9.99" at 101
	accum total book-val-book format ">>>,>>>,>>9.99" at 116 skip
	accum total acc-tax1 format ">>>,>>>,>>9.99" at 101
	accum total book-val-tx1 format ">>>,>>>,>>9.99" at 116 skip
	accum total acc-tax2 format ">>>,>>>,>>9.99" at 101
	accum total book-val-tx2 format ">>>,>>>,>>9.99" at 116 skip
	with {&UGUI-RPT} no-box no-labels frame total1 width 255.


if wpage-number eq 0 then
   display " " with {&UGUI-RPT} no-labels no-box frame last-frame.

form header skip(1) "End-of-Report." accum count 1 format ">>>,>>9"
     "Record(s) Printed."
     if r-memo > "" then " Memo: " + r-memo else "" format "x(42)"
     with {&UGUI-RPT} page-bottom no-labels no-box frame last-footer width 132.

hide frame footer.
view frame last-footer.
page.
