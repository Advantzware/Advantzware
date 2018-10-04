/* fa/rcyadd.p - Current Year Addition */
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
define            variable acc-dep# as decimal format "->>,>>>,>>9.99"
					       extent 3.
define            variable loop as integer.


{fa/rcyadd.y}                /* YOUR PARAMETERS INCLUDE FILE */
{fa/std}                /* YOU MAY WANT TO CHANGE IT TO GL/STD */
{fa/ctrlwrk}                 /* create fa-control workfile */

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
"New Used"              AT 54 /* YR2000 */
"Salvage"               AT 67
"Life-Bk"               AT 79
"Cost Book"             AT 93
"Dep-Basis-Bk"          AT 105
" Cy Dep Book"        AT 120
SKIP
"Code"                  AT 1
"GL Group"              AT 10
"Loc"                   AT 19
"Yr/Depr"               at 23
"Service"               AT 42
"Status"                AT 54 /* YR2000 */
"Entity"                AT 67   /* mod-sh 11/91 */
/* "SEC 179"               AT 67*/
"Life-T1"               AT 79
""                      at 92
"Dep-Basis-T1"          AT 105
" Cy Dep Tax1"        AT 120
SKIP
"Book  Tax1    Tax2"    AT 10
"P.O. No."              AT 42
"ITC Amt"               AT 67
"Life-T2"               AT 79
"Dep-Basis-T2"          AT 105
" CY Dep Tax2"          AT 120
SKIP                              /* rpm 01/95 */
"Adjustment"            at 67     /* rpm 01/95 */

	    fill("-",132) format "x(132)" skip(1)

     with {&UGUI-RPT} page-top no-box width 132 frame top.

			   /* PAGE FOOTER */

form header skip(1) "Continued on page:" page-number + 1 format ">>>>9"
     with {&UGUI-RPT} page-bottom no-box no-labels frame footer width 132.

view frame top.
if page-size ne 0 then view frame footer.
page.

if end-prd >= 13 then end-prd = fa-control.number-prd.


MAIN-LOOP:
  FOR EACH fa-mast WHERE
    fa-mast.fa-entity = fa-control.fa-entity    /* rpm 01/95 */
    AND fa-mast.ASSET-CODE GE BEG-CODE
    AND fa-mast.ASSET-CODE LE END-CODE
    AND fa-mast.GL-CODE GE BEG-GL
    AND fa-mast.GL-CODE LE END-GL
    AND fa-mast.location GE beg-loc
    AND fa-mast.location LE end-loc
    AND (INDEX(t-status,FA-MAST.ASSET-STATUS) NE 0)
    AND (IF SORT1 NE "" THEN CAN-DO(SORT1,SORT-CODE1) ELSE TRUE)
    AND (IF SORT2 NE "" THEN CAN-DO(SORT2,SORT-CODE2) ELSE TRUE) NO-LOCK
    BY   IF t-order = "l":U THEN location
    ELSE IF t-order = "1":U THEN sort-code1
    ELSE IF t-order = "2":U THEN sort-code2
    ELSE IF t-order = "g":U THEN gl-code
    ELSE asset-code

    ON ERROR UNDO, LEAVE MAIN-LOOP
    WITH {&UGUI-RPT} FRAME BODY NO-ATTR-SPACE NO-LABELS NO-BOX WIDTH 255: /* mod-sh 11/91*/

      if fa-control.aqui-service = yes then do:
	 if fa-mast.date-service < fa-control#.from-date[beg-prd] or
	    fa-mast.date-service > fa-control#.to-date[end-prd]
	    then next.
      end.
      if fa-control.aqui-service = no then do:
	 if fa-mast.date-aquired < fa-control#.from-date[beg-prd] or
	    fa-mast.date-aquired > fa-control#.to-date[end-prd]
	    then next.
      end.
  {pt/newpage main-loop string(fa-mast.asset-code)}

  acc-dep#[1] = fa-mast.acc-dep-book.
  acc-dep#[2] = fa-mast.acc-dep-tax1.
  acc-dep#[3] = fa-mast.acc-dep-tax2.

  if fa-mast.asset-status ne "R":U then
  do loop = 1 to fa-control.number-prd :
      acc-dep#[1] = acc-dep#[1] + fa-mast.period[loop].
      acc-dep#[2] = acc-dep#[2] + fa-mast.tax1-period[loop].
      acc-dep#[3] = acc-dep#[3] + fa-mast.tax2-period[loop].
  end.


  find location of fa-mast.
  DISPLAY
SKIP
ASSET-CODE              AT 1
ASSET-DESC              AT 10
DATE-AQUIRED            AT 42
NEW-USED                AT 54
SALVAGE                 AT 60
LIFE-BOOK               AT 81
mth-year format "M/Y"
COST-BOOK  (total)      AT 88
DEP-BASIS-BK(total)     AT 103
acc-dep#[1] (total)     AT 118
SKIP
GL-CODE                 AT 10
FA-MAST.LOCATION        AT 19
yr-of-depr              at 23
DATE-SERVICE            AT 42
ASSET-STATUS            AT 54
fa-mast.entity-code     at 67    /* rpm 01/95 */
LIFE-TAX-1              AT 83
DEP-BASIS-T1 (total)    AT 103
ACC-DEP#[2] (total)    AT 118
SKIP

FA-MAST.METHOD-BOOK     AT 10
FA-MAST.METHOD-TAX-1   AT 18
FA-MAST.METHOD-TAX-2   AT 25
PURCH-ORDER#            AT 42
ITC-AMT                 AT 60
LIFE-TAX-2              AT 83
DEP-BASIS-T2 (total)    AT 103
ACC-DEP#[3] (total)    AT 118
SKIP
fa-mast.sec-179         at 60         /* rpm 01/95 */
SKIP(1) with {&UGUI-RPT} frame body.
  ACCUM 1 (COUNT).

END.                       /* END OF MAIN-LOOP */

			   /* PRINT REPORT TOTALS IF APPLICABLE */

if wpage-number eq 0 then
   display " " with {&UGUI-RPT} no-labels no-box frame last-frame.

form header skip(1) "End-of-Report." accum count 1 format ">>>,>>9"
     "Record(s) Printed."
     if r-memo > "" then " Memo: " + r-memo else "" format "x(42)"
     with {&UGUI-RPT} page-bottom no-labels no-box frame last-footer width 132.

hide frame footer.
view frame last-footer.
page.
