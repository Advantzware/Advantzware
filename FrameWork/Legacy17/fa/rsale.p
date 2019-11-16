/* fa/rsale.p - Current Year Disposals */
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
DEFINE  VARIABLE BOOK-VAL-BOOK like fa-mast.cost-book.
DEFINE  VARIABLE BOOK-VAL-TX1 like fa-mast.cost-book.
DEFINE  VARIABLE BOOK-VAL-TX2 like fa-mast.cost-book.

{fa/rsale.y}                /* YOUR PARAMETERS INCLUDE FILE */
{fa/std}                /* YOU MAY WANT TO CHANGE IT TO GL/STD */
{fa/ctrlwrk}                /* create fa-control workfile */

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
"Salvage"               AT 49
"Life-Bk"               AT 60
"Cost Book"             AT 77
"Book Val - Bk"         AT 88
"Proceeds"              AT 108
"Profit Book"           AT 120
SKIP
"Code"                  AT 1
"GL Group"              AT 10
"Location"              AT 25
"Adjustment"            AT 49  /* rpm 01/95 */
"Life-T1"               AT 60
"Book Val -Tx1"         AT 88
"Cred. Recap"           at 105
"Profit Tax1"           AT 120
SKIP
"ITC Amt"               AT 49
"Life-T2"               AT 60
"Book Val -Tx2"         AT 88
"Profit Tax2"           AT 120
SKIP
"Entity"                AT 10  /* rpm 01/95 */
"Retire Code"           at 25  /* rpm 01/95 */

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

  FOR EACH fa-mast WHERE fa-mast.asset-status = "R":U
    AND fa-mast.fa-entity = fa-control.fa-entity
    AND fa-mast.ASSET-CODE GE BEG-CODE
    AND fa-mast.ASSET-CODE LE END-CODE
    AND fa-mast.GL-CODE GE BEG-GL
    AND fa-mast.GL-CODE LE END-GL
    AND fa-mast.location GE beg-loc
    AND fa-mast.location LE end-loc
    AND (INDEX(t-status,FA-MAST.ASSET-STATUS) NE 0)
    AND (IF SORT1 NE "" THEN CAN-DO(SORT1,SORT-CODE1) ELSE TRUE)
    AND (IF SORT2 NE "" THEN CAN-DO(SORT2,SORT-CODE2) ELSE TRUE)
    AND  fa-mast.date-retired ge fa-control#.from-date[beg-prd]
    AND  fa-mast.date-retired le fa-control#.to-date[end-prd] NO-LOCK
    BY   IF t-order = "l":U THEN location
    ELSE IF t-order = "1":U THEN sort-code1
    ELSE IF t-order = "2":U THEN sort-code2
    ELSE IF t-order = "g":U THEN gl-code
    ELSE asset-code

    ON ERROR UNDO, LEAVE MAIN-LOOP
    WITH {&UGUI-RPT} FRAME BODY NO-ATTR-SPACE NO-LABELS NO-BOX WIDTH 255: /* mod-sh 11/91*/

  {pt/newpage main-loop string(fa-mast.asset-code)}



  find location of fa-mast.
BOOK-VAL-BOOK = COST-BOOK  -  ACC-DEP-BOOK.
BOOK-VAL-TX1 = COST-BOOK   -  ACC-DEP-TAX1.
BOOK-VAL-TX2 = COST-BOOK   -  ACC-DEP-TAX2.

DISPLAY
SKIP
ASSET-CODE              AT 1
ASSET-DESC              AT 10
SALVAGE                 AT 42
LIFE-BOOK               AT 62
mth-year format "Mths/Yrs"
COST-BOOK (total)       AT 72
BOOK-VAL-BOOK (total)   AT 87
PROCEEDS  (total)       AT 102
PROFIT-BK (total)       AT 117

SKIP
GL-CODE                 AT 10
FA-MAST.LOCATION        AT 25
fa-mast.sec-179         AT 42         /* rpm 01/95 */
LIFE-TAX-1              AT 64
COST-BOOK   (total)     AT 72
BOOK-VAL-TX1 (total)    AT 87
itc-recap  (total)      at 105        /* rpm 01/95 */
PROFIT-T1   (total)     AT 117
SKIP
LOCATION.DESCRIPTION    AT 10
ITC-AMT                 AT 42
LIFE-TAX-2              AT 64
COST-BOOK    (total)    AT 72
BOOK-VAL-TX2 (total)    AT 87
PROFIT-T2    (total)    AT 117
SKIP
fa-mast.entity-code     at 10         /* rpm 01/95 */
fa-mast.ret-code        at 25         /* rpm 01/95 */
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
