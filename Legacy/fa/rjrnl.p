/* fa/rjrnl.p - List Journal Entries */
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
define variable pmethod-list as char format "x(4)".
{fa/rjrnl.y}                /* YOUR PARAMETERS INCLUDE FILE */
{fa/shared}                /* YOU MAY WANT TO CHANGE IT TO GL/STD */

if terminal > "" then input stream crt from terminal.

form header systemname
	    "Page:"                     to 117 space(4)
	    page-number  format ">>>>9"

	    functdesc                   at 1
	    "As-of date:"               to 117
	    r-date
	    string(time,"hh:mm") format "x(5)"

	    fill("-",132) format "x(132)"

"Entity"                AT 1
"Entry#"                AT 10
"Asset Code"            AT 18
"Asset Description"     AT 30
"Trans Date"            AT 61
"Prd"                   AT 72
"Yr"                    AT 76
"GL-Code"               AT 80
"Debit"                 AT 98
"Credit"                AT 113
/*"Rev"                   AT 122   */
"Origin"                at 122
SKIP
"Explanation"           AT 18   /* rpm 01/95 */
"Currency"              AT 61   /* rpm 01/95 */
"Job no"                AT 80   /* rpm 01/95 */
"Home debit"            AT 93   /* rpm 01/95 */
"Home credit"           AT 108   /* rpm 01/95 */

	    fill("-",132) format "x(132)" skip(1)

     with {&UGUI-RPT} page-top no-box width 132 frame top.

			   /* PAGE FOOTER */

form header skip(1) "Continued on page:" page-number + 1 format ">>>>9"
     with {&UGUI-RPT} page-bottom no-box no-labels frame footer width 132.

view frame top.
if page-size ne 0 then view frame footer.
page.

if pmethod = "1":U then pmethod-list = "book":U.
if pmethod = "2":U then pmethod-list = "tax1":U.
if pmethod = "3":U then pmethod-list = "tax2":U.
if pmethod = "4":U then pmethod-list = "*":U.

if fr-year = 0 then fr-year =  login.current-yr.
if beg-prd = 0 then beg-prd =  login.current-prd.
if to-year = 0 then to-year =  login.current-yr.
if end-prd = 0 then end-prd =  login.current-prd.

MAIN-LOOP: FOR EACH  FA-JRNL WHERE FA-JRNL.fa-entity EQ logincontrol AND
		FA-JRNL.METHOD matches  PMETHOD-list
	       and fa-jrnl.yr ge fr-year
	       and fa-jrnl.prd ge beg-prd
	       and fa-jrnl.asset-code ge beg-code
	       and fa-jrnl.asset-code le end-code
	       no-lock
	       ON ERROR UNDO, LEAVE MAIN-LOOP
	       WITH {&UGUI-RPT} FRAME BODY no-attr-space NO-LABELS NO-BOX WIDTH 255:

   if fa-jrnl.yr * 100 + fa-jrnl.prd > to-year * 100 + end-prd
   then next.
   if fa-jrnl.yr * 100 + fa-jrnl.prd < fr-year * 100 + beg-prd
   then next.

      find fa-mast where fa-mast.asset-code = fa-jrnl.asset-code and fa-mast.fa-entity = fa-jrnl.fa-entity no-lock.  /* rpm 01/95 */

  {pt/newpage main-loop string(fa-jrnl.entry-no)}

DISPLAY
SKIP
FA-JRNL.ENTITY-code          AT 1
fa-JRNL.entry-no       AT 10
FA-MAST.ASSET-CODE      AT 18
FA-MAST.ASSET-DESC      AT 30
TRANS-DATE              AT 62
PRD                     AT 72
FA-JRNL.YR mod 100 format "99" AT 76 /* YR2000 */
FA-JRNL.GL-CODE         AT 80
DEBIT-AMT               AT 90   (total)
CREDIT-AMT              AT 106  (total)
/* REV                     AT 122   */
fa-jrnl.method          at 122
SKIP
FA-JRNL.EXPLANATION     AT 18   /* rpm 01/95 */
FA-JRNL.CURRENCY-COD    AT 61   /* rpm 01/95 */
FA-JRNL.JOB-NO          AT 80   /* rpm 01/95 */
FA-JRNL.HM-DEBIT-AMT    AT 90  (total)  /* rpm 01/95 */
FA-JRNL.HM-CREDIT-AMT   AT 106 (total)  /* rpm 01/95 */ with {&UGUI-RPT}.
ACCUM 1 (COUNT).

END.                       /* END OF MAIN-LOOP */

			   /* PRINT REPORT TOTALS IF APPLICABLE */
hide frame footer.

if wpage-number eq 0 then
   display " " with {&UGUI-RPT} no-labels no-box frame last-frame.

form header skip(1) "End-of-Report." accum count 1 format ">>>,>>9"
     "Record(s) Printed."
     if r-memo > "" then " Memo: " + r-memo else "" format "x(42)"
     with {&UGUI-RPT} page-bottom no-labels no-box frame last-footer width 132.

hide frame footer.
view frame last-footer.
page.
