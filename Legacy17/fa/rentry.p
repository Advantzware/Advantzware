/* fa/rentry.p - Print Entries Created */
/**************** Copyright Statement Follows ***************************
* (C) Copyright, 1996 - 1997 Foresight Software, Inc.  All Rights       *
* Reserved.  This is unpublished material and contains trade secrets    *
* and other confidential information and is subject to licensing and    *
* a confidentiality agreement.  The unauthorized possession, use,       *
* reproduction, reverse engineering, distribution, display, or          *
* disclosure of this material or of any information contained herein    *
* or any information derived from this material is strictly prohibited. *
************************************************************************/

define variable old-asset# like fa-mast.asset-code.
define variable wpage-number as integer.
define stream crt.
define            variable msg as character format "x(25)".
define variable pmethod-list as char format "x(4)".

{fa/rentry.y}               /* YOUR PARAMETERS INCLUDE FILE */
{fa/shared}                /* YOU MAY WANT TO CHANGE IT TO GL/STD */


if terminal > "" then input stream crt from terminal.

form header systemname
	    "Page:"                     to 117 space(4)
	    page-number  format ">>>>9"

	    functdesc                   at 1
	    "As-of date:"               to 117
	    r-date
	    string(time,"hh:mm") format "x(5)"

	    fill("-",132) format "x(132)" skip

"Entity"                AT 1
"Asset no"            AT 10
"Asset Description"     AT 20
"Tran Date"             AT 52
"Prd"                   AT 62
"Yr"                    AT 66
"GL Group"              AT 70
"Debit"                 AT 88
"Credit"                AT 103
"Rev"                   AT 112
"Source"                AT 118
SKIP
"Explanation"           AT 20
"Currency"              AT 53   /* rpm 01/95 */
"Job no"                AT 70   /* rpm 01/95 */
"Home debit"            AT 83   /* rpm 01/95 */
"Home credit"           AT 98   /* rpm 01/95 */
 SKIP

	    fill("-",132) format "x(132)" skip(1)
     with {&UGUI-RPT} page-top no-box width 132 frame top.

if pmethod = "1":U then pmethod-list = "book":U.
if pmethod = "2":U then pmethod-list = "tax1":U.
if pmethod = "3":U then pmethod-list = "tax2":U.

			   /* PAGE FOOTER */

form header skip(1) "Continued on page:" page-number + 1 format ">>>>9"
     with {&UGUI-RPT} page-bottom no-box no-labels frame footer width 132.

view frame top.
if page-size ne 0 then view frame footer.
page.

if fr-year = 0 then fr-year =  login.current-yr.
if beg-prd = 0 then beg-prd =  login.current-prd.
if to-year = 0 then to-year =  login.current-yr.
if end-prd = 0 then end-prd =  login.current-prd.

MAIN-LOOP: FOR EACH  FA-ENTRY WHERE ASSET-CODE GE BEG-CODE
   AND ASSET-CODE LE END-CODE
   AND FA-ENTRY.ORIGIN eq "AUTO":U
   AND FA-ENTRY.PRD GE BEG-PRD
  /* AND FA-ENTRY.PRD LE END-PRD */
   and fa-entry.fa-entity = logincontrol
   and fa-entry.yr ge fr-year

   and fa-entry.method = pmethod-list
   no-lock
   break by(fa-entry.asset-code)
   ON ERROR UNDO, LEAVE MAIN-LOOP
   WITH {&UGUI-RPT} FRAME BODY no-attr-space NO-BOX NO-LABELS WIDTH 255:


   if fa-entry.yr * 100 + fa-entry.prd > to-year * 100 + end-prd
   then next.
   if fa-entry.yr * 100 + fa-entry.prd < fr-year * 100 + beg-prd
   then next.

   {pt/newpage main-loop string(fa-entry.asset-code)}


   FIND FA-MAST OF FA-ENTRY no-lock.

   DISPLAY
   SKIP
   FA-ENTRY.entity-code    AT 1             /* rpm 01/95 */
   FA-MAST.ASSET-CODE      AT 10
   FA-MAST.ASSET-DESC      AT 20
   TRANS-DATE              AT 52
   PRD                     AT 62
   FA-ENTRY.YR mod 100 format "99" AT 66 /* YR2000 */
   FA-ENTRY.GL-CODE        AT 70
   DEBIT-AMT               AT 80
   CREDIT-AMT              AT 96
   REV                     AT 112
   ORIGIN                  AT 118
   SKIP
   FA-ENTRY.EXPLANATION    AT 20
   FA-ENTRY.CURRENCY-COD   AT 53   /* rpm 01/95 */
   FA-ENTRY.JOB-NO         AT 70   /* rpm 01/95 */
   FA-ENTRY.HM-DEBIT-AMT   AT 80   /* rpm 01/95 */
   FA-ENTRY.HM-CREDIT-AMT  AT 96   /* rpm 01/95 */
   SKIP(1) with {&UGUI-RPT}.
   ACCUM 1 (COUNT).


END.                       /* END OF MAIN-LOOP */

do transaction:

view frame top.
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
end.
