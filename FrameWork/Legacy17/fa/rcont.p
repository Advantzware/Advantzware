/* fa/rcont.p - Print Fixed Assets Control File                                */
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
{fa/shared}
{fa/rcont.y}               /*  YOUR PARAMETERS INCLUDE FILE */

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
"Description"           at 10
"Year"                  AT 53
"Prd"                   AT 63
"Calc"                  at 70
"Acquired/"             at 80
"Allow"                 at 90
"Clearing"              at 100
"Disposal"              at 110
"Currency"              at 120

skip
"Mode"                  at 70
"Service"               at 80
"Tags"                  at 90
"Account"               at 100  /* rpm 01/95 */
"Account"               at 110  /* rpm 01/95 */


	    fill("-",132) format "x(132)" skip(1)

     with {&UGUI-RPT} page-top no-box width 132 frame top.

			   /* PAGE FOOTER */

form header skip(1) "Continued on page:" page-number + 1 format ">>>>9"
     with {&UGUI-RPT} page-bottom no-box no-labels frame footer width 132.

view frame top.
if page-size ne 0 then view frame footer.
page.

MAIN-LOOP:
for each fa-control where fa-control.fa-entity ge starting
		      and fa-control.fa-entity le ending no-lock
	    with {&UGUI-RPT} no-box width 132 frame main-loop down no-attr-space no-labels:

  {pt/newpage main-loop string(fa-control.fa-entity)}

  DISPLAY
SKIP
fa-control.fa-ENTITY                 AT 1
entity-name              at 10
YR                       AT 55
PRD                      AT 63
calc-mode                at 70
aqui-service             at 80
tag-update               at 90
fa-gl-clear              at 100
fa-gl-disp               at 110
currency-cod             at 120
skip(1) with {&UGUI-RPT}.
ACCUM 1 (COUNT).

END.                       /* END OF MAIN-LOOP */

if wpage-number eq 0 then
   display " " with {&UGUI-RPT} no-labels no-box frame last-frame.

form header skip(1) "End-of-Report." accum count 1 format ">>>,>>9"
     "Record(s) Printed."
     if r-memo > "" then " Memo: " + r-memo else "" format "x(42)"
     with {&UGUI-RPT} page-bottom no-labels no-box frame last-footer width 132.

hide frame footer.
view frame last-footer.
page.
