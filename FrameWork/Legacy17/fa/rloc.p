/* fa/rloc.p - Location list */
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
{fa/rloc.y}                /* YOUR PARAMETERS INCLUDE FILE */
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

"Location"               AT 1
"Description"            AT 10
"Room"                   AT 42
"Building"               AT 50
"Wrk-Cntr"               AT 61
"City"                   AT 72
"St/Pr"               AT 93
"Country"                AT 100
"Entity"                 TO 127

	    fill("-",132) format "x(132)" skip(1)

     with {&UGUI-RPT} page-top no-box width 132 frame top.

			   /* PAGE FOOTER */

form header skip(1) "Continued on page:" page-number + 1 format ">>>>9"
     with {&UGUI-RPT} page-bottom no-box no-labels frame footer width 132.

view frame top.
if page-size ne 0 then view frame footer.
page.
MAIN-LOOP: FOR EACH  LOCATION
	   WHERE location.location ge beg-location
	   and location.location le end-location no-lock
	       ON ERROR UNDO, LEAVE MAIN-LOOP
	       WITH {&UGUI-RPT} FRAME BODY no-attr-space NO-LABELS NO-BOX WIDTH 255:


  {pt/newpage main-loop string(location.location)}
DISPLAY
SKIP
LOCATION                AT 1
location.DESCRIPTION             AT 10
ROOM                    AT 42
BUILDING                AT 50
WORK-CENTER             AT 61
CITY                    AT 72
STATE                   AT 93
COUNTY                  AT 100
location.ENTITY         TO 129
sKIP(1) with {&UGUI-RPT}.
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
