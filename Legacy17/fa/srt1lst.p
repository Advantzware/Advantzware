/* fa/srt1lst.p   List of Sort-1 Codes (from pt/report.m)
		warty 04/09/90
*/
/**************** Copyright Statement Follows ***************************
* (C) Copyright, 1996 - 1997 Foresight Software, Inc.  All Rights       *
* Reserved.  This is unpublished material and contains trade secrets    *
* and other confidential information and is subject to licensing and    *
* a confidentiality agreement.  The unauthorized possession, use,       *
* reproduction, reverse engineering, distribution, display, or          *
* disclosure of this material or of any information contained herein    *
* or any information derived from this material is strictly prohibited. *
************************************************************************/

define variable wpage-number as integer.
define stream crt.

{fa/srt1lst.y}             /* YOUR PARAMETERS INCLUDE FILE */
{fa/shared}

if terminal > "" then input stream crt from terminal.

			   /* PAGE HEADER */

form header tent-name at 1
	    "Page:"                     to 117 space(4)
	    page-number  format ">>>>9"

	    functdesc                   at 1
	    "As-of date:"               to 117
	    r-date
	    string(time,"hh:mm") format "x(5)"

	    fill("-",132) format "x(132)"

			   /* ADD ANY REQUIRED SUB-HEADINGS HERE */

	 /* fill("-",132) format "x(132)"  ACTIVATE WITH SUB-HEADINGS */

	    skip(1)

     with {&UGUI-RPT} page-top no-box width 132 frame top no-attr-space.

			   /* PAGE FOOTER */

form header skip(1) "Continued on page:" page-number + 1 format ">>>>9"
     with {&UGUI-RPT} page-bottom no-box no-labels frame footer width 132 no-attr-space.

view frame top.
if page-size ne 0 then view frame footer.
page.

MAIN-LOOP:
for each  sort1
    where sort1.sort-code1 ge sort-begin
      and sort1.sort-code1 le sort-end
    no-lock

    with {&UGUI-RPT} no-box width 132 frame main-loop down no-attr-space:


  /* INSERT 'FOR-EVERY-RECORD' LOGIC IN HERE */

  display sort1 with {&UGUI-RPT}.


  accumulate 1 /*sort1*/ (count).   /* RECORD COUNT */

  /* INCLUDE AT THE END OF EACH PRINTING LOOP*/

  {pt/newpage main-loop string(sort1.sort-code1)}

end.   /* END OF MAIN-LOOP */

if wpage-number eq 0 then
   display " " with {&UGUI-RPT} no-labels no-box frame last-frame.

form header skip(1) "End-of-Report." accum count 1 /*sort1*/ format ">>>,>>9"
     "sort1(s) Printed."
     if r-memo > "" then " Memo: " + r-memo else "" format "x(42)"
     with {&UGUI-RPT} page-bottom no-labels no-box frame last-footer width 132
	  no-attr-space.

hide frame footer.
view frame last-footer.
page.
