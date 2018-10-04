/*    fa/rdep.p - Print Depreciation Table      */
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
{fa/rdep.y}


if terminal > "" then input stream crt from terminal.

form header systemname
	    "Page:"                     to 117 space(4)
	    page-number  format ">>>>9"

	    functdesc                   at 1
	    "As-of date:"               to 117
	    r-date
	    string(time,"hh:mm") format "x(5)"

	    fill("-",132) format "x(132)"

"Method" at 1  "Decl/Sl" at 10 "Ratio" at 20 "Conv." at 30 "Manual" at 38
"SOYD" at 49 "Manual Dep" at 59 skip
/* "------" at 1  "---------" at 10 "-----" at 20 "-----" at 30 "------" at 40
"----" at 49 "----------" at 58 */

	    fill("-",132) format "x(132)" skip(1)

     with {&UGUI-RPT} page-top no-box width 132 frame top.

			   /* PAGE FOOTER */

form header skip(1) "Continued on page:" page-number + 1 format ">>>>9"
     with {&UGUI-RPT} page-bottom no-box no-labels frame footer width 132.

view frame top.
if page-size ne 0 then view frame footer.
page.

for each dep-table where dep-table.method ge mtd-begin
			and  dep-table.method le mtd-end
			no-lock:

if mtd-type = "manual":U and dep-table.manual = no then next.
if mtd-type = "auto":U and dep-table.manual = yes then next.

if wpage-number ne page-number then wpage-number = page-number.
if not slave then status default
		  string(page-number," Page: zzzz9") + "  Processing: ":U +
		  string(dep-table.method).
if wnice then pause 1 no-message.
if terminal > "" then do:
  readkey stream crt pause 0.
  if keyfunction(lastkey) = "end-error":U or
     keyfunction(lastkey) = "endkey":U then do:
      run value("pt/mess.p":U) ("fa0125":U,  string(today) + "~001":U + string(time,"hh:mm":U) + "~001":U ).
    return.
  end.
end.


    display method at 1 auto-sl at 10 format "S-Line/Declin."
    ratio at 20 conv at 30 manual at 40 soyd at 50
    with {&UGUI-RPT} no-box no-labels no-attr-space width 132 frame frame1.
    accum 1 (count).
    if dep-table.manual = yes then
    display depr at 56
    with {&UGUI-RPT} no-box no-labels no-attr-space width 132 frame frame2.

end.


if wpage-number eq 0 then
   display " " with {&UGUI-RPT} no-labels no-box frame last-frame.

form header skip(1) "End-of-Report." accum count 1 format ">>>,>>9"
     "Record(s) Printed."
     if r-memo > "" then " Memo: " + r-memo else "" format "x(42)"
     with {&UGUI-RPT} page-bottom no-labels no-box frame last-footer width 132.

hide frame footer.
view frame last-footer.
page.
