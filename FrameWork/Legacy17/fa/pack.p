/* fa/pack.p - Purge sold asset data */
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

{fa/pack.y}
{fa/shared}

def new shared var faa-rowid as   rowid.
def new shared var fa-rowid  as   rowid.
define new shared variable waction   as   character.
define new shared variable wsequence like  z_numgen.l-sequence.
define new shared variable wdo-audit like  z_numgen.do-audit.
define new shared variable saverowid as   rowid.
def var i                    as   integer.


if terminal > "" then input stream crt from terminal.

form header systemname
	    "Page:"                     to 117 space(4)
	    page-number  format ">>>>9"

	    functdesc                   at 1
	    "As-of date:"               to 117
	    r-date
	    string(time,"hh:mm") format "x(5)"

	    fill("-",132) format "x(132)"

	    "Retired Date" at 1
	    "Asset-Code"   at 15
	    "Description"  at 27
	    "Entity Code"  at 59      /* rpm 01/95 */
	    "Job No"       at 68      /* rpm 01/95 */

	    fill("-",132) format "x(132)" skip(1)

     with {&UGUI-RPT} page-top no-box width 132 frame top.

			   /* PAGE FOOTER */

form header skip(1) "Continued on page:" page-number + 1 format ">>>>9"
     with {&UGUI-RPT} page-bottom no-box no-labels frame footer width 132.

view frame top.
if page-size ne 0 then view frame footer.
page.


MAIN-LOOP:

    for each fa-mast where fa-mast.asset-code ge fr-asset
		       and fa-mast.asset-code le to-asset
		       and fa-mast.date-retired ge fr-date
		       and fa-mast.date-retired le to-date
		       and fa-mast.fa-entity = fa-control.fa-entity
		       and fa-mast.asset-status = "R":U :

	 {pt/newpage main-loop string(fa-mast.asset-code)}

    for each fa-entry of fa-mast :
	delete fa-entry.
    end.
    display fa-mast.date-retired at 2
	    fa-mast.asset-code   at 17
	    fa-mast.asset-desc   at 27
	    fa-mast.entity-code  at 59
	    fa-mast.job-no
	    with {&UGUI-RPT} no-box no-labels frame frame1.

    find fa-control-d of fa-control.
    fa-a-no = fa-a-no + 1.
    fa-rowid = rowid(fa-mast).
    {fa/faedt3}
    saverowid = rowid(fa-mast).
    run fa/faedt4.p.
    delete fa-mast.
    accum 1 (count).
end.

if wpage-number eq 0 then
   display " " with {&UGUI-RPT} no-labels no-box frame last-frame.

form header skip(1) "End-of-Report." accum count 1 format ">>>,>>9"
     "Record(s) Purged."
     if r-memo > "" then " Memo: " + r-memo else "" format "x(42)"
     with {&UGUI-RPT} page-bottom no-labels no-box frame last-footer width 132.

hide frame footer.
view frame last-footer.
page.
