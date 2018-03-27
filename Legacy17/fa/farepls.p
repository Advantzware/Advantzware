/* fa/farepls.p   liste des actifs ( reparation) (from pt/report.m)
		LTRC CIE 01/06/92
lisakay matchen 4/20/00 call # 14471 change release-no format from 99 to 999

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
define var asset-code# like fa-mast.asset-code.
define var asset-desc#       like fa-mast.asset-desc.
define var location#   like fa-mast.location.
define var entity-code# like fa-mast.entity-code.
define var asset-status# like fa-mast.asset-status.
def var po# as char format "x(15)".
define var tot-cout# as decimal format "z,zzz,zzz,zz9.99".
def var desc# as char format "X(30)".
define stream crt.
{pt/showmemo.dv}      /* Memo definitions */

{fa/farepls.y}             /* YOUR PARAMETERS INCLUDE FILE */
{pt/shared}

if terminal > "" then input stream crt from terminal.

			   /* PAGE HEADER */

form header systemname
	    "Page:"                     to 117 space(4)
	    page-number  format ">>>>9"

	    functdesc                   at 1
	    "Date:"               to 117
	    r-date
	    string(time,"hh:mm") format "x(5)"

	    fill("-",132) format "x(132)"

     with {&UGUI-RPT} page-top no-box width 132 frame top no-attr-space.

 form header skip(1)
      "Asset Code  Description                     Entity    Loc   Status" skip
      "----------  ------------------------------  --------  ----  ------" skip
       asset-code# asset-desc# at 13 entity-code# at 45 location# at 55 asset-status# at 64 skip(1)
      "         Vendor   Date   PO No.          Ln       Cost Book   Reference"
	"Doc. No.     Item No.         Description" skip
      "         ------ -------- --------------- --- ---------------- ---------"
	"------------ ---------------- ------------------------------" skip

 with {&UGUI-RPT} frame top1 no-attr-space no-box width 132 .

			   /* PAGE FOOTER */

form header skip(1) "Continued on page:" page-number + 1 format ">>>>9"
     with {&UGUI-RPT} page-bottom no-box no-labels frame footer width 132 no-attr-space.

find first fa-mast-rep where fa-mast-rep.fa-entity = logincontrol
			 and fa-mast-rep.asset-code ge from-asset
			 and fa-mast-rep.asset-code le to-asset
			 and fa-mast-rep.vendor-code ge from-vendor
			 and fa-mast-rep.vendor-code le to-vendor
			 and fa-mast-rep.cost-book ge montant-min
			      no-lock no-error.
if available fa-mast-rep then do:
   find fa-mast of fa-mast-rep no-lock no-error.
   if available fa-mast and
      fa-mast.gl-code ge fr-gl-group and
      fa-mast.gl-code le to-gl-group
   then
      assign
      asset-code# = fa-mast.asset-code
      asset-desc# = fa-mast.asset-desc
      location#   = fa-mast.location
      asset-status# = fa-mast.asset-status
      entity-code# = fa-mast.entity-code.
end.
view frame top.
if page-size ne 0 then view frame footer.
page.

MAIN-LOOP:
for each  fa-mast-rep
    where fa-mast-rep.asset-code ge from-asset
      and fa-mast-rep.asset-code le to-asset
      and fa-mast-rep.fa-entity = logincontrol
      and fa-mast-rep.vendor-code ge from-vendor
      and fa-mast-rep.vendor-code le to-vendor
      and fa-mast-rep.cost-book ge montant-min
    no-lock
    break by fa-mast-rep.asset-code
    with {&UGUI-RPT} no-box width 132 frame main-loop no-label down no-attr-space:

    find fa-mast of fa-mast-rep no-lock no-error.
    if not available fa-mast then next.
    if fa-mast.gl-code lt fr-gl-group then next.
    if fa-mast.gl-code gt to-gl-group then next.

    if first-of(fa-mast-rep.asset-code) then do:
       assign
       asset-code# = fa-mast.asset-code
       asset-desc# = fa-mast.asset-desc
       location#   = fa-mast.location
       asset-status# = fa-mast.asset-status
       entity-code# = fa-mast.entity-code
       tot-cout# = 0.
       view frame top1.
    end.

   if fa-mast-rep.purch-order# ne "" then
      po# = fa-mast-rep.purch-order# + "-":U + string(release-no,"999":U).
      else po# = "".
  display fa-mast-rep.vendor-code at 10 fa-mast-rep.trans-date po#
	  fa-mast-rep.line-no to 44
	  fa-mast-rep.cost-book to 61 voucher-no to 71 reference at 73
	  fa-mast-rep.item-no at 87
	  fa-mast-rep.description[1] at 103 with {&UGUI-RPT} frame main-loop.
  if fa-mast-rep.description[2] ne "" then do:
     display fa-mast-rep.description[2] at 103
       with {&UGUI-RPT} no-label no-box no-attr-space width 132 frame int1.
  end.
  tot-cout# = tot-cout# +  fa-mast-rep.cost-book.

      wkeylist = fa-mast-rep.fa-entity + ",":U +
		 fa-mast-rep.asset-code + ",":U +
		 string(fa-mast-rep.line-no) + ",":U.
    {pt/showmem1 fa-mast-rep 10 1}
  if last-of(fa-mast-rep.asset-code) then do:
     display "----------------" at 46 skip tot-cout# to 61 label "TOTAL"
     with {&UGUI-RPT} frame tot2 no-attr-space side-label no-box .
  end.
  accumulate 1 /*fa-mast-rep*/ (count).   /* RECORD COUNT */

  /* INCLUDE AT THE END OF EACH PRINTING LOOP*/

  {pt/newpage main-loop string(fa-mast-rep.asset-code)}

end.   /* END OF MAIN-LOOP */

if wpage-number eq 0 then
   display " " with {&UGUI-RPT} no-labels no-box no-attr-space frame last-frame.

form header skip(1) "End-of-report." accum count 1 /*fa-mast-rep*/
      format ">>>,>>9"
     "record(s) fa-mast-rep printed."
     if r-memo > "" then " Memo : " + r-memo else "" format "x(42)"
     with {&UGUI-RPT} page-bottom no-labels no-box frame last-footer width 132
	  no-attr-space.

hide frame footer.
view frame last-footer.
page.
