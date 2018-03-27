/* fa/gldistr.p   GL Distribution Report
		DR FEB 93 sur un model de JW October 86

lisakay matchen 5/23/00 call # 12278 add GL-code to top of page continuation

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

define variable wpage-number as   integer.
define variable year#        like fa-gl-post.yr.
define variable period#      like fa-gl-post.prd.
def var gl-code#            like fa-gl-post.gl-code.  /* 12278 */

define variable pshort-desc  like gl-account.short-desc.
define stream crt.

{fa/gldistr.y}             /* YOUR PARAMETERS INCLUDE FILE */
{fa/std}
if terminal > "" then input stream crt from terminal.

if f-year = 0 then f-year =  login.current-yr.
if f-period = 0 then f-period =  login.current-prd.
if c-year = 0 then c-year =  login.current-yr.
if c-period = 0 then c-period =  login.current-prd.

			   /* PAGE HEADER */

form header systemname
	    "Page:"                     to 117
	    page-number  format ">>>>9" to 126
	    functdesc                   at 1
	    "As-of-date:"               to 117
	    r-date
	    string(time,"hh:mm") format "x(5)"
	    skip fill("-",132) format "x(132)"
	    "Year:" year# space(5) "Period:" period# "GL-Code Continued:" Gl-code# skip(1)
	    "GL-code" at 1
	    "Description" at 10 skip
	    "Explanation" at 10
	    "Entity" at 53
	    "Tr Date" at 63
	    "Net Change" to 98
	    "Xfer" at 103
	    "Home Currency" to 130
	    fill("-",132) format "x(132)"
     with {&UGUI-RPT} page-top no-box width 132 frame top.

			   /* PAGE FOOTER */

form header skip(1) "Continued on page:" page-number + 1 format ">>>>9"
     with {&UGUI-RPT} page-bottom no-box frame footer.

view frame top.
if page-size ne 0 then view frame footer.
page.

main-loop:

for each  fa-gl-post
	       where fa-gl-post.fa-entity eq fa-control.fa-entity
		 and fa-gl-post.gl-code ge from-gl
		 and fa-gl-post.gl-code le to-gl
		 and ((fa-gl-post.posted = no
		       and print-trans = yes) or print-trans = no)
		 and fa-gl-post.yr ge f-year and
		 fa-gl-post.entity-code matches p-entity
	       no-lock
	       break by fa-gl-post.fa-entity
		     by fa-gl-post.yr by fa-gl-post.prd by fa-gl-post.gl-code
		     by fa-gl-post.asset-code
	       with {&UGUI-RPT} no-labels no-attr-space no-box width 132 frame main-loop:
	   if fa-gl-post.yr * 100 + fa-gl-post.prd > c-year * 100 + c-period
	   then leave.
	   if fa-gl-post.yr * 100 + fa-gl-post.prd < f-year * 100 + f-period
	   then next main-loop.

	   if first-of(fa-gl-post.prd) then do:
	      year# = fa-gl-post.yr.
	      period# = fa-gl-post.prd.
	      accumulate 1 /*Yr/prd*/ (count).
	      page.
	   end.

	   if first-of(fa-gl-post.gl-code) then do for gl-account:
	      find gl-account of fa-gl-post no-lock no-error.
	      if available gl-account then pshort-desc = gl-account.short-desc.
	      else pshort-desc = "GL-code not found".
	      if d-s then
	      display fa-gl-post.gl-code at 1
		      pshort-desc at 10 skip(1)
	      with {&UGUI-RPT} no-box no-labels frame gldes.
		     assign gl-code# = fa-gl-post.gl-code.  /* 12278 */
 
	   end.

	   /* DETAIL */
	   if d-s then do:
	      display fa-gl-post.explanation at 10
	      fa-gl-post.entity-code at 53
	      fa-gl-post.trans-date at 63 with {&UGUI-RPT}.
	      display fa-gl-post.currency-cod at 73 with {&UGUI-RPT}.
	      display fa-gl-post.debit-amt - fa-gl-post.credit-amt
		      format "->,>>>,>>>,>>9.99" to 98 with {&UGUI-RPT}.
	      display fa-gl-post.posted at 103 with {&UGUI-RPT}.
	      display fa-gl-post.hm-debit-amt - fa-gl-post.hm-credit-amt
		      format "->,>>>,>>>,>>9.99" to 130 with {&UGUI-RPT}.
	   end.

	   accum fa-gl-post.debit-amt
		 (sub-total by fa-gl-post.prd by fa-gl-post.gl-code).
	   accum fa-gl-post.debit-amt (total).
	   accum fa-gl-post.hm-debit-amt
		 (sub-total by fa-gl-post.prd by fa-gl-post.gl-code).
	   accum fa-gl-post.hm-debit-amt (total).
	   accum fa-gl-post.credit-amt
		 (sub-total by fa-gl-post.prd by fa-gl-post.gl-code).
	   accum fa-gl-post.credit-amt (total).
	   accum fa-gl-post.hm-credit-amt
		 (sub-total by fa-gl-post.prd by fa-gl-post.gl-code).
	   accum fa-gl-post.hm-credit-amt (total).
	   accum fa-gl-post.debit-amt - fa-gl-post.credit-amt
		 (sub-total by fa-gl-post.prd by fa-gl-post.gl-code).
	   accum fa-gl-post.debit-amt - fa-gl-post.credit-amt (total).
	   accum fa-gl-post.hm-debit-amt - fa-gl-post.hm-credit-amt
		 (sub-total by fa-gl-post.prd by fa-gl-post.gl-code).
	   accum fa-gl-post.hm-debit-amt - fa-gl-post.hm-credit-amt (total).

	   /* GL-CODE BREAK */
	   if last-of(fa-gl-post.gl-code) then do:
            assign gl-code# = "" . /* 12278 */

	     if d-s then do with {&UGUI-RPT} frame yg1
	     no-attr-space no-labels no-box width 132:
		display "-------------------" format "x(19)" to 98 with {&UGUI-RPT}.
		display "-------------------" format "x(19)" to 130 with {&UGUI-RPT}.
	     end.

	     do with {&UGUI-RPT} frame yg no-labels no-attr-space no-box width 132:
	      if d-s = no then display fa-gl-post.gl-code pshort-desc with {&UGUI-RPT}.
		 display fa-gl-post.currency-cod at 73 with {&UGUI-RPT}.
		 display accum sub-total by fa-gl-post.gl-code
		 fa-gl-post.debit-amt - fa-gl-post.credit-amt
		       format "->>>,>>>,>>>,>>9.99" to 98 with {&UGUI-RPT}.
		 display accum sub-total by fa-gl-post.gl-code
		 fa-gl-post.hm-debit-amt - fa-gl-post.hm-credit-amt
		       format "->>>,>>>,>>>,>>9.99" to 130 with {&UGUI-RPT}.
	     end.
	     if d-s then display skip(1) with {&UGUI-RPT} frame yg3
	     no-attr-space no-labels no-box.
	   end.

	   /* YR/PRD BREAK */
	   if last-of(fa-gl-post.prd) then do:
	      do with {&UGUI-RPT}
	      frame yg2 no-attr-space no-labels no-box width 132:
		 display "-------------------" format "x(19)" to 98 with {&UGUI-RPT}.
		 display "-------------------" format "x(19)" to 130 with {&UGUI-RPT}.
	      end.
		 display "debits:" at 40 with {&UGUI-RPT} frame d-s1.
		 display accum sub-total by fa-gl-post.prd
		 fa-gl-post.debit-amt
		 format "->>>,>>>,>>>,>>9.99" to 98 with {&UGUI-RPT} frame d-s1.
		 display accum sub-total by fa-gl-post.prd
		 fa-gl-post.hm-debit-amt
		 format "->>>,>>>,>>>,>>9.99" to 130 with {&UGUI-RPT} frame d-s1.
		 display "credits:" at 39 with {&UGUI-RPT} frame d-s1.
		 display accum sub-total by fa-gl-post.prd
		 fa-gl-post.credit-amt
		 format "->>>,>>>,>>>,>>9.99" to 98 with {&UGUI-RPT} frame d-s1.
		 display accum sub-total by fa-gl-post.prd
		 fa-gl-post.hm-credit-amt
		 format "->>>,>>>,>>>,>>9.99" to 130 with {&UGUI-RPT} frame d-s1.
		 display year# at 34
		 "/" at 36
		 period# at 37
		 "totals:" at 40
		 with {&UGUI-RPT} no-attr-space no-labels no-box frame d-s1 width 132.
		 display accum sub-total by fa-gl-post.prd
		 fa-gl-post.debit-amt - fa-gl-post.credit-amt
		 format "->>>,>>>,>>>,>>9.99" to 98 with {&UGUI-RPT} frame d-s1.
		 display accum sub-total by fa-gl-post.prd
		 fa-gl-post.hm-debit-amt - fa-gl-post.hm-credit-amt
		 format "->>>,>>>,>>>,>>9.99" to 130 with {&UGUI-RPT} frame d-s1.
	   end.

  {pt/newpage main-loop fa-gl-post.gl-code}

end.                           /* END OF MAIN-LOOP */
/* REPORT BREAK */
   display fill("-",19) format "x(19)" to 98 with {&UGUI-RPT} no-labels no-box
    no-attr-space frame d-s2 width 132.
   display fill("-",19) format "x(19)" to 130 with {&UGUI-RPT} no-labels no-box
    no-attr-space frame d-s2 width 132.

display skip(2) "debits:" at 40 with {&UGUI-RPT} frame d-s2.
   display accum total fa-gl-post.debit-amt
    format "->>>,>>>,>>>,>>9.99" to 98 with {&UGUI-RPT} frame d-s2.
   display accum total fa-gl-post.hm-debit-amt
    format "->>>,>>>,>>>,>>9.99" to 130 with {&UGUI-RPT} frame d-s2.
display "credits:" at 39 with {&UGUI-RPT} frame d-s2.
   display accum total fa-gl-post.credit-amt
    format "->>>,>>>,>>>,>>9.99" to 98 with {&UGUI-RPT} frame d-s2.
   display accum total fa-gl-post.hm-credit-amt
    format "->>>,>>>,>>>,>>9.99" to 130 with {&UGUI-RPT} frame d-s2.
display "Report totals:" at 33 with {&UGUI-RPT} frame d-s2.
   display accum total fa-gl-post.debit-amt - fa-gl-post.credit-amt
    format "->>>,>>>,>>>,>>9.99" to 98 with {&UGUI-RPT} frame d-s2.
   display accum total fa-gl-post.hm-debit-amt - fa-gl-post.hm-credit-amt
    format "->>>,>>>,>>>,>>9.99" to 130 with {&UGUI-RPT} frame d-s2.
if wpage-number eq 0 then
   display " " with {&UGUI-RPT} no-labels no-box width 132 frame last-frame.

form header skip(1) "End-of-Report." accum count 1 /*Yr/prd*/ format ">>>,>>9"
     "Yr/prd(s) Printed."
     if r-memo > "" then " Memo: " + r-memo else "" format "x(42)"
     with {&UGUI-RPT} page-bottom no-box frame last-footer width 132.

hide frame footer.
view frame last-footer.
page.
