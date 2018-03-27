/* fa/distprg.p - Purge G/L Distribution */
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
def var cntr as int.
{fa/distprg.y}
{fa/std}

if f-year   eq 0 then f-year   =  login.current-yr.
if beg-prd eq 0 then beg-prd =  login.current-prd.
if c-year   eq 0 then c-year   =  login.current-yr.
if end-prd eq 0 then end-prd =  login.current-prd.

if terminal > "" then input stream crt from terminal.

form header systemname
	    "Page:"                     to 117 space(4)
	    page-number  format ">>>>9"

	    functdesc                   at 1
	    "As-of date:"               to 117
	    r-date
	    string(time,"hh:mm") format "x(5)"

	    fill("-",132) format "x(132)"

	    "Entity" AT 1
	    "Gl-Code" AT 10
	    "Description" AT 20
	    "Debit-amt" AT 50
	    "Credit-Amt" AT 66
	    "Home Debit-amt" AT 82
	    "Home Credit-Amt" AT 98
	    "Currency" at 115
	    "Job no" at 125 skip
	    "Explanation" AT 20 skip

	    fill("-",132) format "x(132)" skip(1)

     with {&UGUI-RPT} page-top no-box width 132 frame top.

			   /* PAGE FOOTER */

form header skip(1) "Continued on page:" page-number + 1 format ">>>>9"
     with {&UGUI-RPT} page-bottom no-box no-labels frame footer width 132.

view frame top.
if page-size ne 0 then view frame footer.
page.
MAIN-LOOP:
for each  fa-gl-post
	       where fa-gl-post.fa-entity eq fa-control.fa-entity
		 and fa-gl-post.yr        le c-year
		 and fa-gl-post.yr        ge f-year
	       exclusive-lock use-index fa-gl-post1
	   with {&UGUI-RPT} no-labels no-box width 132 no-attr-space frame MAIN-LOOP:

  {pt/newpage main-loop string(fa-gl-post.gl-code)}

  if (fa-gl-post.yr  eq c-year)   and
     (fa-gl-post.prd gt end-prd)
  then leave.

  if fa-gl-post.yr * 100 + fa-gl-post.prd < f-year * 100 + beg-prd
  then next.

  if tpost and not posted then next.
   find gl-account where gl-account.gl-code = fa-gl-post.gl-code.

   display
   FA-GL-POST.ENTITY-CODE  AT 1
   FA-GL-POST.GL-CODE   AT 10
   GL-ACCOUNT.DESCRIPTION AT 20
   DEBIT-AMT  AT 50
   CREDIT-AMT AT 66
   hm-debit-amt to 95
   hm-credit-amt to 112
   currency-cod at 116
   job-no at 123 skip
   FA-GL-POST.EXPLANATION at 20 with {&UGUI-RPT} width 132 frame c no-labels.

   delete fa-gl-post.
   accum 1 (count).
end.

if wpage-number eq 0 then
   display " " with {&UGUI-RPT} no-labels no-box frame last-frame.

form header skip(1) "End-of-Report." accum count 1 format ">>>,>>9"
     "Record(s) Purged"
     if r-memo > "" then " Memo: " + r-memo else "" format "x(42)"
     with {&UGUI-RPT} page-bottom no-labels no-box frame last-footer width 132.

hide frame footer.
view frame last-footer.
page.
