/* fa/rtax.p - F/A Tax Table list */
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
DEFINE VAR XX AS CHAR FORMAT "X".
define variable tot-perc as decimal format "->>9.9999"
		label "Total % ".
define variable lloop as integer.


{fa/rtax.y}                /* YOUR PARAMETERS INCLUDE FILE */
{fa/shared}                /* YOU MAY WANT TO CHANGE IT TO GL/STD */


if terminal > "" then input stream crt from terminal.

form header systemname
	    "Page:"                     to 117 space(4)
	    page-number  format ">>>>9"

	    functdesc at 1
	    "As-of date:"               to 117
	    r-date
	    string(time,"hh:mm") format "x(5)"
	    fill("-",132) format "x(132)"

"Method"                 AT 1
"Description"            AT 8
"Prd-1"                  AT 71
"Prd-2"                  AT 80
"Prd-3"                  AT 89
"Prd-4"                  AT 98
"Prd-5"                  AT 107
"Prd-6"                  AT 116
skip
"Life"                   AT 30
"Yr"                     AT 35
"Prd-7"                  AT 71
"Prd-8"                  AT 80
"Prd-9"                  AT 89
"Prd-10"                 AT 98
"Prd-11"                 AT 107
"Prd-12"                 AT 116
"Prd-13"                 AT 125

	    fill("-",132) format "x(132)" skip(1)

     with {&UGUI-RPT} page-top no-box width 132 frame top.

			   /* PAGE FOOTER */

form header skip(1) "Continued on page:" page-number + 1 format ">>>>9"
     with {&UGUI-RPT} page-bottom no-box no-labels frame footer width 132.

view frame top.
if page-size ne 0 then view frame footer.
page.
MAIN-LOOP: FOR EACH  TAX-TABLE  where
  tax-table.method ge fr-method and
  tax-table.method le to-method
    NO-LOCK ON ERROR UNDO, LEAVE MAIN-LOOP :

  {pt/newpage main-loop string(tax-table.method)}

  tot-perc = 0.
  do lloop = 1 to 13:
     tot-perc = tot-perc + period[lloop].
  end.
  tot-perc = tot-perc * 100.

PUT
SKIP
METHOD                  AT 1
tax-table.DESCRIPTION             AT 8
PERIOD[1]               AT 71
PERIOD[2]               AT 80
PERIOD[3]               AT 89
PERIOD[4]               AT 98
PERIOD[5]               AT 107
PERIOD[6]               AT 116
SKIP
"Total % :"             at 1
tot-perc
ASSET-LIFE              AT 30
YEAR-OF-DEPR            AT 35
PERIOD[7]               AT 71
PERIOD[8]               AT 80
PERIOD[9]               AT 89
PERIOD[10]              AT 98
PERIOD[11]              AT 107
PERIOD[12]              AT 116
PERIOD[13]              AT 125.
XX = "-":U.
   PUT FILL(XX, 132) FORMAT "x(132)" AT 1
   skip(1).
ACCUM 1 (COUNT).

END.                       /* END OF MAIN-LOOP */

			   /* PRINT REPORT TOTALS IF APPLICABLE */


  if wpage-number eq 0 then
     display " " with {&UGUI-RPT} no-labels no-box frame last-frame.

  form header skip(1)
	      "End-of-report." accum count 1   "Records printed."
	      msg  if r-memo > "" then " Memo: " + r-memo else "" format "x(42)"
	 with {&UGUI-RPT} page-bottom no-labels no-box frame last-footer width 132.

  hide frame footer.
    view frame last-footer.
    page.
