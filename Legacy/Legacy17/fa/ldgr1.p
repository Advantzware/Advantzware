/* fa/ldgr1.p - Asset Ledger TAX - 1 Depreciation */
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

define variable  acc-dep     like fa-mast.cost-book.
define variable  start-value like acc-dep.

define variable  net-value   like acc-dep.
define variable  tot-debit   like fa-entry.debit.
define variable  tot-credit  like tot-debit.
define variable  credit#     like fa-entry.credit-amt.
define variable  debit#      like fa-entry.debit-amt.

define variable t-tot-debit like tot-debit.
define variable t-tot-credit like tot-credit.
define variable t-net-value like net-value.
define variable printed as logical.

define variable  loop as integer.  /* accumulator only */

Define buffer fa-mast# for fa-mast.
define variable liste# as character format "x(5)".

{fa/ldgr1.y}                /* YOUR PARAMETERS INCLUDE FILE */
{fa/std}                /* YOU MAY WANT TO CHANGE IT TO GL/STD */
{fa/ctrlwrk}                /* create fa-control workfile */

if terminal > "" then input stream crt from terminal.

form header systemname
	    "Page:"                     to 117 space(4)
	    page-number  format ">>>>9"

	    functdesc                   at 1
	    "As-of date:"               to 117
	    r-date
	    string(time,"hh:mm") format "x(5)"

	    fill("-",132) format "x(132)"

"Asset"                 AT 1
"Asset Description"     AT 10
"Loc."                  AT 40
"Entity"                AT 50
"Cost Book"             TO 72
"Accum. Dep."           to 128
skip
"Sort Codes 1"               AT 10
"Sort Codes 2"               AT 25
"Status"                     AT 50
"Dep. Basis"                 to 72
"D E P R E C I A T I O N"  TO 109
"Book Value"            to 128
SKIP
"Code"                  AT 1
"Trans Date"            AT 10
"Prd"                   AT 21
"Yr"                    AT 25
"GL Group"                  AT 30
"Src"                       at 40
"E X P L A N A T I O N"           AT 45
"Debit"                           TO 95
"Credit"                          TO 109

	    fill("-",132) format "x(132)" skip(1)

     with {&UGUI-RPT} page-top no-box width 132 frame top.

			   /* PAGE FOOTER */

form header skip(1) "Continued on page:" page-number + 1 format ">>>>9"
     with {&UGUI-RPT} page-bottom no-box no-labels frame footer width 132.

view frame top.
if page-size ne 0 then view frame footer.
page.

if show-detail = no and end-prd > fa-control.number-prd then
   end-prd = fa-control.number-prd.

if by-list = "P":U then liste# = "no":U.
if by-list = "C":U then liste# = "yes":U.
if by-list = "B":U then liste# = "yesno":U.
if by-list = "R":U then liste# = "no":U.
if by-list = "O":U then liste# = "no":U.

MAIN-LOOP:
FOR EACH FA-MAST WHERE fa-mast.fa-entity eq fa-control.fa-entity
    AND fa-mast.ASSET-CODE GE BEG-CODE
    AND fa-mast.ASSET-CODE LE END-CODE
    AND fa-mast.GL-CODE GE BEG-GL
    AND fa-mast.GL-CODE LE END-GL
    AND fa-mast.location GE beg-loc
    AND fa-mast.location LE end-loc
    AND (INDEX(t-status,FA-MAST.ASSET-STATUS) NE 0)
    AND (IF SORT1 NE "" THEN CAN-DO(SORT1,fa-mast.SORT-CODE1) ELSE TRUE)
    AND (IF SORT2 NE "" THEN CAN-DO(SORT2,fa-mast.SORT-CODE2) ELSE TRUE)
    and (index(liste#,string(fa-mast.child-par)) ne 0) NO-LOCK
    BY   IF t-order = "l":U THEN location
    ELSE IF t-order = "1":U THEN sort-code1
    ELSE IF t-order = "2":U THEN sort-code2
    ELSE IF t-order = "g":U THEN gl-code
    ELSE asset-code
    ON ERROR UNDO, LEAVE MAIN-LOOP
    WITH {&UGUI-RPT} FRAME BODY NO-ATTR-SPACE NO-LABELS NO-BOX WIDTH 255: /* mod-sh 11/91*/

  if by-list = "O":U then do:
     find first fa-mast# where fa-mast#.par-asset = fa-mast.asset-code
			   and fa-mast.fa-entity = logincontrol
     no-lock no-error.
     if not available fa-mast# then next.
  end.

  {pt/newpage main-loop string(fa-mast.asset-code)}


    if fa-mast.asset-status = "R":U then
       acc-dep = fa-mast.acc-dep-tax1 - fa-mast.cy-dep-tax-1.
    else acc-dep = fa-mast.acc-dep-tax1.
    start-value = fa-mast.cost-book - acc-dep.

    DISPLAY
    FA-MAST.ASSET-CODE              AT 1
    fa-mast.ASSET-DESC              AT 10
    FA-MAST.LOCATION        AT 41  /* mod-rg 4/92 */
    FA-MAST.entity-code          AT 50  /* rpm 01/95 */
    fa-mast.cost-book               to 72
    acc-dep                 AT 115
    FA-MAST.SORT-CODE1      AT 10  /* mod-rg 4/92 */
    FA-MAST.SORT-CODE2      AT 25  /* mod-rg 4/92 */
    FA-MAST.ASSET-STATUS    AT 53  /* mod-rg 4/92 */
    fa-mast.dep-basis-t1    to 72
    start-value             at 115
    SKIP(1) with {&UGUI-RPT}.

    accum start-value (total).
    accum acc-dep (total).
    accum fa-mast.cost-book (total).
    accum 1 (count).
    tot-debit = 0.
    tot-credit = 0.
    net-value = start-value.

if show-detail = yes then do:
  FIND FIRST fa-entry OF fa-mast WHERE fa-entry.method = "TAX1":U AND
       fa-entry.prd ge beg-prd AND fa-entry.prd le end-prd AND
       fa-entry.yr eq fa-control.yr no-lock NO-ERROR.

       if not available fa-entry  then do:
	  display fill("-",132) format "x(132)" skip with {&UGUI-RPT}.
	  accum net-value (total).
	  next. /* fa-mast */
       end.
end.

       tot-debit = 0.
       tot-credit = 0.

  if show-detail = yes then do:

    FOR EACH FA-ENTRY OF FA-MAST WHERE FA-ENTRY.METHOD = "TAX1":U
				   and fa-entry.prd ge beg-prd
				   AND fa-entry.prd le end-prd
				   AND fa-entry.yr eq fa-control.yr
    NO-LOCK by fa-entry.yr by fa-entry.prd WITH {&UGUI-RPT} FRAME FRAME1  NO-BOX NO-LABELS
    no-attr-space WIDTH 255:
		net-value = net-value - (fa-entry.debit - fa-entry.credit).
		credit# = fa-entry.credit.
		debit# =  fa-entry.debit.
		tot-debit = tot-debit + debit#.
		tot-credit = tot-credit + credit#.

		DISPLAY
		TRANS-DATE              AT 10
		PRD                     AT 21
		FA-ENTRY.YR             AT 25
		FA-MAST.GL-CODE         AT 30
		ORIGIN                  AT 40
		EXPLANATION             AT 45
		DEBIT#               AT 83
		CREDIT#      AT 97
		net-value       AT 115 WITH {&UGUI-RPT}
		FRAME FRAME3
		no-attr-space NO-BOX NO-LABEL
		WIDTH  255.
		accum credit# (total).
		accum debit# (total).

   END.                       /* END OF FOR EACH FA-entry */
end.

if show-detail = no then do:


   do loop = beg-prd to end-prd :

      if fa-mast.tax1-period[loop] >= 0 then debit# = fa-mast.tax1-period[loop].
      else credit# = fa-mast.tax1-period[loop].
      tot-debit = tot-debit + debit#.
      tot-credit = tot-credit + credit#.
      net-value = net-value - (debit# - credit#).

      if fa-mast.tax1-period[loop] ne 0 then do:
      DISPLAY
		fa-control#.to-date[loop] at 10
		FA-control.yr           AT 25
		FA-MAST.GL-CODE         AT 30
		"Summary for period"    AT 45
		loop format ">9"
		DEBIT#               AT 83
		CREDIT#              AT 97
		net-value       AT 115 WITH {&UGUI-RPT}
		FRAME FRAME33
		no-attr-space NO-BOX NO-LABEL
		WIDTH  255.
		accum credit# (total).
		accum debit# (total).
		down with frame frame33.
      end.

   END.                       /* END of loop */
end.





/*PRINT REPORT TOTALS IF APPLICABLE */

DISPLAY "--------------" AT 85
	"------------------" AT 99
	"------------" AT 117
	SKIP
	tot-debit AT 83
	tot-credit AT 97
	net-value AT 115 skip
	fill("-",132) format "x(132)"
	when (by-list ne "R" and by-list ne "O")
	skip
	WITH {&UGUI-RPT} FRAME FRAME5
	no-attr-space NO-BOX NO-LABEL WIDTH 255.
	accum net-value (total).

if by-list = "R":U or by-list = "O":U then do:
   { fa/lldgr1 }
end.


END.

DISPLAY skip(1)
	"TOTAL BOOK VALUE : " to 50
	(ACCUM TOTAL fa-mast.cost-book) + (accum total fa-mast#.cost-book)
	FORMAT "->>,>>>,>>9.99"
	"TOTAL ACC. DEP. - TAX 1 VALUE : " to 50
	(ACCUM TOTAL acc-dep ) FORMAT "->>,>>>,>>9.99"
	"TOTAL BEGINING TAX 1 VALUE : " to 50
	(ACCUM TOTAL start-value) FORMAT "->>,>>>,>>9.99"
	"TOTAl DEBIT : " to 50
	(accum total debit#) FORMAT "->>,>>>,>>9.99"
	"TOTAL CREDIT : " to 50
	(accum total credit#) FORMAT "->>,>>>,>>9.99"
	"TOTAL ENDING TAX 1 VALUE : " to 50
	(accum total net-value) format "->>,>>>,>>9.99"
	skip(1)
	"CURRENCY CODE : " to 50
	(fa-control.currency-cod)
	with {&UGUI-RPT} no-label no-box width 132.

if wpage-number eq 0 then
   display " " with {&UGUI-RPT} no-labels no-box frame last-frame.

form header skip(1) "End-of-Report." accum count 1 format ">>>,>>9"
     "Record(s) Printed."
     if r-memo > "" then " Memo: " + r-memo else "" format "x(42)"
     with {&UGUI-RPT} page-bottom no-labels no-box frame last-footer width 132.

hide frame footer.
view frame last-footer.
page.
