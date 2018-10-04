/* fa/rsch.p - Fixed assets Schedule */
/**************** Copyright Statement Follows ***************************
* (C) Copyright, 1996 - 1997 Foresight Software, Inc.  All Rights       *
* Reserved.  This is unpublished material and contains trade secrets    *
* and other confidential information and is subject to licensing and    *
* a confidentiality agreement.  The unauthorized possession, use,       *
* reproduction, reverse engineering, distribution, display, or          *
* disclosure of this material or of any information contained herein    *
* or any information derived from this material is strictly prohibited. *
************************************************************************/

DEFINE NEW SHARED VARIABLE LINESIZE   AS INTEGER INITIAL 1.
DEFINE NEW SHARED VARIABLE PAGECOUNT  AS INTEGER FORMAT "ZZZ9" INITIAL 1.
DEFINE NEW SHARED VARIABLE LINECOUNT  AS INTEGER.
DEFINE NEW SHARED VARIABLE FIRST-PAGE AS LOGICAL INITIAL TRUE.
DEFINE NEW SHARED VARIABLE BODYSIZE   AS INTEGER INITIAL 60.
DEFINE NEW SHARED VARIABLE REC-COUNT  AS INTEGER.
DEFINE NEW SHARED VARIABLE REC-DESC   AS CHARACTER FORMAT "X(50)".
DEFINE NEW SHARED VARIABLE STATUS-DESC AS CHARACTER FORMAT "X(10)".
DEFINE NEW SHARED VARIABLE TMETHOD LIKE FA-MAST.METHOD-BOOK.
DEFINE NEW SHARED VARIABLE TDEP-BASIS LIKE FA-MAST.DEP-BASIS-BK.
DEFINE NEW SHARED VARIABLE TCY-DEP-BOOK LIKE FA-MAST.CY-DEP-BOOK.
DEFINE NEW SHARED VARIABLE B-ACC-DEP LIKE FA-MAST.ACC-DEP-BOOK.
DEFINE NEW SHARED VARIABLE E-ACC-DEP LIKE FA-MAST.ACC-DEP-BOOK.
DEFINE NEW SHARED VARIABLE TGL-CODE LIKE FA-MAST.GL-CODE.
define new shared variable tsort1 like fa-mast.sort-code1.
define new shared variable tsort2 like fa-mast.sort-code2.
define new shared variable tloc like fa-mast.location.
DEFINE NEW SHARED VARIABLE TTDEP-BASIS LIKE FA-MAST.DEP-BASIS-BK.
DEFINE NEW SHARED VARIABLE TTCY-DEP-BOOK LIKE FA-MAST.CY-DEP-BOOK.
DEFINE NEW SHARED VARIABLE TB-ACC-DEP LIKE FA-MAST.ACC-DEP-BOOK.
DEFINE NEW SHARED VARIABLE TE-ACC-DEP LIKE FA-MAST.ACC-DEP-BOOK.
DEFINE NEW SHARED VARIABLE TCOST-BOOK LIKE FA-MAST.COST-BOOK.
DEFINE NEW SHARED VARIABLE TDESC LIKE SORT1.DESCRIPTION.
DEFINE NEW SHARED VARIABLE FTDEP-BASIS LIKE FA-MAST.DEP-BASIS-BK.
DEFINE NEW SHARED VARIABLE FTCY-DEP-BOOK LIKE FA-MAST.CY-DEP-BOOK.
DEFINE NEW SHARED VARIABLE FB-ACC-DEP LIKE FA-MAST.ACC-DEP-BOOK.
DEFINE NEW SHARED VARIABLE FE-ACC-DEP LIKE FA-MAST.ACC-DEP-BOOK.
DEFINE NEW SHARED VARIABLE FCOST-BOOK LIKE FA-MAST.COST-BOOK.
{fa/rsch.y}
{fa/shared}                /* YOU MAY WANT TO CHANGE IT TO GL/STD */

IF PAGE-SIZE NE 0 THEN BODYSIZE = PAGE-SIZE.

BODYSIZE = BODYSIZE - 12. /* x = LINES IN FRAMES HEADING + FOOTER */

REC-DESC = "Record(s) printed.".  /* YOU MAY WANT TO CHANGE Record(s) TO
				     THE FILENAME PRINTED.  */
MAIN-LOOP:
FOR EACH  FA-MAST
    WHERE ASSET-CODE GE BEG-CODE AND ASSET-CODE LE END-CODE AND
    FA-MAST.fa-entity = fa-control.fa-entity and     /* rpm 01/95 */
    FA-MAST.GL-CODE GE BEG-GL AND FA-MAST.GL-CODE LE END-GL AND
    FA-MAST.LOCATION GE BEG-LOC AND FA-MAST.LOCATION LE END-LOC AND
    (IF SORT1 NE "" THEN CAN-DO(SORT1,SORT-CODE1) ELSE TRUE) AND
    (IF SORT2 NE "" THEN CAN-DO(SORT2,SORT-CODE2) ELSE TRUE) AND
    (INDEX(t-status,FA-MAST.ASSET-STATUS) NE 0)
    BY   IF tby = "l":U THEN location
    ELSE IF tby = "1":U THEN sort-code1
    ELSE IF tby = "2":U THEN sort-code2
    ELSE IF tby = "g":U THEN gl-code
    ELSE asset-code
    ON ERROR UNDO, LEAVE MAIN-LOOP WITH {&UGUI-RPT} FRAME BODY NO-LABELS NO-BOX WIDTH 255 :


IF BOOKTAX = "1":U /* Book */ THEN DO:
	TMETHOD  = FA-MAST.METHOD-BOOK.
	TDEP-BASIS = FA-MAST.DEP-BASIS-BK.
	TCY-DEP-BOOK = FA-MAST.CY-DEP-BOOK.
	B-ACC-DEP = FA-MAST.ACC-DEP-BOOK.
	E-ACC-DEP = FA-MAST.ACC-DEP-BOOK + CY-DEP-BOOK.
END.
IF BOOKTAX = "2":U /* Tax1 */ THEN DO:
	TMETHOD  = FA-MAST.METHOD-TAX-1.
	TDEP-BASIS = FA-MAST.DEP-BASIS-T1.
	TCY-DEP-BOOK = FA-MAST.CY-DEP-TAX-1.
	B-ACC-DEP = FA-MAST.ACC-DEP-TAX1.
	E-ACC-DEP = FA-MAST.ACC-DEP-TAX1 + CY-DEP-TAX-1.
END.

IF BOOKTAX = "3":U /* Tax2 */ THEN DO:
	TMETHOD  = FA-MAST.METHOD-TAX-2.
	TDEP-BASIS = FA-MAST.DEP-BASIS-T2.
	TCY-DEP-BOOK = FA-MAST.CY-DEP-TAX-2.
	B-ACC-DEP = FA-MAST.ACC-DEP-TAX2.
	E-ACC-DEP = FA-MAST.ACC-DEP-TAX2 + CY-DEP-TAX-2.
END.


  LINESIZE = 4.            /* LINES PER PRINT GROUP IN THE BODY */
  IF FIRST-PAGE THEN DO:
    TGL-CODE = FA-MAST.GL-CODE.
    tsort1 = fa-MAST.SORT-CODE1.
    TSORT2 = FA-MAST.SORT-CODE2.
    TLOC = FA-MAST.LOCATION.
  END.
  IF (LINECOUNT + LINESIZE GT BODYSIZE) THEN RUN fa/sch-bkh.p.
  LINECOUNT = LINECOUNT + LINESIZE.
  IF FIRST-PAGE AND TBY = "G":U THEN DO:
    find gl-mast where gl-mast.gl-code = fa-mast.gl-code
		   and gl-mast.fa-entity = fa-mast.fa-entity.  /* rpm 01/95 */
    tdesc = (gl-mast.gl-name).
    RUN fa/sch-bkh.p.
    linecount = linecount + 4.
  END.
  IF FIRST-PAGE AND TBY = "1":U THEN DO:
    find sort1 where sort1.sort-code1 = fa-mast.sort-code1.
    tdesc = (sort1.description).
    RUN fa/sch-bkh.p.
    linecount = linecount + 4.
  END.
  IF FIRST-PAGE AND TBY = "2":U THEN DO:
    find sort2 where sort2.sort-code2 = fa-mast.sort-code2.
    tdesc = (sort2.description).
    RUN fa/sch-bkh.p.
    linecount = linecount + 4.
  END.
  IF FIRST-PAGE AND TBY = "L":U THEN DO:
    find location where location.location = fa-mast.location.
    tdesc = (location.description).
    RUN fa/sch-bkh.p.
    linecount = linecount + 4.
  END.
  IF FIRST-PAGE AND TBY = "A":U THEN
  DO:
    tdesc = "BY ASSET CODE".
    RUN fa/sch-bkh.p.
    linecount = linecount + 4.
  END.

  FIRST-PAGE = FALSE.
  find location of fa-mast.
IF ASSET-STATUS EQ "A":U THEN STATUS-DESC = "    ACTIVE".
IF ASSET-STATUS EQ "R":U THEN STATUS-DESC = "   RETIRED".
IF DEP-BASIS-BK EQ (ACC-DEP-BOOK + CY-DEP-BOOK)
   and dep-basis-t1 eq (acc-dep-tax1 + cy-dep-tax-1)   /* rpm 01/95 */
   and dep-basis-t2 eq (acc-dep-tax2 + cy-dep-tax-2)   /* rpm 01/95 */
   THEN STATUS-DESC = " FULLY DEP".
IF ASSET-STATUS EQ "I":U THEN STATUS-DESC = " INACTIVE".


  IF TGL-CODE NE fa-mast.GL-CODE and tby = "G":U THEN DO:
    DISPLAY "---------------" AT 85
     "---------------" AT 100
     "---------------" AT 115
    SKIP
    TCOST-BOOK AT 85 TTCY-DEP-BOOK AT 100
    TB-ACC-DEP AT 115 TTDEP-BASIS AT 85  TE-ACC-DEP AT 115 SKIP
    "---------------" AT 85
     "---------------" AT 100
     "---------------" AT 115 with {&UGUI-RPT} frame frame1
     no-attr-space no-box no-labels width 255.

TCOST-BOOK = 0.
TTCY-DEP-BOOK  = 0.
TB-ACC-DEP = 0.
TE-ACC-DEP = 0.
TTDEP-BASIS = 0.
    LINECOUNT = LINECOUNT + 5.
    TGL-CODE = fa-mast.GL-CODE.
    find gl-mast where gl-mast.gl-code = fa-mast.gl-code
		   and gl-mast.fa-entity = fa-mast.fa-entity.  /* rpm 01/95 */
    tdesc = (gl-mast.gl-name).
    RUN fa/sch-bkh.p.
    linecount = linecount + 4.
  END.

  IF Tsort1   NE fa-mast.sort-code1 and tby = "1":U THEN DO:
    DISPLAY "---------------" AT 85
     "---------------" AT 100
     "---------------" AT 115
    SKIP
    TCOST-BOOK AT 85 TTCY-DEP-BOOK AT 100
    TB-ACC-DEP AT 115 TTDEP-BASIS AT 85  TE-ACC-DEP AT 115 SKIP
    "---------------" AT 85
     "---------------" AT 100
     "---------------" AT 115 with {&UGUI-RPT} frame frame2
     no-attr-space no-box no-labels width 255.
    LINECOUNT = LINECOUNT + 5.
TCOST-BOOK = 0.
TTCY-DEP-BOOK  = 0.
TB-ACC-DEP = 0.
TE-ACC-DEP = 0.
TTDEP-BASIS = 0.
    Tsort1   = fa-mast.sort-code1.
    find sort1 where sort1.sort-code1 = fa-mast.sort-code1.
    tdesc = (sort1.description).
    RUN fa/sch-bkh.p.
    linecount = linecount + 4.
  END.

  IF Tsort2   NE fa-mast.sort-code2 and tby = "2":U THEN DO:
    DISPLAY "---------------" AT 85
     "---------------" AT 100
     "---------------" AT 115
    SKIP
    TCOST-BOOK AT 85 TTCY-DEP-BOOK AT 100
    TB-ACC-DEP AT 115 TTDEP-BASIS AT 85  TE-ACC-DEP AT 115 SKIP
    "---------------" AT 85
     "---------------" AT 100
     "---------------" AT 115 with {&UGUI-RPT} frame frame3
     no-attr-space no-box no-labels width 255.
    LINECOUNT = LINECOUNT + 5.
TCOST-BOOK = 0.
TTCY-DEP-BOOK  = 0.
TB-ACC-DEP = 0.
TE-ACC-DEP = 0.
TTDEP-BASIS = 0.
    Tsort2   = fa-mast.sort-code2.
    find sort2 where sort2.sort-code2 = fa-mast.sort-code2.
    tdesc = (sort2.description) .
    RUN fa/sch-bkh.p.
    linecount = linecount + 4.
  END.

  IF Tloc     NE fa-mast.location and tby = "l":U THEN DO:
    DISPLAY "---------------" AT 85
     "---------------" AT 100
     "---------------" AT 115
    SKIP
    TCOST-BOOK AT 85 TTCY-DEP-BOOK AT 100
    TB-ACC-DEP AT 115 TTDEP-BASIS AT 85  TE-ACC-DEP AT 115 SKIP
    "---------------" AT 85
     "---------------" AT 100
     "---------------" AT 115 with {&UGUI-RPT} frame frame4
     no-attr-space no-box no-labels width 255.
    LINECOUNT = LINECOUNT + 5.
TCOST-BOOK = 0.
TTCY-DEP-BOOK  = 0.
TB-ACC-DEP = 0.
TE-ACC-DEP = 0.
TTDEP-BASIS = 0.
    Tloc = fa-mast.location.
    find location where location.location = fa-mast.location.
    tdesc  = location.description.
    RUN fa/sch-bkh.p.
    linecount = linecount + 4.
  END.

DISPLAY
SKIP
ASSET-CODE              AT 1
ASSET-DESC              AT 10
DATE-SERVICE            AT 42
NEW-USED                AT 54
fa-mast.entity-code     to 68       /* rpm 01/95 */
SALVAGE                 AT 70
COST-BOOK               AT 85
TCY-DEP-BOOK            AT 100
B-ACC-DEP               AT 115
SKIP
fa-mast.GL-CODE         AT 10
TMETHOD                 AT 21
FA-MAST.LOCATION        AT 29
BUSINESS-%              AT 42
LIFE-BOOK               AT 51
YR-OF-DEPR              AT 55
STATUS-DESC             AT 59
ITC-AMT                 to 83       /* rpm 01/95 */
TDEP-BASIS      AT 85
E-ACC-DEP      AT 115
SKIP
FA-MAST.SORT-CODE1      AT 10
FA-MAST.SORT-CODE2      AT 42
fa-mast.sec-179         at 70       /* rpm 01/95 */

SKIP(1) with {&UGUI-RPT}.
  ACCUM 1 (COUNT).

TCOST-BOOK = COST-BOOK + TCOST-BOOK.
TTCY-DEP-BOOK  = TTCY-DEP-BOOK + TCY-DEP-BOOK.
TB-ACC-DEP = TB-ACC-DEP + B-ACC-DEP.
TE-ACC-DEP = TE-ACC-DEP + E-ACC-DEP.
TTDEP-BASIS = TTDEP-BASIS + TDEP-BASIS.

FCOST-BOOK = COST-BOOK + FCOST-BOOK.
FTCY-DEP-BOOK  = FTCY-DEP-BOOK + TCY-DEP-BOOK.
FB-ACC-DEP = FB-ACC-DEP + B-ACC-DEP.
FE-ACC-DEP = FE-ACC-DEP + E-ACC-DEP.
FTDEP-BASIS = FTDEP-BASIS + TDEP-BASIS.


END.                       /* END OF MAIN-LOOP */

    DISPLAY skip(3) "---------------" AT 85
     "---------------" AT 100
     "---------------" AT 115
    SKIP
    TCOST-BOOK AT 85 TTCY-DEP-BOOK AT 100
    TB-ACC-DEP AT 115 TTDEP-BASIS AT 85  TE-ACC-DEP AT 115 SKIP
    "---------------" AT 85
     "---------------" AT 100
     "---------------" AT 115 with {&UGUI-RPT} frame frame5
     no-attr-space no-box no-labels width 255.
			   /* PRINT REPORT TOTALS IF APPLICABLE */

REC-COUNT = ACCUM COUNT 1.
LINECOUNT = LINECOUNT + 8.
IF TBY NE "A":U THEN DO:
   tdesc = "REPORT TOTALS FOR ALL ASSETS".
   RUN fa/sch-bkh.p.

    DISPLAY skip(3) "---------------" AT 85
     "---------------" AT 100
     "---------------" AT 115
    SKIP
    FCOST-BOOK AT 85 FTCY-DEP-BOOK AT 100
    FB-ACC-DEP AT 115 FTDEP-BASIS AT 85  FE-ACC-DEP AT 115 SKIP
    "---------------" AT 85
     "---------------" AT 100
     "---------------" AT 115 with {&UGUI-RPT} frame frame6
     no-attr-space no-box no-labels width 255.
  LINECOUNT = LINECOUNT + 8.
END.
RUN fa/rschf.p.             /*END-OF-REPORT STANDARD MODULE */
