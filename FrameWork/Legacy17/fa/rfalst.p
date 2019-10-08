/* fa/rfalst.p - Fixed Assets List */
/**************** Copyright Statement Follows ***************************
* (C) Copyright, 1996 - 1997 Foresight Software, Inc.  All Rights       *
* Reserved.  This is unpublished material and contains trade secrets    *
* and other confidential information and is subject to licensing and    *
* a confidentiality agreement.  The unauthorized possession, use,       *
* reproduction, reverse engineering, distribution, display, or          *
* disclosure of this material or of any information contained herein    *
* or any information derived from this material is strictly prohibited. *
************************************************************************/

DEF BUFFER fa-mast# for fa-mast.

DEF NEW SHARED VAR r-date as date no-undo.
DEF NEW SHARED VAR r-output as char no-undo.
DEF NEW SHARED VAR p-output as char no-undo.
DEF NEW SHARED VAR r-memo as char no-undo.
DEF NEW SHARED VAR pcap-list as char initial "," no-undo.
DEF NEW SHARED VAR pcap-sequence as char extent 100 no-undo.
DEF NEW SHARED VAR slave as logical no-undo.
DEF NEW SHARED VAR z_tempfile as char no-undo.

DEF NEW SHARED VAR beg-code like fa-mast.asset-code label "From Asset Code" extent 0 no-undo.
DEF NEW SHARED VAR end-code like fa-mast.asset-code label "To Asset code" initial "zzzzzzzz" extent 0 no-undo.
DEF NEW SHARED VAR torder as character format "x(1)" label "Sort Order (ALG12)" initial "A" extent 0 no-undo.
DEF NEW SHARED VAR beg-gl like fa-mast.gl-code label "From GL Group" extent 0 no-undo.
DEF NEW SHARED VAR end-gl like fa-mast.gl-code label "To GL Group" initial "zzzzzzzz" extent 0 no-undo.
DEF NEW SHARED VAR beg-loc like fa-mast.location label "From Location" extent 0 no-undo.
DEF NEW SHARED VAR end-loc like fa-mast.location label "To Location" initial "zzzz" extent 0 no-undo.
DEF NEW SHARED VAR sort1 as character format "x(45)" label "Sort Code 1" extent 0 no-undo.
DEF NEW SHARED VAR sort2 as character format "x(45)" label "Sort Code 2" extent 0 no-undo.
DEF NEW SHARED VAR t-status as character format "x(4)" label "Status (AIRZ)" initial "AIRZ" extent 0 no-undo.
DEF NEW SHARED VAR by-list as character format "x(1)" label "Par,Child,Both,Relation,r-Only" initial "B" extent 0 no-undo.

DEF VAR wpage-number as integer.
DEF VAR msg as character format "x(25)".
DEF VAR liste# as character format "x(5)".
DEF VAR acc-book like fa-mast.acc-dep-book.
DEF VAR acc-tax1 like acc-book.
DEF VAR acc-tax2 like acc-book.

form header "Advantzware Fixed Assets"
    "Page:"                     to 117 space(4)
    page-number  format ">>>>9"
    "Fixed Assets Master Listing"     at 1
    "As-of date:"               to 117
    r-date
    string(time,"hh:mm") format "x(5)"
    fill("-",132) format "x(132)"

    "Asset"                 AT 1
    "Asset Description"     AT 10
    "Acquired"              AT 42
    "New Used"              AT 54 /* YR2000 */
    "Salvage"               AT 67
    "Life-Bk"               AT 79
    "Cost Book"             AT 91
    "Dep-Basis-Bk"          AT 103
    "Acc Dep Book"        AT 118
    SKIP
    "Code"                  AT 1
    "GL Group"              AT 10
    "Loc"                   AT 19
    "Yr/Depr"               at 23
    "Service"               AT 42
    "Status"                AT 54   
    "Entity"                AT 67   
    "Life-T1"               AT 79
    " "                     at 91
    "Dep-Basis-T1"          AT 103
    "Acc Dep Tax1"          AT 118
    SKIP
    "Parent #"              at 1
    "Book  Tax1    Tax2"    AT 10
    "P.O. No."              AT 42
    "Bus%"                  TO 57
    "ITC Amt"               AT 67
    "Life-T2"               AT 79
    "No. Assets"            TO 99
    "Dep-Basis-T2"          AT 103
    "Acc Dep Tax2"          AT 118
    SKIP
    "Sort1"                 AT 10
    "Sort2   "              AT 25
    "Serial#"               at 34
    "Book Life"             at 79
    "Sl-Conv-Amt"           at 103
    "Last Auto Depr"        at 118
    SKIP
    "Exch. Rate"            at 1     /* rpm 01/95 */
    "Job no"                at 15    /* rpm 01/95 */
    "Adjustment"            at 34    /* rpm 01/95 */
    "Tag-from      Tag-to"  at 54    /* rpm 01/95 */
    fill("-",132) format "x(132)" skip(1)
    with stream-io page-top no-box width 132 frame top no-attr-space.

/* PAGE FOOTER */
form header skip(1) "Continued on page:" page-number + 1 format ">>>>9"
     with stream-io page-bottom no-box no-labels frame footer width 132 no-attr-space.

view frame top.
if page-size ne 0 then view frame footer.
page.

if by-list = "P":U then liste# = "no":U.
if by-list = "C":U then liste# = "yes":U.
if by-list = "B":U then liste# = "yesno":U.
if by-list = "R":U then liste# = "no":U.
if by-list = "O":U then liste# = "no":U.

MAIN-LOOP:
FOR EACH FA-MAST no-lock WHERE 
    FA-MAST.ASSET-CODE GE BEG-CODE AND
    FA-MAST.ASSET-CODE LE END-CODE AND
    fa-mast.fa-entity = fa-control.fa-entity and
    FA-MAST.GL-CODE GE BEG-GL AND FA-MAST.GL-CODE LE END-GL AND
    FA-MAST.LOCATION GE BEG-LOC AND FA-MAST.LOCATION LE END-LOC AND
    (IF SORT1 NE "" THEN CAN-DO(SORT1,SORT-CODE1) ELSE TRUE) AND
    (IF SORT2 NE "" THEN CAN-DO(SORT2,SORT-CODE2) ELSE TRUE) AND
    (INDEX(t-status,FA-MAST.ASSET-STATUS) NE 0) and 
    (index(liste#,string(fa-mast.child-par)) ne 0)
    BY   IF torder = "l":U THEN location
    ELSE IF torder = "1":U THEN sort-code1
    ELSE IF torder = "2":U THEN sort-code2
    ELSE IF torder = "g":U THEN gl-code
    ELSE asset-code
    ON ERROR UNDO, LEAVE MAIN-LOOP WITH stream-io FRAME BODY
    NO-ATTR-SPACE NO-LABELS NO-BOX WIDTH 255:  /* mod-sh 11/91 */

    {pt/newpage main-loop string(fa-mast.asset-code)}

    if by-list = "O":U then do:
        find first fa-mast# where 
            fa-mast#.par-asset = fa-mast.asset-code and 
            fa-mast#.fa-entity = fa-mast.fa-entity
            no-lock no-error.
        if not available fa-mast# then next.
    end.

    find location of fa-mast no-lock no-error.
    if fa-mast.asset-status ne "R":U then do:
        acc-book = fa-mast.acc-dep-book + fa-mast.cy-dep-book.
        acc-tax1 = fa-mast.acc-dep-tax1 + fa-mast.cy-dep-tax-1.
        acc-tax2 = fa-mast.acc-dep-tax2 + fa-mast.cy-dep-tax-2.
    end.
    else do:
        acc-book = fa-mast.acc-dep-book .
        acc-tax1 = fa-mast.acc-dep-tax1 .
        acc-tax2 = fa-mast.acc-dep-tax2 .
    end.

    DISPLAY
        SKIP
        fa-mast.ASSET-CODE              AT 1
        fa-mast.ASSET-DESC              AT 10
        fa-mast.DATE-AQUIRED            AT 42
        fa-mast.NEW-USED                AT 54
        fa-mast.SALVAGE                 AT 60
        fa-mast.LIFE-BOOK               AT 81
        fa-mast.COST-BOOK               AT 86
        fa-mast.DEP-BASIS-BK            AT 101
        ACC-BOOK            AT 116
        SKIP
        fa-mast.child-par               at 1
        fa-mast.GL-CODE                 AT 10
        FA-MAST.LOCATION        AT 19
        fa-mast.yr-of-depr      at 25
        fa-mast.DATE-SERVICE            AT 42
        fa-mast.ASSET-STATUS            AT 54
        fa-mast.entity-code             at 67
        fa-mast.LIFE-TAX-1              AT 83
        fa-mast.DEP-BASIS-T1            AT 101
        ACC-TAX1            AT 116
        SKIP
        fa-mast.par-asset       at 1
        FA-MAST.METHOD-BOOK     AT 10
        FA-MAST.METHOD-TAX-1    AT 18
        FA-MAST.METHOD-TAX-2    AT 25
        fa-mast.PURCH-ORDER#    format "x(8)"        AT 42
        fa-mast.business-%  format "999"  to 57  /* inserted by RAW on 4/9/90. */
        fa-mast.ITC-AMT    format "->,>>>,>>9.99"   to 73
        fa-mast.LIFE-TAX-2              AT 83
        fa-mast.multiple  to 95  format "99"      /* inserted by RAW on 4/9/90. */
        fa-mast.DEP-BASIS-T2   format "->,>>>,>>9.99"   to 114
        ACC-TAX2            AT 116
        SKIP
        fa-mast.sort-code1  format "x(8)"  at 10
        fa-mast.sort-code2  format "x(8)"  at 25
        fa-mast.serial#
        fa-mast.mth-year       at 81
        fa-mast.sl-conv-amt    to 114
        fa-mast.last-autodepr-date to 130
        SKIP                              /* rpm 01/95 */
        fa-mast.exch-rate      at 1       /* rpm 01/95 */
        fa-mast.job-no         at 15      /* rpm 01/95 */
        fa-mast.sec-179        at 25      /* rpm 01/95 */
        with stream-io no-attr-space no-box.

    for each fa-tags no-lock where 
        fa-tags.asset-code eq fa-mast.asset-code and 
        fa-tags.fa-entity eq fa-mast.fa-entity:

        display 
            fa-tags.tag-nof at 53 space (4)
            fa-tags.tag-not
            skip with stream-io no-labels no-attr-space no-box.
    end.

    ACCUM 1 (COUNT).

    if by-list = "R":U  or by-list = "O":U then do:
        for each fa-mast# no-lock where 
            fa-mast#.par-asset = fa-mast.asset-code no-lock
            with stream-io NO-ATTR-SPACE NO-LABELS NO-BOX WIDTH 255:  /* mod-sh 11/91 */

            if fa-mast.asset-status ne "R":U then do:
                acc-book = fa-mast#.acc-dep-book + fa-mast#.cy-dep-book.
                acc-tax1 = fa-mast#.acc-dep-tax1 + fa-mast#.cy-dep-tax-1.
                acc-tax2 = fa-mast#.acc-dep-tax2 + fa-mast#.cy-dep-tax-2.
            end.
            else do:
                acc-book = fa-mast#.acc-dep-book .
                acc-tax1 = fa-mast#.acc-dep-tax1 .
                acc-tax2 = fa-mast#.acc-dep-tax2 .
            end.
  
            find location of fa-mast# no-lock no-error.

            DISPLAY
                SKIP
                fa-mast#.ASSET-CODE              AT 1
                fa-mast#.ASSET-DESC              AT 10
                fa-mast#.DATE-AQUIRED            AT 42
                fa-mast#.NEW-USED                AT 54
                fa-mast#.SALVAGE                 AT 60
                fa-mast#.LIFE-BOOK               AT 81
                fa-mast#.COST-BOOK               AT 86
                fa-mast#.DEP-BASIS-BK            AT 101
                ACC-BOOK            AT 116
                SKIP
                fa-mast#.child-par               at 1
                fa-mast#.GL-CODE                 AT 10
                fa-mast#.LOCATION        AT 19
                fa-mast#.yr-of-depr      at 25
                fa-mast#.DATE-SERVICE            AT 42
                fa-mast#.ASSET-STATUS            AT 54
                fa-mast#.fa-entity          at 67
                fa-mast#.LIFE-TAX-1              AT 83
                fa-mast#.DEP-BASIS-T1            AT 101
                ACC-TAX1            AT 116
                SKIP
                fa-mast#.par-asset       at 1
                fa-mast#.METHOD-BOOK     AT 10
                fa-mast#.METHOD-TAX-1    AT 18
                fa-mast#.METHOD-TAX-2    AT 25
                fa-mast#.PURCH-ORDER#    format "x(8)"        AT 42
                fa-mast#.business-%  format "999"  to 57  /* inserted by RAW on 4/9/90. */
                fa-mast#.ITC-AMT    format "->,>>>,>>9.99"   to 73
                fa-mast#.LIFE-TAX-2              AT 83
                fa-mast#.multiple  to 95  format "99"      /* inserted by RAW on 4/9/90. */
                fa-mast#.DEP-BASIS-T2   format "->,>>>,>>9.99"   to 114
                ACC-TAX2            AT 116
                SKIP
                fa-mast#.sort-code1  format "x(8)"  at 10
                fa-mast#.sort-code2  format "x(8)"  at 25
                fa-mast#.serial#
                fa-mast#.mth-year       at 81
                fa-mast#.sl-conv-amt    to 114
                fa-mast#.last-autodepr-date to 130
                SKIP                               /* rpm 01/95 */
                fa-mast#.entity-code    at 2       /* rpm 01/95 */
                fa-mast#.job-no         at 15      /* rpm 01/95 */
                fa-mast#.sec-179        at 25      /* rpm 01/95 */
                with stream-io no-attr-space no-box.

            for each fa-tags no-lock where 
                fa-tags.asset-code eq fa-mast#.asset-code and 
                fa-tags.fa-entity eq fa-mast#.fa-entity:
                display 
                    fa-tags.tag-nof at 53 space (4)
                    fa-tags.tag-not
                    skip with stream-io no-labels no-attr-space no-box.
            end.

            ACCUM 1 (COUNT).
        end.   /* OF FOR EACH FA-MAST# */
    end.   /* OF IF SEE-CHILD      */

    display skip(1) with stream-io frame skipline.

END.                       /* END OF MAIN-LOOP */

/* PRINT REPORT TOTALS IF APPLICABLE */

if wpage-number eq 0 then display 
    " " with stream-io no-labels no-box frame last-frame no-attr-space.

form header 
    skip(1) "End-of-Report." accum count 1 format ">>>,>>9"
    "Record(s) Printed."
    if r-memo > "" then " Memo: " + r-memo else "" format "x(42)"
    with stream-io page-bottom no-labels no-box frame last-footer width 132.

hide frame footer.
view frame last-footer.
page.
