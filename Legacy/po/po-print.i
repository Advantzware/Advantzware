
def new shared workfile w-box-design-line no-undo
  field line-no         like box-design-line.line-no
  field wscore-c        as char format "x(06)"
  field wscore-d        as dec decimals 2 format "999.99"
  field wcum-score-c    as char format "x(06)"
  field wcum-score-d    as dec decimals 2 format "999.99"
  field wscore-fld-id   as int.

def new shared var v-lscore-c     like box-design-hdr.lscore no-undo.
def new shared var v-lcum-score-c like box-design-hdr.lcum-score no-undo.
def new shared var v-num-lines    as int no-undo.

def {1} shared var v-start-po like po-ord.po-no label "Starting PO".
def {1} shared var v-end-po like po-ord.po-no label "Ending PO".
def {1} shared var v-start-vend like po-ord.vend-no label "Starting Vendor".
def {1} shared var v-end-vend like po-ord.vend-no label "Ending Vendor".
def {1} shared var v-reprint-po as log
                                label "Do you want to reprint the PO's?".
def {1} shared var v-printde-po as log
                               label "Do you want to print deleted line items?".
def {1} shared var v-repeat-print as int no-undo.
def {1} shared var v-print-sn as log label "Do you want to print Specification Notes?".
def {1} shared var v-corrugator as log label "Transfer to Corrugator?".

def {1} SHARED var v-shtsiz as log no-undo.
def {1} SHARED var v-pre-printed-forms as LOG NO-UNDO.
def {1} SHARED var v-company           as LOG NO-UNDO.
DEF {1} SHARED VAR v-faxprog AS cha NO-UNDO.
DEF {1} SHARED VAR v-faxnum AS cha FORM "x(20)" NO-UNDO.
DEF {1} SHARED VAR v-sendfax AS LOG INIT NO NO-UNDO.
DEF {1} SHARED VAR v-tmp-fax AS cha FORM "x(15)" NO-UNDO.
def {1} SHARED var v-print-fmt  as char NO-UNDO.
DEF {1} SHARED VAR v-lines-per-page AS INT NO-UNDO.
def {1} shared var v-term-id as char.
def {1} shared var v-summarize-by-item as LOG NO-UNDO.
DEF {1} SHARED VAR v-itemDescription AS LOG NO-UNDO.
DEF {1} SHARED VAR v-score-types AS LOG NO-UNDO.
DEF {1} SHARED VAR v-metric AS LOG NO-UNDO.
DEF {1} SHARED VAR v-print-terms AS LOG NO-UNDO.
DEF {1} SHARED VAR lCustCode AS LOG NO-UNDO.
DEF {1} SHARED VAR lPrintMach AS LOG NO-UNDO.


DEF VAR lv-val LIKE reftable.val EXTENT 20 NO-UNDO.
DEF VAR lv-typ LIKE reftable.dscr EXTENT 20 NO-UNDO.
DEF VAR lv-int AS INT NO-UNDO.

def var v-last-page         as   int.
def var v-page-tot          as   int.
DEF VAR v-slash             AS   CHAR NO-UNDO.

v-slash = IF OPSYS EQ "unix" THEN "~/" ELSE "~\".
