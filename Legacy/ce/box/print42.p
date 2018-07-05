/* ------------------------------------------------- ce/box/print42.p 10/94 gb*/
/* copy of com for 2 sheet boxes                                              */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.
def new shared buffer xop for est-op.

{ce/print4.i shared "new shared"}

def new shared var v-do-gsa like do-gsa.
def new shared var tmp-dir as cha no-undo.
DEF NEW SHARED VAR maxpage AS INT NO-UNDO.
def new shared var day_str as cha form "x(10)" no-undo.
def new shared var tim_str as cha form "x(8)" no-undo.
def NEW SHARED var v-prep-mat AS DEC NO-UNDO.
def NEW SHARED var v-prep-lab AS DEC NO-UNDO.
def new shared var v-drop-rc as log no-undo.
DEF NEW SHARED VAR v-match-up AS DEC NO-UNDO.

def var v-module as char format "x(60)" no-undo.
DEF VAR CALL_id AS RECID NO-UNDO.
DEF VAR lv-error AS LOG NO-UNDO.
def var ls-outfile as cha no-undo.
def var ls-probetime as cha no-undo.  /* time display */
def var v-brd-cost as dec no-undo.
DEF VAR lv-override AS LOG NO-UNDO.

def new shared workfile w-form
    field form-no like ef.form-no
    field min-msf as   log init no.

def TEMP-TABLE q-sort no-undo field qty as dec field rel as int.
def TEMP-TABLE q-sort1 no-undo field qty as dec field rel as int.
def TEMP-TABLE q-sort2 no-undo field qty as dec field rel as int.
def new shared temp-table tt-qtty field qtty like qtty
                                  field rel like rels.

DEF SHARED VAR qty AS INT NO-UNDO.

DEF BUFFER b-cost FOR reftable.
DEF BUFFER b-qty FOR reftable.

DEF TEMP-TABLE tt-ei NO-UNDO
    FIELD run-qty AS DECIMAL DECIMALS 3 EXTENT 20
    FIELD run-cost AS DECIMAL DECIMALS 4 EXTENT 20.

DEFINE NEW SHARED VARIABLE cCEBrowseBaseDir AS CHARACTER NO-UNDO.    
RUN est/EstimateProcs.p (cocode, OUTPUT cCeBrowseBaseDir, OUTPUT tmp-dir).

{ce/print42p.i}

/* end ---------------------------------- copr. 1992  advanced software, inc. */
