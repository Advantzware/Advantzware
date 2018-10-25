/* ------------------------------------------------ cec/box/print42.p 10/94 gb*/
/* copy of com for 2 sheet boxes                                              */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.
def new shared buffer xop for est-op.

{cec/print4.i shared "new shared"}
{cec/print42.i shared}

def new shared var v-qtty like qtty NO-UNDO.
def var v as INT NO-UNDO.
def new shared var v-do-gsa like do-gsa NO-UNDO.
def new shared var v-drop-rc as log no-undo.

def var v-brd-only like sys-ctrl.log-fld init NO NO-UNDO.
def var v-brd-cost as DEC NO-UNDO.
def var v-module as char format "x(60)" NO-UNDO.
def var v-gsa as LOG NO-UNDO.

def new shared workfile w-form
    field form-no like xef.form-no
    field min-msf as   log init no.

def TEMP-TABLE q-sort no-undo field qty as dec field rel as int.
def TEMP-TABLE q-sort1 no-undo field qty as dec field rel as int.
def TEMP-TABLE q-sort2 no-undo field qty as dec field rel as int.

def new shared temp-table tt-qtty field qtty like qtty
                                  field rel like rels
                                  FIELD lRunShip LIKE lRunShips.
def var lv-error as log no-undo.
def new shared var k_frac as dec init "6.25" no-undo.
def new shared var day_str as cha form "x(8)" no-undo.
def new shared var tim_str as cha form "x(8)" no-undo.
def new shared var maxpage as int form ">9" no-undo.
def new shared var tmp-dir as cha no-undo.
DEFINE NEW SHARED VARIABLE cCeBrowseBaseDir AS CHARACTER NO-UNDO.

def new shared var col-norm as cha init "White/Blue" no-undo.
def new shared var v-prep-mat like tprep-mat no-undo.  /* for probemk cost */
def new shared var v-prep-lab like tprep-lab no-undo.
def var ls-outfile as cha no-undo.
def var ls-probetime as cha no-undo.  /* time display */
DEF VAR lv-override AS LOG NO-UNDO.

DEF NEW SHARED VAR v-update-qty-gsa AS LOG NO-UNDO.
DEF NEW SHARED VAR ld-gsa-brd AS DEC NO-UNDO.
DEF NEW SHARED VAR ld-gsa-mat AS DEC NO-UNDO.
DEF NEW SHARED VAR ld-gsa-lab AS DEC NO-UNDO.

def var call_id as recid no-undo.  
def var fil_id as recid no-undo.

DEF SHARED VAR qty AS INT NO-UNDO.
DEF SHARED VAR v-shared-rel AS INT NO-UNDO.

find first xef where xef.company = xest.company
                 AND xef.est-no eq xest.est-no no-lock.
find first xeb where xeb.company = xest.company 
                 AND xeb.est-no   eq xest.est-no and
                     xeb.form-no eq xef.form-no no-lock.

RUN est/EstimateProcsOld.p (xest.company, OUTPUT cCeBrowseBaseDir, OUTPUT tmp-dir).
    
{sys/inc/f16to32.i}
{cec/print42p.i }

/* end ---------------------------------- copr. 1992  advanced software, inc. */
