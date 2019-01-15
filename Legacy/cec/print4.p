/*------------------------------------------------------------------------------
  Purpose: GUI cec/print4.p     copied from Ch_UI & browsers/probe.w.print4 procedure
           called from jc-calc.p 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*{sys/inc/var.i shared} */
def shared var cocode as cha no-undo.
def shared var locode as cha no-undo.
def new shared var k as int no-undo.
def var i as int no-undo.
def var j as int no-undo.
def shared var lv-qty as int no-undo.  /* oe-ordl.qty */
def shared buffer xest for est.
def shared buffer xef for ef.
def shared buffer xeb for eb.
def TEMP-TABLE q-sort no-undo field qty as dec field rel as int.
def TEMP-TABLE q-sort1 no-undo field qty as dec field rel as int.
def TEMP-TABLE q-sort2 no-undo field qty as dec field rel as int.

def new shared buffer xop for est-op.
{cec/print4.i shared "new shared"}
{cec/print42.i shared}

def var v-gsa as log init no no-undo.

def var v-bqty as int no-undo.
def var v-module as char format "x(60)" no-undo.
def new shared var v-drop-rc as log no-undo.
def new shared var k_frac as dec init "6.25" no-undo.
def new shared var day_str as cha form "x(8)" no-undo.
def new shared var tim_str as cha form "x(8)" no-undo.
def new shared var maxpage as int form ">9" no-undo.
def new shared var tmp-dir as cha no-undo.
DEFINE NEW SHARED VARIABLE cCeBrowseBaseDir AS CHARACTER NO-UNDO.

def new shared var col-norm as cha init "White/Blue" no-undo.
def new shared var v-prep-mat AS DEC no-undo.  /* for probemk cost */
def new shared var v-prep-lab AS DEC no-undo.
def var ls-outfile as cha no-undo.
def var ls-probetime as cha no-undo.  /* time display */
def var v as int no-undo.
def new shared var v-qtty like qtty no-undo.

DEF SHARED VAR qty AS INT NO-UNDO.
DEF SHARED VAR v-shared-rel AS INT NO-UNDO.

def new shared workfile w-form
    field form-no like xef.form-no
    field min-msf as   log init no.

def var call_id as recid no-undo.  
def var fil_id as recid no-undo.
def var lv-error as log no-undo.
def var v-vend-no   like e-item-vend.vend-no init "".
DEF var v-vend-list AS CHAR NO-UNDO.
def var lv-ef-recid as recid no-undo.
def var v-brd-only like sys-ctrl.log-fld init no no-undo.
def var v-brd-cost as dec no-undo.
DEF VAR lv-override AS LOG NO-UNDO.
 
def new shared temp-table tt-qtty field qtty like qtty
                                  field rel like rels
                                  FIELD lRunShip LIKE lRunShips.

DEF TEMP-TABLE tt-bqty NO-UNDO FIELD tt-bqty AS INT FIELD tt-brel AS INT.

{sys/inc/f16to32.i}

lv-ef-recid = recid(xef).

RUN est/EstimateProcs.p (cocode, OUTPUT cCeBrowseBaseDir, OUTPUT tmp-dir).

{cec/print4p.i}
