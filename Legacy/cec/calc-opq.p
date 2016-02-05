/*------------------------------------------------------------------------------
  Purpose: GUI cec/print4.p     copied from Ch_UI & browsers/probe.w.print4 procedure
           called from jc-calc.p 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER ipcocode AS CHAR NO-UNDO.
DEF INPUT PARAMETER iplocode AS CHAR NO-UNDO.
DEF INPUT PARAMETER ipr-est AS ROWID NO-UNDO.
DEF INPUT PARAMETER ipr-ef AS ROWID NO-UNDO.
DEF INPUT PARAMETER ipr-eb AS ROWID NO-UNDO.

DEF SHARED TEMP-TABLE tt-est-op LIKE est-op.  

def new shared var cocode as cha NO-UNDO INIT '001'.
def new shared var locode as cha NO-UNDO INIT 'main'.
def new shared var k as int no-undo.
def var i as int no-undo.
def var j as int no-undo.
def new shared var lv-qty as int no-undo.  /* oe-ordl.qty */
def new shared buffer xest for est.
def new shared buffer xef for ef.
def new shared buffer xeb for eb.
def TEMP-TABLE q-sort no-undo field qty as dec field rel as int.
def TEMP-TABLE q-sort1 no-undo field qty as dec field rel as int.
def TEMP-TABLE q-sort2 no-undo field qty as dec field rel as int.
cocode = ipcocode.
locode = iplocode.
def new shared buffer xop for est-op.
{cec/print4.i "new shared" "new shared"}
{cec/print42.i "new shared"}
FIND FIRST xest WHERE ROWID(xest) = ipr-est NO-ERROR.
FIND FIRST xef WHERE ROWID(xef) = ipr-ef NO-ERROR.

def var v-gsa as log init no no-undo.

FIND FIRST xeb WHERE ROWID(xeb) = ipr-eb NO-ERROR.


def var v-bqty as int no-undo.
def var v-module as char format "x(60)" no-undo.
def new shared var v-drop-rc as log no-undo.
def new shared var k_frac as dec init "6.25" no-undo.
def new shared var day_str as cha form "x(8)" no-undo.
def new shared var tim_str as cha form "x(8)" no-undo.
def new shared var maxpage as int form ">9" no-undo.
def new shared var tmp-dir as cha no-undo.
def new shared var col-norm as cha init "White/Blue" no-undo.
def new shared var v-prep-mat AS DEC no-undo.  /* for probemk cost */
def new shared var v-prep-lab AS DEC no-undo.
def var ls-outfile as cha no-undo.
def var ls-probetime as cha no-undo.  /* time display */
def var v as int no-undo.
def new shared var v-qtty like qtty no-undo.

DEF NEW SHARED VAR qty AS INT NO-UNDO.
DEF NEW SHARED VAR v-shared-rel AS INT NO-UNDO.

def new shared workfile w-form
    field form-no like xeb.form-no
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
                                  field rel like rels.

DEF TEMP-TABLE tt-bqty NO-UNDO FIELD tt-bqty AS INT FIELD tt-brel AS INT.

{sys/inc/f16to32.i}

lv-ef-recid = recid(xef).

find first sys-ctrl where
    sys-ctrl.company eq cocode AND
    sys-ctrl.name    eq "CEBROWSE"
    no-lock no-error.

  if not avail sys-ctrl then DO TRANSACTION:
        create sys-ctrl.
        assign sys-ctrl.company = cocode
               sys-ctrl.name    = "CEBROWSE"
               sys-ctrl.descrip = "# of Records to be displayed in browser"
               sys-ctrl.log-fld = YES
               sys-ctrl.char-fld = "CE"
               sys-ctrl.int-fld = 30.
        
  end.

IF sys-ctrl.char-fld NE "" THEN
   tmp-dir = sys-ctrl.char-fld.
ELSE
   tmp-dir = "users\".

IF LOOKUP(SUBSTRING(tmp-dir,LENGTH(tmp-dir)),"\,/") EQ 0 THEN
   tmp-dir = tmp-dir + "\".

tmp-dir = REPLACE(tmp-dir,"/","\").

{cec/print4p.i}
FOR EACH tt-est-op.
    DELETE tt-est-op.
END.
FOR EACH est-op WHERE est-op.company = cocode
    AND est-op.est-no = xest.est-no AND est-op.LINE GT 500
   .
  CREATE tt-est-op.
  BUFFER-COPY est-op TO tt-est-op.

END.

      
