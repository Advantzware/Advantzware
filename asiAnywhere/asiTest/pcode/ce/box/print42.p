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

def workfile q-sort no-undo field qty as dec field rel as int.
def workfile q-sort1 no-undo field qty as dec field rel as int.
def workfile q-sort2 no-undo field qty as dec field rel as int.
def new shared temp-table tt-qtty field qtty like qtty
                                  field rel like rels.

DEF SHARED VAR qty AS INT NO-UNDO.

find first sys-ctrl where
    sys-ctrl.company eq cocode AND
    sys-ctrl.name    eq "CEBROWSE"
    no-lock no-error.

  /*if not avail sys-ctrl then DO TRANSACTION:
        create sys-ctrl.
        assign sys-ctrl.company = cocode
               sys-ctrl.name    = "CEBROWSE"
               sys-ctrl.descrip = "# of Records to be displayed in browser"
               sys-ctrl.log-fld = YES
               sys-ctrl.char-fld = "CE"
               sys-ctrl.int-fld = 30.
        
  end.*/

IF sys-ctrl.char-fld NE "" THEN
   tmp-dir = sys-ctrl.char-fld.
ELSE
   tmp-dir = "users\".

   IF INDEX(tmp-dir ,'P:',1) > 0 THEN ASSIGN
     tmp-dir  = REPLACE(tmp-dir ,'P:',"D:").

IF LOOKUP(SUBSTRING(tmp-dir,LENGTH(tmp-dir)),"\,/") EQ 0 THEN
   tmp-dir = tmp-dir + "\".

tmp-dir = REPLACE(tmp-dir,"/","\").

{ce/print42p.i}

/* end ---------------------------------- copr. 1992  advanced software, inc. */
