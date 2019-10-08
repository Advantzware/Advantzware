/* ----------------------------------------------- oe/rep/acknowl.i 01/99 JLF */
/* ORDER ACKNOLEDGEMENT                                                       */
/* for ORDER STATUS = (R), (U)                                                */
/* -------------------------------------------------------------------------- */

def {1} shared var fcust as char init "".
def {1} shared var tcust like fcust init "zzzzzzzz".
def {1} shared var ford-no as int format  ">>>>>>".
def {1} shared var tord-no like ford-no init 999999.
def {1} shared var fdate as date format "99/99/9999" init "01/01/0001".
def {1} shared var tdate like fdate init today.
def {1} shared var v-reprint as log init no.
def {1} shared var v-prntinst as log init yes.
def {1} shared var v-schrel as log init yes.
def {1} shared var v-actrel as log init no.
def {1} shared var v-shipto as log init yes.

def {1} SHARED var v-print-fmt  as char NO-UNDO.
def {1} SHARED var v-print-head as LOG NO-UNDO.
DEF {1} SHARED VAR v-lines-per-page AS INT NO-UNDO.
def {1} shared var v-term-id as char.

/* gdm - 04160907 */
DEF {1} SHARED VAR v-terms    AS LOG NO-UNDO.
DEF {1} SHARED VAR v-termfile AS CHAR NO-UNDO.
DEF {1} SHARED VAR v-print-pen-notes AS LOG NO-UNDO.
DEF {1} SHARED VAR v-hide-price AS LOG NO-UNDO.
DEF {1} SHARED VAR slBillNotes AS LOG NO-UNDO.


DEF VAR ld-date AS DATE NO-UNDO.
DEF VAR lv-stat AS CHAR NO-UNDO.

/* end ---------------------------------- copr. 1999  Advanced Software, Inc. */
