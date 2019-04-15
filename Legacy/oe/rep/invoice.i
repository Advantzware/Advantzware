/* invoice.i */

def {1} shared var fcust like inv-head.cust-no.
def {1} shared var tcust like fcust init "zzzzzzzz".
def {1} shared var finv like inv-head.inv-no format ">>>>>>".
def {1} shared var tinv like finv init 999999.
def {1} shared var v-prntinst as log init yes.
def {1} shared var v-reprint as log init no.
def {1} shared var v-sort as log format "Customer/BOL" init yes.
def {1} shared var v-term-id as CHAR NO-UNDO.
def {1} shared var fdate as date init today no-undo.
def {1} shared var tdate like fdate.
def {1} shared var fbol like oe-bolh.bol-no format ">>>>>>" init 0 no-undo.
def {1} shared var tbol like oe-bolh.bol-no init 999999 no-undo.
def {1} SHARED var v-print-fmt  as char no-undo format 'x'.
def {1} SHARED var v-print-head as log no-undo.
DEF {1} SHARED VAR v-lines-per-page AS INT NO-UNDO.
DEF {1} SHARED VAR v-print-dept AS LOG NO-UNDO.
DEF {1} SHARED VAR v-depts AS CHAR NO-UNDO.
def {1} SHARED var dPrintFmtDec  as Decimal no-undo .
def {1} SHARED var lPrintQtyAll  as LOGICAL no-undo .

def var v-last-page as   int no-undo.
def var v-page-tot  as   int no-undo.
def var v-sort-name as   log no-undo.

/* rstark 05291402 */
&IF DEFINED(cXML) EQ 0 &THEN
/* rstark 05291402 */
do transaction:
  {sys/inc/invcopys.i}
end.
/* rstark 05291402 */
&ENDIF
/* rstark 05291402 */
