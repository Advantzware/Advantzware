
{sys/form/r-top3w.f}

/*form header skip(1) with frame r-top.*/

def var v-fcust like cust.cust-no extent 2 init ["","zzzzzzzz"] no-undo.
def var v-ford-no as int format ">>>>>>" extent 2 init [0,999999] no-undo.
def var v-fdate as date extent 2 format "99/99/9999" init [today, 12/31/9999] no-undo.
def var v-fitem as char format "x(15)" extent 2 init ["","zzzzzzzzzzzzzzz"] no-undo.
def var v-fsman as char format "xxx" extent 2 init ["","zzz"] no-undo.
def var v-fcarr as char format "x(5)" extent 2 init ["","zzzzz"] no-undo.
DEF VAR v-floc AS CHAR FORMAT 'X(5)' EXTENT 2 INIT ['','zzzzz'] NO-UNDO.
def var v-ponum as log init yes no-undo.
def var v-sort as char format "!" init "C" no-undo.
def var v-print as char format "!" init "I" no-undo.
def var v-types as char format "x(7)" init "PALSBIC" no-undo.
def var v-comps as log init no no-undo.
def var v-by-job as log init no no-undo.
def var chosen as int init {1} no-undo.
def var v-qty like oe-rel.qty no-undo.
def var v-date like oe-rel.rel-date no-undo.
def var v-po-no like oe-rel.po-no no-undo.
def var v-rel-no like oe-rel.rel-no no-undo.
def var v-ship-id like oe-rel.ship-id no-undo.
def var v-carrier like oe-rel.carrier no-undo.  
def var v-type as char no-undo.
def var v-tot-qty as int format "->>>,>>>,>>9" EXTENT 2 no-undo.
def var v-tot-val as int format "->>>,>>>,>>9" extent 2 no-undo.
def var v-tot-msf as dec format "->>>,>>9.999" extent 2 no-undo.
DEF VAR ld-qty-ord AS DEC FORMAT ">>>,>>>,>>9" NO-UNDO.
DEF VAR ld-qty-rec AS DEC FORMAT ">>>,>>>,>>9" NO-UNDO.
DEF VAR ll-po AS LOG NO-UNDO.
DEF VAR lv-text AS CHAR NO-UNDO.
DEF VAR v-qty-opt AS CHAR NO-UNDO.
DEFINE VARIABLE cBeginCsr as CHARACTER NO-UNDO .
DEFINE VARIABLE cEndCsr as CHARACTER NO-UNDO .

def workfile w-ord
  field ord-no like oe-ord.ord-no
  field est-no like oe-ord.est-no
  field onh-qty like itemfg.q-onh
  field cust-no like oe-ord.cust-no
  field cust-name like oe-ord.cust-name
  field part-no like oe-ordl.part-no
  field i-no like oe-ordl.i-no
  field i-name like oe-ordl.i-name
  field qty like oe-ordl.qty
  field cost like oe-ordl.cost
  field price like oe-ordl.price
  field t-price like oe-ordl.t-price format "->>,>>>,>>9"
  field rel-qty like oe-rel.qty
  field rel-date as char format "x(9)"
  field job as char format "x(9)"
  field job-no like oe-ordl.job-no
  field job-no2 like oe-ordl.job-no2
  field rel-no like oe-rel.rel-no
  field ship-id like oe-rel.ship-id
  field po-num like oe-ordl.po-no
  field ord-qty like oe-ordl.qty
  field shp-qty like oe-ordl.ship-qty
  field msf as dec format "->>9.999"
  field component as int
  field prom-code like oe-ordl.prom-code FORMAT 'X(5)'
  field last-date like oe-ord.last-date format "99/99/99"
  field carrier like oe-relh.carrier
  field is-a-component like oe-ordl.is-a-component
  field palls as int format "->>,>>>,>>9"
  FIELD xls-rel-date  LIKE oe-rel.rel-date format "99/99/99"
  FIELD xls-status    AS CHAR
  field prom-date like oe-ordl.prom-date.

def buffer b-w-ord for w-ord.

{fg/fullset.i new}

{custom/formtext.i NEW}
