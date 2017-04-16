
def {1} shared buffer xoe-ord  for oe-ord.
def {1} shared buffer xoe-relh for oe-relh.
def {1} shared buffer yoe-relh for oe-relh.
def {1} shared buffer xoe-rell for oe-rell.

{oe/oe-bolpi.i "{1}"}

def {1} shared var v-u-inv like oe-ctrl.u-inv init no.
def {1} shared var save_id as recid.
def {1} shared var time_stamp as char.
def {1} shared var v-tot-post as int format ">>>9".
def {1} shared var v-no-post as int format ">>>9".
def {1} shared var v-tax-rate as dec format ">,>>9.99<<<".
def {1} shared var v-frt-tax-rate like v-tax-rate.
def {1} shared var v-dcr-val like oe-ordl.cost init 0.
def {1} shared var v-uom-rate as int init 0.

def {1} shared var v-s-bol  like oe-bolh.bol-no format ">>>>>>" init 0.
def {1} shared var v-e-bol  like v-s-bol init 999999.
def {1} shared var v-s-date like oe-bolh.bol-date format "99/99/9999"
                                                                     init today.
def {1} shared var v-e-date like v-s-date.
def {1} shared var v-s-cust like oe-bolh.cust-no init "".
def {1} shared var v-e-cust like v-s-cust init "zzzzzzzz".
def {1} shared var v-tried  as log.

{oe/bolcheck.i "{1}"}

def {1} shared workfile w-bolh no-undo
  field bol-no like oe-bolh.bol-no
  field ord-no like oe-bolh.ord-no
  field w-recid as recid
  field rel-no like oe-bolh.rel-no
  field b-ord-no like oe-bolh.b-ord-no
  field cust-no like oe-bolh.cust-no
  field edibol as log.

def {1} shared workfile w-nopost no-undo
  field ord-no like oe-bolh.ord-no
  field bol-date like oe-bolh.bol-date
  field bol-no like oe-bolh.bol-no
  field rel-no like oe-bolh.rel-no
  field b-ord-no like oe-bolh.b-ord-no
  field cust-no like oe-bolh.cust-no
  field po-no like oe-bolh.po-no
  field i-no like oe-boll.i-no
  field i-name like itemfg.i-name
  field reason as char format "x(30)".

