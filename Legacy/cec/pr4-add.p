/* ---------------------------------------------------- cec/pr4-add.p 2/94 cd */

def input parameter v-vend-list AS CHAR NO-UNDO.

def shared var cocode as cha no-undo.
def shared var locode as cha no-undo.
def var i as int no-undo.
def var j as int no-undo.

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.

{cec/print4.i shared shared}

def buffer xitem        for   item.
def buffer xe-item      for e-item.
def buffer xe-item-vend for e-item-vend.
def shared var qty as INT NO-UNDO.
def var v-qty as DEC  NO-UNDO.
def var v-cost as dec NO-UNDO.
def var a-cost as dec NO-UNDO.
def var qm as dec NO-UNDO.

def var v-bsqft as dec NO-UNDO.
def var v-ssqft as dec NO-UNDO.
def var v-setup like e-item-vend.setup NO-UNDO.
def var v-alf as char format "x(7)" NO-UNDO.
def VAR v-vend-no like e-item-vend.vend-no NO-UNDO.

DEF BUFFER b-cost FOR reftable.
DEF BUFFER b-qty FOR reftable.
DEF BUFFER b-setup FOR reftable.

DEF TEMP-TABLE tt-eiv NO-UNDO
    FIELD run-qty AS DECIMAL DECIMALS 3 EXTENT 20
    FIELD run-cost AS DECIMAL DECIMALS 4 EXTENT 20
    FIELD setups AS DECIMAL DECIMALS 2 EXTENT 20.

DEF TEMP-TABLE tt-ei NO-UNDO
    FIELD run-qty AS DECIMAL DECIMALS 3 EXTENT 20
    FIELD run-cost AS DECIMAL DECIMALS 4 EXTENT 20.

{cec/msfcalc.i}

{cec/rollfac.i}

qm = qty / 1000.

find first ce-ctrl {sys/look/ce-ctrlW.i} no-lock no-error.

v-vend-no = ENTRY(1,v-vend-list).

{cec/pr4-add.i}

  v-alf = if v-setup eq 0 then ""
          else fill(" ",7 - length(trim(string(v-setup,">>>9.99")))) +
               trim(string(v-setup,">>>9.99")).

  put xitem.i-name
      "$" + string(v-cost,">>>>9.99") format "x(9)" to 48 space(1) b-uom
      v-alf format "x(7)" to 59
      a-cost / qm / v-sqft-fac format ">>>>9.99" to 68
      a-cost to 80 format ">>>>,>>9.99" skip.
    
end.


/* end ---------------------------------- copr. 1994  advanced software, inc. */
