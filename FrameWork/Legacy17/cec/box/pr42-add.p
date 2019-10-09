/* ------------------------------------------------ cec/box/pr4-add.p 2/94 cd */

def input parameter v-vend-list AS CHAR NO-UNDO.

def shared var cocode as cha no-undo.
def shared var locode as cha no-undo.
def var i as int no-undo.
def var j as int no-undo.
def shared var qty as INT NO-UNDO.

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.

{cec/print4.i shared shared}
{cec/print42.i shared}

def buffer xitem        for   item.
def buffer xe-item      for e-item.
def buffer xe-item-vend for e-item-vend.
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

def var v-qty as dec no-undo.
DEF VAR v-qty-tmp AS DEC NO-UNDO.
def var v-cost as dec no-undo.
def var a-cost as dec no-undo.
def var qm as dec no-undo.
def var v-bsqft as dec no-undo.
def var v-ssqft as dec no-undo.
def var v-ssqft-tmp as dec no-undo.
def var v-setup like e-item-vend.setup no-undo.
def var v-alf as char format "x(7)" no-undo.
def VAR v-vend-no like e-item-vend.vend-no NO-UNDO.
DEF VAR brd-sf-tmp AS DEC NO-UNDO.

DEF BUFFER b-ef FOR ef.

{cec/msfcalc.i}

{cec/rollfac.i}

qm = qty / 1000.

find first ce-ctrl {sys/look/ce-ctrlW.i} no-lock no-error.

for each xef where xef.company  eq xest.company
               and xef.est-no   eq xest.est-no
               and (xef.form-no eq v-form-no or (not vmclean2))
    NO-LOCK:

    v-vend-no = ENTRY(IF vmclean2 THEN 1 ELSE xef.form-no,v-vend-list).

    DO i = 1 TO 6:
       IF xef.adder[i] NE "" THEN
       DO:
          /*find first est-op WHERE
               est-op.company eq xest.company AND
               est-op.est-no  eq xest.est-no AND
               est-op.s-num   eq xef.form-no AND
               est-op.line    ge 500
               NO-LOCK NO-ERROR.*/
          
          assign
             v-ssqft-tmp = if v-corr then (xef.gsh-len * xef.gsh-wid) * .007
                           else (xef.gsh-len * xef.gsh-wid) / 144.

          /*IF AVAIL est-op THEN*/
             brd-sf-tmp = (v-ssqft-tmp * /*est-op.num-sh*/ xef.gsh-qty ) / 1000.
         
          for each b-ef where
              b-ef.company  eq xest.company AND
              b-ef.est-no   eq xest.est-no AND
              (b-ef.form-no eq v-form-no or (not vmclean2)) AND
              ROWID(b-ef) NE ROWID(xef)
              NO-LOCK:
         
              IF b-ef.adder[1] EQ xef.adder[i] OR
                 b-ef.adder[2] EQ xef.adder[i] OR
                 b-ef.adder[3] EQ xef.adder[i] OR
                 b-ef.adder[4] EQ xef.adder[i] OR
                 b-ef.adder[5] EQ xef.adder[i] OR
                 b-ef.adder[6] EQ xef.adder[i] THEN
                 DO:
                    /*find first est-op WHERE
                         est-op.company eq xest.company AND
                         est-op.est-no  eq xest.est-no AND
                         est-op.s-num   eq b-ef.form-no AND
                         est-op.line    ge 500
                         NO-LOCK NO-ERROR.*/
                   
                    v-ssqft-tmp = if v-corr then (b-ef.gsh-len * b-ef.gsh-wid) * .007
                                  else (b-ef.gsh-len * b-ef.gsh-wid) / 144.
                   
                    /*IF AVAIL est-op THEN*/
                       brd-sf-tmp = brd-sf-tmp + ((v-ssqft-tmp * /*est-op.num-sh*/ b-ef.gsh-qty ) / 1000).
                 END.
          END.
       END.
    END.

    {cec/pr4-add-com.i}

    for each blk where blk.snum = xef.form-no:
      blk.cost = blk.cost + (a-cost * blk.pct).
    end.

    v-alf = if v-setup eq 0 then ""
            else fill(" ",7 - length(trim(string(v-setup,">>>9.99")))) +
                 trim(string(v-setup,">>>9.99")).

    put string(xef.form-no,"99") + "-0" format "x(4)"
        space(1)
        xitem.i-name
        "$" + string(v-cost,">>>9.99")  to 50 space(1) b-uom
        v-alf format "x(7)" to 63
        a-cost / qm / v-sqft-fac format ">>>9.99" to 71
        a-cost format ">>>>>9.99" to 80 skip.
  end.
end.

/* end ---------------------------------- copr. 1994  advanced software, inc. */
