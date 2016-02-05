/* ------------------------------------------------- cec/pr4-mcl2.p 11/96 JLF */
/* probe    print totals for McLean Set                                       */
/* -------------------------------------------------------------------------- */

def input param v-recid as recid.

{sys/inc/var.i shared}
{sys/form/s-top.f}
{sys/form/r-top.i}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.

DEF BUFFER b-probe FOR probe.

DEF TEMP-TABLE w-probe NO-UNDO FIELD w-rowid AS ROWID.

{ce/print4.i shared shared}
{ce/print42.i shared}

def var v as int.

def var v-part-no like xeb.part-no.
def var v-part-d1 like xeb.part-dscr1.
def var v-part-d2 like xeb.part-dscr2.
def var v-module as char format "x(60)" NO-UNDO.

DEF VAR v-start LIKE probe.line NO-UNDO.
DEF VAR rowi AS ROWID EXTENT 100 NO-UNDO.

form day_str v-module tim_str to 79  skip(1)
     with frame hdr page-top STREAM-IO width 80 no-labels no-box.

form "Estimate: #" xest.est-no FORMAT "x(8)"
     "Set Identity:" to 49
     v-part-no
     skip
     "Desc:"         to 49
     v-part-d1
     skip
     "Customer:"
     kli.cust-add[1]
     v-part-d2       at 51
     skip(1)
     "SalesRep:"
     kli.sman kli.sname
     skip(3)

    with no-labels no-box down STREAM-IO width 80 frame kli.

find first xeb
    where xeb.company  eq xest.company
      and xeb.est-no   eq xest.est-no
      and xeb.form-no  eq 0
    no-lock.
assign
 v-part-no = xeb.part-no
 v-part-d1 = xeb.part-dscr1
 v-part-d2 = xeb.part-dscr2.

DO TRANSACTION:
  {sys/inc/cerun.i C}

  v-module = str-tit.
  if sys-ctrl.char-fld eq "Brick" then
    assign
     v-module = v-module + " - ISO# CS-03-1-F"
     {sys/inc/ctrtext.i "v-module" 60}.
END.

display day_str v-module tim_str with frame hdr.

find first xeb
    where xeb.company  eq xest.company
      and xeb.est-no   eq xest.est-no
      and xeb.form-no  ne 0
    no-lock.

find first sman
    where sman.company eq cocode
      and sman.sman eq xeb.sman
    no-lock no-error.
find first cust
    where cust.company eq cocode
      and cust.cust-no eq xeb.cust-no
    no-lock no-error.
         
display trim(xest.est-no)          @ xest.est-no
        v-part-no
        v-part-d1
        v-part-d2
        cust.name                  @ kli.cust-add[1]
          "TEMP" when not avail cust or xeb.cust-no eq "TEMP"
                                   @ kli.cust-add[1]
        xeb.sman                   @ kli.sman
        sman.sname when avail sman @ kli.sname
        
    with frame kli.
        

find probe where recid(probe) eq v-recid no-lock no-error.

v = 0.

FOR EACH b-probe
    WHERE b-probe.company    EQ probe.company
      AND b-probe.est-no     EQ probe.est-no
      AND b-probe.probe-date EQ probe.probe-date
    NO-LOCK
    BY b-probe.est-qty
    BY b-probe.freight
    BY b-probe.rec_key:

  IF v GE 5 THEN
    IF CAN-FIND(FIRST w-probe WHERE w-rowid EQ ROWID(probe)) THEN LEAVE.
    ELSE DO:
      EMPTY TEMP-TABLE w-probe.
      v = 0.
    END.

  CREATE w-probe.
  ASSIGN
   w-rowid = ROWID(b-probe)
   v       = v + 1.
END.

ASSIGN
 v    = 0
 qtty = 0.

FOR EACH w-probe,
    FIRST b-probe WHERE ROWID(b-probe) EQ w-rowid NO-LOCK
    BY b-probe.est-qty
    BY b-probe.freight
    BY b-probe.rec_key:

  v = v + 1.


  ASSIGN
   qtty[v] = b-probe.est-qty
   rels[v] = b-probe.freight
   rowi[v] = ROWID(b-probe).

  FOR EACH est-summ
      WHERE est-summ.company EQ b-probe.company
        AND est-summ.est-no  EQ b-probe.est-no
        AND est-summ.e-num   EQ b-probe.line
      USE-INDEX est-qty NO-LOCK:

    v-form-no = INT(SUBSTR(est-summ.summ-tot,21,10)).

    {ce/pr4-mcln.i SUBSTR(est-summ.summ-tot,31) v est-summ.per-m}
    mclean.rec-type = SUBSTR(est-summ.summ-tot,01,20).
  END.

  IF v GE 5 THEN LEAVE.
END.

assign
 vmcl = 1
 vhld = vmcl.

put "Individual Part #" format "x(30)".

do v = vmcl to vhld + 4:
  if qtty[v] ne 0 then put qtty[v] format ">>,>>>,>>9".
end.

put skip
    space(30).

do v = vmcl to vhld + 4:
  if qtty[v] ne 0 then put rels[v] format ">>,>>>,>>9".
end.

put skip
    fill("-",30) format "x(30)".

do v = vmcl to vhld + 4:
  if qtty[v] ne 0 then put unformatted fill("-",10) format "x(10)".
end.

put skip.

for each ef
    where ef.company eq xest.company
      and ef.est-no  eq xest.est-no
    no-lock
    break by ef.form-no:

  find first eb
      where eb.company eq ef.company
        and eb.est-no  eq ef.est-no
        and eb.form-no eq ef.form-no
      no-lock.

  mclean-loop:
  for each mclean
      where mclean.rec-type eq ""
        and mclean.descr    eq "SELLING PRICE"
        and mclean.form-no  eq ef.form-no.

    do v = vmcl to vhld + 4:
      if mclean.cost[v] ne 0 then leave.
      if v ge vhld + 4 then next mclean-loop.
    end.

    put eb.part-no format "x(30)".

    do v = vmcl to vhld + 4:
      if mclean.cost[v] ne 0 then put mclean.cost[v].
    end.

    put skip.
  end.
end.

put skip(1)
    "Set Match Charge" format "x(30)".

do v = vmcl to vhld + 4:
  find probe where rowid(probe) eq rowi[v] no-lock no-error.
  if not avail probe then leave.
  put probe.sell-price * (probe.set-chg / 100) format ">>>,>>9.99".
end.

put skip
    "Set Match Charge %" format "x(30)".

do v = vmcl to vhld + 4:
  find probe where rowid(probe) eq rowi[v] no-lock no-error.
  if not avail probe then leave.
  put probe.set-chg format ">>>,>>9.99".
end.

put skip
    fill("",30) format "x(30)".

do v = vmcl to vhld + 4:
  if qtty[v] ne 0 then put unformatted fill("-",10) format "x(10)".
end.

put skip
    "Set Total Price" format "x(30)".

do v = vmcl to vhld + 4:
  find probe where rowid(probe) eq rowi[v] no-lock no-error.
  if not avail probe then leave.
  put probe.sell-price format ">>>,>>9.99".
end.

put skip.

FOR EACH mclean:
  DELETE mclean.
END

/* end ---------------------------------- copr. 1996  advanced software, inc. */
