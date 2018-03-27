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

DEF TEMP-TABLE w-probe NO-UNDO
    FIELD w-rowid AS ROWID.

{cec/print4.i shared shared}
{cec/print42.i shared}

def var v as INT NO-UNDO.

def var v-part-no like xeb.part-no NO-UNDO.
def var v-part-d1 like xeb.part-dscr1 NO-UNDO.
def var v-part-d2 like xeb.part-dscr2 NO-UNDO.
def var v-module as char format "x(60)" NO-UNDO.
DEF VAR v-col-tot AS DEC EXTENT 5 NO-UNDO.
DEF VAR cerunc-dec AS DEC NO-UNDO.

DEF VAR v-start LIKE probe.line NO-UNDO.
DEF VAR rowi AS ROWID EXTENT 100 NO-UNDO.
DEF VAR vhld-last AS INT NO-UNDO.
DEF VAR v-count AS INT NO-UNDO.
DEF VAR v-total-list AS CHAR NO-UNDO.
DEF VAR v-total-index AS INT NO-UNDO.
DEF VAR v-non-total-index AS INT NO-UNDO. 

v-total-list = "Commission on GM,Commission on SP,    Commission %,"
             + "FULL COST,FULL COST PER ROLL,Margin on Fact Cost,"
             + "    Fact Margin %,Net Margin,    Net Margin %,"
             + "Available Margin,    Available Margin %,Matching Markup,    Matching Markup %,"
             + "SELLING PRICE,SELLING PRICE PER ROLL,OVERALL".

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

  ASSIGN
     v-module = str-tit
     cerunc-dec = sys-ctrl.dec-fld.

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
      AND b-probe.LINE       GE probe.LINE 
    NO-LOCK
    BY b-probe.est-qty
    BY b-probe.freight
    BY b-probe.rec_key:

  IF (cerunc-dec EQ 0 AND v GE 5) OR
     (cerunc-dec EQ 1 AND v GE 4) THEN
    IF CAN-FIND(FIRST w-probe WHERE w-rowid EQ ROWID(probe)) THEN LEAVE.
    ELSE DO:
      FOR EACH w-probe:
        DELETE w-probe.
      END.
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

  ASSIGN
   v = v + 1
   qtty[v] = b-probe.est-qty
   rels[v] = b-probe.freight
   rowi[v] = ROWID(b-probe)
   v-total-index = 100000
   v-non-total-index = 0.

  FOR EACH est-summ
      WHERE est-summ.company EQ b-probe.company
        AND est-summ.est-no  EQ b-probe.est-no
        AND est-summ.e-num   EQ b-probe.line
      USE-INDEX est-qty NO-LOCK:

    v-form-no = INT(SUBSTR(est-summ.summ-tot,21,10)).

    /*have total fields appear at end of estimate output 07310906*/
    IF INDEX(v-total-list,SUBSTR(est-summ.summ-tot,31)) GT 0 THEN
    DO:
       {cec/pr4-mcln.i SUBSTR(est-summ.summ-tot,31) v est-summ.per-m v-total-index}
       v-total-index = v-total-index + 1.
    END.
    ELSE
    DO:
       {cec/pr4-mcln.i SUBSTR(est-summ.summ-tot,31) v est-summ.per-m v-non-total-index}
        v-non-total-index = v-non-total-index + 1.
    END.

    mclean.rec-type = SUBSTR(est-summ.summ-tot,01,20).
  END.

  IF (cerunc-dec EQ 0 AND v GE 5) OR
     (cerunc-dec EQ 1 AND v GE 4) THEN LEAVE.
END.

assign
 vmcl = 1.
 vhld = vmcl.

IF LOOKUP(cerunc,"Protagon,CERunC 3") EQ 0 /*cerunc NE "Protagon"*/ THEN
   PUT "<P9>" "Individual Part #" FORMAT "x(30)".
ELSE
   PUT "<P9>" "Individual Part # Description                     Qty/Set".

IF cerunc-dec EQ 0 THEN
   vhld-last = 4.
ELSE
   vhld-last = 3.

do v = vmcl to vhld + vhld-last:
  if qtty[v] ne 0 then
  DO:
     IF cerunc-dec EQ 0 THEN
        put qtty[v] format ">>,>>>,>>9".
     ELSE
        put qtty[v] format ">,>>>,>>>,>>9".
  END.
end.

IF LOOKUP(cerunc,"Protagon,CERunC 3") EQ 0 /*cerunc NE "Protagon"*/ THEN
   PUT SKIP SPACE(30).
ELSE
   PUT SKIP SPACE(57).

do v = vmcl to vhld + vhld-last:
  if qtty[v] ne 0 then
  DO:
     IF cerunc-dec EQ 0 THEN
        put rels[v] format ">>,>>>,>>9".
     ELSE
        put rels[v] format ">,>>>,>>>,>>9".
  END.
end.

IF LOOKUP(cerunc,"Protagon,CERunC 3") EQ 0 /*cerunc NE "Protagon"*/ THEN
   put SKIP fill("-",30) format "x(30)".
ELSE
   put SKIP fill("-",57) format "x(57)".

do v = vmcl to vhld + vhld-last:
  if qtty[v] ne 0 then
  DO:
     IF cerunc-dec EQ 0 THEN
        put unformatted fill("-",10) format "x(10)".
     ELSE
        put unformatted fill("-",13) format "x(13)".
  END.
end.

put skip.
IF LOOKUP(cerunc,"Protagon,CERunC 3") EQ 0 /*cerunc NE "Protagon"*/ THEN
DO:

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
    
        do v = vmcl to vhld + vhld-last:
          if mclean.cost[v] ne 0 then leave.
          if v ge vhld + vhld-last then next mclean-loop.
        end.
    
        IF LOOKUP(cerunc,"Protagon,CERunC 3") EQ 0 THEN
           put eb.part-no format "x(30)".
        ELSE
           PUT eb.part-no format "x(18)"
               eb.part-dscr1 FORMAT "x(30)"
               eb.yld-qty FORMAT "->>>>>>>9".
    
        do v = vmcl to vhld + vhld-last:
           if mclean.cost[v] ne 0 then
           DO:
              IF LOOKUP(cerunc,"Protagon,CERunC 3") EQ 0 THEN
                IF cerunc-dec EQ 0 THEN
                    put mclean.cost[v].
                ELSE
                    put mclean.cost[v] FORMAT "->,>>>,>>>.99".
              ELSE /*Protagon wants values in each not M per 05171201*/
                  IF cerunc-dec EQ 0 THEN
                    put mclean.cost[v] / 1000.
                ELSE
                    put mclean.cost[v] / 1000 FORMAT "->,>>>,>>>.99".
           END.
        end.
        IF LOOKUP(cerunc,"Protagon,CERunC 3") NE 0 /*cerunc = "Protagon"*/ THEN /*05171201*/
            PUT SKIP eb.part-dscr2 AT 19 FORMAT "X(30)".
        put skip.
      end.
    end.
END.
ELSE DO: /* Protagon */
    v-col-tot = 0.
    for each ef
        where ef.company eq xest.company
          and ef.est-no  eq xest.est-no
        no-lock,
      EACH eb
          where eb.company eq ef.company
            and eb.est-no  eq ef.est-no
            and eb.form-no eq ef.form-no
          NO-LOCK
        break by ef.form-no
              BY eb.blank-no:


            PUT eb.part-no format "x(18)"
                eb.part-dscr1 FORMAT "x(30)"
                eb.yld-qty FORMAT "->>>>>>>9".
            v-count = 0. 
            do v = vmcl to vhld + vhld-last:
              if qtty[v] ne 0 then
              DO:
                 v-count = v-count + 1.
                 FIND probe WHERE rowid(probe) EQ rowi[v] NO-LOCK NO-ERROR.
                 FIND FIRST probeit WHERE probeit.company EQ eb.company 
                              AND probeit.est-no  EQ eb.est-no
                              AND probeit.part-no EQ eb.part-no 
                              AND probeit.LINE EQ /* v */ probe.LINE
                            NO-LOCK NO-ERROR.
                 
                 /* WFK - 09251201 - should match by line, but just in case... */
                 IF NOT AVAIL probeit THEN
                   FIND FIRST probeit WHERE probeit.company EQ eb.company 
                                AND probeit.est-no  EQ eb.est-no
                                AND probeit.part-no EQ eb.part-no 
                                AND (abs(probeit.yld-qty) EQ ABS(eb.yld-qty * qtty[v]) OR
                                     abs(probeit.yld-qty * eb.yld-qty) EQ ABS(qtty[v]))
                              NO-LOCK NO-ERROR.

              IF cerunc-dec EQ 0 THEN do:   
                 IF AVAIL probeit THEN DO:
                   IF eb.yld-qty GE 0 THEN DO:
                     PUT probeit.sell-price * eb.yld-qty / 1000 FORMAT "->>>>>9.99".
                     v-col-tot[v-count] = v-col-tot[v-count] + ROUND(probeit.sell-price * eb.yld-qty / 1000, 2).
                   END.

                   ELSE DO:
                     PUT probeit.sell-price / ABS(eb.yld-qty) / 1000 FORMAT "->>>>>9.99".
                     v-col-tot[v-count] = v-col-tot[v-count] + ROUND(probeit.sell-price / ABS(eb.yld-qty) / 1000, 2).
                   END.
                 END.
              END.
               ELSE do:
                 IF AVAIL probeit THEN DO:
                   IF eb.yld-qty GE 0 THEN DO:
                     PUT probeit.sell-price * eb.yld-qty / 1000 FORMAT "->,>>>,>>9.99".
                     v-col-tot[v-count] = v-col-tot[v-count] + ROUND(probeit.sell-price * eb.yld-qty / 1000, 2).
                   END.

                   ELSE DO:
                     PUT probeit.sell-price / ABS(eb.yld-qty) / 1000 FORMAT "->,>>>,>>9.99".
                     v-col-tot[v-count] = v-col-tot[v-count] + ROUND(probeit.sell-price / ABS(eb.yld-qty) / 1000, 2).
                   END.
                 END.
               END.


              END.
            end.
            PUT SKIP.

    END.
END.
IF LOOKUP(cerunc,"Protagon,CERunC 3") EQ 0 THEN
DO:
   IF cerunc-dec EQ 0 THEN
      put SKIP fill("",31) format "x(31)".
   ELSE
      put skip fill("",34) format "x(34)".
END.
ELSE
DO:
   IF cerunc-dec EQ 0 THEN
      put SKIP fill("",58) format "x(58)".
   ELSE
      put skip fill("",61) format "x(61)".
END.

do v = vmcl to vhld + vhld-last:
  if qtty[v] ne 0 then
  DO:
     IF cerunc-dec EQ 0 THEN
        put unformatted fill("-",10) format "x(10)".
     ELSE
        put unformatted fill("-",12) format "x(12)".
  END.
end.

IF LOOKUP(cerunc,"Protagon,CERunC 3") EQ 0 THEN
   put SKIP "Set Total Price" format "x(30)".
ELSE
   put SKIP "Set Total Price" format "x(57)".
v-count = 0.
do v = vmcl to vhld + vhld-last:
  find probe where rowid(probe) eq rowi[v] no-lock no-error.
  if not avail probe then leave.
  v-count = v-count + 1.
  IF cerunc NE "Protagon" THEN
    IF cerunc-dec EQ 0 THEN
        put probe.sell-price format ">>>,>>9.99".
    ELSE
        put probe.sell-price format "->,>>>,>>9.99".
  ELSE /*Protagon wants values in each not M per 05171201*/
      
    IF cerunc-dec EQ 0 THEN
        PUT v-col-tot[v] /* probe.sell-price  / 1000 */ format ">>>,>>9.99".
    ELSE
        put v-col-tot[v] /*probe.sell-price / 1000 */ format "->,>>>,>>9.99".
end.

put skip.

FOR EACH mclean:
  DELETE mclean.
END

/* end ---------------------------------- copr. 1996  advanced software, inc. */
