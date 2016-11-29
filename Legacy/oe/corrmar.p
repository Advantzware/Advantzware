/*Corrugated Available Margin logic
  oe\corrmar.p */
/*NOTE:  Found no caller to this */
DEF SHARED VAR cocode AS CHAR NO-UNDO.
DEF SHARED VAR locode AS CHAR NO-UNDO.

DEFINE INPUT PARAMETER ip-est-no AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER ip-qty AS DEC NO-UNDO.
DEFINE OUTPUT PARAMETER v-margin AS DEC NO-UNDO.

DEFINE BUFFER xeb FOR eb.
DEFINE BUFFER xest FOR est.
DEFINE BUFFER xef FOR ef.
DEF VAR v-com AS DEC NO-UNDO.
def var v-gsh-qty as dec NO-UNDO.
def var v-msf as dec NO-UNDO.
DEF VAR lv-sell-by AS CHAR NO-UNDO.
def var j          as   int no-undo.
DEF VAR i AS INT NO-UNDO.
def NEW shared var qty as INT NO-UNDO .

{sys/inc/msfcalc.i}
{sys/inc/cecomm.i}

DEF VAR v-pct AS DEC NO-UNDO.

FIND FIRST ce-ctrl {sys/look/ce-ctrlW.i} NO-LOCK.

ASSIGN
   lv-sell-by = ce-ctrl.sell-by
   v-pct = ce-ctrl.prof-mrkup.

FIND FIRST xest WHERE
     xest.company EQ cocode AND
     xest.est-no EQ ip-est-no
     NO-LOCK NO-ERROR.

IF NOT AVAIL xest THEN LEAVE.

FIND FIRST xeb
      WHERE xeb.company  EQ xest.company
        AND xeb.est-no   EQ xest.est-no
        AND xeb.form-no  NE 0
      NO-LOCK NO-ERROR.
  
IF AVAIL xeb THEN DO:
   v-com = xeb.comm.

   FIND FIRST cust WHERE
        cust.company eq cocode AND
        cust.cust-no eq xeb.cust-no
        NO-LOCK NO-ERROR.

   IF AVAIL cust AND cust.markup NE 0 THEN
      v-pct = cust.markup.
END.

IF NOT cecomm-log THEN v-com = 0.

find first xef where xef.company = xest.company 
             AND xef.est-no eq xest.est-no NO-LOCK.

IF AVAIL xef THEN
   v-gsh-qty = xef.gsh-qty.

{sys/inc/roundup.i v-gsh-qty}

ASSIGN
v-msf  = if v-corr then
        round(((xef.gsh-len * xef.gsh-wid) * .007) * v-gsh-qty,0)
      else
        round(((xef.gsh-len * xef.gsh-wid) / 144) * v-gsh-qty,0)
qty = ip-qty.

IF lv-sell-by EQ "S" THEN DO:
   {cec/sqftmrkp.i "v-msf / 1000" v-pct}
END.


RUN custom/markup.p (ROWID(xeb),
                     0,
                     INPUT-OUTPUT lv-sell-by,
                     INPUT-OUTPUT v-pct).

RUN est/getsmanmtrx.p (ROWID(xest), "M",
                       INPUT-OUTPUT v-com,
                       INPUT-OUTPUT v-pct).

v-margin = v-pct.
  

