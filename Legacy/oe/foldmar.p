/*Folding Logic
  oe\foldmar.p*/
/*NOTE:  Found no caller to this */

DEFINE INPUT PARAMETER ip-est-no AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER ip-qty AS DEC NO-UNDO.
DEFINE OUTPUT PARAMETER v-margin AS DEC NO-UNDO.


DEF SHARED VAR cocode AS CHAR NO-UNDO.
DEF SHARED VAR locode AS CHAR NO-UNDO.

DO TRANSACTION:
   {sys/inc/ceprice.i}
   {sys/inc/cecomm.i}
   {sys/inc/cerun.i F}
END.

DEF VAR v-pct AS DEC EXTENT 3 NO-UNDO.

DEF BUFFER xest FOR est.
DEF BUFFER xef  FOR ef.
DEF BUFFER xeb  FOR eb.

DEF SHARED VAR qty AS INT NO-UNDO.
DEF VAR lv-sell-by AS CHAR NO-UNDO.
DEF VAR v-com LIKE eb.comm INIT 0 NO-UNDO.

qty = ip-qty.

FIND FIRST xest WHERE
     xest.company EQ cocode AND
     xest.est-no EQ ip-est-no
     NO-LOCK NO-ERROR.

IF NOT AVAIL xest THEN LEAVE.

find first xef where
     xef.company = xest.company AND
     xef.est-no eq xest.est-no
     NO-LOCK NO-ERROR.

IF avail xef THEN
   find first xeb where
        xeb.company = xest.company AND
        xeb.est-no eq xest.est-no AND
        xeb.form-no = xef.form-no
        NO-LOCK NO-ERROR.

IF NOT AVAIL xeb THEN LEAVE.

IF cecomm-log THEN
   v-com = xeb.comm.

FIND FIRST ce-ctrl NO-LOCK
    WHERE ce-ctrl.company EQ xeb.company
      AND ce-ctrl.loc     EQ xeb.loc
    NO-ERROR.

IF NOT AVAIL ce-ctrl THEN
   FIND FIRST ce-ctrl NO-LOCK
       WHERE ce-ctrl.company    EQ cocode
         AND ce-ctrl.prof-mrkup NE 0
    NO-ERROR.

IF AVAIL ce-ctrl THEN
   ASSIGN
      v-pct[1] = ce-ctrl.prof-mrkup
      lv-sell-by = ce-ctrl.sell-by.

FIND FIRST cust NO-LOCK
    WHERE cust.company EQ xeb.company
      AND cust.cust-no EQ xeb.cust-no
    NO-ERROR.

IF AVAIL cust AND cust.markup NE 0 THEN
   v-pct[2] = cust.markup.

IF AVAIL xeb THEN
   RUN custom/markup.p (ROWID(xeb),
                        0,
                        INPUT-OUTPUT lv-sell-by,
                        INPUT-OUTPUT v-pct[3]).

v-pct[1] = v-pct[1] + v-markup.

IF v-pct[3] NE 0 THEN
   v-pct[1] = v-pct[3].

/*assuming cerunf is Fibre*/

/*IF cerunf EQ "Dee" THEN
   v-pct[1] = v-pct[1] + (ctrl[1] * 100) + ctrl2[18] + v-pct[2].
ELSE DO:*/
   IF v-pct[3] EQ 0 AND v-pct[2] NE 0 THEN
      v-pct[1] = v-pct[2] + v-markup.
   /*v-pct[2] = 0.*/
/*END.*/

RUN est/getsmanmtrx.p (ROWID(xest), "M",
                       INPUT-OUTPUT v-com,
                       INPUT-OUTPUT v-pct[1]).

v-margin = v-pct[1].
