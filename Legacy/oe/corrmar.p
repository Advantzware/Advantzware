/*Corrugated Available Margin logic
  oe\corrmar.p */
/*NOTE:  Found no caller to this */
DEFINE SHARED VARIABLE cocode AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE locode AS CHARACTER NO-UNDO.

DEFINE INPUT PARAMETER ip-est-no AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ip-qty AS DECIMAL NO-UNDO.
DEFINE OUTPUT PARAMETER v-margin AS DECIMAL NO-UNDO.

DEFINE BUFFER xeb  FOR eb.
DEFINE BUFFER xest FOR est.
DEFINE BUFFER xef  FOR ef.
DEFINE            VARIABLE v-com      AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE v-gsh-qty  AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE v-msf      AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE lv-sell-by AS CHARACTER NO-UNDO.
DEFINE            VARIABLE j          AS INTEGER   NO-UNDO.
DEFINE            VARIABLE i          AS INTEGER   NO-UNDO.
DEFINE NEW SHARED VARIABLE qty        AS INTEGER   NO-UNDO .

{sys/inc/msfcalc.i}
{sys/inc/cecomm.i}

DEFINE VARIABLE v-pct AS DECIMAL NO-UNDO.

FIND FIRST ce-ctrl {sys/look/ce-ctrlW.i} NO-LOCK.

ASSIGN
    lv-sell-by = ce-ctrl.sell-by
    v-pct      = ce-ctrl.prof-mrkup.

FIND FIRST xest WHERE
    xest.company EQ cocode AND
    xest.est-no EQ ip-est-no
    NO-LOCK NO-ERROR.

IF NOT AVAILABLE xest THEN LEAVE.

FIND FIRST xeb
    WHERE xeb.company  EQ xest.company
    AND xeb.est-no   EQ xest.est-no
    AND xeb.form-no  NE 0
    NO-LOCK NO-ERROR.
  
IF AVAILABLE xeb THEN 
DO:
    v-com = xeb.comm.

    FIND FIRST cust WHERE
        cust.company EQ cocode AND
        cust.cust-no EQ xeb.cust-no
        NO-LOCK NO-ERROR.

    IF AVAILABLE cust AND cust.markup NE 0 THEN
        v-pct = cust.markup.
END.

IF NOT cecomm-log THEN v-com = 0.

FIND FIRST xef WHERE xef.company = xest.company 
    AND xef.est-no EQ xest.est-no NO-LOCK.

IF AVAILABLE xef THEN
    v-gsh-qty = xef.gsh-qty.

    {sys/inc/roundup.i v-gsh-qty}

ASSIGN
    v-msf = IF v-corr THEN
        ROUND(((xef.gsh-len * xef.gsh-wid) * .007) * v-gsh-qty,0)
      ELSE
        ROUND(((xef.gsh-len * xef.gsh-wid) / 144) * v-gsh-qty,0)
    qty   = ip-qty.

IF lv-sell-by EQ "S" THEN 
DO:
{cec/sqftmrkp.i "v-msf / 1000" v-pct}
END.


RUN custom/markup.p (ROWID(xeb),
                     0,
                     0,
                     0,
                     0,
                     INPUT-OUTPUT lv-sell-by,
                     INPUT-OUTPUT v-pct).

RUN est/getsmanmtrx.p (ROWID(xest), "M",
    INPUT-OUTPUT v-com,
    INPUT-OUTPUT v-pct).

v-margin = v-pct.
  

