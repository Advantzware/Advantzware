/* check-release.i */

DEFINE VARIABLE lv-msg AS CHARACTER NO-UNDO.
DEF VAR lHoldOK AS LOGICAL NO-UNDO.

FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ xoe-ord.company NO-LOCK NO-ERROR.
IF AVAIL oe-ctrl THEN
    lHoldOK = oe-ctrl.p-pick.

IF lv-msg EQ '' AND xoe-ord.stat eq 'H' AND NOT lHoldOK THEN
lv-msg = 'orders on Hold'.

IF lv-msg EQ '' AND xoe-ord.priceHold AND NOT lHoldOK THEN
lv-msg = 'orders on Price Hold'.

IF lv-msg EQ '' AND xoe-ord.stat EQ 'W' THEN
lv-msg = 'unapproved web orders'.

IF lv-msg EQ '' AND NOT xoe-ord.opened THEN
lv-msg = 'closed orders'.

IF lv-msg EQ '' AND TRIM(oe-ordl.job-no) NE ''
                AND CAN-FIND(FIRST job
                             WHERE job.company EQ oe-ordl.company
                               AND job.job-no  EQ oe-ordl.job-no
                               AND job.job-no2 EQ oe-ordl.job-no2
                               AND job.stat    EQ 'H') THEN
lv-msg = 'jobs on hold'.

&IF '{&programVersion}' EQ '' &THEN
IF lv-msg NE '' THEN DO:
  MESSAGE 'Can~'t release items for ' +
          TRIM(lv-msg) + '...' VIEW-AS ALERT-BOX ERROR.
  RETURN ERROR.
END.
&ENDIF
