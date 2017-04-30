/* d-cadcamrun.i */

DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE cellColumn AS WIDGET-HANDLE NO-UNDO.

IF cadcam-chr EQ 'ARTIOS' AND cadcam-log THEN
DO:
  cadcamValue = 'cad-no,?,die-no,?,t-len,?,t-wid,?,t-sqin,?,lin-in,?,'.
  DO i = 1 TO {&BROWSE-NAME}:NUM-COLUMNS IN FRAME {&FRAME-NAME}:
    cellColumn = {&BROWSE-NAME}:GET-BROWSE-COLUMN[i].
    cadcamValue = cadcamValue + cellColumn:NAME + ',?,'.
  END.
  RUN est/d-cadcam.w (gcompany,INPUT-OUTPUT cadcamValue).
  IF cadcamValue EQ '' THEN RETURN NO-APPLY.
END.
