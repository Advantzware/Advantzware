/*custom/usrprint.i */

DEF VAR lv-group-hdl AS HANDLE NO-UNDO.
DEF VAR lv-field-hdl AS HANDLE NO-UNDO.
DEF VAR lv-frame-hdl AS HANDLE NO-UNDO.
DEF VAR li AS INT NO-UNDO.
/*custom/usrprtb.i  Input param assignment for Batch */

&SCOPED-DEFINE where-phrase                   ~
    WHERE user-print.company    EQ cocode     ~
      AND user-print.program-id EQ v-prgmname ~
      AND user-print.batch      <> ""


FIND FIRST user-print
    {&where-phrase}
      AND user-print.user-id EQ "Batch"
    NO-LOCK NO-ERROR.

IF NOT AVAIL user-print THEN
FIND FIRST user-print 
    {&where-phrase}
      AND user-print.user-id EQ ""
    NO-LOCK NO-ERROR.

v-run-param = "".
IF AVAIL user-print THEN DO:
  /*ASSIGN
   lv-frame-hdl = FRAME {&frame-name}:HANDLE
   lv-group-hdl = lv-frame-hdl:FIRST-CHILD
   lv-field-hdl = lv-group-hdl:FIRST-CHILD.
  
  DO WHILE VALID-HANDLE(lv-field-hdl):
    DO li = 1 TO EXTENT(user-print.field-name):
      IF TRIM(user-print.field-name[li]) NE ""          AND 
         user-print.field-name[li] EQ lv-field-hdl:NAME THEN DO:
        lv-field-hdl:SCREEN-VALUE = user-print.field-value[li] NO-ERROR.
        LEAVE.
      END.
    END.

    lv-field-hdl = lv-field-hdl:NEXT-SIBLING.
  END.
  */
  v-run-param = v-run-param + user-print.field-value[li] + ",".
END.
     /* remove "," in the last */
IF SUBSTRING(v-run-param,LENGTH(v-run-param),1) = "," THEN
    v-run-param = SUBSTRING(v-run-param,1,LENGTH(v-run-param) - 1).

