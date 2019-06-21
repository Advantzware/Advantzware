/*custom/usrprint.i */

DEF VAR lv-group-hdl AS HANDLE NO-UNDO.
DEF VAR lv-field-hdl AS HANDLE NO-UNDO.
DEF VAR lv-frame-hdl AS HANDLE NO-UNDO.
DEF VAR li AS INT NO-UNDO.

&SCOPED-DEFINE where-phrase                   ~
    WHERE user-print.company    EQ cocode     ~
      AND user-print.program-id EQ v-prgmname ~
      AND user-print.batch      EQ ""

RELEASE user-print.

IF g_batch THEN
FIND FIRST user-print NO-LOCK WHERE ROWID(user-print) EQ g_batch-rowid NO-ERROR.

ELSE
FIND FIRST user-print NO-LOCK
    WHERE user-print.user-id EQ USERID("nosweat") NO-ERROR.

IF NOT AVAIL user-print THEN
FIND FIRST user-print NO-LOCK WHERE user-print.user-id EQ "" NO-ERROR.

IF AVAIL user-print THEN DO:
  ASSIGN
  /* lv-frame-hdl = FRAME {&FRAME-NAME}:HANDLE*/
   lv-group-hdl = lv-frame-hdl
   lv-field-hdl = lv-group-hdl.

  DO WHILE lv-field-hdl:
    DO li = 1 TO EXTENT(user-print.field-name):
      IF TRIM(user-print.field-name[li]) NE "" AND 
         user-print.field-name[li] EQ lv-field-hdl THEN DO:

        lv-field-hdl = user-print.field-value[li] NO-ERROR.
        
      END.
    END.
    lv-field-hdl = lv-field-hdl.
  END.
END.
