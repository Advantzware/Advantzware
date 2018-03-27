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
     {&where-phrase} AND user-print.user-id EQ USERID("nosweat") NO-ERROR.

IF NOT AVAIL user-print THEN
FIND FIRST user-print NO-LOCK {&where-phrase} AND user-print.user-id EQ "" NO-ERROR.

IF AVAIL user-print THEN DO:
  ASSIGN
   lv-frame-hdl = FRAME {&FRAME-NAME}:HANDLE
   lv-group-hdl = lv-frame-hdl:FIRST-CHILD
   lv-field-hdl = lv-group-hdl:FIRST-CHILD.

  DO WHILE VALID-HANDLE(lv-field-hdl):
    DO li = 1 TO EXTENT(user-print.field-name):
      IF TRIM(user-print.field-name[li]) NE "" AND
         lv-field-hdl:TYPE NE "BUTTON" AND 
         user-print.field-name[li] EQ lv-field-hdl:NAME {1} THEN DO:
         IF user-print.field-name[li] BEGINS "sl" THEN
            lv-field-hdl:LIST-ITEMS = user-print.field-value[li] NO-ERROR.
         ELSE lv-field-hdl:SCREEN-VALUE = user-print.field-value[li] NO-ERROR.
           LEAVE.
      END.
    END.
    lv-field-hdl = lv-field-hdl:NEXT-SIBLING.
  END.
END.
