/* custom/secpanel.i */

  DO WITH FRAME {&FRAME-NAME}:
    IF NOT v-can-create THEN
      ASSIGN
       btn-add:SENSITIVE  = NO
       btn-copy:SENSITIVE = NO.

    IF NOT v-can-update THEN btn-save:SENSITIVE = NO.
    IF NOT v-can-delete THEN btn-delete:SENSITIVE = NO.

    IF v-can-create                 AND
       Btn-Save:LABEL EQ "&Save"    AND
       panel-state NE "disable-all" AND
       panel-state NE "add-only"    THEN btn-save:SENSITIVE = yes.

    IF NOT v-can-run THEN DISABLE ALL.
  END.
