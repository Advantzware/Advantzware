/* validDim.i - used in procedure valid-dimensions in
                viewers/item.w & cec/v-item.w */

  DEFINE VARIABLE rollWidth AS DECIMAL NO-UNDO.
  DEFINE VARIABLE sheetWidth AS DECIMAL NO-UNDO.
  DEFINE VARIABLE sheetLength AS DECIMAL NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    IF item.i-code:SCREEN-VALUE = 'R' AND
       CAN-DO('A,B,P',fi_mat-type:SCREEN-VALUE) THEN DO:
      ASSIGN
        rollWidth = DECIMAL(item.r-wid:SCREEN-VALUE)
        sheetWidth = DECIMAL(item.s-wid:SCREEN-VALUE)
        sheetLength = DECIMAL(item.s-len:SCREEN-VALUE).
      IF rollWidth EQ 0 AND sheetWidth EQ 0 THEN DO:
        MESSAGE 'Both Roll Width and Sheet Width cannot be Zero, Please Re-enter...' VIEW-AS ALERT-BOX ERROR.
        APPLY 'ENTRY':U TO item.s-wid.
        RETURN ERROR.
      END.
      ELSE
      IF rollWidth NE 0 AND sheetWidth NE 0 THEN DO:
        MESSAGE 'Both Roll Width and Sheet Width cannot be Greater Than Zero, Please Re-enter...' VIEW-AS ALERT-BOX ERROR.
        APPLY 'ENTRY':U TO item.s-wid.
        RETURN ERROR.
      END.
      ELSE
      IF sheetWidth NE 0 AND sheetLength EQ 0 THEN DO:
        MESSAGE 'Sheet Length cannot be Zero, Please Re-enter...' VIEW-AS ALERT-BOX ERROR.
        APPLY 'ENTRY':U TO item.s-len.
        RETURN ERROR.
      END.
    END. /* if i-code */
  END. /* do with */
