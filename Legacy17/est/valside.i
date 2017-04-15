DO WITH FRAME {&FRAME-NAME}:
  IF fi_side-{1}:SCREEN-VALUE NE "" AND LOOKUP(fi_side-{1}:SCREEN-VALUE,"F,B") EQ 0 THEN
  DO:
     MESSAGE "Invalid Side Value.  Valid Values are F and B."
         VIEW-AS ALERT-BOX ERROR BUTTONS OK.
     APPLY "ENTRY" TO fi_side-{1} IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END.
  
END.
