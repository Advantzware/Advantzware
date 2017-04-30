
  DEF VAR ll-valid AS LOG INIT NO NO-UNDO.
  DEF VAR li-cnt AS INT NO-UNDO.

    
  DO WITH FRAME {&FRAME-NAME}:
    FOR EACH stack-flute
        WHERE stack-flute.company EQ cocode
          AND stack-flute.loc     EQ locode
          AND stack-flute.code    EQ {1}{3}{4}
        NO-LOCK:

      li-cnt = 1.                            
      DO WHILE (NOT ll-valid) AND li-cnt LE 16:  
        ll-valid = stack-flute.row-value[li-cnt] EQ {2}{3}{4}.
        li-cnt = li-cnt + 1.
      END.

      IF ll-valid THEN LEAVE.
    END.

    IF NOT ll-valid THEN DO:
      MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO {2}{4}.
      RETURN ERROR.
    END.
  END.
