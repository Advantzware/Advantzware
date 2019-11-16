
  DO WITH FRAME {&FRAME-NAME}:
    IF NOT CAN-FIND(FIRST flute WHERE flute.company EQ cocode
                                  AND flute.code    EQ {1}{2}{3})
    THEN DO:
      MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO {1}{3}.
      RETURN ERROR.
    END.
  END.
