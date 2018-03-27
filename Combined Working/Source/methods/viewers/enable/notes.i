/* notes.i */

&IF '{&spec-note}' NE '' &THEN do:
   IF AVAIL notes THEN DO:
      if notes.note_type = "G" then  ENABLE notes.note_group WITH FRAME {&FRAME-NAME}.
      else if notes.note_type = "D" then ENABLE notes.note_code WITH FRAME {&FRAME-NAME}.
   END.
 end.  
&ENDIF
