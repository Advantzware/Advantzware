&Scoped-Define ENABLED-OBJECTS RECT-7 Btn_bol btn_update_rel ~
btn_pick-ticket Btn_Close 
DEFINE BUTTON btn_pick-ticket 
     LABEL "Print Pick Ticket" 
     SIZE 33 BY 2
     FONT 6.

     SIZE 35 BY 8.81.
     btn_pick-ticket AT ROW 5.52 COL 2
     Btn_Close AT ROW 7.52 COL 2
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_pick-ticket
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_pick-ticket s-object
ON CHOOSE OF btn_pick-ticket IN FRAME F-Main /* Print Pick Ticket */
DO:
   IF CAN-FIND(FIRST asi._file WHERE
      asi._file._File-Name = "ssrelbol") THEN
   DO:
       RUN oerep/r-relprt.w.
      
   END.
