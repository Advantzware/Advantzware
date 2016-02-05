DEFINE VARIABLE wh AS WIDGET-HANDLE.
DEFINE BUTTON b_create LABEL "Create Button".
DEFINE BUTTON b_del    LABEL "Delete Buttons".
DEFINE BUTTON b_quit LABEL "Quit"
      TRIGGERS:    
           ON CHOOSE    DO:
          IF VALID-HANDLE(wh) THEN        DELETE WIDGET-POOL "new-buttons".  
              QUIT.
           END.
      END.
                 
 DEFINE FRAME butt-frame  b_create b_del b_quit  WITH ROW SCREEN-LINES - 2.
 DEFINE FRAME fnew-buttons  WITH SIZE 76 BY 11 CENTERED ROW 2
                TITLE "New Buttons".
               
ON CHOOSE OF b_create IN FRAME butt-frameDO:
    STATUS INPUT "Press RETURN to select a new button".
        IF wh = ? OR NOT VALID-HANDLE(wh) THEN
                         CREATE WIDGET-POOL "new-buttons" PERSISTENT.

        CREATE BUTTON wh IN WIDGET-POOL "new-buttons" 
        ASSIGN FRAME = FRAME fnew-buttons:HANDLE    
                           ROW = RANDOM(2, 9)      
                         COLUMN = RANDOM(2, 58)    
                           LABEL = "BUTTON " + STRING(etime)  
                             SENSITIVE = TRUE    
                           VISIBLE = TRUE     
        TRIGGERS:      
                           ON CHOOSE PERSISTENT RUN dispmsg.      
        END.
END.

ON CHOOSE OF b_del IN FRAME butt-frameDO:  IF VALID-HANDLE(wh) THEN    DELETE WIDGET-POOL "new-buttons".  STATUS INPUT. 
END.
    ENABLE b_create b_del b_quit WITH FRAME butt-frame.
        DO ON ENDKEY UNDO, LEAVE:   
        WAIT-FOR CHOOSE OF b_quit IN FRAME butt-frame.
        END.
            IF VALID-HANDLE(wh)THEN DELETE WIDGET-POOL "new-buttons".
                PROCEDURE dispmsg:
                  MESSAGE "You chose button " SELF:LABEL.END.
