/* sys/inc/f3helpd.i for dialog-box help */

ON CTRL-F OF FRAME {&FRAME-NAME}
    DO:
        {methods/ctrl-f.i}
    END.

ON F3 OF FRAME {&frame-name}
    ANYWHERE
    DO:
        DEFINE VARIABLE ls-prog-name AS cha NO-UNDO.   
        IF NOT CONNECTED("asihlp") THEN 
        DO:
            IF SEARCH("asihelp.pf") <> ? THEN CONNECT -pf value(search("asihelp.pf")).
            ELSE IF SEARCH("asihlp.pf") <> ? THEN CONNECT -pf value(search("asihlp.pf")).
        END.
        IF NOT CONNECTED("asihlp") THEN 
        DO:
            MESSAGE "ASI Help Database is not connected. Contact System Administrator." VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END.
        /* frame-field,frame-file and frame-db are not working when f3 key pressed in a row */
        ls-prog-name = IF PROGRAM-NAME(1) BEGINS "user" THEN ENTRY(2,PROGRAM-NAME(1)," ")      
        ELSE PROGRAM-NAME(1).

        IF CAN-DO("Browse,Frame",SELF:type) THEN
            /* self:name or focus:name */
            RUN sys/ref/hlpd.w (SELF:name, FRAME-FILE, FRAME-DB,ls-prog-name, "English") .
        ELSE RUN sys/ref/hlpd.w (FOCUS:NAME, FOCUS:TABLE, FOCUS:DBNAME,"{&frame-name}", "English") .
        RETURN NO-APPLY.
    END.
