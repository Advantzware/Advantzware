/* asiload.p  Load new data into system files */
/* Revised 6/22/17 - MYT - reformatted, added modules table entries */

DEF TEMP-TABLE ttPrgms LIKE prgrms.
DEF TEMP-TABLE ttPrgmxref LIKE prgmxref.
DEF TEMP-TABLE ttEmailcod LIKE emailcod.
DEF TEMP-TABLE ttPrgmxreftable LIKE reftable.
DEF TEMP-TABLE ttNotes LIKE notes.
DEF TEMP-TABLE ttModule LIKE module.

DEF VAR ll-ans AS LOG NO-UNDO.
DEF VAR i AS INT NO-UNDO.
DEF VAR v1 AS CHAR FORMAT "x(15)" NO-UNDO.
DEF VAR v2 AS CHAR FORMAT "x(15)" NO-UNDO.
DEF VAR v3 LIKE lookups.frame_field NO-UNDO.
DEF VAR v4 LIKE lookups.prgmname NO-UNDO.
DEF VAR v5 LIKE lookups.rec_key NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF reftable.

ASSIGN
    ll-ans = YES.

IF SEARCH (".\prgrms.d") NE ? 
AND CAN-DO('ASI,NoSweat',USERID(ldbname(1))) THEN DO:
    IF ll-ans THEN DO:
        SESSION:SET-WAIT-STATE("general").
        OUTPUT TO prgmxref.sav.
        FOR EACH prgmxref:
            EXPORT prgmxref.
        END.
        OUTPUT CLOSE.
      
        OUTPUT TO prgrms.sav.
        FOR EACH prgrms:
            EXPORT prgrms.
        END.
        OUTPUT CLOSE.
      
        /* update  prgrms */
        INPUT FROM prgrms.d NO-ECHO.
        REPEAT :
            create ttPrgms.
            IMPORT 
                ttPrgms.prgmname
                ttPrgms.prgtitle
                ttPrgms.run_persistent
                ttPrgms.can_run
                ttPrgms.can_create
                ttPrgms.can_update
                ttPrgms.can_delete
                ttPrgms.dir_group
                ttPrgms.use_colors
                ttPrgms.use_fonts
                ttPrgms.widget_bgc
                ttPrgms.widget_fgc
                ttPrgms.widget_font
                ttPrgms.track_usage
                ttPrgms.popup
                ttPrgms.prgm_ver
                ttPrgms.menu_item
                ttPrgms.mfgroup 
                ttPrgms.rec_key.

            FIND FIRST prgrms EXCLUSIVE WHERE 
                prgrms.prgmname = ttPrgms.prgmname 
                NO-ERROR.
            IF NOT AVAIL prgrms THEN DO:
                CREATE prgrms.
                BUFFER-COPY ttPrgms TO prgrms
                ASSIGN 
                    prgrms.can_run = '*'
                    prgrms.can_create = '*'
                    prgrms.can_update = '*'
                    prgrms.can_delete = '*'.
            END.
            ELSE DO:
                ASSIGN 
                    prgrms.prgtitle  = ttPrgms.prgtitle
                    prgrms.run_persistent = ttPrgms.RUN_persistent
                    prgrms.dir_group = ttPrgms.DIR_group
                    prgrms.use_colors = ttPrgms.USE_colors
                    prgrms.use_fonts = ttPrgms.USE_fonts
                    prgrms.track_usage = ttPrgms.track_usage
                    prgrms.popup = ttPrgms.popup
                    prgrms.prgm_ver = ttPrgms.prgm_ver
                    prgrms.menu_item = ttPrgms.MENU_item
                    prgrms.mfgroup = ttPrgms.mfgroup.

                DO i = 1 TO 13:
                    ASSIGN 
                        prgrms.widget_bgc[i] = ttPrgms.WIDGET_bgc[i]
                        prgrms.widget_fgc[i] = ttPrgms.WIDGET_fgc[i]
                        prgrms.widget_font[i] = ttPrgms.WIDGET_font[i].
                END.
            END.
        END.
        
        /* delete records no longer used */
        DISABLE TRIGGERS FOR LOAD OF prgrms.
        FOR EACH prgrms EXCLUSIVE WHERE 
            NOT CAN-FIND(FIRST ttPrgms WHERE ttPrgms.prgmname = prgrms.prgmname ):
            FOR EACH prgmxref EXCLUSIVE WHERE
                prgmxref.prgmname = prgrms.prgmname:
                DELETE prgmxref.
            END.
            DELETE prgrms.
        END.

        /* update prgmxref */
        INPUT FROM prgmxref.d NO-ECHO.
        REPEAT:
            CREATE ttPrgmxref.
            SET 
                ttPrgmxref.table_name
                ttPrgmxref.prgmname
                ttPrgmxref.pageno.
            FIND FIRST prgmxref EXCLUSIVE WHERE 
                prgmxref.TABLE_name = ttPrgmxref.TABLE_name 
                NO-ERROR.
            IF NOT AVAIL prgmxref THEN DO:
                CREATE prgmxref.
                BUFFER-COPY ttPrgmxref TO prgmxref.
            END.
            ELSE DO:
                ASSIGN 
                    prgmxref.prgmname = ttPrgmxref.prgmname
                    prgmxref.pageno = ttPrgmxref.pageno.
            END.
        END.
        OS-COPY prgrms.d prgrms.OLD.
        OS-COPY prgmxref.d prgmxref.OLD.
        OS-DELETE prgrms.d.
        OS-DELETE prgmxref.d.
    END.
END.

IF SEARCH (".\lookups.d") NE ? 
AND CAN-DO('ASI,NoSweat',USERID(ldbname(1))) THEN DO:
    IF NOT ll-ans THEN MESSAGE 
        "Load Lookup Data?" 
        VIEW-AS ALERT-BOX QUESTION
        BUTTONS YES-NO UPDATE ll-ans.
    IF ll-ans THEN DO:
        SESSION:SET-WAIT-STATE("general").
        OUTPUT TO lookups.sav.
        FOR EACH lookups:
            EXPORT lookups.
            DELETE lookups.
        END.
        OUTPUT CLOSE.
      
        INPUT FROM lookups.d NO-ECHO.
        REPEAT:
            SET v1 v2 v3 v4 v5.
            CREATE lookups.
            ASSIGN 
                lookups.FRAME_db = v1
                lookups.FRAME_file = v2
                lookups.FRAME_field = v3
                lookups.prgmname = v4
                lookups.rec_key = v5.
        END.
        INPUT CLOSE.
        OS-COPY lookups.d lookups.OLD.
        OS-DELETE lookups.d. 
    END.
END.

IF SEARCH(".\emailcod.d") NE ? 
AND CAN-DO('ASI,NoSweat',USERID(ldbname(1))) THEN DO:
    CREATE ttEmailcod.
    IF NOT ll-ans THEN MESSAGE 
        "Load Email Codes Data?" 
        VIEW-AS ALERT-BOX QUESTION
        BUTTONS YES-NO UPDATE ll-ans.
    IF ll-ans THEN DO:
        SESSION:SET-WAIT-STATE("general").
        OUTPUT TO emailcod.sav.
        FOR EACH emailcod NO-LOCK:
            EXPORT emailcod.
        END.
        OUTPUT CLOSE.
    
        INPUT FROM emailcod.d NO-ECHO.
        REPEAT:
            IMPORT ttEmailcod.
            IF CAN-FIND(emailcod WHERE 
                        emailcod.emailcod EQ ttEmailcod.emailcod) THEN NEXT.
            CREATE emailcod.
            BUFFER-COPY ttEmailcod EXCEPT rec_key TO emailcod.
        END.
        INPUT CLOSE.
        OS-COPY emailcod.d emailcod.old.
        OS-DELETE emailcod.d. 
    END.
END.

/* reftable: utility notes */
IF SEARCH(".\notes.d") NE ? 
AND CAN-DO('ASI,NoSweat',USERID(ldbname(1))) THEN DO:
    CREATE ttNotes.
    IF NOT ll-ans THEN MESSAGE 
        "Load Utility Code Notes Data?" 
        VIEW-AS ALERT-BOX QUESTION
        BUTTONS YES-NO UPDATE ll-ans.
    IF ll-ans THEN DO:
        SESSION:SET-WAIT-STATE("general").
        OUTPUT TO notes.sav.
        FOR EACH reftable NO-LOCK WHERE 
            reftable.reftable EQ 'Utilities':
            FOR EACH notes NO-LOCK WHERE 
                notes.rec_key EQ reftable.rec_key:
                EXPORT notes.
                DELETE notes.
            END. /* each notes */
        END. /* each reftable */
        OUTPUT CLOSE.
        
        INPUT FROM notes.d NO-ECHO.
        REPEAT:
            IMPORT ttNotes.
            FIND FIRST notes EXCLUSIVE WHERE 
                notes.rec_key EQ ttNotes.rec_key AND 
                notes.note_date EQ ttNotes.note_date AND 
                notes.note_time EQ ttNotes.note_time 
                NO-ERROR.
            IF NOT AVAILABLE notes THEN
                CREATE notes.
            BUFFER-COPY ttNotes TO notes.
        END. /* repeat */
        INPUT CLOSE.
        OS-COPY notes.d notes.old.
        OS-DELETE notes.d. 
    END.
END. 

IF SEARCH(".\reftable.d") NE ? 
AND CAN-DO('ASI,NoSweat',USERID(ldbname(1))) THEN DO:
    CREATE ttPrgmxreftable.
    IF NOT ll-ans THEN MESSAGE 
        "Load Utility Codes Data?" 
        VIEW-AS ALERT-BOX QUESTION
        BUTTONS YES-NO UPDATE ll-ans.

    IF ll-ans THEN DO:
        SESSION:SET-WAIT-STATE("general").
        OUTPUT TO reftable.sav.
        FOR EACH reftable EXCLUSIVE WHERE 
            reftable.reftable EQ 'Utilities':
            EXPORT reftable.
            DELETE reftable.
        END. /* each reftable */
        OUTPUT CLOSE.
        INPUT FROM reftable.d NO-ECHO.
        REPEAT:
            IMPORT ttPrgmxreftable.
            FIND FIRST reftable EXCLUSIVE WHERE 
                reftable.reftable EQ ttPrgmxreftable.reftable AND 
                reftable.company EQ ttPrgmxreftable.company AND 
                reftable.loc EQ ttPrgmxreftable.loc 
                NO-ERROR.
            IF NOT AVAILABLE reftable THEN
                CREATE reftable.
            BUFFER-COPY ttPrgmxreftable TO reftable.
        END.
        INPUT CLOSE.
        OS-COPY reftable.d reftable.old.
        OS-DELETE reftable.d. 
    END.
END.

IF SEARCH (".\module.d") <> ? 
AND CAN-DO('ASI,NoSweat',USERID(ldbname(1))) THEN DO:
    CREATE ttModule.
    IF NOT ll-ans THEN MESSAGE 
        "Load Modules Data?" 
        VIEW-AS ALERT-BOX QUESTION
        BUTTONS YES-NO UPDATE ll-ans.
    IF ll-ans THEN DO:
        SESSION:SET-WAIT-STATE("general").
        OUTPUT TO module.sav.
        FOR EACH module NO-LOCK:
            EXPORT module.
        END.
        OUTPUT CLOSE.
        INPUT FROM module.d NO-ECHO.
        REPEAT:
            IMPORT ttModule.
            IF CAN-FIND(module WHERE 
                        module.db-name EQ ttModule.db-name AND
                        module.module EQ ttModule.module) THEN NEXT.
            CREATE module.
            BUFFER-COPY ttModule TO module.
        END.
        INPUT CLOSE.
        OS-COPY module.d module.old.
        OS-DELETE module.d. 
    END.
END.

SESSION:SET-WAIT-STATE("").
