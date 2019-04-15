/* sys/inc/f3help.i */

&IF DEFINED(SysCtrlHelp) NE 0 &THEN
DEFINE VARIABLE cHelpField AS CHARACTER NO-UNDO.
&ENDIF

ON f3 OF FRAME {&frame-name}
    ANYWHERE
    DO:
        DEFINE VARIABLE ls-prog-name AS cha NO-UNDO.   
        IF NOT CONNECTED("asihlp") THEN DO:
            IF SEARCH("asihelp.pf") <> ? THEN CONNECT -pf VALUE(SEARCH("asihelp.pf")).
            ELSE IF SEARCH("asihlp.pf") <> ? THEN CONNECT -pf VALUE(SEARCH("asihlp.pf")).
        END.
        IF NOT CONNECTED("asihlp") THEN DO:
            MESSAGE
                "ASI Help Database is not connected. Contact System Administrator."
                VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END.   
        /* frame-field,frame-file and frame-db are not working when f3 key pressed in a row */
        ls-prog-name = IF PROGRAM-NAME(1) BEGINS "user" THEN ENTRY(2,PROGRAM-NAME(1)," ")      
        ELSE PROGRAM-NAME(1).
        IF CAN-DO("Browse,Frame",SELF:TYPE) THEN DO:
            &IF DEFINED(SysCtrlHelp) EQ 0 &THEN
            IF ls-prog-name MATCHES "*viewers/sys-ctrl*" THEN
            RUN sys/ref/hlpd.w (sys-ctrl.name, sys-ctrl.company, FRAME-FIELD, ls-prog-name, "English") .
            &ELSE
            IF ls-prog-name MATCHES "*system/sys-ctrl*" THEN
            RUN sys/ref/hlpd.w (ttSysCtrl.name, g_company, FRAME-FIELD, REPLACE(ls-prog-name,"system","viewers"), "English") .
            &ENDIF
            /* self:name or focus:name */
            ELSE RUN sys/ref/hlpd.w (SELF:NAME, FRAME-FILE, FRAME-DB, ls-prog-name, "English") .
        END.
        ELSE
        &IF DEFINED(SysCtrlHelp) EQ 0 &THEN
        RUN sys/ref/hlpd.w (FOCUS:NAME, FOCUS:TABLE, FOCUS:DBNAME, "{&FRAME-NAME}", "English") .
        &ELSE
        IF ls-prog-name MATCHES "*system/sys-ctrl*" THEN DO:
            MESSAGE 
                "FOCUS:NAME:" FOCUS:NAME
            VIEW-AS ALERT-BOX.
            cHelpField = SUBSTR(FOCUS:NAME,2).
            IF FOCUS:NAME EQ "cFieldDescrip" THEN
            CASE ttSysCtrl.dataType:
                WHEN "Character" THEN
                cHelpField = "char_field_descrip".
                WHEN "Date" THEN
                cHelpField = "date-fld_descrip".
                WHEN "Decimal" THEN
                cHelpField = "dec-fld_descrip".
                WHEN "Integer" THEN
                cHelpField = "int-fld_descrip".
                WHEN "Logical" THEN
                cHelpField = "log-fld_descrip".
            END CASE.
            ELSE IF CAN-DO("cFieldValue,hDate,hDecimal,hInteger,hLogical",FOCUS:NAME) THEN
            CASE ttSysCtrl.dataType:
                WHEN "Character" THEN
                cHelpField = "char-fld".
                WHEN "Date" THEN
                cHelpField = "date-fld".
                WHEN "Decimal" THEN
                cHelpField = "dec-fld".
                WHEN "Integer" THEN
                cHelpField = "int-fld".
                WHEN "Logical" THEN
                cHelpField = "log-fld".
            END CASE.
            ELSE IF FOCUS:NAME EQ "cFieldDefault" THEN
            CASE ttSysCtrl.dataType:
                WHEN "Character" THEN
                cHelpField = "char-fld_default".
                WHEN "Date" THEN
                cHelpField = "date-fld_default".
                WHEN "Decimal" THEN
                cHelpField = "dec-fld_default".
                WHEN "Integer" THEN
                cHelpField = "int-fld_default".
                WHEN "Logical" THEN
                cHelpField = "log-fld_default".
            END CASE.
            RUN sys/ref/hlpd.w (cHelpField, "sys-ctrl", "ASI", "F-Main", "English") .
        END. /* if system/sys-ctrl */
        &ENDIF
        RETURN NO-APPLY.
    END.
