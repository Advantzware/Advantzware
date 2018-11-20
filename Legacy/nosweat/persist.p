/* persist.p */

DEFINE NEW SHARED VARIABLE g_lookup-var  AS CHARACTER     NO-UNDO.
DEFINE SHARED     VARIABLE g_track_usage AS LOGICAL       NO-UNDO.
DEFINE            VARIABLE phandle       AS WIDGET-HANDLE NO-UNDO.
DEFINE            VARIABLE is-running    AS LOGICAL       NO-UNDO.

&Scoped-define IAMPERSIST yes

PROCEDURE Check-Exit :
/* ---------------------------------------------------------------------------
  Purpose:    
  Parameters: 
  Notes :     
  --------------------------------------------------------------------------- */
    DEFINE OUTPUT PARAMETER close-ok AS LOGICAL NO-UNDO.

    DEFINE BUFFER b1-prgrms FOR prgrms.
    DEFINE BUFFER b2-prgrms FOR prgrms.

    ASSIGN
        close-ok = YES
        phandle  = SESSION:FIRST-PROCEDURE
        .
    DO WHILE VALID-HANDLE(phandle):
        IF phandle:PRIVATE-DATA NE ? AND
       INDEX(PROGRAM-NAME(2),phandle:PRIVATE-DATA) NE 0 THEN DO:
            close-ok = NO.
            /* get parent record */
            FIND b1-prgrms WHERE b1-prgrms.prgmname EQ phandle:PRIVATE-DATA NO-LOCK.
            /* get child record */
            FIND b2-prgrms WHERE b2-prgrms.prgmname EQ
                SUBSTR(phandle:FILE-NAME,R-INDEX(phandle:FILE-NAME,"/") + 1,
        INDEX(phandle:FILE-NAME,".") - R-INDEX(phandle:FILE-NAME,"/")) NO-LOCK.
            MESSAGE "Unable to CLOSE '" + b1-prgrms.prgtitle "'." SKIP(1)
                "Child Program '" + b2-prgrms.prgtitle + "' is OPEN!!!" SKIP(1)
                "Please Close '" + b2-prgrms.prgtitle + "' before" SKIP
                "attempting to Close '" + b1-prgrms.prgtitle + "'!!!"
                VIEW-AS ALERT-BOX INFORMATION.
            RUN Set-Focus IN phandle.
            RETURN.
        END.
        phandle = phandle:NEXT-SIBLING.
    END.

END PROCEDURE.

PROCEDURE Enhance :
/* ---------------------------------------------------------------------------
  Purpose:    set font and color settings by user or program
  Parameters: current-frame (WIDGET-HANDLE) - Handle of the frame to process.
  Notes :     
  Syntax :    RUN Enhance IN Persistent-Handle (FRAME {&FRAME-NAME}:HANDLE).
  --------------------------------------------------------------------------- */
    DEFINE INPUT PARAMETER current-frame AS WIDGET-HANDLE NO-UNDO.

    DEFINE VARIABLE prgm-name      AS CHARACTER     NO-UNDO.
    DEFINE VARIABLE current-widget AS WIDGET-HANDLE NO-UNDO.
    DEFINE VARIABLE i              AS INTEGER       NO-UNDO.
    DEFINE VARIABLE use_fonts      AS LOGICAL       NO-UNDO.
    DEFINE VARIABLE use_colors     AS LOGICAL       NO-UNDO.
    DEFINE VARIABLE w-bgc          AS INTEGER       EXTENT 13 NO-UNDO.
    DEFINE VARIABLE w-fgc          AS INTEGER       EXTENT 13 NO-UNDO.
    DEFINE VARIABLE w-font         AS INTEGER       EXTENT 13 NO-UNDO.
    DEFINE VARIABLE widget-list    AS CHARACTER INITIAL
        "BROWSE,BUTTON,COMBO-BOX,DIALOG-BOX,EDITOR,FILL-IN,FRAME,~
LITERAL,RADIO-SET,SELECTION-LIST,RECTANGLE,TEXT,TOGGLE-BOX" NO-UNDO.

    FIND users WHERE users.user_id = USERID("NOSWEAT") NO-LOCK NO-ERROR.
    IF AVAILABLE users THEN
        ASSIGN
            use_fonts  = users.use_font
            use_colors = users.use_colors
            .

    IF NOT(use_fonts OR use_colors) THEN DO:
        ASSIGN
            prgm-name = SUBSTR(PROGRAM-NAME(2),INDEX(PROGRAM-NAME(2),"/") + 1)
            prgm-name = SUBSTR(prgm-name,1,LENGTH(prgm-name) - 1)
            .
        FIND prgrms WHERE prgrms.prgmname = prgm-name NO-LOCK NO-ERROR.
        IF AVAILABLE prgrms THEN
            ASSIGN
                use_fonts  = prgrms.use_font
                use_colors = prgrms.use_colors
                .
        IF NOT(use_fonts OR use_colors) THEN
            RETURN.
        DO i = 1 TO NUM-ENTRIES(widget-list):
            ASSIGN
                w-bgc[i]  = prgrms.widget_bgc[i]
                w-fgc[i]  = prgrms.widget_fgc[i]
                w-font[i] = prgrms.widget_font[i]
                .
        END.
    END.
    ELSE
    DO i = 1 TO NUM-ENTRIES(widget-list):
        ASSIGN
            w-bgc[i]  = users.widget_bgc[i]
            w-fgc[i]  = users.widget_fgc[i]
            w-font[i] = users.widget_font[i]
            .
    END.

    current-widget = current-frame.
    IF CAN-DO(widget-list,current-widget:TYPE) THEN DO:
        IF use_colors THEN
            ASSIGN
                current-widget:BGCOLOR = w-bgc[LOOKUP(current-widget:TYPE,widget-list)]
                current-widget:FGCOLOR = w-fgc[LOOKUP(current-widget:TYPE,widget-list)]
                .
    END.
    ASSIGN
        current-widget = current-widget:FIRST-CHILD
        current-widget = current-widget:FIRST-CHILD
        .
    DO WHILE current-widget NE ?:
        IF CAN-DO(widget-list,current-widget:TYPE) THEN DO:
            IF use_fonts AND NOT CAN-DO("BROWSE,RECTANGLE",current-widget:TYPE) THEN
                current-widget:FONT = w-font[LOOKUP(current-widget:TYPE,widget-list)].
            IF use_colors THEN
                ASSIGN
                    current-widget:BGCOLOR = w-bgc[LOOKUP(current-widget:TYPE,widget-list)]
                    current-widget:FGCOLOR = w-fgc[LOOKUP(current-widget:TYPE,widget-list)]
                    .
        END.
        current-widget = current-widget:NEXT-SIBLING.
    END.

END PROCEDURE.

PROCEDURE Get_Procedure :
/* -----------------------------------------------------------------------------------
  Purpose:    Make sure procedure exists, if not give MESSAGE.
  Parameters: Input procedure name to find, 
              Output full path if found, null if not found,
              Input logical if procedure should be executed here.
  Notes :     
  Syntax :    RUN Get_Procedure IN Persistent-Handle (proc-name,OUTPUT run-proc,yes).
              ************************************ or *******************************
              RUN Get_Procedure IN Persistent-Handle (proc-name,OUTPUT run-proc,no).
              IF run-proc NE "" THEN
              RUN VALUE(run-proc) (INPUT's and/or OUTPUT's as needed).
-------------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER proc-name AS CHARACTER.
    DEFINE OUTPUT PARAMETER run-proc  AS CHARACTER.
    DEFINE INPUT  PARAMETER run-now   AS LOGICAL.

    IF INDEX(proc-name,"..") NE 0 OR INDEX(proc-name,".") = 0 THEN
        RETURN.

    DEFINE BUFFER buf-prgrms FOR prgrms.

    IF NOT SESSION:BATCH-MODE THEN
        RUN Set_Cursor ("WAIT").
  
    FIND buf-prgrms WHERE buf-prgrms.prgmname = proc-name NO-LOCK NO-ERROR.

    IF AVAILABLE buf-prgrms THEN DO:
        run-proc = buf-prgrms.dir_group + "/" + proc-name + "r".
        IF SEARCH(run-proc) = ? THEN
            run-proc = buf-prgrms.dir_group + "/" + proc-name + "p".
        IF SEARCH(run-proc) = ? THEN
            run-proc = buf-prgrms.dir_group + "/" + proc-name + "w".
        IF SEARCH(run-proc) = ? THEN DO:
            IF NOT SESSION:BATCH-MODE THEN DO:
                RUN Set_Cursor ("").
                MESSAGE "Procedure" SUBSTR(run-proc,1,LENGTH(run-proc) - 2) "Does Not Exist"
                    VIEW-AS ALERT-BOX ERROR.
            END.
            run-proc = "".
        END.
        ELSE DO:
            IF buf-prgrms.track_usage OR g_track_usage THEN 
            IF run-now THEN DO:
                IF buf-prgrms.run_persistent THEN DO:
                    IF INDEX(proc-name,"_.") NE 0 THEN DO:
                        run-proc = "system/listrqst.w".
                        {methods/smartrun.i (buf-prgrms.prgmname)}
                    END.
                    ELSE
                    {methods/smartrun.i}
                END.
                ELSE
                    IF proc-name= "about." THEN
                        RUN VALUE(run-proc) (PROGRAM-NAME(2)).
                    ELSE
                        IF proc-name= "help." AND INDEX(PROGRAM-NAME(2),"mainmenu") NE 0 THEN
                            RUN VALUE(run-proc) ("mainmenu.",0).
                        ELSE RUN VALUE(run-proc).
                run-proc = "".
            END.
        END.
    END.
    ELSE
        IF NOT SESSION:BATCH-MODE THEN DO:
            RUN Set_Cursor ("").
            MESSAGE "Program :" proc-name SKIP(1)
                "Program Master Record Does Not Exist - Contact Systems Manager" 
                VIEW-AS ALERT-BOX ERROR.
        END.
 
END PROCEDURE.

PROCEDURE Run_applhelp :
/*---------------------------------------------------------------------------------
  Purpose:    Allow user to access applhelp in a run statement
  Parameters: current-frame (WIDGET-HANDLE) - Handle of the frame to process.
  Notes :     
  Syntax :    RUN Run_applhelp IN Persistent-Handle (FRAME {&FRAME-NAME}:HANDLE).
----------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER current-frame AS WIDGET-HANDLE NO-UNDO.
    
    DEFINE VARIABLE current-widget AS WIDGET-HANDLE NO-UNDO.

    ASSIGN
        current-widget = current-frame
        current-widget = current-widget:FIRST-CHILD
        current-widget = current-widget:FIRST-CHILD
        .
    DO WHILE current-widget NE ?:
        IF current-widget:NAME = FRAME-FIELD THEN
            LEAVE.
        current-widget = current-widget:NEXT-SIBLING.
    END.
    RUN applhelp.p.
    IF g_lookup-var NE "" THEN
        current-widget:SCREEN-VALUE = g_lookup-var.
    APPLY "ENTRY" TO current-widget.

END PROCEDURE.

PROCEDURE Running_Procedures :
/*---------------------------------------------------------------------------------
  Purpose:    Check Persistent Procedures Running
  Parameters: Program Name
  Notes :     
----------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER progname   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER is-running AS LOGICAL   NO-UNDO.

    DEFINE BUFFER bprgrms FOR prgrms.

    ASSIGN
        is-running = NO
        phandle    = SESSION:FIRST-PROCEDURE
        .
    DO WHILE VALID-HANDLE(phandle):
        IF INDEX(phandle:FILE-NAME,progname) NE 0 THEN DO:
            IF INDEX(progname,"notes.") NE 0 OR
         INDEX(progname,"mfvalues.") NE 0 OR
         INDEX(progname,"listrqst.") NE 0 THEN
                DELETE PROCEDURE phandle.
            ELSE DO:
                is-running = YES.
                RUN Set_Cursor ("").
                RUN Set-Focus IN phandle NO-ERROR.
            END.
            RETURN.
        END.
        ELSE
            phandle = phandle:NEXT-SIBLING.
    END.

END PROCEDURE.

PROCEDURE Set_Cursor :
/*---------------------------------------------------------------------------------
  Purpose:    Set Cursor Appearance
  Parameters: either "WAIT" or "".
  Notes :     
----------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER cursor_type AS CHARACTER NO-UNDO.

    SESSION:SET-WAIT-STATE(IF cursor_type = "WAIT" THEN "GENERAL" ELSE "").
END PROCEDURE.

PROCEDURE Set_Primary_Fields :
/*---------------------------------------------------------------------------------
  Purpose:    Sets different background color for primary index fields
  Parameters: current-frame (WIDGET-HANDLE) - Handle of the frame to process.
  Notes :     
----------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER current-frame AS WIDGET-HANDLE NO-UNDO.
    DEFINE VARIABLE current-widget AS WIDGET-HANDLE NO-UNDO.
    DEFINE VARIABLE indx-fields    AS CHARACTER     NO-UNDO.
    DEFINE VARIABLE run-proc       AS CHARACTER     NO-UNDO.

    ASSIGN
        current-widget = current-frame
        current-widget = current-widget:FIRST-CHILD
        current-widget = current-widget:FIRST-CHILD
        .
    DO WHILE current-widget NE ?:
        IF current-widget:DBNAME NE ? THEN DO:
            IF indx-fields = "" THEN DO:
                RUN Get_Procedure IN THIS-PROCEDURE ("primflds.",OUTPUT run-proc,NO).
                IF run-proc NE "" THEN DO:
                    CREATE ALIAS dictdb FOR DATABASE VALUE(current-widget:DBNAME).
                    RUN VALUE(run-proc) (current-widget:TABLE,OUTPUT indx-fields).
                END.
            END.
            IF CAN-DO(indx-fields,current-widget:NAME) THEN
                ASSIGN
                    current-widget:BGCOLOR = 3
                    current-widget:FGCOLOR = 15
                    .
        END.
        current-widget = current-widget:NEXT-SIBLING.
    END.
END PROCEDURE.

PROCEDURE Tool_Tips :
/*---------------------------------------------------------------------------------
  Purpose:    Set Tool Tips to objects
  Parameters: current-frame (WIDGET-HANDLE) - Handle of the frame to process.
  Notes :     
  Syntax :    RUN Tool_Tips IN Persistent-Handle (FRAME {&FRAME-NAME}:HANDLE).
----------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER current-frame AS WIDGET-HANDLE NO-UNDO.

/*    DEFINE VARIABLE current-widget AS WIDGET-HANDLE NO-UNDO.                                */
/*                                                                                            */
/*    IF TRUE THEN RETURN.                                                                    */
/*                                                                                            */
/*    ASSIGN                                                                                  */
/*        current-widget = current-frame                                                      */
/*        current-widget = current-widget:FIRST-CHILD                                         */
/*        current-widget = current-widget:FIRST-CHILD                                         */
/*        .                                                                                   */
/*    DO WHILE current-widget NE ?:                                                           */
/*        IF current-widget:TYPE NE "RECTANGLE" AND current-widget:LABEL NE "" THEN DO:       */
/*            FIND acclrtrs WHERE acclrtrs.acclrtr = current-widget:LABEL NO-LOCK NO-ERROR.   */
/*            IF AVAILABLE acclrtrs THEN                                                      */
/*                current-widget:TOOLTIP = current-widget:TOOLTIP + " - " + acclrtrs.func_key.*/
/*        END.                                                                                */
/*        current-widget = current-widget:NEXT-SIBLING.                                       */
/*    END.                                                                                    */

END PROCEDURE.

{custom/persist.i}
