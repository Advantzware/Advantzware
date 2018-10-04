/* nosweat.p */

&IF DEFINED(execProgram) EQ 0 &THEN
&SCOPED-DEFINE execProgram mainmenu.
&ENDIF

{methods/defines/globdefs.i &NEW="NEW GLOBAL"}
{methods/defines/hndldefs.i &NEW="NEW"}

DEFINE NEW SHARED        VARIABLE quit_login      AS LOGICAL   NO-UNDO.
DEFINE                   VARIABLE m_id            LIKE ASI._user._userid NO-UNDO.
DEFINE                   VARIABLE ldummy          AS LOGICAL   NO-UNDO.
DEFINE                   VARIABLE i               AS INTEGER   NO-UNDO.
DEFINE                   VARIABLE cEulaFile       AS CHARACTER NO-UNDO.
DEFINE                   VARIABLE cEulaVersion    AS CHARACTER NO-UNDO.
DEFINE                   VARIABLE lEulaAccepted   AS LOGICAL   NO-UNDO.

DEFINE NEW GLOBAL SHARED VARIABLE g-sharpshooter  AS LOG       NO-UNDO.  /* no, it's yes only from sharpsh.p */

ASSIGN
    g-sharpshooter = NO
    ldummy         = SESSION:SET-WAIT-STATE("GENERAL")
    g_version      = "2.1A-8.2A"
    m_id           = OS-GETENV("opsysid")
    .
IF m_id EQ ? THEN m_id = "".

IF NOT SETUSERID(m_id,"","ASI") OR m_id EQ "" THEN
    RUN nosweat/login.w.

IF USERID("ASI") EQ "" OR quit_login THEN DO:
    ldummy = SESSION:SET-WAIT-STATE("").
    QUIT.
END.

FIND users NO-LOCK WHERE users.user_id EQ USERID("ASI") NO-ERROR.
IF NOT AVAILABLE users THEN DO:     
    ldummy = SESSION:SET-WAIT-STATE("").
    MESSAGE "User Login Does Not Exist in Users File" SKIP(1)
        "Contact Systems Manager" VIEW-AS ALERT-BOX ERROR.
    QUIT.
END.
g_track_usage = users.track_usage.

/*FOR EACH parmfile NO-LOCK:                                                             */
/*    IF SEARCH (parmfile.parmfile) > "" THEN                                            */
/*        CONNECT -pf VALUE(parmfile.parmfile) NO-ERROR.                                 */
/*    ELSE                                                                               */
/*        IF SEARCH (REPLACE (parmfile.parmfile, ".~\", "")) > "" THEN                   */
/*            CONNECT -pf VALUE(SEARCH(REPLACE (parmfile.parmfile, ".~\", ""))) NO-ERROR.*/
/*        ELSE DO:                                                                       */
/*            MESSAGE "Cannot find .pf file: " parmfile.parmfile SKIP                    */
/*                REPLACE (parmfile.parmfile, ".~\", "")                                 */
/*                VIEW-AS ALERT-BOX ERROR .                                              */
/*                                                                                       */
/*            RETURN .                                                                   */
/*        END.                                                                           */
/*    IF ERROR-STATUS:ERROR THEN                                                         */
/*    DO i = 1 TO ERROR-STATUS:NUM-MESSAGES:                                             */
/*        MESSAGE ERROR-STATUS:GET-NUMBER(i) ERROR-STATUS:GET-MESSAGE(i)                 */
/*            VIEW-AS ALERT-BOX ERROR.                                                   */
/*    END.                                                                               */
/*END.                                                                                   */
/* ======= 
  Load program & lookup data 
  =========*/
  
if connected(ldbname(1))
and ldbname(1) = "ASI" then do:
    create alias nosweat for database value(ldbname(1)).
    create alias emptrack for database value(ldbname(1)).
    create alias jobs for database value(ldbname(1)).
    create alias rfq for database value(ldbname(1)).
    create alias asihelp for database value(ldbname(1)).
    create alias asihlp for database value(ldbname(1)).
    create alias asinos for database value(ldbname(1)).
    
    IF USERID("ASI") EQ "ASI" OR USERID("ASI") EQ "NOSWEAT" THEN RUN asiload.p.

    RUN chkdate.p.
    cEulaFile = SEARCH("Eula.txt").

    lEulaAccepted = NO.
    IF cEulaFile NE ? THEN DO:
        RUN system/checkEula.p (INPUT cEulaFile, OUTPUT lEulaAccepted, OUTPUT cEulaVersion).
        IF NOT lEulaAccepted THEN 
            RUN windows/wUserEula.w (INPUT cEulaFile, OUTPUT lEulaAccepted).
    END. 
    ELSE 
        MESSAGE "User Agreement File Not Found! Exiting."
            VIEW-AS ALERT-BOX.
          
    IF NOT lEulaAccepted THEN 
        QUIT.
    
    RUN system/checkExpiredLicense.p.
            
/*    RUN createSingleUserPFs.*/
    {methods/setdevid.i}
    RUN nosweat/persist.p PERSISTENT SET Persistent-Handle.
    RUN lstlogic/persist.p PERSISTENT SET ListLogic-Handle.
    RUN Get_Procedure IN Persistent-HANDLE ("user_dir.",OUTPUT run-proc,YES).
    g_groups = "". /* YSK need to reset */
    FOR EACH usergrps NO-LOCK:
        IF CAN-DO(usergrps.users,USERID("ASI")) THEN
            g_groups = g_groups + usergrps.usergrps + ",".  /* YSK "," added  */
    END.
  
    init_menu = YES.
    DO WHILE init_menu:
        init_menu = NO.
        RUN Get_Procedure IN Persistent-Handle ("{&execProgram}",OUTPUT run-proc,YES).
    END.
END.
ELSE DO:
    ldummy = SESSION:SET-WAIT-STATE("").
    MESSAGE "CONNECT to ASI'S Database Failed" SKIP(1)
        "Contact Systems Manager" VIEW-AS ALERT-BOX ERROR.
END. 

ldummy = SESSION:SET-WAIT-STATE("").
QUIT.

/*PROCEDURE createSingleUserPFs:                    */
/*    DEFINE VARIABLE i AS INTEGER NO-UNDO.         */
/*                                                  */
/*    DO i = 1 TO NUM-DBS:                          */
/*        OUTPUT TO VALUE(LC(LDBNAME(i)) + '-1.pf').*/
/*        PUT UNFORMATTED                           */
/*            '-db ' PDBNAME(i) ' -1' SKIP.         */
/*        OUTPUT CLOSE.                             */
/*    END. /* do i */                               */
/*END PROCEDURE.                                    */
