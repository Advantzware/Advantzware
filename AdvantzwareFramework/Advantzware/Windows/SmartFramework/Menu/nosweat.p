/* nosweat.p */

&IF DEFINED(execProgram) EQ 0 &THEN
&SCOPED-DEFINE execProgram mainmenu.
&ENDIF

{methods/defines/globdefs.i &NEW="NEW GLOBAL"}
{methods/defines/hndldefs.i &NEW="NEW"}

DEFINE NEW SHARED VARIABLE quit_login AS LOGICAL NO-UNDO.
DEFINE VARIABLE m_id LIKE NOSWEAT._user._userid NO-UNDO.
DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEF NEW GLOBAL SHARED VAR g-sharpshooter AS LOG NO-UNDO.  /* no, it's yes only from sharpsh.p */

g-sharpshooter = NO.

ASSIGN
  ldummy = SESSION:SET-WAIT-STATE("GENERAL")
  g_version = "2.1A-8.2A".

/*m_id = OS-GETENV("opsysid").                          */
/*                                                      */
/*IF m_id = ? THEN m_id = "".                           */
/*                                                      */
/*IF NOT SETUSERID(m_id,"","NOSWEAT") OR m_id EQ "" THEN*/
/*RUN nosweat/login.w.                                  */
/*                                                      */
/*IF USERID("NOSWEAT") = "" OR quit_login  THEN         */
/*DO:                                                   */
/*  ldummy = SESSION:SET-WAIT-STATE("").                */
/*  QUIT.                                               */
/*END.                                                  */

/*FIND users WHERE users.user_id = USERID("NOSWEAT") NO-LOCK NO-ERROR.*/
/*IF NOT AVAILABLE users THEN                                         */
/*DO:                                                                 */
/*  ldummy = SESSION:SET-WAIT-STATE("").                              */
/*  MESSAGE "User Login Does Not Exist in Users File" SKIP(1)         */
/*    "Contact Systems Manager" VIEW-AS ALERT-BOX ERROR.              */
/*  QUIT.                                                             */
/*END.                                                                */
/*g_track_usage = users.track_usage.                                  */

FOR EACH parmfile NO-LOCK:

  IF SEARCH (parmfile.parmfile) > "" THEN 
    CONNECT -pf VALUE(parmfile.parmfile) NO-ERROR.
  ELSE 
      IF SEARCH (REPLACE (parmfile.parmfile, ".~\", "")) > "" THEN   
        CONNECT -pf VALUE(SEARCH(REPLACE (parmfile.parmfile, ".~\", ""))) NO-ERROR.
  ELSE DO:
      MESSAGE "Cannot find .pf file: " parmfile.parmfile SKIP 
      REPLACE (parmfile.parmfile, ".~\", "")
          VIEW-AS ALERT-BOX ERROR .
          
      RETURN .     
  END.
  
  IF ERROR-STATUS:ERROR THEN
  DO i = 1 TO ERROR-STATUS:NUM-MESSAGES:
    MESSAGE ERROR-STATUS:GET-NUMBER(i) ERROR-STATUS:GET-MESSAGE(i)
        VIEW-AS ALERT-BOX ERROR.
  END.
END.
/* ======= 
  Load program & lookup data 
  =========*/

/*IF userid("nosweat") = "ASI" OR USERID("nosweat") = "NOSWEAT" THEN RUN asiload.p.*/

RUN chkdate.p.
IF CONNECTED("NOSWEAT") THEN
DO:
  RUN createSingleUserPFs.
  {methods/setdevid.i}
  RUN nosweat/persist.p PERSISTENT SET Persistent-Handle.
  RUN lstlogic/persist.p PERSISTENT SET ListLogic-Handle.
  RUN Get_Procedure IN Persistent-HANDLE ("user_dir.",OUTPUT run-proc,YES).
  g_groups = "". /* YSK need to reset */
  FOR EACH usergrps NO-LOCK:
    IF CAN-DO(usergrps.users,USERID("NOSWEAT")) THEN
    g_groups = g_groups + usergrps.usergrps + ",".  /* YSK "," added  */
  END.
  
  init_menu = YES.
  DO WHILE init_menu:
    init_menu = NO.


/*    RUN Advantzware/Windows/SmartFramework/Menu/mainmenu_stub.p PERSISTENT .*/

/*ASSIGN                                                                       */
/*  g_init = TRUE                                                              */
/*  g_company = ""                                                             */
/*  g_loc = "".                                                                */
/*RUN Get_Procedure IN Persistent-Handle ("comp_loc.",OUTPUT run-proc,yes).    */
/*                                                                             */
/*                                                                             */
/*IF g_company = "" OR g_loc = "" THEN                                         */
/*DO:                                                                          */
/*  MESSAGE "No Company and/or Location found for your login ID." SKIP         */
/*      "Please Contact System's Administrator." VIEW-AS ALERT-BOX INFORMATION.*/
/*  RETURN.                                                                    */
/*END.                                                                         */

    RUN Advantzware/Windows/SmartFramework/Menu/start.p . 

/*    RUN Get_Procedure IN Persistent-Handle ("{&execProgram}",OUTPUT run-proc,yes).*/
  END.
END.
ELSE
DO:
  ldummy = SESSION:SET-WAIT-STATE("").
  MESSAGE "CONNECT to NOSWEAT'S Database Failed" SKIP(1)
    "Contact Systems Manager" VIEW-AS ALERT-BOX ERROR.
END. 

ldummy = SESSION:SET-WAIT-STATE("").
QUIT.

PROCEDURE createSingleUserPFs:
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  DO i = 1 TO NUM-DBS:
    OUTPUT TO VALUE(LC(LDBNAME(i)) + '-1.pf').
    PUT UNFORMATTED '-db ' PDBNAME(i) ' -1' SKIP.
    OUTPUT CLOSE.
  END. /* do i */
END PROCEDURE.
