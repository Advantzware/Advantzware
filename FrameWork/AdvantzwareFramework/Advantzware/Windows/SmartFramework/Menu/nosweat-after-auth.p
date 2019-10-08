/* nosweat-after-auth.p */

&IF DEFINED(execProgram) EQ 0 &THEN
&SCOPED-DEFINE execProgram mainmenu.
&ENDIF

{methods/defines/globdefs.i}
{methods/defines/hndldefs.i}

DEFINE NEW SHARED VARIABLE quit_login AS LOGICAL NO-UNDO.
DEFINE VARIABLE m_id LIKE ASI._user._userid NO-UNDO.
DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEF NEW GLOBAL SHARED VAR g-sharpshooter AS LOG NO-UNDO.  /* no, it's yes only from sharpsh.p */

ASSIGN
  g-sharpshooter = NO
  ldummy = SESSION:SET-WAIT-STATE("GENERAL")
  g_version = "2.1A-8.2A"
  .
FIND users NO-LOCK WHERE users.user_id EQ USERID("ASI") NO-ERROR.
IF NOT AVAILABLE users THEN DO:     
  SESSION:SET-WAIT-STATE("").
  MESSAGE "User Login Does Not Exist in Users File" SKIP(1)
    "Contact Systems Manager" VIEW-AS ALERT-BOX ERROR.
  QUIT.
END.
g_track_usage = users.track_usage.
IF CONNECTED(LDBNAME(1)) AND LDBNAME(1) EQ "ASI" THEN DO:
    CREATE ALIAS nosweat  FOR DATABASE VALUE(LDBNAME(1)).
    CREATE ALIAS emptrack FOR DATABASE VALUE(LDBNAME(1)).
    CREATE ALIAS jobs     FOR DATABASE VALUE(LDBNAME(1)).
    CREATE ALIAS rfq      FOR DATABASE VALUE(LDBNAME(1)).
    CREATE ALIAS asihelp  FOR DATABASE VALUE(LDBNAME(1)).
    CREATE ALIAS asihlp   FOR DATABASE VALUE(LDBNAME(1)).
    CREATE ALIAS asinos   FOR DATABASE VALUE(LDBNAME(1)).  
  IF USERID("ASI") EQ "ASI" OR USERID("ASI") EQ "NOSWEAT" THEN RUN asiload.p.
  RUN chkdate.p.
  {methods/setdevid.i}
  IF NOT VALID-HANDLE (Persistent-Handle) THEN
  RUN nosweat/persist.p PERSISTENT SET Persistent-Handle.
  IF NOT VALID-HANDLE (ListLogic-Handle) THEN
  RUN lstlogic/persist.p PERSISTENT SET ListLogic-Handle.
  RUN Get_Procedure IN Persistent-HANDLE ("user_dir.",OUTPUT run-proc,yes).
  g_groups = "". /* YSK need to reset */
  FOR EACH usergrps NO-LOCK:
    IF CAN-DO(usergrps.users,USERID("ASI")) THEN
    g_groups = g_groups + usergrps.usergrps + ",".  /* YSK "," added  */
  END.  
  init_menu = yes.
  DO WHILE init_menu:
    init_menu = no.
    RUN Advantzware/Windows/SmartFramework/Menu/mainmenu_stub.p PERSISTENT .
    ASSIGN
      g_init = TRUE 
      g_company = ""
      g_loc = ""
      .
    RUN Get_Procedure IN Persistent-Handle ("comp_loc.",OUTPUT run-proc,yes).    
    IF g_company EQ "" OR g_loc EQ "" THEN DO:
      MESSAGE "No Company and/or Location found for your login ID." SKIP
          "Please Contact System's Administrator." VIEW-AS ALERT-BOX INFORMATION.
      RETURN.
    END.
  END.
END.
ELSE DO:
  ldummy = SESSION:SET-WAIT-STATE("").
  MESSAGE "CONNECT to ASI Database Failed" SKIP(1)
    "Contact Systems Manager" VIEW-AS ALERT-BOX ERROR.
END. 
SESSION:SET-WAIT-STATE("").
