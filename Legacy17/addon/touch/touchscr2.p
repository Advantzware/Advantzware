/* touch/touchscr2.p */

{methods/defines/globdefs.i}

DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE quit_login AS LOGICAL NO-UNDO.
DEFINE VARIABLE m_id LIKE NOSWEAT._user._userid NO-UNDO.

{sys/inc/tslogin.i}

IF tslogin-log THEN DO:
  m_id = OS-GETENV("OPSYSID").
  IF m_id = ? THEN m_id = "".
  IF NOT SETUSERID(m_id,"",ldbname(1)) THEN RUN nosweat/login.w.

  IF USERID(ldbname(1)) = "" OR quit_login THEN
  DO:
    ldummy = SESSION:SET-WAIT-STATE("").
    RETURN error.
  END.

  FIND users WHERE users.user_id = USERID(ldbname(1)) NO-LOCK NO-ERROR.
  IF NOT AVAILABLE users THEN
  DO:     
    ldummy = SESSION:SET-WAIT-STATE("").
    MESSAGE "User Login Does Not Exist in Users File" SKIP(1)
      "Contact Systems Manager" VIEW-AS ALERT-BOX ERROR.
    RETURN error.
  END.
  ASSIGN g_company = ""
         g_loc = "".

  FIND FIRST usercomp WHERE usercomp.user_id = USERID(ldbname(1)) AND usercomp.company_default 
       NO-LOCK NO-ERROR.
  IF AVAIL usercomp THEN do:
    g_company = usercomp.company.
    FIND FIRST ASI.usercomp WHERE usercomp.user_id = USERID(ldbname(1)) AND
            usercomp.company = g_company AND
            usercomp.loc NE "" AND usercomp.loc_default = yes
            NO-LOCK NO-ERROR.
    IF AVAIL usercomp THEN g_loc = usercomp.loc.
  END.
  IF g_company = "" THEN DO:
      MESSAGE
      "No Default Company or Location Exists. Please contact system administrator for help."
        VIEW-AS ALERT-BOX ERROR.
      RETURN error.
  END.
  g_track_usage = users.track_usage.
END.  /* prompt login */

RUN custom/gettime.p.   /* time-source */
