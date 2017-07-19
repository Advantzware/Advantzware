/* nosweat.p */

{methods/defines/globdefs.i &NEW="NEW GLOBAL"}
{methods/defines/hndldefs.i &NEW="NEW"}

DEFINE NEW GLOBAL SHARED VARIABLE g-sharpshooter AS LOGICAL NO-UNDO.
DEFINE NEW        SHARED VARIABLE quit_login     AS LOGICAL NO-UNDO.

ASSIGN
    g-sharpshooter = NO
    g_version      = "2.1A-8.2A"
    .
IF CONNECTED(LDBNAME(1)) AND ldbname(1) EQ "ASI" THEN DO:
    CREATE ALIAS nosweat  FOR DATABASE VALUE(LDBNAME(1)).
    CREATE ALIAS emptrack FOR DATABASE VALUE(LDBNAME(1)).
    CREATE ALIAS jobs     FOR DATABASE VALUE(LDBNAME(1)).
    CREATE ALIAS rfq      FOR DATABASE VALUE(LDBNAME(1)).
    CREATE ALIAS asihelp  FOR DATABASE VALUE(LDBNAME(1)).
    CREATE ALIAS asihlp   FOR DATABASE VALUE(LDBNAME(1)).
    CREATE ALIAS asinos   FOR DATABASE VALUE(LDBNAME(1)).
    init_menu = YES.
    DO WHILE init_menu:
        init_menu = NO.
        RUN Advantzware/Windows/SmartFramework/Menu/start.p . 
    END.
END.
ELSE DO:
    SESSION:SET-WAIT-STATE("").
    MESSAGE "CONNECT to ASI Database Failed" SKIP(1)
      "Contact Systems Manager" VIEW-AS ALERT-BOX ERROR.
END. 
SESSION:SET-WAIT-STATE("").
QUIT.
