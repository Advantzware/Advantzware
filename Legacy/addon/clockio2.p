/* touchscr.p */
/*PROPATH = "..\," + PROPATH. */
/*assign propath = "r:\asi9test\patch\rco300,r:\asi9test\patch\rco300\addon,r:\asi9test\patch\pco100,r:\asi9test\patch\pco100\addon,r:\asi9test\rco300\,r:\asi9test\rco300\addon,r:\asi9test\pco100\,r:\asi9test\pco100\addon," + PROPATH.*/

propath = "p:\asi10test\patch\rco1010,p:\asi10test\patch\rco1010\addon,p:\asi10test\rco1010,p:\asi10test\rco1010\addon," + PROPATH.

{methods/defines/globdefs.i &NEW="NEW GLOBAL"}
{methods/defines/hndldefs.i &NEW="NEW"}

DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.

ldummy = SESSION:SET-WAIT-STATE("GENERAL").

FOR EACH parmfile NO-LOCK:
  CONNECT -pf VALUE(parmfile.parmfile) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
  DO i = 1 TO ERROR-STATUS:NUM-MESSAGES:
    MESSAGE ERROR-STATUS:GET-NUMBER(i) ERROR-STATUS:GET-MESSAGE(i)
        VIEW-AS ALERT-BOX ERROR.
  END.
END.

IF CONNECTED("NOSWEAT") THEN DO:
    /*SESSION:TIME-SOURCE = "nosweat". */
    RUN ./nosweat/persist.p PERSISTENT SET Persistent-Handle.
    RUN touch/clockio.w.
END.
ELSE
DO:
  ldummy = SESSION:SET-WAIT-STATE("").
  MESSAGE "CONNECT to NOSWEAT'S Database Failed" SKIP(1)
    "Contact Systems Manager" VIEW-AS ALERT-BOX ERROR.
END. 

ldummy = SESSION:SET-WAIT-STATE("").
QUIT.
