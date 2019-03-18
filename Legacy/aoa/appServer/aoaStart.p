/* aoaStart.p */

DEFINE INPUT PARAMETER cStartup AS CHARACTER NO-UNDO .

IF CONNECTED(LDBNAME(1)) AND LDBNAME(1) EQ "ASI" THEN DO: 
    CREATE ALIAS nosweat FOR DATABASE VALUE(LDBNAME (1)). 
    CREATE ALIAS emptrack FOR DATABASE VALUE(LDBNAME (1)). 
    CREATE ALIAS jobs FOR DATABASE VALUE(LDBNAME (1)). 
    CREATE ALIAS rfq FOR DATABASE VALUE(LDBNAME (1)). 
    CREATE ALIAS asihelp FOR DATABASE VALUE(LDBNAME (1)). 
    CREATE ALIAS asihlp FOR DATABASE VALUE(LDBNAME (1)). 
    CREATE ALIAS asinos FOR DATABASE VALUE(LDBNAME (1)). 
END. 

DEFINE VARIABLE hProc    AS HANDLE  NO-UNDO.
DEFINE VARIABLE iAuditID AS INTEGER NO-UNDO.

RUN AOA\appServer\aoaBin.p PERSISTENT SET hProc.
SESSION:ADD-SUPER-PROCEDURE (hProc).

RUN AOA\appServer\aoaAdmin.p PERSISTENT SET hProc.
SESSION:ADD-SUPER-PROCEDURE (hProc).

RUN AOA\appServer\aoaAP.p PERSISTENT SET hProc.
SESSION:ADD-SUPER-PROCEDURE (hProc).

RUN AOA\appServer\aoaAR.p PERSISTENT SET hProc.
SESSION:ADD-SUPER-PROCEDURE (hProc).

RUN AOA\appServer\aoaDC.p PERSISTENT SET hProc.
SESSION:ADD-SUPER-PROCEDURE (hProc).

RUN AOA\appServer\aoaEQ.p PERSISTENT SET hProc.
SESSION:ADD-SUPER-PROCEDURE (hProc).

RUN AOA\appServer\aoaFG.p PERSISTENT SET hProc.
SESSION:ADD-SUPER-PROCEDURE (hProc).

RUN AOA\appServer\aoaGL.p PERSISTENT SET hProc.
SESSION:ADD-SUPER-PROCEDURE (hProc).

RUN AOA\appServer\aoaHS.p PERSISTENT SET hProc.
SESSION:ADD-SUPER-PROCEDURE (hProc).

RUN AOA\appServer\aoaJC.p PERSISTENT SET hProc.
SESSION:ADD-SUPER-PROCEDURE (hProc).

RUN AOA\appServer\aoaNS.p PERSISTENT SET hProc.
SESSION:ADD-SUPER-PROCEDURE (hProc).

RUN AOA\appServer\aoaOE.p PERSISTENT SET hProc.
SESSION:ADD-SUPER-PROCEDURE (hProc).

RUN AOA\appServer\aoaPO.p PERSISTENT SET hProc.
SESSION:ADD-SUPER-PROCEDURE (hProc).

RUN AOA\appServer\aoaRM.p PERSISTENT SET hProc.
SESSION:ADD-SUPER-PROCEDURE (hProc).

RUN AOA\appServer\aoaSB.p PERSISTENT SET hProc.
SESSION:ADD-SUPER-PROCEDURE (hProc).

RUN AOA\appServer\aoaTS.p PERSISTENT SET hProc.
SESSION:ADD-SUPER-PROCEDURE (hProc).

RUN system\session.p PERSISTENT SET hProc.
SESSION:ADD-SUPER-PROCEDURE (hProc).

RUN spCreateAuditHdr (
    "LOG",       /* type  */
    "ASI",       /* db    */
    "aoaStart.", /* table */
    "",          /* key   */
    OUTPUT iAuditID
    ).
RUN spCreateAuditDtl (
    iAuditID,        /* audit id    */
    "AdvantzwareOA", /* field       */
    0,               /* extent      */
    STRING(TODAY,"99.99.9999") + " @ " + STRING(TIME,"hh:mm:ss"), /* before value */
    "",              /* after value */
    NO               /* index field */
    ).
