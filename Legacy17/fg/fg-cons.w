/* fg/fg-cons.w */

DEFINE NEW SHARED VARIABLE choice    AS LOGICAL NO-UNDO. /* for post fg */
DEFINE            VARIABLE h_fg-cons AS HANDLE  NO-UNDO.

IF Consultingwerk.WindowIntegrationKit.WinKitSettings:WinKitActive EQ TRUE THEN DO:
    RUN addon/fg/fg-cons.w PERSISTENT SET h_fg-cons .
    RUN dispatch IN h_fg-cons ("initialize") .
END.
ELSE
RUN addon/fg/fg-cons.w.
