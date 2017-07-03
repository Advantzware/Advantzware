/* select_attcust.i */

DEFINE VARIABLE hAttach AS HANDLE NO-UNDO. 

IF Consultingwerk.WindowIntegrationKit.WinKitSettings:WinKitActive EQ TRUE THEN DO:
    RUN windows/cstattch.w PERSISTENT SET hAttach ({1}, {2}, {3}).
    RUN dispatch IN hAttach ("initialize") .
END.
ELSE
RUN windows/cstattch.w ({1}, {2}, {3}).
