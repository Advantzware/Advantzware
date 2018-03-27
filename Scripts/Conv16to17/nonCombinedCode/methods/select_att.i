/* select_att.i */

    DEFINE VARIABLE hAttach AS HANDLE NO-UNDO.

    IF Consultingwerk.WindowIntegrationKit.WinKitSettings:WinKitActive EQ TRUE THEN DO:
        RUN windows/attach.w PERSISTENT SET hAttach (rec_key_value, header_value).
        RUN dispatch IN hAttach ("initialize") .
    END.
    ELSE
    RUN windows/attach.w (rec_key_value, header_value).
