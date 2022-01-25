DEFINE VARIABLE lChoice  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.

IF system.SharedConfig:Instance:ConsumeValue("IsSettingUpdated") EQ "YES" THEN DO:
    cMessage = "Settings may have changed. ~n"
             + "Do you want to exit the window for changes to take effect?".
    
    RUN sharpShooter/messageDialog.w (
        INPUT  cMessage,
        INPUT  YES,
        INPUT  YES,
        INPUT  NO,
        OUTPUT lChoice
        ).
    
    IF lChoice AND VALID-HANDLE({&WINDOW-NAME}) THEN
        APPLY "WINDOW-CLOSE" TO {&WINDOW-NAME}. 
END.