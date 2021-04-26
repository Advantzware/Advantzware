/* ---------------------- UpdateSurAccount.p ---------------------- */
DEFINE BUFFER bf-surcharge  FOR surcharge.

FOR EACH ar-ctrl NO-LOCK:
    FOR EACH bf-surcharge EXCLUSIVE-LOCK 
        WHERE bf-surcharge.company EQ ar-ctrl.company:
        IF bf-surcharge.account EQ "" THEN
        ASSIGN bf-surcharge.account = ar-ctrl.sales.
        ELSE DO:
            FIND FIRST account NO-LOCK
                WHERE account.company EQ bf-surcharge.company
                AND account.actnum    EQ bf-surcharge.account
                NO-ERROR.
            IF NOT AVAILABLE account THEN 
                ASSIGN bf-surcharge.account = ar-ctrl.sales.
        END.
    END.
END.
