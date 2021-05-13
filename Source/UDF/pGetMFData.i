/* pGetMFData.i */

PROCEDURE pGetMFData:
    DEFINE INPUT PARAMETER ipcMFGroup AS CHARACTER NO-UNDO.

    OS-CREATE-DIR "users".
    OS-CREATE-DIR VALUE("users\" + USERID("ASI")).
    OUTPUT TO VALUE("users/" + USERID("ASI") + "/miscflds.dat").
    FOR EACH mfdata NO-LOCK
        WHERE ENTRY(1,mfdata.mfgroup_data,"|") EQ ipcMFGroup
        :
        PUT UNFORMATTED mfdata.miscflds_data SKIP.
    END.
    OUTPUT CLOSE.
    INPUT FROM VALUE("users/" + USERID("ASI") + "/miscflds.dat") NO-ECHO.
    REPEAT:
        CREATE ttAttrb.
        IMPORT ttAttrb.
    END.
    INPUT CLOSE.
    IF ttAttrb.attr_type EQ "" THEN DELETE ttAttrb.
END PROCEDURE.
