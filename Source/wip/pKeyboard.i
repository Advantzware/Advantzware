/* pKeyboard.i - rstark - 4.2.2019 */

PROCEDURE pKeyboard:
    DEFINE INPUT PARAMETER iphFocusField AS HANDLE    NO-UNDO.
    DEFINE INPUT PARAMETER ipcKeyboard   AS CHARACTER NO-UNDO.
    
    CASE ipcKeyboard:
        WHEN "Numeric" THEN DO:
            ipcKeyboard = "touch/numeric.w".
            IF VALID-HANDLE(hKeyboard) AND INDEX(hKeyboard:NAME,"Qwerty") NE 0 THEN
            DELETE OBJECT hKeyboard.
        END.
        WHEN "Qwerty" THEN DO:
            ipcKeyboard = "system/qwerty.w".
            IF VALID-HANDLE(hKeyboard) AND INDEX(hKeyboard:NAME,"Numeric") NE 0 THEN
            DELETE OBJECT hKeyboard.
        END.
    END CASE.
    IF NOT VALID-HANDLE(hKeyboard) THEN
    RUN VALUE(ipcKeyboard) PERSISTENT SET hKeyboard (
        THIS-PROCEDURE:HANDLE,
        FRAME {&FRAME-NAME}:HANDLE,
        YES
        ).
    ASSIGN
        hFocusField = iphFocusField
        cFieldValue = hFocusField:SCREEN-VALUE
        .
    IF VALID-HANDLE(hKeyboard) THEN
    RUN pSetPosition IN hKeyboard (
        {&WINDOW-NAME}:COL + hFocusField:COL - 2,
        {&WINDOW-NAME}:ROW + hFocusField:ROW + 1.67
        ).

END PROCEDURE.

PROCEDURE Key_Stroke:
    DEFINE INPUT PARAMETER ipcKeyStroke AS CHARACTER NO-UNDO.
    
    IF VALID-HANDLE(hFocusField) THEN DO:
        CASE ipcKeyStroke:
            WHEN "BACKSPACE" THEN
            cFieldValue = SUBSTR(cFieldValue,1,LENGTH(cFieldValue) - 1).
            WHEN "CLEAR" THEN
            cFieldValue = "".
            WHEN "SPACE" THEN
            cFieldValue = cFieldValue + "`".
            WHEN "TAB" OR WHEN "ENTER" THEN
            APPLY "LEAVE":U TO hFocusField.
            OTHERWISE
            cFieldValue = cFieldValue + ipcKeyStroke.
        END CASE.
        hFocusField:SCREEN-VALUE = REPLACE(cFieldValue,"`"," ") NO-ERROR.
        IF INDEX(hFocusField:SCREEN-VALUE,"~?") NE 0 THEN DO:
            MESSAGE
                "Value" cFieldValue "is Invalid."
            VIEW-AS ALERT-BOX ERROR.
            ASSIGN
                cFieldValue = SUBSTR(cFieldValue,1,LENGTH(cFieldValue) - 1)
                hFocusField:SCREEN-VALUE = REPLACE(cFieldValue,"`"," ")
                .
        END.
        APPLY "ENTRY":U TO hFocusField.
    END. /* if valid-handle */

END PROCEDURE.
