 
 /*------------------------------------------------------------------------
    File        : system.Keyboard.cls
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : DEVA$!
    Created     : Tue Sep 14 23:14:03 IST 2021
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS system.Keyboard: 
    
    DEFINE VARIABLE cFieldValue       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hKeyboard         AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hdProcedureHandle AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hdFrameHandle     AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hdWindowHandle    AS HANDLE    NO-UNDO.
        
    DEFINE PUBLIC PROPERTY FocusField AS HANDLE NO-UNDO 
    GET.
    SET. 

    DEFINE PUBLIC PROPERTY DisplayKeyboard AS LOGICAL NO-UNDO 
    GET.
    SET. 

	DEFINE PUBLIC PROPERTY LastKeyClicked AS CHARACTER NO-UNDO 
	GET.
	SET.     

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC VOID SetWindow( iphdWindowHandle AS HANDLE ):
		hdWindowHandle = iphdWindowHandle.
	END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID SetProcedure( iphdProcedureHandle AS HANDLE ):
        hdProcedureHandle = iphdProcedureHandle.
    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID SetFrame( iphdFrameHandle AS HANDLE ):
        hdFrameHandle = iphdFrameHandle.
    END METHOD.

    METHOD PUBLIC VOID OpenKeyboardOverride(iphFocusField AS HANDLE, ipcKeyboard AS CHARACTER): 
        pOpenKeyboard (iphFocusField, ipcKeyboard, TRUE).
    END.
    
    METHOD PUBLIC VOID OpenKeyboard(iphFocusField AS HANDLE, ipcKeyboard AS CHARACTER): 
        pOpenKeyboard (iphFocusField, ipcKeyboard, DisplayKeyboard).
    END.
        
    METHOD PRIVATE VOID pOpenKeyboard(iphFocusField AS HANDLE, ipcKeyboard AS CHARACTER, iplDisplayKeyboard AS LOGICAL): 
        LastKeyClicked = "".
            
        IF NOT iplDisplayKeyboard THEN
            RETURN.
            
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
                hdProcedureHandle,
                hdFrameHandle,
                YES
                ).

        ASSIGN
            FocusField = iphFocusField
            cFieldValue = FocusField:SCREEN-VALUE
            .

        IF VALID-HANDLE(hKeyboard) THEN
            RUN pSetPosition IN hKeyboard (
                hdWindowHandle:COL + FocusField:FRAME:COL + FocusField:COL - 2,
                hdWindowHandle:ROW + FocusField:FRAME:ROW + FocusField:ROW + 1
                ).
    
    END METHOD.
    
    METHOD PUBLIC VOID KeyStroke (ipcKeyStroke AS CHARACTER):
        IF VALID-HANDLE(FocusField) THEN DO:
            LastKeyClicked = ipcKeyStroke.
            
            CASE ipcKeyStroke:
                WHEN "BACKSPACE" THEN
                    cFieldValue = SUBSTR(cFieldValue,1,LENGTH(cFieldValue) - 1).
                WHEN "CLEAR" THEN
                    cFieldValue = "".
                WHEN "SPACE" THEN
                    cFieldValue = cFieldValue + "`".
                WHEN "TAB" THEN
                    APPLY "TAB":U TO FocusField.
                WHEN "ENTER" THEN
                    APPLY "LEAVE":U TO FocusField. 
                OTHERWISE
                    cFieldValue = cFieldValue + ipcKeyStroke.
            END CASE.
            FocusField:SCREEN-VALUE = REPLACE(cFieldValue,"`"," ") NO-ERROR.
            IF INDEX(FocusField:SCREEN-VALUE,"~?") NE 0 THEN DO:
                MESSAGE
                    "Value" cFieldValue "is Invalid."
                VIEW-AS ALERT-BOX ERROR.
                ASSIGN
                    cFieldValue = SUBSTR(cFieldValue,1,LENGTH(cFieldValue) - 1)
                    FocusField:SCREEN-VALUE = REPLACE(cFieldValue,"`"," ")
                    .
            END.
            APPLY "ENTRY":U TO FocusField.
        END. /* if valid-handle */
    
    END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	DESTRUCTOR PUBLIC Keyboard ( ):
        IF VALID-HANDLE(hKeyboard) THEN
            DELETE OBJECT hKeyboard.
	END DESTRUCTOR.
END CLASS.