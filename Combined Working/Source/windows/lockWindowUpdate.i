/* lockWindowUpdate.i */

/* suspends windows from displaying graphical changes, speeds screen repaint */
PROCEDURE LockWindowUpdate EXTERNAL 'user32.dll':
  /* pass current-window:hwnd to suspend, set to 0 (zero) to enable again */
  DEFINE INPUT PARAMETER intWindowHwnd AS LONG NO-UNDO.
  DEFINE RETURN PARAMETER intResult AS LONG NO-UNDO.
END PROCEDURE.
