/* ChangeWindowSize.i */

PROCEDURE ChangeWindowSize :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiScreenHeight AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiScreenWidth  AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiScreenTop    AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiScreenLeft   AS INTEGER NO-UNDO.

    ASSIGN
        iScreenHeight = ipiScreenHeight
        iScreenWidth  = ipiScreenWidth
        iScreenTop    = IF ipiScreenTop NE ? THEN ipiScreenTop ELSE iScreenTop
        iScreenLeft   = IF ipiScreenLeft NE ? THEN ipiScreenLeft ELSE iScreenTop
        .

    RUN pWinReSize NO-ERROR.
END PROCEDURE.
