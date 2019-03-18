/* fSuperRunning.i - rstark - 9.26.2018 */

DEFINE VARIABLE cCuePrgmName AS CHARACTER NO-UNDO.
DEFINE VARIABLE hCueWindow   AS HANDLE    NO-UNDO.
DEFINE VARIABLE hCueFrame    AS HANDLE    NO-UNDO.
DEFINE VARIABLE lCueActive   AS LOGICAL   NO-UNDO INITIAL YES.
DEFINE VARIABLE lRunCueCard  AS LOGICAL   NO-UNDO.

cCuePrgmName = ENTRY(1,THIS-PROCEDURE:NAME,".").

FUNCTION fSuperRunning RETURN LOGICAL (ipcSuperProcedure AS CHARACTER):
    DEFINE VARIABLE hSession AS HANDLE  NO-UNDO.
    DEFINE VARIABLE lSession AS LOGICAL NO-UNDO.
    DEFINE VARIABLE idx      AS INTEGER NO-UNDO.
    
    /* find if super is running */
    DO idx = 1 TO NUM-ENTRIES(SESSION:SUPER-PROCEDURES):
        hSession = HANDLE(ENTRY(idx,SESSION:SUPER-PROCEDURES)).
        IF INDEX(hSession:NAME,ipcSuperProcedure) EQ 0 THEN NEXT.
        /* found session.p */
        lSession = YES.
        LEAVE.
    END. /* do idx */
    RETURN lSession.
END FUNCTION.
