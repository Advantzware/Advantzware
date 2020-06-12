/* fShowRestrictMessage.i - rstark - 6.11.2020 */

FUNCTION fShowRestrictionMessage RETURNS LOGICAL (ipcCompany AS CHARACTER):
    DEFINE VARIABLE cShowRestrictionMessage AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lShowRestrictionMessage AS LOGICAL   NO-UNDO.

    RUN sys/ref/nk1look.p (
        ipcCompany,
        "ShowRestrictionMessage",
        "L",
        NO,
        NO,
        "",
        "",
        OUTPUT cShowRestrictionMessage,
        OUTPUT lShowRestrictionMessage
        ).
    RETURN lShowRestrictionMessage AND cShowRestrictionMessage EQ "YES".
END FUNCTION.
