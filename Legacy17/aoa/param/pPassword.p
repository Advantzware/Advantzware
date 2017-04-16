/* pPassword.p */
/* used in aoaParam.w to return if secure for columns per specific program */

DEFINE INPUT  PARAMETER ipcProgramID       AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcSelectedColumns AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oplSecure          AS LOGICAL   NO-UNDO.

oplSecure = YES.
/* add additional programs alphabetically */
CASE ipcProgramID:
    WHEN "r-fgohbb." THEN
        IF CAN-DO(ipcSelectedColumns,"labCost") OR
           CAN-DO(ipcSelectedColumns,"matCost") OR
           CAN-DO(ipcSelectedColumns,"totCost") OR
           CAN-DO(ipcSelectedColumns,"uomCost") THEN
        RUN sys/ref/d-passwd.w (3, OUTPUT oplSecure).
END CASE.
