/* pNavigate.i - rstark - 4.1.2019 */

PROCEDURE pNavigate:
    DEFINE INPUT PARAMETER iphNavPanel AS HANDLE NO-UNDO.
    
    IF AVAILABLE ttBrowseInventory THEN DO:
        CASE iphNavPanel:LABEL:
            WHEN "First" THEN
            APPLY "HOME":U TO BROWSE {&BROWSE-NAME}.
            WHEN "Previous" THEN
            BROWSE {&BROWSE-NAME}:SELECT-PREV-ROW().
            WHEN "Next" THEN
            BROWSE {&BROWSE-NAME}:SELECT-NEXT-ROW().
            WHEN "Last" THEN
            APPLY "END":U TO BROWSE {&BROWSE-NAME}.
        END CASE.
        APPLY "VALUE-CHANGED":U TO BROWSE {&BROWSE-NAME}.
    END. /* if avail */

END PROCEDURE.
