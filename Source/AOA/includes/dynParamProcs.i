/* dynParamProcs.i - rstark - 10.17.2020 */

PROCEDURE pGetSearchBar :
    DEFINE OUTPUT PARAMETER opcSearchBar AS CHARACTER NO-UNDO.

    opcSearchBar = searchBar.

END PROCEDURE.

PROCEDURE pGetTargetSettings:
    DEFINE OUTPUT PARAMETER ophColumnLabel AS HANDLE  NO-UNDO.
    DEFINE OUTPUT PARAMETER oplAscending   AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER ophMoveColumn  AS LOGICAL NO-UNDO.

    ASSIGN
    ophColumnLabel = hColumnLabel
    oplAscending   = lAscending
    ophMoveColumn  = BROWSE {&BROWSE-NAME}:COLUMN-MOVABLE
    .

END PROCEDURE.

PROCEDURE pMoveColumn :
    DEFINE INPUT PARAMETER iplMoveColumn  AS LOGICAL NO-UNDO.

    BROWSE {&BROWSE-NAME}:COLUMN-MOVABLE = iplMoveColumn.

END PROCEDURE.

PROCEDURE pSearchBar :
    DEFINE INPUT PARAMETER ipcSearchBar AS CHARACTER NO-UNDO.

    searchBar = ipcSearchBar.
    RUN pReopenBrowse.
    APPLY "VALUE-CHANGED":U TO BROWSE {&BROWSE-NAME}.

END PROCEDURE.

PROCEDURE pSort :
    DEFINE INPUT PARAMETER iplAscending AS LOGICAL NO-UNDO.
    
    lAscending = iplAscending.
    RUN pReopenBrowse.

END PROCEDURE.
