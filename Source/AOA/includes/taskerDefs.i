/* taskerDefs.i - rstark - 2.9.2021 */

DEFINE VARIABLE cRun            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTasker         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTaskLog        AS CHARACTER NO-UNDO.
DEFINE VARIABLE dTaskTimeLimit  AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dttDateTime     AS DATETIME  NO-UNDO.
DEFINE VARIABLE dttOpenDateTime AS DATETIME  NO-UNDO.
DEFINE VARIABLE iEmailConfigID  AS INTEGER   NO-UNDO.
DEFINE VARIABLE lJasperStarter  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lUpdated        AS LOGICAL   NO-UNDO INITIAL YES.
