/* asiDept.i */

DEFINE TEMP-TABLE asiDept NO-UNDO
  FIELD rptName AS CHARACTER
  FIELD asiDept AS CHARACTER
    INDEX asiDept IS PRIMARY UNIQUE rptName.

PROCEDURE asiDeptBuild:
  DEFINE VARIABLE lvRptName AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lvAsiDept AS CHARACTER NO-UNDO.

  EMPTY TEMP-TABLE asiDept.
  INPUT FROM VALUE(findProgram('{&data}/',ID,'/asiDept.dat')) NO-ECHO.
  REPEAT:
    IMPORT lvRptName lvAsiDept.
    CREATE asiDept.
    ASSIGN
      asiDept.rptName = lvRptName
      asiDept.asiDept = lvAsiDept.
  END. /* repeat */
  INPUT CLOSE.
END PROCEDURE.
