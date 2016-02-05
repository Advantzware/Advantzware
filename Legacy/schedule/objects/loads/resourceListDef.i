/* resourceListDef.i - used in load programs and getResource in board.w */

DEFINE VARIABLE resourceUse AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvResource AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvPriority AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE resourceList NO-UNDO
  FIELD resource AS CHARACTER
  INDEX resourceList IS PRIMARY UNIQUE resource.

DEFINE TEMP-TABLE priorityList NO-UNDO
  FIELD resource AS CHARACTER
  FIELD priority AS INTEGER
  INDEX priorityList IS PRIMARY UNIQUE resource.
