/* objectName.i - used in procedure objectName in schedule.w */

  DEFINE INPUT PARAMETER ipType AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipName AS CHARACTER NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      btnPackResource:LABEL = ipName
      btnPackResource:PRIVATE-DATA = ipType
      btnPackResource:TOOLTIP = IF ipType EQ 'Job' THEN 'Pack this Job'
                                ELSE 'Pack Jobs for this Resource'.
  END.
