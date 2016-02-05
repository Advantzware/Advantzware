/* boardProc.i */

DEFINE VARIABLE jobHandle AS HANDLE NO-UNDO.

{{&includes}/Basic/boardProc.i}

PROCEDURE createLightBulb:
  {{&includes}/{&Board}/createLightBulb.i}
END PROCEDURE.

PROCEDURE createNote:
  {{&includes}/{&Board}/createNote.i}
END PROCEDURE.

PROCEDURE detailJob:
  {{&includes}/{&Board}/detailJob.i}
END PROCEDURE.

PROCEDURE detailResource:
  DEFINE INPUT PARAMETER ipResource AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipDescription AS CHARACTER NO-UNDO.

  {{&includes}/Pro/btnJobBrowse.i ipResource}
END PROCEDURE.

PROCEDURE downtimeSpan :
  {{&includes}/Pro/downtimeSpan.i}
END PROCEDURE.

PROCEDURE getJobNotes:
  {{&includes}/{&Board}/getJobNotes.i}
END PROCEDURE.

PROCEDURE hideNote:
  DEFINE INPUT PARAMETER ipIdx AS INTEGER NO-UNDO.

  {{&includes}/ttblWidgetHide.i "noteWidget" ipIdx} /* noteIcon */
END PROCEDURE.

PROCEDURE jobDeselection:
  DEFINE INPUT PARAMETER ipWidget AS WIDGET-HANDLE NO-UNDO.
  {{&includes}/{&Board}/jobDeselection.i}
END PROCEDURE.

PROCEDURE jobDowntimeSpan :
  {{&includes}/Pro/jobDowntimeSpan.i}
END PROCEDURE.

PROCEDURE jobNote:
  {{&includes}/{&Board}/jobNote.i}
END PROCEDURE.

PROCEDURE jobSelection:
  DEFINE INPUT PARAMETER ipWidget AS WIDGET-HANDLE NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  RUN gridLine (ipWidget).
  {{&includes}/{&Board}/jobSelection.i}
  RUN detailJob (ROWID(ttblJob),ttblJob.rowIDs).
END PROCEDURE.

PROCEDURE newEnd :
  {{&includes}/Pro/newEnd.i}
END PROCEDURE.

PROCEDURE positionBoard:
  {{&includes}/{&Board}/positionBoard.i}
END PROCEDURE.

PROCEDURE showFlashLight:
  {{&includes}/{&Board}/showFlashLight.i}
END PROCEDURE.

PROCEDURE showLightBulb:
  {{&includes}/{&Board}/showLightBulb.i}
END PROCEDURE.
