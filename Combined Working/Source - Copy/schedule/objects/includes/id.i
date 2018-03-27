/* id.i - used in all objects contained in scheduler.w */

  DEFINE VARIABLE pHandle AS HANDLE NO-UNDO.
  
  pHandle = SESSION:FIRST-PROCEDURE.
  DO WHILE VALID-HANDLE(pHandle):
    IF pHandle:FILE-NAME EQ '{&prompts}/scheduleID.p' THEN
    DO:
      RUN ID IN pHandle (OUTPUT ID).
      RUN scenario IN pHandle (OUTPUT scenario).
      LEAVE.
    END.
    pHandle = pHandle:NEXT-SIBLING.
  END.
