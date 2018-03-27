/* createNote.i - used in boardProc.i both Pro and View versions */

/*------------------------------------------------------------------------------
  Purpose:     create note image button for a job bar and triggers
  Parameters:  lockicon exists, object number, X, Y, job rowid & job note exists
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipLockButton AS LOGICAL NO-UNDO.
  DEFINE INPUT PARAMETER ipIdx AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipX AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipY AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipRowID AS ROWID NO-UNDO.
  DEFINE INPUT PARAMETER ipNote AS LOGICAL NO-UNDO.

  DEFINE VARIABLE pWidget AS WIDGET-HANDLE NO-UNDO.

  IF ipLockButton THEN ipX = ipX + 14.

  IF ipX GE FRAME {&FRAME-NAME}:WIDTH-PIXELS - 14 THEN RETURN.

  /* noteIcon */
  {{&includes}/ttblWidgetFind.i "noteWidget" ipIdx}
  {{&includes}/ttblWidgetAssign.i "noteWidget" pWidget}
  ELSE
  DO:
    CREATE BUTTON pWidget IN WIDGET-POOL 'notePool'
        ASSIGN
          FRAME = FRAME {&FRAME-NAME}:HANDLE
          SENSITIVE = YES
          HEIGHT-PIXELS = 14
    TRIGGERS:
      ON CHOOSE
         PERSISTENT RUN jobNote IN THIS-PROCEDURE (pWidget:HANDLE).
    END TRIGGERS.
    {{&includes}/ttblWidgetCreate.i "noteWidget" ipIdx pWidget}
  END.
  ASSIGN
    pWidget:HIDDEN = YES
    pWidget:WIDTH-PIXELS = 14
    pWidget:X = ipX + 1
    pWidget:Y = ipY + 1
    pWidget:PRIVATE-DATA = STRING(ipRowID)
    ldummy = pWidget:LOAD-IMAGE(IF AVAIL(ttblJob) AND ipNote THEN
             '{&images}/noteTack.bmp' ELSE '{&images}/noteTackRed.bmp').
