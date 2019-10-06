/* applhelp.p */

{methods/defines/globdefs.i}
{methods/defines/hndldefs.i}

DEFINE VARIABLE m_response AS LOGICAL NO-UNDO.
DEFINE VARIABLE object_name AS CHARACTER NO-UNDO.

IF KEYLABEL(LASTKEY) = "CTRL-F" THEN
  IF FRAME-DB = "" AND FRAME-FILE = "" THEN
  DO:
  
  /*MESSAGE "Widget Object: '" + FRAME-FIELD + "'"
      VIEW-AS ALERT-BOX TITLE "CTRL-F Object View".*/
  END.
  ELSE
  DO:
    object_name = FRAME-DB + "." + FRAME-FILE + "." + FRAME-FIELD +
        (IF FRAME-INDEX NE 0 THEN "[" + STRING(FRAME-INDEX) + "]" ELSE "") + "'".
    /*MESSAGE "Field: '" + object_name
        VIEW-AS ALERT-BOX TITLE "CTRL-F Field View".*/
  END.
ELSE
IF KEYLABEL(LASTKEY) = "CTRL-P" THEN
RUN Get_Procedure IN Persistent-Handle ("popups.",OUTPUT run-proc,yes).
ELSE
IF KEYLABEL(LASTKEY) = "F1" THEN
DO: /* F1 function key */
  DEFINE VARIABLE m_lookup_prgm AS CHARACTER NO-UNDO.
  DEFINE VARIABLE m_frame_db AS CHARACTER NO-UNDO.
  DEFINE VARIABLE m_frame_file AS CHARACTER NO-UNDO.
  DEFINE VARIABLE m_frame_field AS CHARACTER NO-UNDO.
  DEF VAR lv-userid AS CHAR NO-UNDO.

  ASSIGN
    m_frame_db = FRAME-DB
    m_frame_file = FRAME-FILE
    m_frame_field = FRAME-FIELD
    lv-userid = USERID(ldbname(1)).

  FIND lookups 
      WHERE lookups.frame_db = m_frame_db AND
            lookups.frame_file = m_frame_file AND 
            lookups.frame_field = m_frame_field 
          NO-LOCK NO-ERROR.
  IF AVAILABLE lookups THEN
  DO:
    IF lv-userid NE "" AND CAN-DO(g_developer,lv-userid) THEN
    DO:   
      MESSAGE "Update Lookup Browser ~"" + lookups.prgmname + "~" ?" SKIP(1)
          "'Cancel' to Remove Attached Browser from this Field."
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL UPDATE m_response.
      CASE m_response:
        WHEN yes THEN
        RUN Run_Lookup (lookups.prgmname).
        WHEN NO THEN DO: 
            IF SEARCH("lookups/" + lookups.prgmname + "r") NE ? THEN
            RUN VALUE(SEARCH("lookups/" + lookups.prgmname + "r")).
            ELSE IF SEARCH("lookups/" + lookups.prgmname + "p") NE ? THEN  
            RUN VALUE(SEARCH("lookups/" + lookups.prgmname + "p")).
        END.
        OTHERWISE
        DO:
          MESSAGE "Remove Attached Browser from this Field?"
              VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE m_response.
          IF m_response THEN
          DO TRANSACTION:
            FIND CURRENT lookups EXCLUSIVE-LOCK.
            DELETE lookups.
          END. /* transaction */
        END. /* otherwise */
      END CASE.
    END. /* can-do(g_developer) */
    ELSE
    RUN VALUE("lookups/" + lookups.prgmname + "p").
  END. /* avail lookups */
  ELSE
  IF lv-userid NE "" AND CAN-DO(g_developer,lv-userid) THEN
  DO:
    MESSAGE "Create Lookup Browser for :" SKIP(1)
        "Program:" PROGRAM-NAME(2) SKIP
        "Database :" m_frame_db SKIP
        "Table :" m_frame_file SKIP
        "Field :" m_frame_field
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE m_response.
    IF m_response THEN
    DO:
      RUN Get_Procedure IN Persistent-Handle ("lkupdate.",OUTPUT run-proc,no).
      IF run-proc NE "" THEN
      DO:
        RUN VALUE(run-proc) (OUTPUT m_lookup_prgm).
        RUN Run_Lookup (m_lookup_prgm).
      END.
    END. /* IF m_response THEN DO */
  END. /* IF CAN-DO */
  ELSE
  MESSAGE "No Lookup Browser for this Field Exists" VIEW-AS ALERT-BOX INFORMATION.
END.

PROCEDURE Run_Lookup :
/* --------------------------------------------------------------------------------
  Purpose: Run Lookup.w, and capture if Lookup Browser Program was created/saved
  Parameters: Lookup Browser Name, output if saved in lookup.w
   ------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER m_lookup_prgm AS CHARACTER.
                                                                                 
  RUN Get_Procedure IN Persistent-Handle ("lookups.",OUTPUT run-proc,no).
  IF run-proc NE "" THEN
  RUN VALUE(run-proc) (m_lookup_prgm).
  IF NOT AVAILABLE lookups THEN
  DO TRANSACTION:
    CREATE lookups.
    ASSIGN
      lookups.frame_db = m_frame_db
      lookups.frame_file = m_frame_file
      lookups.frame_field = m_frame_field
      lookups.prgmname = m_lookup_prgm.
  END. 
  RUN VALUE("lookups/" + lookups.prgmname + "p").
END PROCEDURE.
