/* applhelp.p */

{methods/defines/globdefs.i}
{methods/defines/hndldefs.i}

DEFINE VARIABLE lResponse   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cObjectName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLookupPrgm AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFrameDB    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFrameFile  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFrameField AS CHARACTER NO-UNDO.
DEFINE VARIABLE cUserID     AS CHARACTER NO-UNDO.

IF KEYLABEL(LASTKEY) EQ "CTRL-F" THEN
  IF FRAME-DB EQ "" AND FRAME-FILE EQ "" THEN
  MESSAGE "Widget Object: '" + FRAME-FIELD + "'"
      VIEW-AS ALERT-BOX TITLE "CTRL-F Object View".
  ELSE DO:
    cObjectName = FRAME-DB + "." + FRAME-FILE + "." + FRAME-FIELD +
        (IF FRAME-INDEX NE 0 THEN "[" + STRING(FRAME-INDEX) + "]" ELSE "") + "'".
    MESSAGE "Field: '" + cObjectName
        VIEW-AS ALERT-BOX TITLE "CTRL-F Field View".
  END. /* else */
ELSE IF KEYLABEL(LASTKEY) EQ "CTRL-P" THEN
RUN Get_Procedure IN Persistent-Handle ("popups.",OUTPUT run-proc,yes).
ELSE IF KEYLABEL(LASTKEY) EQ "F1" THEN DO: /* F1 function key */
  ASSIGN
    cFrameDB    = FRAME-DB
    cFrameFile  = FRAME-FILE
    cFrameField = FRAME-FIELD
    cUserID     = USERID("NOSWEAT")
    .
  FIND FIRST lookups NO-LOCK
       WHERE lookups.frame_db    EQ cFrameDB
         AND lookups.frame_file  EQ cFrameFile
         AND lookups.frame_field EQ cFrameField 
       NO-ERROR.
  IF AVAILABLE lookups THEN DO:
    IF cUserID NE "" AND CAN-DO(g_developer,cUserID) THEN DO:   
      MESSAGE "Update Lookup Browser ~"" + lookups.prgmname + "~" ?" SKIP(1)
          "'Cancel' to Remove Attached Browser from this Field."
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL UPDATE lResponse.
      CASE lResponse:
        WHEN YES THEN
        RUN pRunLookup (lookups.prgmname).
        WHEN NO THEN DO: 
            IF SEARCH("lookups/" + lookups.prgmname + "r") NE ? THEN
            RUN VALUE(SEARCH("lookups/" + lookups.prgmname + "r")).
            ELSE IF SEARCH("lookups/" + lookups.prgmname + "p") NE ? THEN  
            RUN VALUE(SEARCH("lookups/" + lookups.prgmname + "p")).
        END.
        OTHERWISE DO:
          MESSAGE "Remove Attached Browser from this Field?"
              VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lResponse.
          IF lResponse THEN
          DO TRANSACTION:
            FIND CURRENT lookups EXCLUSIVE-LOCK.
            DELETE lookups.
          END. /* transaction */
        END. /* otherwise */
      END CASE.
    END. /* can-do(g_developer) */
    ELSE RUN VALUE("lookups/" + lookups.prgmname + "p").
  END. /* avail lookups */
  ELSE IF cUserID NE "" AND CAN-DO(g_developer,cUserID) THEN DO:
    MESSAGE "Create Lookup Browser for :" SKIP(1)
        "Program:" PROGRAM-NAME(2) SKIP
        "Database :" cFrameDB SKIP
        "Table :" cFrameFile SKIP
        "Field :" cFrameField
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lResponse.
    IF lResponse THEN DO:
      RUN Get_Procedure IN Persistent-Handle ("lkupdate.",OUTPUT run-proc,no).
      IF run-proc NE "" THEN DO:
        RUN VALUE(run-proc) (OUTPUT cLookupPrgm).
        RUN pRunLookup (cLookupPrgm).
      END.
    END. /* IF lResponse THEN DO */
  END. /* IF CAN-DO */
  ELSE
  MESSAGE "No Lookup Browser for this Field Exists" VIEW-AS ALERT-BOX INFORMATION.
END.

PROCEDURE pRunLookup :
/* --------------------------------------------------------------------------------
  Purpose: Run Lookup.w, and capture if Lookup Browser Program was created/saved
  Parameters: Lookup Browser Name, output if saved in lookup.w
   ------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER cLookupPrgm AS CHARACTER.
                                                                                 
  RUN Get_Procedure IN Persistent-Handle ("lookups.",OUTPUT run-proc,no).
  IF run-proc NE "" THEN
  RUN VALUE(run-proc) (cLookupPrgm).
  IF NOT AVAILABLE lookups THEN
  DO TRANSACTION:
    CREATE lookups.
    ASSIGN
      lookups.frame_db    = cFrameDB
      lookups.frame_file  = cFrameFile
      lookups.frame_field = cFrameField
      lookups.prgmname    = cLookupPrgm
      .
  END. 
  RUN VALUE("lookups/" + lookups.prgmname + "p").
END PROCEDURE.
