&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEF INPUT PARAMETER ipcUserID AS CHAR NO-UNDO.

/* Local Variable Definitions ---                                       */
DEF VAR iCtr AS INT NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS eInstructions fiTxnDtTm eStackTrace bPrev ~
bNext bExit 
&Scoped-Define DISPLAYED-OBJECTS eInstructions fiTxnDtTm eStackTrace 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON bExit 
     IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U
     LABEL "" 
     SIZE 8 BY 1.67 TOOLTIP "Exit"
     BGCOLOR 8 .

DEFINE BUTTON bNext 
     IMAGE-UP FILE "Graphics/32x32/import.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/import_disabled.ico":U
     LABEL "" 
     SIZE 8 BY 1.67 TOOLTIP "Next".

DEFINE BUTTON bPrev 
     IMAGE-UP FILE "Graphics/32x32/export.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/export_disabled.ico":U
     LABEL "" 
     SIZE 8 BY 1.67 TOOLTIP "Previous".

DEFINE VARIABLE eInstructions AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 86 BY 4 NO-UNDO.

DEFINE VARIABLE eStackTrace AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 75 BY 16.19 NO-UNDO.

DEFINE VARIABLE fiTxnDtTm AS DATETIME FORMAT "99/99/99 HH:MM:SS":U 
     LABEL "Date/Time of Transaction" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 38 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     eInstructions AT ROW 1.48 COL 4 NO-LABEL
     fiTxnDtTm AT ROW 5.76 COL 28 COLON-ALIGNED NO-TAB-STOP 
     eStackTrace AT ROW 6.95 COL 4 NO-LABEL
     bPrev AT ROW 11.95 COL 82
     bNext AT ROW 14.33 COL 82
     bExit AT ROW 21.48 COL 82
     SPACE(3.59) SKIP(0.51)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Stack Trace for User".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       eInstructions:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

ASSIGN 
       fiTxnDtTm:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Stack Trace for User */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bExit Dialog-Frame
ON CHOOSE OF bExit IN FRAME Dialog-Frame
OR CHOOSE OF bNext 
OR CHOOSE OF bPrev
DO:
    CASE SELF:NAME:
        WHEN "bExit" THEN DO: 
            APPLY 'go' TO FRAME dialog-frame.
        END.
        WHEN "bPrev" THEN DO:
            /* This is counter-intuitive, but index is ASCENDING and we want the next MOST recent */
            FIND NEXT audithdr NO-LOCK 
            USE-INDEX auditdatetime WHERE 
                audituser = ipcUserID AND 
                audithdr.auditstackid NE 0 
                NO-ERROR. 
            IF NOT AVAIL audithdr THEN DO:
                ASSIGN 
                    SELF:SENSITIVE = FALSE.
                RETURN NO-APPLY.
            END.
            ELSE DO:
                ASSIGN
                    SELF:SENSITIVE = TRUE 
                    bPrev:SENSITIVE = TRUE  
                    fiTxnDtTm:SCREEN-VALUE = STRING(auditHdr.auditdatetime)
                    eStackTrace:SCREEN-VALUE = "".
                FIND auditstack NO-LOCK WHERE  
                    auditStack.auditstackid EQ  audithdr.auditstackid
                    NO-ERROR.
                IF NOT AVAIL auditstack THEN RETURN.
                ELSE DO ictr = 1 TO NUM-ENTRIES(auditstack.auditstack):    
                    ASSIGN  
                        eStackTrace:SCREEN-VALUE = eStackTrace:SCREEN-VALUE + ENTRY(iCtr,auditstack.auditstack) + CHR(10).
                END.
            END.
            RETURN NO-APPLY.
        END.
        WHEN "bNext" THEN DO:
            /* This is counter-intuitive, but index is ASCENDING and we want the next MOST recent */
            FIND PREV audithdr NO-LOCK 
                USE-INDEX auditdatetime WHERE 
                audituser = ipcUserID AND 
                audithdr.auditstackid NE 0 
                NO-ERROR. 
            IF NOT AVAIL audithdr THEN DO:
                ASSIGN 
                    SELF:SENSITIVE = FALSE.
                RETURN NO-APPLY.
            END.
            ELSE DO:
                ASSIGN
                    SELF:SENSITIVE = TRUE 
                    bPrev:SENSITIVE = TRUE  
                    fiTxnDtTm:SCREEN-VALUE = STRING(auditHdr.auditdatetime)
                    eStackTrace:SCREEN-VALUE = "".
                FIND auditstack NO-LOCK WHERE  
                    auditStack.auditstackid EQ  audithdr.auditstackid
                    NO-ERROR.
                IF NOT AVAIL auditstack THEN RETURN.
                ELSE DO ictr = 1 TO NUM-ENTRIES(auditstack.auditstack):    
                    ASSIGN 
                        eStackTrace:SCREEN-VALUE = eStackTrace:SCREEN-VALUE + ENTRY(iCtr,auditstack.auditstack) + CHR(10).
                END.
            END.
            RETURN NO-APPLY.
        END.
    END CASE.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  
    FIND LAST audithdr NO-LOCK 
    USE-INDEX auditdatetime WHERE 
        audituser = ipcUserID AND 
        audithdr.auditstackid NE 0 
        NO-ERROR. 
    IF NOT AVAIL audithdr THEN RETURN.
    ELSE DO:
        FIND auditstack NO-LOCK WHERE  
            auditStack.auditstackid EQ  audithdr.auditstackid
            NO-ERROR.
        IF NOT AVAIL auditstack THEN RETURN.
        ELSE DO ictr = 1 TO NUM-ENTRIES(auditstack.auditstack):    
            ASSIGN 
                eStackTrace:SCREEN-VALUE = eStackTrace:SCREEN-VALUE + ENTRY(iCtr,auditstack.auditstack) + CHR(10).
        END.
    END.
  
    ASSIGN 
        bPrev:SENSITIVE = FALSE 
        FRAME dialog-frame:TITLE = "Stack Trace for User " + ipcUserID
        bPrev:SENSITIVE = FALSE 
        eInstructions:SCREEN-VALUE = 
            "The list below displays the programs that were in use the last time user " + CAPS(ipcUserID) + " initiated a system transaction. " +
            "System transactions include ADD, CHANGE, and DELETE actions.  If your site allows multiple users to use the same ASI " +
            "login, or if a user has a LOCK on an uncommitted transaction, this list might be deceptive." + CHR(10) +
            "You can use the UP/DOWN arrows to search for other transactions for this user.".
    
    FIND LAST audithdr NO-LOCK 
        USE-INDEX auditdatetime WHERE 
        audituser = ipcUserID AND 
        audithdr.auditstackid NE 0 
        NO-ERROR. 
    IF NOT AVAIL audithdr THEN RETURN.
    ELSE 
    DO:
        ASSIGN 
            fiTxnDtTm:SCREEN-VALUE = STRING(audithdr.auditDateTime).
        FIND auditstack NO-LOCK WHERE  
            auditStack.auditstackid EQ  audithdr.auditstackid
            NO-ERROR.
        IF NOT AVAIL auditstack THEN RETURN.
        ELSE 
        DO ictr = 1 TO NUM-ENTRIES(auditstack.auditstack):    
            ASSIGN 
                eStackTrace:SCREEN-VALUE = eStackTrace:SCREEN-VALUE + ENTRY(iCtr,auditstack.auditstack) + CHR(10).
        END.
    END.

  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY eInstructions fiTxnDtTm eStackTrace 
      WITH FRAME Dialog-Frame.
  ENABLE eInstructions fiTxnDtTm eStackTrace bPrev bNext bExit 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

