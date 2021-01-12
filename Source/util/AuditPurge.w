&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME c-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS c-Win 
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
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DISABLE TRIGGERS FOR LOAD OF auditHdr.
DISABLE TRIGGERS FOR LOAD OF auditDtl.
DISABLE TRIGGERS FOR LOAD OF auditStack.

DEF VAR daTargetDate AS DATE NO-UNDO.
DEF VAR lStop AS LOG NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnRun eInstructions fiEndDate btnStop ~
btnExit 
&Scoped-Define DISPLAYED-OBJECTS eInstructions fiEndDate fiCurrDate ~
fiTotHdr fiTotDtl fiTotStk 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR c-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnExit AUTO-END-KEY 
     IMAGE-UP FILE "Graphics/32x32/exit_white.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8.4 BY 2 TOOLTIP "Exit"
     BGCOLOR 8 .

DEFINE BUTTON btnRun 
     IMAGE-UP FILE "Graphics/32x32/media_play.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/media_play_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "&Run" 
     SIZE 8 BY 1.91 TOOLTIP "Start".

DEFINE BUTTON btnStop 
     IMAGE-UP FILE "Graphics/32x32/delete.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/delete_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91 TOOLTIP "Stop".

DEFINE VARIABLE eInstructions AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 99 BY 6.19 NO-UNDO.

DEFINE VARIABLE fiCurrDate AS DATE FORMAT "99/99/9999":U 
     LABEL "Earliest Audit Record on File" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE fiEndDate AS DATE FORMAT "99/99/9999":U 
     LABEL "Delete Audit Records Created Prior To" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE fiTotDtl AS INTEGER FORMAT "->>>,>>>,>>9":U INITIAL 0 
     LABEL "(Dtl)" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fiTotHdr AS INTEGER FORMAT "->>>,>>>,>>9":U INITIAL 0 
     LABEL "Records Processed (Hdrs)" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fiTotStk AS INTEGER FORMAT "->>>,>>>,>>9":U INITIAL 0 
     LABEL "(Stk)" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnRun AT ROW 1.24 COL 111
     eInstructions AT ROW 1.48 COL 5 NO-LABEL NO-TAB-STOP 
     fiEndDate AT ROW 8.62 COL 42 COLON-ALIGNED
     fiCurrDate AT ROW 8.62 COL 100 COLON-ALIGNED
     btnStop AT ROW 1.24 COL 121
     fiTotHdr AT ROW 10.52 COL 42 COLON-ALIGNED
     fiTotDtl AT ROW 10.52 COL 71 COLON-ALIGNED
     fiTotStk AT ROW 10.52 COL 100 COLON-ALIGNED
     btnExit AT ROW 1.24 COL 131
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 141 BY 11.52
         CANCEL-BUTTON btnExit.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW c-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Purge Audit History"
         HEIGHT             = 11.52
         WIDTH              = 141
         MAX-HEIGHT         = 22.48
         MAX-WIDTH          = 141
         VIRTUAL-HEIGHT     = 22.48
         VIRTUAL-WIDTH      = 141
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW c-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       eInstructions:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fiCurrDate IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiTotDtl IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiTotHdr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiTotStk IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(c-Win)
THEN c-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME c-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-Win c-Win
ON END-ERROR OF c-Win /* Purge Audit History */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF 
  THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-Win c-Win
ON WINDOW-CLOSE OF c-Win /* Purge Audit History */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExit c-Win
ON CHOOSE OF btnExit IN FRAME DEFAULT-FRAME
OR mouse-select-down OF btnExit
DO:
    ASSIGN 
        lStop = TRUE.
    APPLY 'window-close' TO c-Win.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRun
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRun c-Win
ON CHOOSE OF btnRun IN FRAME DEFAULT-FRAME /* Run */
DO:
    APPLY 'leave' TO fiEndDate IN FRAME {&frame-name}.
    
    ASSIGN 
        daTargetDate = DATE(fiEndDate:SCREEN-VALUE)
        btnStop:SENSITIVE = TRUE 
        btnRun:SENSITIVE = FALSE
        lStop = FALSE.
        
    FOR EACH auditHdr WHERE 
        AuditHdr.AuditDateTime LT DATETIME(daTargetDate):
        PROCESS EVENTS.    
        ASSIGN 
            fiCurrDate:SCREEN-VALUE = STRING(DATE(AuditHdr.AuditDateTime),"99/99/9999").
        FOR EACH auditDtl OF auditHdr:
            PROCESS EVENTS.    
            DELETE auditDtl.
            ASSIGN 
                fiTotDtl = fiTotDtl + 1.
        END.
        FOR EACH auditStack OF auditHdr:
            PROCESS EVENTS.    
            DELETE auditStack.
            ASSIGN 
                fiTotStk = fiTotStk + 1.
        END.
        PROCESS EVENTS.    
        DELETE auditHdr.
        fiTotHdr = fiTotHdr + 1.
        DISPLAY 
            fiTotHdr
            fiTotDtl
            fiTotStk
            WITH FRAME {&frame-name}.
        PROCESS EVENTS.
        IF lStop THEN DO:
            RELEASE auditHdr.
            RELEASE auditDtl.
            RELEASE auditStack.
            ASSIGN 
                btnStop:SENSITIVE = FALSE 
                btnRun:SENSITIVE = TRUE.
            RETURN NO-APPLY.
        END. 
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStop
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStop c-Win
ON CHOOSE OF btnStop IN FRAME DEFAULT-FRAME
OR MOUSE-select-down OF btnStop 
DO:
    ASSIGN 
        lStop = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiEndDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiEndDate c-Win
ON LEAVE OF fiEndDate IN FRAME DEFAULT-FRAME /* Delete Audit Records Created Prior To */
DO:
    IF DATE(SELF:SCREEN-VALUE) GT (TODAY - 30) THEN DO:
        MESSAGE 
            "Advantzware recommends keeping 30 days of history in the audit database.  Are you sure?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lSure AS LOG.
        IF NOT lSure THEN DO:
            ASSIGN 
                SELF:SCREEN-VALUE = STRING(TODAY - 30,"99/99/9999").
            RETURN.
        END.
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK c-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  
    FIND FIRST auditHdr NO-LOCK NO-ERROR.
    IF NOT AVAIL auditHdr THEN DO:
        MESSAGE 
            "There are no records in your AUDIT database."
            VIEW-AS ALERT-BOX INFO.
        RETURN.
    END.
    
    ASSIGN 
        eInstructions:SCREEN-VALUE IN FRAME {&frame-name} = 
            "This function will purge (delete) all records in the AUDIT database prior to the date specified below." + CHR(10) + CHR(10) +
            "FOR LARGE DATABASES, OR IF A PURGE HAS NOT BEEN RUN RECENTLY, THIS PROCESS MAY TAKE SEVERAL HOURS." + CHR(10) + CHR(10) +
            "If you need to cancel this process while it is running, press the STOP (X) button at the top.  Records deleted at that " +
            "point will NOT be restored."
        fiEndDate:SCREEN-VALUE = STRING(TODAY - 183,"99/99/9999")
        fiCurrDate:SCREEN-VALUE = STRING(DATE(auditHdr.auditDateTime),"99/99/9999").  
        
    RELEASE auditHdr.
    
    APPLY 'entry' TO fiEndDate.
  
  /* IF NOT THIS-PROCEDURE:PERSISTENT THEN */
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI c-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(c-Win)
  THEN DELETE WIDGET c-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI c-Win  _DEFAULT-ENABLE
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
  DISPLAY eInstructions fiEndDate fiCurrDate fiTotHdr fiTotDtl fiTotStk 
      WITH FRAME DEFAULT-FRAME IN WINDOW c-Win.
  ENABLE btnRun eInstructions fiEndDate btnStop btnExit 
      WITH FRAME DEFAULT-FRAME IN WINDOW c-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW c-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

