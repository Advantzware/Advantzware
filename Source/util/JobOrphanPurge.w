&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
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
{custom/globdefs.i}
{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = g_company.

DEFINE TEMP-TABLE ttJobNo LIKE job.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS eInstructions fiEndDate fiThroughJob ~
rsCompany bReview bPurge bExit slJobList 
&Scoped-Define DISPLAYED-OBJECTS eInstructions fiEndDate fiThroughJob ~
rsCompany fiToBePurged fiColumnHeader slJobList 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bExit AUTO-END-KEY 
     LABEL "Exit" 
     SIZE 15 BY 1.14.

DEFINE BUTTON bPurge 
     LABEL "Purge" 
     SIZE 18 BY 1.14.

DEFINE BUTTON bReview 
     LABEL "Review and Display" 
     SIZE 25 BY 1.14.

DEFINE VARIABLE eInstructions AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 72 BY 4.05 NO-UNDO.

DEFINE VARIABLE fiColumnHeader AS CHARACTER FORMAT "X(256)":U INITIAL "Company   Job       Create Date" 
     VIEW-AS FILL-IN 
     SIZE 71 BY .81
     FONT 3 NO-UNDO.

DEFINE VARIABLE fiEndDate AS DATE FORMAT "99/99/9999":U 
     LABEL "Purge Jobs Through Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fiThroughJob AS CHARACTER FORMAT "X(256)":U 
     LABEL "Through Job Number (optional)" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fiToBePurged AS CHARACTER FORMAT "X(256)":U INITIAL "The following job records will be purged:" 
     VIEW-AS FILL-IN 
     SIZE 71 BY .81 NO-UNDO.

DEFINE VARIABLE rsCompany AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "This Company", "This",
"All Companies", "All"
     SIZE 34 BY .95 NO-UNDO.

DEFINE VARIABLE slJobList AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 72 BY 13.1
     FONT 3 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     eInstructions AT ROW 1.24 COL 4 NO-LABEL WIDGET-ID 16
     fiEndDate AT ROW 5.76 COL 33 COLON-ALIGNED WIDGET-ID 2
     fiThroughJob AT ROW 6.95 COL 33 COLON-ALIGNED WIDGET-ID 8
     rsCompany AT ROW 8.14 COL 35 NO-LABEL WIDGET-ID 4
     bReview AT ROW 9.33 COL 5 WIDGET-ID 10
     bPurge AT ROW 9.33 COL 36 WIDGET-ID 12
     bExit AT ROW 9.33 COL 61 WIDGET-ID 14
     fiToBePurged AT ROW 10.76 COL 3 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     fiColumnHeader AT ROW 11.48 COL 3 COLON-ALIGNED NO-LABEL WIDGET-ID 24
     slJobList AT ROW 12.43 COL 5 NO-LABEL WIDGET-ID 22
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 78.2 BY 25.43
         CANCEL-BUTTON bExit WIDGET-ID 100.


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
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Purge Orphan Job Numbers"
         HEIGHT             = 25.43
         WIDTH              = 78.2
         MAX-HEIGHT         = 26.57
         MAX-WIDTH          = 137
         VIRTUAL-HEIGHT     = 26.57
         VIRTUAL-WIDTH      = 137
         RESIZE             = YES
         SCROLL-BARS        = NO
         STATUS-AREA        = NO
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = YES
         THREE-D            = YES
         MESSAGE-AREA       = NO
         SENSITIVE          = YES.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       eInstructions:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fiColumnHeader IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiToBePurged IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Purge Orphan Job Numbers */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Purge Orphan Job Numbers */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bReview
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bReview C-Win
ON CHOOSE OF bReview IN FRAME DEFAULT-FRAME /* Review and Display */
OR CHOOSE OF bPurge
DO:
    CASE SELF:NAME:
        WHEN "bReview" THEN DO:
            ASSIGN 
                slJobList:LIST-ITEMS = "".
            FOR EACH job NO-LOCK WHERE
                job.company EQ (IF rsCompany:SCREEN-VALUE = "All" THEN job.company ELSE cocode) AND 
                job.create-date LT DATE(fiEndDate:SCREEN-VALUE) AND 
                job.job-no LT (IF fiThroughJob:SCREEN-VALUE EQ "" THEN "zzzzzzzzz" ELSE fiThroughJob:SCREEN-VALUE) AND  
                NOT CAN-FIND(FIRST job-hdr WHERE 
                    job-hdr.company EQ job.company AND 
                    job-hdr.job-no EQ job.job-no AND 
                    job-hdr.job-no2 EQ job.job-no2)
                BY job.company BY job.job-no BY job.job-no2:
                slJobList:ADD-LAST(
                        STRING(job.company,"x(4)") + "   " +
                        STRING(DYNAMIC-FUNCTION('sfFormat_JobFormatWithHyphen', job.job-no, job.job-no2)) + "   " +
                        STRING(job.create-date,"99/99/99")
                        ).
            END. 
        END.
        WHEN "bPurge" THEN DO:
            FOR EACH job EXCLUSIVE WHERE
                job.company EQ (IF rsCompany:SCREEN-VALUE = "All" THEN job.company ELSE cocode) AND 
                job.create-date LT DATE(fiEndDate:SCREEN-VALUE) AND 
                job.job-no LT (IF fiThroughJob:SCREEN-VALUE EQ "" THEN "zzzzzzzzz" ELSE fiThroughJob:SCREEN-VALUE) AND  
                NOT CAN-FIND(FIRST job-hdr WHERE 
                    job-hdr.company EQ job.company AND 
                    job-hdr.job-no EQ job.job-no AND 
                    job-hdr.job-no2 EQ job.job-no2)
                BY job.company BY job.job-no BY job.job-no2:
                DELETE job.
            END. 
        END.
    END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


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
  
  ASSIGN 
    eInstructions:SCREEN-VALUE =
        'This function will scan your database for any "job" records where there are no associated job details, ' +
        'and will (optionally) allow you to purge these records.'
    fiEndDate:SCREEN-VALUE = STRING(TODAY - 60)
    .
  
    
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY eInstructions fiEndDate fiThroughJob rsCompany fiToBePurged 
          fiColumnHeader slJobList 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE eInstructions fiEndDate fiThroughJob rsCompany bReview bPurge bExit 
         slJobList 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

