&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: packOptions.w

  Description: packing options date prompt

  Input Parameters: <none>

  Output Parameters: <none>

  Author: Ron Stark

  Created: 11.7.2005
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{schedule/scopDir.i}

/* Parameters Definitions ---                                           */

DEFINE INPUT PARAMETER ipTitle AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopPackOption AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER ipPackOptionPrompt AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER ipFirstDate AS DATE NO-UNDO.
DEFINE INPUT PARAMETER ipFirstTime AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER ipJobDate AS DATE NO-UNDO.
DEFINE INPUT PARAMETER ipJobTime AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER ipStartDate AS DATE NO-UNDO.
DEFINE INPUT PARAMETER ipStartTime AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER ipEndDate AS DATE NO-UNDO.
DEFINE INPUT PARAMETER ipEndTime AS INTEGER NO-UNDO.

DEFINE OUTPUT PARAMETER opStartDate AS DATE NO-UNDO.
DEFINE OUTPUT PARAMETER opStartTime AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER opEndDate AS DATE NO-UNDO.
DEFINE OUTPUT PARAMETER opEndTime AS INTEGER NO-UNDO.

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnPackOption-1 btnPackOption-2 ~
btnPackOption-3 btnPackOption-4 btnPackOption-5 btnPackOption-6 ~
btnPackOption-7 btnPackOption-8 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD btnPackOptionLabel Dialog-Frame 
FUNCTION btnPackOptionLabel RETURNS LOGICAL
  (ipButton AS HANDLE,ipSDate AS DATE,ipSTime AS INTEGER,
   ipEDate AS DATE,ipETime AS INTEGER,ipPackOption AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnPackOption-1 AUTO-END-KEY 
     LABEL "First Job to Visible End" 
     SIZE 56 BY 1.67.

DEFINE BUTTON btnPackOption-2 AUTO-END-KEY 
     LABEL "Current Date/Time to Visible End" 
     SIZE 56 BY 1.67.

DEFINE BUTTON btnPackOption-3 AUTO-END-KEY 
     LABEL "Visible Begin to Visible End" 
     SIZE 56 BY 1.67.

DEFINE BUTTON btnPackOption-4 AUTO-END-KEY 
     LABEL "Visible First Job to Visible End" 
     SIZE 56 BY 1.67.

DEFINE BUTTON btnPackOption-5 AUTO-END-KEY 
     LABEL "First Job to Board End" 
     SIZE 56 BY 1.67.

DEFINE BUTTON btnPackOption-6 AUTO-END-KEY 
     LABEL "Current Date/Time to Board End" 
     SIZE 56 BY 1.67.

DEFINE BUTTON btnPackOption-7 AUTO-END-KEY 
     LABEL "Visible Begin to Board End" 
     SIZE 56 BY 1.67.

DEFINE BUTTON btnPackOption-8 AUTO-END-KEY 
     LABEL "Visible First Job to Board End" 
     SIZE 56 BY 1.67.

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 1 GRAPHIC-EDGE  
     SIZE 58 BY 2.86
     BGCOLOR 1 .

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 1 GRAPHIC-EDGE  
     SIZE 58 BY 2.86
     BGCOLOR 2 .

DEFINE RECTANGLE RECT-14
     EDGE-PIXELS 1 GRAPHIC-EDGE  
     SIZE 58 BY 2.86
     BGCOLOR 3 .

DEFINE RECTANGLE RECT-15
     EDGE-PIXELS 1 GRAPHIC-EDGE  
     SIZE 58 BY 2.86
     BGCOLOR 4 .

DEFINE RECTANGLE RECT-16
     EDGE-PIXELS 1 GRAPHIC-EDGE  
     SIZE 58 BY 2.86
     BGCOLOR 5 .

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 1 GRAPHIC-EDGE  
     SIZE 58 BY 2.86
     BGCOLOR 6 .

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 1 GRAPHIC-EDGE  
     SIZE 58 BY 2.86
     BGCOLOR 12 FGCOLOR 15 .

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 1 GRAPHIC-EDGE  
     SIZE 58 BY 2.86
     BGCOLOR 13 FGCOLOR 15 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     btnPackOption-1 AT ROW 2.19 COL 3
     btnPackOption-2 AT ROW 5.29 COL 3
     btnPackOption-3 AT ROW 8.38 COL 3
     btnPackOption-4 AT ROW 11.48 COL 3
     btnPackOption-5 AT ROW 2.19 COL 62
     btnPackOption-6 AT ROW 5.29 COL 62
     btnPackOption-7 AT ROW 8.38 COL 62
     btnPackOption-8 AT ROW 11.48 COL 62
     "2. Current Date/Time to Visible End" VIEW-AS TEXT
          SIZE 56 BY .62 AT ROW 4.57 COL 3
          BGCOLOR 2 FGCOLOR 15 FONT 6
     "6. Current Date/Time to Board End" VIEW-AS TEXT
          SIZE 56 BY .62 AT ROW 4.57 COL 62
          BGCOLOR 5 FGCOLOR 15 FONT 6
     "1. First Job to Visible End" VIEW-AS TEXT
          SIZE 56 BY .62 AT ROW 1.48 COL 3
          BGCOLOR 1 FGCOLOR 15 FONT 6
     "4. Visible First Job to Visible End" VIEW-AS TEXT
          SIZE 56 BY .62 AT ROW 10.76 COL 3
          BGCOLOR 12 FGCOLOR 15 FONT 6
     "3. Visible Begin to Visible End" VIEW-AS TEXT
          SIZE 56 BY .62 AT ROW 7.67 COL 3
          BGCOLOR 3 FGCOLOR 15 FONT 6
     "7. Visible Begin to Board End" VIEW-AS TEXT
          SIZE 56 BY .62 AT ROW 7.67 COL 62
          BGCOLOR 6 FGCOLOR 15 FONT 6
     "8. Visible First Job to Board End" VIEW-AS TEXT
          SIZE 56 BY .62 AT ROW 10.76 COL 62
          BGCOLOR 13 FGCOLOR 15 FONT 6
     "5. First Job to Board End" VIEW-AS TEXT
          SIZE 56 BY .62 AT ROW 1.48 COL 62
          BGCOLOR 4 FGCOLOR 15 FONT 6
     RECT-12 AT ROW 1.24 COL 2
     RECT-13 AT ROW 4.33 COL 2
     RECT-14 AT ROW 7.43 COL 2
     RECT-15 AT ROW 1.24 COL 61
     RECT-16 AT ROW 4.33 COL 61
     RECT-17 AT ROW 7.43 COL 61
     RECT-18 AT ROW 10.52 COL 2
     RECT-19 AT ROW 10.52 COL 61
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE NO-VALIDATE THREE-D NO-AUTO-VALIDATE  SCROLLABLE 
         BGCOLOR 14 
         TITLE "Options for Packing".


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
   L-To-R,COLUMNS                                                       */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR RECTANGLE RECT-12 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-13 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-14 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-15 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-16 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-17 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-18 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-19 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Options for Packing */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPackOption-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPackOption-1 Dialog-Frame
ON CHOOSE OF btnPackOption-1 IN FRAME Dialog-Frame /* First Job to Visible End */
,btnPackOption-2,btnPackOption-3,btnPackOption-4,btnPackOption-5
,btnPackOption-6,btnPackOption-7,btnPackOption-8
DO:
  ASSIGN
    opStartDate = DATE(ENTRY(1,SELF:PRIVATE-DATA))
    opStartTime = INTEGER(ENTRY(2,SELF:PRIVATE-DATA))
    opEndDate = DATE(ENTRY(3,SELF:PRIVATE-DATA))
    opEndTime = INTEGER(ENTRY(4,SELF:PRIVATE-DATA))
    iopPackOption = INTEGER(ENTRY(5,SELF:PRIVATE-DATA)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

FRAME {&FRAME-NAME}:TITLE = FRAME {&FRAME-NAME}:TITLE + ' ' + ipTitle.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  btnPackOptionLabel(btnPackOption-1:HANDLE,ipFirstDate,ipFirstTime,ipEndDate,ipEndTime,1).
  btnPackOptionLabel(btnPackOption-2:HANDLE,TODAY,TIME,ipEndDate,ipEndTime,2).
  btnPackOptionLabel(btnPackOption-3:HANDLE,ipStartDate,ipStartTime,ipEndDate,ipEndTime,3).
  btnPackOptionLabel(btnPackOption-4:HANDLE,ipJobDate,ipJobTime,ipEndDate,ipEndTime,3).
  btnPackOptionLabel(btnPackOption-5:HANDLE,ipFirstDate,ipFirstTime,{{&includes}/lastDate.i},86400,5).
  btnPackOptionLabel(btnPackOption-6:HANDLE,TODAY,TIME,{{&includes}/lastDate.i},86400,6).
  btnPackOptionLabel(btnPackOption-7:HANDLE,ipStartDate,ipStartTime,{{&includes}/lastDate.i},86400,7).
  btnPackOptionLabel(btnPackOption-8:HANDLE,ipJobDate,ipJobTime,{{&includes}/lastDate.i},86400,8).
  IF ipPackOptionPrompt THEN
  CASE iopPackOption:
    WHEN 1 THEN
    APPLY 'ENTRY':U TO btnPackOption-1.
    WHEN 2 THEN
    APPLY 'ENTRY':U TO btnPackOption-2.
    WHEN 3 THEN
    APPLY 'ENTRY':U TO btnPackOption-3.
    WHEN 4 THEN
    APPLY 'ENTRY':U TO btnPackOption-4.
    WHEN 5 THEN
    APPLY 'ENTRY':U TO btnPackOption-5.
    WHEN 6 THEN
    APPLY 'ENTRY':U TO btnPackOption-6.
    WHEN 7 THEN
    APPLY 'ENTRY':U TO btnPackOption-7.
    WHEN 8 THEN
    APPLY 'ENTRY':U TO btnPackOption-8.
  END CASE.
  ELSE /* no prompt */
  CASE iopPackOption:
    WHEN 1 THEN
    APPLY 'CHOOSE':U TO btnPackOption-1.
    WHEN 2 THEN
    APPLY 'CHOOSE':U TO btnPackOption-2.
    WHEN 3 THEN
    APPLY 'CHOOSE':U TO btnPackOption-3.
    WHEN 4 THEN
    APPLY 'CHOOSE':U TO btnPackOption-4.
    WHEN 5 THEN
    APPLY 'CHOOSE':U TO btnPackOption-5.
    WHEN 6 THEN
    APPLY 'CHOOSE':U TO btnPackOption-6.
    WHEN 7 THEN
    APPLY 'CHOOSE':U TO btnPackOption-7.
    WHEN 8 THEN
    APPLY 'CHOOSE':U TO btnPackOption-8.
  END CASE.
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
  ENABLE btnPackOption-1 btnPackOption-2 btnPackOption-3 btnPackOption-4 
         btnPackOption-5 btnPackOption-6 btnPackOption-7 btnPackOption-8 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION btnPackOptionLabel Dialog-Frame 
FUNCTION btnPackOptionLabel RETURNS LOGICAL
  (ipButton AS HANDLE,ipSDate AS DATE,ipSTime AS INTEGER,
   ipEDate AS DATE,ipETime AS INTEGER,ipPackOption AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  ASSIGN
    ipButton:PRIVATE-DATA = STRING(ipSDate) + ',' +
                            STRING(ipSTime) + ',' +
                            STRING(ipEDate) + ',' +
                            STRING(ipETime) + ',' +
                            STRING(ipPackOption)
    ipButton:LABEL = STRING(ipSDate,'99.99.9999') + ' @ ' +
                     STRING(ipSTime,'HH:MM:SS am') + ' - ' +
                     STRING(ipEDate,'99.99.9999') + ' @ ' + 
                     STRING(ipETime,'HH:MM:SS am').

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

