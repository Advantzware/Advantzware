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

&IF DEFINED(UIB_is_Running) EQ 0 &THEN
DEFINE INPUT-OUTPUT PARAMETER iopiMenuSize AS INTEGER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopiLanguage AS INTEGER NO-UNDO.
DEFINE       OUTPUT PARAMETER iplOK        AS LOGICAL NO-UNDO.
&ELSE
DEFINE VARIABLE iopiMenuSize AS INTEGER NO-UNDO.
DEFINE VARIABLE iopiLanguage AS INTEGER NO-UNDO.
DEFINE VARIABLE iplOK        AS LOGICAL NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE cLanguageList AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFlagList     AS CHARACTER NO-UNDO.

IF iopiMenuSize LT 1 THEN iopiMenuSize = 1.
IF iopiLanguage LT 1 THEN iopiLanguage = 1.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnCancel btnOK svLanguageList svMenuSize 
&Scoped-Define DISPLAYED-OBJECTS svLanguageList svMenuSize 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCancel AUTO-END-KEY 
     IMAGE-UP FILE "Graphics/32x32/navigate_cross.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Cancel" 
     SIZE 9 BY 1.91
     BGCOLOR 8 .

DEFINE BUTTON btnLanguage-1  NO-FOCUS FLAT-BUTTON
     LABEL "Lang 1" 
     SIZE 9 BY 1.67.

DEFINE BUTTON btnLanguage-2  NO-FOCUS FLAT-BUTTON
     LABEL "Lang 2" 
     SIZE 9 BY 1.67.

DEFINE BUTTON btnLanguage-3  NO-FOCUS FLAT-BUTTON
     LABEL "Lang 3" 
     SIZE 9 BY 1.67.

DEFINE BUTTON btnOK AUTO-GO 
     IMAGE-UP FILE "Graphics/32x32/navigate_check.ico":U NO-FOCUS FLAT-BUTTON NO-CONVERT-3D-COLORS
     LABEL "OK" 
     SIZE 9 BY 1.91
     BGCOLOR 8 .

DEFINE IMAGE IMAGE-1
     FILENAME "Graphics/16x16/calendar_clock.png":U TRANSPARENT
     SIZE 3.2 BY .76.

DEFINE IMAGE IMAGE-2
     FILENAME "Graphics/24x24/calendar_clock.png":U TRANSPARENT
     SIZE 4.8 BY 1.14.

DEFINE IMAGE IMAGE-3
     FILENAME "Graphics/32x32/calendar_clock.ico":U TRANSPARENT
     SIZE 6.4 BY 1.52.

DEFINE VARIABLE svLanguageList AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Item 1", 1,
"Item 2", 2,
"Item 3", 3
     SIZE 50 BY 4.76 NO-UNDO.

DEFINE VARIABLE svMenuSize AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Small", 1,
"Medium", 2,
"Large", 3
     SIZE 11 BY 5.95 NO-UNDO.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 49 BY 1.29
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-14
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 49 BY 1.67
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-15
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 49 BY 2.05
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-16
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 65 BY 5.71.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 65 BY 7.62.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 20 BY 2.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     btnCancel AT ROW 15.76 COL 57 WIDGET-ID 2
     btnLanguage-1 AT ROW 1.95 COL 6 WIDGET-ID 24
     btnOK AT ROW 15.76 COL 48 WIDGET-ID 4
     btnLanguage-2 AT ROW 3.62 COL 6 WIDGET-ID 26
     btnLanguage-3 AT ROW 5.29 COL 6 WIDGET-ID 28
     svLanguageList AT ROW 2.19 COL 16 NO-LABEL WIDGET-ID 30
     svMenuSize AT ROW 7.91 COL 5 NO-LABEL WIDGET-ID 34
     "(Screen Size: 653 x 800)" VIEW-AS TEXT
          SIZE 24 BY .62 AT ROW 8.62 COL 41 WIDGET-ID 72
          BGCOLOR 8 
     "(Screen Size: 805 x 800)" VIEW-AS TEXT
          SIZE 24 BY .62 AT ROW 10.52 COL 41 WIDGET-ID 70
          BGCOLOR 8 
     "** Menu Size Change Requires a Close/Reopen **" VIEW-AS TEXT
          SIZE 49 BY .62 AT ROW 14.33 COL 17 WIDGET-ID 66
     " Menu Size" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 7.43 COL 5 WIDGET-ID 62
     " Language" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 1.24 COL 5 WIDGET-ID 58
     "Scheduling..." VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 10.05 COL 24 WIDGET-ID 48
          BGCOLOR 8 FONT 6
     "Scheduling..." VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 12.19 COL 26 WIDGET-ID 54
          BGCOLOR 8 FONT 6
     "Scheduling..." VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 8.38 COL 23 WIDGET-ID 42
          BGCOLOR 8 FONT 6
     "(Screen Size: 957 x 800)" VIEW-AS TEXT
          SIZE 24 BY .62 AT ROW 13.14 COL 41 WIDGET-ID 68
          BGCOLOR 8 
     RECT-13 AT ROW 8.14 COL 17 WIDGET-ID 38
     IMAGE-1 AT ROW 8.38 COL 18 WIDGET-ID 40
     RECT-14 AT ROW 9.81 COL 17 WIDGET-ID 46
     IMAGE-2 AT ROW 10.05 COL 18 WIDGET-ID 44
     RECT-15 AT ROW 11.95 COL 17 WIDGET-ID 52
     IMAGE-3 AT ROW 12.19 COL 18 WIDGET-ID 50
     RECT-16 AT ROW 1.48 COL 2 WIDGET-ID 56
     RECT-17 AT ROW 7.67 COL 2 WIDGET-ID 60
     RECT-18 AT ROW 15.52 COL 47 WIDGET-ID 64
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 15 
         TITLE BGCOLOR 15 "User Settings" WIDGET-ID 100.


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

/* SETTINGS FOR BUTTON btnLanguage-1 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       btnLanguage-1:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR BUTTON btnLanguage-2 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       btnLanguage-2:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR BUTTON btnLanguage-3 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       btnLanguage-3:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR IMAGE IMAGE-1 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-2 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-3 IN FRAME Dialog-Frame
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
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* User Settings */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel Dialog-Frame
ON CHOOSE OF btnCancel IN FRAME Dialog-Frame /* Cancel */
DO:
    iplOK = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLanguage-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLanguage-1 Dialog-Frame
ON CHOOSE OF btnLanguage-1 IN FRAME Dialog-Frame /* Lang 1 */
DO:
    svLanguageList:SCREEN-VALUE = "1".
    APPLY "VALUE-CHANGED":U TO svLanguageList.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLanguage-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLanguage-2 Dialog-Frame
ON CHOOSE OF btnLanguage-2 IN FRAME Dialog-Frame /* Lang 2 */
DO:
    svLanguageList:SCREEN-VALUE = "2".
    APPLY "VALUE-CHANGED":U TO svLanguageList.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLanguage-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLanguage-3 Dialog-Frame
ON CHOOSE OF btnLanguage-3 IN FRAME Dialog-Frame /* Lang 3 */
DO:
    svLanguageList:SCREEN-VALUE = "3".
    APPLY "VALUE-CHANGED":U TO svLanguageList.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOK Dialog-Frame
ON CHOOSE OF btnOK IN FRAME Dialog-Frame /* OK */
DO:
    ASSIGN
        iopiLanguage = svLanguageList
        iopiMenuSize = svMenuSize
        iplOK        = YES
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svLanguageList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svLanguageList Dialog-Frame
ON VALUE-CHANGED OF svLanguageList IN FRAME Dialog-Frame
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svMenuSize
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svMenuSize Dialog-Frame
ON VALUE-CHANGED OF svMenuSize IN FRAME Dialog-Frame
DO:
    ASSIGN {&SELF-NAME}.
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
  RUN pGetUserSettings.
  RUN enable_UI.
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
  DISPLAY svLanguageList svMenuSize 
      WITH FRAME Dialog-Frame.
  ENABLE btnCancel btnOK svLanguageList svMenuSize 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetUserSettings Dialog-Frame 
PROCEDURE pGetUserSettings :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cList AS CHARACTER NO-UNDO.
    DEFINE VARIABLE i     AS INTEGER   NO-UNDO.
    
    INPUT FROM VALUE(SEARCH("menuTrans.dat")) NO-ECHO.
    IMPORT UNFORMATTED cLanguageList. /* first row is list of avail languages */
    IMPORT UNFORMATTED cFlagList. /* second row is list of language flag images */
    INPUT CLOSE.
    svLanguageList:RADIO-BUTTONS IN FRAME {&FRAME-NAME} = ",0".
    DO i = 1 TO NUM-ENTRIES(cLanguageList):
        {system/btnLanguage.i 1}
        {system/btnLanguage.i 2}
        {system/btnLanguage.i 3}
        cList = cList + ENTRY(i,cLanguageList) + ","
                      + STRING(i) + ","
                      .
    END. /* do idx */
    ASSIGN
        cList = TRIM(cList,",")
        svLanguageList:RADIO-BUTTONS = cList
        svLanguageList = iopiLanguage
        svMenuSize = iopiMenuSize
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

