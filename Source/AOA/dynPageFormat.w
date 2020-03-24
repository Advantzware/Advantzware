&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: AOA/dynPageFormat.w

  Description: Page Format Options

  Input Parameters: ?

  Output Parameters: ?

  Author: Ron Stark

  Created: 3.16.2020
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEFINE INPUT-OUTPUT PARAMETER iopiPageFormat  AS INTEGER   NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopcOrientation AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopiPageWidth   AS INTEGER   NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopiPageHeight  AS INTEGER   NO-UNDO.

DEFINE OUTPUT PARAMETER oplContinue AS LOGICAL NO-UNDO.

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE cPageFormat AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPageSize   AS CHARACTER NO-UNDO.
DEFINE VARIABLE idx         AS INTEGER   NO-UNDO.

ASSIGN
    cPageFormat = "~
Letter,~
Note,~
Legal,~
A0,~
A1,~
A2,~
A3,~
A4,~
A5,~
A6,~
A7,~
A8,~
A9,~
A10,~
B0,~
B1,~
B2,~
B3,~
B4,~
B5,~
ARCH_E,~
ARCH_D,~
ARCH_C,~
ARCH_B,~
ARCH_A,~
FLSA,~
FLSE,~
HalfLetter,~
11X17,~
Ledger,~
Custom"
    cPageSize = "~
612x792,~
540x720,~
612x1008,~
2380x3368,~
1684x3368,~
1190x1684,~
842x1190,~
595x842,~
421x595,~
297x421,~
210x297,~
148x210,~
105x148,~
74x105,~
2836x4008,~
2004x2836,~
1418x2004,~
1002x1418,~
709x1002,~
501x709,~
2592x3456,~
1728x2593,~
1296x1728,~
864x1296,~
648x864,~
612x936,~
612x936,~
396x612,~
792x1224,~
1224x792,~
0x0".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnOK btnCancel PageFormat iPageHeight ~
cOrientation 
&Scoped-Define DISPLAYED-OBJECTS PageFormat iPageWidth iPageHeight cOrientation 

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
     SIZE 8 BY 1.91
     BGCOLOR 8 .

DEFINE BUTTON btnOK AUTO-GO 
     IMAGE-UP FILE "Graphics/32x32/navigate_check.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "OK" 
     SIZE 8 BY 1.91
     BGCOLOR 8 .

DEFINE VARIABLE iPageHeight AS INTEGER FORMAT ">,>>9":U INITIAL 9999 
     LABEL "Page Height" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE iPageWidth AS INTEGER FORMAT ">,>>9":U INITIAL 9999 
     LABEL "Page Width" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE cOrientation AS CHARACTER INITIAL "Landscape" 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Portrait", "Portrait",
"Landscape", "Landscape"
     SIZE 14 BY 2 NO-UNDO.

DEFINE VARIABLE PageFormat AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "", 1
     SIZE 29 BY 28.1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 17.8 BY 2.38.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 31 BY 28.57.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 16 BY 2.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     btnOK AT ROW 27.43 COL 60 WIDGET-ID 6
     btnCancel AT ROW 27.43 COL 68 WIDGET-ID 4
     PageFormat AT ROW 1.24 COL 17 NO-LABEL WIDGET-ID 12
     iPageWidth AT ROW 11.24 COL 59 COLON-ALIGNED WIDGET-ID 18
     iPageHeight AT ROW 12.43 COL 59 COLON-ALIGNED WIDGET-ID 20
     cOrientation AT ROW 13.86 COL 62 NO-LABEL WIDGET-ID 24
     "Orientation:" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 13.86 COL 49 WIDGET-ID 30
     "Page Format:" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 1.24 COL 2 WIDGET-ID 14
     RECT-1 AT ROW 27.19 COL 59 WIDGET-ID 2
     RECT-2 AT ROW 1 COL 16 WIDGET-ID 16
     RECT-3 AT ROW 13.62 COL 61 WIDGET-ID 28
     SPACE(0.00) SKIP(13.57)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 15 FGCOLOR 1 
         TITLE "Page Format Selection" WIDGET-ID 100.


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

/* SETTINGS FOR FILL-IN iPageWidth IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-2 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-3 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Page Format Selection */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel Dialog-Frame
ON CHOOSE OF btnCancel IN FRAME Dialog-Frame /* Cancel */
DO:
    oplContinue = NO.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOK Dialog-Frame
ON CHOOSE OF btnOK IN FRAME Dialog-Frame /* OK */
DO:
    ASSIGN
        PageFormat
        cOrientation
        iPageWidth
        iPageHeight
        iopiPageFormat  = PageFormat
        iopcOrientation = cOrientation
        iopiPageWidth   = iPageWidth
        iopiPageHeight  = iPageHeight
        oplContinue     = YES
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cOrientation
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cOrientation Dialog-Frame
ON VALUE-CHANGED OF cOrientation IN FRAME Dialog-Frame
DO:
    ASSIGN {&SELF-NAME}.
    APPLY "VALUE-CHANGED":U TO PageFormat.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME PageFormat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL PageFormat Dialog-Frame
ON VALUE-CHANGED OF PageFormat IN FRAME Dialog-Frame
DO:
    ASSIGN {&SELF-NAME}.
    CASE cOrientation:
        WHEN "Portrait" THEN
        ASSIGN
            iPageWidth:SCREEN-VALUE  = ENTRY(1,ENTRY({&SELF-NAME},cPageSize),"x")
            iPageHeight:SCREEN-VALUE = ENTRY(2,ENTRY({&SELF-NAME},cPageSize),"x")
            .
        WHEN "Landscape" THEN
        ASSIGN
            iPageWidth:SCREEN-VALUE  = ENTRY(2,ENTRY({&SELF-NAME},cPageSize),"x")
            iPageHeight:SCREEN-VALUE = ENTRY(1,ENTRY({&SELF-NAME},cPageSize),"x")
            .
    END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  DO idx = 1 TO NUM-ENTRIES(cPageFormat):
    PageFormat:ADD-LAST(ENTRY(idx,cPageFormat)
              + " - [ "
              + REPLACE(ENTRY(idx,cPageSize),"x"," x ")
              + " ]", idx).
  END. /* do idx */
  PageFormat:DELETE("").
  ASSIGN
    PageFormat:SCREEN-VALUE   = STRING(iopiPageFormat)
    cOrientation:SCREEN-VALUE = iopcOrientation
    iPageWidth:SCREEN-VALUE   = STRING(iopiPageWidth)
    iPageHeight:SCREEN-VALUE  = STRING(iopiPageHeight)
    .
  APPLY "VALUE-CHANGED":U TO PageFormat.
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
  DISPLAY PageFormat iPageWidth iPageHeight cOrientation 
      WITH FRAME Dialog-Frame.
  ENABLE btnOK btnCancel PageFormat iPageHeight cOrientation 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

