&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: about.w

  Description: About Box

  Input Parameters: Calling Program

  Output Parameters: <none>

  Author: Ron Stark

  Created: 03/07/98
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

&IF DEFINED(UIB_is_Running) = 0 &THEN
DEFINE INPUT PARAMETER callingprgm AS CHARACTER NO-UNDO.
&ELSE
DEFINE VARIABLE callingprgm AS CHARACTER INITIAL "prgrms." NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE intTotalPhys AS INTEGER NO-UNDO.
DEFINE VARIABLE intAvailPhys AS INTEGER NO-UNDO.
DEFINE VARIABLE ptrToMemStatStruct AS MEMPTR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-2 RECT-3 Btn_OK 
&Scoped-Define DISPLAYED-OBJECTS physical_file version copyrite memory 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "&OK" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE copyrite AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 54 BY .62 NO-UNDO.

DEFINE VARIABLE memory AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 54 BY .62 NO-UNDO.

DEFINE VARIABLE physical_file AS CHARACTER FORMAT "X(256)":U 
     LABEL "Physical File" 
      VIEW-AS TEXT 
     SIZE 38 BY .62 NO-UNDO.

DEFINE VARIABLE version AS CHARACTER FORMAT "X(256)":U 
     LABEL "Version" 
      VIEW-AS TEXT 
     SIZE 19.6 BY .62 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "Graphics/asiicon.ico":U
     SIZE 6.6 BY 1.52.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 54 BY .24.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 54 BY .24.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     Btn_OK AT ROW 7.43 COL 28
     physical_file AT ROW 1.48 COL 27 COLON-ALIGNED
     version AT ROW 2.43 COL 21 COLON-ALIGNED
     copyrite AT ROW 3.38 COL 11 COLON-ALIGNED NO-LABEL
     memory AT ROW 5.29 COL 11 COLON-ALIGNED NO-LABEL
     IMAGE-1 AT ROW 1.48 COL 4
     RECT-2 AT ROW 4.33 COL 13
     RECT-3 AT ROW 6.48 COL 13
     SPACE(0.99) SKIP(2.18)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "About AdvantzWare".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
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

/* SETTINGS FOR FILL-IN copyrite IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-1 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN memory IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN physical_file IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN version IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* About AdvantzWare */
DO:
  APPLY "END-ERROR":U TO SELF.
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
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      physical_file:SCREEN-VALUE = ENTRY(NUM-ENTRIES(callingprgm," "),callingprgm," ")
      copyrite:SCREEN-VALUE = "{copyrite}"
      callingprgm = ENTRY(NUM-ENTRIES(callingprgm," "),callingprgm," ")
      callingprgm = SUBSTR(callingprgm,R-INDEX(callingprgm,"/") + 1)
      callingprgm = SUBSTR(callingprgm,1,LENGTH(callingprgm) - 1).
    FIND prgrms WHERE prgrms.prgmname = callingprgm NO-LOCK NO-ERROR.
    IF AVAILABLE prgrms THEN
    ASSIGN
      FRAME {&FRAME-NAME}:TITLE = FRAME {&FRAME-NAME}:TITLE + " " + prgrms.prgtitle
      version:SCREEN-VALUE = prgrms.prgm_ver.
    SET-SIZE(ptrToMemStatStruct) = 32.
    PUT-LONG(ptrToMemStatStruct,1) = 32.
    RUN GlobalMemoryStatus (INPUT ptrToMemStatStruct).
    ASSIGN
       intTotalPhys = GET-LONG(ptrToMemStatStruct,09)
       intAvailPhys = GET-LONG(ptrToMemStatStruct,13)
       memory:SCREEN-VALUE = "Phys. Memory " +
              TRIM(STRING(INTEGER(intTotalPhys / 1000),">,>>>,>>9")) + "K - " +
              TRIM(STRING(INTEGER(intAvailPhys / 1000),">,>>>,>>9")) + "K Free".
    SET-SIZE(ptrToMemStatStruct) = 0.
  END.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

PROCEDURE GlobalMemoryStatus EXTERNAL "KERNEL32.DLL":
    DEFINE INPUT PARAMETER ptrToMemStatStruct AS MEMPTR.
END PROCEDURE.

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
  DISPLAY physical_file version copyrite memory 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-2 RECT-3 Btn_OK 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

