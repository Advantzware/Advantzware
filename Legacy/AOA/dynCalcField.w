&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: dynCalcField.w

  Description: Dynamic Calculated Fields

  Input Parameters: <none>

  Output Parameters: <none>

  Author: Ron Stark

  Created: 2.21.2019
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEFINE INPUT        PARAMETER iphCalcField    AS HANDLE    NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopcFieldName   AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopcFieldLabel  AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopcFieldFormat AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopcCalcProc    AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopcCalcParam   AS CHARACTER NO-UNDO.
DEFINE INPUT        PARAMETER ipcFieldList    AS CHARACTER NO-UNDO.
DEFINE       OUTPUT PARAMETER oplSave         AS LOGICAL   NO-UNDO.

/* Local Variable Definitions ---                                       */

IF NOT VALID-HANDLE(iphCalcField) THEN
RUN AOA/spCalcField.p PERSISTENT SET iphCalcField.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnCancel cFieldLabel cFieldFormat ~
cCalcParam cAvailFields cAvailCalcProc btnOK 
&Scoped-Define DISPLAYED-OBJECTS cFieldName cFieldLabel cFieldFormat ~
cCalcParam cAvailFields cCalcProc cAvailCalcProc 

/* Custom List Definitions                                              */
/* calcFields,List-2,List-3,List-4,List-5,List-6                        */
&Scoped-define calcFields cFieldName cFieldLabel cFieldFormat cCalcParam ~
cCalcProc 

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

DEFINE VARIABLE cCalcProc AS CHARACTER FORMAT "X(256)":U 
     LABEL "Calc Procedure" 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cFieldFormat AS CHARACTER FORMAT "X(256)":U INITIAL "x(256)" 
     LABEL "Field Format" 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE cFieldLabel AS CHARACTER FORMAT "X(256)":U 
     LABEL "Field Label" 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE cFieldName AS CHARACTER FORMAT "X(256)":U INITIAL "Calc" 
     LABEL "Field Name" 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 17.8 BY 2.38.

DEFINE VARIABLE cAvailCalcProc AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SORT SCROLLBAR-VERTICAL 
     SIZE 87 BY 27.14 TOOLTIP "Available Calculated Procedures" NO-UNDO.

DEFINE VARIABLE cAvailFields AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SORT SCROLLBAR-VERTICAL 
     SIZE 36 BY 18.81 NO-UNDO.

DEFINE VARIABLE cCalcParam AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 36 BY 5.71 TOOLTIP "Selected Parameters" NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     btnCancel AT ROW 8.38 COL 64
     cFieldName AT ROW 1.24 COL 16 COLON-ALIGNED WIDGET-ID 4
     cFieldLabel AT ROW 2.43 COL 16 COLON-ALIGNED WIDGET-ID 6
     cFieldFormat AT ROW 3.62 COL 16 COLON-ALIGNED WIDGET-ID 8
     cCalcParam AT ROW 4.81 COL 18 NO-LABEL WIDGET-ID 16
     cAvailFields AT ROW 10.76 COL 18 NO-LABEL WIDGET-ID 22
     cCalcProc AT ROW 1.24 COL 72 COLON-ALIGNED WIDGET-ID 10
     cAvailCalcProc AT ROW 2.43 COL 74 NO-LABEL WIDGET-ID 12
     btnOK AT ROW 8.38 COL 56
     "Available Fields:" VIEW-AS TEXT
          SIZE 16 BY 1 AT ROW 10.76 COL 2 WIDGET-ID 20
     "Calc Params:" VIEW-AS TEXT
          SIZE 13 BY 1 AT ROW 4.81 COL 5 WIDGET-ID 18
     "Avail Calc Proc:" VIEW-AS TEXT
          SIZE 15 BY 1 AT ROW 2.43 COL 59 WIDGET-ID 14
     RECT-1 AT ROW 8.14 COL 55 WIDGET-ID 2
     SPACE(88.19) SKIP(19.04)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FGCOLOR 1 
         TITLE "Calculated Field" WIDGET-ID 100.


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
   FRAME-NAME L-To-R,COLUMNS                                            */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR SELECTION-LIST cCalcParam IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR FILL-IN cCalcProc IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN cFieldFormat IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR FILL-IN cFieldLabel IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR FILL-IN cFieldName IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Calculated Field */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel Dialog-Frame
ON CHOOSE OF btnCancel IN FRAME Dialog-Frame /* Cancel */
DO:
    oplSave = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOK Dialog-Frame
ON CHOOSE OF btnOK IN FRAME Dialog-Frame /* OK */
DO:
    ASSIGN {&calcFields}.
    IF cFieldLabel EQ "" THEN DO:
        MESSAGE
            "Field Label cannot be Blank"
        VIEW-AS ALERT-BOX ERROR.
        APPLY "ENTRY":U TO cFieldLabel.
        RETURN NO-APPLY.
    END. /* if cfieldlabel */
    IF cFieldFormat EQ "" THEN DO:
        MESSAGE
            "Field Format cannot be Blank"
        VIEW-AS ALERT-BOX ERROR.
        APPLY "ENTRY":U TO cFieldFormat.
        RETURN NO-APPLY.
    END. /* if cfieldlabel */
    IF cCalcProc EQ "" THEN DO:
        MESSAGE
            "Calculation Procedure cannot be Blank"
        VIEW-AS ALERT-BOX ERROR.
        APPLY "ENTRY":U TO cAvailCalcProc.
        RETURN NO-APPLY.
    END. /* if cfieldlabel */
    ASSIGN
        iopcFieldName   = cFieldName
        iopcFieldLabel  = cFieldLabel
        iopcFieldFormat = cFieldFormat
        iopcCalcProc    = cCalcProc
        iopcCalcParam   = cCalcParam:LIST-ITEMS
        oplSave         = YES
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cAvailCalcProc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cAvailCalcProc Dialog-Frame
ON DEFAULT-ACTION OF cAvailCalcProc IN FRAME Dialog-Frame
DO:
    cCalcProc:SCREEN-VALUE = ENTRY(1,SELF:SCREEN-VALUE," ").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cAvailFields
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cAvailFields Dialog-Frame
ON DEFAULT-ACTION OF cAvailFields IN FRAME Dialog-Frame
DO:
    IF INDEX(cCalcParam:LIST-ITEMS,SELF:SCREEN-VALUE) EQ 0 OR
       cCalcParam:LIST-ITEMS EQ ? THEN
    cCalcParam:ADD-LAST(SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cCalcParam
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cCalcParam Dialog-Frame
ON DEFAULT-ACTION OF cCalcParam IN FRAME Dialog-Frame
DO:
    SELF:DELETE(SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cFieldLabel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cFieldLabel Dialog-Frame
ON VALUE-CHANGED OF cFieldLabel IN FRAME Dialog-Frame /* Field Label */
DO:
    cFieldName:SCREEN-VALUE = "Calc" + REPLACE(SELF:SCREEN-VALUE," ","").
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
  RUN pGetCalcProc.
  RUN enable_UI.
  APPLY "VALUE-CHANGED":U TO cFieldName.
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
  DISPLAY cFieldName cFieldLabel cFieldFormat cCalcParam cAvailFields cCalcProc 
          cAvailCalcProc 
      WITH FRAME Dialog-Frame.
  ENABLE btnCancel cFieldLabel cFieldFormat cCalcParam cAvailFields 
         cAvailCalcProc btnOK 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetCalcProc Dialog-Frame 
PROCEDURE pGetCalcProc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cCalcProcSign AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCalcProcList AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSignature    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE jdx           AS INTEGER   NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            cFieldName                = iopcFieldName
            cFieldLabel               = iopcFieldLabel
            cFieldFormat              = iopcFieldFormat
            cCalcProc                 = iopcCalcProc
            cCalcParam:DELIMITER      = "|"
            cCalcParam:LIST-ITEMS     = iopcCalcParam
            cAvailFields:LIST-ITEMS   = ipcFieldList
            cAvailCalcProc:DELIMITER  = "|"
            cAvailCalcProc:LIST-ITEMS = ?
            cCalcProcList             = iphCalcField:INTERNAL-ENTRIES
            .
        DO idx = 1 TO NUM-ENTRIES(cCalcProcList):
            IF ENTRY(idx,cCalcProcList) BEGINS "Calc" THEN DO:
                ASSIGN
                    cCalcProcSign  = ENTRY(idx,cCalcProcList)
                    cSignature     = LC(iphCalcField:GET-SIGNATURE(cCalcProcSign))
                    cCalcProcSign  = cCalcProcSign + " ("
                    .
                DO jdx = 3 TO NUM-ENTRIES(cSignature):
                    cCalcProcSign  = cCalcProcSign
                                   + (IF ENTRY(1,ENTRY(jdx,cSignature)," ") EQ "OUTPUT" THEN "OUT " ELSE "")
                                   + ENTRY(3,ENTRY(jdx,cSignature)," ")
                                   + ", "
                                   .
                END. /* do jdx */
                ASSIGN
                    cCalcProcSign = TRIM(cCalcProcSign,", ")
                    cCalcProcSign = cCalcProcSign + ")"
                    .
                cAvailCalcProc:ADD-LAST(cCalcProcSign).
            END. /* if calc proc */
        END. /* do idx */
    END. /* with frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

