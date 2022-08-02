&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
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
{est\ttEstCopySheet.i}

/* Parameters Definitions ---                                           */
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttBlanksToCopy. 
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttBlanksToCopyInto.
DEFINE OUTPUT PARAMETER lopError  AS LOGICAL NO-UNDO.

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE ghBrowse2 AS HANDLE NO-UNDO.
DEFINE VARIABLE iCounter AS INTEGER NO-UNDO.
DEFINE VARIABLE ghColumn AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttBlanksToCopy ttBlanksToCopyInto

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 ttBlanksToCopy.lIsSelected ttBlanksToCopy.iFormNo ttBlanksToCopy.iBlankNo ttBlanksToCopy.cPartNo ttBlanksToCopy.cPartDescription   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2 ttBlanksToCopy.lIsSelected   
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-2 ttBlanksToCopy
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-2 ttBlanksToCopy
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH ttBlanksToCopy       NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY {&SELF-NAME} FOR EACH ttBlanksToCopy       NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 ttBlanksToCopy
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 ttBlanksToCopy


/* Definitions for BROWSE BROWSE-3                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-3 ttBlanksToCopyInto.lIsSelected ttBlanksToCopyInto.iFormNo ttBlanksToCopyInto.iBlankNo ttBlanksToCopyInto.cPartNo ttBlanksToCopyInto.cPartDescription ttBlanksToCopyInto.lCopyDie ttBlanksToCopyInto.lCopyCad ttBlanksToCopyInto.lCopyOtherAttributes ttBlanksToCopyInto.lDoReCalculation   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-3 ttBlanksToCopyInto.lIsSelected ~
   ttBlanksToCopyInto.lCopyDie ~
   ttBlanksToCopyInto.lCopyCad ~
   ttBlanksToCopyInto.lCopyOtherAttributes ~
   ttBlanksToCopyInto.lDoReCalculation   
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-3 ttBlanksToCopyInto
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-3 ttBlanksToCopyInto
&Scoped-define SELF-NAME BROWSE-3
&Scoped-define QUERY-STRING-BROWSE-3 FOR EACH ttBlanksToCopyInto       NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-3 OPEN QUERY {&SELF-NAME} FOR EACH ttBlanksToCopyInto       NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-3 ttBlanksToCopyInto
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-3 ttBlanksToCopyInto


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-2}~
    ~{&OPEN-QUERY-BROWSE-3}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 BROWSE-2 BROWSE-3 Btn_Cancel Btn_OK 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "Copy" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 134 BY 14.29.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      ttBlanksToCopy SCROLLING.

DEFINE QUERY BROWSE-3 FOR 
      ttBlanksToCopyInto SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 Dialog-Frame _FREEFORM
  QUERY BROWSE-2 NO-LOCK DISPLAY
      ttBlanksToCopy.lIsSelected COLUMN-LABEL "" VIEW-AS TOGGLE-BOX 
      ttBlanksToCopy.iFormNo COLUMN-LABEL "Form" FORMAT ">9"
      ttBlanksToCopy.iBlankNo COLUMN-LABEL "Blank" FORMAT ">9"
      ttBlanksToCopy.cPartNo COLUMN-LABEL "Part" FORMAT "X(15)"
      ttBlanksToCopy.cPartDescription COLUMN-LABEL "Description" FORMAT "X(30)"
      ENABLE ttBlanksToCopy.lIsSelected
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 130 BY 5.52 FIT-LAST-COLUMN.

DEFINE BROWSE BROWSE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-3 Dialog-Frame _FREEFORM
  QUERY BROWSE-3 NO-LOCK DISPLAY
      ttBlanksToCopyInto.lIsSelected COLUMN-LABEL "" VIEW-AS TOGGLE-BOX 
      ttBlanksToCopyInto.iFormNo COLUMN-LABEL "Form" FORMAT ">9"
      ttBlanksToCopyInto.iBlankNo COLUMN-LABEL "Blank" FORMAT ">9"
      ttBlanksToCopyInto.cPartNo COLUMN-LABEL "Part" FORMAT "X(15)"
      ttBlanksToCopyInto.cPartDescription COLUMN-LABEL "Description" FORMAT "X(30)"
      ttBlanksToCopyInto.lCopyDie COLUMN-LABEL "Copy Die" VIEW-AS TOGGLE-BOX 
      ttBlanksToCopyInto.lCopyCad COLUMN-LABEL "Copy Cad" VIEW-AS TOGGLE-BOX
      ttBlanksToCopyInto.lCopyOtherAttributes COLUMN-LABEL "Copy OtherAttributes" VIEW-AS TOGGLE-BOX  
      ttBlanksToCopyInto.lDoReCalculation COLUMN-LABEL "Do Recalc" WIDTH 9 VIEW-AS TOGGLE-BOX 
      ENABLE ttBlanksToCopyInto.lIsSelected
             ttBlanksToCopyInto.lCopyDie
             ttBlanksToCopyInto.lCopyCad
             ttBlanksToCopyInto.lCopyOtherAttributes
             ttBlanksToCopyInto.lDoReCalculation
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 130 BY 5.52 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BROWSE-2 AT ROW 2.14 COL 4 WIDGET-ID 200
     BROWSE-3 AT ROW 8.48 COL 4 WIDGET-ID 300
     Btn_Cancel AT ROW 14.14 COL 85
     Btn_OK AT ROW 14.19 COL 57
     "Copy From Item" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 1.33 COL 4.6 WIDGET-ID 2
     "Copy To Item(s)" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 7.67 COL 4 WIDGET-ID 4
     RECT-1 AT ROW 1.24 COL 2 WIDGET-ID 6
     SPACE(1.39) SKIP(0.03)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Estimate Item Copy"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel WIDGET-ID 100.


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
/* BROWSE-TAB BROWSE-2 RECT-1 Dialog-Frame */
/* BROWSE-TAB BROWSE-3 BROWSE-2 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttBlanksToCopy
      NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-3
/* Query rebuild information for BROWSE BROWSE-3
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttBlanksToCopyInto
      NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE BROWSE-3 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Estimate Item Copy */
DO:
    lopError = YES.
    APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel Dialog-Frame
ON CHOOSE OF Btn_Cancel IN FRAME Dialog-Frame /* Cancel */
DO:
    lopError = YES.
    APPLY "CLOSE":U TO SELF.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* Copy */
DO: 
    FIND FIRST ttBlanksToCopyInto NO-LOCK 
        WHERE  ttBlanksToCopyInto.lIsSelected = YES NO-ERROR.
    IF NOT AVAILABLE ttBlanksToCopyInto THEN 
    DO:
        lopError = YES. 
        MESSAGE "Please select Form or Blank from CopyToItem(s)"
        VIEW-AS ALERT-BOX WARNING. 
        RETURN NO-APPLY.
    END. 
    ELSE 
    DO:
        MESSAGE "Are you sure you want to Copy Blank Info? " VIEW-AS ALERT-BOX WARNING
        BUTTON YES-NO UPDATE lCopyBlankInfo AS LOG.
        IF lCopyBlankInfo THEN 
            lopError = NO.
        ELSE 
            lopError = YES.    
    END.     
           
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   ghBrowse2 = BROWSE Browse-2:HANDLE. 
   
   DO iCounter = 1 TO ghBrowse2:NUM-COLUMNS:
       ghColumn = ghBrowse2:GET-BROWSE-COLUMN(iCounter).
       IF ghColumn:NAME = "lisSelected" THEN 
           ghColumn:READ-ONLY = TRUE.
       ELSE NEXT.
    END.
      
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
  ENABLE RECT-1 BROWSE-2 BROWSE-3 Btn_Cancel Btn_OK 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

