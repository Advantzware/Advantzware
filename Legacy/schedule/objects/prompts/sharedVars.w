&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: sharedVars.w

  Description: display shared var values

  Input Parameters: <none>

  Output Parameters: <none>

  Author: Ron Stark

  Created: 5.28.2004
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{schedule/scopDir.i}
{{&includes}/defBoard.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.

{{&includes}/sharedVars.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS customValueListValue statusObjectValue ~
userLabelValue userWidthValue udfLabelValue udfWidthValue btnCancel 
&Scoped-Define DISPLAYED-OBJECTS asOfTimeValue customValueListValue ~
statusObjectValue userLabelValue userWidthValue udfLabelValue udfWidthValue ~
capacityLoadValue cascadeJobValue changeResourceValue copyToResourceValue ~
endDateMoveValue idValue printPrgmValue departmentListValue 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCancel AUTO-END-KEY 
     IMAGE-UP FILE "schedule/images/exit1.bmp":U
     LABEL "" 
     SIZE 7 BY 1.67
     BGCOLOR 8 .

DEFINE VARIABLE asOfTimeValue AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE departmentListValue AS CHARACTER FORMAT "X(256)":U 
     LABEL "departmentList" 
     VIEW-AS FILL-IN 
     SIZE 161 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE idValue AS CHARACTER FORMAT "X(256)":U 
     LABEL "ID" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE printPrgmValue AS CHARACTER FORMAT "X(256)":U 
     LABEL "printPrgm" 
     VIEW-AS FILL-IN 
     SIZE 161 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE customValueListValue AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 28 BY 12.86 NO-UNDO.

DEFINE VARIABLE statusObjectValue AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 46 BY 12.86 NO-UNDO.

DEFINE VARIABLE udfLabelValue AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 25 BY 12.86 NO-UNDO.

DEFINE VARIABLE udfWidthValue AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 16 BY 12.86 NO-UNDO.

DEFINE VARIABLE userLabelValue AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 25 BY 12.86 NO-UNDO.

DEFINE VARIABLE userWidthValue AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 16 BY 12.86 NO-UNDO.

DEFINE VARIABLE capacityLoadValue AS LOGICAL INITIAL no 
     LABEL "capacityLoad" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .81 NO-UNDO.

DEFINE VARIABLE cascadeJobValue AS LOGICAL INITIAL no 
     LABEL "cascadeJob" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE changeResourceValue AS LOGICAL INITIAL no 
     LABEL "changeResource" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE copyToResourceValue AS LOGICAL INITIAL no 
     LABEL "copyToResource" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE endDateMoveValue AS LOGICAL INITIAL no 
     LABEL "endDateMove" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     asOfTimeValue AT ROW 1.71 COL 2 NO-LABEL
     customValueListValue AT ROW 1.71 COL 31 NO-LABEL
     statusObjectValue AT ROW 1.71 COL 60 NO-LABEL
     userLabelValue AT ROW 1.71 COL 107 NO-LABEL
     userWidthValue AT ROW 1.71 COL 133 NO-LABEL
     udfLabelValue AT ROW 1.71 COL 150 NO-LABEL WIDGET-ID 2
     udfWidthValue AT ROW 1.71 COL 176 NO-LABEL WIDGET-ID 4
     capacityLoadValue AT ROW 2.91 COL 7
     cascadeJobValue AT ROW 3.86 COL 7
     changeResourceValue AT ROW 4.81 COL 7
     copyToResourceValue AT ROW 5.76 COL 7
     endDateMoveValue AT ROW 6.71 COL 7
     idValue AT ROW 7.67 COL 5 COLON-ALIGNED
     btnCancel AT ROW 9.1 COL 14
     printPrgmValue AT ROW 14.81 COL 29 COLON-ALIGNED
     departmentListValue AT ROW 16 COL 29 COLON-ALIGNED
     "udfLabel" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 1 COL 157 WIDGET-ID 8
     "udfWidth" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 1 COL 178 WIDGET-ID 6
     "customValueList" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 1 COL 36
     "userLabel" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 1 COL 114
     "userWidth" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 1 COL 135
     "statusObject" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 1 COL 78
     SPACE(101.99) SKIP(15.37)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Shared Var Values".


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

/* SETTINGS FOR FILL-IN asOfTimeValue IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR TOGGLE-BOX capacityLoadValue IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX cascadeJobValue IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX changeResourceValue IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX copyToResourceValue IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN departmentListValue IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX endDateMoveValue IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN idValue IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN printPrgmValue IN FRAME Dialog-Frame
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
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Shared Var Values */
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
  ASSIGN
    asOfTimeValue:SCREEN-VALUE = REPLACE(REPLACE(asOfTime,'(as of',''),')','')
    cascadeJobValue:SCREEN-VALUE = STRING(cascadeJob)
    changeResourceValue:SCREEN-VALUE = STRING(changeResource)
    copyToResourceValue:SCREEN-VALUE = STRING(copyToResource)
    customValueListValue:LIST-ITEMS = customValueList
    departmentListValue:SCREEN-VALUE = departmentList
    endDateMoveValue:SCREEN-VALUE = STRING(endDateMove)
    idValue:SCREEN-VALUE = ID
    printPrgmValue:SCREEN-VALUE = printPrgm.
  DO i = 1 TO EXTENT(statusObject):
    IF statusObject[i] EQ '' THEN LEAVE.
    ldummy = statusObjectValue:ADD-LAST(REPLACE(statusObject[i],',',' ; ')).
  END.
  DO i = 1 TO EXTENT(userLabel):
    ASSIGN
      ldummy = userLabelValue:ADD-LAST(userLabel[i])
      ldummy = userWidthValue:ADD-LAST(STRING(userWidth[i])).
  END.
  DO i = 1 TO EXTENT(udfLabel):
    ASSIGN
      ldummy = udfLabelValue:ADD-LAST(udfLabel[i])
      ldummy = udfWidthValue:ADD-LAST(STRING(udfWidth[i])).
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
  DISPLAY asOfTimeValue customValueListValue statusObjectValue userLabelValue 
          userWidthValue udfLabelValue udfWidthValue capacityLoadValue 
          cascadeJobValue changeResourceValue copyToResourceValue 
          endDateMoveValue idValue printPrgmValue departmentListValue 
      WITH FRAME Dialog-Frame.
  ENABLE customValueListValue statusObjectValue userLabelValue userWidthValue 
         udfLabelValue udfWidthValue btnCancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

