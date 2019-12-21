&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS s-object 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File: userTasks.w

  Description: from SMART.W - Template for basic SmartObject

  Author: Ron Stark
  Created: 1.14.2019

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

&Scoped-define prgmName userParam.

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE char-hdl        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPoolName       AS CHARACTER NO-UNDO.
DEFINE VARIABLE hAppSrvBin      AS HANDLE    NO-UNDO.
DEFINE VARIABLE hDynDescripProc AS HANDLE    NO-UNDO.
DEFINE VARIABLE hDynInitProc    AS HANDLE    NO-UNDO.
DEFINE VARIABLE hDynValProc     AS HANDLE    NO-UNDO.
DEFINE VARIABLE lSave           AS LOGICAL   NO-UNDO.
DEFINE VARIABLE pHandle         AS HANDLE    NO-UNDO.
DEFINE VARIABLE rRowID          AS ROWID     NO-UNDO.

{AOA/tempTable/ttDynAction.i}

/* function fDateOptions */
{AOA/includes/fDateOptions.i}
/* function fDateOptionValue */
{AOA/includes/fDateOptionValue.i}

{AOA/includes/dynFuncs.i}

RUN AOA/spDynDescriptionProc.p PERSISTENT SET hDynDescripProc.
RUN AOA/spDynInitializeProc.p  PERSISTENT SET hDynInitProc.
RUN AOA/spDynValidateProc.p    PERSISTENT SET hDynValProc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartObject
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME paramFrame

/* Custom List Definitions                                              */
/* outputObjects,showFields,List-3,List-4,List-5,List-6                 */
&Scoped-define outputObjects btnSave btnVisibleSets svRecipients ~
svSetAlignment defaultOutputFormat svShowAll svShowReportHeader ~
svShowReportFooter svShowPageHeader svShowPageFooter svShowGroupHeader ~
svShowGroupFooter svShowParameters btnAddEmail 
&Scoped-define showFields svShowAll svShowReportHeader svShowReportFooter ~
svShowPageHeader svShowPageFooter svShowGroupHeader svShowGroupFooter ~
svShowParameters 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fSetShowAll s-object 
FUNCTION fSetShowAll RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAddEmail 
     IMAGE-UP FILE "AOA/images/navigate_plus.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Email" 
     SIZE 4.4 BY 1.05 TOOLTIP "Add Recipents".

DEFINE BUTTON btnSave 
     IMAGE-UP FILE "Graphics/32x32/floppy_disk.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Save" 
     SIZE 8 BY 1.91 TOOLTIP "Save".

DEFINE BUTTON btnVisibleSets 
     IMAGE-UP FILE "Graphics/32x32/window_dialog.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91 TOOLTIP "Show/Hide Parameter Sets".

DEFINE VARIABLE svRecipients AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 69 BY 2.38
     BGCOLOR 15 .

DEFINE VARIABLE defaultOutputFormat AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Grid", "Grid",
"CSV", "CSV",
"XLS", "XLS",
"DocX", "DOCX",
"PDF", "PDF",
"HTML", "HTML"
     SIZE 48 BY 1 NO-UNDO.

DEFINE VARIABLE svSetAlignment AS CHARACTER INITIAL "Custom" 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Columns", "Columns",
"Rows", "Rows",
"Custom", "Custom"
     SIZE 13 BY 1.91 NO-UNDO.

DEFINE RECTANGLE RECT-PANEL
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 66 BY 2.38.

DEFINE RECTANGLE RECT-SHOW
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 156 BY 1.19.

DEFINE VARIABLE svShowAll AS LOGICAL INITIAL yes 
     LABEL "Show ALL" 
     VIEW-AS TOGGLE-BOX
     SIZE 13 BY .81 NO-UNDO.

DEFINE VARIABLE svShowGroupFooter AS LOGICAL INITIAL yes 
     LABEL "Group Footer" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE svShowGroupHeader AS LOGICAL INITIAL yes 
     LABEL "Group Header" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .81 NO-UNDO.

DEFINE VARIABLE svShowPageFooter AS LOGICAL INITIAL yes 
     LABEL "Page Footer" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE svShowPageHeader AS LOGICAL INITIAL yes 
     LABEL "Page Header" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE svShowParameters AS LOGICAL INITIAL yes 
     LABEL "Parameters" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .81 NO-UNDO.

DEFINE VARIABLE svShowReportFooter AS LOGICAL INITIAL yes 
     LABEL "Report Footer" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 NO-UNDO.

DEFINE VARIABLE svShowReportHeader AS LOGICAL INITIAL yes 
     LABEL "Report Header" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME paramFrame
     SPACE(158.01) SKIP(5.25)
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FGCOLOR 1  WIDGET-ID 100.

DEFINE FRAME outputFrame
     btnSave AT ROW 1.48 COL 149 HELP
          "Update/Save" WIDGET-ID 248
     btnVisibleSets AT ROW 1.48 COL 141 HELP
          "Show/Hide Parameter Sets" WIDGET-ID 660
     svRecipients AT ROW 1.24 COL 8 NO-LABEL WIDGET-ID 600
     svSetAlignment AT ROW 1.71 COL 78 NO-LABEL WIDGET-ID 654
     defaultOutputFormat AT ROW 2.43 COL 93 NO-LABEL WIDGET-ID 644
     svShowAll AT ROW 4.1 COL 8 WIDGET-ID 18
     svShowReportHeader AT ROW 4.1 COL 23 WIDGET-ID 2
     svShowReportFooter AT ROW 4.1 COL 44 WIDGET-ID 4
     svShowPageHeader AT ROW 4.1 COL 65 WIDGET-ID 6
     svShowPageFooter AT ROW 4.1 COL 85 WIDGET-ID 8
     svShowGroupHeader AT ROW 4.1 COL 103 WIDGET-ID 10
     svShowGroupFooter AT ROW 4.1 COL 123 WIDGET-ID 12
     svShowParameters AT ROW 4.1 COL 142 WIDGET-ID 16
     btnAddEmail AT ROW 2.19 COL 3 HELP
          "Add Recipents" WIDGET-ID 636
     "Set Alignment" VIEW-AS TEXT
          SIZE 13.6 BY .62 AT ROW 1 COL 78 WIDGET-ID 658
     "Default Output Format:" VIEW-AS TEXT
          SIZE 23 BY 1 AT ROW 1.48 COL 93 WIDGET-ID 652
     "Email:" VIEW-AS TEXT
          SIZE 6 BY .62 AT ROW 1.48 COL 2 WIDGET-ID 640
     RECT-PANEL AT ROW 1.24 COL 92 WIDGET-ID 256
     RECT-SHOW AT ROW 3.86 COL 2 WIDGET-ID 642
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 158 BY 5.24
         BGCOLOR 15 
         TITLE BGCOLOR 15 "Parameters" WIDGET-ID 1300.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartObject
   Allow: Basic
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW s-object ASSIGN
         HEIGHT             = 27
         WIDTH              = 158.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB s-object 
/* ************************* Included-Libraries *********************** */

{src/adm/method/smart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW s-object
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME outputFrame:FRAME = FRAME paramFrame:HANDLE.

/* SETTINGS FOR FRAME outputFrame
                                                                        */
/* SETTINGS FOR BUTTON btnAddEmail IN FRAME outputFrame
   1                                                                    */
/* SETTINGS FOR BUTTON btnSave IN FRAME outputFrame
   1                                                                    */
/* SETTINGS FOR BUTTON btnVisibleSets IN FRAME outputFrame
   1                                                                    */
/* SETTINGS FOR RADIO-SET defaultOutputFormat IN FRAME outputFrame
   1                                                                    */
/* SETTINGS FOR RECTANGLE RECT-PANEL IN FRAME outputFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-SHOW IN FRAME outputFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR svRecipients IN FRAME outputFrame
   1                                                                    */
/* SETTINGS FOR RADIO-SET svSetAlignment IN FRAME outputFrame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX svShowAll IN FRAME outputFrame
   1 2                                                                  */
/* SETTINGS FOR TOGGLE-BOX svShowGroupFooter IN FRAME outputFrame
   1 2                                                                  */
/* SETTINGS FOR TOGGLE-BOX svShowGroupHeader IN FRAME outputFrame
   1 2                                                                  */
/* SETTINGS FOR TOGGLE-BOX svShowPageFooter IN FRAME outputFrame
   1 2                                                                  */
/* SETTINGS FOR TOGGLE-BOX svShowPageHeader IN FRAME outputFrame
   1 2                                                                  */
/* SETTINGS FOR TOGGLE-BOX svShowParameters IN FRAME outputFrame
   1 2                                                                  */
/* SETTINGS FOR TOGGLE-BOX svShowReportFooter IN FRAME outputFrame
   1 2                                                                  */
/* SETTINGS FOR TOGGLE-BOX svShowReportHeader IN FRAME outputFrame
   1 2                                                                  */
/* SETTINGS FOR FRAME paramFrame
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME paramFrame:HEIGHT           = 26.95
       FRAME paramFrame:WIDTH            = 158.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME paramFrame
/* Query rebuild information for FRAME paramFrame
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME paramFrame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define FRAME-NAME outputFrame
&Scoped-define SELF-NAME btnAddEmail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAddEmail s-object
ON CHOOSE OF btnAddEmail IN FRAME outputFrame /* Email */
DO:
    RUN pRecipients (svRecipients:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave s-object
ON CHOOSE OF btnSave IN FRAME outputFrame /* Save */
DO:
    RUN pSaveDynParamValues (defaultOutputFormat).
    MESSAGE
        "Parameter Values Saved!"
    VIEW-AS ALERT-BOX TITLE "Save".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnVisibleSets
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnVisibleSets s-object
ON CHOOSE OF btnVisibleSets IN FRAME outputFrame
DO:
    RUN AOA/dynVisibleSets.w (ROWID(dynParamValue), OUTPUT lSave).
    IF lSave THEN DO:
        FIND CURRENT dynParamValue NO-LOCK.
        RUN pShowParameterSets.
    END. /* if lsave */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME defaultOutputFormat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL defaultOutputFormat s-object
ON VALUE-CHANGED OF defaultOutputFormat IN FRAME outputFrame
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svSetAlignment
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svSetAlignment s-object
ON VALUE-CHANGED OF svSetAlignment IN FRAME outputFrame
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svShowAll
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowAll s-object
ON VALUE-CHANGED OF svShowAll IN FRAME outputFrame /* Show ALL */
DO:
  ASSIGN {&SELF-NAME}
      svShowReportHeader = {&SELF-NAME}
      svShowParameters   = {&SELF-NAME}
      svShowPageHeader   = {&SELF-NAME}
      svShowGroupHeader  = {&SELF-NAME}
      svShowGroupFooter  = {&SELF-NAME}
      svShowPageFooter   = {&SELF-NAME}
      svShowReportFooter = {&SELF-NAME}
      .
  DISPLAY {&showFields} WITH FRAME outputFrame.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svShowGroupFooter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowGroupFooter s-object
ON VALUE-CHANGED OF svShowGroupFooter IN FRAME outputFrame /* Group Footer */
DO:
    ASSIGN {&SELF-NAME}.
    fSetShowAll().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svShowGroupHeader
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowGroupHeader s-object
ON VALUE-CHANGED OF svShowGroupHeader IN FRAME outputFrame /* Group Header */
DO:
    ASSIGN {&SELF-NAME}.
    fSetShowAll().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svShowPageFooter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowPageFooter s-object
ON VALUE-CHANGED OF svShowPageFooter IN FRAME outputFrame /* Page Footer */
DO:
    ASSIGN {&SELF-NAME}.
    fSetShowAll().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svShowPageHeader
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowPageHeader s-object
ON VALUE-CHANGED OF svShowPageHeader IN FRAME outputFrame /* Page Header */
DO:
    ASSIGN {&SELF-NAME}.
    fSetShowAll().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svShowParameters
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowParameters s-object
ON VALUE-CHANGED OF svShowParameters IN FRAME outputFrame /* Parameters */
DO:
    ASSIGN {&SELF-NAME}.
    fSetShowAll().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svShowReportFooter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowReportFooter s-object
ON VALUE-CHANGED OF svShowReportFooter IN FRAME outputFrame /* Report Footer */
DO:
    ASSIGN {&SELF-NAME}.
    fSetShowAll().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svShowReportHeader
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowReportHeader s-object
ON VALUE-CHANGED OF svShowReportHeader IN FRAME outputFrame /* Report Header */
DO:
    ASSIGN {&SELF-NAME}.
    fSetShowAll().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME paramFrame
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK s-object 


/* ***************************  Main Block  *************************** */

/* If testing in the UIB, initialize the SmartObject. */  
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{AOA/includes/pDynParamProcs.i "dyn"}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI s-object  _DEFAULT-DISABLE
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
  HIDE FRAME outputFrame.
  HIDE FRAME paramFrame.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize s-object 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {methods/run_link.i "CONTAINER" "pGethAppSrvBin" "(OUTPUT hAppSrvBin)"}
  {methods/run_link.i "CONTAINER" "pGetCompany" "(OUTPUT cCompany)"}
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-view s-object 
PROCEDURE local-view :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'view':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN pShowParameterSets.
  ENABLE {&outputObjects} WITH FRAME outputFrame.
  ASSIGN
      btnSave:HIDDEN = NOT AVAILABLE dynParamValue OR dynParamValue.user-id EQ "_default"
      btnVisibleSets:HIDDEN = btnSave:HIDDEN
      .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetDynParamValue s-object 
PROCEDURE pGetDynParamValue :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {methods/run_link.i "CONTAINER" "pGetParamValueRowID" "(OUTPUT rRowID)"}
    IF rRowID EQ ? THEN RETURN.
    FIND FIRST dynParamValue NO-LOCK WHERE ROWID(dynParamValue) EQ rRowID.
    ASSIGN
        defaultOutputFormat:SCREEN-VALUE IN FRAME outputFrame =
            IF dynParamValue.outputFormat NE "View" THEN dynParamValue.outputFormat
            ELSE "Grid"
        defaultOutputFormat
        .
    FIND FIRST dynSubject NO-LOCK
         WHERE dynSubject.subjectID EQ dynParamValue.subject
         NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetSettings s-object 
PROCEDURE pGetSettings :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcUserID AS CHARACTER NO-UNDO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pShowParameterSets s-object 
PROCEDURE pShowParameterSets :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN pCreateDynParameters (FRAME {&FRAME-NAME}:HANDLE, YES).
  FRAME {&FRAME-NAME}:MOVE-TO-TOP().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pWinReSize s-object 
PROCEDURE pWinReSize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipdHeight AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdWidth  AS DECIMAL NO-UNDO.
    
    HIDE FRAME {&FRAME-NAME}.
    ASSIGN
        FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT = ipdHeight
        FRAME {&FRAME-NAME}:VIRTUAL-WIDTH  = ipdWidth
        FRAME {&FRAME-NAME}:HEIGHT         = ipdHeight
        FRAME {&FRAME-NAME}:WIDTH          = ipdWidth
        .
    VIEW FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed s-object 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     Receive and process 'state-changed' methods
               (issued by 'new-state' event).
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
  END CASE.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fSetShowAll s-object 
FUNCTION fSetShowAll RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DO WITH FRAME outputFrame:
        svShowAll = svShowReportHeader AND
                    svShowParameters   AND
                    svShowPageHeader   AND
                    svShowGroupHeader  AND
                    svShowGroupFooter  AND
                    svShowPageFooter   AND
                    svShowReportFooter
                    .
        DISPLAY {&showFields}.
    END. /* do with */
    RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

