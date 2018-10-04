&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS s-object
/*------------------------------------------------------------------------

  File: smartMsg.w

  Description: from SMART.W - Template for basic SmartObject

  Author: Ron Stark
  Created: 1.1.1999 (updated 12.4.2016)

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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{methods/defines/hndlset.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartObject
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnNote btnUDF
&Scoped-Define DISPLAYED-OBJECTS mf-message notes-message

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnNote  NO-FOCUS FLAT-BUTTON
     LABEL "No Note"
     SIZE 5 BY 1.14.

DEFINE BUTTON btnUDF  NO-FOCUS FLAT-BUTTON
     LABEL "No UDF"
     SIZE 5 BY 1.14.

DEFINE VARIABLE mf-message AS CHARACTER FORMAT "X(256)":U
      VIEW-AS TEXT
     SIZE 22 BY .62
     FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE notes-message AS CHARACTER FORMAT "X(256)":U
      VIEW-AS TEXT
     SIZE 22 BY .62
     FGCOLOR 0  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btnNote AT ROW 1 COL 1 WIDGET-ID 8
     btnUDF AT ROW 1 COL 6 WIDGET-ID 10
     mf-message AT ROW 1 COL 11 NO-LABEL
     notes-message AT ROW 1.52 COL 11 NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY
         SIDE-LABELS NO-UNDERLINE THREE-D
         AT COL 1 ROW 1 SCROLLABLE
         BGCOLOR 15 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartObject
   Allow: Basic
   Frames: 1
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
         HEIGHT             = 1.14
         WIDTH              = 32.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB s-object
/* ************************* Included-Libraries *********************** */

{Advantzware/WinKit/winkit-panel.i}
{src/adm/method/smart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW s-object
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN mf-message IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN notes-message IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME btnNote
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNote s-object
ON CHOOSE OF btnNote IN FRAME F-Main /* No Note */
DO:
    {methods/run_link.i "CONTAINER-SOURCE" "Select_Notes"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUDF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUDF s-object
ON CHOOSE OF btnUDF IN FRAME F-Main /* No UDF */
DO:
    {methods/run_link.i "CONTAINER-SOURCE" "UDF"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mf-message
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mf-message s-object
ON LEFT-MOUSE-DOWN OF mf-message IN FRAME F-Main
DO:
    {methods/run_link.i "CONTAINER-SOURCE" "UDF"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME notes-message
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL notes-message s-object
ON LEFT-MOUSE-DOWN OF notes-message IN FRAME F-Main
DO:
    {methods/run_link.i "CONTAINER-SOURCE" "Select_Notes"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK s-object


/* ***************************  Main Block  *************************** */

/* If testing in the UIB, initialize the SmartObject. */
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN
  RUN dispatch IN THIS-PROCEDURE ('initialize':U).
&ENDIF

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
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize s-object
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lUDFActive AS LOGICAL NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF Consultingwerk.WindowIntegrationKit.WinKitSettings:WinKitActive EQ TRUE THEN
  DO WITH FRAME {&FRAME-NAME}:
    {methods/setButton.i btnNote "No Note" 16}
    {methods/run_link.i "CONTAINER-SOURCE" "pUDFACtive" "(OUTPUT lUDFActive)"}
    IF lUDFActive EQ NO THEN DO:
        ASSIGN
            btnUDF:HIDDEN = YES
            btnUDF:LABEL  = ""
            .
        btnUDF:LOAD-IMAGE("").
    END.
    ELSE
        {methods/setButton.i btnUDF "No UDF" 16}
  END.
  ELSE
  ASSIGN
    btnNote:HIDDEN = YES
    btnUDF:HIDDEN  = YES
    .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE make-insensitive s-object
PROCEDURE make-insensitive :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            mf-message:SENSITIVE    = FALSE
            notes-message:SENSITIVE = FALSE
            .
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE make-sensitive s-object
PROCEDURE make-sensitive :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            mf-message:SENSITIVE    = TRUE
            notes-message:SENSITIVE = TRUE
            .
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRefreshRibbonButtons s-object
PROCEDURE pRefreshRibbonButtons :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hWinKitWidgetHandle AS HANDLE NO-UNDO.

    Consultingwerk.Util.UltraToolbarsHelper:RefreshTools (oForm:ToolbarsManager, FALSE, FALSE) .
    {Consultingwerk/foreach.i Infragistics.Win.UltraWinToolbars.ToolBase oTool in oForm:ToolbarsManager:Tools}
        hWinKitWidgetHandle = WIDGET-HANDLE (UNBOX (oTool:Tag)) .
        IF VALID-HANDLE (hWinKitWidgetHandle) AND CAN-QUERY (hWinKitWidgetHandle, "LABEL":U) THEN DO:
                oTool:SharedProps:Caption = hWinKitWidgetHandle:LABEL .
                {Consultingwerk/foreach.i Infragistics.Win.UltraWinToolbars.ToolBase oInstance in oTool:SharedProps:ToolInstances }
                oInstance:InstanceProps:Caption = hWinKitWidgetHandle:LABEL .
            END.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Show-MF-Message s-object
PROCEDURE Show-MF-Message :
/*------------------------------------------------------------------------------
  Purpose:     Show or Clear Misc Fields Message
  Parameters:  ip-notes
  Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-misc-flds AS LOGICAL NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            mf-message:SENSITIVE = YES
            mf-message:SCREEN-VALUE = "UDF Data"
                                    + IF ip-misc-flds THEN " Exists"
                                      ELSE ""
            .
        IF Consultingwerk.WindowIntegrationKit.WinKitSettings:WinKitActive EQ TRUE THEN DO:
             IF ip-misc-flds THEN
             {methods/setButton.i btnUDF "UDF" 16}
             ELSE
             {methods/setButton.i btnUDF "No UDF" 16}
             RUN pRefreshRibbonButtons.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Show-Notes-Message s-object
PROCEDURE Show-Notes-Message :
/*------------------------------------------------------------------------------
  Purpose:     Show or Clear Notes Message
  Parameters:  ip-notes
  Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-notes AS LOGICAL NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN
      notes-message:HIDDEN = NOT ip-notes
      notes-message:SCREEN-VALUE = IF ip-notes THEN "Notes Exist " ELSE ""
      .
    IF Consultingwerk.WindowIntegrationKit.WinKitSettings:WinKitActive EQ TRUE THEN DO:
         IF ip-notes THEN
         {methods/setButton.i btnNote "Note" 16}
         ELSE
         {methods/setButton.i btnNote "No Note" 16}
         RUN pRefreshRibbonButtons.
    END.
  END.

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

