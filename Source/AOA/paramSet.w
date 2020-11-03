&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS s-object 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File: paramSet.w

  Description: from SMART.W - Template for basic SmartObject

  Author: Ron Stark
  Created: 2.24.2019

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

&Scoped-define prgmName paramSet.

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE cCompany    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMode       AS CHARACTER NO-UNDO.
DEFINE VARIABLE char-hdl    AS CHARACTER NO-UNDO.
DEFINE VARIABLE hParamBldr  AS HANDLE    NO-UNDO.
DEFINE VARIABLE iParamSetID AS INTEGER   NO-UNDO.
DEFINE VARIABLE lSortMove   AS LOGICAL   NO-UNDO INITIAL YES.
DEFINE VARIABLE pHandle     AS HANDLE    NO-UNDO.

{methods/defines/sortByDefs.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartObject
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME dynParamSetBrowse

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES dynParamSet

/* Definitions for BROWSE dynParamSetBrowse                             */
&Scoped-define FIELDS-IN-QUERY-dynParamSetBrowse dynParamSet.paramSetID dynParamSet.setName dynParamSet.setTitle dynParamSet.paramSetType   
&Scoped-define ENABLED-FIELDS-IN-QUERY-dynParamSetBrowse   
&Scoped-define SELF-NAME dynParamSetBrowse
&Scoped-define QUERY-STRING-dynParamSetBrowse FOR EACH dynParamSet WHERE STRING(dynParamSet.paramSetID) + "|" + dynParamSet.setName + "|" + dynParamSet.setTitle MATCHES "*" + searchBar + "*"  ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-dynParamSetBrowse OPEN QUERY {&SELF-NAME} FOR EACH dynParamSet WHERE STRING(dynParamSet.paramSetID) + "|" + dynParamSet.setName + "|" + dynParamSet.setTitle MATCHES "*" + searchBar + "*"  ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-dynParamSetBrowse dynParamSet
&Scoped-define FIRST-TABLE-IN-QUERY-dynParamSetBrowse dynParamSet


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-dynParamSetBrowse}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS searchBar dynParamSetBrowse ~
btnRestoreDefaults btnSortMove 
&Scoped-Define DISPLAYED-OBJECTS searchBar 

/* Custom List Definitions                                              */
/* transPanel,transInit,transUpdate,displayFields,enabledFields,List-6  */
&Scoped-define transPanel RECT-SETBUILDER btnSetBuilder btnUpdate btnCancel ~
btnAdd btnCopy btnDelete btnReset 
&Scoped-define transInit btnSetBuilder btnUpdate btnAdd btnCopy btnDelete 
&Scoped-define transUpdate btnUpdate btnCancel btnReset 
&Scoped-define displayFields RECT-SETBUILDER dynParamSet.paramSetID ~
dynParamSet.setName dynParamSet.paramSetType dynParamSet.setWidth ~
dynParamSet.setHeight dynParamSet.setRectangle dynParamSet.setTitle ~
dynParamSet.paramPrompt 
&Scoped-define enabledFields dynParamSet.setName dynParamSet.paramSetType ~
dynParamSet.setWidth dynParamSet.setHeight dynParamSet.setRectangle ~
dynParamSet.setTitle dynParamSet.paramPrompt 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnRestoreDefaults 
     IMAGE-UP FILE "Graphics/16x16/rename.jpg":U NO-FOCUS FLAT-BUTTON
     LABEL "Defaults" 
     SIZE 4 BY .95 TOOLTIP "Restore Defaults".

DEFINE BUTTON btnSortMove 
     IMAGE-UP FILE "Graphics/16x16/sort_up_down2.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Sort/Move" 
     SIZE 4 BY .95 TOOLTIP "Toggle Sort/Move Columns".

DEFINE VARIABLE searchBar AS CHARACTER FORMAT "X(256)":U 
     LABEL "Search" 
     VIEW-AS FILL-IN 
     SIZE 58 BY 1 TOOLTIP "Search Bar" NO-UNDO.

DEFINE BUTTON btnAdd 
     IMAGE-UP FILE "Graphics/32x32/navigate_plus.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/navigate_plus_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Add" 
     SIZE 8 BY 1.91 TOOLTIP "Add".

DEFINE BUTTON btnCancel 
     IMAGE-UP FILE "Graphics/32x32/navigate_cross.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/navigate_cross_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Cancel" 
     SIZE 8 BY 1.91 TOOLTIP "Cancel".

DEFINE BUTTON btnCopy 
     IMAGE-UP FILE "Graphics/32x32/element_copy.ico":U
     IMAGE-INSENSITIVE FILE "Graphics\32x32\form_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Copy" 
     SIZE 8 BY 1.91 TOOLTIP "Copy".

DEFINE BUTTON btnDelete 
     IMAGE-UP FILE "Graphics/32x32/navigate_minus.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/navigate_minus_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Delete" 
     SIZE 8 BY 1.91 TOOLTIP "Delete".

DEFINE BUTTON btnReset 
     IMAGE-UP FILE "Graphics/32x32/undo_32.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/undo_32_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Reset" 
     SIZE 8 BY 1.91 TOOLTIP "Reset".

DEFINE BUTTON btnSetBuilder 
     IMAGE-UP FILE "Graphics/32x32/window_dialog.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/window_dialog_disabled.ico":U
     LABEL "Parameter Set Builder" 
     SIZE 8 BY 1.91 TOOLTIP "Parameter Set Builder".

DEFINE BUTTON btnUpdate 
     IMAGE-UP FILE "Graphics/32x32/pencil.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/pencil_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Update" 
     SIZE 8 BY 1.91 TOOLTIP "Update/Save".

DEFINE RECTANGLE RECT-PANEL
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 49.6 BY 2.38
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-SETBUILDER
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 9.8 BY 2.38.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY dynParamSetBrowse FOR 
      dynParamSet SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE dynParamSetBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS dynParamSetBrowse s-object _FREEFORM
  QUERY dynParamSetBrowse DISPLAY
dynParamSet.setName LABEL-BGCOLOR 14
dynParamSet.paramSetID LABEL-BGCOLOR 14
dynParamSet.setTitle LABEL-BGCOLOR 14
dynParamSet.paramSetType
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 75 BY 25.95
         TITLE "Dynamic Parameter Sets".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     searchBar AT ROW 1 COL 16 COLON-ALIGNED HELP
          "Search" WIDGET-ID 6
     dynParamSetBrowse AT ROW 1.95 COL 1 WIDGET-ID 200
     btnRestoreDefaults AT ROW 1 COL 1 HELP
          "Restore Defaults" WIDGET-ID 42
     btnSortMove AT ROW 1 COL 5 HELP
          "Toggle Sort/Move Columns" WIDGET-ID 48
     SPACE(150.00) SKIP(26.00)
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 15 FGCOLOR 1  WIDGET-ID 100.

DEFINE FRAME viewFrame
     btnSetBuilder AT ROW 1.48 COL 74 HELP
          "Parameter Set Builder" WIDGET-ID 286
     btnUpdate AT ROW 11.71 COL 20 HELP
          "Update/Save" WIDGET-ID 128
     dynParamSet.paramSetID AT ROW 1.71 COL 33 COLON-ALIGNED WIDGET-ID 132
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
          BGCOLOR 15 
     dynParamSet.setName AT ROW 2.91 COL 33 COLON-ALIGNED WIDGET-ID 136
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 
     dynParamSet.paramSetType AT ROW 4.1 COL 33 COLON-ALIGNED WIDGET-ID 144
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEMS "System","User" 
          DROP-DOWN-LIST
          SIZE 16 BY 1
     dynParamSet.setWidth AT ROW 5.29 COL 33 COLON-ALIGNED WIDGET-ID 140
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     dynParamSet.setHeight AT ROW 6.48 COL 33 COLON-ALIGNED WIDGET-ID 134
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     dynParamSet.setRectangle AT ROW 7.67 COL 35 WIDGET-ID 142
          VIEW-AS TOGGLE-BOX
          SIZE 13.2 BY 1
     dynParamSet.setTitle AT ROW 8.86 COL 33 COLON-ALIGNED WIDGET-ID 138
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
          BGCOLOR 15 
     dynParamSet.paramPrompt AT ROW 10.05 COL 35 WIDGET-ID 146
          VIEW-AS TOGGLE-BOX
          SIZE 13.2 BY 1
     btnCancel AT ROW 11.71 COL 60 HELP
          "Cancel" WIDGET-ID 120
     btnAdd AT ROW 11.71 COL 28 HELP
          "Add" WIDGET-ID 118
     btnCopy AT ROW 11.71 COL 36 HELP
          "Copy" WIDGET-ID 122
     btnDelete AT ROW 11.71 COL 44 HELP
          "Delete" WIDGET-ID 124
     btnReset AT ROW 11.71 COL 52 HELP
          "Reset" WIDGET-ID 126
     RECT-PANEL AT ROW 11.48 COL 19 WIDGET-ID 130
     RECT-SETBUILDER AT ROW 1.24 COL 73 WIDGET-ID 284
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 76 ROW 1.95
         SIZE 83 BY 26
         FGCOLOR 1 
         TITLE "View" WIDGET-ID 1500.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartObject
   Allow: Basic,DB-Fields
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
         HEIGHT             = 26.95
         WIDTH              = 158.
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
ASSIGN FRAME viewFrame:FRAME = FRAME F-Main:HANDLE.

/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME viewFrame:MOVE-AFTER-TAB-ITEM (dynParamSetBrowse:HANDLE IN FRAME F-Main)
/* END-ASSIGN-TABS */.

/* BROWSE-TAB dynParamSetBrowse searchBar F-Main */
ASSIGN 
       FRAME F-Main:HIDDEN           = TRUE
       FRAME F-Main:HEIGHT           = 26.95
       FRAME F-Main:WIDTH            = 158.

ASSIGN 
       dynParamSetBrowse:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

/* SETTINGS FOR FRAME viewFrame
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME viewFrame:MOVABLE          = TRUE.

/* SETTINGS FOR BUTTON btnAdd IN FRAME viewFrame
   1 2                                                                  */
/* SETTINGS FOR BUTTON btnCancel IN FRAME viewFrame
   NO-ENABLE 1 3                                                        */
/* SETTINGS FOR BUTTON btnCopy IN FRAME viewFrame
   1 2                                                                  */
/* SETTINGS FOR BUTTON btnDelete IN FRAME viewFrame
   1 2                                                                  */
/* SETTINGS FOR BUTTON btnReset IN FRAME viewFrame
   NO-ENABLE 1 3                                                        */
/* SETTINGS FOR BUTTON btnSetBuilder IN FRAME viewFrame
   1 2                                                                  */
/* SETTINGS FOR BUTTON btnUpdate IN FRAME viewFrame
   1 2 3                                                                */
/* SETTINGS FOR TOGGLE-BOX dynParamSet.paramPrompt IN FRAME viewFrame
   4 5                                                                  */
/* SETTINGS FOR FILL-IN dynParamSet.paramSetID IN FRAME viewFrame
   4                                                                    */
/* SETTINGS FOR COMBO-BOX dynParamSet.paramSetType IN FRAME viewFrame
   4 5                                                                  */
/* SETTINGS FOR RECTANGLE RECT-PANEL IN FRAME viewFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-SETBUILDER IN FRAME viewFrame
   NO-ENABLE 1 4                                                        */
/* SETTINGS FOR FILL-IN dynParamSet.setHeight IN FRAME viewFrame
   4 5                                                                  */
/* SETTINGS FOR FILL-IN dynParamSet.setName IN FRAME viewFrame
   4 5                                                                  */
/* SETTINGS FOR TOGGLE-BOX dynParamSet.setRectangle IN FRAME viewFrame
   4 5                                                                  */
/* SETTINGS FOR FILL-IN dynParamSet.setTitle IN FRAME viewFrame
   4 5                                                                  */
/* SETTINGS FOR FILL-IN dynParamSet.setWidth IN FRAME viewFrame
   4 5                                                                  */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE dynParamSetBrowse
/* Query rebuild information for BROWSE dynParamSetBrowse
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH dynParamSet
WHERE STRING(dynParamSet.paramSetID) + "|" +
dynParamSet.setName + "|" +
dynParamSet.setTitle MATCHES "*" + searchBar + "*"
 ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE dynParamSetBrowse */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME viewFrame
/* Query rebuild information for FRAME viewFrame
     _Query            is NOT OPENED
*/  /* FRAME viewFrame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define FRAME-NAME viewFrame
&Scoped-define SELF-NAME btnAdd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAdd s-object
ON CHOOSE OF btnAdd IN FRAME viewFrame /* Add */
DO:
    RUN pCRUD (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel s-object
ON CHOOSE OF btnCancel IN FRAME viewFrame /* Cancel */
DO:
    RUN pCRUD (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCopy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCopy s-object
ON CHOOSE OF btnCopy IN FRAME viewFrame /* Copy */
DO:
    RUN pCRUD (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDelete s-object
ON CHOOSE OF btnDelete IN FRAME viewFrame /* Delete */
DO:
    RUN pCRUD (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnReset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnReset s-object
ON CHOOSE OF btnReset IN FRAME viewFrame /* Reset */
DO:
    RUN pCRUD (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME btnRestoreDefaults
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRestoreDefaults s-object
ON CHOOSE OF btnRestoreDefaults IN FRAME F-Main /* Defaults */
DO:
    RUN pGetSettings ("_default").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME viewFrame
&Scoped-define SELF-NAME btnSetBuilder
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSetBuilder s-object
ON CHOOSE OF btnSetBuilder IN FRAME viewFrame /* Parameter Set Builder */
DO:
    IF NOT VALID-HANDLE(hParamBldr) THEN
    RUN AOA/paramSetBldr.w PERSISTENT SET hParamBldr (
        THIS-PROCEDURE,
        "Set",
        dynParamSet.paramSetID
        ).
    ELSE
    RUN pReset IN hParamBldr (dynParamSet.paramSetID).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME btnSortMove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSortMove s-object
ON CHOOSE OF btnSortMove IN FRAME F-Main /* Sort/Move */
DO:
    ASSIGN
        BROWSE {&BROWSE-NAME}:COLUMN-MOVABLE = lSortMove
        lSortMove = NOT lSortMove
        .
    SELF:LOAD-IMAGE("Graphics/16x16/"
        + IF lSortMove THEN "sort_up_down2.gif"
          ELSE "left_right_arrows.gif")
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME viewFrame
&Scoped-define SELF-NAME btnUpdate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUpdate s-object
ON CHOOSE OF btnUpdate IN FRAME viewFrame /* Update */
DO:
    RUN pCRUD (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME dynParamSetBrowse
&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME dynParamSetBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dynParamSetBrowse s-object
ON START-SEARCH OF dynParamSetBrowse IN FRAME F-Main /* Dynamic Parameter Sets */
DO:
    IF {&BROWSE-NAME}:CURRENT-COLUMN:NAME NE ? THEN DO:
        cColumnLabel = BROWSE {&BROWSE-NAME}:CURRENT-COLUMN:NAME.
        IF cColumnLabel EQ cSaveLabel THEN
        lAscending = NOT lAscending.
        cSaveLabel = cColumnLabel.
        RUN pReopenBrowse.
        APPLY "VALUE-CHANGED":U TO {&BROWSE-NAME}.
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dynParamSetBrowse s-object
ON VALUE-CHANGED OF dynParamSetBrowse IN FRAME F-Main /* Dynamic Parameter Sets */
DO:
    {methods/run_link.i "CONTAINER" "pSetParamSetID" "(dynParamSet.paramSetID)"}
    {methods/run_link.i "CONTAINER" "pGetParamSetDtl"}
    RUN pDisplay.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME searchBar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL searchBar s-object
ON VALUE-CHANGED OF searchBar IN FRAME F-Main /* Search */
DO:
    ASSIGN {&SELF-NAME}.
    {&OPEN-QUERY-{&BROWSE-NAME}}
    APPLY "VALUE-CHANGED":U TO BROWSE {&BROWSE-NAME}.
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

{methods/sortByProc.i "pByParamSetID" "dynParamSet.paramSetID"}
{methods/sortByProc.i "pBySetName" "dynParamSet.setName"}
{methods/sortByProc.i "pBySetTitle" "dynParamSet.setTitle"}

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
  HIDE FRAME viewFrame.
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

  /* Code placed here will execute PRIOR to standard behavior. */
  {methods/run_link.i "CONTAINER" "pGetCompany" "(OUTPUT cCompany)"}
  RUN pGetSettings (USERID("ASI")).

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  cColumnLabel = "setName".
  RUN pReopenBrowse.
  IF AVAILABLE dynParamSet THEN
  {methods/run_link.i "CONTAINER" "pSetParamSetID" "(dynParamSet.paramSetID)"}
  ENABLE {&transInit} WITH FRAME viewFrame.
  RUN pDisplay.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAssign s-object 
PROCEDURE pAssign :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO TRANSACTION WITH FRAME viewFrame:
        FIND CURRENT dynParamSet EXCLUSIVE-LOCK.
        ASSIGN
            dynParamSet.paramSetID
            {&enabledFields}
            .
        FIND CURRENT dynParamSet NO-LOCK.
    END. /* with frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pClearView s-object 
PROCEDURE pClearView :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hWidget AS HANDLE NO-UNDO.
    
    ASSIGN
        hWidget = FRAME viewFrame:HANDLE
        hWidget = hWidget:FIRST-CHILD
        hWidget = hWidget:FIRST-CHILD
        .
    DO WHILE VALID-HANDLE(hWidget):
        IF hWidget:TYPE NE "BUTTON" AND
           hWidget:SELECTABLE EQ NO AND 
           hWidget:SENSITIVE THEN
        hWidget:SCREEN-VALUE = if hWidget:TYPE EQ "TOGGLE-BOX" THEN "YES" ELSE "".
        hWidget = hWidget:NEXT-SIBLING.
    END. /* do while */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCRUD s-object 
PROCEDURE pCRUD :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphMode AS HANDLE NO-UNDO.

    DEFINE VARIABLE cMsg      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iNextID   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lContinue AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE rRowID    AS ROWID     NO-UNDO.
    
    DEFINE BUFFER bDynParamSetDtl FOR dynParamSetDtl.
    
    DO WITH FRAME viewFrame:
        CASE iphMode:LABEL:
            WHEN "Add" OR WHEN "Copy" OR WHEN "Update" THEN DO:
                DISABLE {&transPanel}.
                ENABLE {&transUpdate} {&enabledFields}.
                btnUpdate:LOAD-IMAGE("Graphics\32x32\Save_As.ico").
                IF AVAILABLE dynParamSet THEN
                iParamSetID = dynParamSet.paramSetID.
                IF iphMode:LABEL EQ "Add" THEN DO:
                    RUN pClearView.
                    ASSIGN
                        dynParamSet.paramSetID:SCREEN-VALUE   = ""
                        dynParamSet.paramPrompt:SCREEN-VALUE  = "yes"
                        dynParamSet.setRectangle:SCREEN-VALUE = "yes"
                        dynParamSet.paramSetType:SCREEN-VALUE = "System"
                        dynParamSet.setWidth:SCREEN-VALUE     = "119"
                        dynParamSet.setHeight:SCREEN-VALUE    = "21.43"
                        .
                    DISABLE btnReset.
                END. /* add */
                ASSIGN
                    FRAME viewFrame:TITLE = iphMode:LABEL
                    btnUpdate:LABEL = "Save"
                    .
            END. /* add copy update */
            WHEN "Cancel" OR WHEN "Save" THEN DO:
                IF iphMode:LABEL EQ "Save" THEN DO:
                    IF cMode EQ "Add" OR cMode EQ "Copy" THEN DO TRANSACTION:
                        DO WHILE TRUE:
                            iNextID = NEXT-VALUE(paramSetID).
                            IF dynParamSet.paramSetType:SCREEN-VALUE NE "System" THEN
                            iNextID = iNextID + 5000.
                            IF NOT CAN-FIND(FIRST dynParamSet
                                WHERE dynParamSet.paramSetID EQ iNextID) THEN
                            LEAVE.
                        END. /* do while */
                        CREATE dynParamSet.
                        ASSIGN
                            dynParamSet.paramSetID:SCREEN-VALUE = STRING(iNextID)
                            rRowID = ROWID(dynParamSet)
                            .
                    END. /* if add/copy */
                    RUN pAssign.
                    IF cMode EQ "Add" OR cMode EQ "Copy" THEN DO:
                        RUN pReopenBrowse.
                        REPOSITION {&BROWSE-NAME} TO ROWID rRowID.
                        IF cMode EQ "Copy" THEN DO TRANSACTION:
                            FOR EACH dynParamSetDtl NO-LOCK
                                WHERE dynParamSetDtl.paramSetID EQ iParamSetID
                                :
                                CREATE bDynParamSetDtl.
                                BUFFER-COPY dynParamSetDtl EXCEPT paramSetID rec_key TO bDynParamSetDtl
                                    ASSIGN bDynParamSetDtl.paramSetID = dynParamSet.paramSetID.
                            END. /* each dynparamsetdtl */
                        END. /* if copy */
                    END. /* if add/copy */
                    ELSE
                    BROWSE {&BROWSE-NAME}:REFRESH().
                END. /* save */
                DISABLE {&transPanel} {&enabledFields}.
                ENABLE {&transInit}.
                btnUpdate:LOAD-IMAGE("Graphics\32x32\Pencil.ico").
                ASSIGN
                    FRAME viewFrame:TITLE = "View"
                    btnUpdate:LABEL = "Update"
                    .
                APPLY "VALUE-CHANGED":U TO BROWSE {&BROWSE-NAME}.
            END. /* cancel save */
            WHEN "Delete" THEN DO:
                IF AVAILABLE dynParamSet THEN DO:
                    IF CAN-FIND(FIRST dynSubjectParamSet
                        WHERE dynSubjectParamSet.paramSetID EQ dynParamSet.paramSetID) THEN
                    cMsg = "Parameter Set linked to Subject and will be Removed!" + CHR(10) + CHR(10).
                    MESSAGE
                        cMsg
                        "Delete Currently Selected Record?"
                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                    UPDATE lContinue.
                    IF lContinue THEN DO TRANSACTION:
                        cMode = iphMode:LABEL.
                        FOR EACH dynSubjectParamSet EXCLUSIVE-LOCK
                            WHERE dynSubjectParamSet.paramSetID EQ dynParamSet.paramSetID
                            :
                            DELETE dynSubjectParamSet.
                        END. /* each dynsubjectparamset */
                        FOR EACH dynParamSetDtl EXCLUSIVE-LOCK
                            WHERE dynParamSetDtl.paramSetID EQ dynParamSet.paramSetID
                            :
                            DELETE dynParamSetDtl.
                        END. /* each dynparamsetdtl */
                        FIND CURRENT dynParamSet EXCLUSIVE-LOCK.
                        DELETE dynParamSet.
                        BROWSE {&BROWSE-NAME}:DELETE-CURRENT-ROW().
                    END. /* if lcontinue */
                    IF AVAILABLE dynParamSet THEN
                    BROWSE {&BROWSE-NAME}:REFRESH().
                    RUN pDisplay.
                END. /* if avail */
            END. /* delete */
            WHEN "Reset" THEN DO:
                RUN pDisplay.
                DISABLE {&transPanel}.
                ENABLE {&transUpdate}.
            END. /* reset */
        END CASE. /* ipcmode:label */
        IF dynParamSet.setName:SENSITIVE THEN
        APPLY "ENTRY":U TO dynParamSet.setName.
        ELSE
        APPLY "ENTRY":U TO BROWSE {&BROWSE-NAME}.
        /* save the mode for when logic returns to this procedure */
        cMode = iphMode:LABEL.
    END. /* do frame */    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDisplay s-object 
PROCEDURE pDisplay :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME viewFrame:
        IF AVAILABLE dynParamSet THEN DO:
            DISPLAY {&displayFields}.
            ENABLE {&transInit}.
        END. /* if avail */
        ELSE DO:
            RUN pClearView.
            DISABLE {&transPanel}.
            ENABLE btnAdd.
        END. /* else */
    END. /* with frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetParamSetID s-object 
PROCEDURE pGetParamSetID :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {methods/run_link.i "CONTAINER" "pGetParamSetDtl"}

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

    DEFINE VARIABLE hColumn AS HANDLE  NO-UNDO.
    DEFINE VARIABLE idx     AS INTEGER NO-UNDO.
    DEFINE VARIABLE jdx     AS INTEGER NO-UNDO.
    DEFINE VARIABLE kdx     AS INTEGER NO-UNDO.
    
    IF NOT CAN-FIND(FIRST user-print
                    WHERE user-print.company    EQ cCompany
                      AND user-print.program-id EQ "{&programID}"
                      AND user-print.user-id    EQ "_default") THEN
    RUN pSaveSettings ("_default").
    FIND FIRST user-print NO-LOCK
         WHERE user-print.company    EQ cCompany
           AND user-print.program-id EQ "{&program-id}"
           AND user-print.user-id    EQ ipcUserID
         NO-ERROR.
    IF AVAILABLE user-print THEN DO:
        DO idx = 1 TO EXTENT(user-print.field-name):
            IF user-print.field-name[idx] EQ "" THEN LEAVE.
            /* set browse column width, hidden & order */
            DO kdx = 1 TO BROWSE {&BROWSE-NAME}:NUM-COLUMNS:
                IF user-print.field-name[idx] EQ BROWSE {&BROWSE-NAME}:GET-BROWSE-COLUMN(kdx):NAME THEN DO:
                    ASSIGN
                        jdx           = idx - 4
                        hColumn       = BROWSE {&BROWSE-NAME}:GET-BROWSE-COLUMN(jdx)
                        hColumn:WIDTH = DECIMAL(user-print.field-value[idx])
                        .
                    BROWSE {&BROWSE-NAME}:MOVE-COLUMN(kdx,jdx).
                END. /* if name */
            END. /* do kdx */
        END. /* do idx */
    END. /* if avail */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pNavPanel s-object 
PROCEDURE pNavPanel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphNavPanel AS HANDLE NO-UNDO.
    
    CASE iphNavPanel:LABEL:
        WHEN "First" THEN
        APPLY "HOME":U TO BROWSE dynParamSetBrowse.
        WHEN "Previous" THEN
        BROWSE dynParamSetBrowse:SELECT-PREV-ROW().
        WHEN "Next" THEN
        BROWSE dynParamSetBrowse:SELECT-NEXT-ROW().
        WHEN "Last" THEN
        APPLY "END":U TO BROWSE dynParamSetBrowse.
    END CASE.
    IF AVAILABLE dynParamSet THEN
    APPLY "VALUE-CHANGED":U TO BROWSE dynParamSetBrowse.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRefresh s-object 
PROCEDURE pRefresh :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReopenBrowse s-object 
PROCEDURE pReopenBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    CASE cColumnLabel:
        WHEN "paramSetID" THEN
        RUN pByParamSetID.
        WHEN "setName" THEN
        RUN pBySetName.
        WHEN "setTitle" THEN
        RUN pBySetTitle.
        OTHERWISE
        {&OPEN-QUERY-{&BROWSE-NAME}}
    END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSaveSettings s-object 
PROCEDURE pSaveSettings :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcUserID AS CHARACTER NO-UNDO.

    DEFINE VARIABLE hColumn AS HANDLE  NO-UNDO.
    DEFINE VARIABLE idx     AS INTEGER NO-UNDO.
    DEFINE VARIABLE jdx     AS INTEGER NO-UNDO.
    
    FIND FIRST user-print EXCLUSIVE-LOCK
         WHERE user-print.company    EQ cCompany
           AND user-print.program-id EQ "{&program-id}"
           AND user-print.user-id    EQ ipcUserID
         NO-ERROR.
    IF NOT AVAILABLE user-print THEN DO:
        CREATE user-print.
        ASSIGN
            user-print.company    = cCompany
            user-print.program-id = "{&program-id}"
            user-print.user-id    = ipcUserID
            user-print.last-date  = TODAY
            user-print.last-time  = TIME
            .
    END. /* not avail */
    ASSIGN
        user-print.next-date   = TODAY
        user-print.next-time   = TIME
        user-print.field-name  = ""
        user-print.field-value = ""
        user-print.field-label = ""
        .
    /* save browse column order and width */
    DO jdx = 1 TO BROWSE {&BROWSE-NAME}:NUM-COLUMNS:
        ASSIGN
            idx                         = idx + 1
            hColumn                     = BROWSE {&BROWSE-NAME}:GET-BROWSE-COLUMN(jdx)
            user-print.field-label[idx] = "BrowseColumn"
            user-print.field-name[idx]  = hColumn:NAME
            user-print.field-value[idx] = STRING(MAX(hColumn:WIDTH, .2 /*BROWSE taskBrowse:MIN-COLUMN-WIDTH-CHARS*/ ))
            .
    END. /* do jdx */
    FIND CURRENT user-print NO-LOCK.

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
        FRAME {&FRAME-NAME}:HEIGHT     = ipdHeight
        FRAME {&FRAME-NAME}:WIDTH      = ipdWidth
        BROWSE {&BROWSE-NAME}:HEIGHT   = ipdHeight - BROWSE {&BROWSE-NAME}:ROW + 1
        FRAME viewFrame:HEIGHT         = ipdHeight - FRAME viewFrame:ROW + 1
        FRAME viewFrame:WIDTH          = ipdWidth  - FRAME viewFrame:COL + 1
        FRAME viewFrame:VIRTUAL-HEIGHT = FRAME viewFrame:HEIGHT
        FRAME viewFrame:VIRTUAL-WIDTH  = FRAME viewFrame:WIDTH
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

