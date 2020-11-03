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

  File: param.w

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

&Scoped-define prgmName param.

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE cCompany        AS CHARACTER NO-UNDO.
DEFINE VARIABLE char-hdl        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMode           AS CHARACTER NO-UNDO.
DEFINE VARIABLE searchBar       AS CHARACTER NO-UNDO.
DEFINE VARIABLE hDynDescripProc AS HANDLE    NO-UNDO.
DEFINE VARIABLE hDynInitProc    AS HANDLE    NO-UNDO.
DEFINE VARIABLE hDynValProc     AS HANDLE    NO-UNDO.
DEFINE VARIABLE hParamBldr      AS HANDLE    NO-UNDO.
DEFINE VARIABLE iParamID        AS INTEGER   NO-UNDO.
DEFINE VARIABLE lMoveColumn     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lSortMove       AS LOGICAL   NO-UNDO INITIAL YES.
DEFINE VARIABLE pHandle         AS HANDLE    NO-UNDO.

{methods/defines/sortByDefs.i}

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
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME dynParamBrowse

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES dynParam

/* Definitions for BROWSE dynParamBrowse                                */
&Scoped-define FIELDS-IN-QUERY-dynParamBrowse dynParam.paramID dynParam.paramName dynParam.paramLabel dynParam.viewAs dynParam.dataType dynParam.paramFormat dynParam.paramType   
&Scoped-define ENABLED-FIELDS-IN-QUERY-dynParamBrowse   
&Scoped-define SELF-NAME dynParamBrowse
&Scoped-define QUERY-STRING-dynParamBrowse FOR EACH dynParam WHERE STRING(dynParam.paramID) + "|" + dynParam.paramName + "|" + dynParam.paramType + "|" + dynParam.paramLabel + "|" + dynParam.dataType + "|" + dynParam.viewAs MATCHES "*" + searchBar + "*"  ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-dynParamBrowse OPEN QUERY {&SELF-NAME} FOR EACH dynParam WHERE STRING(dynParam.paramID) + "|" + dynParam.paramName + "|" + dynParam.paramType + "|" + dynParam.paramLabel + "|" + dynParam.dataType + "|" + dynParam.viewAs MATCHES "*" + searchBar + "*"  ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-dynParamBrowse dynParam
&Scoped-define FIRST-TABLE-IN-QUERY-dynParamBrowse dynParam


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-dynParamBrowse}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS dynParamBrowse 

/* Custom List Definitions                                              */
/* widgetPanel,List-2,List3,displayFields,enabledFields,List-6          */
&Scoped-define widgetPanel btnCombo-Box btnEditor btnFill-In btnRadio-Set ~
btnSelection-List btnToggle-Box 
&Scoped-define List-2 btnCombo-Box btnEditor btnFill-In btnRadio-Set ~
btnSelection-List btnToggle-Box 
&Scoped-define displayFields dynParam.paramID dynParam.paramName ~
dynParam.paramLabel dynParam.paramType dynParam.action ~
dynParam.actionParamName dynParam.dataType dynParam.paramFormat ~
dynParam.viewAs dynParam.innerLines dynParam.paramWidth ~
dynParam.paramHeight dynParam.initialValue dynParam.initialItems ~
dynParam.initializeProc dynParam.validateProc dynParam.descriptionProc 
&Scoped-define enabledFields dynParam.paramName dynParam.paramLabel ~
dynParam.paramType dynParam.action dynParam.actionParamName ~
dynParam.dataType dynParam.paramFormat dynParam.viewAs dynParam.innerLines ~
dynParam.paramWidth dynParam.paramHeight dynParam.initialValue ~
dynParam.initialItems dynParam.initializeProc dynParam.validateProc ~
dynParam.descriptionProc 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCombo-Box  NO-FOCUS FLAT-BUTTON
     LABEL "Combo-Box" 
     SIZE 6.4 BY 1.52 TOOLTIP "Create Combo Box"
     FONT 4.

DEFINE BUTTON btnEditor  NO-FOCUS FLAT-BUTTON
     LABEL "Editor" 
     SIZE 6.4 BY 1.52 TOOLTIP "Create Editor"
     FONT 4.

DEFINE BUTTON btnFill-In  NO-FOCUS FLAT-BUTTON
     LABEL "Fill-In" 
     SIZE 6.4 BY 1.52 TOOLTIP "Create Fill In"
     FONT 4.

DEFINE BUTTON btnRadio-Set  NO-FOCUS FLAT-BUTTON
     LABEL "Radio-Set" 
     SIZE 6.4 BY 1.52 TOOLTIP "Create Radio Set"
     FONT 4.

DEFINE BUTTON btnSelection-List  NO-FOCUS FLAT-BUTTON
     LABEL "Selection-List" 
     SIZE 6.4 BY 1.52 TOOLTIP "Create Selection List"
     FONT 4.

DEFINE BUTTON btnToggle-Box  NO-FOCUS FLAT-BUTTON
     LABEL "Toggle-Box" 
     SIZE 6.4 BY 1.52 TOOLTIP "Create Toggle Box"
     FONT 4.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 44 BY 1.91.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY dynParamBrowse FOR 
      dynParam SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE dynParamBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS dynParamBrowse s-object _FREEFORM
  QUERY dynParamBrowse DISPLAY
      dynParam.paramID LABEL-BGCOLOR 22
dynParam.paramName LABEL-BGCOLOR 22
dynParam.paramLabel LABEL-BGCOLOR 22
dynParam.viewAs LABEL-BGCOLOR 22
dynParam.dataType LABEL-BGCOLOR 22
dynParam.paramFormat
dynParam.paramType LABEL-BGCOLOR 22
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 75 BY 24.57
         TITLE "Dynamic Parameters".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     dynParamBrowse AT ROW 1 COL 1 WIDGET-ID 200
     SPACE(83.00) SKIP(0.00)
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 15 FGCOLOR 1  WIDGET-ID 100.

DEFINE FRAME viewFrame
     btnCombo-Box AT ROW 22.19 COL 26 HELP
          "Create New COMBO-BOX" WIDGET-ID 190
     dynParam.paramID AT ROW 1.24 COL 20 COLON-ALIGNED WIDGET-ID 166
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
          BGCOLOR 15 
     btnEditor AT ROW 22.19 COL 33 HELP
          "Create New EDITOR" WIDGET-ID 192
     dynParam.paramName AT ROW 2.43 COL 20 COLON-ALIGNED WIDGET-ID 170 FORMAT "x(40)"
          VIEW-AS FILL-IN 
          SIZE 50 BY 1
          BGCOLOR 15 
     dynParam.paramLabel AT ROW 3.62 COL 20 COLON-ALIGNED WIDGET-ID 168
          VIEW-AS FILL-IN 
          SIZE 50 BY 1
          BGCOLOR 15 
     dynParam.paramType AT ROW 5.76 COL 20 COLON-ALIGNED WIDGET-ID 180
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEMS "System","User" 
          DROP-DOWN-LIST
          SIZE 16 BY 1
     dynParam.action AT ROW 5.76 COL 45 NO-LABEL WIDGET-ID 186
          VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
          LIST-ITEMS "NO:DISABLE","NO:ENABLE","NO:LOW","NO:HI","YES:DISABLE","YES:ENABLE","YES:LOW","YES:HI","CALENDAR","DATEPICKLIST","EMAIL","HORIZONTAL","VERTICAL","START DESCRIPTION","END DESCRIPTION","LIST-ITEM-PAIRS" 
          SIZE 27 BY 10.48
     dynParam.actionParamName AT ROW 6.95 COL 20 COLON-ALIGNED WIDGET-ID 302
          LABEL "Action Param Name"
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
          BGCOLOR 15 
     dynParam.dataType AT ROW 8.14 COL 20 COLON-ALIGNED WIDGET-ID 182
          VIEW-AS COMBO-BOX INNER-LINES 6
          LIST-ITEMS "Character","Date","DateTime","Decimal","Integer","Logical" 
          DROP-DOWN-LIST
          SIZE 16 BY 1
     dynParam.paramFormat AT ROW 9.33 COL 20 COLON-ALIGNED WIDGET-ID 162
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
          BGCOLOR 15 
     dynParam.viewAs AT ROW 10.52 COL 20 COLON-ALIGNED WIDGET-ID 184
          VIEW-AS COMBO-BOX INNER-LINES 6
          LIST-ITEMS "Combo-Box","Editor","Fill-In","Radio-Set","Selection-List","Toggle-Box" 
          DROP-DOWN-LIST
          SIZE 22 BY 1
     dynParam.innerLines AT ROW 11.71 COL 20 COLON-ALIGNED WIDGET-ID 160
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
          BGCOLOR 15 
     dynParam.paramWidth AT ROW 12.91 COL 20 COLON-ALIGNED WIDGET-ID 174
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     dynParam.paramHeight AT ROW 14.1 COL 20 COLON-ALIGNED WIDGET-ID 164
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     dynParam.initialValue AT ROW 15.29 COL 20 COLON-ALIGNED WIDGET-ID 158
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
          BGCOLOR 15 
     dynParam.initialItems AT ROW 16.48 COL 20 COLON-ALIGNED WIDGET-ID 154 FORMAT "x(256)"
          VIEW-AS FILL-IN 
          SIZE 61 BY 1
          BGCOLOR 15 
     dynParam.initializeProc AT ROW 17.67 COL 20 COLON-ALIGNED WIDGET-ID 296
          VIEW-AS COMBO-BOX SORT INNER-LINES 100
          LIST-ITEMS "Item 1" 
          DROP-DOWN-LIST
          SIZE 50 BY 1
     dynParam.validateProc AT ROW 18.86 COL 20 COLON-ALIGNED WIDGET-ID 298
          VIEW-AS COMBO-BOX SORT INNER-LINES 100
          LIST-ITEMS "Item 1" 
          DROP-DOWN-LIST
          SIZE 50 BY 1
     dynParam.descriptionProc AT ROW 20.05 COL 20 COLON-ALIGNED WIDGET-ID 300
          LABEL "Descript Procedure"
          VIEW-AS COMBO-BOX SORT INNER-LINES 100
          LIST-ITEMS "Item 1" 
          DROP-DOWN-LIST
          SIZE 50 BY 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 76 ROW 1
         SIZE 83 BY 24.57
         FGCOLOR 1  WIDGET-ID 1500.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME viewFrame
     btnFill-In AT ROW 22.19 COL 40 HELP
          "Create New FILL-IN" WIDGET-ID 194
     btnRadio-Set AT ROW 22.19 COL 47 HELP
          "Create New RADIO-SET" WIDGET-ID 196
     btnSelection-List AT ROW 22.19 COL 54 HELP
          "Create New SELECTION-LIST" WIDGET-ID 198
     btnToggle-Box AT ROW 22.19 COL 61 HELP
          "Create New TOGGLE-BOX" WIDGET-ID 200
     "Action:" VIEW-AS TEXT
          SIZE 7 BY .81 AT ROW 4.81 COL 45 WIDGET-ID 188
     RECT-9 AT ROW 21.95 COL 25 WIDGET-ID 288
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 76 ROW 1
         SIZE 83 BY 24.57
         FGCOLOR 1 
         TITLE "View" WIDGET-ID 1500.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartObject
   Allow: Basic,Browse,DB-Fields
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
         HEIGHT             = 24.62
         WIDTH              = 158.6.
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

ASSIGN XXTABVALXX = FRAME viewFrame:MOVE-AFTER-TAB-ITEM (dynParamBrowse:HANDLE IN FRAME F-Main)
/* END-ASSIGN-TABS */.

/* BROWSE-TAB dynParamBrowse 1 F-Main */
ASSIGN 
       FRAME F-Main:HIDDEN           = TRUE
       FRAME F-Main:HEIGHT           = 24.57
       FRAME F-Main:WIDTH            = 158.

ASSIGN 
       dynParamBrowse:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

/* SETTINGS FOR FRAME viewFrame
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME viewFrame:MOVABLE          = TRUE.

/* SETTINGS FOR SELECTION-LIST dynParam.action IN FRAME viewFrame
   4 5                                                                  */
/* SETTINGS FOR FILL-IN dynParam.actionParamName IN FRAME viewFrame
   4 5 EXP-LABEL                                                        */
/* SETTINGS FOR BUTTON btnCombo-Box IN FRAME viewFrame
   1 2                                                                  */
/* SETTINGS FOR BUTTON btnEditor IN FRAME viewFrame
   1 2                                                                  */
/* SETTINGS FOR BUTTON btnFill-In IN FRAME viewFrame
   1 2                                                                  */
/* SETTINGS FOR BUTTON btnRadio-Set IN FRAME viewFrame
   1 2                                                                  */
/* SETTINGS FOR BUTTON btnSelection-List IN FRAME viewFrame
   1 2                                                                  */
/* SETTINGS FOR BUTTON btnToggle-Box IN FRAME viewFrame
   1 2                                                                  */
/* SETTINGS FOR COMBO-BOX dynParam.dataType IN FRAME viewFrame
   4 5                                                                  */
/* SETTINGS FOR COMBO-BOX dynParam.descriptionProc IN FRAME viewFrame
   4 5 EXP-LABEL                                                        */
/* SETTINGS FOR FILL-IN dynParam.initialItems IN FRAME viewFrame
   4 5 EXP-FORMAT                                                       */
/* SETTINGS FOR COMBO-BOX dynParam.initializeProc IN FRAME viewFrame
   4 5                                                                  */
/* SETTINGS FOR FILL-IN dynParam.initialValue IN FRAME viewFrame
   4 5                                                                  */
/* SETTINGS FOR FILL-IN dynParam.innerLines IN FRAME viewFrame
   4 5                                                                  */
/* SETTINGS FOR FILL-IN dynParam.paramFormat IN FRAME viewFrame
   4 5                                                                  */
/* SETTINGS FOR FILL-IN dynParam.paramHeight IN FRAME viewFrame
   4 5                                                                  */
/* SETTINGS FOR FILL-IN dynParam.paramID IN FRAME viewFrame
   4                                                                    */
/* SETTINGS FOR FILL-IN dynParam.paramLabel IN FRAME viewFrame
   4 5                                                                  */
/* SETTINGS FOR FILL-IN dynParam.paramName IN FRAME viewFrame
   4 5 EXP-FORMAT                                                       */
/* SETTINGS FOR COMBO-BOX dynParam.paramType IN FRAME viewFrame
   4 5                                                                  */
/* SETTINGS FOR FILL-IN dynParam.paramWidth IN FRAME viewFrame
   4 5                                                                  */
/* SETTINGS FOR RECTANGLE RECT-9 IN FRAME viewFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX dynParam.validateProc IN FRAME viewFrame
   4 5                                                                  */
/* SETTINGS FOR COMBO-BOX dynParam.viewAs IN FRAME viewFrame
   4 5                                                                  */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE dynParamBrowse
/* Query rebuild information for BROWSE dynParamBrowse
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH dynParam
WHERE STRING(dynParam.paramID) + "|" +
dynParam.paramName + "|" +
dynParam.paramType + "|" +
dynParam.paramLabel + "|" +
dynParam.dataType + "|" +
dynParam.viewAs MATCHES "*" + searchBar + "*"
 ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE dynParamBrowse */
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
&Scoped-define SELF-NAME btnCombo-Box
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCombo-Box s-object
ON CHOOSE OF btnCombo-Box IN FRAME viewFrame /* Combo-Box */
DO:
    RUN pCreateDynParam (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnEditor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnEditor s-object
ON CHOOSE OF btnEditor IN FRAME viewFrame /* Editor */
DO:
    RUN pCreateDynParam (SELF:LABEL).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFill-In
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFill-In s-object
ON CHOOSE OF btnFill-In IN FRAME viewFrame /* Fill-In */
DO:
    RUN pCreateDynParam (SELF:LABEL).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRadio-Set
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRadio-Set s-object
ON CHOOSE OF btnRadio-Set IN FRAME viewFrame /* Radio-Set */
DO:
    RUN pCreateDynParam (SELF:LABEL).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSelection-List
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSelection-List s-object
ON CHOOSE OF btnSelection-List IN FRAME viewFrame /* Selection-List */
DO:
    RUN pCreateDynParam (SELF:LABEL).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnToggle-Box
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnToggle-Box s-object
ON CHOOSE OF btnToggle-Box IN FRAME viewFrame /* Toggle-Box */
DO:
    RUN pCreateDynParam (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dynParam.dataType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dynParam.dataType s-object
ON VALUE-CHANGED OF dynParam.dataType IN FRAME viewFrame /* Data Type */
DO:
    CASE dynParam.dataType:SCREEN-VALUE:
        WHEN "Character" THEN
        dynParam.paramFormat:SCREEN-VALUE = "x(8)".
        WHEN "Date" THEN
        dynParam.paramFormat:SCREEN-VALUE = "99/99/9999".
        WHEN "DateTime" THEN
        dynParam.paramFormat:SCREEN-VALUE = "99/99/9999 HH:MM:SS.SSS".
        WHEN "Decimal" THEN
        dynParam.paramFormat:SCREEN-VALUE = "->>,>>9.99".
        WHEN "Integer" THEN
        dynParam.paramFormat:SCREEN-VALUE = "->,>>>,>>9".
        WHEN "Logical" THEN
        dynParam.paramFormat:SCREEN-VALUE  = "yes/no".
    END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME dynParamBrowse
&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME dynParamBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dynParamBrowse s-object
ON DEFAULT-ACTION OF dynParamBrowse IN FRAME F-Main /* Dynamic Parameters */
DO:
    RUN pBuilder.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dynParamBrowse s-object
ON START-SEARCH OF dynParamBrowse IN FRAME F-Main /* Dynamic Parameters */
DO:
    &Scoped-define startSearchValueChanged
    {AOA/includes/startSearch.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dynParamBrowse s-object
ON VALUE-CHANGED OF dynParamBrowse IN FRAME F-Main /* Dynamic Parameters */
DO:
    RUN pDisplay.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK s-object 


/* ***************************  Main Block  *************************** */

{methods/template/brwcustom.i}

/* If testing in the UIB, initialize the SmartObject. */  
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{methods/sortByProc.i "pByParamID" "dynParam.paramID"}
{methods/sortByProc.i "pByParamName" "dynParam.paramName"}
{methods/sortByProc.i "pByParamType" "dynParam.paramType"}
{methods/sortByProc.i "pByParamLabel" "dynParam.paramLabel"}
{methods/sortByProc.i "pByDataType" "dynParam.dataType"}
{methods/sortByProc.i "pByViewAs" "dynParam.viewAs"}

{AOA/includes/dynParamProcs.i}

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
  DO WITH FRAME viewFrame:
    btnCombo-Box:LOAD-IMAGE-UP("Graphics/wp_up",1,141,28,28).
    btnEditor:LOAD-IMAGE-UP("Graphics/wp_up",29,113,28,28).
    btnFill-In:LOAD-IMAGE-UP("Graphics/wp_up",29,141,28,28).
    btnRadio-Set:LOAD-IMAGE-UP("Graphics/wp_up",1,57,28,28).
    btnSelection-List:LOAD-IMAGE-UP("Graphics/wp_up",1,113,28,28).
    btnToggle-Box:LOAD-IMAGE-UP("Graphics/wp_up",29,57,28,28).
    btnCombo-Box:LOAD-IMAGE-INSENSITIVE("Graphics/wp_down",1,141,28,28).
    btnEditor:LOAD-IMAGE-INSENSITIVE("Graphics/wp_down",29,113,28,28).
    btnFill-In:LOAD-IMAGE-INSENSITIVE("Graphics/wp_down",29,141,28,28).
    btnRadio-Set:LOAD-IMAGE-INSENSITIVE("Graphics/wp_down",1,57,28,28).
    btnSelection-List:LOAD-IMAGE-INSENSITIVE("Graphics/wp_down",1,113,28,28).
    btnToggle-Box:LOAD-IMAGE-INSENSITIVE("Graphics/wp_down",29,57,28,28).
  END. /* with frame */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {methods/run_link.i "CONTAINER" "pGetCompany" "(OUTPUT cCompany)"}
  RUN pGetDynProcs.
  {&OPEN-QUERY-{&BROWSE-NAME}}
  RUN pDisplay.

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
  FRAME {&FRAME-NAME}:MOVE-TO-TOP().

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
        IF dynParam.dataType:SCREEN-VALUE EQ "Logical" AND
           dynParam.initialValue:SCREEN-VALUE NE "yes" AND
           dynParam.initialValue:SCREEN-VALUE NE "no" THEN
        dynParam.initialValue:SCREEN-VALUE = "yes".
        IF INTEGER(dynParam.paramWidth:SCREEN-VALUE) EQ 0 THEN
        dynParam.paramWidth:SCREEN-VALUE = "14".
        IF INTEGER(dynParam.paramHeight:SCREEN-VALUE) EQ 0 THEN
        dynParam.paramHeight:SCREEN-VALUE = "1".
        FIND CURRENT dynParam EXCLUSIVE-LOCK.
        ASSIGN
            dynParam.paramID
            {&enabledFields}
            .
        FIND CURRENT dynParam NO-LOCK.
    END. /* with frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBuilder s-object 
PROCEDURE pBuilder :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    IF NOT VALID-HANDLE(hParamBldr) THEN
    RUN AOA/paramSetBldr.w PERSISTENT SET hParamBldr (
        THIS-PROCEDURE,
        "Param",
        dynParam.paramID
        ).
    ELSE
    RUN pReset IN hParamBldr (dynParam.paramID).

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
        hWidget:SCREEN-VALUE = IF hWidget:TYPE EQ "TOGGLE-BOX" THEN "YES" ELSE "".
        hWidget = hWidget:NEXT-SIBLING.
    END. /* do while */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCreateDynParam s-object 
PROCEDURE pCreateDynParam :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcWidgetType AS CHARACTER NO-UNDO.
    
/*    APPLY "CHOOSE":U TO btnAdd IN FRAME viewFrame.*/
    {methods/run_link.i "CONTAINER" "pAddParam"}
    DO WITH FRAME viewFrame:
        ASSIGN
            dynParam.paramName:SCREEN-VALUE    = ipcWidgetType
            dynParam.paramLabel:SCREEN-VALUE   = ipcWidgetType
            dynParam.paramType:SCREEN-VALUE    = "System"
            dynParam.dataType:SCREEN-VALUE     = "Character"
            dynParam.viewAs:SCREEN-VALUE       = ipcWidgetType
            dynParam.initialValue:SCREEN-VALUE = "Item 1"
            dynParam.paramWidth:SCREEN-VALUE   = "16"
            dynParam.paramHeight:SCREEN-VALUE  = "1"
            .
        CASE ipcWidgetType:
            WHEN "COMBO-BOX" THEN
            ASSIGN
                dynParam.paramFormat:SCREEN-VALUE  = "x(256)"
                dynParam.initialItems:SCREEN-VALUE = "Item 1,Item 2,Item 3"
                dynParam.innerLines:SCREEN-VALUE   = "5"
                .
            WHEN "EDITOR" THEN
            ASSIGN
                dynParam.initialValue:SCREEN-VALUE = ""
                dynParam.paramWidth:SCREEN-VALUE   = "30"
                dynParam.paramHeight:SCREEN-VALUE  = "4"
                dynParam.action:SCREEN-VALUE       = "HORIZONTAL,VERTICAL"
                .
            WHEN "FILL-IN" THEN
            ASSIGN
                dynParam.paramFormat:SCREEN-VALUE = "x(256)"
                .
            WHEN "RADIO-SET" THEN
            ASSIGN
                dynParam.initialValue:SCREEN-VALUE = "1"
                dynParam.initialItems:SCREEN-VALUE = "Item 1,1,Item 2,2,Item 3,3"
                dynParam.paramWidth:SCREEN-VALUE   = "30"
                dynParam.action:SCREEN-VALUE       = "HORIZONTAL"
                .
            WHEN "SELECTION-LIST" THEN DO:
            ASSIGN
                dynParam.initialItems:SCREEN-VALUE = "Item 1,Item 2,Item 3"
                dynParam.innerLines:SCREEN-VALUE   = "3"
                dynParam.action:SCREEN-VALUE       = "VERTICAL"
                .
            END. /* selection-list */
            WHEN "TOGGLE-BOX" THEN
            ASSIGN
                dynParam.dataType:SCREEN-VALUE     = "Logical"
                dynParam.paramFormat:SCREEN-VALUE  = "yes/no"
                dynParam.initialValue:SCREEN-VALUE = "yes"
                .
        END CASE.
    END. /* do with frame */

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
    
    DEFINE BUFFER bdynParamSetDtl FOR dynParamSetDtl.
    
    DO WITH FRAME viewFrame:
        CASE iphMode:LABEL:
            WHEN "Add" OR WHEN "Copy" OR WHEN "Update" THEN DO:
                {methods/run_link.i "CONTAINER" "pTransPanel"}
                {methods/run_link.i "CONTAINER" "pTransUpdate"}
                DISABLE {&widgetPanel}.
                ENABLE {&enabledFields}.
                IF AVAILABLE dynParam THEN
                iparamID = dynParam.paramID.
                IF iphMode:LABEL EQ "Add" THEN DO:
                    RUN pClearView.
                    ASSIGN
                        dynParam.paramID:SCREEN-VALUE         = ""
                        dynParam.paramType:SCREEN-VALUE       = "System"
                        dynParam.initializeProc:SCREEN-VALUE  = ""
                        dynParam.validateProc:SCREEN-VALUE    = ""
                        dynParam.descriptionProc:SCREEN-VALUE = ""
                        .
                    {methods/run_link.i "CONTAINER" "pSetButton" "('btnReset', NO)"}
                END. /* add */
                FRAME viewFrame:TITLE = iphMode:LABEL.
            END. /* add copy update */
            WHEN "Cancel" OR WHEN "Save" THEN DO:
                IF iphMode:LABEL EQ "Save" THEN DO:
                    IF cMode EQ "Add" OR cMode EQ "Copy" THEN DO TRANSACTION:
                        DO WHILE TRUE:
                            iNextID = NEXT-VALUE(paramID).
                            IF dynParam.paramType:SCREEN-VALUE NE "System" THEN
                            iNextID = iNextID + 5000.
                            IF NOT CAN-FIND(FIRST dynParam
                                WHERE dynParam.paramID EQ iNextID) THEN
                            LEAVE.
                        END. /* do while */
                        CREATE dynParam.
                        ASSIGN
                            dynParam.paramID:SCREEN-VALUE = STRING(iNextID)
                            rRowID = ROWID(dynParam)
                            .
                    END. /* if add/copy */
                    RUN pAssign.
                    IF cMode EQ "Add" OR cMode EQ "Copy" THEN DO:
                        RUN pReopenBrowse.
                        REPOSITION {&BROWSE-NAME} TO ROWID rRowID.
                        RUN pBuilder.
                    END. /* if add/copy */
                    ELSE
                    BROWSE {&BROWSE-NAME}:REFRESH().
                END. /* save */
                {methods/run_link.i "CONTAINER" "pTransPanel"}
                DISABLE {&widgetPanel} {&enabledFields}.
                {methods/run_link.i "CONTAINER" "pTransInit"}
                ENABLE {&widgetPanel}.
                FRAME viewFrame:TITLE = "View".
                APPLY "VALUE-CHANGED":U TO BROWSE {&BROWSE-NAME}.
            END. /* cancel save */
            WHEN "Delete" THEN DO:
                IF AVAILABLE dynParam THEN DO:
                    IF CAN-FIND(FIRST dynSubjectParamSet
                        WHERE dynParamSetDtl.paramID EQ dynParam.paramID) THEN
                    cMsg = "Parameter linked to Parameter Set Detail and will be Removed!" + CHR(10) + CHR(10).
                    MESSAGE
                        cMsg
                        "Delete Currently Selected Record?"
                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                    UPDATE lContinue.
                    IF lContinue THEN DO TRANSACTION:
                        cMode = iphMode:LABEL.
                        FOR EACH dynParamSetDtl EXCLUSIVE-LOCK
                            WHERE dynParamSetDtl.paramID EQ dynParam.paramID
                            :
                            DELETE dynParamSetDtl.
                        END. /* each dynParamSetDtl */
                        FIND CURRENT dynParam EXCLUSIVE-LOCK.
                        DELETE dynParam.
                        BROWSE {&BROWSE-NAME}:DELETE-CURRENT-ROW().
                    END. /* if lcontinue */
                    IF AVAILABLE dynParam THEN
                    BROWSE {&BROWSE-NAME}:REFRESH().
                    RUN pDisplay.
                END. /* if avail */
            END. /* delete */
            WHEN "Reset" THEN DO:
                RUN pDisplay.
                {methods/run_link.i "CONTAINER" "pTransPanel"}
                {methods/run_link.i "CONTAINER" "pTransUpdate"}
                DISABLE {&widgetPanel}.
            END. /* reset */
        END CASE. /* ipcmode:label */
        IF dynParam.paramName:SENSITIVE THEN
        APPLY "ENTRY":U TO dynParam.paramName.
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
        IF AVAILABLE dynParam THEN DO:
            ASSIGN
                dynParam.action:SCREEN-VALUE          = ""
                dynParam.initializeProc:SCREEN-VALUE  = " "
                dynParam.validateProc:SCREEN-VALUE    = " "
                dynParam.descriptionProc:SCREEN-VALUE = " "
                .
            DISPLAY {&displayFields}.
            {methods/run_link.i "CONTAINER" "pTransInit"}
            ENABLE {&widgetPanel}.
        END. /* if avail */
        ELSE DO:
            RUN pClearView.
            {methods/run_link.i "CONTAINER" "pTransPanel"}
            {methods/run_link.i "CONTAINER" "pSetButton" "('btnAdd', YES)"}
            DISABLE {&widgetPanel}.
        END. /* else */
    END. /* with frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetDynProcs s-object 
PROCEDURE pGetDynProcs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cDynProcs AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx       AS INTEGER   NO-UNDO.
    
    DO WITH FRAME viewFrame:
        ASSIGN
            cDynProcs = hDynDescripProc:INTERNAL-ENTRIES
            dynParam.descriptionProc:LIST-ITEMS = CHR(32)
            .
        DO idx = 1 TO NUM-ENTRIES(cDynProcs):
            IF ENTRY(idx,cDynProcs) BEGINS "f" THEN NEXT.
            dynParam.descriptionProc:ADD-LAST(ENTRY(idx,cDynProcs)).
        END. /* do idx */
        ASSIGN
            cDynProcs = hDynInitProc:INTERNAL-ENTRIES
            dynParam.initializeProc:LIST-ITEMS = CHR(32)
            .
        DO idx = 1 TO NUM-ENTRIES(cDynProcs):
            IF ENTRY(idx,cDynProcs) BEGINS "f" THEN NEXT.
            dynParam.initializeProc:ADD-LAST(ENTRY(idx,cDynProcs)).
        END. /* do idx */
        ASSIGN
            cDynProcs = hDynValProc:INTERNAL-ENTRIES
            dynParam.validateProc:LIST-ITEMS = CHR(32)
            .
        DO idx = 1 TO NUM-ENTRIES(cDynProcs):
            IF ENTRY(idx,cDynProcs) BEGINS "f" THEN NEXT.
            dynParam.validateProc:ADD-LAST(ENTRY(idx,cDynProcs)).
        END. /* do idx */
    END. /* with frame */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRefresh s-object 
PROCEDURE pRefresh :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    BROWSE dynParamBrowse:REFRESH().
    RUN pDisplay.

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
        WHEN "paramID" THEN
        RUN pByParamID.
        WHEN "paramName" THEN
        RUN pByParamName.
        WHEN "paramType" THEN
        RUN pByParamType.
        WHEN "paramLabel" THEN
        RUN pByParamLabel.
        WHEN "dataType" THEN
        RUN pByDataType.
        WHEN "viewAs" THEN
        RUN pByViewAs.
        OTHERWISE
        {&OPEN-QUERY-{&BROWSE-NAME}}
    END CASE.
    {AOA/includes/pReopenBrowse.i}
    {methods/run_link.i "CONTAINER" "pColumnLabel" "(hColumnLabel, lAscending)"}

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

