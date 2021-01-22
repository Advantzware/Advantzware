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

  File: paramSetDtl.w

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

&Scoped-define prgmName paramSetDtl.

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
DEFINE VARIABLE iParamSetID     AS INTEGER   NO-UNDO.
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
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME dynParamSetDtlBrowse

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES dynParamSetDtl

/* Definitions for BROWSE dynParamSetDtlBrowse                          */
&Scoped-define FIELDS-IN-QUERY-dynParamSetDtlBrowse dynParamSetDtl.paramID dynParamSetDtl.paramName dynParamSetDtl.paramLabel   
&Scoped-define ENABLED-FIELDS-IN-QUERY-dynParamSetDtlBrowse   
&Scoped-define SELF-NAME dynParamSetDtlBrowse
&Scoped-define QUERY-STRING-dynParamSetDtlBrowse FOR EACH dynParamSetDtl WHERE dynParamSetDtl.paramSetID EQ iParamSetID   AND STRING(dynParamSetDtl.paramID) + "|" + dynParamSetDtl.paramName + "|" + dynParamSetDtl.paramLabel MATCHES "*" + searchBar + "*"  ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-dynParamSetDtlBrowse OPEN QUERY {&SELF-NAME} FOR EACH dynParamSetDtl WHERE dynParamSetDtl.paramSetID EQ iParamSetID   AND STRING(dynParamSetDtl.paramID) + "|" + dynParamSetDtl.paramName + "|" + dynParamSetDtl.paramLabel MATCHES "*" + searchBar + "*"  ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-dynParamSetDtlBrowse dynParamSetDtl
&Scoped-define FIRST-TABLE-IN-QUERY-dynParamSetDtlBrowse dynParamSetDtl


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-dynParamSetDtlBrowse}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS dynParamSetDtlBrowse 
&Scoped-Define DISPLAYED-OBJECTS cSetName 

/* Custom List Definitions                                              */
/* List-1,List-2,List3,displayFields,enabledFields,List-6               */
&Scoped-define displayFields dynParamSetDtl.paramSetID ~
dynParamSetDtl.paramID dynParamSetDtl.paramName dynParamSetDtl.paramLabel ~
dynParamSetDtl.actionParamName dynParamSetDtl.action ~
dynParamSetDtl.paramWidth dynParamSetDtl.paramHeight ~
dynParamSetDtl.paramCol dynParamSetDtl.paramRow dynParamSetDtl.paramPrompt ~
dynParamSetDtl.initialValue dynParamSetDtl.initialItems ~
dynParamSetDtl.initializeProc dynParamSetDtl.validateProc ~
dynParamSetDtl.descriptionProc 
&Scoped-define enabledFields dynParamSetDtl.paramName ~
dynParamSetDtl.paramLabel dynParamSetDtl.actionParamName ~
dynParamSetDtl.action dynParamSetDtl.paramWidth dynParamSetDtl.paramHeight ~
dynParamSetDtl.paramCol dynParamSetDtl.paramRow dynParamSetDtl.paramPrompt ~
dynParamSetDtl.initialValue dynParamSetDtl.initialItems ~
dynParamSetDtl.initializeProc dynParamSetDtl.validateProc ~
dynParamSetDtl.descriptionProc 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE cSetName AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 75 BY .95 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY dynParamSetDtlBrowse FOR 
      dynParamSetDtl SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE dynParamSetDtlBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS dynParamSetDtlBrowse s-object _FREEFORM
  QUERY dynParamSetDtlBrowse DISPLAY
      dynParamSetDtl.paramID LABEL-BGCOLOR 14
dynParamSetDtl.paramName LABEL-BGCOLOR 14
dynParamSetDtl.paramLabel LABEL-BGCOLOR 14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 75 BY 23.62
         TITLE "Dynamic Parameter Set Detail".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     cSetName AT ROW 1 COL 1 NO-LABEL WIDGET-ID 50
     dynParamSetDtlBrowse AT ROW 1.95 COL 1 WIDGET-ID 200
     SPACE(83.00) SKIP(0.00)
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 15 FGCOLOR 1  WIDGET-ID 100.

DEFINE FRAME viewFrame
     dynParamSetDtl.paramSetID AT ROW 1.24 COL 20 COLON-ALIGNED WIDGET-ID 166
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
          BGCOLOR 15 
     dynParamSetDtl.paramID AT ROW 1.24 COL 55 COLON-ALIGNED WIDGET-ID 158
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
          BGCOLOR 15 
     dynParamSetDtl.paramName AT ROW 2.43 COL 20 COLON-ALIGNED WIDGET-ID 162 FORMAT "x(40)"
          VIEW-AS FILL-IN 
          SIZE 50 BY 1
          BGCOLOR 15 
     dynParamSetDtl.paramLabel AT ROW 3.62 COL 20 COLON-ALIGNED WIDGET-ID 160
          VIEW-AS FILL-IN 
          SIZE 50 BY 1
          BGCOLOR 15 
     dynParamSetDtl.actionParamName AT ROW 4.81 COL 20 COLON-ALIGNED WIDGET-ID 302
          LABEL "Action Param Name"
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
          BGCOLOR 15 
     dynParamSetDtl.action AT ROW 4.81 COL 45 NO-LABEL WIDGET-ID 186
          VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
          LIST-ITEMS "NO:DISABLE","NO:ENABLE","NO:LOW","NO:HI","YES:DISABLE","YES:ENABLE","YES:LOW","YES:HI","CALENDAR","DATEPICKLIST","EMAIL","HORIZONTAL","VERTICAL","START DESCRIPTION","END DESCRIPTION","LIST-ITEM-PAIRS" 
          SIZE 27 BY 10.24
     dynParamSetDtl.paramWidth AT ROW 8.14 COL 20 COLON-ALIGNED WIDGET-ID 306
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     dynParamSetDtl.paramHeight AT ROW 9.33 COL 20 COLON-ALIGNED WIDGET-ID 304
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     dynParamSetDtl.paramCol AT ROW 10.52 COL 20 COLON-ALIGNED WIDGET-ID 156
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     dynParamSetDtl.paramRow AT ROW 11.71 COL 20 COLON-ALIGNED WIDGET-ID 164
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     dynParamSetDtl.paramPrompt AT ROW 12.91 COL 22 WIDGET-ID 288
          VIEW-AS TOGGLE-BOX
          SIZE 11 BY 1
     dynParamSetDtl.initialValue AT ROW 14.1 COL 20 COLON-ALIGNED WIDGET-ID 154
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
          BGCOLOR 15 
     dynParamSetDtl.initialItems AT ROW 15.29 COL 20 COLON-ALIGNED WIDGET-ID 152 FORMAT "x(256)"
          VIEW-AS FILL-IN 
          SIZE 61 BY 1
          BGCOLOR 15 
     dynParamSetDtl.initializeProc AT ROW 16.48 COL 20 COLON-ALIGNED WIDGET-ID 296
          VIEW-AS COMBO-BOX SORT INNER-LINES 100
          LIST-ITEMS "Item 1" 
          DROP-DOWN-LIST
          SIZE 50 BY 1
     dynParamSetDtl.validateProc AT ROW 17.67 COL 20 COLON-ALIGNED WIDGET-ID 298
          VIEW-AS COMBO-BOX SORT INNER-LINES 100
          LIST-ITEMS "Item 1" 
          DROP-DOWN-LIST
          SIZE 50 BY 1
     dynParamSetDtl.descriptionProc AT ROW 18.86 COL 20 COLON-ALIGNED WIDGET-ID 300
          LABEL "Descript Procedure"
          VIEW-AS COMBO-BOX SORT INNER-LINES 100
          LIST-ITEMS "Item 1" 
          DROP-DOWN-LIST
          SIZE 50 BY 1
     "Action:" VIEW-AS TEXT
          SIZE 7 BY 1 AT ROW 6 COL 37 WIDGET-ID 188
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
         HEIGHT             = 24.67
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
ASSIGN FRAME viewFrame:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME viewFrame:MOVE-BEFORE-TAB-ITEM (dynParamSetDtlBrowse:HANDLE IN FRAME DEFAULT-FRAME)
/* END-ASSIGN-TABS */.

/* BROWSE-TAB dynParamSetDtlBrowse viewFrame DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 24.57
       FRAME DEFAULT-FRAME:WIDTH            = 158.

/* SETTINGS FOR FILL-IN cSetName IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       dynParamSetDtlBrowse:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

/* SETTINGS FOR FRAME viewFrame
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME viewFrame:MOVABLE          = TRUE.

/* SETTINGS FOR SELECTION-LIST dynParamSetDtl.action IN FRAME viewFrame
   4 5                                                                  */
/* SETTINGS FOR FILL-IN dynParamSetDtl.actionParamName IN FRAME viewFrame
   4 5 EXP-LABEL                                                        */
/* SETTINGS FOR COMBO-BOX dynParamSetDtl.descriptionProc IN FRAME viewFrame
   4 5 EXP-LABEL                                                        */
/* SETTINGS FOR FILL-IN dynParamSetDtl.initialItems IN FRAME viewFrame
   4 5 EXP-FORMAT                                                       */
/* SETTINGS FOR COMBO-BOX dynParamSetDtl.initializeProc IN FRAME viewFrame
   4 5                                                                  */
/* SETTINGS FOR FILL-IN dynParamSetDtl.initialValue IN FRAME viewFrame
   4 5                                                                  */
/* SETTINGS FOR FILL-IN dynParamSetDtl.paramCol IN FRAME viewFrame
   4 5                                                                  */
/* SETTINGS FOR FILL-IN dynParamSetDtl.paramHeight IN FRAME viewFrame
   4 5                                                                  */
/* SETTINGS FOR FILL-IN dynParamSetDtl.paramID IN FRAME viewFrame
   4                                                                    */
/* SETTINGS FOR FILL-IN dynParamSetDtl.paramLabel IN FRAME viewFrame
   4 5                                                                  */
/* SETTINGS FOR FILL-IN dynParamSetDtl.paramName IN FRAME viewFrame
   4 5 EXP-FORMAT                                                       */
/* SETTINGS FOR TOGGLE-BOX dynParamSetDtl.paramPrompt IN FRAME viewFrame
   4 5                                                                  */
/* SETTINGS FOR FILL-IN dynParamSetDtl.paramRow IN FRAME viewFrame
   4 5                                                                  */
/* SETTINGS FOR FILL-IN dynParamSetDtl.paramSetID IN FRAME viewFrame
   4                                                                    */
/* SETTINGS FOR FILL-IN dynParamSetDtl.paramWidth IN FRAME viewFrame
   4 5                                                                  */
/* SETTINGS FOR COMBO-BOX dynParamSetDtl.validateProc IN FRAME viewFrame
   4 5                                                                  */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE dynParamSetDtlBrowse
/* Query rebuild information for BROWSE dynParamSetDtlBrowse
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH dynParamSetDtl
WHERE dynParamSetDtl.paramSetID EQ iParamSetID
  AND STRING(dynParamSetDtl.paramID) + "|" +
dynParamSetDtl.paramName + "|" +
dynParamSetDtl.paramLabel MATCHES "*" + searchBar + "*"
 ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE dynParamSetDtlBrowse */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME viewFrame
/* Query rebuild information for FRAME viewFrame
     _Query            is NOT OPENED
*/  /* FRAME viewFrame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME dynParamSetDtlBrowse
&Scoped-define SELF-NAME dynParamSetDtlBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dynParamSetDtlBrowse s-object
ON DEFAULT-ACTION OF dynParamSetDtlBrowse IN FRAME DEFAULT-FRAME /* Dynamic Parameter Set Detail */
DO:
    RUN pBuilder.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dynParamSetDtlBrowse s-object
ON START-SEARCH OF dynParamSetDtlBrowse IN FRAME DEFAULT-FRAME /* Dynamic Parameter Set Detail */
DO:
    &Scoped-define startSearchValueChanged
    {AOA/includes/startSearch.i}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dynParamSetDtlBrowse s-object
ON VALUE-CHANGED OF dynParamSetDtlBrowse IN FRAME DEFAULT-FRAME /* Dynamic Parameter Set Detail */
DO:
    RUN pDisplay.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME viewFrame
&Scoped-define SELF-NAME dynParamSetDtl.paramID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dynParamSetDtl.paramID s-object
ON LEAVE OF dynParamSetDtl.paramID IN FRAME viewFrame /* Parameter ID */
DO:
    FIND FIRST dynParam NO-LOCK
         WHERE dynParam.paramID EQ INTEGER(SELF:SCREEN-VALUE)
         NO-ERROR.
    IF AVAILABLE dynParam THEN DO:
        ASSIGN
            dynParamSetDtl.paramName:SCREEN-VALUE      = dynParam.paramName
            dynParamSetDtl.paramLabel:SCREEN-VALUE     = dynParam.paramLabel
            dynParamSetDtl.initialItems:SCREEN-VALUE   = dynParam.initialItems
            dynParamSetDtl.initialValue:SCREEN-VALUE   = dynParam.initialValue
            dynParamSetDtl.initializeProc:SCREEN-VALUE = dynParam.initializeProc
            dynParamSetDtl.validateProc:SCREEN-VALUE   = dynParam.validateProc
            dynParamSetDtl.paramHeight:SCREEN-VALUE    = STRING(dynParam.paramHeight)
            dynParamSetDtl.paramWidth:SCREEN-VALUE     = STRING(dynParam.paramWidth)
            dynParamSetDtl.action:SCREEN-VALUE         = dynParam.action
            dynParamSetDtl.paramCol:SCREEN-VALUE       = "20"
            dynParamSetDtl.paramRow:SCREEN-VALUE       = "1"
            .
    END. /* if avail */
    ELSE DO:
        MESSAGE
            "Invalid Parameter ID..."
        VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END. /* else */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK s-object 


/* ***************************  Main Block  *************************** */
{methods/template/brwcustom2.i}
/* If testing in the UIB, initialize the SmartObject. */  
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{methods/sortByProc.i "pByParamID" "dynParamSetDtl.paramID"}
{methods/sortByProc.i "pByParamName" "dynParamSetDtl.paramName"}
{methods/sortByProc.i "pByParamLabel" "dynParamSetDtl.paramLabel"}

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
  HIDE FRAME DEFAULT-FRAME.
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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {methods/run_link.i "CONTAINER" "pGetCompany" "(OUTPUT cCompany)"}
  RUN pGetDynProcs.

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
        FIND CURRENT dynParamSetDtl EXCLUSIVE-LOCK.
        ASSIGN
            dynParamSetDtl.paramSetID
            dynParamSetDtl.paramID
            {&enabledFields}
            .
        FIND CURRENT dynParamSetDtl NO-LOCK.
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
        "Set",
        dynParamSetDtl.paramSetID
        ).
    ELSE
    RUN pReset IN hParamBldr (dynParamSetDtl.paramSetID).

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

    DEFINE VARIABLE lContinue AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE rRowID    AS ROWID     NO-UNDO.
    
    DO WITH FRAME viewFrame:
        CASE iphMode:LABEL:
            WHEN "Add" OR WHEN "Copy" OR WHEN "Update" THEN DO:
                {methods/run_link.i "CONTAINER" "pTransPanel"}
                {methods/run_link.i "CONTAINER" "pTransUpdate"}
                ENABLE {&enabledFields}.
                IF iphMode:LABEL EQ "Add" THEN DO:
                    RUN pClearView.
                    ASSIGN
                        dynParamSetDtl.paramSetID:SCREEN-VALUE      = ""
                        dynParamSetDtl.paramID:SCREEN-VALUE         = ""
                        dynParamSetDtl.paramPrompt:SCREEN-VALUE     = "yes"
                        dynParamSetDtl.initializeProc:SCREEN-VALUE  = ""
                        dynParamSetDtl.validateProc:SCREEN-VALUE    = ""
                        dynParamSetDtl.descriptionProc:SCREEN-VALUE = ""
                        .
                    {methods/run_link.i "CONTAINER" "pSetButton" "('btnReset', NO)"}
                END. /* add */
                ASSIGN
                    dynParamSetDtl.paramID:SENSITIVE = iphMode:LABEL EQ "Add"
                    FRAME viewFrame:TITLE = iphMode:LABEL
                    .
            END. /* add copy update */
            WHEN "Cancel" OR WHEN "Save" THEN DO:
                IF iphMode:LABEL EQ "Save" THEN DO:
                    IF cMode EQ "Add" OR cMode EQ "Copy" THEN DO TRANSACTION:
                        CREATE dynParamSetDtl.
                        ASSIGN
                            dynParamSetDtl.paramSetID:SCREEN-VALUE = STRING(iParamSetID)
                            rRowID = ROWID(dynParamSetDtl)
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
                DISABLE {&enabledFields} dynParamSetDtl.paramID.
                {methods/run_link.i "CONTAINER" "pTransInit"}
                FRAME viewFrame:TITLE = "View".
                APPLY "VALUE-CHANGED":U TO BROWSE {&BROWSE-NAME}.
            END. /* cancel save */
            WHEN "Delete" THEN DO:
                IF AVAILABLE dynParamSetDtl THEN DO:
                    MESSAGE
                        "Delete Currently Selected Record?"
                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                    UPDATE lContinue.
                    IF lContinue THEN DO TRANSACTION:
                        cMode = iphMode:LABEL.
                        FIND CURRENT dynParamSetDtl EXCLUSIVE-LOCK.
                        DELETE dynParamSetDtl.
                        BROWSE {&BROWSE-NAME}:DELETE-CURRENT-ROW().
                    END. /* if lcontinue */
                    IF AVAILABLE dynParamSetDtl THEN
                    BROWSE {&BROWSE-NAME}:REFRESH().
                    RUN pDisplay.
                END. /* if avail */
            END. /* delete */
            WHEN "Reset" THEN DO:
                RUN pDisplay.
                {methods/run_link.i "CONTAINER" "pTransPanel"}
                {methods/run_link.i "CONTAINER" "pTransUpdate"}
            END. /* reset */
        END CASE. /* ipcmode:label */
        IF dynParamSetDtl.paramID:SENSITIVE THEN
        APPLY "ENTRY":U TO dynParamSetDtl.paramID.
        ELSE
        IF dynParamSetDtl.paramName:SENSITIVE THEN
        APPLY "ENTRY":U TO dynParamSetDtl.paramName.
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
        IF AVAILABLE dynParamSetDtl THEN DO:
            ASSIGN
                dynParamSetDtl.action:SCREEN-VALUE          = ""
                dynParamSetDtl.initializeProc:SCREEN-VALUE  = " "
                dynParamSetDtl.validateProc:SCREEN-VALUE    = " "
                dynParamSetDtl.descriptionProc:SCREEN-VALUE = " "
                .
            DISPLAY {&displayFields}.
            {methods/run_link.i "CONTAINER" "pTransInit"}
        END. /* if avail */
        ELSE DO:
            RUN pClearView.
            {methods/run_link.i "CONTAINER" "pTransPanel"}
            {methods/run_link.i "CONTAINER" "pSetButton" "('btnAdd', YES)"}
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
            dynParamSetDtl.descriptionProc:LIST-ITEMS = CHR(32)
            .
        DO idx = 1 TO NUM-ENTRIES(cDynProcs):
            IF ENTRY(idx,cDynProcs) BEGINS "f" THEN NEXT.
            dynParamSetDtl.descriptionProc:ADD-LAST(ENTRY(idx,cDynProcs)).
        END. /* do idx */
        ASSIGN
            cDynProcs = hDynInitProc:INTERNAL-ENTRIES
            dynParamSetDtl.initializeProc:LIST-ITEMS = CHR(32)
            .
        DO idx = 1 TO NUM-ENTRIES(cDynProcs):
            IF ENTRY(idx,cDynProcs) BEGINS "f" THEN NEXT.
            dynParamSetDtl.initializeProc:ADD-LAST(ENTRY(idx,cDynProcs)).
        END. /* do idx */
        ASSIGN
            cDynProcs = hDynValProc:INTERNAL-ENTRIES
            dynParamSetDtl.validateProc:LIST-ITEMS = CHR(32)
            .
        DO idx = 1 TO NUM-ENTRIES(cDynProcs):
            IF ENTRY(idx,cDynProcs) BEGINS "f" THEN NEXT.
            dynParamSetDtl.validateProc:ADD-LAST(ENTRY(idx,cDynProcs)).
        END. /* do idx */
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
  {methods/run_link.i "CONTAINER" "pGetParamSetID" "(OUTPUT iParamSetID)"}
  FIND FIRST dynParamSet NO-LOCK
       WHERE dynParamSet.paramSetID EQ iParamSetID
       NO-ERROR.
  cSetName:SCREEN-VALUE IN FRAME {&FRAME-NAME} =
    "Set: " + STRING(iParamSetID)
            + IF AVAILABLE dynParamSet THEN (" - " + dynParamSet.setName) ELSE ""
            .
  {&OPEN-QUERY-{&BROWSE-NAME}}
  RUN pDisplay.

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
    
    {methods/run_link.i "CONTAINER" "pNavPanel" "(iphNavPanel)"}

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
    BROWSE dynParamSetDtlBrowse:REFRESH().
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
        WHEN "paramLabel" THEN
        RUN pByParamLabel.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pTableHandle s-object
PROCEDURE pTableHandle:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER ophTable AS HANDLE NO-UNDO.

    ophTable = BUFFER dynParamSetDtl:HANDLE.

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

