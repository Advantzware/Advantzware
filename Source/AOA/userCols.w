&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
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

&Scoped-define prgmName userCols.

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE char-hdl    AS CHARACTER NO-UNDO.
DEFINE VARIABLE hColumn     AS HANDLE    NO-UNDO EXTENT 4.
DEFINE VARIABLE hContainer  AS HANDLE    NO-UNDO.
DEFINE VARIABLE lAdvanced   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lUpdateMode AS LOGICAL   NO-UNDO.
DEFINE VARIABLE pHandle     AS HANDLE    NO-UNDO.
DEFINE VARIABLE rRowID      AS ROWID     NO-UNDO.

DEFINE TEMP-TABLE ttSubjectColumn NO-UNDO LIKE dynSubjectColumn
    FIELD allData AS CHARACTER
        INDEX allData sortOrder allData
        .
DEFINE TEMP-TABLE ttSort NO-UNDO
    FIELD subjectID      LIKE ttSubjectColumn.subjectID
    FIELD sortOrder      LIKE ttSubjectColumn.sortCol
    FIELD fieldLabel     LIKE ttSubjectColumn.fieldLabel
    FIELD sortDescending LIKE ttSubjectColumn.sortDescending
    FIELD colRowID         AS ROWID
        INDEX ttSort IS PRIMARY subjectID sortOrder
        .
DEFINE TEMP-TABLE ttGroup NO-UNDO
    FIELD subjectID  LIKE ttSubjectColumn.subjectID
    FIELD fieldLabel LIKE ttSubjectColumn.fieldLabel
    FIELD groupLabel LIKE ttSubjectColumn.groupLabel
    FIELD colRowID AS ROWID
    .
{AOA/tempTable/ttGroupCalc.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartObject
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME groupBrowse

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttGroup ttSort ttSubjectColumn

/* Definitions for BROWSE groupBrowse                                   */
&Scoped-define FIELDS-IN-QUERY-groupBrowse ttGroup.fieldLabel ttGroup.groupLabel   
&Scoped-define ENABLED-FIELDS-IN-QUERY-groupBrowse ttGroup.groupLabel   
&Scoped-define ENABLED-TABLES-IN-QUERY-groupBrowse ttGroup
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-groupBrowse ttGroup
&Scoped-define SELF-NAME groupBrowse
&Scoped-define QUERY-STRING-groupBrowse FOR EACH ttGroup
&Scoped-define OPEN-QUERY-groupBrowse OPEN QUERY {&SELF-NAME} FOR EACH ttGroup.
&Scoped-define TABLES-IN-QUERY-groupBrowse ttGroup
&Scoped-define FIRST-TABLE-IN-QUERY-groupBrowse ttGroup


/* Definitions for BROWSE sortBrowse                                    */
&Scoped-define FIELDS-IN-QUERY-sortBrowse ttSort.sortOrder ttSort.sortDescending ttSort.fieldLabel   
&Scoped-define ENABLED-FIELDS-IN-QUERY-sortBrowse ttSort.sortDescending   
&Scoped-define ENABLED-TABLES-IN-QUERY-sortBrowse ttSort
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-sortBrowse ttSort
&Scoped-define SELF-NAME sortBrowse
&Scoped-define QUERY-STRING-sortBrowse FOR EACH ttSort
&Scoped-define OPEN-QUERY-sortBrowse OPEN QUERY {&SELF-NAME} FOR EACH ttSort.
&Scoped-define TABLES-IN-QUERY-sortBrowse ttSort
&Scoped-define FIRST-TABLE-IN-QUERY-sortBrowse ttSort


/* Definitions for BROWSE subjectColumnBrowse                           */
&Scoped-define FIELDS-IN-QUERY-subjectColumnBrowse ttSubjectColumn.isActive ttSubjectColumn.fieldLabel ttSubjectColumn.sortOrder ttSubjectColumn.groupCalc ttSubjectColumn.fieldName ttSubjectColumn.fieldFormat   
&Scoped-define ENABLED-FIELDS-IN-QUERY-subjectColumnBrowse ttSubjectColumn.isActive ttSubjectColumn.fieldLabel   
&Scoped-define ENABLED-TABLES-IN-QUERY-subjectColumnBrowse ttSubjectColumn
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-subjectColumnBrowse ttSubjectColumn
&Scoped-define SELF-NAME subjectColumnBrowse
&Scoped-define QUERY-STRING-subjectColumnBrowse FOR EACH ttSubjectColumn WHERE ttSubjectColumn.subjectID EQ dynSubject.subjectID AND ttSubjectColumn.allData MATCHES "*" + searchBar + "*" BY ttSubjectColumn.sortOrder
&Scoped-define OPEN-QUERY-subjectColumnBrowse OPEN QUERY {&SELF-NAME} FOR EACH ttSubjectColumn WHERE ttSubjectColumn.subjectID EQ dynSubject.subjectID AND ttSubjectColumn.allData MATCHES "*" + searchBar + "*" BY ttSubjectColumn.sortOrder.
&Scoped-define TABLES-IN-QUERY-subjectColumnBrowse ttSubjectColumn
&Scoped-define FIRST-TABLE-IN-QUERY-subjectColumnBrowse ttSubjectColumn


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-groupBrowse}~
    ~{&OPEN-QUERY-sortBrowse}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS searchBar sortBrowse subjectColumnBrowse ~
btnColMoveDown groupBrowse btnColMoveUp btnAdvanced btnGroupCalc btnSave ~
btnAddGroup btnAddSort btnRemoveGroup btnRemoveSort btnSortMoveDown ~
btnSortMoveUp 
&Scoped-Define DISPLAYED-OBJECTS searchBar 

/* Custom List Definitions                                              */
/* SortGroupButtons,List-2,List-3,List-4,List-5,List-6                  */
&Scoped-define SortGroupButtons btnColMoveDown btnColMoveUp btnGroupCalc ~
btnAddGroup btnAddSort btnRemoveGroup btnRemoveSort btnSortMoveDown ~
btnSortMoveUp 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAddGroup 
     IMAGE-UP FILE "Graphics/32x32/media_play.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/media_play_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Add Group" 
     SIZE 8 BY 1.91 TOOLTIP "Add Group".

DEFINE BUTTON btnAddSort 
     IMAGE-UP FILE "Graphics/32x32/media_play.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/media_play_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Add Sort" 
     SIZE 8 BY 1.91 TOOLTIP "Add Sort".

DEFINE BUTTON btnAdvanced 
     IMAGE-UP FILE "Graphics/32x32/indent_increase.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     CONTEXT-HELP-ID 0
     SIZE 8 BY 1.91 TOOLTIP "Advanced".

DEFINE BUTTON btnColMoveDown 
     IMAGE-UP FILE "Graphics/32x32/navigate_close.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/navigate_close_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Move Down Column" 
     SIZE 8 BY 1.91 TOOLTIP "Move Down Column".

DEFINE BUTTON btnColMoveUp 
     IMAGE-UP FILE "Graphics/32x32/navigate_open.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/navigate_open_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Move Up Column" 
     SIZE 8 BY 1.91 TOOLTIP "Move Up Column".

DEFINE BUTTON btnGroupCalc 
     IMAGE-UP FILE "Graphics/32x32/spreadsheet_sum.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/spreadsheet_sum_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91 TOOLTIP "Group Calculations".

DEFINE BUTTON btnRemoveGroup 
     IMAGE-UP FILE "Graphics/32x32/garbage_can.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/garbage_can_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Remove Group" 
     SIZE 8 BY 1.91 TOOLTIP "Remove Group".

DEFINE BUTTON btnRemoveSort 
     IMAGE-UP FILE "Graphics/32x32/garbage_can.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/garbage_can_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Remove Sort" 
     SIZE 8 BY 1.91 TOOLTIP "Remove Sort".

DEFINE BUTTON btnReset 
     IMAGE-UP FILE "Graphics/32x32/undo_32.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/undo_32_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Reset" 
     SIZE 8 BY 1.91 TOOLTIP "Update/Save Sort".

DEFINE BUTTON btnSave 
     IMAGE-UP FILE "Graphics/32x32/pencil.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/pencil_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Update/Save Columns" 
     SIZE 8 BY 1.91 TOOLTIP "Update/Save Columns".

DEFINE BUTTON btnSortMoveDown 
     IMAGE-UP FILE "Graphics/32x32/navigate_close.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/navigate_close_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Move Down" 
     SIZE 8 BY 1.91 TOOLTIP "Move Down".

DEFINE BUTTON btnSortMoveUp 
     IMAGE-UP FILE "Graphics/32x32/navigate_open.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/navigate_open_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Move Up" 
     SIZE 8 BY 1.91 TOOLTIP "Move Up".

DEFINE VARIABLE searchBar AS CHARACTER FORMAT "X(256)":U 
     LABEL "Search" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY groupBrowse FOR 
      ttGroup SCROLLING.

DEFINE QUERY sortBrowse FOR 
      ttSort SCROLLING.

DEFINE QUERY subjectColumnBrowse FOR 
      ttSubjectColumn SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE groupBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS groupBrowse s-object _FREEFORM
  QUERY groupBrowse DISPLAY
      ttGroup.fieldLabel
ttGroup.groupLabel
ENABLE
ttGroup.groupLabel
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 56 BY 8.33
         TITLE "Column Groups" ROW-HEIGHT-CHARS .81.

DEFINE BROWSE sortBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS sortBrowse s-object _FREEFORM
  QUERY sortBrowse DISPLAY
      ttSort.sortOrder
ttSort.sortDescending COLUMN-LABEL "Descending" VIEW-AS TOGGLE-BOX
ttSort.fieldLabel
ENABLE
ttSort.sortDescending
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 56 BY 15.71
         TITLE "Column Sort By" ROW-HEIGHT-CHARS .81 FIT-LAST-COLUMN.

DEFINE BROWSE subjectColumnBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS subjectColumnBrowse s-object _FREEFORM
  QUERY subjectColumnBrowse DISPLAY
      ttSubjectColumn.isActive VIEW-AS TOGGLE-BOX
ttSubjectColumn.fieldLabel
ttSubjectColumn.sortOrder
ttSubjectColumn.groupCalc
ttSubjectColumn.fieldName FORMAT "x(40)"
ttSubjectColumn.fieldFormat
ENABLE
ttSubjectColumn.isActive
ttSubjectColumn.fieldLabel
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 42 BY 23.33
         TITLE "Column Order" ROW-HEIGHT-CHARS .81.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     searchBar AT ROW 1.24 COL 7 COLON-ALIGNED HELP
          "Enter Search" WIDGET-ID 22
     sortBrowse AT ROW 1.24 COL 103 WIDGET-ID 300
     subjectColumnBrowse AT ROW 2.19 COL 9 WIDGET-ID 200
     btnColMoveDown AT ROW 8.38 COL 1 HELP
          "Move Down Column" WIDGET-ID 14
     groupBrowse AT ROW 17.19 COL 103 WIDGET-ID 400
     btnColMoveUp AT ROW 6.48 COL 1 HELP
          "Move Up Column" WIDGET-ID 16
     btnAdvanced AT ROW 2.67 COL 1 HELP
          "Expand/Collapse Columns" WIDGET-ID 668
     btnGroupCalc AT ROW 18.14 COL 1 HELP
          "Group Calculations" WIDGET-ID 272
     btnSave AT ROW 12.19 COL 1 HELP
          "Update/Save Columns" WIDGET-ID 24
     btnReset AT ROW 14.1 COL 1 HELP
          "Reset" WIDGET-ID 28
     btnAddGroup AT ROW 18.62 COL 95 HELP
          "Add Group" WIDGET-ID 10
     btnAddSort AT ROW 2.67 COL 95 HELP
          "Add Sort" WIDGET-ID 2
     btnRemoveGroup AT ROW 23.62 COL 95 HELP
          "Remove Group" WIDGET-ID 12
     btnRemoveSort AT ROW 15.05 COL 95 HELP
          "Remove Sort" WIDGET-ID 4
     btnSortMoveDown AT ROW 8.38 COL 95 HELP
          "Move Down" WIDGET-ID 8
     btnSortMoveUp AT ROW 6.48 COL 95 HELP
          "Move Up" WIDGET-ID 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 15 FGCOLOR 1  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartObject
   Allow: Basic,Browse
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
         WIDTH              = 158.4.
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
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB sortBrowse searchBar F-Main */
/* BROWSE-TAB subjectColumnBrowse sortBrowse F-Main */
/* BROWSE-TAB groupBrowse btnColMoveDown F-Main */
ASSIGN 
       FRAME F-Main:HIDDEN           = TRUE
       FRAME F-Main:HEIGHT           = 24.52
       FRAME F-Main:WIDTH            = 158.

/* SETTINGS FOR BUTTON btnAddGroup IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON btnAddSort IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON btnColMoveDown IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON btnColMoveUp IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON btnGroupCalc IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON btnRemoveGroup IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON btnRemoveSort IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON btnReset IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnSortMoveDown IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON btnSortMoveUp IN FRAME F-Main
   1                                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE groupBrowse
/* Query rebuild information for BROWSE groupBrowse
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttGroup.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE groupBrowse */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE sortBrowse
/* Query rebuild information for BROWSE sortBrowse
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttSort.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE sortBrowse */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE subjectColumnBrowse
/* Query rebuild information for BROWSE subjectColumnBrowse
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttSubjectColumn
WHERE ttSubjectColumn.subjectID EQ dynSubject.subjectID
AND ttSubjectColumn.allData MATCHES "*" + searchBar + "*"
BY ttSubjectColumn.sortOrder.
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE subjectColumnBrowse */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME btnAddGroup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAddGroup s-object
ON CHOOSE OF btnAddGroup IN FRAME F-Main /* Add Group */
DO:
    RUN pAddGroup.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAddSort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAddSort s-object
ON CHOOSE OF btnAddSort IN FRAME F-Main /* Add Sort */
DO:
    RUN pAddSort.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAdvanced
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAdvanced s-object
ON CHOOSE OF btnAdvanced IN FRAME F-Main
DO:
    lAdvanced = NOT lAdvanced.
    SELF:LOAD-IMAGE("Graphics/32x32/"
        + IF lAdvanced THEN "indent_decrease.png"
          ELSE "indent_increase.png")
        .
    RUN pAdvanced.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnColMoveDown
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnColMoveDown s-object
ON CHOOSE OF btnColMoveDown IN FRAME F-Main /* Move Down Column */
DO:
    RUN pMoveCol (1).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnColMoveUp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnColMoveUp s-object
ON CHOOSE OF btnColMoveUp IN FRAME F-Main /* Move Up Column */
DO:
    RUN pMoveCol (-1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnGroupCalc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnGroupCalc s-object
ON CHOOSE OF btnGroupCalc IN FRAME F-Main
DO:
    RUN pJasperGroupCalc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRemoveGroup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRemoveGroup s-object
ON CHOOSE OF btnRemoveGroup IN FRAME F-Main /* Remove Group */
DO:
    RUN pRemoveGroup.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRemoveSort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRemoveSort s-object
ON CHOOSE OF btnRemoveSort IN FRAME F-Main /* Remove Sort */
DO:
    RUN pRemoveSort.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnReset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnReset s-object
ON CHOOSE OF btnReset IN FRAME F-Main /* Reset */
DO:
    RUN pReset.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave s-object
ON CHOOSE OF btnSave IN FRAME F-Main /* Update/Save Columns */
DO:
    IF lUpdateMode THEN
    RUN pSave.
    ELSE DO:
        RUN pSetSortGroupButtons (NO).
        RUN pUpdateMode (YES).
        APPLY "ENTRY":U TO ttSubjectColumn.fieldLabel IN BROWSE subjectColumnBrowse.
    END. /* else */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSortMoveDown
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSortMoveDown s-object
ON CHOOSE OF btnSortMoveDown IN FRAME F-Main /* Move Down */
DO:
    RUN pMoveSort (1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSortMoveUp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSortMoveUp s-object
ON CHOOSE OF btnSortMoveUp IN FRAME F-Main /* Move Up */
DO:
    RUN pMoveSort (-1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME searchBar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL searchBar s-object
ON VALUE-CHANGED OF searchBar IN FRAME F-Main /* Search */
DO:
    ASSIGN {&SELF-NAME}.
    {&OPEN-QUERY-subjectColumnBrowse}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME groupBrowse
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK s-object 


/* ***************************  Main Block  *************************** */

&Scoped-define sdBrowseName subjectColumnBrowse
{methods/template/brwcustom2.i 1}
&Scoped-define sdBrowseName sortBrowse
{methods/template/brwcustom2.i 2}
&Scoped-define sdBrowseName groupBrowse
{methods/template/brwcustom2.i 3}

/* If testing in the UIB, initialize the SmartObject. */  
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{AOA/includes/pJasperGroupCalc.i "dyn"}

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
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,'CONTAINER':U,OUTPUT char-hdl).
  hContainer = WIDGET-HANDLE(char-hdl).
  RUN pAdvanced.
  RUN pSetSortGroupButtons (YES).

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
  RUN pUserColumns.
  FRAME {&FRAME-NAME}:MOVE-TO-TOP().
  RUN pSetSortGroupButtons (dynParamValue.user-id NE "_default").
  btnSave:SENSITIVE = dynParamValue.user-id NE "_default".
  RUN pUpdateMode (NO).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAddGroup s-object 
PROCEDURE pAddGroup :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE rRowID AS ROWID NO-UNDO.

    IF NOT AVAILABLE ttSubjectColumn THEN RETURN.
    IF CAN-FIND(FIRST ttGroup
                WHERE ttGroup.colRowID EQ ROWID(ttSubjectColumn)) THEN
    RETURN.
    CREATE ttGroup.
    ASSIGN
        ttGroup.colRowID   = ROWID(ttSubjectColumn)
        ttGroup.subjectID  = ttSubjectColumn.subjectID
        ttGroup.fieldLabel = ttSubjectColumn.fieldLabel
        rRowID             = ROWID(ttGroup)
        ttSubjectColumn.isGroup = YES
        .
    {&OPEN-QUERY-groupBrowse}
    REPOSITION groupBrowse TO ROWID rRowID.
    RUN pUpdateMode (YES).
    APPLY "ENTRY":U TO ttGroup.groupLabel IN BROWSE groupBrowse.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAddSort s-object 
PROCEDURE pAddSort :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iOrder AS INTEGER NO-UNDO.
    DEFINE VARIABLE rRowID AS ROWID   NO-UNDO.

    IF NOT AVAILABLE ttSubjectColumn THEN RETURN.
    IF CAN-FIND(FIRST ttSort
                WHERE ttSort.colRowID EQ ROWID(ttSubjectColumn)) THEN
    RETURN.
    FIND LAST ttSort NO-ERROR.
    iOrder = IF AVAILABLE ttSort THEN ttSort.sortOrder + 1 ELSE 1.
    CREATE ttSort.
    ASSIGN
        ttSort.colRowID         = ROWID(ttSubjectColumn)
        ttSort.subjectID        = ttSubjectColumn.subjectID
        ttSort.fieldLabel       = ttSubjectColumn.fieldLabel
        ttSort.sortOrder        = iOrder
        rRowID                  = ROWID(ttSort)
        ttSubjectColumn.sortCol = iOrder
        .
    {&OPEN-QUERY-sortBrowse}
    REPOSITION sortBrowse TO ROWID rRowID.
    RUN pUpdateMode (YES).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAdvanced s-object 
PROCEDURE pAdvanced :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE BUFFER bttSubjectColumn FOR ttSubjectColumn.

    ASSIGN
        lAdvanced = ttSubjectColumn.fieldName:VISIBLE IN BROWSE subjectColumnBrowse
        lAdvanced = NOT lAdvanced
        ttSubjectColumn.sortOrder:VISIBLE      = lAdvanced
        ttSubjectColumn.groupCalc:VISIBLE      = lAdvanced
        ttSubjectColumn.fieldName:VISIBLE      = lAdvanced
        ttSubjectColumn.fieldFormat:VISIBLE    = lAdvanced
        BROWSE subjectColumnBrowse:WIDTH       = IF lAdvanced THEN 86 ELSE 42
        searchBar:WIDTH IN FRAME {&FRAME-NAME} = BROWSE subjectColumnBrowse:WIDTH
        .
     FOR EACH bttSubjectColumn:
         bttSubjectColumn.allData = bttSubjectColumn.fieldLabel
                                  + (IF lAdvanced THEN "|" + bttSubjectColumn.fieldName
                                     ELSE "")
                                  .
     END. /* each bttsubjectcolumn */
     {&OPEN-QUERY-subjectColumnBrowse}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pMoveCol s-object 
PROCEDURE pMoveCol :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiMove AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE iCurrent   AS INTEGER NO-UNDO.
    DEFINE VARIABLE iMoveTo    AS INTEGER NO-UNDO.
    DEFINE VARIABLE iSubjectID AS INTEGER NO-UNDO.
    DEFINE VARIABLE rRowID     AS ROWID   NO-UNDO.
    
    DEFINE BUFFER bttSubjectColumn FOR ttSubjectColumn.
    
    IF NOT AVAILABLE dynSubject THEN RETURN.
    iSubjectID = dynSubject.subjectID.
    {AOA/includes/pMove.i "ttSubjectColumn" "subjectColumnBrowse"}
    RUN pUpdateMode (YES).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pMoveSort s-object 
PROCEDURE pMoveSort :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiMove AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE iCurrent   AS INTEGER NO-UNDO.
    DEFINE VARIABLE iMoveTo    AS INTEGER NO-UNDO.
    DEFINE VARIABLE iSubjectID AS INTEGER NO-UNDO.
    DEFINE VARIABLE rRowID     AS ROWID   NO-UNDO.
    
    DEFINE BUFFER bttSort FOR ttSort.
    
    IF NOT AVAILABLE dynSubject THEN RETURN.
    iSubjectID = dynSubject.subjectID.
    {AOA/includes/pMove.i "ttSort" "sortBrowse"}
    RUN pUpdateMode (YES).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRemoveGroup s-object 
PROCEDURE pRemoveGroup :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cGroupCalc AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx        AS INTEGER   NO-UNDO.

    DEFINE BUFFER bttSubjectColumn FOR ttSubjectColumn.
    
    IF NOT AVAILABLE ttGroup THEN RETURN.
    FIND FIRST bttSubjectColumn
         WHERE ROWID(bttSubjectColumn) EQ ttGroup.colRowID
         NO-ERROR.
    IF NOT AVAILABLE bttSubjectColumn THEN RETURN.
    ASSIGN
        bttSubjectColumn.groupLabel = ""
        bttSubjectColumn.isGroup = NO
        .
    /* locate any records using the group removed */
    FOR EACH bttSubjectColumn
        WHERE bttSubjectColumn.subjectID EQ ttGroup.subjectID
          AND INDEX(bttSubjectColumn.groupCalc,ttGroup.fieldLabel) NE 0
        :
        DO idx =  1 TO NUM-ENTRIES(bttSubjectColumn.groupCalc):
            IF INDEX(ENTRY(idx,bttSubjectColumn.groupCalc),ttGroup.fieldLabel) NE 0 THEN
            ASSIGN
                ENTRY(idx + 1,bttSubjectColumn.groupCalc) = ""
                ENTRY(idx,bttSubjectColumn.groupCalc) = ""
                .
        END. /* do idx */
        ASSIGN
            /* remove emptry entries */
            bttSubjectColumn.groupCalc = REPLACE(bttSubjectColumn.groupCalc,",,",",")
            /* remove entry entry on the end */
            bttSubjectColumn.groupCalc = TRIM(bttSubjectColumn.groupCalc,",")
            .
    END. /* each bttSubjectColumn */
    DELETE ttGroup.
    BROWSE groupBrowse:DELETE-CURRENT-ROW ().
    FIND FIRST ttGroup NO-ERROR.
    IF AVAILABLE ttGroup THEN
    BROWSE groupBrowse:REFRESH().
    BROWSE subjectColumnBrowse:REFRESH() NO-ERROR.
    RUN pUpdateMode (YES).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRemoveSort s-object 
PROCEDURE pRemoveSort :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.

    DEFINE BUFFER bttSubjectColumn FOR ttSubjectColumn.
    
    IF NOT AVAILABLE ttSort THEN RETURN.
    FIND FIRST bttSubjectColumn
         WHERE ROWID(bttSubjectColumn) EQ ttSort.colRowID
         NO-ERROR.
    IF NOT AVAILABLE bttSubjectColumn THEN RETURN.
    ASSIGN
        bttSubjectColumn.sortCol = 0
        bttSubjectColumn.sortDescending = NO
        .
    DELETE ttSort.
    BROWSE sortBrowse:DELETE-CURRENT-ROW ().
    FOR EACH ttSort:
        ASSIGN
            idx = idx + 1
            ttSort.sortOrder = idx
            .
        IF idx GT 10 THEN LEAVE.
    END. /* each ttsort */
    IF idx GT 0 THEN
    BROWSE sortBrowse:REFRESH().
    RUN pUpdateMode (YES).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReset s-object 
PROCEDURE pReset :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RUN pUserColumns.
    RUN pUpdateMode (YES).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSave s-object 
PROCEDURE pSave :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.
    
    DEFINE BUFFER ttSubjectColumn FOR ttSubjectColumn.
    
    APPLY "ROW-LEAVE":U TO BROWSE subjectColumnBrowse.
    APPLY "ROW-LEAVE":U TO BROWSE sortBrowse.
    APPLY "ROW-LEAVE":U TO BROWSE groupBrowse.
    DO TRANSACTION:
        FOR EACH dynValueColumn EXCLUSIVE-LOCK
            WHERE dynValueColumn.subjectID    EQ dynParamValue.subjectID
              AND dynValueColumn.user-id      EQ dynParamValue.user-id
              AND dynValueColumn.prgmName     EQ dynParamValue.prgmName
              AND dynValueColumn.paramValueID EQ dynParamValue.paramValueID
               BY dynValueColumn.sortOrder
            :
            DELETE dynValueColumn.
        END. /* each dynvaluecolumn */
        FOR EACH ttSubjectColumn
              BY ttSubjectColumn.sortOrder
            :
            FIND FIRST ttSort
                 WHERE ttSort.colRowID EQ ROWID(ttSubjectColumn)
                 NO-ERROR.
            IF AVAILABLE ttSort THEN
            ASSIGN
                ttSubjectColumn.sortCol        = ttSort.sortOrder
                ttSubjectColumn.sortDescending = ttSort.sortDescending
                .
            FIND FIRST ttGroup
                 WHERE ttGroup.colRowID EQ ROWID(ttSubjectColumn)
                 NO-ERROR.
            IF AVAILABLE ttGroup THEN
            ASSIGN
                ttSubjectColumn.groupLabel = ttGroup.groupLabel
                ttSubjectColumn.isGroup = YES
                .
            CREATE dynValueColumn.
            ASSIGN
                dynValueColumn.subjectID         = dynParamValue.subjectID
                dynValueColumn.user-id           = dynParamValue.user-id
                dynValueColumn.prgmName          = dynParamValue.prgmName
                dynValueColumn.paramValueID      = dynParamValue.paramValueID
                dynValueColumn.sortOrder         = ttSubjectColumn.sortOrder
                dynValueColumn.calcFormula       = ttSubjectColumn.calcFormula
                dynValueColumn.calcParam         = ttSubjectColumn.calcParam
                dynValueColumn.calcProc          = ttSubjectColumn.calcProc
                dynValueColumn.cellColor         = ttSubjectColumn.cellColor
                dynValueColumn.colFormat         = ttSubjectColumn.fieldFormat
                dynValueColumn.colLabel          = ttSubjectColumn.fieldLabel
                dynValueColumn.colName           = ttSubjectColumn.fieldName
                dynValueColumn.columnSize        = ttSubjectColumn.columnSize
                dynValueColumn.compareValue      = ttSubjectColumn.compareValue
                dynValueColumn.dataType          = ttSubjectColumn.dataType
                dynValueColumn.filterInitValue   = ttSubjectColumn.filterInitValue
                dynValueColumn.groupCalc         = ttSubjectColumn.groupCalc
                dynValueColumn.groupLabel        = ttSubjectColumn.groupLabel
                dynValueColumn.isActive          = ttSubjectColumn.isActive
                dynValueColumn.isCalcField       = ttSubjectColumn.isCalcField
                dynValueColumn.isFilterInitField = ttSubjectColumn.isFilterInitField
                dynValueColumn.isGroup           = ttSubjectColumn.isGroup
                dynValueColumn.isReturnValue     = ttSubjectColumn.isReturnValue
                dynValueColumn.isSearchable      = ttSubjectColumn.isSearchable
                dynValueColumn.isSortable        = ttSubjectColumn.isSortable
                dynValueColumn.isStatusField     = ttSubjectColumn.isStatusField
                dynValueColumn.sortCol           = ttSubjectColumn.sortCol
                dynValueColumn.sortDescending    = ttSubjectColumn.sortDescending
                dynValueColumn.statusAction      = ttSubjectColumn.statusAction
                dynValueColumn.statusCompare     = ttSubjectColumn.statusCompare
                dynValueColumn.textColor         = ttSubjectColumn.textColor
                .
        END. /* each ttSubjectColumn */
    END. /* do trans */
    IF CAN-FIND(FIRST ttSubjectColumn) THEN
    BROWSE subjectColumnBrowse:REFRESH().
    IF CAN-FIND(FIRST ttSort) THEN
    BROWSE sortBrowse:REFRESH().
    IF CAN-FIND(FIRST ttGroup) THEN
    BROWSE groupBrowse:REFRESH().
    RUN pUpdateMode (NO).
    RUN pSetSortGroupButtons (YES).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetSortGroupButtons s-object 
PROCEDURE pSetSortGroupButtons :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iplSensitive AS LOGICAL NO-UNDO.
    
    IF iplSensitive THEN
    ENABLE {&SortGroupButtons} WITH FRAME {&FRAME-NAME}.
    ELSE
    DISABLE {&SortGroupButtons} WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateMode s-object 
PROCEDURE pUpdateMode :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iplUpdateMode AS LOGICAL NO-UNDO.

    ASSIGN
        lUpdateMode = iplUpdateMode
        hColumn[1] = BROWSE subjectColumnBrowse:GET-BROWSE-COLUMN(1)
        hColumn[1]:READ-ONLY = NOT lUpdateMode
        hColumn[2] = BROWSE subjectColumnBrowse:GET-BROWSE-COLUMN(2)
        hColumn[2]:READ-ONLY = NOT lUpdateMode
        hColumn[3] = BROWSE sortBrowse:GET-BROWSE-COLUMN(2)
        hColumn[3]:READ-ONLY = NOT lUpdateMode
        hColumn[4] = BROWSE groupBrowse:GET-BROWSE-COLUMN(2)
        hColumn[4]:READ-ONLY = NOT lUpdateMode
        btnReset:SENSITIVE IN FRAME {&FRAME-NAME} = lUpdateMode
        .
    btnSave:LOAD-IMAGE("Graphics/32x32/" 
        + IF lUpdateMode THEN "floppy_disk.png"
          ELSE "pencil.png").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUserColumns s-object 
PROCEDURE pUserColumns :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE idx        AS INTEGER NO-UNDO.
    DEFINE VARIABLE jdx        AS INTEGER NO-UNDO.
    DEFINE VARIABLE lSensitive AS LOGICAL NO-UNDO.

    EMPTY TEMP-TABLE ttGroup.
    EMPTY TEMP-TABLE ttSort.
    EMPTY TEMP-TABLE ttSubjectColumn.
    {&OPEN-QUERY-subjectColumnBrowse}
    {methods/run_link.i "CONTAINER" "pGetParamValueRowID" "(OUTPUT rRowID)"}
    IF rRowID EQ ? THEN RETURN.
    FIND FIRST dynParamValue NO-LOCK WHERE ROWID(dynParamValue) EQ rRowID.
    FIND FIRST dynSubject NO-LOCK
         WHERE dynSubject.subjectID EQ dynParamValue.subjectID
         NO-ERROR.
    IF NOT AVAILABLE dynSubject THEN RETURN.
    FOR EACH dynValueColumn NO-LOCK
        WHERE dynValueColumn.subjectID    EQ dynParamValue.subjectID
          AND dynValueColumn.user-id      EQ dynParamValue.user-id
          AND dynValueColumn.prgmName     EQ dynParamValue.prgmName
          AND dynValueColumn.paramValueID EQ dynParamValue.paramValueID
           BY dynValueColumn.sortOrder
        :
        CREATE ttSubjectColumn.
        ASSIGN
            ttSubjectColumn.subjectID         = dynValueColumn.subjectID
            ttSubjectColumn.sortOrder         = dynValueColumn.sortOrder     
            ttSubjectColumn.calcFormula       = dynValueColumn.calcFormula   
            ttSubjectColumn.calcParam         = dynValueColumn.calcParam     
            ttSubjectColumn.calcProc          = dynValueColumn.calcProc
            ttSubjectColumn.cellColor         = dynValueColumn.cellColor      
            ttSubjectColumn.columnSize        = dynValueColumn.columnSize    
            ttSubjectColumn.compareValue      = dynValueColumn.compareValue
            ttSubjectColumn.dataType          = dynValueColumn.dataType      
            ttSubjectColumn.fieldFormat       = dynValueColumn.colFormat     
            ttSubjectColumn.fieldLabel        = dynValueColumn.colLabel
            ttSubjectColumn.fieldName         = dynValueColumn.colName
            ttSubjectColumn.filterInitValue   = dynValueColumn.filterInitValue 
            ttSubjectColumn.groupCalc         = dynValueColumn.groupCalc     
            ttSubjectColumn.groupLabel        = dynValueColumn.groupLabel    
            ttSubjectColumn.isActive          = dynValueColumn.isActive      
            ttSubjectColumn.isCalcField       = dynValueColumn.isCalcField
            ttSubjectColumn.isFilterInitField = dynValueColumn.isFilterInitField
            ttSubjectColumn.isGroup           = dynValueColumn.isGroup       
            ttSubjectColumn.isReturnValue     = dynValueColumn.isReturnValue 
            ttSubjectColumn.isSearchable      = dynValueColumn.isSearchable  
            ttSubjectColumn.isSortable        = dynValueColumn.isSortable    
            ttSubjectColumn.isStatusField     = dynValueColumn.isStatusField
            ttSubjectColumn.sortCol           = dynValueColumn.sortCol       
            ttSubjectColumn.sortDescending    = dynValueColumn.sortDescending
            ttSubjectColumn.statusAction      = dynValueColumn.statusAction
            ttSubjectColumn.statusCompare     = dynValueColumn.statusCompare
            ttSubjectColumn.textColor         = dynValueColumn.textColor
            ttSubjectColumn.allData           = ttSubjectColumn.fieldLabel
                                              + (IF lAdvanced THEN "|" + ttSubjectColumn.fieldName
                                                 ELSE "")
            .
        IF ttSubjectColumn.sortCol NE 0 THEN DO:
            CREATE ttSort.
            ASSIGN
                ttSort.colRowID       = ROWID(ttSubjectColumn)
                ttSort.subjectID      = ttSubjectColumn.subjectID
                ttSort.sortOrder      = ttSubjectColumn.sortCol
                ttSort.fieldLabel     = ttSubjectColumn.fieldLabel
                ttSort.sortDescending = ttSubjectColumn.sortDescending                
                .
        END. /* if isgroup */
        IF ttSubjectColumn.isGroup THEN DO:
            CREATE ttGroup.
            ASSIGN
                ttGroup.colRowID   = ROWID(ttSubjectColumn)
                ttGroup.subjectID  = ttSubjectColumn.subjectID
                ttGroup.fieldLabel = ttSubjectColumn.fieldLabel
                ttGroup.groupLabel = ttSubjectColumn.groupLabel
                .
        END. /* if isgroup */
        IF ttSubjectColumn.groupCalc NE "" THEN
        DO jdx = 1 TO NUM-ENTRIES(ttSubjectColumn.groupCalc) BY 2:
            CREATE ttGroupCalc.
            ASSIGN
                ttGroupCalc.subjectID = ttSubjectColumn.subjectID
                ttGroupCalc.fieldName = ttSubjectColumn.fieldName
                ttGroupCalc.groupName = ENTRY(jdx,ttSubjectColumn.groupCalc)
                ttGroupCalc.calcType  = ENTRY(jdx + 1,ttSubjectColumn.groupCalc)
                .
        END. /* do idx */
    END. /* each dynvaluecolumn */
    {&OPEN-QUERY-subjectColumnBrowse}
    {&OPEN-QUERY-sortBrowse}
    {&OPEN-QUERY-groupBrowse}

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
        BROWSE subjectColumnBrowse:HEIGHT  = ipdHeight - BROWSE subjectColumnBrowse:ROW + 1
        BROWSE groupBrowse:HEIGHT          = ipdHeight - BROWSE groupBrowse:ROW + 1
        btnRemoveGroup:ROW                 = ipdHeight - btnRemoveGroup:HEIGHT + 1
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

