&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File: system/d-tableFieldPicker.w

  Description: from cntnrdlg.w - ADM SmartDialog Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
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
DEFINE OUTPUT PARAMETER opcTableName       AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcFieldName       AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opiFieldExtent     AS INTEGER   NO-UNDO.
DEFINE OUTPUT PARAMETER oplIsFieldSelected AS LOGICAL   NO-UNDO.

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE TEMP-TABLE ttTable NO-UNDO
    FIELD tableName AS CHARACTER
    .
    
DEFINE TEMP-TABLE ttField NO-UNDO
    FIELD fieldName       AS CHARACTER
    FIELD fieldWithExtent AS CHARACTER
    FIELD fieldExtent     AS INTEGER
    .

{methods/template/brwcustomdef.i}

DEFINE VARIABLE hdBrowseTableColumn AS HANDLE NO-UNDO.
DEFINE VARIABLE hdBrowseFieldColumn AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog
&Scoped-define BROWSE-NAME BROWSE-FIELD

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttField ttTable

/* Definitions for BROWSE BROWSE-FIELD                                  */
&Scoped-define FIELDS-IN-QUERY-BROWSE-FIELD ttField.fieldWithExtent   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-FIELD   
&Scoped-define SELF-NAME BROWSE-FIELD
&Scoped-define QUERY-STRING-BROWSE-FIELD FOR EACH ttField WHERE ttField.fieldWithExtent BEGINS fiField
&Scoped-define OPEN-QUERY-BROWSE-FIELD OPEN QUERY {&SELF-NAME} FOR EACH ttField WHERE ttField.fieldWithExtent BEGINS fiField.
&Scoped-define TABLES-IN-QUERY-BROWSE-FIELD ttField
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-FIELD ttField


/* Definitions for BROWSE BROWSE-TABLE                                  */
&Scoped-define FIELDS-IN-QUERY-BROWSE-TABLE ttTable.tableName   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-TABLE   
&Scoped-define SELF-NAME BROWSE-TABLE
&Scoped-define QUERY-STRING-BROWSE-TABLE FOR EACH ttTable WHERE ttTable.tableName BEGINS fiTable
&Scoped-define OPEN-QUERY-BROWSE-TABLE OPEN QUERY {&SELF-NAME} FOR EACH ttTable WHERE ttTable.tableName BEGINS fiTable.
&Scoped-define TABLES-IN-QUERY-BROWSE-TABLE ttTable
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-TABLE ttTable


/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-D-Dialog ~
    ~{&OPEN-QUERY-BROWSE-FIELD}~
    ~{&OPEN-QUERY-BROWSE-TABLE}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btCancel fiTable fiField BROWSE-TABLE ~
BROWSE-FIELD btOk 
&Scoped-Define DISPLAYED-OBJECTS fiTable fiField 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btCancel AUTO-END-KEY 
     IMAGE-UP FILE "Graphics/32x32/navigate_cross.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Cancel" 
     SIZE 8 BY 1.91.

DEFINE BUTTON btOk AUTO-GO 
     IMAGE-UP FILE "Graphics/32x32/navigate_check.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Ok" 
     SIZE 8 BY 1.91.

DEFINE VARIABLE fiField AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 70 BY 1 NO-UNDO.

DEFINE VARIABLE fiTable AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 60 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-FIELD FOR 
      ttField SCROLLING.

DEFINE QUERY BROWSE-TABLE FOR 
      ttTable SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-FIELD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-FIELD D-Dialog _FREEFORM
  QUERY BROWSE-FIELD DISPLAY
      ttField.fieldWithExtent COLUMN-LABEL "Field" FORMAT "X(50)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 70 BY 29.29
         BGCOLOR 25 FGCOLOR 0 FONT 22 ROW-HEIGHT-CHARS .84 FIT-LAST-COLUMN.

DEFINE BROWSE BROWSE-TABLE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-TABLE D-Dialog _FREEFORM
  QUERY BROWSE-TABLE DISPLAY
      ttTable.tableName COLUMN-LABEL "Table" FORMAT "X(50)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 60 BY 29.29
         BGCOLOR 25 FGCOLOR 0 FONT 22 ROW-HEIGHT-CHARS .84 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     btCancel AT ROW 4.33 COL 134 WIDGET-ID 8
     fiTable AT ROW 1 COL 2.2 NO-LABEL WIDGET-ID 2
     fiField AT ROW 1 COL 61 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     BROWSE-TABLE AT ROW 2.19 COL 2 WIDGET-ID 200
     BROWSE-FIELD AT ROW 2.19 COL 63 WIDGET-ID 300
     btOk AT ROW 1.95 COL 134 WIDGET-ID 6
     SPACE(1.19) SKIP(27.70)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 15 FONT 6
         TITLE "Select Table Field" WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-TABLE fiField D-Dialog */
/* BROWSE-TAB BROWSE-FIELD BROWSE-TABLE D-Dialog */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

ASSIGN 
       BROWSE-FIELD:SEPARATOR-FGCOLOR IN FRAME D-Dialog      = 15.

ASSIGN 
       BROWSE-TABLE:SEPARATOR-FGCOLOR IN FRAME D-Dialog      = 15.

/* SETTINGS FOR FILL-IN fiTable IN FRAME D-Dialog
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-FIELD
/* Query rebuild information for BROWSE BROWSE-FIELD
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttField WHERE ttField.fieldWithExtent BEGINS fiField.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-FIELD */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-TABLE
/* Query rebuild information for BROWSE BROWSE-TABLE
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttTable WHERE ttTable.tableName BEGINS fiTable.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-TABLE */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Select Table Field */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-FIELD
&Scoped-define SELF-NAME BROWSE-FIELD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-FIELD D-Dialog
ON ROW-DISPLAY OF BROWSE-FIELD IN FRAME D-Dialog
DO:
    IF VALID-HANDLE(hdBrowseFieldColumn) THEN
        hdBrowseFieldColumn:BGCOLOR = IF CURRENT-RESULT-ROW("BROWSE-TABLE") MOD 2 EQ 0 THEN 25
                                      ELSE 26.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-TABLE
&Scoped-define SELF-NAME BROWSE-TABLE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-TABLE D-Dialog
ON ROW-DISPLAY OF BROWSE-TABLE IN FRAME D-Dialog
DO:
    IF VALID-HANDLE(hdBrowseTableColumn) THEN
        hdBrowseTableColumn:BGCOLOR = IF CURRENT-RESULT-ROW("BROWSE-TABLE") MOD 2 EQ 0 THEN 25
                                      ELSE 26.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-TABLE D-Dialog
ON VALUE-CHANGED OF BROWSE-TABLE IN FRAME D-Dialog
DO:
    RUN pBuildTableFields (IF AVAILABLE ttTable THEN ttTable.tableName ELSE "").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btOk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btOk D-Dialog
ON CHOOSE OF btOk IN FRAME D-Dialog /* Ok */
DO:
    IF AVAILABLE ttField AND AVAILABLE ttTable THEN
        ASSIGN
            opcTableName       = ttTable.tableName
            opcFieldName       = ttField.fieldName
            opiFieldExtent     = ttField.fieldExtent
            oplIsFieldSelected = TRUE
            .        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiField
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiField D-Dialog
ON VALUE-CHANGED OF fiField IN FRAME D-Dialog
DO:
    ASSIGN fiField.
    
   {&OPEN-QUERY-BROWSE-FIELD}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiTable
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTable D-Dialog
ON VALUE-CHANGED OF fiTable IN FRAME D-Dialog
DO:
    ASSIGN fiTable.
    
    {&OPEN-QUERY-BROWSE-TABLE}
    
    APPLY "VALUE-CHANGED" TO BROWSE-TABLE IN FRAME {&FRAME-NAME}.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-FIELD
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

{src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog  _DEFAULT-DISABLE
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
  HIDE FRAME D-Dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog  _DEFAULT-ENABLE
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
  DISPLAY fiTable fiField 
      WITH FRAME D-Dialog.
  ENABLE btCancel fiTable fiField BROWSE-TABLE BROWSE-FIELD btOk 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable D-Dialog 
PROCEDURE local-enable :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    RUN pInit.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBuildTableFields D-Dialog 
PROCEDURE pBuildTableFields PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcTable AS CHARACTER.
    
    DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.
    
    EMPTY TEMP-TABLE ttField.
    
    FIND FIRST ASI._file NO-LOCK
         WHERE ASI._file._file-Name EQ ipcTable
         NO-ERROR.
    IF NOT AVAILABLE ASI._file THEN
        RETURN.
        
    FOR EACH ASI._field NO-LOCK
         WHERE ASI._field._file-recid = RECID(ASI._file):
        IF ASI._field._Extent GT 1 THEN DO:
            DO iIndex = 1 TO ASI._field._Extent:
                CREATE ttField.
                ASSIGN
                    ttField.fieldName       = ASI._field._Field-Name
                    ttField.fieldExtent     = iIndex
                    ttField.fieldWithExtent = ASI._field._Field-Name + "[" + STRING(iIndex) + "]"
                    .
            END.
        END.
        ELSE DO:
            CREATE ttField.
            ASSIGN
                ttField.fieldName       = ASI._field._Field-Name
                ttField.fieldExtent     = iIndex
                ttField.fieldWithExtent = ASI._field._Field-Name
                .
        END.
    END.
    
    {&OPEN-QUERY-BROWSE-FIELD}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInit D-Dialog 
PROCEDURE pInit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    ASSIGN
        hdBrowseTableColumn = BROWSE BROWSE-TABLE:GET-BROWSE-COLUMN(1)
        hdBrowseFieldColumn = BROWSE BROWSE-FIELD:GET-BROWSE-COLUMN(1)
        NO-ERROR.
        
    FOR EACH ASI._File:
        CREATE ttTable.
        ttTable.tableName = ASI._File._File-Name.
    END.
    
    {&OPEN-QUERY-BROWSE-TABLE}
    
    APPLY "VALUE-CHANGED" TO BROWSE-TABLE IN FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records D-Dialog  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "ttTable"}
  {src/adm/template/snd-list.i "ttField"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed D-Dialog 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

