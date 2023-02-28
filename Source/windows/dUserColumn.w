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

  File: windows/dUserColumn.w

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
{system/ttUserColumn.i}
{methods/template/brwcustomdef.i}
/* Parameters Definitions ---                                           */
DEFINE INPUT  PARAMETER ipoUserColumn AS system.UserColumn NO-UNDO.
DEFINE OUTPUT PARAMETER oplSave       AS LOGICAL           NO-UNDO.

DEFINE VARIABLE cProgramName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cUser        AS CHARACTER NO-UNDO.

IF VALID-OBJECT(ipoUserColumn) THEN DO:
    ASSIGN
        cProgramName = ipoUserColumn:ProgramName
        cUser        = ipoUserColumn:CurrentUser
        .
    
    ipoUserColumn:GetUserColumn(OUTPUT TABLE ttUserColumn).
END.
/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttUserColumn

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 ttUserColumn.defaultcolLabel ttUserColumn.colLabel ttUserColumn.colVisible ttUserColumn.colWidth   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2 ttUserColumn.colLabel ~
ttUserColumn.colVisible ~
ttUserColumn.colWidth   
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-2 ttUserColumn
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-2 ttUserColumn
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH ttUserColumn     BY ttUserColumn.colPosition INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY {&SELF-NAME} FOR EACH ttUserColumn     BY ttUserColumn.colPosition INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 ttUserColumn
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 ttUserColumn


/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-D-Dialog ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-2 btMoveUp btMoveDown btRestore ~
btSave 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btMoveDown 
     IMAGE-UP FILE "Graphics/32x32/next.png":U
     LABEL "Move Down" 
     SIZE 7.4 BY 1.76 TOOLTIP "Move Down"
     FONT 5.

DEFINE BUTTON btMoveUp 
     IMAGE-UP FILE "Graphics/32x32/previous.png":U
     LABEL "Move Up" 
     SIZE 7.4 BY 1.76 TOOLTIP "Move Up"
     FONT 5.

DEFINE BUTTON btRestore 
     IMAGE-UP FILE "Graphics/32x32/back_white.png":U
     LABEL "Restore Defaults" 
     SIZE 7.4 BY 1.76 TOOLTIP "Restore Defaults"
     FONT 5.

DEFINE BUTTON btSave 
     IMAGE-UP FILE "Graphics/32x32/floppy_disk.png":U
     LABEL "Save and Close" 
     SIZE 7.4 BY 1.76 TOOLTIP "Save and Close".

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      ttUserColumn SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 D-Dialog _FREEFORM
  QUERY BROWSE-2 NO-LOCK DISPLAY
      ttUserColumn.defaultcolLabel COLUMN-LABEL "Name" FORMAT "x(25)":U WIDTH 24.4
      ttUserColumn.colLabel FORMAT "x(30)":U
      ttUserColumn.colVisible FORMAT "yes/no":U WIDTH 10.2 VIEW-AS TOGGLE-BOX
      ttUserColumn.colWidth COLUMN-LABEL "Width" FORMAT ">>>>>9.99<<<<":U
            WIDTH 16.4
  ENABLE
      ttUserColumn.colLabel
      ttUserColumn.colVisible
      ttUserColumn.colWidth
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 94 BY 13.57
         BGCOLOR 15 FGCOLOR 0 FONT 5 ROW-HEIGHT-CHARS .85 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     BROWSE-2 AT ROW 1 COL 1 WIDGET-ID 200
     btMoveUp AT ROW 2.19 COL 97 WIDGET-ID 2
     btMoveDown AT ROW 4.33 COL 97 WIDGET-ID 4
     btRestore AT ROW 9.57 COL 97 WIDGET-ID 6
     btSave AT ROW 12.67 COL 97 WIDGET-ID 8
     SPACE(1.19) SKIP(0.14)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 21 FGCOLOR 15 FONT 5
         TITLE "Browse settings" WIDGET-ID 100.


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
/* BROWSE-TAB BROWSE-2 1 D-Dialog */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttUserColumn
    BY ttUserColumn.colPosition INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "ASI.userColumn.colPosition|yes"
     _Where[1]         = "ASI.userColumn.programName = cProgramName
 AND ASI.userColumn.usrId = cUser"
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
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
ON WINDOW-CLOSE OF FRAME D-Dialog /* Browse settings */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
&Scoped-define SELF-NAME BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 D-Dialog
ON ROW-DISPLAY OF BROWSE-2 IN FRAME D-Dialog
DO:
    {methods/template/brwrowdisplay.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btMoveDown
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btMoveDown D-Dialog
ON CHOOSE OF btMoveDown IN FRAME D-Dialog /* Move Down */
DO:
    RUN pMove("Down").  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btMoveUp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btMoveUp D-Dialog
ON CHOOSE OF btMoveUp IN FRAME D-Dialog /* Move Up */
DO:
    RUN pMove("Up").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btRestore
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btRestore D-Dialog
ON CHOOSE OF btRestore IN FRAME D-Dialog /* Restore Defaults */
DO:
    ipoUserColumn:RestoreDefaults().
    ipoUserColumn:GetUserColumn(OUTPUT TABLE ttUserColumn).
    
    {&OPEN-QUERY-{&BROWSE-NAME}}   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSave D-Dialog
ON CHOOSE OF btSave IN FRAME D-Dialog /* Save and Close */
DO:
    ipoUserColumn:SetUserColumn(INPUT TABLE ttUserColumn).
    
    oplSave = TRUE.
    
    APPLY "GO" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

{src/adm/template/dialogmn.i}
{methods/template/brwcustom.i}

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
  ENABLE BROWSE-2 btMoveUp btMoveDown btRestore btSave 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetUserColumn D-Dialog 
PROCEDURE GetUserColumn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opoUserColumn AS system.UserColumn NO-UNDO.
    
    IF VALID-OBJECT(ipoUserColumn) THEN
        opoUserColumn = ipoUserColumn.
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
    FRAME {&FRAME-NAME}:TITLE = FRAME {&FRAME-NAME}:TITLE + " for Program: '" + cProgramName + "' User:'" + cUser + "'" .

    ASSIGN
        {&BROWSE-NAME}:BGCOLOR           = 25
        {&BROWSE-NAME}:FGCOLOR           = 0
        {&BROWSE-NAME}:SEPARATOR-FGCOLOR = 15
        {&BROWSE-NAME}:ROW-HEIGHT-CHARS  = 0.84
        {&BROWSE-NAME}:FONT              = 22
        .

    hColumnRowColor = {&BROWSE-NAME}:FIRST-COLUMN.
    DO WHILE VALID-HANDLE(hColumnRowColor):
        ASSIGN
            cColHandList    = cColHandList + ","  + string(hColumnRowColor)
            hColumnRowColor = hColumnRowColor:NEXT-COLUMN
            .
    END. /* do while */
    cColHandList = TRIM(cColHandList, ",").
        
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .

    /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pMove D-Dialog 
PROCEDURE pMove :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcMove AS CHARACTER NO-UNDO.

    DEFINE VARIABLE riCurrentRow  AS ROWID   NO-UNDO.
    DEFINE VARIABLE iFromPosition AS INTEGER NO-UNDO.
    DEFINE VARIABLE iToPosition   AS INTEGER NO-UNDO.

    DEFINE BUFFER bf-ttUserColumn FOR ttUserColumn.
    
    IF NOT AVAILABLE ttUserColumn THEN
        RETURN.
    
    IF ipcMove EQ "Up" THEN
        FIND LAST bf-ttUserColumn
             WHERE bf-ttUserColumn.programName EQ ttUserColumn.programName
               AND bf-ttUserColumn.usrID       EQ ttUserColumn.usrID
               AND bf-ttUserColumn.colPosition LT ttUserColumn.colPosition
             NO-ERROR.
    ELSE IF ipcMove EQ "Down" THEN
        FIND FIRST bf-ttUserColumn
             WHERE bf-ttUserColumn.programName EQ ttUserColumn.programName
               AND bf-ttUserColumn.usrID       EQ ttUserColumn.usrID
               AND bf-ttUserColumn.colPosition GT ttUserColumn.colPosition
             NO-ERROR.
    
    IF NOT AVAILABLE bf-ttUserColumn OR (AVAILABLE bf-ttUserColumn AND ROWID(ttUserColumn) EQ ROWID(bf-ttUserColumn)) THEN
        RETURN.
    
    IF AVAILABLE bf-ttUserColumn AND AVAILABLE ttUserColumn THEN DO:
        ASSIGN
            riCurrentRow                = ROWID(ttUserColumn)
            iFromPosition               = ttUserColumn.colPosition
            iToPosition                 = bf-ttUserColumn.colPosition
            ttUserColumn.colPosition    = iToPosition
            bf-ttUserColumn.colPosition = iFromPosition
            .

        {&OPEN-QUERY-{&BROWSE-NAME}}
        
        REPOSITION {&BROWSE-NAME} TO ROWID riCurrentRow NO-ERROR.      
    END. 
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
  {src/adm/template/snd-list.i "ttUserColumn"}

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

