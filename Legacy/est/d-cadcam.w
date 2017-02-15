&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: est/d-cadcam.w

  Description: from cntnrdlg.w - ADM SmartDialog Template

  Input Parameters: cadcam comma delimited field list

  Output Parameters: cadcam comma delimited field list and associated values

  Author: Ron Stark

  Created: 3.1.2005
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

&IF DEFINED(UIB_is_Running) EQ 0 &THEN
DEFINE INPUT PARAMETER ipCompany AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER opCADCAM AS CHARACTER NO-UNDO.
&ELSE
DEFINE VARIABLE ipCompany AS CHARACTER NO-UNDO INITIAL '001'.
DEFINE VARIABLE opCADCAM AS CHARACTER NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */

DEFINE TEMP-TABLE ttblCADCAM NO-UNDO
  FIELD board LIKE ef.board
  FIELD boardID AS INTEGER
  FIELD cad-no LIKE eb.cad-no
  FIELD cal LIKE ef.cal
  FIELD dep LIKE eb.dep
  FIELD die-no LIKE eb.die-no
  FIELD len LIKE eb.len
  FIELD lin-in LIKE eb.lin-in
  FIELD style LIKE eb.style
  FIELD t-len LIKE eb.t-len
  FIELD t-sqin LIKE eb.t-sqin
  FIELD t-wid LIKE eb.t-wid
  FIELD weight LIKE ef.weight
  FIELD wid LIKE eb.wid.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME D-Dialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS cadNumber dieNumber Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS cadNumber dieNumber 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "&Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "&OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE cadNumber LIKE eb.cad-no
     LABEL "CAD#" 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1 NO-UNDO.

DEFINE VARIABLE dieNumber LIKE eb.die-no
     LABEL "DIE#" 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     cadNumber AT ROW 1.24 COL 7 COLON-ALIGNED
          LABEL "CAD#"
     dieNumber AT ROW 2.43 COL 7 COLON-ALIGNED
          LABEL "DIE#"
     Btn_OK AT ROW 3.62 COL 18
     Btn_Cancel AT ROW 3.62 COL 34
     SPACE(7.19) SKIP(0.09)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Create Estimate from CADCAM Software"
         CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: COMPILE
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
                                                                        */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN cadNumber IN FRAME D-Dialog
   LIKE = asi.eb.cad-no EXP-LABEL EXP-SIZE                              */
/* SETTINGS FOR FILL-IN dieNumber IN FRAME D-Dialog
   LIKE = asi.eb.die-no EXP-LABEL EXP-SIZE                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Create Estimate from CADCAM Software */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel D-Dialog
ON CHOOSE OF Btn_Cancel IN FRAME D-Dialog /* Cancel */
DO:
  opCADCAM = ''.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK D-Dialog
ON CHOOSE OF Btn_OK IN FRAME D-Dialog /* OK */
DO:
  ASSIGN cadNumber dieNumber.
  IF SEARCH('c:/fibre/cadcam.txt') NE ? THEN
  RUN getCADCAM2 (ipCompany,cadNumber,dieNumber).
  ELSE
  RUN getCADCAM (ipCompany,cadNumber,dieNumber).
  RUN loadCADCAM.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
  DISPLAY cadNumber dieNumber 
      WITH FRAME D-Dialog.
  ENABLE cadNumber dieNumber Btn_OK Btn_Cancel 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getCADCAM D-Dialog 
PROCEDURE getCADCAM :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  input cad# and die#
  Notes:       ttblCADCAM values extracted and used in ce/b-estitem.w
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipCompany AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipCADNumber AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipDieNumber AS CHARACTER NO-UNDO.

  DEFINE VARIABLE lvBoardID AS INTEGER NO-UNDO.
  DEFINE VARIABLE hRecordSet AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE hConnection AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE hCommand AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE odbcDSN AS CHARACTER NO-UNDO.
  DEFINE VARIABLE odbcServer AS CHARACTER NO-UNDO.
  DEFINE VARIABLE odbcUserID AS CHARACTER NO-UNDO.
  DEFINE VARIABLE odbcPassword AS CHARACTER NO-UNDO.
  DEFINE VARIABLE odbcQuery AS CHARACTER NO-UNDO.
  DEFINE VARIABLE odbcStatus AS CHARACTER NO-UNDO.
  DEFINE VARIABLE odbcRecCount AS INTEGER NO-UNDO.
  DEFINE VARIABLE odbcNull AS CHARACTER NO-UNDO.
  DEFINE VARIABLE odbcCursor AS INTEGER NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  
  FIND sys-ctrl NO-LOCK WHERE sys-ctrl.company EQ ipCompany
                          AND sys-ctrl.name EQ 'Artios' NO-ERROR.
  IF NOT AVAILABLE sys-ctrl THEN
  DO:
    MESSAGE 'No System Control Record for Artios Exists!' SKIP
      'Unable to connect to CADCAM Database.' VIEW-AS ALERT-BOX.
    RETURN.
  END.

  /* Create the connection object for the link to SQL */
  CREATE 'ADODB.Connection' hConnection.
  /* Create a recordset object ready to return the data */
  CREATE 'ADODB.RecordSet' hRecordSet.
  /* Create a command object for sending the SQL statement */
  CREATE 'ADODB.Command' hCommand.
  /* Change the below values as necessary */
  ASSIGN
    odbcDSN = ENTRY(1,sys-ctrl.char-fld) /* The ODBC DSN */
    odbcServer = ENTRY(2,sys-ctrl.char-fld) /* The name of the server hosting the SQL DB and DSN */
    odbcUserID = '' /* The user id for access to the SQL Database */
    odbcPassword = ''. /* Password required by above user-id */
  /* Open up the connection to the ODBC Layer */
  hConnection:Open ('data source=' + odbcDSN + ';server=' +
                     odbcServer,odbcUserID,odbcPassword,0) NO-ERROR.
  
  IF ERROR-STATUS:ERROR THEN
  MESSAGE 'Error:' ERROR-STATUS:NUM-MESSAGES VIEW-AS ALERT-BOX.
  
  /* Check for connection errors */
  IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
  DO i = 1 TO ERROR-STATUS:NUM-MESSAGES:
    MESSAGE ERROR-STATUS:GET-NUMBER(i)
            ERROR-STATUS:GET-MESSAGE(i) VIEW-AS ALERT-BOX.
  END.
  ELSE
  DO:
    CREATE ttblCADCAM.
    ttblCADCAM.die-no = ipDieNumber.
    ASSIGN
      odbcQuery = 'SELECT * FROM design, board WHERE board.boardid = design.boardid'
      hCommand:ActiveConnection  = hConnection
      hCommand:CommandText = odbcQuery
      hCommand:CommandType = 1 /* adCmdText */
      hConnection:CursorLocation = 3 /* adUseClient */
      hRecordSet:CursorType = 3 /* adOpenStatic */
      hRecordSet = hCommand:Execute (OUTPUT odbcNull,'',32)
      odbcRecCount = hRecordSet:RecordCount.
    /* Have we returned any rows ? */
    If odbcRecCount GT 0 AND NOT odbcRecCount EQ ? THEN
    DO:
      hRecordSet:MoveFirst no-error.
      DO WHILE odbcCursor LT odbcRecCount:
        ttblCADCAM.cad-no = hRecordSet:Fields ('DESIGNNAME'):VALUE.
        IF ttblCADCAM.cad-no EQ ipCadNumber THEN
        DO:
          ASSIGN
            ttblCADCAM.board = hRecordSet:Fields ('BOARDCODE'):VALUE
            ttblCADCAM.cal = hRecordSet:Fields ('CALIPER'):VALUE
            ttblCADCAM.dep = hRecordSet:Fields ('DEPTH'):VALUE
            ttblCADCAM.len = hRecordSet:Fields ('LENGTH'):VALUE
            ttblCADCAM.lin-in = hRecordSet:Fields ('RULELENGTH'):VALUE
            ttblCADCAM.style = '' /* hRecordSet:Fields ('STYLE'):VALUE */
            ttblCADCAM.t-len = hRecordSet:Fields ('BLANKLENGTH'):VALUE
            ttblCADCAM.t-sqin = hRecordSet:Fields ('AREA'):VALUE
            ttblCADCAM.t-wid = hRecordSet:Fields ('BLANKHEIGHT'):VALUE
            ttblCADCAM.weight = hRecordSet:Fields ('BASISWEIGHT'):VALUE
            ttblCADCAM.wid = hRecordSet:Fields ('WIDTH'):VALUE.
          LEAVE.
        END.
        odbcCursor = odbcCursor + 1.
        hRecordSet:MoveNext NO-ERROR.
      END. /* retrieved a single data row */
    END. /* retrieved all data rows */
    ELSE odbcStatus = 'No records found.'.

    /* Close the ADO connection */
    hConnection:Close no-error.
  END. /* The connection opened correctly */
  
  /* Release the memory!! */
  RELEASE OBJECT hConnection NO-ERROR.
  RELEASE OBJECT hCommand NO-ERROR.
  RELEASE OBJECT hRecordSet NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getCADCAM2 D-Dialog 
PROCEDURE getCADCAM2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipCompany AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipCADNumber AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipDieNumber AS CHARACTER NO-UNDO.

  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE hAccess AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE tables AS CHARACTER NO-UNDO.
  DEFINE VARIABLE hFile AS CHARACTER NO-UNDO.
  DEFINE VARIABLE hTable AS CHARACTER NO-UNDO.

  DEFINE VARIABLE lvBoardID AS INTEGER NO-UNDO.

  CREATE ttblCADCAM.
  ASSIGN
    ttblCADCAM.die-no = ipDieNumber
    tables = 'Design,Board'.
  CREATE 'Access.Application' hAccess CONNECT TO 'c:\fibre\dcenter.mdb'.
  DO i = 1 TO NUM-ENTRIES(tables):
    ASSIGN
      hTable = CAPS(ENTRY(i,tables))
      hFile = 'c:\fibre\' + ENTRY(i,tables) + '.txt'.
    hAccess:application:docmd:TransferText (2,,hTable,hFile,TRUE,).
  END.
  RELEASE OBJECT hAccess.
  DO i = 1 TO NUM-ENTRIES(tables):
    INPUT FROM VALUE('c:\fibre\' + ENTRY(i,tables) + '.txt') NO-ECHO.
    IMPORT DELIMITER ',' ^.
    REPEAT:
      CASE ENTRY(i,tables):
        WHEN 'Board' THEN
        DO:
          IMPORT DELIMITER ',' lvBoardID
            ttblCADCAM.board
            ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^
            ttblCADCAM.weight.
          IF ttblCADCAM.boardID EQ boardID THEN LEAVE.
        END.
        WHEN 'Design' THEN
        DO:
          IMPORT DELIMITER ','
            ttblCADCAM.cad-no
            ^ ^ ^ ^ ^
            ttblCADCAM.boardID
            ^ ^ ^ ^ ^ ^
            ttblCADCAM.len
            ttblCADCAM.wid
            ttblCADCAM.dep
            ttblCADCAM.t-len
            ttblCADCAM.t-wid
            ttblCADCAM.t-sqin
            ttblCADCAM.cal
            ttblCADCAM.lin-in.
          IF ttblCADCAM.cad-no EQ ipCadNumber THEN LEAVE.
        END.
      END CASE.
    END.
    INPUT CLOSE.
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadCADCAM D-Dialog 
PROCEDURE loadCADCAM :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  DO i = 1 TO NUM-ENTRIES(opCADCAM) BY 2:
    CASE ENTRY(i,opCADCAM):
      WHEN 'board' THEN
      ENTRY(i + 1,opCADCAM) = ttblCADCAM.board.
      WHEN 'cad-no' THEN
      ENTRY(i + 1,opCADCAM) = ttblCADCAM.cad-no.
      WHEN 'cal' THEN
      ENTRY(i + 1,opCADCAM) = STRING(ttblCADCAM.cal).
      WHEN 'dep' THEN
      ENTRY(i + 1,opCADCAM) = STRING(ttblCADCAM.dep).
      WHEN 'die-no' THEN
      ENTRY(i + 1,opCADCAM) = ttblCADCAM.die-no.
      WHEN 'len' THEN
      ENTRY(i + 1,opCADCAM) = STRING(ttblCADCAM.len).
      WHEN 'lin-in' THEN
      ENTRY(i + 1,opCADCAM) = STRING(ttblCADCAM.lin-in).
      WHEN 'style' THEN
      ENTRY(i + 1,opCADCAM) = ttblCADCAM.style.
      WHEN 't-len' THEN
      ENTRY(i + 1,opCADCAM) = STRING(ttblCADCAM.t-len).
      WHEN 't-sqin' THEN
      ENTRY(i + 1,opCADCAM) = STRING(ttblCADCAM.t-sqin).
      WHEN 't-wid' THEN
      ENTRY(i + 1,opCADCAM) = STRING(ttblCADCAM.t-wid).
      WHEN 'weight' THEN
      ENTRY(i + 1,opCADCAM) = STRING(ttblCADCAM.weight).
      WHEN 'wid' THEN
      ENTRY(i + 1,opCADCAM) = STRING(ttblCADCAM.wid).
    END CASE.
  END. /* do i */

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

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartDialog, and there are no
     tables specified in any contained Browse, Query, or Frame. */

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

