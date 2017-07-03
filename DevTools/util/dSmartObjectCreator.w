&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME gDialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS gDialog 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrdlg.w - ADM2 SmartDialog Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
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
/*DEF BUFFER bzVisObj FOR zVisObj.
DEF BUFFER czVisObj FOR zVisObj.
*/
DEF VAR cDirName AS CHAR FORMAT "!(3)".
DEF VAR deColWidth AS DECI.
DEF VAR iPosStart AS INT.
DEF VAR iPosEnd AS INT.
DEF VAR cDefLang AS CHAR.

DEF TEMP-TABLE ttFile
    FIELD ttFileName AS CHAR FORMAT "x(40)"
    FIELD ttModFileName AS CHAR FORMAT "x(40)"
    FIELD ttFieldName AS CHAR FORMAT "x(40)"
    FIELD ttLabel AS CHAR FORMAT "x(40)"
    FIELD ttColLabel AS CHAR FORMAT "x(40)"
    FIELD ttDataType AS CHAR FORMAT "x(20)"
    FIELD ttFormat AS CHAR FORMAT "x(20)"
    FIELD ttHelpString AS CHAR FORMAT "x(40)"
    FIELD ttMandatory AS LOG
    FIELD ttOrder AS INT
    FIELD ttNumExtents AS INT FORMAT ">>>>9"
    FIELD ttObjOrder AS INT 
    FIELD ttObjViewAs AS CHAR
    FIELD ttObjRow AS DECI
    FIELD ttObjCol AS DECI
    FIELD ttObjWidth AS DECI
    FIELD ttObjHeight AS DECI
    FIELD ttObjFixedPos AS LOG
    FIELD ttObjFixedSize AS LOG
    INDEX iOrder IS UNIQUE PRIMARY ttOrder
    INDEX iObjOrder ttObjOrder.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME gDialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS IMAGE-1 fiDirectory Btn_Cancel tbAllTables ~
tbAllFields rsVersion slTables slFields tbSuite fiSuite tbUseDesc tbType ~
tbAddVersion fiVersion tbAddDate fiRevDate tbAuthor fiAuthor tbSDO tbOver-2 ~
tbCompileSDO tbBrowse tbOver2 tbUseZVisObj bGenerate 
&Scoped-Define DISPLAYED-OBJECTS fiDirectory tbAllTables tbAllFields ~
rsVersion slTables slFields tbSuite fiSuite tbUseDesc tbType tbAddVersion ~
fiVersion tbAddDate fiRevDate tbAuthor fiAuthor tbSDO tbOver-2 tbCompileSDO ~
tbBrowse tbOver2 tbUseZVisObj tbViewer tbOver3 tbUseZVisObj2 rsProgVer 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON bGenerate 
     LABEL "Generate" 
     SIZE 17 BY 1.17.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Quit" 
     SIZE 15 BY 1.17.

DEFINE VARIABLE fiAuthor AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiDirectory AS CHARACTER FORMAT "X(256)":U 
     LABEL "Top-level Source Directory" 
     VIEW-AS FILL-IN 
     SIZE 69 BY 1 NO-UNDO.

DEFINE VARIABLE fiRevDate AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiSuite AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiVersion AS DECIMAL FORMAT ">>9.9999<<":U INITIAL 10.7 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "img/folder1.bmp":U
     SIZE 5 BY 1.04.

DEFINE VARIABLE rsProgVer AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "PSC v9 format", 1,
"OE v10 format", 2
     SIZE 43 BY 1.04 NO-UNDO.

DEFINE VARIABLE rsVersion AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Nyqqi", 1,
"Decade", 2
     SIZE 25 BY .92 NO-UNDO.

DEFINE VARIABLE slFields AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 40 BY 23.17 NO-UNDO.

DEFINE VARIABLE slTables AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 40 BY 23.17 NO-UNDO.

DEFINE VARIABLE tbAddDate AS LOGICAL INITIAL yes 
     LABEL "Add Create date?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE tbAddVersion AS LOGICAL INITIAL yes 
     LABEL "Add Version number?" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE tbAllFields AS LOGICAL INITIAL yes 
     LABEL "Select All" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.43 BY .92 NO-UNDO.

DEFINE VARIABLE tbAllTables AS LOGICAL INITIAL no 
     LABEL "Select All" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.43 BY .92 NO-UNDO.

DEFINE VARIABLE tbAuthor AS LOGICAL INITIAL yes 
     LABEL "Add Author info?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE tbBrowse AS LOGICAL INITIAL no 
     LABEL "Create Browser Program(s)?" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY .92 NO-UNDO.

DEFINE VARIABLE tbCompileSDO AS LOGICAL INITIAL no 
     LABEL "Compile after create?" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY .92 NO-UNDO.

DEFINE VARIABLE tbOver-2 AS LOGICAL INITIAL yes 
     LABEL "Overwrite if exists?" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .92 NO-UNDO.

DEFINE VARIABLE tbOver2 AS LOGICAL INITIAL yes 
     LABEL "Overwrite if exists?" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .92 NO-UNDO.

DEFINE VARIABLE tbOver3 AS LOGICAL INITIAL yes 
     LABEL "Overwrite if exists?" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .92 NO-UNDO.

DEFINE VARIABLE tbSDO AS LOGICAL INITIAL yes 
     LABEL "Create SDO Program(s)?" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .92 NO-UNDO.

DEFINE VARIABLE tbSuite AS LOGICAL INITIAL yes 
     LABEL "Auto-assign program directory?" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY .92 NO-UNDO.

DEFINE VARIABLE tbType AS LOGICAL INITIAL no 
     LABEL "Append directory type ( _sdo/ _brw)?" 
     VIEW-AS TOGGLE-BOX
     SIZE 41 BY .92 NO-UNDO.

DEFINE VARIABLE tbUseDesc AS LOGICAL INITIAL yes 
     LABEL "Use _file description for directory assignment?" 
     VIEW-AS TOGGLE-BOX
     SIZE 48 BY 1.04 NO-UNDO.

DEFINE VARIABLE tbUseZVisObj AS LOGICAL INITIAL yes 
     LABEL "Use Visual Object data (if exists)?" 
     VIEW-AS TOGGLE-BOX
     SIZE 39 BY .92 NO-UNDO.

DEFINE VARIABLE tbUseZVisObj2 AS LOGICAL INITIAL yes 
     LABEL "Use Visual Object data (if exists)?" 
     VIEW-AS TOGGLE-BOX
     SIZE 37 BY .92 NO-UNDO.

DEFINE VARIABLE tbViewer AS LOGICAL INITIAL no 
     LABEL "Create Viewer Program(s)?" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .92 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME gDialog
     fiDirectory AT ROW 1.54 COL 29 COLON-ALIGNED
     Btn_Cancel AT ROW 1.54 COL 134
     tbAllTables AT ROW 3.13 COL 18
     tbAllFields AT ROW 3.13 COL 59
     rsVersion AT ROW 3.13 COL 109 NO-LABEL
     slTables AT ROW 4.17 COL 6 NO-LABEL
     slFields AT ROW 4.17 COL 48 NO-LABEL
     tbSuite AT ROW 4.42 COL 91
     fiSuite AT ROW 4.42 COL 124 COLON-ALIGNED NO-LABEL
     tbUseDesc AT ROW 5.75 COL 91
     tbType AT ROW 7.04 COL 91
     tbAddVersion AT ROW 8.38 COL 91
     fiVersion AT ROW 8.38 COL 124 COLON-ALIGNED NO-LABEL
     tbAddDate AT ROW 9.67 COL 91
     fiRevDate AT ROW 9.67 COL 124 COLON-ALIGNED NO-LABEL
     tbAuthor AT ROW 11 COL 91
     fiAuthor AT ROW 11 COL 124 COLON-ALIGNED NO-LABEL
     tbSDO AT ROW 12.58 COL 91
     tbOver-2 AT ROW 13.63 COL 95
     tbCompileSDO AT ROW 14.67 COL 95
     tbBrowse AT ROW 16.54 COL 91
     tbOver2 AT ROW 17.58 COL 95
     tbUseZVisObj AT ROW 18.63 COL 95
     tbViewer AT ROW 20.46 COL 91
     tbOver3 AT ROW 21.54 COL 95
     tbUseZVisObj2 AT ROW 22.58 COL 95
     rsProgVer AT ROW 24.42 COL 90 NO-LABEL
     bGenerate AT ROW 26.25 COL 104
     "Tables:" VIEW-AS TEXT
          SIZE 9 BY .92 AT ROW 3.13 COL 5
     "Fields:" VIEW-AS TEXT
          SIZE 8 BY .92 AT ROW 3.13 COL 48
     "For MXP Version:" VIEW-AS TEXT
          SIZE 18 BY .92 AT ROW 3.13 COL 91
     IMAGE-1 AT ROW 1.54 COL 102
     SPACE(43.79) SKIP(25.15)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "SmartObject Program Generator Utility"
         DEFAULT-BUTTON bGenerate CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB gDialog 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX gDialog
                                                                        */
ASSIGN 
       FRAME gDialog:SCROLLABLE       = FALSE
       FRAME gDialog:HIDDEN           = TRUE.

/* SETTINGS FOR RADIO-SET rsProgVer IN FRAME gDialog
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tbOver3 IN FRAME gDialog
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tbUseZVisObj2 IN FRAME gDialog
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tbViewer IN FRAME gDialog
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX gDialog
/* Query rebuild information for DIALOG-BOX gDialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX gDialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME gDialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gDialog gDialog
ON WINDOW-CLOSE OF FRAME gDialog /* SmartObject Program Generator Utility */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bGenerate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bGenerate gDialog
ON CHOOSE OF bGenerate IN FRAME gDialog /* Generate */
DO:
    DEF VAR iCtr AS INT.
    DEF VAR cTable AS CHAR.

    IF tbSuite:CHECKED
    AND NOT tbUseDesc:CHECKED
    AND fiSuite:SCREEN-VALUE = "" THEN DO:
        MESSAGE
            "You must specify a Suite (directory) for creation of files."
            VIEW-AS ALERT-BOX ERROR.
        APPLY 'entry' TO fiSuite.
        RETURN NO-APPLY.
    END.
    IF tbAddVersion:CHECKED
    AND DECIMAL(fiVersion:SCREEN-VALUE) = 0 THEN DO:
        MESSAGE
            "Please enter a version number, or UNcheck 'Add Version number' box."
            VIEW-AS ALERT-BOX ERROR.
        APPLY 'entry' TO fiVersion.
        RETURN NO-APPLY.
    END.
    IF tbBrowse:CHECKED
    OR tbViewer:CHECKED THEN DO:
        MESSAGE
            "This will remove any user-level customizations to browsers and viewers."
            VIEW-AS ALERT-BOX QUESTION UPDATE lContinue AS LOG.
        IF NOT lContinue THEN RETURN NO-APPLY.
    END.

    DO iCtr = 1 TO NUM-ENTRIES(slTables:SCREEN-VALUE):
        ASSIGN 
            cTable = ENTRY(iCtr,slTables:SCREEN-VALUE).
        IF NOT tbSuite:CHECKED THEN DO:
            DISPLAY
                "For table " + cTable + ", enter target directory:" FORMAT "x(55)"
                WITH FRAME dTarget VIEW-AS DIALOG-BOX THREE-D
                WIDTH 90 NO-LABELS.
            UPDATE
                cDirName NO-LABEL
                WITH FRAME dTarget.
            ASSIGN
                cDirName = LOWER(cDirName).
        END.
        ELSE ASSIGN
            cDirName = LOWER(fiSuite:SCREEN-VALUE).
        
        RUN ipCreateTtFile IN THIS-PROCEDURE (INPUT cTable).

        IF tbSDO:CHECKED IN FRAME gDialog THEN 
            RUN ipCreateSDO IN THIS-PROCEDURE (INPUT cTable).
        IF tbBrowse:CHECKED IN FRAME gDialog THEN 
            RUN ipCreateBrowse IN THIS-PROCEDURE (INPUT cTable).
        IF tbViewer:CHECKED IN FRAME gDialog THEN 
            RUN ipCreateViewer IN THIS-PROCEDURE (INPUT cTable).
    END.

    MESSAGE 
        "Done!"
        VIEW-AS ALERT-BOX.
        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbAllTables
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbAllTables gDialog
ON VALUE-CHANGED OF tbAllTables IN FRAME gDialog /* Select All */
OR VALUE-CHANGED OF tbAllFields
OR VALUE-CHANGED OF slTables
DO:
    CASE SELF:NAME:
        WHEN "tbAllTables" THEN DO:
            CASE SELF:CHECKED:
                WHEN TRUE THEN ASSIGN 
                    slTables:SCREEN-VALUE = slTables:LIST-ITEMS
                    tbAllFields:CHECKED = TRUE
                    slFields:SCREEN-VALUE = slFields:LIST-ITEMS
                    tbSuite:CHECKED = FALSE.
                WHEN FALSE THEN ASSIGN 
                    slTables:SCREEN-VALUE = ?
                    slTables:SCREEN-VALUE = slTables:ENTRY(1).
            END CASE.
        END.
        WHEN "tbAllFields" THEN DO:
            CASE SELF:CHECKED:
                WHEN TRUE THEN ASSIGN slFields:SCREEN-VALUE = slFields:LIST-ITEMS.
                WHEN FALSE THEN ASSIGN 
                    slFields:SCREEN-VALUE = ?
                    slFields:SCREEN-VALUE = slFields:ENTRY(1).
            END CASE.
        END.
        WHEN "slTables" THEN DO:
            ASSIGN
                slFields:LIST-ITEMS IN FRAME gDialog = "".
            FIND _file WHERE
                _file._file-name = slTables:SCREEN-VALUE
                NO-LOCK NO-ERROR.
            IF AVAIL _file THEN DO:
                FOR EACH _field OF _file:
                    slFields:ADD-LAST(_field._field-name).
                END.
            END.
            IF tbAllFields:CHECKED THEN ASSIGN
                slFields:SCREEN-VALUE = slFields:LIST-ITEMS.
            ELSE ASSIGN
                slFields:SCREEN-VALUE = slFields:ENTRY(1).
        END.
    END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK gDialog 


/* ***************************  Main Block  *************************** */

{src/adm2/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects gDialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI gDialog  _DEFAULT-DISABLE
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
  HIDE FRAME gDialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI gDialog  _DEFAULT-ENABLE
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
  DISPLAY fiDirectory tbAllTables tbAllFields rsVersion slTables slFields 
          tbSuite fiSuite tbUseDesc tbType tbAddVersion fiVersion tbAddDate 
          fiRevDate tbAuthor fiAuthor tbSDO tbOver-2 tbCompileSDO tbBrowse 
          tbOver2 tbUseZVisObj tbViewer tbOver3 tbUseZVisObj2 rsProgVer 
      WITH FRAME gDialog.
  ENABLE IMAGE-1 fiDirectory Btn_Cancel tbAllTables tbAllFields rsVersion 
         slTables slFields tbSuite fiSuite tbUseDesc tbType tbAddVersion 
         fiVersion tbAddDate fiRevDate tbAuthor fiAuthor tbSDO tbOver-2 
         tbCompileSDO tbBrowse tbOver2 tbUseZVisObj bGenerate 
      WITH FRAME gDialog.
  VIEW FRAME gDialog.
  {&OPEN-BROWSERS-IN-QUERY-gDialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject gDialog 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR iCtr AS INT.

    FOR EACH _file WHERE
        SUBSTRING(_file._file-name,1,1) <> "_" AND
        SUBSTRING(_file._file-name,1,3) <> "SYS":
        slTables:ADD-LAST(_file._file-name) IN FRAME gDialog.
    END.

    IF NOT CONNECTED("DICTDB") THEN RUN _admin.p.

  RUN SUPER.
/*
    FIND FIRST zLang WHERE
        zLang.isDefault = TRUE
        NO-LOCK NO-ERROR.
    IF AVAIL zLang THEN ASSIGN
        cDefLang = zLang.langCode.
    ELSE */ ASSIGN
        cDefLang = "ENG".

    ASSIGN
        fidirectory:SCREEN-VALUE = SUBSTRING(PROGRAM-NAME(2),1,R-INDEX(PROGRAM-NAME(2),"\") - 1)
        fidirectory:SCREEN-VALUE = SUBSTRING(fidirectory:SCREEN-VALUE,1,R-INDEX(fidirectory:SCREEN-VALUE,"\") - 1)
        fiDirectory:SCREEN-VALUE = "c:\mxp\10_7"
        slTables:SCREEN-VALUE = ENTRY(1,slTables:LIST-ITEMS)
        fiRevDate:SCREEN-VALUE = STRING(TODAY)
        fiAuthor:SCREEN-VALUE = USERID("DICTDB").
    APPLY 'value-changed' TO slTables.
    APPLY 'entry' TO slTables.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipCreateBrowse gDialog 
PROCEDURE ipCreateBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER cTableName AS CHAR.
    DEF VAR cLongFileName AS CHAR.
    DEF VAR cShortFileName AS CHAR.
    DEF VAR iCtr AS INT.
    DEF VAR jCtr AS INT.
    DEF VAR iNumFields AS INT.
    DEF VAR cFieldList AS CHAR.
    DEF VAR cMandList AS CHAR.

    FOR EACH ttFile USE-INDEX iOrder:
        ASSIGN
            iNumFields = iNumFields + 1.
        IF ttFile.ttNumExtents < 1 THEN ASSIGN
            cFieldList = cFieldList + ttFile.ttFieldName + ",".
        ELSE DO iCtr = 1 TO ttFile.ttNumExtents:
            ASSIGN
                cFieldList = cFieldList + ttFile.ttFieldName + STRING(iCtr) + ",".
        END.
        IF ttFile.ttMandatory THEN DO:
            IF ttFile.ttNumExtents < 1 THEN ASSIGN
                cMandList = cMandList + ttFile.ttFieldName + ",".
            ELSE DO iCtr = 1 TO ttFile.ttNumExtents:
                ASSIGN
                    cMandList = cMandList + ttFile.ttFieldName + STRING(iCtr) + ",".
            END.
        END.
    END.
    ASSIGN
        cFieldList = TRIM(cFieldList,",").
    FIND FIRST ttFile
        NO-LOCK NO-ERROR.

    IF NOT AVAIL ttFile THEN DO:
    END.
    ELSE DO:
    /* Create brw.w */
        FIND FIRST ttFile
            NO-LOCK NO-ERROR.
        ASSIGN
            cLongFileName = fiDirectory:SCREEN-VALUE IN FRAME gDialog + "\" + cDirName +  
                            (IF tbType:CHECKED THEN "\_brw" ELSE "") + 
                            (IF rsVersion:SCREEN-VALUE = "1" THEN "\brw" ELSE "\b") + 
                            ttFile.ttModFileName + ".w"
            cShortFileName = cDirName + (IF tbType:CHECKED THEN "\_brw" ELSE "") + 
                            (IF rsVersion:SCREEN-VALUE = "1" THEN "\brw" ELSE "\b") + 
                            ttFile.ttModFileName + ".w".
        OUTPUT TO VALUE(cLongFileName).
        {util/createBRW1.i}
        {util/createBRW2.i}
        OUTPUT CLOSE.
        
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipCreateSDO gDialog 
PROCEDURE ipCreateSDO :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER cTableName AS CHAR.
    DEF VAR cLongFileName AS CHAR.
    DEF VAR cShortFileName AS CHAR.
    DEF VAR iCtr AS INT.
    DEF VAR jCtr AS INT.
    DEF VAR iNumFields AS INT.
    DEF VAR cFieldList AS CHAR.
    DEF VAR cMandList AS CHAR.
    DEF VAR deWidth AS DECIMAL FORMAT ">>9.9".
    DEF VAR cObjDir AS CHAR.

    FOR EACH ttFile USE-INDEX iOrder:
        ASSIGN
            iNumFields = iNumFields + 1.
        IF ttFile.ttNumExtents < 1 THEN ASSIGN
            cFieldList = cFieldList + ttFile.ttFieldName + ",".
        ELSE DO iCtr = 1 TO ttFile.ttNumExtents:
            ASSIGN
                cFieldList = cFieldList + ttFile.ttFieldName + STRING(iCtr) + ",".
        END.
        IF ttFile.ttMandatory THEN DO:
            IF ttFile.ttNumExtents < 1 THEN ASSIGN
                cMandList = cMandList + ttFile.ttFieldName + ",".
            ELSE DO iCtr = 1 TO ttFile.ttNumExtents:
                ASSIGN
                    cMandList = cMandList + ttFile.ttFieldName + STRING(iCtr) + ",".
            END.
        END.
    END.
    ASSIGN
        cFieldList = TRIM(cFieldList,",").
    FIND FIRST ttFile
        NO-LOCK NO-ERROR.

    IF NOT AVAIL ttFile THEN DO:
    END.
    ELSE DO:
    /* Create sdo.w */
        FIND FIRST ttFile
            NO-LOCK NO-ERROR.
        ASSIGN
            cLongFileName = fiDirectory:SCREEN-VALUE IN FRAME gDialog + "\" + cDirName + 
                            (IF tbType:CHECKED THEN "\_sdo" ELSE "") + "\sdo" + ttFile.ttModFileName + ".w"
            cShortFileName = cDirName + (IF tbType:CHECKED THEN "\_sdo" ELSE "") + "\sdo" + ttFile.ttModFileName + ".w".
        OUTPUT TO VALUE(cLongFileName).
        {util/createSDO1.i}
        {util/createSDO2.i}
        OUTPUT CLOSE.
        
    /* Create sdo.i */
        FIND FIRST ttFile
            NO-LOCK NO-ERROR.
        ASSIGN
            cLongFileName = fiDirectory:SCREEN-VALUE + "\" + cDirName + 
                            (IF tbType:CHECKED THEN "\_sdo" ELSE "") + "\sdo" + ttFile.ttModFileName + ".i"
            cShortFileName = cDirName + (IF tbType:CHECKED THEN "\_sdo" ELSE "") + "\sdo" + ttFile.ttModFileName + ".i".
        OUTPUT TO VALUE(cLongFileName).
        {util/createSDO3.i}
        OUTPUT CLOSE.

    /* Create sdo_cl.w */
        FIND FIRST ttFile
            NO-LOCK NO-ERROR.
        ASSIGN
            cLongFileName = fiDirectory:SCREEN-VALUE + "\" + cDirName + 
                            (IF tbType:CHECKED THEN "\_sdo" ELSE "") + "\sdo" + ttFile.ttModFileName + "_cl.w"
            cShortFileName = cDirName + (IF tbType:CHECKED THEN "\_sdo" ELSE "") + "\sdo" + ttFile.ttModFileName + ".w".
        OUTPUT TO VALUE(cLongFileName).
        {util/createSDO4.i}
        OUTPUT CLOSE.
    END.

    /* Compile SDO if requested */
    IF tbCompileSDO:CHECKED THEN DO:
        FIND FIRST ttFile
            NO-LOCK NO-ERROR.
        ASSIGN
            cObjDir = fiDirectory:SCREEN-VALUE IN FRAME gDialog + "\" + cDirName + (IF tbType:CHECKED THEN "\_sdo" ELSE "")
            cObjDir = REPLACE(cObjDir,"\src","\obj")
            cObjDir = REPLACE(cObjDir,"\\","\")
            cLongFileName = fiDirectory:SCREEN-VALUE IN FRAME gDialog + "\" + cDirName + 
                            (IF tbType:CHECKED THEN "\_sdo" ELSE "") + "\sdo" + ttFile.ttModFileName + ".w"
            cShortFileName = cDirName + (IF tbType:CHECKED THEN "\_sdo" ELSE "") + "\sdo" + ttFile.ttModFileName + ".w".
        
        COMPILE VALUE(REPLACE(cLongFileName,"\\","\")) SAVE INTO VALUE(cObjDir).
        
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipCreateTTFile gDialog 
PROCEDURE ipCreateTTFile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER cTableName AS CHAR.
    DEF VAR cModFileName AS CHAR.
    DEF VAR iCount AS INT.

    FIND _file WHERE
        _file._file-name = cTableName
        NO-LOCK NO-ERROR.
    IF NOT AVAIL _file THEN DO:
        MESSAGE
            "Unable to locate file" cTableName "in database. Skipping..."
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.
    ELSE DO:
        ASSIGN 
            cModFileName = UPPER(SUBSTRING(_file._file-name,1,1)) +
                           SUBSTRING(_file._file-name,2).
        IF INDEX(cModFileName,"-") <> 0 THEN DO WHILE INDEX(cModFileName,"-") <> 0:
            ASSIGN
                SUBSTRING(cModFileName,INDEX(cModFileName,"-") + 1,1) = 
                UPPER(SUBSTRING(cModFileName,INDEX(cModFileName,"-") + 1,1))
                SUBSTRING(cModFileName,INDEX(cModFileName,"-"),1) = "".
        END.
        EMPTY TEMP-TABLE ttFile.

        /* Get primary index and put those fields FIRST in ttFile table */
        FIND _index WHERE
            _index._file-recid = _file._prime-index
            NO-LOCK NO-ERROR.
        FOR EACH _index-field OF _index:
            CREATE ttFile.
            ASSIGN
                iCount = iCount + 1
                ttFile.ttFileName = _file._file-name
                ttFile.ttFieldName = _field._field-name
                ttFile.ttOrder = iCount.
        END.

        FOR EACH _field OF _file BY _field._order:
            IF NUM-ENTRIES(slTables:SCREEN-VALUE IN FRAME gDialog) = 1
            AND INDEX(slFields:SCREEN-VALUE IN FRAME gDialog,_field._field-name) = 0 THEN NEXT.
            /* Calculate display width for columns in browse */
            IF INDEX(_field._format,"x") > 0 THEN DO:
                IF INDEX(_field._format,"(") > 0 THEN ASSIGN
                    iPosStart = INDEX(_field._format,"(") + 1
                    iPosEnd = INDEX(_field._format,")") 
                    deColWidth = DECIMAL(SUBSTRING(_field._format,iPosStart,iPosEnd - iPosStart)).
                ELSE ASSIGN
                    deColWidth = DECIMAL(LENGTH(_field._format)).
            END.
            ELSE IF INDEX(_field._format,"/") > 0 THEN ASSIGN
                deColWidth = DECIMAL(LENGTH(SUBSTRING(_field._format,1,INDEX(_field._format,"/") - 1))).
            ELSE ASSIGN 
                deColWidth = DECIMAL(LENGTH(_field._format)).
            ASSIGN
                deColWidth = MAXIMUM(deColWidth,DECIMAL(LENGTH(TRIM(_field._label))))
                deColWidth = deColWidth * 1.3.

            /* Build ttFile record from data defaults */
            IF NOT CAN-FIND(FIRST ttFile WHERE
                            ttFile.ttFileName = _file._file-name AND 
                            ttFile.ttFieldName = _field._field-name) THEN DO:
                CREATE ttFile.
                ASSIGN
                    iCount = iCount + 1.
            END.
            ELSE FIND FIRST ttFile WHERE
                ttFile.ttFileName = _file._file-name AND 
                ttFile.ttFieldName = _field._field-name
                EXCLUSIVE.
            
            ASSIGN
                ttFile.ttFileName = _file._file-name
                ttFile.ttModFileName = cModFileName
                ttFile.ttFieldName = _field._field-name
                ttFile.ttLabel = _field._label
                ttFile.ttColLabel = IF _field._col-label <> "" THEN _field._col-label ELSE _field._label
                ttFile.ttDataType = _field._data-type
                ttFile.ttFormat = _field._format
                ttFile.ttHelpString = _field._help
                ttFile.ttMandatory = _field._mandatory
                ttFile.ttNumExtents = _field._extent
                ttFile.ttOrder = IF ttFile.ttOrder <> 0 THEN ttFile.ttOrder ELSE iCount
                ttFile.ttObjOrder = ttFile.ttOrder
                ttFile.ttObjWidth = deColWidth
                .

            IF tbUseDesc:CHECKED IN FRAME gDialog THEN ASSIGN
                cDirName = IF rsVersion:SCREEN-VALUE = "1" THEN ENTRY(1,_file._desc,":") ELSE
                            ENTRY(3,_file._desc,":").
/*
            /* If user has set default values in zVisObj table, use them */            
            FIND FIRST zVisObj WHERE
                zVisObj.objType = "BRW" AND
                zVisObj.objCode = "brw" + ttFile.ttModFileName AND
                zVisObj.iOrder = 0 AND
                zVisObj.parentCode = "" AND
                zVisObj.zUserID = "*"
                NO-LOCK NO-ERROR.
            IF NOT AVAIL zVisObj THEN DO:
                CREATE zVisObj.
                ASSIGN
                    zVisObj.objType = "BRW"
                    zVisObj.objCode = "brw" + ttFile.ttModFileName
                    zVisObj.iOrder = 0
                    zVisObj.parentCode = ""
                    zVisObj.zUserID = "*"
                    zVisObj.objName = "brw" + ttFile.ttModFileName
                    zVisObj.functCode = ""
                    zVisObj.pgmName = cDirName + (IF tbType:CHECKED IN FRAME gDialog THEN "\_brw" ELSE "") + 
                                             (IF rsVersion:SCREEN-VALUE = "1" THEN "\brw" ELSE "\b") + 
                                             ttFile.ttModFileName + ".w"
                    zVisObj.objViewAs = "Browser"
                    zVisObj.deRow = 1
                    zVisObj.deCol = 1
                    zVisObj.deWidth = .99
                    zVisObj.deHeight = .33.
            END.
            /* Remove user-level column customizations */
            FOR EACH bzVisObj WHERE
                bzVisObj.objType = "BRWCOL" AND
                bzVisObj.parentCode = zVisObj.objCode AND
                bzVisObj.iOrder = ttFile.ttOrder AND
                bzVisObj.objCode = ttFile.ttFieldName AND
                bzVisObj.zUserID <> "*":
                DELETE bzVisObj.
            END.
            FIND FIRST bzVisObj WHERE
                bzVisObj.objType = "BRWCOL" AND
                bzVisObj.parentCode = zVisObj.objCode AND
                bzVisObj.iOrder = ttFile.ttOrder AND
                bzVisObj.objCode = ttFile.ttFieldName AND
                bzVisObj.zUserID = "*"
                EXCLUSIVE NO-ERROR.
            IF NOT AVAIL bzVisObj THEN DO:
                CREATE bzVisObj.
                ASSIGN
                    bzVisObj.objType = "BRWCOL"
                    bzVisObj.objCode = ttFile.ttFieldName
                    bzVisObj.parentCode = zVisObj.objCode
                    bzVisObj.iOrder = ttFile.ttObjOrder
                    bzVisObj.objName = ttFile.ttFieldName
                    bzVisObj.zUserID = "*"
                    bzVisObj.functCode = ""
                    bzVisObj.pgmName = cDirName + (IF tbType:CHECKED IN FRAME gDialog THEN "\_brw" ELSE "") + 
                                             (IF rsVersion:SCREEN-VALUE = "1" THEN "\brw" ELSE "\b") + 
                                             ttFile.ttModFileName + ".w"
                    bzVisObj.isVisible = TRUE
                    bzVisObj.objViewAs = "FILL-IN"
                    bzVisObj.deRow = 0
                    bzVisObj.deCol = 0
                    bzVisObj.deWidth = ttFile.ttObjWidth
                    bzVisObj.deHeight = 0.
            END.
            ELSE IF tbUseZVisObj:CHECKED IN FRAME gDialog THEN ASSIGN
                ttFile.ttObjOrder = bzVisObj.iOrder
                ttFile.ttObjWidth = bzVisObj.deWidth.
*/        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipCreateViewer gDialog 
PROCEDURE ipCreateViewer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER cTableName AS CHAR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipResetDB gDialog 
PROCEDURE ipResetDB :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN
        slTables:LIST-ITEMS IN FRAME gDialog = "".
    FOR EACH _file WHERE
        SUBSTRING(_file._file-name,1,1) <> "_" AND
        SUBSTRING(_file._file-name,1,3) <> "SYS":
        slTables:ADD-LAST(_file._file-name) IN FRAME gDialog.
    END.
    APPLY 'value-changed' TO slTables.
    APPLY 'entry' TO slTables.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

