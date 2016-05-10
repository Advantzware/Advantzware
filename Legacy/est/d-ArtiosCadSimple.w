&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: est/d-ArtiosCadSimple.w
  
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
DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER   NO-UNDO.
DEFINE VARIABLE opCadCam AS CHARACTER   NO-UNDO.
&ELSE
DEFINE VARIABLE ipcCompany AS CHARACTER NO-UNDO INITIAL '001'.
DEFINE VARIABLE opCadCam AS CHARACTER NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */

{est/artiosvar.i "shared"}
DEFINE VARIABLE gchCadX AS COM-HANDLE      NO-UNDO.

DEFINE VARIABLE gcArtiosDir AS CHARACTER   NO-UNDO.
DEFINE VARIABLE gcImageDir AS CHARACTER   NO-UNDO.
DEFINE VARIABLE gcCadFileSelected AS CHARACTER   NO-UNDO.


DEFINE VARIABLE giFormNumber AS INTEGER     NO-UNDO.
DEFINE VARIABLE giBlankNumber AS INTEGER     NO-UNDO.
DEFINE VARIABLE giProjectCount AS INTEGER INIT 50 NO-UNDO.
DEFINE VARIABLE gcAttributeList AS CHARACTER   NO-UNDO.
DEFINE VARIABLE gcAttributeTypeList AS CHARACTER   NO-UNDO.
    
ASSIGN 
    gcAttributeList =  
                    "Cad Number," +
                    "Customer Number," +
                    "Customer Name," +
                    "Part Number," +
                    "Part Name," +
                    "Style," +
                    "Flute," +
                    "Test," + 
                    "Board Code," +
                    "Length," +
                    "Width," +
                    "Depth," +
                    "Trim Width," +
                    "Trim Length," +
                    "Die Inches," +
                    "Qty," +
                    "Qty Per Set," +
                    "Product Category," +
                    "Die Number," +
                    "Number Up," +
                    "Manufactured Or Purchased," +
                    "Salesperson ID," +
                    "Grain Direction," +
                    "Designer Firstname," +
                    "Designer Lastname"
                    
    gcAttributeTypeList = 
                    "T," +
                    "T," +
                    "T," +
                    "T," +
                    "T," +
                    "T," +
                    "T," +
                    "T," + 
                    "T," +
                    "N," +
                    "N," +
                    "N," +
                    "N," +
                    "N," +
                    "N," +
                    "N," +
                    "N," +
                    "T," +
                    "T," +
                    "N," +
                    "T," +
                    "T," +
                    "T," +
                    "T," +
                    "T"
        .


DEFINE TEMP-TABLE ttSubDir NO-UNDO
    FIELD DirName AS CHARACTER
    .

DEFINE TEMP-TABLE ttAttribute
    FIELD CadFile AS CHARACTER
    FIELD Attribute AS CHARACTER
    FIELD AttributeType AS CHARACTER
    FIELD ValueImported AS CHARACTER
    FIELD ValueMapped AS CHARACTER
    FIELD LoadMessage AS CHARACTER
    .

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
&Scoped-define INTERNAL-TABLES ttAttribute

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 ttAttribute.Attribute   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2   
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH ttAttribute WHERE ttAttribute.CadFile EQ gcCadFileSelected
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY {&SELF-NAME} FOR EACH ttAttribute WHERE ttAttribute.CadFile EQ gcCadFileSelected.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 ttAttribute
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 ttAttribute


/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-D-Dialog ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-2 btnLoad cadPath btnCADPathLookup ~
cadNumber btnCAD#Lookup fiQty cCategory cb-CadSeq cStyle Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS cadPath cadNumber fiQty cCategory ~
cb-CadSeq fiNumberCadForms cStyle 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetArtiosDir D-Dialog 
FUNCTION GetArtiosDir RETURNS CHARACTER
  ( ipcCo AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetImageDir D-Dialog 
FUNCTION GetImageDir RETURNS CHARACTER
  ( ipcCo AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetXref D-Dialog 
FUNCTION GetXref RETURNS CHARACTER
  ( ipcType AS CHARACTER, ipcLookup AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetXrefField D-Dialog 
FUNCTION GetXrefField RETURNS CHARACTER
  (ipcLookup AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCAD#Lookup 
     IMAGE-UP FILE "images/find.bmp":U
     LABEL "" 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnCADPathLookup 
     IMAGE-UP FILE "images/find.bmp":U
     LABEL "" 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnLoad 
     LABEL "Load" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "&Cancel" 
     SIZE 15 BY 1.71
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "&OK" 
     SIZE 15 BY 1.71
     BGCOLOR 8 .

DEFINE VARIABLE cb-CadSeq AS CHARACTER FORMAT "X(256)":U 
     LABEL "CAD File" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 77 BY 1 NO-UNDO.

DEFINE VARIABLE cadNumber AS CHARACTER FORMAT "x(200)" 
     LABEL "CAD File/Project #" 
     VIEW-AS FILL-IN 
     SIZE 77.4 BY 1 NO-UNDO.

DEFINE VARIABLE cadPath AS CHARACTER FORMAT "x(200)" 
     LABEL "CAD File Path" 
     VIEW-AS FILL-IN 
     SIZE 77.4 BY 1 NO-UNDO.

DEFINE VARIABLE cCategory AS CHARACTER FORMAT "X(5)":U 
     LABEL "Category" 
     VIEW-AS FILL-IN 
     SIZE 11.8 BY 1.38 NO-UNDO.

DEFINE VARIABLE cStyle AS CHARACTER FORMAT "X(256)":U 
     LABEL "Style" 
     VIEW-AS FILL-IN 
     SIZE 18.8 BY 1.29 NO-UNDO.

DEFINE VARIABLE fiNumberCadForms AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1 NO-UNDO.

DEFINE VARIABLE fiQty AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     LABEL "Estimate Qty" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      ttAttribute SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 D-Dialog _FREEFORM
  QUERY BROWSE-2 NO-LOCK DISPLAY
      ttAttribute.Attribute
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 130 BY 9.76 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     BROWSE-2 AT ROW 9.33 COL 4 WIDGET-ID 100
     btnLoad AT ROW 4.14 COL 21 WIDGET-ID 164
     cadPath AT ROW 2.76 COL 19 COLON-ALIGNED WIDGET-ID 114
     btnCADPathLookup AT ROW 2.81 COL 98.4 WIDGET-ID 116
     cadNumber AT ROW 1.24 COL 19 COLON-ALIGNED
     btnCAD#Lookup AT ROW 1.24 COL 98.4 WIDGET-ID 4
     fiQty AT ROW 7.19 COL 19 COLON-ALIGNED WIDGET-ID 42
     cCategory AT ROW 11.48 COL 140 COLON-ALIGNED WIDGET-ID 88
     cb-CadSeq AT ROW 6 COL 19 COLON-ALIGNED WIDGET-ID 84
     fiNumberCadForms AT ROW 4.33 COL 37 COLON-ALIGNED NO-LABEL WIDGET-ID 86
     cStyle AT ROW 14.33 COL 141 COLON-ALIGNED WIDGET-ID 6
     Btn_OK AT ROW 21.24 COL 29
     Btn_Cancel AT ROW 21.71 COL 65
     SPACE(88.59) SKIP(7.09)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Create Estimate from ESKO Artios"
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

{Advantzware/WinKit/embedwindow.i}
{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB BROWSE-2 1 D-Dialog */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

ASSIGN 
       btnCADPathLookup:HIDDEN IN FRAME D-Dialog           = TRUE.

ASSIGN 
       cadPath:HIDDEN IN FRAME D-Dialog           = TRUE.

ASSIGN 
       cb-CadSeq:HIDDEN IN FRAME D-Dialog           = TRUE.

/* SETTINGS FOR FILL-IN fiNumberCadForms IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttAttribute WHERE ttAttribute.CadFile EQ gcCadFileSelected.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
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
ON WINDOW-CLOSE OF FRAME D-Dialog /* Create Estimate from ESKO Artios */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCAD#Lookup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCAD#Lookup D-Dialog
ON CHOOSE OF btnCAD#Lookup IN FRAME D-Dialog
DO:
    DEFINE VARIABLE cCadFile AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE lOKClicked AS LOGICAL     NO-UNDO.
    
    SYSTEM-DIALOG GET-FILE cCadFile 
                TITLE 'Select Artios CAD File to insert'
                FILTERS 'ARD Files    (*.ard)' '*.ard'
                INITIAL-DIR gcArtiosDir
                MUST-EXIST USE-FILENAME UPDATE lOKClicked.

    IF lOKClicked THEN
        ASSIGN cadNumber:SCREEN-VALUE = cCadFile.
     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCADPathLookup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCADPathLookup D-Dialog
ON CHOOSE OF btnCADPathLookup IN FRAME D-Dialog
DO:
    DEFINE VARIABLE cCadFilepath AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE lOKClicked AS LOGICAL     NO-UNDO.
    
    SYSTEM-DIALOG GET-DIR cCadFilePath
        TITLE 'Select Artios CAD File Path to insert'
        INITIAL-DIR gcArtiosDir
        UPDATE lOKClicked.
   
    IF lOKClicked THEN
        cadpath:SCREEN-VALUE = cCadFilePath.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLoad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLoad D-Dialog
ON CHOOSE OF btnLoad IN FRAME D-Dialog /* Load */
DO:
    RUN CadXConnect.
    IF cadPath NE "" THEN
        RUN BuildProject(INPUT cadPath).
    ELSE IF cadNumber NE "" THEN
        RUN BuildAttributes(cadNumber).
    ELSE DO:
        MESSAGE "No Artios File or Project folder specified."
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY "entry" TO cadNumber.
    END.
    RUN CadXDisconnect.
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
/*   def var iExt as int no-undo.                                                                                         */
/*   def var cArtiosCadFile as cha no-undo.                                                                               */
/*   def var iRevision as int init 65 no-undo.                                                                            */
/*   def var iRevExt as int no-undo.                                                                                      */
/*                                                                                                                        */
/*   assign giFormNumber = 0                                                                                               */
/*          giBlankNumber = 0                                                                                              */
/*          cadNumber cStyle cCategory rs-man-pur fiQty                                                                   */
/*              /* dRatio-1 dRatio-2                                                                                      */
/*               dRatio-3 dRatio-4 dRatio-5 dRatio-6 dRatio-7 dRatio-8 dRatio-9*/                                         */
/*               .                                                                                                        */
/*   if artioscad-chr = "" then artioscad-chr = "c:\artios\asi\".                                                         */
/*   if substring(artioscad-chr,length(artioscad-chr),1) <> "/" and                                                       */
/*      substring(artioscad-chr,length(artioscad-chr),1) <> "\" then                                                      */
/*      artioscad-chr = artioscad-chr + "\".                                                                              */
/*                                                                                                                        */
/*   if index(cadNumber,"\") > 0 or index(cadNumber,"/") > 0 or                                                           */
/*      index(cadNumber,"ARD") > 0 then cArtiosCadFile = cadNumber.                                                       */
/*   else    cArtiosCadFile = artioscad-chr + cadnumber + ".ard".                                                         */
/*                                                                                                                        */
/*   session:set-wait-state("general").                                                                                   */
/*                                                                                                                        */
/*   run AssignCADFormInfo.                                                                                               */
/*                                                                                                                        */
/*   /* run getSubDirList.   run from leave of cadnumber */                                                               */
/*                                                                                                                        */
/*   if search(cArtiosCadFile ) <> ? then do:  /* import single CAD file */                                               */
/*      run create-ttCad (cArtiosCadFile).                                                                                */
/*   end.                                                                                                                 */
/*   else do iExt = 1 to giProjectCount:                                                                                   */
/*     /* import Project CAD file ###### + %%(2 digit extension) +  @ (1 character revision) */                           */
/*                                                                                                                        */
/*      for each ttSubDir :                                                                                              */
/*        if search(ttSubDir.DirName + cadNumber + string(iExt,"99") + ".ARD" ) <> ? then do:                            */
/*                                                                                                                        */
/*           run create-ttCad (ttSubDir.DirName + cadNumber + string(iExt,"99") + ".ARD" ).                              */
/*        end.                                                                                                            */
/*         /* check revision file */                                                                                      */
/*        do iRevExt = 65 to 90:                                                                                          */
/*                                                                                                                        */
/*           if search(ttSubDir.DirName + cadNumber + string(iExt,"99") + chr(iRevExt) + ".ARD" ) <> ?                   */
/*              then  run create-ttCad (ttSubDir.DirName + cadNumber + string(iExt,"99") + chr(iRevExt) + ".ARD" ).      */
/*           else if search(ttSubDir.DirName + cadNumber + string(iExt,"99") + chr(iRevExt + 32) + ".ARD" ) <> ?         */
/*              then  run create-ttCad (ttSubDir.DirName + cadNumber + string(iExt,"99") + chr(iRevExt + 32) + ".ARD" ). */
/*        end.                                                                                                            */
/*      end.  /* each ttSubDir */                                                                                        */
/*                                                                                                                        */
/*   end.                                                                                                                 */
  FOR EACH tt-artios NO-LOCK:
      DISPLAY tt-artios.cust-no.
  END.
  session:set-wait-state("").
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cadNumber
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cadNumber D-Dialog
ON LEAVE OF cadNumber IN FRAME D-Dialog /* CAD File/Project # */
DO:
    DEFINE VARIABLE iCnt AS INTEGER     NO-UNDO.
    
    ASSIGN cadNumber cadPath .
    RUN getNumofCADForm (OUTPUT iCnt).
    
    fiNumberCadForms:SCREEN-VALUE  = "Total Number of CAD Files: " + STRING(iCnt).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cadPath
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cadPath D-Dialog
ON LEAVE OF cadPath IN FRAME D-Dialog /* CAD File Path */
DO:
    DEFINE VARIABLE iCnt AS INTEGER     NO-UNDO.
    
    ASSIGN cadNumber .
    RUN getNumofCADForm (OUTPUT iCnt).

    fiNumberCADForms:SCREEN-VALUE  = "Total Number of CAD Files: " + STRING(iCnt).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-CadSeq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-CadSeq D-Dialog
ON VALUE-CHANGED OF cb-CadSeq IN FRAME D-Dialog /* CAD File */
DO:
    gcCadFileSelected = SELF:SCREEN-VALUE.
    RUN displayCadFormInfo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cCategory
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cCategory D-Dialog
ON HELP OF cCategory IN FRAME D-Dialog /* Category */
DO:
    DEFINE VARIABLE cReturn AS CHARACTER   NO-UNDO.
    
    RUN windows/l-fgcat.w (INPUT ipcCompany,
                           SELF:SCREEN-VALUE,
                           OUTPUT cReturn).

    IF cReturn <> "" AND SELF:SCREEN-VALUE <> ENTRY(1,cReturn) THEN 
        SELF:SCREEN-VALUE  = ENTRY(1,cReturn).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cCategory D-Dialog
ON LEAVE OF cCategory IN FRAME D-Dialog /* Category */
DO:
    DEFINE VARIABLE lError AS LOGICAL     NO-UNDO.
    
    RUN ValidateCategory(OUTPUT lError).
    IF lError THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cStyle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cStyle D-Dialog
ON HELP OF cStyle IN FRAME D-Dialog /* Style */
DO:
    DEFINE VARIABLE cReturn AS CHARACTER   NO-UNDO.

    RUN windows/l-stylec.w (INPUT ipcCompany,
                            cStyle:SCREEN-VALUE, 
                            OUTPUT cReturn).
    IF cReturn <> "" AND SELF:SCREEN-VALUE <> ENTRY(1,cReturn) THEN 
        cStyle:SCREEN-VALUE  = ENTRY(1,cReturn).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cStyle D-Dialog
ON LEAVE OF cStyle IN FRAME D-Dialog /* Style */
DO:
    DEFINE VARIABLE lError AS LOGICAL     NO-UNDO.
        
    RUN ValidateStyle(INPUT cstyle:SCREEN-VALUE IN FRAME {&FRAME-NAME},
                      OUTPUT lError).
    IF lError THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

{src/adm/template/dialogmn.i}
gcArtiosDir = GetArtiosDir(ipcCompany).
gcImageDir = GetImageDir(ipcCompany).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildAttributes D-Dialog 
PROCEDURE BuildAttributes :
/*------------------------------------------------------------------------------
  Purpose: Builds the Attributes temp table for the cadFile passed in    
  Parameters:  ipcCadFile as character
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcCadFile AS CHARACTER   NO-UNDO.

DEFINE VARIABLE cImageFile AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iResult AS INTEGER     NO-UNDO.                                                                          
DEFINE VARIABLE iCounter AS INTEGER     NO-UNDO.
DEFINE VARIABLE cArtiosField AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cAttribute AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cAttributeType AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cMessage AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cCadXValue AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cMappedValue AS CHARACTER   NO-UNDO.


ASSIGN /*Open CadFile and Build The Image File*/
    iResult = gchCadX:OpenDesign (ipcCadFile,0)
    cImageFile = gcImageDir + SUBSTRING(ipcCadFile, R-INDEX(ipcCadFile, "\") + 1) + ".jpg"
    iResult = gchCadX:SetOverlayClass(3,1) /*print dimensions*/
    iResult = gchCadX:SaveAsBitmap(1,cImageFile, 600, 600, 20, , , 1, 100)
.

CREATE ttAttribute.
ASSIGN 
    CadFile = ipcCadFile
    Attribute = "ImageFile"
    ValueImported = ""
    ValueMapped = cImageFile
    AttributeType = "T"
    LoadMessage = "Created"
    .

DO iCounter = 1 TO NUM-ENTRIES(gcAttributeList):
    ASSIGN 
        cArtiosField = ""
        cMessage = ""
        cCadXValue = ""
        cMappedValue = ""
        cAttribute = ENTRY(iCounter,gcAttributeList)
        cAttributeType = ENTRY(iCounter,gcAttributeTypeList)
        cArtiosField = GetXrefField(cAttribute) 
        /*find the Artios DB field for this attribute*/
        .
    IF cArtiosField NE "" THEN DO:
        /*Get the value of the field from the Artios DB*/
        IF cAttributeType EQ "N" THEN
            cCadXValue = STRING(gchCadX:ReturnNumericCode4Param(cArtiosField)).
        ELSE 
            cCadXValue = gchCadX:ReturnTextCode4Param(cArtiosField).
        IF cCadXValue NE "" THEN DO:
            /*See if the value itself has a cross reference value*/
            cMappedValue = GetXref(cAttribute,cCadXValue).
            IF cMappedValue NE "" THEN
                cMessage = "Value mapped via cross reference".
            ELSE DO:
                cMessage = "No cross reference available".
                cMappedValue = cCadXValue.
            END.
        END.
        ELSE
            cMessage = "No value returned from Artios DB for attribute: " + cAttribute.
    END.
    ELSE
        cMessage = "No artios field defined for attribute: " + cAttribute.
 
    CREATE ttAttribute.
    ASSIGN
        ttAttribute.CadFile = ipcCadFile
        ttAttribute.Attribute = cAttribute
        ttAttribute.ValueImported = cCadXValue
        ttAttribute.ValueMapped = cMappedValue
        ttAttribute.AttributeType = cAttributeType
        ttAttribute.LoadMessage = cMessage
        .

END.
iResult = gchCadX:CloseDesign().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildProject D-Dialog 
PROCEDURE BuildProject :
/*------------------------------------------------------------------------------
  Purpose: Builds the Project with multiple ard files    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcProjectPath AS CHARACTER   NO-UNDO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CadXConnect D-Dialog 
PROCEDURE CadXConnect :
/*------------------------------------------------------------------------------
  Purpose:  Instantiates the CadX Handle   
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 
    CREATE "ArtiosCAD-X.CadXCtrl.1" gchCadX CONNECT NO-ERROR.
    IF NOT VALID-HANDLE (gchCadX) THEN
        CREATE "ArtiosCAD-X.CadXCtrl.1" gchCadX NO-ERROR.
    IF NOT VALID-HANDLE (gchCadX) THEN DO:
        MESSAGE "Unable to Connect to Artios CadX. Please make sure Artios CadX is installed."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.
                                            
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CadXDisconnect D-Dialog 
PROCEDURE CadXDisconnect :
/*------------------------------------------------------------------------------
  Purpose: Destroys the CadXHandle    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-ttCad D-Dialog 
PROCEDURE create-ttCad :
/*------------------------------------------------------------------------------
  Purpose: Build the temp table to be passed back that will contain the
  information necessary for the estimate to be built 
  Parameters:  ipcFilename - Cad file path
  Notes:       
------------------------------------------------------------------------------*/
/* DEFINE INPUT  PARAMETER ipcFilename AS CHARACTER   NO-UNDO.                                                               */
/*                                                                                                                           */
/* DEFINE VARIABLE cFilenameJpg AS CHARACTER   NO-UNDO.                                                                      */
/* DEFINE VARIABLE iResult AS INTEGER     NO-UNDO.                                                                           */
/* DEFINE VARIABLE iSequence AS INTEGER     NO-UNDO.                                                                         */
/* DEFINE VARIABLE cDieInch AS CHARACTER   NO-UNDO.                                                                          */
/* DEFINE VARIABLE cCadPath AS CHARACTER   NO-UNDO.                                                                          */
/* DEFINE VARIABLE chCadX AS COM-HANDLE      NO-UNDO.                                                                        */
/*                                                                                                                           */
/*                                                                                                                           */
/* CREATE "ArtiosCAD-X.CadXCtrl.1" chCadX CONNECT NO-ERROR.                                                                  */
/* IF NOT VALID-HANDLE (chCadX) THEN                                                                                         */
/*     CREATE "ArtiosCAD-X.CadXCtrl.1" chCadX NO-ERROR.                                                                      */
/* IF NOT VALID-HANDLE (chCadX) THEN RETURN.                                                                                 */
/*                                                                                                                           */
/* FIND FIRST sys-ctrl                                                                                                       */
/*     WHERE sys-ctrl.company EQ ipcCompany                                                                                  */
/*       AND sys-ctrl.name    EQ "CADFILE"                                                                                   */
/*     NO-LOCK NO-ERROR.                                                                                                     */
/*                                                                                                                           */
/* ASSIGN                                                                                                                    */
/*     iSequence = 1                                                                                                         */
/*     giFormNumber = giFormNumber + 1                                                                                         */
/*     giBlankNumber = 1                                                                                                      */
/*     cCadPath = IF AVAIL sys-ctrl AND sys-ctrl.char-fld <> ""                                                              */
/*                 THEN sys-ctrl.char-fld else "c:\tmp\"                                                                     */
/*     cFileNameJPG = cCadPath + SUBSTRING(ipcFilename, R-INDEX(ipcFilename, "\") + 1) + ".jpg"                              */
/*     iResult = chCadX:OpenDesign (ipcFilename,0)                                                                           */
/*     iResult = chCadX:SetOverlayClass(3,1) /*print dimensions*/                                                            */
/*     iResult = chCadX:SaveAsBitmap(1,cFilenameJPG, 600, 600, 20, , , 1, 100)                                               */
/*     .                                                                                                                     */
/*                                                                                                                           */
/* FIND FIRST tt-CompStyle                                                                                                   */
/*     WHERE tt-CompStyle.form-num EQ giFormNumber                                                                            */
/*     NO-ERROR.                                                                                                             */
/*                                                                                                                           */
/* cDieInch = chCadX:ReturnTextCode4Param("#LENRULE").                                                                       */
/* IF INDEX(cDieInch,"+") > 0                                                                                                */
/*     THEN cDieInch = SUBSTRING(cDieInch,1,INDEX(cDieInch,"+") - 1).                                                        */
/*                                                                                                                           */
/* CREATE tt-artios.                                                                                                         */
/* ASSIGN tt-artios.cadnum = /*cadNumber*/ chCadX:ReturnTextCode4Param("#ITEM$")                                             */
/*     tt-artios.cadfile = ipcFilename                                                                                       */
/*     tt-artios.custname = chCadX:ReturnTextCode4Param("DBGET(CUST,NAME$)")                                                 */
/*     tt-artios.partname = chCadX:ReturnTextCode4Param("DBGET(DESIGN,SDSC1$)")                                              */
/*     tt-artios.partnum = chCadX:ReturnTextCode4Param("#ITEM$")                                                             */
/*     tt-artios.style = /*chCadX:ReturnTextCode4Param("#CFN$")*/                                                            */
/*                                if avail tt-CompStyle then tt-CompStyle.CompStyle[1] else cStyle                           */
/*          tt-artios.flute = ""                                                                                             */
/*          tt-artios.test = ""                                                                                              */
/*          tt-artios.board = substring(chCadX:ReturnTextCode4Param("BRD$"),1,9)                                             */
/*          tt-artios.len = chCadX:ReturnNumericCode4Param("L")                                                              */
/*          tt-artios.wid = chCadX:ReturnNumericCode4Param("W")                                                              */
/*          tt-artios.dep = chCadX:ReturnNumericCode4Param("D")                                                              */
/*          tt-artios.t-len = chCadX:ReturnNumericCode4Param("#MANSIZEX")                                                    */
/*          tt-artios.t-wid = chCadX:ReturnNumericCode4Param("#MANSIZEY")                                                    */
/*          tt-artios.seq = iSequence                                                                                        */
/*          tt-artios.ratio = if avail tt-CompStyle then tt-CompStyle.CompRatio[1] else dRatio-1                             */
/*          tt-artios.form-num = giFormNumber                                                                                 */
/*          tt-artios.blank-num = giBlankNumber                                                                               */
/*          tt-artios.die-in = DEC(cDieInch)                                                                                 */
/*          tt-artios.setQty = fiQty                                                                                         */
/*          tt-artios.CompQty = if avail tt-CompStyle then tt-CompStyle.CompQty[1] else iCompQty-1                           */
/*          tt-artios.procat = cCategory                                                                                     */
/*          tt-artios.cust-no = chCadX:ReturnTextCode4Param("DBGET(CUST,NUMBR$)")                                            */
/*          tt-artios.sman    = chCadX:ReturnTextCode4Param("DBGET(SLSPN,SNAME$)")                                           */
/*          tt-artios.dienum =  chCadX:ReturnTextCode4Param("DBGET(DESIGN,SDSC3$)")                                          */
/*          tt-artios.NumOfComponents = tt-CompStyle.NumOfComponents                                                         */
/*          tt-artios.CompNumUp = if avail tt-CompStyle then tt-CompStyle.CompNumUp[1] else 1                                */
/*          tt-artios.DesignImg = cFileNameJPG                                                                               */
/*          tt-artios.pur-man = IF AVAIL tt-CompStyle THEN tt-CompStyle.CompPurMan[1] ELSE                                   */
/*                              IF rs-man-pur = "M" THEN NO                                                                  */
/*                              ELSE YES                                                                                     */
/*          tt-artios.grain   = chCadX:ReturnTextCode4Param("#GRAIN$")                                                       */
/*          tt-artios.DesignerName = chCadX:ReturnTextCode4Param("DBGET(DSGNR,FNAME$)")                                      */
/*                                  + " " +                                                                                  */
/*                                  chCadX:ReturnTextCode4Param("DBGET(DSGNR,LNAME$)")                                       */
/*                           .                                                                                               */
/*                                                                                                                           */
/*        if avail tt-compstyle and tt-CompStyle.NumOfComponents > 1 then do while iSequence < tt-CompStyle.NumOfComponents: */
/*                                                                                                                           */
/*           create tt-artios.                                                                                               */
/*           assign iSequence = iSequence + 1                                                                                */
/*                  giBlankNumber = giBlankNumber + 1                                                                          */
/*                  tt-artios.cadnum = /*cadNumber*/ chCadX:ReturnTextCode4Param("#ITEM$")                                   */
/*                  tt-artios.cadfile = ipcFilename                                                                          */
/*                  tt-artios.custname = chCadX:ReturnTextCode4Param("DBGET(CUST,NAME$)")                                    */
/*                  tt-artios.partname = chCadX:ReturnTextCode4Param("DBGET(DESIGN,SDSC1$)")                                 */
/*                  tt-artios.partnum = chCadX:ReturnTextCode4Param("#ITEM$")                                                */
/*                  tt-artios.style = /*chCadX:ReturnTextCode4Param("#CFN$")*/                                               */
/*                                      if avail tt-CompStyle then tt-CompStyle.CompStyle[iSequence] else cStyle             */
/*                  tt-artios.flute = ""                                                                                     */
/*                  tt-artios.test = ""                                                                                      */
/*                  tt-artios.board = substring(chCadX:ReturnTextCode4Param("BRD$"),1,9)                                     */
/*                  tt-artios.len = chCadX:ReturnNumericCode4Param("L")                                                      */
/*                  tt-artios.wid = chCadX:ReturnNumericCode4Param("W")                                                      */
/*                  tt-artios.dep = chCadX:ReturnNumericCode4Param("D")                                                      */
/*                  tt-artios.t-len = chCadX:ReturnNumericCode4Param("#MANSIZEX")                                            */
/*                  tt-artios.t-wid = chCadX:ReturnNumericCode4Param("#MANSIZEY")                                            */
/*                  tt-artios.seq = iSequence                                                                                */
/*                  tt-artios.ratio = if avail tt-CompStyle then tt-CompStyle.CompRatio[iSequence] else dRatio-2             */
/*                  tt-artios.form-num = giFormNumber                                                                         */
/*                  tt-artios.blank-num = giBlankNumber                                                                       */
/*                  tt-artios.die-in =  DEC(cDieInch)                                                                        */
/*                  tt-artios.setQty = fiQty                                                                                 */
/*                  tt-artios.CompQty =  if avail tt-CompStyle then tt-CompStyle.CompQty[iSequence] else 0                   */
/*                  tt-artios.procat = cCategory                                                                             */
/*                  tt-artios.cust-no = chCadX:ReturnTextCode4Param("DBGET(CUST,NUMBR$)")                                    */
/*                  tt-artios.sman    = chCadX:ReturnTextCode4Param("DBGET(SLSPN,SNAME$)")                                   */
/*                  tt-artios.dienum =  chCadX:ReturnTextCode4Param("DBGET(DESIGN,SDSC3$)")                                  */
/*                  tt-artios.NumOfComponents = tt-CompStyle.NumOfComponents                                                 */
/*                  tt-artios.CompNumUp = if avail tt-CompStyle then tt-CompStyle.CompNumUp[iSequence] else 1                */
/*                  tt-artios.DesignImg = cFileNameJPG                                                                       */
/*                  tt-artios.pur-man = if avail tt-CompStyle then tt-CompStyle.CompPurMan[iSequence] else                   */
/*                                      IF rs-man-pur EQ "M" THEN NO ELSE YES                                                */
/*                  tt-artios.grain   = chCadX:ReturnTextCode4Param("#GRAIN$")                                               */
/*                  tt-artios.DesignerName = chCadX:ReturnTextCode4Param("DBGET(DSGNR,FNAME$)")                              */
/*                                  + " " +                                                                                  */
/*                                  chCadX:ReturnTextCode4Param("DBGET(DSGNR,LNAME$)")                                       */
/*                   .                                                                                                       */
/*        end.                                                                                                               */
/*                                                                                                                           */
/*        iResult = chCadX:CloseDesign().                                                                                    */
           
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
  DISPLAY cadPath cadNumber fiQty cCategory cb-CadSeq fiNumberCadForms cStyle 
      WITH FRAME D-Dialog.
  ENABLE BROWSE-2 btnLoad cadPath btnCADPathLookup cadNumber btnCAD#Lookup 
         fiQty cCategory cb-CadSeq cStyle Btn_OK Btn_Cancel 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getNumofCADForm D-Dialog 
PROCEDURE getNumofCADForm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opiNumCadForm AS INTEGER     NO-UNDO.

    DEFINE VARIABLE cTmpFile AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iExt AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iRevExt AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cCad# AS CHARACTER   NO-UNDO.
 
    EMPTY TEMP-TABLE ttSubDir.
/*                                                      */
/*     IF cadPath = "" THEN cadPath =                   */
/*     if cadPath = "" then cadPath = "c:\artios\asi\". */
 
IF cadPath EQ '' THEN DO:
    opiNumCadForm = 1. 
    RETURN.
END.
IF SUBSTRING(cadPath,LENGTH(cadPath),1) <> "/" 
    AND SUBSTRING(cadPath,LENGTH(cadPath),1) <> "\" THEN
    cadPath = cadPath + "\".

CREATE ttSubDir.
ASSIGN ttSubDir.DirName = cadPath.

RUN getSubDirList (cadPAth).
    
IF INDEX(CadNumber,".ard") = 0 
    AND SEARCH(cadNumber + ".ard") = ? THEN DO:
    
    FOR EACH ttSubDir:
        cTmpFile = ttSubDir.DirName + CadNumber.
    

       do iExt = 1 to giProjectCount:
          if search(cTmpFile + string(iExt,"99") + ".ARD" ) <> ? then do:
             assign opiNumCadForm = opiNumCadForm + 1
                       cCAD# = CadNumber + string(iExt,"99").       
             cb-CADSeq:add-last(string(opiNumCadForm,"99") + " " + cCAD#) in frame {&frame-name}.
          end.
          /* check revision file existing */
         do iRevExt = 65 to 90:  /* A - Z */
     
           if search(cTmpFile + string(iExt,"99") + chr(iRevExt) + ".ARD" ) <> ? or
              search(cTmpFile + string(iExt,"99") + chr(iRevExt + 32) + ".ARD" ) <> ? 
            then do:
               assign opiNumCadForm = opiNumCadForm + 1
                          cCAD# = CadNumber + string(iExt,"99") + chr(iRevExt). 
               cb-CADSeq:add-last(string(opiNumCadForm,"99") + " " + cCAD#).                 
            end.
         end.
       end.
     end.  /*for each */
  end.  /* if search = ? */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getSubDirList D-Dialog 
PROCEDURE getSubDirList :
/*------------------------------------------------------------------------------
  Purpose:  Build the ttSubDir temp table   
  Parameters:  ipcCurrentDirectory
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCurrentDirectory AS CHARACTER.

    DEFINE VARIABLE cFileName AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cFilePath AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cAttrib AS CHARACTER   NO-UNDO.
    
    INPUT FROM OS-DIR (ipcCurrentDirectory).
    
    REPEAT:
        IMPORT cFileName.
        IF cFileName = '.' OR cFileName = '..' OR cFileName = ? THEN NEXT.
        FILE-INFO:FILE-NAME = ipcCurrentDirectory + cFileName.
        IF NOT FILE-INFO:FILE-TYPE BEGINS "D" THEN NEXT.
        IF FILE-INFO:FULL-PATHNAME <> ? THEN DO:
            /*PUT UNFORMATTED FILE-INFO:FULL-PATHNAME SKIP.*/
            CREATE ttSubDir.
            ASSIGN ttSubDir.dirname = FILE-INFO:FULL-PATHNAME + "\" 
                   .
            RUN getSubDirList(INPUT FILE-INFO:FULL-PATHNAME + "\").
        END.
    END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValidateCategory D-Dialog 
PROCEDURE ValidateCategory :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE OUTPUT PARAMETER op-error AS LOG NO-UNDO.
   
   DO WITH FRAME {&FRAME-NAME}:
   
      IF NOT CAN-FIND(FIRST fgcat WHERE
         fgcat.company EQ ipcCompany AND
         fgcat.procat  EQ cCategory:SCREEN-VALUE) OR
         cCategory:SCREEN-VALUE EQ "" THEN DO:
         MESSAGE "Invalid Category." VIEW-AS ALERT-BOX ERROR.
         op-error = YES.
         APPLY "entry" TO cCategory.
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValidateStyle D-Dialog 
PROCEDURE ValidateStyle :
/*------------------------------------------------------------------------------
  Purpose:  Validate Style
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER ipcStyle AS CHARACTER   NO-UNDO.
   DEFINE OUTPUT PARAMETER oplError AS LOGICAL     NO-UNDO.
   
   IF ipcStyle NE '' THEN 
       FIND FIRST style
           WHERE style.company  EQ ipcCompany
             AND style.style    EQ ipcStyle
             AND style.industry EQ "2"
           NO-LOCK NO-ERROR.
    IF NOT AVAIL style THEN DO:
        MESSAGE "Invalid Style." VIEW-AS ALERT-BOX ERROR.
        oplError = YES.
    END.
           
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetArtiosDir D-Dialog 
FUNCTION GetArtiosDir RETURNS CHARACTER
  ( ipcCo AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  Get the file path from the NK1
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cPath AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE lFound AS LOGICAL     NO-UNDO.

    RUN sys/ref/nk1look.p (INPUT ipcCo,
                           INPUT "ARTIOSCAD",
                           INPUT "C",
                           INPUT NO,
                           INPUT NO,
                           INPUT "",
                           INPUT "",
                           OUTPUT cPath,
                           OUTPUT lFound).
    IF NOT lFound OR cPath EQ '' THEN
        cPath = 'C:\tmp\'.
    
    RETURN cPath.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetImageDir D-Dialog 
FUNCTION GetImageDir RETURNS CHARACTER
  ( ipcCo AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  Get the file path for images from NK1
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cPath AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE lFound AS LOGICAL     NO-UNDO.

    RUN sys/ref/nk1look.p (INPUT ipcCo,
                           INPUT "CADFILE",
                           INPUT "C",
                           INPUT NO,
                           INPUT NO,
                           INPUT "",
                           INPUT "",
                           OUTPUT cPath,
                           OUTPUT lFound).
    IF NOT lFound OR cPath EQ '' THEN
        cPath = 'C:\tmp\'.
    
    RETURN cPath.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetXref D-Dialog 
FUNCTION GetXref RETURNS CHARACTER
  ( ipcType AS CHARACTER, ipcLookup AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose: Lookup a xref value  
    Notes:  
------------------------------------------------------------------------------*/
DEFINE VARIABLE cReturn AS CHARACTER   NO-UNDO.

RUN sys/ref/getXref.p (INPUT ipcCompany,
                       INPUT ipcType, 
                       INPUT ipcLookup, 
                       OUTPUT cReturn).
RETURN cReturn.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetXrefField D-Dialog 
FUNCTION GetXrefField RETURNS CHARACTER
  (ipcLookup AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  Runs a lookup for an Artios DB field
    Notes:  
------------------------------------------------------------------------------*/
DEFINE VARIABLE cReturn AS CHARACTER   NO-UNDO.
cReturn = GetXRef(INPUT "Artios",
                  INPUT ipcLookup).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

