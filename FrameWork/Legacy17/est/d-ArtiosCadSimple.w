&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
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

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME bAttributes

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttAttribute

/* Definitions for BROWSE bAttributes                                   */
&Scoped-define FIELDS-IN-QUERY-bAttributes ttAttribute.Attribute ttAttribute.ValueImported ttAttribute.ValueMapped ttAttribute.LoadMessage   
&Scoped-define ENABLED-FIELDS-IN-QUERY-bAttributes   
&Scoped-define SELF-NAME bAttributes
&Scoped-define QUERY-STRING-bAttributes FOR EACH ttAttribute
&Scoped-define OPEN-QUERY-bAttributes OPEN QUERY {&SELF-NAME} FOR EACH ttAttribute.
&Scoped-define TABLES-IN-QUERY-bAttributes ttAttribute
&Scoped-define FIRST-TABLE-IN-QUERY-bAttributes ttAttribute


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-bAttributes}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi_CADFile btnCAD#Lookup btnLoad bAttributes ~
btnOK btnCancel 
&Scoped-Define DISPLAYED-OBJECTS fi_CADFile 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetArtiosDir Dialog-Frame 
FUNCTION GetArtiosDir RETURNS CHARACTER
  ( ipcCo AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetImageDir Dialog-Frame 
FUNCTION GetImageDir RETURNS CHARACTER
  ( ipcCo AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetXref Dialog-Frame 
FUNCTION GetXref RETURNS CHARACTER
  ( ipcType AS CHARACTER, ipcLookup AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetXrefField Dialog-Frame 
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

DEFINE BUTTON btnCancel 
     LABEL "Cancel" 
     SIZE 26 BY 1.14.

DEFINE BUTTON btnLoad 
     LABEL "Load" 
     SIZE 15 BY 1.

DEFINE BUTTON btnOK AUTO-GO 
     LABEL "OK" 
     SIZE 26 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE fi_CADFile AS CHARACTER FORMAT "X(256)":U 
     LABEL "Select CAD File" 
     VIEW-AS FILL-IN 
     SIZE 78 BY 1
     FONT 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY bAttributes FOR 
      ttAttribute SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE bAttributes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS bAttributes Dialog-Frame _FREEFORM
  QUERY bAttributes DISPLAY
      ttAttribute.Attribute FORMAT "x(20)" COLUMN-LABEL "Field"
    ttAttribute.ValueImported FORMAT "x(30)" COLUMN-LABEL "Imported Value"
    ttAttribute.ValueMapped FORMAT "x(30)" COLUMN-LABEL "Mapped Value"
    ttAttribute.LoadMessage FORMAT "x(50)" COLUMN-LABEL "Note"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 165 BY 15.48
         FONT 1 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     fi_CADFile AT ROW 1.95 COL 21 COLON-ALIGNED WIDGET-ID 6
     btnCAD#Lookup AT ROW 1.95 COL 101 WIDGET-ID 4
     btnLoad AT ROW 3.62 COL 23 WIDGET-ID 164
     bAttributes AT ROW 5.52 COL 2 WIDGET-ID 100
     btnOK AT ROW 21.71 COL 41
     btnCancel AT ROW 21.71 COL 79 WIDGET-ID 2
     SPACE(64.59) SKIP(0.38)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "Build Estimate from Artios CAD File"
         DEFAULT-BUTTON btnOK CANCEL-BUTTON btnCancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
/* BROWSE-TAB bAttributes btnLoad Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE bAttributes
/* Query rebuild information for BROWSE bAttributes
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttAttribute.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE bAttributes */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Build Estimate from Artios CAD File */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCAD#Lookup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCAD#Lookup Dialog-Frame
ON CHOOSE OF btnCAD#Lookup IN FRAME Dialog-Frame
DO:
    DEFINE VARIABLE cCadFile AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE lOKClicked AS LOGICAL     NO-UNDO.
    
    SYSTEM-DIALOG GET-FILE cCadFile 
                TITLE 'Select Artios CAD File to insert'
                FILTERS 'ARD Files    (*.ard)' '*.ard'
                INITIAL-DIR gcArtiosDir
                MUST-EXIST USE-FILENAME UPDATE lOKClicked.

    IF lOKClicked THEN
        ASSIGN fi_CADFile:SCREEN-VALUE = cCadFile.
     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel Dialog-Frame
ON CHOOSE OF btnCancel IN FRAME Dialog-Frame /* Cancel */
DO:
    opCADCAM = ''.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLoad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLoad Dialog-Frame
ON CHOOSE OF btnLoad IN FRAME Dialog-Frame /* Load */
DO:
    SESSION:SET-WAIT-STATE("GENERAL").
    RUN CadXConnect.
    ASSIGN fi_CADFile.
    EMPTY TEMP-TABLE ttAttribute.

/*     IF cadPath NE "" THEN                */
/*         RUN BuildProject(INPUT cadPath). */
/*     ELSE                                 */
    IF fi_CADFile NE "" THEN DO:
        RUN BuildAttributes(fi_CADFile).
        gcCadFileSelected = fi_CADFile.
        {&CLOSE-QUERY-bAttributes}   
        {&OPEN-QUERY-bAttributes}
    END.
    ELSE DO:
        MESSAGE "No Artios File or Project folder specified."
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY "entry" TO fi_CADFile.
    END.
    RUN CadXDisconnect.
    SESSION:SET-WAIT-STATE("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME bAttributes
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
gcArtiosDir = GetArtiosDir(ipcCompany).
gcImageDir = GetImageDir(ipcCompany).
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildAttributes Dialog-Frame 
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
                cMessage = "Value stored direct from Artios".
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
FOR EACH ttAttribute WHERE ttAttribute.CadFile EQ gcCadFileSelected:
    MESSAGE ttAttribute.attribute ttAttribute.ValueImported ttAttribute.LoadMessage
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CadXConnect Dialog-Frame 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CadXDisconnect Dialog-Frame 
PROCEDURE CadXDisconnect :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  DISPLAY fi_CADFile 
      WITH FRAME Dialog-Frame.
  ENABLE fi_CADFile btnCAD#Lookup btnLoad bAttributes btnOK btnCancel 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetArtiosDir Dialog-Frame 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetImageDir Dialog-Frame 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetXref Dialog-Frame 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetXrefField Dialog-Frame 
FUNCTION GetXrefField RETURNS CHARACTER
  (ipcLookup AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  Runs a lookup for an Artios DB field
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cReturn AS CHARACTER   NO-UNDO.
    
    cReturn = GetXRef(INPUT "Artios",
                      INPUT ipcLookup).
    RETURN cReturn.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

