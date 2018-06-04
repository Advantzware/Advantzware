
/*------------------------------------------------------------------------
    File        : ArtiosProcs.p
    Purpose     : 

    Syntax      :

    Description : Houses Artios ARD file processing programs

    Author(s)   : BV
    Created     : Sun Jun 03 12:47:38 EDT 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{est\ttArtiosAttributes.i NEW SHARED}

DEFINE VARIABLE gcAttributeList     AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcAttributeTypeList AS CHARACTER NO-UNDO.
    
ASSIGN 
    gcAttributeList     = "Cad Number," +
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
                    "Blank Width," +
                    "Blank Length," +
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
                    
    gcAttributeTypeList = "T," +
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


/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fGetArtiosDir RETURNS CHARACTER 
    (ipcCompany AS CHARACTER) FORWARD.

FUNCTION fGetArtiosImageDir RETURNS CHARACTER 
    (ipcCompany AS CHARACTER) FORWARD.

FUNCTION fGetXref RETURNS CHARACTER
    (ipcCompany AS CHARACTER, ipcType AS CHARACTER, ipcLookup AS CHARACTER ) FORWARD.
  
FUNCTION fGetXrefField RETURNS CHARACTER
    (ipcCompany AS CHARACTER, ipcLookup AS CHARACTER ) FORWARD.  
/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

PROCEDURE GetArtiosDir:
/*------------------------------------------------------------------------------
 Purpose: Public Procedure for getting the Artios Directory
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcDir AS CHARACTER NO-UNDO.

opcDir = fGetArtiosDir(ipcCompany).

END PROCEDURE.

PROCEDURE pBuildAttributes:
    /*------------------------------------------------------------------------------
      Purpose: Builds the Attributes temp table for the cadFile passed in    
      Parameters:  ipcCadFile as character
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCadFile AS CHARACTER   NO-UNDO.

    DEFINE VARIABLE cImageDir      AS CHARACTER        NO-UNDO.
    DEFINE VARIABLE cImageFile     AS CHARACTER        NO-UNDO.
    DEFINE VARIABLE iResult        AS INTEGER          NO-UNDO.                                                                          
    DEFINE VARIABLE iCounter       AS INTEGER          NO-UNDO.
    DEFINE VARIABLE cArtiosField   AS CHARACTER        NO-UNDO.
    DEFINE VARIABLE cAttribute     AS CHARACTER        NO-UNDO.
    DEFINE VARIABLE cAttributeType AS CHARACTER        NO-UNDO.
    DEFINE VARIABLE cMessage       AS CHARACTER        NO-UNDO.
    DEFINE VARIABLE cCadXValue     AS CHARACTER        NO-UNDO.
    DEFINE VARIABLE cMappedValue   AS CHARACTER        NO-UNDO.
    DEFINE VARIABLE hdCadX         AS COMPONENT-HANDLE NO-UNDO.
    
    RUN pCADXConnect(OUTPUT hdCadX).
    ASSIGN /*Open CadFile and Build The Image File*/
        cImageDir  = fGetArtiosImageDir(ipcCompany)
        iResult    = hdCadX:OpenDesign (ipcCadFile,0)
        cImageFile = cImageDir + SUBSTRING(ipcCadFile, R-INDEX(ipcCadFile, "\") + 1) + ".jpg"
        iResult    = hdCadX:SetOverlayClass(3,1) /*print dimensions*/
        iResult    = hdCadX:SaveAsBitmap(1,cImageFile, 600, 600, 20, , , 1, 100)
        .

    CREATE ttArtiosAttributes.
    ASSIGN 
        ttArtiosAttributes.CadFile       = ipcCadFile
        ttArtiosAttributes.Attribute     = "ImageFile"
        ttArtiosAttributes.ValueImported = ""
        ttArtiosAttributes.ValueMapped   = cImageFile
        ttArtiosAttributes.AttributeType = "T"
        ttArtiosAttributes.LoadMessage   = "Created"
        .

    DO iCounter = 1 TO NUM-ENTRIES(gcAttributeList):
        ASSIGN 
            cArtiosField   = ""
            cMessage       = ""
            cCadXValue     = ""
            cMappedValue   = ""
            cAttribute     = ENTRY(iCounter,gcAttributeList)
            cAttributeType = ENTRY(iCounter,gcAttributeTypeList)
            cArtiosField   = fGetXrefField(ipcCompany,cAttribute) 
            /*find the Artios DB field for this attribute*/
            .
        IF cArtiosField NE "" THEN 
        DO:
            /*Get the value of the field from the Artios DB*/
            IF cAttributeType EQ "N" THEN
                cCadXValue = STRING(hdCadX:ReturnNumericCode4Param(cArtiosField)).
            ELSE
                cCadXValue = hdCadX:ReturnTextCode4Param(cArtiosField).
            IF cCadXValue NE "" THEN 
            DO:
                /*See if the value itself has a cross reference value*/
                cMappedValue = fGetXref(ipcCompany, cAttribute, cCadXValue).
                IF cMappedValue NE "" THEN
                    cMessage = "Value mapped via cross reference".
                ELSE 
                DO:
                    cMessage = "Value stored direct from Artios".
                    cMappedValue = cCadXValue.
                END.
            END.
            ELSE
                cMessage = "No value returned from Artios DB for attribute: " + cAttribute.
        END.
        ELSE
            cMessage = "No artios field defined for attribute: " + cAttribute.
 
        CREATE ttArtiosAttributes.
        ASSIGN
            ttArtiosAttributes.CadFile       = ipcCadFile
            ttArtiosAttributes.Attribute     = cAttribute
            ttArtiosAttributes.ValueImported = cCadXValue
            ttArtiosAttributes.ValueMapped   = cMappedValue
            ttArtiosAttributes.AttributeType = cAttributeType
            ttArtiosAttributes.LoadMessage   = cMessage
            .

    END.
    iResult = hdCadX:CloseDesign().
    

END PROCEDURE.

PROCEDURE pCADXConnect PRIVATE:
    /*------------------------------------------------------------------------------
      Purpose:  Instantiates the CadX Handle   
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER ophdCadX AS COMPONENT-HANDLE NO-UNDO.
    
    CREATE "ArtiosCAD-X.CadXCtrl.1" ophdCadX CONNECT NO-ERROR.
    IF NOT VALID-HANDLE (ophdCadX) THEN
        CREATE "ArtiosCAD-X.CadXCtrl.1" ophdCadX NO-ERROR.
    IF NOT VALID-HANDLE (ophdCadX) THEN 
    DO:
        MESSAGE "Unable to Connect to Artios CadX. Please make sure Artios CadX is installed."
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        RETURN.
    END.

END PROCEDURE.

PROCEDURE pSetArtiosData:
    /*------------------------------------------------------------------------------
     Purpose: Sets the cData array for a given Artios file
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFile AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcData AS CHARACTER EXTENT 200.

    DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.

    EMPTY TEMP-TABLE ttArtiosAttributes.
    RUN pBuildAttributes(ipcCompany, ipcFile).

    FOR EACH ttArtiosAttributes:
        CASE ttArtiosAttributes.Attribute:
            WHEN "Cad Number" THEN 
                opcData[27] = ttArtiosAttributes.ValueMapped.
            WHEN "Customer Number" THEN 
                opcData[6] = ttArtiosAttributes.ValueMapped.
            WHEN "Part Number" THEN
                opcData[8] = ttArtiosAttributes.ValueMapped.
            WHEN "Part Name" THEN
                opcData[9] = ttArtiosAttributes.ValueMapped.                    
            WHEN "Style" THEN
                opcData[12] = ttArtiosAttributes.ValueMapped.
            WHEN "Flute" THEN
                opcData[13] = ttArtiosAttributes.ValueMapped.
            WHEN "Test" THEN
                opcData[14] = ttArtiosAttributes.ValueMapped.
            WHEN "Board Code" THEN
                opcData[15] = ttArtiosAttributes.ValueMapped.
            WHEN "Length" THEN
                opcData[19] = ttArtiosAttributes.ValueMapped.
            WHEN "Width" THEN
                opcData[20] = ttArtiosAttributes.ValueMapped.
            WHEN "Depth" THEN
                opcData[21] = ttArtiosAttributes.ValueMapped.
            WHEN "Blank Width" THEN
                opcData[25] = ttArtiosAttributes.ValueMapped.
            WHEN "Blank Length" THEN
                opcData[24] = ttArtiosAttributes.ValueMapped.
            WHEN "Die Inches" THEN
                opcData[29] = ttArtiosAttributes.ValueMapped.
            WHEN "Qty" THEN
                opcData[3] = ttArtiosAttributes.ValueMapped.
            WHEN "Qty Per Set" THEN
                opcData[90] = ttArtiosAttributes.ValueMapped.
            WHEN "Product Category" THEN
                opcData[18] = ttArtiosAttributes.ValueMapped.
            WHEN "Die Number" THEN
                opcData[28] = ttArtiosAttributes.ValueMapped.
            WHEN "Number Up" THEN
                opcData[91] = ttArtiosAttributes.ValueMapped.
            WHEN "Manufactured Or Purchased" THEN
                opcData[26] = ttArtiosAttributes.ValueMapped.
            WHEN "Salesperson ID" THEN
                opcData[30] = ttArtiosAttributes.ValueMapped.
            WHEN "Grain Direction" THEN
                opcData[93] = ttArtiosAttributes.ValueMapped.
            WHEN "Designer Firstname" THEN DO:
                IF opcData[32] EQ "" THEN 
                    opcData[32] = ttArtiosAttributes.ValueMapped.
                ELSE opcData[32] = ttArtiosAttributes.ValueMapped + " " + opcData[32].
            END.    
            WHEN "Designer Lastname" THEN DO:
                IF opcData[32] EQ "" THEN 
                    opcData[32] = ttArtiosAttributes.ValueMapped.
                ELSE opcData[32] = opcData[32] + " " + ttArtiosAttributes.ValueMapped.
            END.
            WHEN "ImageFile" THEN DO:
                opcData[94] = ttArtiosAttributes.ValueMapped.
            END.
        END CASE.
    END.
    /*Force Fix for Demo*/
    IF TRIM(opcData[1]) EQ '' THEN opcData[1] = 'C'.  /*Industry*/
    IF TRIM(opcData[3]) EQ '' THEN opcData[3] = '1000'.  /*Quantity*/
    IF TRIM(opcData[6]) EQ '' THEN opcData[6] = 'STOCK'. /*Cust No*/
    IF TRIM(opcData[8]) EQ '' THEN opcData[8] = 'SamplePartNo'.  /*Part ID*/
    IF TRIM(opcData[12]) EQ '' THEN opcData[12] = 'RSC'. /*Style*/
    IF TRIM(opcData[13]) EQ '' THEN opcData[13] = 'C'.  /*Flute*/
    IF TRIM(opcData[14]) EQ '' THEN opcData[14] = '200'. /*Test*/
    IF TRIM(opcData[18]) EQ '' THEN opcData[18] = 'BOXES'. /*Category*/
    IF TRIM(opcData[19]) EQ '' THEN opcData[19] = '10'. /*Length*/
    IF TRIM(opcData[20]) EQ '' THEN opcData[20] = '10'. /*Width*/
    
END PROCEDURE.

/* ************************  Function Implementations ***************** */


FUNCTION fGetArtiosDir RETURNS CHARACTER 
    ( ipcCompany AS CHARACTER ) :
    /*------------------------------------------------------------------------------
      Purpose:  Get the file path from the NK1
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cPath  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound AS LOGICAL   NO-UNDO.

    RUN sys/ref/nk1look.p (INPUT ipcCompany,
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

FUNCTION fGetArtiosImageDir RETURNS CHARACTER
    ( ipcCompany AS CHARACTER ) :
    /*------------------------------------------------------------------------------
      Purpose:  Get the file path for images from NK1
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cPath  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound AS LOGICAL   NO-UNDO.

    RUN sys/ref/nk1look.p (INPUT ipcCompany,
        INPUT "CADFILE",
        INPUT "C",
        INPUT NO,
        INPUT NO,
        INPUT "",
        INPUT "",
        OUTPUT cPath,
        OUTPUT lFound).
    IF RIGHT-TRIM(cPath,"\") EQ cPath THEN cPath = cPath + "\".
    IF NOT lFound OR cPath EQ '' THEN
        cPath = 'C:\tmp\'.
    
    RETURN cPath.   /* Function return value. */

END FUNCTION.

FUNCTION fGetXref RETURNS CHARACTER 
    (ipcCompany AS CHARACTER, ipcType AS CHARACTER, ipcLookup AS CHARACTER ) :
    /*------------------------------------------------------------------------------
      Purpose: Lookup a xref value  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.

    RUN sys/ref/getXref.p (INPUT ipcCompany,
        INPUT ipcType, 
        INPUT ipcLookup, 
        OUTPUT cReturn).

    RETURN cReturn.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */

FUNCTION fGetXrefField RETURNS CHARACTER
    (ipcCompany AS CHARACTER, ipcLookup AS CHARACTER ) :
    /*------------------------------------------------------------------------------
      Purpose:  Runs a lookup for an Artios DB field
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
    
    cReturn = fGetXRef(INPUT ipcCompany,
        INPUT "Artios",
        INPUT ipcLookup).
    RETURN cReturn.

END FUNCTION.