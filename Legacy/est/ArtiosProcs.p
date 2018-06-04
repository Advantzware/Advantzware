
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
{est\ttArtiosAttributes.i SHARED}

DEFINE VARIABLE gchCadX             AS COMPONENT-HANDLE NO-UNDO.

DEFINE VARIABLE gcAttributeList     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE gcAttributeTypeList AS CHARACTER  NO-UNDO.
    
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


FUNCTION GetArtiosDir RETURNS CHARACTER 
    (ipcCompany AS CHARACTER) FORWARD.

FUNCTION GetArtiosImageDir RETURNS CHARACTER 
    (ipcCompany AS CHARACTER) FORWARD.

FUNCTION GetXref RETURNS CHARACTER
    (ipcCompany AS CHARACTER, ipcType AS CHARACTER, ipcLookup AS CHARACTER ) FORWARD.
  
FUNCTION GetXrefField RETURNS CHARACTER
    (ipcCompany AS CHARACTER, ipcLookup AS CHARACTER ) FORWARD.  
/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

PROCEDURE pBuildAttributes:
    /*------------------------------------------------------------------------------
      Purpose: Builds the Attributes temp table for the cadFile passed in    
      Parameters:  ipcCadFile as character
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCadFile AS CHARACTER   NO-UNDO.

    DEFINE VARIABLE cImageDir      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cImageFile     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iResult        AS INTEGER   NO-UNDO.                                                                          
    DEFINE VARIABLE iCounter       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cArtiosField   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cAttribute     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cAttributeType AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMessage       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCadXValue     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMappedValue   AS CHARACTER NO-UNDO.


    ASSIGN /*Open CadFile and Build The Image File*/
        cImageDir  = GetArtiosImageDir(ipcCompany)
        iResult    = gchCadX:OpenDesign (ipcCadFile,0)
        cImageFile = cImageDir + SUBSTRING(ipcCadFile, R-INDEX(ipcCadFile, "\") + 1) + ".jpg"
        iResult    = gchCadX:SetOverlayClass(3,1) /*print dimensions*/
        iResult    = gchCadX:SaveAsBitmap(1,cImageFile, 600, 600, 20, , , 1, 100)
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
            cArtiosField   = GetXrefField(ipcCompany,cAttribute) 
            /*find the Artios DB field for this attribute*/
            .
        IF cArtiosField NE "" THEN 
        DO:
            /*Get the value of the field from the Artios DB*/
            IF cAttributeType EQ "N" THEN
                cCadXValue = STRING(gchCadX:ReturnNumericCode4Param(cArtiosField)).
            ELSE
                cCadXValue = gchCadX:ReturnTextCode4Param(cArtiosField).
            IF cCadXValue NE "" THEN 
            DO:
                /*See if the value itself has a cross reference value*/
                cMappedValue = GetXref(ipcCompany, cAttribute, cCadXValue).
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
    iResult = gchCadX:CloseDesign().
    

END PROCEDURE.

PROCEDURE pCADXConnect PRIVATE:
    /*------------------------------------------------------------------------------
      Purpose:  Instantiates the CadX Handle   
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
 
    CREATE "ArtiosCAD-X.CadXCtrl.1" gchCadX CONNECT NO-ERROR.
    IF NOT VALID-HANDLE (gchCadX) THEN
        CREATE "ArtiosCAD-X.CadXCtrl.1" gchCadX NO-ERROR.
    IF NOT VALID-HANDLE (gchCadX) THEN 
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
ASSIGN 
    opcData[1] = "C"
    opcData[2] = "1"
   .
FOR EACH ttArtiosAttributes:
    CASE ttArtiosAttributes.Attribute:
        WHEN "Cad Number" THEN 
            opcData[3] = ttArtiosAttributes.ValueMapped.
        WHEN "Customer Number" THEN 
            opcData[3] = ttArtiosAttributes.ValueMapped.
        WHEN "Customer Number" THEN
            opcData[3] = ttArtiosAttributes.ValueMapped.
        WHEN "Customer Name" THEN
            opcData[3] = ttArtiosAttributes.ValueMapped.
        WHEN "Part Number" THEN
            opcData[3] = ttArtiosAttributes.ValueMapped.
        WHEN "Part Name" THEN
            opcData[3] = ttArtiosAttributes.ValueMapped.                    
        WHEN "Style" THEN
            opcData[3] = ttArtiosAttributes.ValueMapped.
        WHEN "Flute" THEN
            opcData[3] = ttArtiosAttributes.ValueMapped.
        WHEN "Test" THEN
            opcData[3] = ttArtiosAttributes.ValueMapped.
        WHEN "Board Code" THEN
            opcData[3] = ttArtiosAttributes.ValueMapped.
        WHEN "Length" THEN
            opcData[3] = ttArtiosAttributes.ValueMapped.
        WHEN "Width" THEN
            opcData[3] = ttArtiosAttributes.ValueMapped.
        WHEN "Depth" THEN
            opcData[3] = ttArtiosAttributes.ValueMapped.
        WHEN "Trim Width" THEN
            opcData[3] = ttArtiosAttributes.ValueMapped.
        WHEN "Trim Length" THEN
            opcData[3] = ttArtiosAttributes.ValueMapped.
        WHEN "Die Inches" THEN
            opcData[3] = ttArtiosAttributes.ValueMapped.
        WHEN "Qty" THEN
            opcData[3] = ttArtiosAttributes.ValueMapped.
        WHEN "Qty Per Set" THEN
            opcData[3] = ttArtiosAttributes.ValueMapped.
        WHEN "Product Category" THEN
            opcData[3] = ttArtiosAttributes.ValueMapped.
        WHEN "Die Number" THEN
            opcData[3] = ttArtiosAttributes.ValueMapped.
        WHEN "Number Up" THEN
            opcData[3] = ttArtiosAttributes.ValueMapped.
        WHEN "Manufactured Or Purchased" THEN
            opcData[3] = ttArtiosAttributes.ValueMapped.
        WHEN "Salesperson ID" THEN
            opcData[3] = ttArtiosAttributes.ValueMapped.
        WHEN "Grain Direction" THEN
            opcData[3] = ttArtiosAttributes.ValueMapped.
        WHEN "Designer Firstname" THEN
            opcData[3] = ttArtiosAttributes.ValueMapped.
        WHEN "Designer Lastname" THEN
            opcData[3] = ttArtiosAttributes.ValueMapped.
    END CASE.
END.
END PROCEDURE.

/* ************************  Function Implementations ***************** */


FUNCTION GetArtiosDir RETURNS CHARACTER 
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

FUNCTION GetArtiosImageDir RETURNS CHARACTER
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
    IF NOT lFound OR cPath EQ '' THEN
        cPath = 'C:\tmp\'.
    
    RETURN cPath.   /* Function return value. */

END FUNCTION.

FUNCTION GetXref RETURNS CHARACTER
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

FUNCTION GetXrefField RETURNS CHARACTER
    (ipcCompany AS CHARACTER, ipcLookup AS CHARACTER ) :
    /*------------------------------------------------------------------------------
      Purpose:  Runs a lookup for an Artios DB field
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
    
    cReturn = GetXRef(INPUT ipcCompany,
        INPUT "Artios",
        INPUT ipcLookup).
    RETURN cReturn.

END FUNCTION.