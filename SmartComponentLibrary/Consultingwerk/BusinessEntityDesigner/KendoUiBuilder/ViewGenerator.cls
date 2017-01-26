/**********************************************************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
 * www.consultingwerk.de and other contributors as listed             *
 * below.  All Rights Reserved.                                       *
 *                                                                    *
 *  Software is distributed on an "AS IS", WITHOUT WARRANTY OF ANY    *
 *   KIND, either express or implied.                                 *
 *                                                                    *
 *  Contributors:                                                     *
 *                                                                    *
 **********************************************************************/
/*------------------------------------------------------------------------
    File        : ViewGenerator
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Oct 02 19:15:33 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

BLOCK-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                                               FROM PROPATH .
USING Consultingwerk.Framework.Collections.*                         FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Generator.Viewer.*       FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.KendoUiBuilder.*         FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.KendoUiBuilder.Builder.* FROM PROPATH .
USING Consultingwerk.Web.KendoUiBuilder.Module.*                     FROM PROPATH .
USING Consultingwerk.Util.*                                          FROM PROPATH .
USING Progress.Json.ObjectModel.*                                    FROM PROPATH .
USING Progress.Lang.*                                                FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.BusinessEntityDesigner.KendoUiBuilder.ViewGenerator:

    {Consultingwerk/Util/TempTables/ttFileNames.i}
    {Consultingwerk/BusinessEntityDesigner/Services/dsBusinessEntity.i}

    /**
     * Purpose: Gets and sets the AppLocation Folder
     * Notes:
     */
    DEFINE PUBLIC PROPERTY AppLocation AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Gets and sets the Name of the Module
     * Notes:
     */
    DEFINE PUBLIC PROPERTY ModuleName AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Constructor for the ViewGenerator class
     * Notes:
     * @param pcModuleName The name of the Module to generate
     * @param pcAppLocation The folder of the Kendo UI Builder builder project
     */
    CONSTRUCTOR PUBLIC ViewGenerator (pcModuleName           AS CHARACTER,
                                      pcAppLocation          AS CHARACTER):

        THIS-OBJECT () .

        ASSIGN THIS-OBJECT:ModuleName  = pcModuleName
               THIS-OBJECT:AppLocation = pcAppLocation .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the ViewGenerator class
     * Notes:
     */
    CONSTRUCTOR PUBLIC ViewGenerator():
    END CONSTRUCTOR.

    /**
     * Purpose: Adds the Serialized Type Information to the
     *          nodes of the Json Object
     * Notes:
     * @param poJsonObject The JSON object to process
     */
    METHOD PROTECTED VOID AddSerializedTypeInformation (poJsonObject AS JsonObject):

        DEFINE VARIABLE i          AS INTEGER   NO-UNDO .
        DEFINE VARIABLE j          AS INTEGER   NO-UNDO .
        DEFINE VARIABLE n          AS INTEGER   NO-UNDO .
        DEFINE VARIABLE oChildren1 AS JsonArray NO-UNDO .
        DEFINE VARIABLE oChildren2 AS JsonArray NO-UNDO .
        DEFINE VARIABLE oFields    AS JsonArray NO-UNDO .

        IF NOT poJsonObject:Has("children":U) THEN
            RETURN .

        ASSIGN oChildren1 = poJsonObject:GetJsonArray("children":U) .

        childrenLoop:
        DO i = 1 TO oChildren1:Length:

            oChildren1:GetJsonObject(i):Add ("SerializedType":U, "Consultingwerk.Web.KendoUiBuilder.Module.View":U) .

            IF NOT oChildren1:GetJsonObject(i):Has ("children":U) THEN
                NEXT childrenLoop .

            oChildren2 = oChildren1:GetJsonObject(i):GetJsonArray("children":U) .

            DO j = 1 TO oChildren2:Length:

                CASE oChildren2:GetJsonObject(j):GetCharacter("type":U):
                    WHEN "data-grid":U THEN
                        oChildren2:GetJsonObject(j):Add ("SerializedType":U, "Consultingwerk.Web.KendoUiBuilder.Module.DataGrid":U) .
                    WHEN "data-grid-form":U THEN
                        oChildren2:GetJsonObject(j):Add ("SerializedType":U, "Consultingwerk.Web.KendoUiBuilder.Module.DataGridForm":U) .
                    WHEN "data-grid-separate-form":U THEN
                        oChildren2:GetJsonObject(j):Add ("SerializedType":U, "Consultingwerk.Web.KendoUiBuilder.Module.DataGridSeparateForm":U) .
                END CASE .

                IF oChildren2:GetJsonObject(j):Has("fields":U) THEN DO:
                    ASSIGN oFields = oChildren2:GetJsonObject(j):GetJsonArray("fields":U) .

                    IF VALID-OBJECT (oFields) THEN
                        DO n = 1 TO oFields:Length:
                            oFields:GetJsonObject(n):Add ("SerializedType":U, "Consultingwerk.Web.KendoUiBuilder.Module.Field":U) .
                        END.
                END.

                IF oChildren2:GetJsonObject(j):Has("columns":U) THEN DO:
                    ASSIGN oFields = oChildren2:GetJsonObject(j):GetJsonArray("columns":U) .

                    IF VALID-OBJECT (oFields) THEN
                        DO n = 1 TO oFields:Length:
                            oFields:GetJsonObject(n):Add ("SerializedType":U, "Consultingwerk.Web.KendoUiBuilder.Module.Column":U) .
                        END.
                END.
            END.
        END.

    END METHOD.

    /**
     * Purpose: Returns the file name the generator is writing to
     * Notes:
     * @param pcModuleName The name of the Module to generate
     * @param pcAppLocation The folder of the Kendo UI Builder builder project
     * @return The file name
     */
    METHOD PUBLIC CHARACTER BuildFileName (pcModuleName           AS CHARACTER,
                                           pcAppLocation          AS CHARACTER):

        IF CharacterType:IsNullOrEmpty(pcModuleName) THEN
            ASSIGN pcModuleName = THIS-OBJECT:ModuleName .

        IF CharacterType:IsNullOrEmpty(pcAppLocation) THEN
            ASSIGN pcAppLocation = THIS-OBJECT:AppLocation .

        RETURN SUBSTITUTE ("&1~\meta~\modules~\&2.json":U,
                           pcAppLocation,
                           pcModuleName) .
    END METHOD.

    /**
     * Purpose: Creates a view with the provided specifications to the module
     * Notes:   Launches the specific view generator instance
     * @param poModule The module to add the new view to
     * @param dsBusinessEntity The dataset with the Business Entity Model
     * @param pcTables The entity table and view tables
     * @param pcViewType The type of the view to create
     * @param pcViewName The name of the view to create
     * @param pcDataProvider The name of the data provider
     * @param pcDataSource The name of the data source
     * @param poGridColumns The CharacterList with the column names to use for the grid
     * @param poViewerFields The list with the viewer field specs
     */
    METHOD PUBLIC VOID CreateView (poModule       AS Module,
                                   DATASET dsBusinessEntity,
                                   pcTables       AS CHARACTER,
                                   pcViewType     AS CHARACTER,
                                   pcViewName     AS CHARACTER,
                                   pcDataProvider AS CHARACTER,
                                   pcDataSource   AS CHARACTER,
                                   poGridColumns  AS CharacterList,
                                   poViewerFields AS ListViewerFieldSpec):

        DEFINE VARIABLE oViewBuilder      AS IViewBuilder NO-UNDO .
        DEFINE VARIABLE cAssetFolder      AS CHARACTER    NO-UNDO .
        DEFINE VARIABLE cHtmlFolder       AS CHARACTER    NO-UNDO .
        DEFINE VARIABLE cJavaScriptFolder AS CHARACTER    NO-UNDO .
        DEFINE VARIABLE lcCode            AS LONGCHAR     NO-UNDO .

        FIX-CODEPAGE (lcCode) = Codepages:UTF-8 .

        CASE pcViewType:

            WHEN "data-grid-form":U THEN
                oViewBuilder = NEW DataGridFormViewBuilder () .

            OTHERWISE UNDO, THROW NEW
                UnsupportedViewTypeException (SUBSTITUTE ("Unsupported View Type: &1"{&TRAN}, pcViewType), 0, pcViewType) .

        END CASE .

        oViewBuilder:CreateView(poModule,
                                DATASET dsBusinessEntity BY-REFERENCE,
                                pcTables,
                                pcViewType,
                                pcViewName,
                                pcDataProvider,
                                pcDataSource,
                                poGridColumns,
                                poViewerFields) .

        ASSIGN cAssetFolder      = SUBSTITUTE ("&1-&2":U, poModule:name, pcViewName)
               cHtmlFolder       = THIS-OBJECT:AppLocation + "~\src~\html~\":U + cAssetFolder
               cJavaScriptFolder = THIS-OBJECT:AppLocation + "~\src~\scripts~\":U + cAssetFolder .

        FileHelper:CreateDirectory (cHtmlFolder) .
        FileHelper:CreateDirectory (cJavaScriptFolder) .

        FILE-INFORMATION:FILE-NAME = "Consultingwerk/BusinessEntityDesigner/KendoUiBuilder/Templates/html":U .

        FileHelper:CopyFiles (FILE-INFORMATION:FULL-PATHNAME, "*.*":U, cHtmlFolder) .

        FILE-INFORMATION:FILE-NAME = "Consultingwerk/BusinessEntityDesigner/KendoUiBuilder/Templates/scripts/view-factory.js":U .

        COPY-LOB FROM FILE FILE-INFORMATION:FULL-PATHNAME TO lcCode .

        ASSIGN lcCode = SUBSTITUTE (lcCode,
                                    StringHelper:LowercaseFirstLetter(poModule:Name),
                                    StringHelper:UppercaseFirstLetter(poModule:Name),
                                    StringHelper:UppercaseFirstLetter(pcViewName)) .

        COPY-LOB FROM lcCode TO FILE SUBSTITUTE ("&1~\view-factory.js":U, cJavaScriptFolder) .

    END METHOD.

    /**
     * Purpose: Returns an array of the view names
     * Notes:
     * @param pcAppLocation The folder of the Kendo UI Builder builder project
     * @return The ABL Character Array of Module Names
     */
    METHOD PUBLIC CHARACTER EXTENT GetModuleNames (pcAppLocation AS CHARACTER):

        DEFINE VARIABLE cReturn     AS CHARACTER         NO-UNDO EXTENT .
        DEFINE VARIABLE i           AS INTEGER           NO-UNDO .
        DEFINE VARIABLE iCount      AS INTEGER           NO-UNDO .
        DEFINE VARIABLE oJsonObject AS JsonObject        NO-UNDO .
        DEFINE VARIABLE oParser     AS ObjectModelParser NO-UNDO .

        FileHelper:GetFileList (pcAppLocation + "~\meta~\modules":U, "*.json":U, OUTPUT TABLE ttFileNames) .

        ASSIGN iCount  = BufferHelper:NumRecords (BUFFER ttFileNames:HANDLE)
               oParser = NEW ObjectModelParser().

        EXTENT (cReturn) = iCount .

        FOR EACH ttFileNames:
            ASSIGN i           = i + 1
                   oJsonObject = CAST (oParser:ParseFile(ttFileNames.FileName), JsonObject)
                   cReturn[i]  = oJsonObject:GetCharacter("name":U) .
        END.

        RETURN cReturn .

    END METHOD.

    /**
     * Purpose: Returns if the provided Module contains a view
     *          with the given Name
     * Notes:
     * @param poModule The Module
     * @param pcViewName The name of the View
     * @return Logical value determing if the module contains the view
     */
    METHOD PUBLIC LOGICAL HasView (poModule AS Module,
                                   pcViewName AS CHARACTER):

        {Consultingwerk/linqABL.i View oView in poModule:children where name EQ pcViewName}
            RETURN TRUE .
        END.

        RETURN FALSE.

    END METHOD.

    /**
     * Purpose: Loads the given module
     * Notes:
     * @param pcModuleName The name of the Module to Load
     * @param pcAppLocation The folder of the Kendo UI Builder builder project
     * @return The Module definition
     */
    METHOD PUBLIC Module LoadModule (pcModuleName AS CHARACTER,
                                     pcAppLocation AS CHARACTER):

        DEFINE VARIABLE cFileName   AS CHARACTER  NO-UNDO .
        DEFINE VARIABLE oJsonObject AS JsonObject NO-UNDO .
        DEFINE VARIABLE oModule     AS Module     NO-UNDO .

        IF CharacterType:IsNullOrEmpty(pcModuleName) THEN
            ASSIGN pcModuleName = THIS-OBJECT:ModuleName .

        IF CharacterType:IsNullOrEmpty(pcAppLocation) THEN
            ASSIGN pcAppLocation = THIS-OBJECT:AppLocation .

        ASSIGN cFileName   = THIS-OBJECT:BuildFileName (pcModuleName, pcAppLocation)

               oJsonObject = CAST (NEW ObjectModelParser():ParseFile (FileHelper:FindFile (cFileName)),
                                   JsonObject)
               oModule     = NEW Module () .

        THIS-OBJECT:AddSerializedTypeInformation (oJsonObject) .

        oModule:DeserializeFromJsonObject(oJsonObject) .

        RETURN oModule .

    END METHOD.

    /**
     * Purpose:
     * Notes:
     * @param pcModuleName The name of the Module to Load
     * @param pcAppLocation The folder of the Kendo UI Builder builder project
     * @return
     */
    METHOD PUBLIC LOGICAL ModuleExists (pcModuleName AS CHARACTER,
                                        pcAppLocation AS CHARACTER):

        IF CharacterType:IsNullOrEmpty(pcModuleName) THEN
            ASSIGN pcModuleName = THIS-OBJECT:ModuleName .

        IF CharacterType:IsNullOrEmpty(pcAppLocation) THEN
            ASSIGN pcAppLocation = THIS-OBJECT:AppLocation .

        RETURN FileHelper:Exists (BuildFileName(pcModuleName, pcAppLocation)) .

    END METHOD.

    /**
     * Purpose: Removes the view with the given name from the module
     * Notes:   Does not throw any error when the View does not exist
     * @param poModule The Module
     * @param pcViewName The name of the View
     */
    METHOD PUBLIC VOID RemoveView (poModule AS Module,
                                   pcViewName AS CHARACTER):

        {Consultingwerk/linqABL.i View oView in poModule:children where name EQ pcViewName}

            poModule:children:Remove (oView) .
            RETURN .
        END.

    END METHOD.

    /**
     * Purpose: Saves the module definition
     * Notes:
     * @param poModule The Module definition
     * @param pcModuleName The name of the Module to Load
     * @param pcAppLocation The folder of the Kendo UI Builder builder project
     */
    METHOD PUBLIC VOID SaveModule (poModule AS Module,
                                   pcModuleName AS CHARACTER,
                                   pcAppLocation AS CHARACTER):

        DEFINE VARIABLE cFileName   AS CHARACTER  NO-UNDO .
        DEFINE VARIABLE oJsonObject AS JsonObject NO-UNDO .

        IF CharacterType:IsNullOrEmpty(pcModuleName) THEN
            ASSIGN pcModuleName = THIS-OBJECT:ModuleName .

        IF CharacterType:IsNullOrEmpty(pcAppLocation) THEN
            ASSIGN pcAppLocation = THIS-OBJECT:AppLocation .

        ASSIGN cFileName   = THIS-OBJECT:BuildFileName (pcModuleName, pcAppLocation)

               oJsonObject = poModule:ToJsonObject().

        oJsonObject:WriteFile (cFileName, TRUE) .

        RETURN .

    END METHOD.

END CLASS.
