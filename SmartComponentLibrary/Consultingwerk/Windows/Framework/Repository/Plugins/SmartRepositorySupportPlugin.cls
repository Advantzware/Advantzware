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
    File        : SmartRepositorySupportPlugin
    Purpose     : Business Entity Designer Plugin that with various wizards
                  supporting the SmartFramework repository
    Syntax      :
    Description : Provides functionality to
                  - Import fields as data fields
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Dec 10 08:27:49 CEST 2017
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                                      FROM PROPATH .
USING Consultingwerk.Assertion.*                            FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.*               FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Plugins.*       FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Services.*      FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.UI.*            FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.UI.Shapes.*     FROM PROPATH .
USING Consultingwerk.Framework.*                            FROM PROPATH .
USING Consultingwerk.Framework.Enum.*                       FROM PROPATH .
USING Consultingwerk.Framework.Collections.*                FROM PROPATH .
USING Consultingwerk.SmartFramework.*                       FROM PROPATH .
USING Consultingwerk.OERA.*                                 FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.*            FROM PROPATH .
USING Consultingwerk.SmartFramework.System.*                FROM PROPATH .
USING Consultingwerk.Windows.Framework.Repository.Plugins.* FROM PROPATH .
USING Consultingwerk.Util.*                                 FROM PROPATH .
USING Consultingwerk.Util.Forms.*                           FROM PROPATH .
USING Infragistics.Win.UltraWinToolbars.*                   FROM ASSEMBLY  .
USING Progress.Lang.*                                       FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.Repository.Plugins.SmartRepositorySupportPlugin
    IMPLEMENTS IBusinessEntityDesignerPlugin, IOverwritePromptCallback:

    DEFINE VARIABLE lOverwrite AS LOGICAL NO-UNDO INIT ?.

    { Consultingwerk/BusinessEntityDesigner/Services/dsBusinessEntity.i &REFERENCE-ONLY="REFERENCE-ONLY" }
    { Consultingwerk/SmartFramework/Menu/dsModule.i }

    /**
     * Purpose: Returns the reference to the BusinessEntityDatasetController
     * Notes:
     */
    DEFINE PUBLIC PROPERTY Controller AS BusinessEntityDatasetController NO-UNDO
    GET.
    PRIVATE SET.

    /**
     * Purpose: Returns the reference to the Parent Form
     * Notes:
     */
    DEFINE PUBLIC PROPERTY Form AS System.Windows.Forms.Form NO-UNDO
    GET.
    PRIVATE SET.

    /**
     * Purpose: Constructor for the SmartDataObjectImporterPlugin class
     * Notes:
     */
    CONSTRUCTOR PUBLIC SmartRepositorySupportPlugin ():
        SUPER ().

    END CONSTRUCTOR.

    /**
     * Purpose: Wizard to create Data Fields
     * Notes:
     */
    METHOD PROTECTED VOID CreateDataFields():

        DEFINE VARIABLE oList       AS ListNameValuePair       NO-UNDO .
        DEFINE VARIABLE cModule     AS CHARACTER               NO-UNDO .
        DEFINE VARIABLE cEntityName AS CHARACTER               NO-UNDO .
        DEFINE VARIABLE oRepository AS ISmartRepositoryService NO-UNDO .

        FIND FIRST eBusinessEntity .

        ASSIGN cEntityName = SUBSTITUTE ("&1.&2":U,
                                         eBusinessEntity.BusinessEntityPackage,
                                         eBusinessEntity.BusinessEntityName) .

        FrameworkSettings:ServiceAdapter:RetrieveData("":U,
                                                      "Consultingwerk.SmartFramework.Menu.ModuleBusinessEntity":U,
                                                      NEW FetchDataRequest("*":U),
                                                      OUTPUT DATASET dsModule) .

        oList = NEW ListNameValuePair() .

        FOR EACH eSmartModule BY eSmartModule.ModuleName ON ERROR UNDO, THROW:
            oList:Add (eSmartModule.ModuleName, eSmartModule.ModuleName) .
        END.

        IF NOT InputPromptHelper:PromptForCharacterValue ("Select Product Module"{&TRAN},
                                                          "Please select the product module to import to:"{&TRAN},
                                                          oList,
                                                          NEW NotEmptyInputValidator(),
                                                          INPUT-OUTPUT cModule) THEN RETURN .

        ASSIGN lOverwrite = ? .

        oRepository = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.Repository.ISmartRepositoryService
                                                    "NEW SmartRepositoryService ()"} .

        oRepository:CreateDataFields (DATASET dsBusinessEntity:HANDLE, cModule, THIS-OBJECT).

        FINALLY:
            ASSIGN lOverwrite = ? .
        END FINALLY.

    END METHOD.

    /**
     * Purpose: Prompts the user if a resource should be overwritten
     * Notes:
     * @param pcObjectName The name of the resources that
     * @return Logical value indicating if an existing object should be overwritten
     */
    METHOD PUBLIC LOGICAL PromptForOverwrite (pcObjectName AS CHARACTER):

        DEFINE VARIABLE oDialogResult    AS DialogResultEnum NO-UNDO .
        DEFINE VARIABLE oMessageProvider AS IMessageProvider NO-UNDO .

        IF lOverwrite <> ? THEN
            RETURN lOverwrite .

        oMessageProvider = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.IMessageProvider
                                                         "NEW MessageProvider ()"} .

        oDialogResult = MessageFormHelper:ShowMessage (oMessageProvider:GetMessage("SFR":U, 402),
                                                       MessageFormDefaults:LabelYesNo,
                                                       MessageFormDefaults:GetDialogResultYesNo(),
                                                       DialogResultEnum:DialogResultYes) .

        lOverwrite = oDialogResult:ToLogical().

        RETURN lOverwrite .

    END METHOD.

    /**
     * Purpose: Shuts down the plugin
     * Notes:
     */
    METHOD PUBLIC VOID Shutdown ():

    END METHOD .

    /**
     * Purpose: Initializes the plugin
     * Notes:
     * @param dsBusinessEntity Business Entity with the Design dataset, called with the BIND keyword
     * @param poController The BusinessEntityDatasetController
     * @param poForm The main Form of the BusinessEntityDesigner
     * @param poToolbar The reference to the UltraToolbarsManager used by the BusinessEntityDesigner
     */
    METHOD PUBLIC VOID Startup (DATASET FOR dsBusinessEntity BIND,
                                poController AS BusinessEntityDatasetController,
                                poForm AS BusinessEntityDesignerForm,
                                poToolbar AS UltraToolbarsManager):

        DEFINE VARIABLE oButtonTool AS ButtonTool  NO-UNDO .
        DEFINE VARIABLE oGroup      AS RibbonGroup NO-UNDO .
        DEFINE VARIABLE oRibbonTab  AS RibbonTab   NO-UNDO .

        THIS-OBJECT:Form = poForm .
        THIS-OBJECT:Controller = poController .

        oButtonTool = NEW ButtonTool ("SmartRepositorySupport_Plugin_CreateDataFields":U) .
        oButtonTool:SharedProps:Caption = "Create Data Fields"{&TRAN} .
        oButtonTool:SharedProps:ToolTipText = "Create or update Data Fields for this Business Entity"{&TRAN} .
        oButtonTool:SharedProps:DisplayStyle = ToolDisplayStyle:ImageAndText .
        poToolbar:Tools:Add (oButtonTool) .

        FILE-INFORMATION:FILE-NAME = "Consultingwerk/Windows/Framework/Repository/Images/datafield_import_16.png":U .
        IF FILE-INFORMATION:FULL-PATHNAME > "":U THEN
            ASSIGN oButtonTool:SharedProps:AppearancesSmall:Appearance:Image = ImageHelper:Load (FILE-INFORMATION:FULL-PATHNAME)  .

        FILE-INFORMATION:FILE-NAME = "Consultingwerk/Windows/Framework/Repository/Images/datafield_import_32.png":U .
        IF FILE-INFORMATION:FULL-PATHNAME > "":U THEN
            ASSIGN oButtonTool:SharedProps:AppearancesLarge:Appearance:Image = ImageHelper:Load (FILE-INFORMATION:FULL-PATHNAME)  .

        IF poToolbar:Ribbon:Tabs:Exists ("SmartRepository":U) THEN
            oRibbonTab = poToolbar:Ribbon:Tabs ["SmartRepository":U] .
        ELSE DO:
            oRibbonTab = poToolbar:Ribbon:Tabs:Add("SmartRepository":U) .
            oRibbonTab:Caption = "Smart Repository"{&TRAN} .
        END.

        IF oRibbonTab:Groups:Exists ("RepositoryMetaData":U) THEN
            ASSIGN oGroup = oRibbonTab:Groups["RepositoryMetaData":U] .
        ELSE DO:
            oGroup = oRibbonTab:Groups:Add ("RepositoryMetaData":U) .
            oGroup:Caption = "Meta Data":U .
        END.

        oGroup:Tools:AddTool ("SmartRepositorySupport_Plugin_CreateDataFields":U):InstanceProps:PreferredSizeOnRibbon = RibbonToolSize:Large .
        oGroup:Tools["SmartRepositorySupport_Plugin_CreateDataFields":U]:InstanceProps:IsFirstInGroup = TRUE .

        poToolbar:ToolClick:Subscribe (ToolClickHandler) .

    END METHOD .

    /**
     * Purpose: Handles the ToolClick event of the Business Entity Designer's
     *          UltraToolbarsManager
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The ToolClickEventArgs with the data for this event
     */
    METHOD PROTECTED VOID ToolClickHandler (sender AS System.Object,
                                            e AS ToolClickEventArgs):

        CASE e:Tool:Key:

            WHEN "SmartRepositorySupport_Plugin_CreateDataFields":U THEN
                THIS-OBJECT:CreateDataFields () .

        END CASE .

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

END CLASS.
