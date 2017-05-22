/**********************************************************************
 * Copyright (C) 2006-2013 by Consultingwerk Ltd. ("CW") -            *
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
    File        : CsharpGeneratorPlugin
    Purpose     : Business Entity Designer Plugin to create C# classes
                  from the dataset design using the xsd.exe utility
    Syntax      :
    Description : Creates a "C# Classes" button in the Business Entity
                  Designer. Allows to generate C# typed dataset or
                  value classes based on the Business Entity ProDataset
                  structure.
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Dec 21 14:50:56 CET 2014
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.BusinessEntityDesigner.*                              FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Plugins.*                      FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Plugins.CsharpGenerator.*      FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Services.*                     FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.UI.*                           FROM PROPATH .
USING Consultingwerk.Exceptions.*                                          FROM PROPATH.
USING Consultingwerk.Util.*                                                FROM PROPATH .
USING Consultingwerk.Windows.Util.Forms.*                                  FROM PROPATH .
USING Infragistics.Win.UltraWinToolbars.*                                  FROM ASSEMBLY .
USING Progress.Lang.*                                                      FROM PROPATH .
USING System.Windows.Forms.*                                               FROM ASSEMBLY .

CLASS Consultingwerk.BusinessEntityDesigner.Plugins.CsharpGeneratorPlugin
    IMPLEMENTS IBusinessEntityDesignerPlugin:

    { Consultingwerk/BusinessEntityDesigner/Services/dsBusinessEntity.i &REFERENCE-ONLY="REFERENCE-ONLY" }

    DEFINE VARIABLE oController AS BusinessEntityDatasetController NO-UNDO .
    DEFINE VARIABLE oForm AS BusinessEntityDesignerForm            NO-UNDO .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the CsharpGeneratorPlugin class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC CsharpGeneratorPlugin ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Shows the Wizzard to create an C# Classes
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID GenerateCsharp ():

        DEFINE VARIABLE oWizard       AS CsharpGeneratorWizard NO-UNDO .
        DEFINE VARIABLE hDataset      AS HANDLE                NO-UNDO .
        DEFINE VARIABLE cXsdFileName  AS CHARACTER             NO-UNDO .
        DEFINE VARIABLE oDialogResult AS DialogResult          NO-UNDO .

        DEFINE VARIABLE cXsdPath             AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cOutputFolder        AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cGenerate            AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cNamespace           AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cAdditionalParameter AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cCommandLine         AS CHARACTER NO-UNDO.

        DEFINE VARIABLE oExecuteDialog AS ExecuteShellCommandDialog NO-UNDO .

        ASSIGN cXsdFileName = SUBSTITUTE ("&1/&2.xsd":U,
                                          oController:BusinessEntityViewerControl:SmartDataSource:GetFieldValues ("DatasetPath":U),
                                          oController:BusinessEntityViewerControl:SmartDataSource:GetFieldValues ("DatasetName":U)) .

        IF NOT FileHelper:Exists (cXsdFileName) THEN
            UNDO, THROW NEW ExceptionWithTitle ("Business Entity not found. Please generate source code first."{&TRAN},
                                                0,
                                                "C# Generator Plugin"{&TRAN}) .
        FILE-INFORMATION:FILE-NAME = cXsdFileName .
        cXsdFileName = FILE-INFORMATION:FULL-PATHNAME .

        DO ON ERROR UNDO, THROW:
            oWizard = NEW CsharpGeneratorWizard () .

            CATCH err AS Progress.Lang.SysError:
                IF err:NumMessages = 0 THEN DO:
                    oWizard = NEW CsharpGeneratorWizard () .
                END.
                ELSE
                    UNDO, THROW err .

            END CATCH.
        END.

        ASSIGN hDataset = DATASET dsBusinessEntity:HANDLE .

        oWizard:BusinessEntityDatasetController = oController .
        oWizard:GetDatasetHandle (DATASET-HANDLE hDataset BIND) .

        oWizard:Owner = oForm .

        WAIT-FOR oWizard:ShowDialog () SET oDialogResult .

        {Consultingwerk/check-dialogresult-ok.i oDialogResult} .

        ASSIGN cXsdPath             = oWizard:GetXsdExe()
               cOutputFolder        = oWizard:GetOutputFolder()
               cGenerate            = oWizard:GetGenerate()
               cNamespace           = oWizard:GetNamespace()
               cAdditionalParameter = oWizard:GetAdditionalParameter()

               cCommandLine         = SUBSTITUTE ('"&1" "&2" /&3 /n:&4 /o:"&5" &6':U,
                                                  cXsdPath,
                                                  cXsdFileName,
                                                  cGenerate,
                                                  cNamespace,
                                                  cOutputFolder,
                                                  cAdditionalParameter) .

        oExecuteDialog = NEW ExecuteShellCommandDialog () .
        oExecuteDialog:Execute (cCommandLine) .

     END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Shuts down the plugin
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Shutdown ():

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Initializes the plugin
        Notes:
        @param dsBusinessEntity Business Entity with the Design dataset, called with the BIND keyword
        @param poController The BusinessEntityDatasetController
        @param poForm The main Form of the BusinessEntityDesigner
        @param poToolbar The reference to the UltraToolbarsManager used by the BusinessEntityDesigner
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Startup (DATASET FOR dsBusinessEntity BIND,
                                poController AS BusinessEntityDatasetController,
                                poForm AS BusinessEntityDesignerForm,
                                poToolbar AS UltraToolbarsManager):

        DEFINE VARIABLE oTool  AS ButtonTool  NO-UNDO .
        DEFINE VARIABLE oGroup AS RibbonGroup NO-UNDO .
        DEFINE VARIABLE oTab   AS RibbonTab   NO-UNDO .

        oController = poController .
        oForm = poForm .

        oTool = NEW ButtonTool ("CsharpGenerator_Plugin_Generate":U) .
        oTool:SharedProps:Caption = "C# Classes"{&TRAN} .
        oTool:SharedProps:DisplayStyle = ToolDisplayStyle:ImageAndText .
        poToolbar:Tools:Add (oTool) .

        FILE-INFORMATION:FILE-NAME = "Consultingwerk/BusinessEntityDesigner/UI/Images/code_csharp_16.png":U .
        IF FILE-INFORMATION:FULL-PATHNAME > "":U THEN
            ASSIGN oTool:SharedProps:AppearancesSmall:Appearance:Image = System.Drawing.Image:FromFile
                        (FILE-INFORMATION:FULL-PATHNAME)  .

        FILE-INFORMATION:FILE-NAME = "Consultingwerk/BusinessEntityDesigner/UI/Images/code_csharp_32.png":U .
        IF FILE-INFORMATION:FULL-PATHNAME > "":U THEN
            ASSIGN oTool:SharedProps:AppearancesLarge:Appearance:Image = System.Drawing.Image:FromFile
                        (FILE-INFORMATION:FULL-PATHNAME)  .

        IF poToolbar:Ribbon:Tabs:Exists ("Generate":U) THEN
            oTab = poToolbar:Ribbon:Tabs["Generate":U] .
        ELSE DO:
            oTab = poToolbar:Ribbon:Tabs:Add ("Generate":U) .
            oTab:Caption = "Generate":U .
        END.

        IF oTab:Groups:Exists ("plugin_3rdPartyCodeGenerator":U) THEN
            oGroup = oTab:Groups ["plugin_3rdPartyCodeGenerator":U] .
        ELSE DO:
            oGroup = oTab:Groups:Add ("plugin_3rdPartyCodeGenerator":U) .
            oGroup:Caption = "3rd Party Code":U .
        END .

        oGroup:Tools:AddTool ("CsharpGenerator_Plugin_Generate":U):InstanceProps:PreferredSizeOnRibbon = RibbonToolSize:Large .

        poToolbar:ToolClick:Subscribe (ToolClickHandler) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Handles the ToolClick event of the Business Entity Designer's
                 UltraToolbarsManager
        Notes:
        @param sender The reference to the object that raised the event
        @param e The ToolClickEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ToolClickHandler (sender AS System.Object,
                                            e AS ToolClickEventArgs):

        CASE e:Tool:Key:
            WHEN "CsharpGenerator_Plugin_Generate":U THEN
                GenerateCsharp () .
        END CASE .

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

END CLASS.
