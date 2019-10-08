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
    File        : OpenAccessSdkGeneratorPlugin
    Purpose     : Business Entity Designer Plugin to create OpenAccess
                  SDK proxy procedure and Catalog file
    Syntax      :
    Description : Adds a "DataDirect OpenAccess" button to the Ribbon of
                  the Business Entity Designer Ribbon. This button allows
                  to generate proxy procedure and catalog descriptions that
                  allow the use of Business Entities as ODBC data sources
                  using DataDirect OpenAccess for OpenEdge.
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Sep 21 20:38:49 CEST 2012
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.BusinessEntityDesigner.*                              FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Plugins.*                      FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Plugins.OpenAccessSdk.*        FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Services.*                     FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.UI.*                           FROM PROPATH .
USING Infragistics.Win.UltraWinToolbars.*                                  FROM ASSEMBLY .
USING Progress.Lang.*                                                      FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Plugins.OpenAccessSdkGeneratorPlugin
    IMPLEMENTS IBusinessEntityDesignerPlugin:

    { Consultingwerk/BusinessEntityDesigner/Services/dsBusinessEntity.i &REFERENCE-ONLY="REFERENCE-ONLY" }

    DEFINE VARIABLE oController AS BusinessEntityDatasetController NO-UNDO .
    DEFINE VARIABLE oForm AS BusinessEntityDesignerForm            NO-UNDO .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the OpenAccessSdkGeneratorPlugin class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC OpenAccessSdkGeneratorPlugin ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Shows the Wizzard to create an OpenAccess SDK Proxy and catalog
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID GenerateOpenAccessSdk ():

        DEFINE VARIABLE oWizard AS OpenAccessSdkWizard NO-UNDO .
        DEFINE VARIABLE hDataset AS HANDLE NO-UNDO.

        DO ON ERROR UNDO, THROW:
            oWizard = NEW OpenAccessSdkWizard () .

            CATCH err AS Progress.Lang.SysError:
                IF err:NumMessages = 0 THEN DO:
                    oWizard = NEW OpenAccessSdkWizard () .
                END.
                ELSE
                    UNDO, THROW err .

            END CATCH.
        END.

        ASSIGN hDataset = DATASET dsBusinessEntity:HANDLE .

        oWizard:GetDatasetHandle (DATASET-HANDLE hDataset BIND) .

        oWizard:BusinessEntityName = oController:BusinessEntityViewerControl:SmartDataSource:GetFieldValues ("BusinessEntityName":U) .
        oWizard:BusinessEntityPackage = oController:BusinessEntityViewerControl:SmartDataSource:GetFieldValues ("BusinessEntityPackage":U) .
        oWizard:Owner = oForm .

        WAIT-FOR oWizard:ShowDialog () .

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

        oTool = NEW ButtonTool ("OpenAccessSdkGenerator_Plugin_Generate":U) .
        oTool:SharedProps:Caption = "DataDirect OpenAccess"{&TRAN} .
        oTool:SharedProps:DisplayStyle = ToolDisplayStyle:ImageAndText .
        poToolbar:Tools:Add (oTool) .

        FILE-INFORMATION:FILE-NAME = "Consultingwerk/BusinessEntityDesigner/UI/Images/chart_pie_16.png":U .
        IF FILE-INFORMATION:FULL-PATHNAME > "":U THEN
            ASSIGN oTool:SharedProps:AppearancesSmall:Appearance:Image = System.Drawing.Image:FromFile
                        (FILE-INFORMATION:FULL-PATHNAME)  .

        FILE-INFORMATION:FILE-NAME = "Consultingwerk/BusinessEntityDesigner/UI/Images/chart_pie_32.png":U .
        IF FILE-INFORMATION:FULL-PATHNAME > "":U THEN
            ASSIGN oTool:SharedProps:AppearancesLarge:Appearance:Image = System.Drawing.Image:FromFile
                        (FILE-INFORMATION:FULL-PATHNAME)  .

        IF poToolbar:Ribbon:Tabs:Exists ("Generate":U) THEN
            oTab = poToolbar:Ribbon:Tabs["Generate":U] .
        ELSE DO:
            oTab = poToolbar:Ribbon:Tabs:Add ("Generate":U) .
            oTab:Caption = "Generate":U .
        END.

        IF oTab:Groups:Exists ("plugin_DataIntegrationGenerator":U) THEN
            oGroup = oTab:Groups ["plugin_DataIntegrationGenerator":U] .
        ELSE DO:
            oGroup = oTab:Groups:Add ("plugin_DataIntegrationGenerator":U) .
            oGroup:Caption = "Data Integration":U .
        END .

        oGroup:Tools:AddTool ("OpenAccessSdkGenerator_Plugin_Generate":U):InstanceProps:PreferredSizeOnRibbon = RibbonToolSize:Large .

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
            WHEN "OpenAccessSdkGenerator_Plugin_Generate":U THEN
                GenerateOpenAccessSdk () .
        END CASE .

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

END CLASS.
