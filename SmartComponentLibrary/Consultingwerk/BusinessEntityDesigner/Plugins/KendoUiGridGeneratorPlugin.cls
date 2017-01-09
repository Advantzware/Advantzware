/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : KendoUiGridGeneratorPlugin
    Purpose     : Business Entity Designer Plugin to generate html pages
                  including Kendo UI Grid
    Syntax      :
    Description : Adds a "Generate Kendo UI Grid" button to the Ribbon of
                  the Business Entity Designer. Allows wizard and template
                  based generation of HTML pages containing a Kendo UI
                  Grid and JSDO
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Sep 21 20:38:49 CEST 2012
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.BusinessEntityDesigner.*                 FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Plugins.*         FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Plugins.KendoUI.* FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Services.*        FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.UI.*              FROM PROPATH .
USING Infragistics.Win.UltraWinToolbars.*                     FROM ASSEMBLY .
USING Progress.Lang.*                                         FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Plugins.KendoUiGridGeneratorPlugin
    IMPLEMENTS IBusinessEntityDesignerPlugin:

    { Consultingwerk/BusinessEntityDesigner/Services/dsBusinessEntity.i &REFERENCE-ONLY="REFERENCE-ONLY" }

    DEFINE VARIABLE oController AS BusinessEntityDatasetController NO-UNDO .
    DEFINE VARIABLE oForm AS BusinessEntityDesignerForm            NO-UNDO .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the KendoUiGridGeneratorPlugin class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC KendoUiGridGeneratorPlugin ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Shows the Wizzard to create a SmartWindowForm
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID GenerateWindow ():

        DEFINE VARIABLE oWizard AS KendoUiGridWizard NO-UNDO .

        DO ON ERROR UNDO, THROW:
            oWizard = NEW KendoUiGridWizard () .

            CATCH err AS Progress.Lang.SysError:
                IF err:NumMessages = 0 THEN DO:
                    oWizard = NEW KendoUiGridWizard () .
                END.
                ELSE
                    UNDO, THROW err .

            END CATCH.
        END.

        oWizard:BusinessEntityName       = oController:BusinessEntityViewerControl:SmartDataSource:GetFieldValues ("BusinessEntityName":U) .
        oWizard:BusinessEntityPackage    = oController:BusinessEntityViewerControl:SmartDataSource:GetFieldValues ("BusinessEntityPackage":U) .

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

        DEFINE VARIABLE oGenerarteWindow AS ButtonTool  NO-UNDO .
        DEFINE VARIABLE oGroup           AS RibbonGroup NO-UNDO .
        DEFINE VARIABLE oTab             AS RibbonTab   NO-UNDO .

        oController = poController .
        oForm = poForm .

        oGenerarteWindow = NEW ButtonTool ("KendoUiGridGenerator_Plugin_GenerateKendoUiGrid":U) .
        oGenerarteWindow:SharedProps:Caption = "Generate Kendo UI Grid"{&TRAN} .
        oGenerarteWindow:SharedProps:DisplayStyle = ToolDisplayStyle:ImageAndText .
        poToolbar:Tools:Add (oGenerarteWindow) .

        FILE-INFORMATION:FILE-NAME = "Consultingwerk/BusinessEntityDesigner/UI/Images/window_earth_16.png":U .
        IF FILE-INFORMATION:FULL-PATHNAME > "":U THEN
            ASSIGN oGenerarteWindow:SharedProps:AppearancesSmall:Appearance:Image = System.Drawing.Image:FromFile
                        (FILE-INFORMATION:FULL-PATHNAME)  .

        FILE-INFORMATION:FILE-NAME = "Consultingwerk/BusinessEntityDesigner/UI/Images/window_earth_32.png":U .
        IF FILE-INFORMATION:FULL-PATHNAME > "":U THEN
            ASSIGN oGenerarteWindow:SharedProps:AppearancesLarge:Appearance:Image = System.Drawing.Image:FromFile
                        (FILE-INFORMATION:FULL-PATHNAME)  .

        IF poToolbar:Ribbon:Tabs:Exists ("Generate":U) THEN
            oTab = poToolbar:Ribbon:Tabs["Generate":U] .
        ELSE DO:
            oTab = poToolbar:Ribbon:Tabs:Add ("Generate":U) .
            oTab:Caption = "Generate":U .
        END.

        IF oTab:Groups:Exists ("plugin_KendoUiGenerator":U) THEN
            oGroup = oTab:Groups ["plugin_KendoUiGenerator":U] .
        ELSE DO:
            oGroup = oTab:Groups:Add ("plugin_KendoUiGenerator":U) .
            oGroup:Caption = "Kendo UI":U .
        END .

        oGroup:Tools:AddTool ("KendoUiGridGenerator_Plugin_GenerateKendoUiGrid":U):InstanceProps:PreferredSizeOnRibbon = RibbonToolSize:Large .

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
            WHEN "KendoUiGridGenerator_Plugin_GenerateKendoUiGrid":U THEN
                GenerateWindow () .
        END CASE .

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

END CLASS.
