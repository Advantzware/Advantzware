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
    File        : TelerikMobilePlugin
    Purpose     : Business Entity Designer Plugin to generate fragments
                  of html mobile views used in the Telerik Platform
    Syntax      :
    Description : Adds a "Generate Detail View" button to the Business Entity
                  Designer Ribbon that allows generating a wizard for a
                  detail view that can be copied and pasted into the Telerik
                  AppBuilder (Telerik Platform). Does further add a button
                  that opens the Telerik Platform in the web browser.
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Sep 21 20:38:49 CEST 2012
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.BusinessEntityDesigner.*                       FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Plugins.*               FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Plugins.TelerikMobile.* FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Services.*              FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.UI.*                    FROM PROPATH .
USING Consultingwerk.Windows.API.*                                  FROM PROPATH .
USING Infragistics.Win.UltraWinToolbars.*                           FROM ASSEMBLY .
USING Progress.Lang.*                                               FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Plugins.TelerikMobilePlugin
    IMPLEMENTS IBusinessEntityDesignerPlugin:

    { Consultingwerk/BusinessEntityDesigner/Services/dsBusinessEntity.i &REFERENCE-ONLY="REFERENCE-ONLY" }

    DEFINE VARIABLE oController AS BusinessEntityDatasetController NO-UNDO .
    DEFINE VARIABLE oForm AS BusinessEntityDesignerForm            NO-UNDO .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the TelerikMobilePlugin class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC TelerikMobilePlugin ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Shows the Wizzard to create a Detail View
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID GenerateDetailView ():

        DEFINE VARIABLE oWizard AS DetailViewWizard NO-UNDO .

        DO ON ERROR UNDO, THROW:
            oWizard = NEW DetailViewWizard () .

            CATCH err AS Progress.Lang.SysError:
                IF err:NumMessages = 0 THEN DO:
                    oWizard = NEW DetailViewWizard () .
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

        DEFINE VARIABLE oGenerarteDetailView AS ButtonTool  NO-UNDO .
        DEFINE VARIABLE oTelerikPlatform     AS ButtonTool  NO-UNDO .
        DEFINE VARIABLE oGroup               AS RibbonGroup NO-UNDO .
        DEFINE VARIABLE oTab                 AS RibbonTab   NO-UNDO .

        oController = poController .
        oForm = poForm .

        oGenerarteDetailView = NEW ButtonTool ("TelerikMobileGenerator_Plugin_GenerateDetailView":U) .
        oGenerarteDetailView:SharedProps:Caption = "Generate Detail View"{&TRAN} .
        oGenerarteDetailView:SharedProps:DisplayStyle = ToolDisplayStyle:ImageAndText .
        poToolbar:Tools:Add (oGenerarteDetailView) .

        oTelerikPlatform = NEW ButtonTool ("TelerikMobileGenerator_Plugin_TelerikPlatform":U) .
        oTelerikPlatform:SharedProps:Caption = "Telerik Platform"{&TRAN} .
        oTelerikPlatform:SharedProps:DisplayStyle = ToolDisplayStyle:ImageAndText .
        poToolbar:Tools:Add (oTelerikPlatform) .

        FILE-INFORMATION:FILE-NAME = "Consultingwerk/BusinessEntityDesigner/UI/Images/form_blue_16.png":U .
        IF FILE-INFORMATION:FULL-PATHNAME > "":U THEN
            ASSIGN oGenerarteDetailView:SharedProps:AppearancesSmall:Appearance:Image = System.Drawing.Image:FromFile
                        (FILE-INFORMATION:FULL-PATHNAME)  .

        FILE-INFORMATION:FILE-NAME = "Consultingwerk/BusinessEntityDesigner/UI/Images/form_blue_32.png":U .
        IF FILE-INFORMATION:FULL-PATHNAME > "":U THEN
            ASSIGN oGenerarteDetailView:SharedProps:AppearancesLarge:Appearance:Image = System.Drawing.Image:FromFile
                        (FILE-INFORMATION:FULL-PATHNAME)  .

        FILE-INFORMATION:FILE-NAME = "Consultingwerk/BusinessEntityDesigner/UI/Images/telerik_platform_16.png":U .
        IF FILE-INFORMATION:FULL-PATHNAME > "":U THEN
            ASSIGN oTelerikPlatform:SharedProps:AppearancesSmall:Appearance:Image = System.Drawing.Image:FromFile
                        (FILE-INFORMATION:FULL-PATHNAME)  .

        FILE-INFORMATION:FILE-NAME = "Consultingwerk/BusinessEntityDesigner/UI/Images/telerik_platform_32.png":U .
        IF FILE-INFORMATION:FULL-PATHNAME > "":U THEN
            ASSIGN oTelerikPlatform:SharedProps:AppearancesLarge:Appearance:Image = System.Drawing.Image:FromFile
                        (FILE-INFORMATION:FULL-PATHNAME)  .

        IF poToolbar:Ribbon:Tabs:Exists ("Generate":U) THEN
            oTab = poToolbar:Ribbon:Tabs["Generate":U] .
        ELSE DO:
            oTab = poToolbar:Ribbon:Tabs:Add ("Generate":U) .
            oTab:Caption = "Generate":U .
        END.

        IF oTab:Groups:Exists ("plugin_TelerikPlatform":U) THEN
            oGroup = oTab:Groups ["plugin_TelerikPlatform":U] .
        ELSE DO:
            oGroup = oTab:Groups:Add ("plugin_TelerikPlatform":U) .
            oGroup:Caption = "Telerik Mobile":U .
        END .

        oGroup:Tools:AddTool ("TelerikMobileGenerator_Plugin_GenerateDetailView":U):InstanceProps:PreferredSizeOnRibbon = RibbonToolSize:Large .
        oGroup:Tools:AddTool ("TelerikMobileGenerator_Plugin_TelerikPlatform":U):InstanceProps:PreferredSizeOnRibbon = RibbonToolSize:Large .

        poToolbar:ToolClick:Subscribe (ToolClickHandler) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Launches the Telerik Platform
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID TelerikPlatform ():

        Win32:ShellExecute ("open":U, "https://platform.telerik.com":U) .

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
            WHEN "TelerikMobileGenerator_Plugin_GenerateDetailView":U THEN
                THIS-OBJECT:GenerateDetailView () .
            WHEN "TelerikMobileGenerator_Plugin_TelerikPlatform":U THEN
                THIS-OBJECT:TelerikPlatform ().
        END CASE .

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

END CLASS.
