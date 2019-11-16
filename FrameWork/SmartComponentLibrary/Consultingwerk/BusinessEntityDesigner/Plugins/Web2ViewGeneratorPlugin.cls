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
    File        : Web2ViewGeneratorPlugin
    Purpose     : Business Entity Designer Plugin to generate fragments
                  of html views used in the SmartComponents2.Web (Angular JS)
    Syntax      :
    Description : Adds a "Generate Web2 Viewer" and "Generate Web2 Form"
                  button to the Business Entity Designer Ribbon that allows
                  opening a wizard for a detail view and form.
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Sep 21 20:38:49 CEST 2012
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.BusinessEntityDesigner.*                       FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Plugins.*               FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Plugins.TelerikMobile.* FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Plugins.Web2.*          FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Services.*              FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.UI.*                    FROM PROPATH .
USING Consultingwerk.Util.*                                         FROM PROPATH .
USING Consultingwerk.Windows.API.*                                  FROM PROPATH .
USING Infragistics.Win.UltraWinToolbars.*                           FROM ASSEMBLY .
USING Progress.Lang.*                                               FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Plugins.Web2ViewGeneratorPlugin
    IMPLEMENTS IBusinessEntityDesignerPlugin:

    { Consultingwerk/BusinessEntityDesigner/Services/dsBusinessEntity.i &REFERENCE-ONLY="REFERENCE-ONLY" }

    DEFINE VARIABLE oController AS BusinessEntityDatasetController NO-UNDO .
    DEFINE VARIABLE oForm AS BusinessEntityDesignerForm            NO-UNDO .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the Web2ViewGeneratorPlugin class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC Web2ViewGeneratorPlugin ():
        SUPER ().

    END CONSTRUCTOR.

    /**
     * Purpose: Shows the wizard to create a Form
     * Notes:
     */
    METHOD PROTECTED VOID GenerateWeb2Form():

        DEFINE VARIABLE oWizard AS Web2FormWizard NO-UNDO .

        DO ON ERROR UNDO, THROW:
            oWizard = NEW Web2FormWizard () .

            CATCH err AS Progress.Lang.SysError:
                IF err:NumMessages = 0 THEN DO:
                    oWizard = NEW Web2FormWizard () .
                END.
                ELSE
                    UNDO, THROW err .
            END CATCH.
        END.

        oWizard:BusinessEntityName    = oController:BusinessEntityViewerControl:SmartDataSource:GetFieldValues ("BusinessEntityName":U) .
        oWizard:BusinessEntityPackage = oController:BusinessEntityViewerControl:SmartDataSource:GetFieldValues ("BusinessEntityPackage":U) .

        oWizard:Owner = oForm .

        WAIT-FOR oWizard:ShowDialog () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Shows the Wizard to create a Detail View
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID GenerateWeb2Viewer ():

        DEFINE VARIABLE oWizard AS Web2ViewerWizard NO-UNDO .

        DO ON ERROR UNDO, THROW:
            oWizard = NEW Web2ViewerWizard () .

            CATCH err AS Progress.Lang.SysError:
                IF err:NumMessages = 0 THEN DO:
                    oWizard = NEW Web2ViewerWizard () .
                END.
                ELSE
                    UNDO, THROW err .
            END CATCH.
        END.

        oWizard:BusinessEntityName    = oController:BusinessEntityViewerControl:SmartDataSource:GetFieldValues ("BusinessEntityName":U) .
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

        DEFINE VARIABLE oGenerarteDetailView AS ButtonTool  NO-UNDO .
        DEFINE VARIABLE oGenerarteForm       AS ButtonTool  NO-UNDO .
        DEFINE VARIABLE oGroup               AS RibbonGroup NO-UNDO .
        DEFINE VARIABLE oTab                 AS RibbonTab   NO-UNDO .

        oController = poController .
        oForm = poForm .

        oGenerarteDetailView = NEW ButtonTool ("Web2ViewGeneratorPlugin_Plugin_GenerateViewer":U) .
        oGenerarteDetailView:SharedProps:Caption = "Generate Web2 Viewer"{&TRAN} .
        oGenerarteDetailView:SharedProps:DisplayStyle = ToolDisplayStyle:ImageAndText .
        poToolbar:Tools:Add (oGenerarteDetailView) .

        ASSIGN oGenerarteDetailView:SharedProps:AppearancesSmall:Appearance:Image = ImageHelper:Load ("Consultingwerk/BusinessEntityDesigner/UI/Images/angular_16.png":U)
               oGenerarteDetailView:SharedProps:AppearancesLarge:Appearance:Image = ImageHelper:Load ("Consultingwerk/BusinessEntityDesigner/UI/Images/angular_32.png":U) .

        oGenerarteForm = NEW ButtonTool ("Web2ViewGeneratorPlugin_Plugin_GenerateForm":U) .
        oGenerarteForm:SharedProps:Caption = "Generate Web2 Form"{&TRAN} .
        oGenerarteForm:SharedProps:DisplayStyle = ToolDisplayStyle:ImageAndText .
        poToolbar:Tools:Add (oGenerarteForm) .

        ASSIGN oGenerarteForm:SharedProps:AppearancesSmall:Appearance:Image = ImageHelper:Load ("Consultingwerk/BusinessEntityDesigner/UI/Images/angular_window_32.png":U)
               oGenerarteForm:SharedProps:AppearancesLarge:Appearance:Image = ImageHelper:Load ("Consultingwerk/BusinessEntityDesigner/UI/Images/angular_window_32.png":U) .

        IF poToolbar:Ribbon:Tabs:Exists ("Generate":U) THEN
            oTab = poToolbar:Ribbon:Tabs["Generate":U] .
        ELSE DO:
            oTab = poToolbar:Ribbon:Tabs:Add ("Generate":U) .
            oTab:Caption = "Generate":U .
        END.

        IF oTab:Groups:Exists ("plugin_Web2":U) THEN
            oGroup = oTab:Groups ["plugin_Web2":U] .
        ELSE DO:
            oGroup = oTab:Groups:Add ("plugin_Web2":U) .
            oGroup:Caption = "Angular JS (Web2)":U .
        END .

        oGroup:Tools:AddTool ("Web2ViewGeneratorPlugin_Plugin_GenerateViewer":U):InstanceProps:PreferredSizeOnRibbon = RibbonToolSize:Large .
        oGroup:Tools:AddTool ("Web2ViewGeneratorPlugin_Plugin_GenerateForm":U):InstanceProps:PreferredSizeOnRibbon = RibbonToolSize:Large .

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
            WHEN "Web2ViewGeneratorPlugin_Plugin_GenerateViewer":U THEN
                THIS-OBJECT:GenerateWeb2Viewer () .
            WHEN "Web2ViewGeneratorPlugin_Plugin_GenerateForm":U THEN
                THIS-OBJECT:GenerateWeb2Form () .
        END CASE .

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

END CLASS.
