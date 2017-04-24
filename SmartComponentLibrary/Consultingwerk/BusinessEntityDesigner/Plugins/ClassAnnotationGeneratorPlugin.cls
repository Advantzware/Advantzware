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
    File        : ClassAnnotationGeneratorPlugin
    Purpose     : Business Entity Designer Plugin to create class
                  annotations for the current Business Entity
    Syntax      :
    Description : Creates a "Extract Class Annotations" button in the
                  Business Entity Designer. See http://confluence.consultingwerkcloud.com/wiki/display/SCL/The+Annotation+based+Type+Descriptor
                  and http://confluence.consultingwerkcloud.com/wiki/display/SCL/Business+Entity+Descriptor
                  for details
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Dec 21 14:50:56 CET 2014
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.BusinessEntityDesigner.*                                  FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Plugins.*                          FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Plugins.ClassAnnotationGenerator.* FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Services.*                         FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.UI.*                               FROM PROPATH .
USING Consultingwerk.Exceptions.*                                              FROM PROPATH.
USING Consultingwerk.Util.*                                                    FROM PROPATH .
USING Consultingwerk.Windows.Util.Forms.*                                      FROM PROPATH .
USING Infragistics.Win.UltraWinToolbars.*                                      FROM ASSEMBLY .
USING Progress.Lang.*                                                          FROM PROPATH .
USING System.Windows.Forms.*                                                   FROM ASSEMBLY .

CLASS Consultingwerk.BusinessEntityDesigner.Plugins.ClassAnnotationGeneratorPlugin
    IMPLEMENTS IBusinessEntityDesignerPlugin:

    { Consultingwerk/BusinessEntityDesigner/Services/dsBusinessEntity.i &REFERENCE-ONLY="REFERENCE-ONLY" }

    DEFINE VARIABLE oController AS BusinessEntityDatasetController NO-UNDO .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ClassAnnotationGeneratorPlugin class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ClassAnnotationGeneratorPlugin ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Shows the Wizzard to create an C# Classes
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID GenerateClassAnnotations ():

        DEFINE VARIABLE cDirectory      AS CHARACTER                 NO-UNDO .
        DEFINE VARIABLE cFileMask       AS CHARACTER                 NO-UNDO .

        DEFINE VARIABLE oGenerator      AS IClassAnnotationGenerator NO-UNDO .

        ASSIGN cDirectory = REPLACE (oController:BusinessEntityViewerControl:SmartDataSource:GetFieldValues ("BusinessEntityPackage":U),
                                     ".":U, "~\":U)
               cFileMask  = SUBSTITUTE ("&1.cls":U,
                                        oController:BusinessEntityViewerControl:SmartDataSource:GetFieldValues ("BusinessEntityName":U)) .

        ASSIGN oGenerator = {Consultingwerk/get-service.i Consultingwerk.BusinessEntityDesigner.Plugins.ClassAnnotationGenerator.IClassAnnotationGenerator
                                                          "NEW ClassAnnotationGenerator ()" } .

        oGenerator:GenerateClassAnnotations (cDirectory,
                                             cFileMask) .

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

        oTool = NEW ButtonTool ("ClassAnnotationGenerator_Plugin_Generate":U) .
        oTool:SharedProps:Caption = "Extract Class Annotations"{&TRAN} .
        oTool:SharedProps:DisplayStyle = ToolDisplayStyle:ImageAndText .
        poToolbar:Tools:Add (oTool) .

        FILE-INFORMATION:FILE-NAME = "Consultingwerk/BusinessEntityDesigner/UI/Images/message_edit_16.png":U .
        IF FILE-INFORMATION:FULL-PATHNAME > "":U THEN
            ASSIGN oTool:SharedProps:AppearancesSmall:Appearance:Image = System.Drawing.Image:FromFile
                        (FILE-INFORMATION:FULL-PATHNAME)  .

        FILE-INFORMATION:FILE-NAME = "Consultingwerk/BusinessEntityDesigner/UI/Images/message_edit_32.png":U .
        IF FILE-INFORMATION:FULL-PATHNAME > "":U THEN
            ASSIGN oTool:SharedProps:AppearancesLarge:Appearance:Image = System.Drawing.Image:FromFile
                        (FILE-INFORMATION:FULL-PATHNAME)  .

        IF poToolbar:Ribbon:Tabs:Exists ("Generate":U) THEN
            oTab = poToolbar:Ribbon:Tabs["Generate":U] .
        ELSE DO:
            oTab = poToolbar:Ribbon:Tabs:Add ("Generate":U) .
            oTab:Caption = "Generate":U .
        END.

        IF oTab:Groups:Exists ("plugin_AnnotationGenerator":U) THEN
            oGroup = oTab:Groups ["plugin_AnnotationGenerator":U] .
        ELSE DO:
            oGroup = oTab:Groups:Add ("plugin_AnnotationGenerator":U) .
            oGroup:Caption = "Annotations":U .
        END .

        oGroup:Tools:AddTool ("ClassAnnotationGenerator_Plugin_Generate":U):InstanceProps:PreferredSizeOnRibbon = RibbonToolSize:Large .

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
            WHEN "ClassAnnotationGenerator_Plugin_Generate":U THEN
                GenerateClassAnnotations () .
        END CASE .

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

END CLASS.
