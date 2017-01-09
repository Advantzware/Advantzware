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
    File        : SmartViewerGeneratorPlugin
    Purpose     : Business Entity Designer Plugin to generate SmartViewerControl
                  inherited user controls.
    Syntax      : 
    Description : Adds a "Generate Viewer" button to the Ribbon of the 
                  Business Entity Designer that allows to access a wizard
                  that supports generating a SmartViewerControl derived 
                  user control.
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Sep 21 20:38:49 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.BusinessEntityDesigner.*                              FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Plugins.*                      FROM PROPATH .  
USING Consultingwerk.BusinessEntityDesigner.Plugins.SmartViewerGenerator.* FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Services.*                     FROM PROPATH . 
USING Consultingwerk.BusinessEntityDesigner.UI.*                           FROM PROPATH . 
USING Infragistics.Win.UltraWinToolbars.*                                  FROM ASSEMBLY . 
USING Progress.Lang.*                                                      FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Plugins.SmartViewerGeneratorPlugin 
    IMPLEMENTS IBusinessEntityDesignerPlugin: 

    { Consultingwerk/BusinessEntityDesigner/Services/dsBusinessEntity.i &REFERENCE-ONLY="REFERENCE-ONLY" }

    DEFINE VARIABLE oController AS BusinessEntityDatasetController NO-UNDO .
    DEFINE VARIABLE oForm AS BusinessEntityDesignerForm            NO-UNDO .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartViewerGeneratorPlugin class                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartViewerGeneratorPlugin ():
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Shows the Wizzard to create a SmartDataViewer
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID GenerateViewer ():
        
        DEFINE VARIABLE oWizard AS SmartViewerWizard NO-UNDO . 
        DEFINE VARIABLE hDataset AS HANDLE NO-UNDO.
        
        DO ON ERROR UNDO, THROW:
            oWizard = NEW SmartViewerWizard () .
            
            CATCH err AS Progress.Lang.SysError:
            	IF err:NumMessages = 0 THEN DO:
            	    oWizard = NEW SmartViewerWizard () .
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
        
        DEFINE VARIABLE oGenerarteViewer AS ButtonTool  NO-UNDO .
        DEFINE VARIABLE oGroup           AS RibbonGroup NO-UNDO .
        
        oController = poController . 
        oForm = poForm . 
        
        oGenerarteViewer = NEW ButtonTool ("SmartViewerGenerator_Plugin_GenerateViewer":U) .
        oGenerarteViewer:SharedProps:Caption = "Generate Viewer"{&TRAN} .
        oGenerarteViewer:SharedProps:DisplayStyle = ToolDisplayStyle:ImageAndText .
        poToolbar:Tools:Add (oGenerarteViewer) .        

        FILE-INFO:FILE-NAME = "Consultingwerk/BusinessEntityDesigner/UI/Images/magic_wand_16.png":U .        
        IF FILE-INFO:FULL-PATHNAME > "":U THEN 
            ASSIGN oGenerarteViewer:SharedProps:AppearancesSmall:Appearance:Image = System.Drawing.Image:FromFile 
                        (FILE-INFO:FULL-PATHNAME)  .        

        FILE-INFO:FILE-NAME = "Consultingwerk/BusinessEntityDesigner/UI/Images/magic_wand_32.png":U .        
        IF FILE-INFO:FULL-PATHNAME > "":U THEN 
            ASSIGN oGenerarteViewer:SharedProps:AppearancesLarge:Appearance:Image = System.Drawing.Image:FromFile 
                        (FILE-INFO:FULL-PATHNAME)  .        
        
        IF poToolbar:Ribbon:Tabs [0]:Groups:Exists ("plugin_UIComponentsGenerator":U) THEN 
            oGroup = poToolbar:Ribbon:Tabs [0]:Groups ["plugin_UIComponentsGenerator":U] . 
        ELSE DO:
            oGroup = poToolbar:Ribbon:Tabs [0]:Groups:Add ("plugin_UIComponentsGenerator":U) .
            oGroup:Caption = "UI Components":U .
        END . 
          
        oGroup:Tools:AddTool ("SmartViewerGenerator_Plugin_GenerateViewer":U):InstanceProps:PreferredSizeOnRibbon = RibbonToolSize:Large .
  
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
            WHEN "SmartViewerGenerator_Plugin_GenerateViewer":U THEN
                GenerateViewer () .
        END.
        
        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.
        
    END METHOD.

END CLASS.
