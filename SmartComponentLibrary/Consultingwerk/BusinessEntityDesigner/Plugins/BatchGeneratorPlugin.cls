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
    File        : BatchGeneratorPlugin
    Purpose     : Business Entity Designer Plugin that supports automated 
                  generation of Business Entities for a Batch of Database 
                  Tables (one Business Entity per Database Table) 
    Syntax      : 
    Description : Creates a "Batch Generator" button in the Ribbon of the 
                  Business Entity Designer 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Sep 21 20:38:49 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.BusinessEntityDesigner.*                        FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Plugins.*                FROM PROPATH .  
USING Consultingwerk.BusinessEntityDesigner.Plugins.BatchGenerator.* FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Services.*               FROM PROPATH . 
USING Consultingwerk.BusinessEntityDesigner.UI.*                     FROM PROPATH . 
USING Infragistics.Win.UltraWinToolbars.*                            FROM ASSEMBLY . 
USING Progress.Lang.*                                                FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Plugins.BatchGeneratorPlugin 
    IMPLEMENTS IBusinessEntityDesignerPlugin: 

    { Consultingwerk/BusinessEntityDesigner/Services/dsBusinessEntity.i &REFERENCE-ONLY="REFERENCE-ONLY" }

    DEFINE VARIABLE oController AS BusinessEntityDatasetController NO-UNDO .
    DEFINE VARIABLE oForm AS BusinessEntityDesignerForm            NO-UNDO .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the BatchGeneratorPlugin class                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC BatchGeneratorPlugin ():
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Shows the Wizzard for the Batch Generator
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID BatchGenerator ():
        
        DEFINE VARIABLE oWizard AS BatchGeneratorWizard NO-UNDO . 
        
        oWizard = NEW BatchGeneratorWizard () .
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
        
        oTool = NEW ButtonTool ("BatchGenerator_Plugin":U) .
        oTool:SharedProps:Caption = "Batch Generator"{&TRAN} .
        oTool:SharedProps:DisplayStyle = ToolDisplayStyle:ImageAndText .
        poToolbar:Tools:Add (oTool) .        

        FILE-INFO:FILE-NAME = "Consultingwerk/BusinessEntityDesigner/UI/Images/gears_16.png":U .        
        IF FILE-INFO:FULL-PATHNAME > "":U THEN 
            ASSIGN oTool:SharedProps:AppearancesSmall:Appearance:Image = System.Drawing.Image:FromFile 
                        (FILE-INFO:FULL-PATHNAME)  .        

        FILE-INFO:FILE-NAME = "Consultingwerk/BusinessEntityDesigner/UI/Images/gears_32.png":U .        
        IF FILE-INFO:FULL-PATHNAME > "":U THEN 
            ASSIGN oTool:SharedProps:AppearancesLarge:Appearance:Image = System.Drawing.Image:FromFile 
                        (FILE-INFO:FULL-PATHNAME)  .        
        
        IF poToolbar:Ribbon:Tabs:Exists ("Generate":U) THEN 
            oTab = poToolbar:Ribbon:Tabs["Generate":U] . 
        ELSE DO:  
            oTab = poToolbar:Ribbon:Tabs:Add ("Generate":U) . 
            oTab:Caption = "Generate":U . 
        END.
        
        IF oTab:Groups:Exists ("plugin_BatchGenerator":U) THEN 
            oGroup = oTab:Groups ["plugin_BatchGenerator":U] . 
        ELSE DO:
            oGroup = oTab:Groups:Add ("plugin_BatchGenerator":U) .
            oGroup:Caption = "Batch Generator":U .
        END . 
          
        oGroup:Tools:AddTool ("BatchGenerator_Plugin":U):InstanceProps:PreferredSizeOnRibbon = RibbonToolSize:Large .
  
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
            WHEN "BatchGenerator_Plugin":U THEN
                THIS-OBJECT:BatchGenerator () .
        END.
        
        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.
        
    END METHOD.

END CLASS.
