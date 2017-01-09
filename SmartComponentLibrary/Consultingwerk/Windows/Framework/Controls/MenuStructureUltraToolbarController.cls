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
    File        : MenuStructureUltraToolbarController
    Purpose     : Abstract base class for UltraToolbarsManager based rendering
                  of menu structures
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Aug 16 07:53:42 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                            FROM PROPATH .
USING Consultingwerk.Assertion.*                  FROM PROPATH . 
USING Consultingwerk.SmartFramework.*             FROM PROPATH .
USING Consultingwerk.SmartFramework.Menu.*        FROM PROPATH .
USING Consultingwerk.Util.*                       FROM PROPATH .
USING Consultingwerk.Windows.Framework.*          FROM PROPATH .  
USING Consultingwerk.Windows.Framework.Controls.* FROM PROPATH .  
USING Infragistics.Win.UltraWinToolbars.*         FROM ASSEMBLY .
USING Progress.Lang.*                             FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.Controls.MenuStructureUltraToolbarController 
    ABSTRACT: 

    {Consultingwerk/Windows/Framework/ttMenuStructure.i &ACCESS=PROTECTED}

    DEFINE PROTECTED VARIABLE oMenuProvider  AS IMenuStructureProvider NO-UNDO .
    DEFINE PROTECTED VARIABLE oImageProvider AS IImageProvider         NO-UNDO . 

    /*------------------------------------------------------------------------------
        Purpose: Returns the root key of the menu structure to render
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY MenuStructureKey AS CHARACTER NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the owning object 
        Notes:   The owner is the object in which to invoke methods 
                 (depending on the type of menu item) 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Owner AS Progress.Lang.Object NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the UltraToolbarsManager that renders
                 the Ribbon Tab 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ToolbarsManager AS UltraToolbarsManager NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the MenuStructureUltraToolbarController class
        Notes:   
        @param poToolbarsManager The UltraToolbarsManager that owns the Ribbon 
        @param poOwner The owner in which to invoke methodes (depending on the type of Menu Item)
        @param pcMenuStructureKey The key of the menu structure to render
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC MenuStructureUltraToolbarController (poToolbarsManager AS UltraToolbarsManager,
                                                            poOwner AS Progress.Lang.Object,
                                                            pcMenuStructureKey AS CHARACTER):
        
        ObjectAssert:IsValid (poToolbarsManager, "UltraToolbarsManager":U) .
        ObjectAssert:IsValid (poOwner, "Owner":U) . 
        Assert:NotNullOrEmpty (pcMenuStructureKey) .
        
        ASSIGN THIS-OBJECT:ToolbarsManager  = poToolbarsManager
               THIS-OBJECT:Owner            = poOwner
               THIS-OBJECT:MenuStructureKey = pcMenuStructureKey .

        oImageProvider = {Consultingwerk/get-service.i Consultingwerk.Windows.Framework.IImageProvider} .
        oMenuProvider = {Consultingwerk/get-service.i Consultingwerk.Windows.Framework.IMenuStructureProvider} .

        poToolbarsManager:ToolClick:Subscribe (ToolClickHandler) .
        
    END CONSTRUCTOR.
    /*------------------------------------------------------------------------------
        Purpose: Renders the child menu items into a tools collection 
        Notes:   
        @param pcParentMenuKey The parent menu key
        @param poTools The parent tools collection
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID RenderTools (pcParentMenuKey AS CHARACTER,
                                       poTools AS ToolsCollection):
        
        DEFINE VARIABLE oTool AS ToolBase NO-UNDO .

        DEFINE BUFFER ttMenuStructure FOR ttMenuStructure . 

        FOR EACH ttMenuStructure WHERE ttMenuStructure.ParentMenuKey = pcParentMenuKey
                BY ttMenuStructure.MenuSequence:
        
            IF THIS-OBJECT:ToolbarsManager:Tools:Exists (ttMenuStructure.MenuKey) THEN 
                oTool = THIS-OBJECT:ToolbarsManager:Tools[ttMenuStructure.MenuKey].
            ELSE DO:
                IF ttMenuStructure.HasChild THEN 
                    oTool = NEW PopupMenuTool (ttMenuStructure.MenuKey) .
                ELSE 
                    oTool = NEW ButtonTool (ttMenuStructure.MenuKey) .

                THIS-OBJECT:ToolbarsManager:Tools:Add (oTool) .
            END.
                
            oTool:SharedProps:Caption = ttMenuStructure.MenuName .

            IF VALID-OBJECT (oImageProvider) THEN DO:
                IF ttMenuStructure.MenuSmallImage > "":U THEN 
                    oTool:SharedProps:AppearancesSmall:Appearance:Image = oImageProvider:ImageForKey (ttMenuStructure.MenuSmallImage) . 
                    
                IF ttMenuStructure.MenuLargeImage > "":U THEN 
                    oTool:SharedProps:AppearancesLarge:Appearance:Image = oImageProvider:ImageForKey (ttMenuStructure.MenuLargeImage) . 
            END.

            IF ttMenuStructure.HasChild THEN 
                THIS-OBJECT:RenderTools (ttMenuStructure.MenuKey,
                                         CAST (oTool, PopupMenuTool):Tools) . 
            
            IF poTools:Exists (ttMenuStructure.MenuKey) THEN 
                oTool = poTools:Item[ttMenuStructure.MenuKey].
            ELSE 
                oTool = poTools:AddTool (ttMenuStructure.MenuKey) .
            
            IF ttMenuStructure.MenuLargeImage > "":U THEN 
                oTool:InstanceProps:PreferredSizeOnRibbon = RibbonToolSize:Large .
            ELSE 
                oTool:InstanceProps:PreferredSizeOnRibbon = RibbonToolSize:Normal .
                
            
            IF ttMenuStructure.MenuBeginsAGroup THEN  
                oTool:InstanceProps:IsFirstInGroup = TRUE . 
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ToolClick event of the UltraToolbarsManager
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The ToolClickEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ToolClickHandler (sender AS System.Object, 
                                            e AS ToolClickEventArgs):
        
        DEFINE VARIABLE oLauncher AS IMenuFunctionLauncher NO-UNDO . 
        DEFINE VARIABLE cKey      AS CHARACTER             NO-UNDO.

        ASSIGN cKey = e:Tool:Key .

        IF NOT CAN-FIND (ttMenuStructure WHERE ttMenuStructure.MenuKey = cKey) 
            THEN RETURN . 

        FIND ttMenuStructure WHERE ttMenuStructure.MenuKey = cKey .

        IF CharacterType:IsNullOrEmpty (ttMenuStructure.FunctionKey) THEN 
            RETURN . 

        oLauncher = {Consultingwerk/get-service.i Consultingwerk.Windows.Framework.IMenuFunctionLauncher} .
        
        IF VALID-OBJECT (oLauncher) THEN 
            oLauncher:LaunchMenuFunction (ttMenuStructure.FunctionKey, 
                                          NEW MenuContext (ttMenuStructure.FunctionKey, ttMenuStructure.MenuName, THIS-OBJECT:Owner)) .

        CATCH err AS Progress.Lang.Error:
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.

    END METHOD .

END CLASS.
