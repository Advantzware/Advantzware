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
    File        : MenuStructureRibbonTabController
    Purpose     : Allows the rendering of SmartFramework Menu Structrures 
                  as Ribbon Tabs 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Aug 15 19:29:09 CEST 2013
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
USING Infragistics.Win.UltraWinExplorerBar.*      FROM ASSEMBLY .
USING Progress.Lang.*                             FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.Controls.MenuStructureExplorerBarGroupController: 

    {Consultingwerk/Windows/Framework/ttMenuStructure.i &ACCESS=PROTECTED}

    DEFINE           VARIABLE oExplorerBarGroup AS UltraExplorerBarGroup  NO-UNDO .
    DEFINE PROTECTED VARIABLE oMenuProvider     AS IMenuStructureProvider NO-UNDO .
    DEFINE PROTECTED VARIABLE oImageProvider    AS IImageProvider         NO-UNDO .
    
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
    DEFINE PUBLIC PROPERTY ExplorerBar AS UltraExplorerBar NO-UNDO 
    GET.
    PROTECTED SET. 
        
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the MenuStructureRibbonTabController class
        Notes:   
        @param poExplorerBar The UltraExplorerBar that owns the Explorer Bar Group 
        @param poOwner The owner in which to invoke methodes (depending on the type of Menu Item)
        @param pcGroupKey The key of the Group to initialize
        @param pcGroupTitle The title of the group to initialize
        @param pcMenuStructureKey The key of the menu structure to render
        @param pcSmallImageKey The key of the small Group Image
        @param pcLargeImageKey The key of the large Group Image
        @param piPosition The position of the Explorer Bar Group or ?
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC MenuStructureExplorerBarGroupController (poExplorerBar AS UltraExplorerBar,
                                                                poOwner AS Progress.Lang.Object,
                                                                pcGroupKey AS CHARACTER,
                                                                pcGroupTitle AS CHARACTER,
                                                                pcMenuStructureKey AS CHARACTER,
                                                                pcSmallImageKey AS CHARACTER,
                                                                pcLargeImageKey AS CHARACTER,
                                                                piPosition AS INTEGER):
        
        DEFINE VARIABLE oGroup         AS UltraExplorerBarGroup                   NO-UNDO .

        ObjectAssert:IsValid (poExplorerBar, "UltraExplorerBar":U) .
        ObjectAssert:IsValid (poOwner, "Owner":U) . 
        Assert:NotNullOrEmpty (pcMenuStructureKey) .
        
        ASSIGN THIS-OBJECT:ExplorerBar      = poExplorerBar
               THIS-OBJECT:Owner            = poOwner
               THIS-OBJECT:MenuStructureKey = pcMenuStructureKey .

        IF piPosition = ? THEN
            oGroup = poExplorerBar:Groups:Add (pcMenuStructureKey, pcGroupTitle) .
        ELSE
            oGroup = poExplorerBar:Groups:Insert (piPosition, pcGroupKey, pcGroupTitle) .

        oGroup:Settings:Style = GroupStyle:LargeImagesWithText .

        oImageProvider = {Consultingwerk/get-service.i Consultingwerk.Windows.Framework.IImageProvider} .
        oMenuProvider = {Consultingwerk/get-service.i Consultingwerk.Windows.Framework.IMenuStructureProvider} .

        IF VALID-OBJECT (oImageProvider) THEN DO:
            oGroup:Settings:AppearancesSmall:HeaderAppearance:Image = oImageProvider:ImageForKey
                        (pcSmallImageKey) .
            oGroup:Settings:AppearancesLarge:HeaderAppearance:Image = oImageProvider:ImageForKey
                        (pcLargeImageKey) .
        END.

        THIS-OBJECT:FillMenuStructure (pcMenuStructureKey, oGroup) .
        
        poExplorerBar:ItemClick:Subscribe (ItemClickHandler) .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Fills the explorer bar group 
        Notes:   
        @param pcMenuStructureKey The key of the menu structure to render
        @param poGroup The UltraExplorerBarGroup to render to
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID FillMenuStructure (pcMenuStructureKey AS CHARACTER,
                                          poGroup AS UltraExplorerBarGroup):
        
        DEFINE VARIABLE cParentNodeKey AS CHARACTER            NO-UNDO .
        DEFINE VARIABLE oItem          AS UltraExplorerBarItem NO-UNDO . 
        
        DEFINE BUFFER ttMenuStructure FOR ttMenuStructure . 
        
        oMenuProvider:GetMenuStructure (NEW GetMenuStructureParameter (pcMenuStructureKey, 
                                                                       0,
                                                                       FALSE),
                                        OUTPUT TABLE ttMenuStructure BY-REFERENCE) .
            
        poGroup:ExplorerBar:BeginUpdate() .

        FOR EACH ttMenuStructure WHERE ttMenuStructure.ParentMenuKey = pcMenuStructureKey
                BY ttMenuStructure.MenuSequence:

            oItem = poGroup:Items:Add (ttMenuStructure.MenuKey,
                                       ttMenuStructure.MenuName) .

            oItem:Settings:AppearancesLarge:Appearance:Image = oImageProvider:ImageForKey (ttMenuStructure.MenuLargeImage) .
            oItem:Settings:AppearancesSmall:Appearance:Image = oImageProvider:ImageForKey (ttMenuStructure.MenuSmallImage) .
            
        END.

        poGroup:ExplorerBar:EndUpdate() .

    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ItemClick Event of the UltraExplorerBar 
        Notes:   
        @param sender The reference to object that raised the event
        @param e The ItemEventArgs instance with the data for this event                                                                      
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ItemClickHandler (sender AS System.Object, 
                                          e AS ItemEventArgs):
        
        DEFINE VARIABLE oLauncher AS IMenuFunctionLauncher NO-UNDO . 
        DEFINE VARIABLE cKey      AS CHARACTER             NO-UNDO.

        ASSIGN cKey = e:Item:Key .

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
