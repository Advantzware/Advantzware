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
USING Infragistics.Win.UltraWinToolbars.*         FROM ASSEMBLY .
USING Progress.Lang.*                             FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.Controls.MenuStructureRibbonTabController
    INHERITS MenuStructureUltraToolbarController: 

    DEFINE VARIABLE oRibbonTab     AS RibbonTab              NO-UNDO .
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the MenuStructureRibbonTabController class
        Notes:   
        @param poToolbarsManager The UltraToolbarsManager that owns the Ribbon 
        @param poOwner The owner in which to invoke methodes (depending on the type of Menu Item)
        @param pcMenuStructureKey The key of the menu structure to render
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC MenuStructureRibbonTabController (poToolbarsManager AS UltraToolbarsManager,
                                                         poOwner AS Progress.Lang.Object,
                                                         pcMenuStructureKey AS CHARACTER):
        
        SUPER (poToolbarsManager,
               poOwner,
               pcMenuStructureKey) .
        
        IF VALID-OBJECT (oMenuProvider) THEN 
            THIS-OBJECT:FillMenuStructure (pcMenuStructureKey) .        
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Fills the tree menu 
        Notes:   
        @param pcMenuStructureKey The key of the menu structure to render
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID FillMenuStructure (pcMenuStructureKey AS CHARACTER):
        
        DEFINE VARIABLE oRibbonGroup   AS RibbonGroup NO-UNDO .

        DEFINE BUFFER ttMenuStructure FOR ttMenuStructure .

        oMenuProvider:GetMenuStructure (NEW GetMenuStructureParameter (pcMenuStructureKey,
                                                                       0,
                                                                       TRUE),
                                        OUTPUT TABLE ttMenuStructure BY-REFERENCE) .

        FIND ttMenuStructure WHERE ttMenuStructure.MenuKey = pcMenuStructureKey NO-ERROR .

        IF NOT AVAILABLE ttMenuStructure THEN
            RETURN .

        IF THIS-OBJECT:ToolbarsManager:Ribbon:Tabs:Exists (pcMenuStructureKey) THEN
            THIS-OBJECT:ToolbarsManager:Ribbon:Tabs:Remove (THIS-OBJECT:ToolbarsManager:Ribbon:Tabs[pcMenuStructureKey]) .

        THIS-OBJECT:ToolbarsManager:BeginUpdate() .

        oRibbonTab = THIS-OBJECT:ToolbarsManager:Ribbon:Tabs:Add (pcMenuStructureKey) .
        oRibbonTab:Caption = ttMenuStructure.MenuName .

        FOR EACH ttMenuStructure WHERE ttMenuStructure.ParentMenuKey = pcMenuStructureKey
            BY ttMenuStructure.MenuSequence ON ERROR UNDO, THROW:

            IF NOT oRibbonTab:Groups:Exists (ttMenuStructure.MenuKey) THEN DO:
                oRibbonGroup = oRibbonTab:Groups:Add (ttMenuStructure.MenuKey) .
                oRibbonGroup:Caption = ttMenuStructure.MenuName .

                THIS-OBJECT:RenderTools (ttMenuStructure.MenuKey,
                                         oRibbonGroup:Tools) .
            END.
        END.

        THIS-OBJECT:ToolbarsManager:EndUpdate() .

    END METHOD . 

END CLASS.
