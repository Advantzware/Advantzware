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
    File        : SampleRibbonBuilder
    Purpose     : Sample IRibbonBuilder implementation
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Feb 28 15:51:32 CET 2012
    Notes       : Used in the WinKit after the UltraToolbarsHelper routines 
                  have built Infragistics Toolbars and menus to turn 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.WindowIntegrationKit.IRibbonBuilder FROM PROPATH .
USING Infragistics.Win.UltraWinToolbars.*                FROM ASSEMBLY .
USING Progress.Lang.*                                    FROM PROPATH .

CLASS Consultingwerk.WindowIntegrationKit.SampleRibbonBuilder IMPLEMENTS IRibbonBuilder: 

    DEFINE TEMP-TABLE ttTabs NO-UNDO 
        FIELD TabKey     AS CHARACTER 
        FIELD TabSort    AS INTEGER 
        INDEX TabKey IS PRIMARY UNIQUE TabKey 
        INDEX TabSort TabSort .

    DEFINE TEMP-TABLE ttGroups NO-UNDO 
        FIELD TabKey       AS CHARACTER 
        FIELD GroupKey     AS CHARACTER 
        FIELD GroupSort    AS INTEGER 
        INDEX GroupKey IS PRIMARY UNIQUE TabKey GroupKey 
        INDEX GroupSort TabKey GroupSort .

    DEFINE TEMP-TABLE ttTools NO-UNDO 
        FIELD TabKey   AS CHARACTER 
        FIELD GroupKey AS CHARACTER 
        FIELD ToolKey  AS CHARACTER 
        FIELD ToolSort AS INTEGER  
        INDEX ToolKey IS PRIMARY UNIQUE TabKey GroupKey ToolKey
        INDEX ToolSort TabKey GroupKey ToolSort .

    /*------------------------------------------------------------------------------
        Purpose: Adds the Tools contained in the MainMenuToolbar to the left 
                 application menu of the Ribbon                                                                     
        Notes:   Executes the function getWinKitExcludeMenuItemsFromRibbon in the callback
                 procedure to return a list of tools (sub menu names) that should be 
                 excluded from the application menu
        @param poToolbarsManager The Infragistics.Win.UltraWinToolbars.UltraToolbarsManager instance to build a Ribbon for
        @param phCallbackProcedure The PROCEDURE to use for callbacks                     
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID AddMainMenuToLeftApplicationMenu (poToolbarsManager AS UltraToolbarsManager,
                                                            phCallbackProcedure AS HANDLE):
        
        /* Mike Fechner, Consultingwerk Ltd. 08.03.2012 - WinKit PoC
           Now add the MainMenuBar elements to the Application Menu */      
        DEFINE VARIABLE oMenu             AS UltraToolbar NO-UNDO . 
        DEFINE VARIABLE oMenuToolInst     AS ToolBase     NO-UNDO. 
        DEFINE VARIABLE lFirst            AS LOGICAL      NO-UNDO INIT TRUE .
        DEFINE VARIABLE cExcludeMenuItems AS CHARACTER    NO-UNDO.
        DEFINE VARIABLE i AS INTEGER NO-UNDO.
        
        IF Consultingwerk.Util.ProcedureHelper:HasEntry (phCallBackProcedure,
                                                         "getWinKitExcludeMenuItemsFromRibbon":U,
                                                         "FUNCTION":U) THEN 
                                                         
            ASSIGN cExcludeMenuItems = DYNAMIC-FUNCTION ("getWinKitExcludeMenuItemsFromRibbon":U IN phCallbackProcedure) .
        
        oMenu = Consultingwerk.Util.UltraToolbarsHelper:GetMainMenuToolbar (poToolbarsManager) .
        
        IF VALID-OBJECT (oMenu) THEN DO:
            {Consultingwerk/foreach.i ToolBase oMenuTool in oMenu:Tools}
            
                /* Menu-Items werden mit mnu_ vom UltraToolbarsHelper geprefixt. Der muss hier wieder abgezogen werden */
                IF cExcludeMenuItems > "":U AND LOOKUP (SUBSTRING (oMenuTool:Key, 5), cExcludeMenuItems) > 0 THEN 
                    NEXT . 
            
                oMenuTool = CreateTool (oMenuTool:Key,
                                        poToolbarsManager:Ribbon:ApplicationMenu:ToolAreaLeft:Tools) .                      
                
                IF lFirst THEN 
                    ASSIGN oMenuTool:InstanceProps:IsFirstInGroup = TRUE 
                           lFirst = FALSE . 
            END.
        END.            

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Builds a Ribbon for a toolbar manager instance       
        Notes:   Used in the WinKit after the UltraToolbarsHelper routines have built 
                 Infragistics Toolbars and menus to turn                                                
        @param poToolbarsManager The Infragistics.Win.UltraWinToolbars.UltraToolbarsManager instance to build a Ribbon for
        @param phCallbackProcedure The PROCEDURE to use for callbacks                     
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID BuildRibbonForToolbar (poToolbarsManager AS UltraToolbarsManager, 
                                              phCallbackProcedure AS HANDLE):
    
        DEFINE VARIABLE oTab      AS RibbonTab   NO-UNDO .
        DEFINE VARIABLE oGroup    AS RibbonGroup NO-UNDO . 
                
        DEFINE VARIABLE lFirst    AS LOGICAL     NO-UNDO.
        DEFINE VARIABLE oToolInst AS ToolBase    NO-UNDO.
                
        Consultingwerk.Assertion.ObjectAssert:IsValid (poToolbarsManager) .
    
        EMPTY TEMP-TABLE ttTabs .
        EMPTY TEMP-TABLE ttGroups .
        EMPTY TEMP-TABLE ttTools .
        
        {Consultingwerk/foreach.i ToolBase oTool in poToolbarsManager:Tools}
             InsertTool (oTool:Key) .
        END.

        FOR EACH ttTabs WHERE ttTabs.TabKey <> ? BY ttTabs.TabSort:
            
            oTab = CreateTab (ttTabs.TabKey,
                              poToolbarsManager) .
            
            FOR EACH ttGroups WHERE ttGroups.TabKey = ttTabs.TabKey
                                 BY ttGroups.GroupSort:
                       
                oGroup = CreateGroup (ttGroups.GroupKey,
                                      oTab) .                       
                              
                FOR EACH ttTools WHERE ttTools.TabKey   = ttTabs.TabKey
                                   AND ttTools.GroupKey = ttGroups.GroupKey 
                                    BY ttTools.ToolSort:
                    
                    CreateTool (ttTools.ToolKey,
                                oGroup:Tools) .
                END. 
            END.
        END.
        
        THIS-OBJECT:AddMainMenuToLeftApplicationMenu (poToolbarsManager, 
                                                      phCallbackProcedure) .

        ASSIGN lFirst = TRUE . 

        FOR EACH ttTools WHERE ttTools.TabKey   = ?
                           AND ttTools.GroupKey = "ToolAreaLeft":U
                            BY ttTools.ToolSort:
        
            oToolInst = CreateTool (ttTools.ToolKey,
                                    poToolbarsManager:Ribbon:ApplicationMenu:ToolAreaLeft:Tools) .  
                                    
            IF lFirst THEN ASSIGN oToolInst:InstanceProps:IsFirstInGroup = TRUE 
                                  lFirst = FALSE .
        END.         

        FOR EACH ttTools WHERE ttTools.TabKey   = ?
                           AND ttTools.GroupKey = "ToolAreaRight":U
                            BY ttTools.ToolSort:
            
            CreateTool (ttTools.ToolKey,
                        poToolbarsManager:Ribbon:ApplicationMenu:ToolAreaRight:Tools) .                                              
        END. 
            
        FOR EACH ttTools WHERE ttTools.TabKey   = ?
                           AND ttTools.GroupKey = "FooterToolbar":U
                            BY ttTools.ToolSort:
            
            CreateTool (ttTools.ToolKey,
                        poToolbarsManager:Ribbon:ApplicationMenu:FooterToolbar:Tools) .                                              
        END. 
        
        FOR EACH ttTools WHERE ttTools.TabKey   = ?
                           AND ttTools.GroupKey = "TabItemToolbar":U
                            BY ttTools.ToolSort:
            
            CreateTool (ttTools.ToolKey,
                        poToolbarsManager:Ribbon:TabItemToolbar:Tools) .                                              
        END.                     
                
        poToolbarsManager:Ribbon:Visible = TRUE .

        FINALLY:
            EMPTY TEMP-TABLE ttTabs .
            EMPTY TEMP-TABLE ttGroups .
            EMPTY TEMP-TABLE ttTools .
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Creates a Ribbon Group                                                                        
        Notes:                         
        @param pcGroupKey The key for the group to create
        @param poTab The reference to the parent RibbonTab
        @return The reference to the created RibbonGroup                                                
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED RibbonGroup CreateGroup (pcGroupKey AS CHARACTER,
                                              poTab AS RibbonTab):
        
        DEFINE VARIABLE oGroup AS RibbonGroup NO-UNDO . 

        oGroup = poTab:Groups:Add (pcGroupKey) .
        
        CASE pcGroupKey:
            WHEN "Navigation":U THEN oGroup:Caption = "Navigation":U .
            WHEN "Edit":U       THEN oGroup:Caption = "Maintenance":U .
            WHEN "Search":U     THEN oGroup:Caption = "Record":U .
        END CASE . 

        RETURN oGroup . 
    
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Creates a Ribbon Tab                                                                        
        Notes:                         
        @param pcTabKey The key for the tab to create
        @param poToolbarsManager The reference to the parent UltraToolbarsManager
        @return The reference to the created RibbonTab                                                
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED RibbonTab CreateTab (pcTabKey AS CHARACTER,
                                          poToolbarsManager AS UltraToolbarsManager):
        
        DEFINE VARIABLE oTab   AS RibbonTab   NO-UNDO .
        
        oTab = poToolbarsManager:Ribbon:Tabs:Add (pcTabKey) .
        
        CASE pcTabKey:
            WHEN "Start":U THEN oTab:Caption = "Start":U .
        END CASE . 

        RETURN oTab . 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Creates a Tool Instance on the Ribbon                                                                        
        Notes:                         
        @param pcToolKey The key for the tool to create an instance of
        @param poToolsCollection The reference to the parent ToolsCollection (e.g. RibbonGroup:Tools)
        @return The reference to the created Tool                                                
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED ToolBase CreateTool (pcToolKey AS CHARACTER,
                                          poToolsCollection AS ToolsCollection):
        
        DEFINE VARIABLE oTool AS ToolBase NO-UNDO . 
        
        oTool = poToolsCollection:AddTool (pcToolKey) .
        
        CASE pcToolKey:
            WHEN "BtnFirst":U  THEN oTool:InstanceProps:PreferredSizeOnRibbon = RibbonToolSize:Large .
            WHEN "BtnPrev":U   THEN oTool:InstanceProps:PreferredSizeOnRibbon = RibbonToolSize:Large .
            WHEN "BtnNext":U   THEN oTool:InstanceProps:PreferredSizeOnRibbon = RibbonToolSize:Large .
            WHEN "BtnLast":U   THEN oTool:InstanceProps:PreferredSizeOnRibbon = RibbonToolSize:Large .
        END CASE . 
        
        RETURN oTool .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Inserts a Ribbon Group in the definition                                                                     
        Notes:                                          
        @param pcTabKey The key of the containing RibbonTab
        @param pcGroupKey The key of the group to create                              
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID InsertGroup (pcTabKey AS CHARACTER,
                                       pcGroupKey AS CHARACTER):
        
        CASE pcGroupKey:
            WHEN "Navigation":U THEN InsertGroup (pcTabKey, pcGroupKey, 1) .
            WHEN "Edit":U       THEN InsertGroup (pcTabKey, pcGroupKey, 2) .
            WHEN "Search":U     THEN InsertGroup (pcTabKey, pcGroupKey, 3) .
            
            OTHERWISE InsertGroup (pcTabKey, pcGroupKey, 99) .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Inserts a Ribbon Group in the definition                                                                     
        Notes:   Alternative Groups in Tab ? are:       
                 ToolAreaLeft, ToolAreaRight, FooterToolbar, TabItemToolbar
        @param pcTabKey The key of the containing RibbonTab
        @param pcGroupKey The key of the group to create                              
        @param piGroupSort The sort weight for the group to create
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID InsertGroup (pcTabKey AS CHARACTER,
                                       pcGroupKey AS CHARACTER,
                                       piGroupSort AS INTEGER):
        
        CREATE ttGroups.
        ASSIGN ttGroups.TabKey    = pcTabKey
               ttGroups.GroupKey  = pcGroupKey
               ttGroups.GroupSort = piGroupSort . 

    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Inserts a Ribbon Tag in the definition                                                                        
        Notes:                                         
        @param pcTabKey The key of the Tab to create                               
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID InsertTab (pcTabKey AS CHARACTER):
        
        CASE pcTabKey:
            WHEN "Start":U THEN InsertTab (pcTabKey, 1) .

            OTHERWISE InsertTab (pcTabKey, 99) .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Inserts a Ribbon Tag in the definition                                                                        
        Notes:                                         
        @param pcTabKey The key of the Tab to create                               
        @param piTabSort The sort weight for the tab to create
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID InsertTab (pcTabKey AS CHARACTER,
                                     piTabSort AS INTEGER):
        
        CREATE ttTabs.
        ASSIGN ttTabs.TabKey = pcTabKey
               ttTabs.TabSort = piTabSort . 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Inserts a Tool in the definition                                                                        
        Notes:                                         
        @param pcToolKey The key of the Tool to create                               
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID InsertTool (pcToolKey AS CHARACTER):
        
        CASE pcToolKey:
            WHEN "btnEdit":U   THEN DO:
                InsertTool ("Start":U, "Edit":U, pcToolKey, 1) . 
                InsertTool (?, "ToolAreaLeft":U, pcToolKey, 1) . 
            END.
            WHEN "btnSave":U   THEN DO:
                InsertTool ("Start":U, "Edit":U, pcToolKey, 2) . 
                InsertTool (?, "ToolAreaLeft":U, pcToolKey, 2) . 
            END.
            WHEN "btnCancel":U THEN DO:
                InsertTool ("Start":U, "Edit":U, pcToolKey, 3) . 
                InsertTool (?, "ToolAreaLeft":U, pcToolKey, 3) . 
            END.
            WHEN "btnDelete":U THEN DO:
                InsertTool ("Start":U, "Edit":U, pcToolKey, 4) .
                InsertTool (?, "ToolAreaLeft":U, pcToolKey, 4) . 
            END.
            WHEN "BtnFirst":U  THEN InsertTool ("Start":U, "Navigation":U, pcToolKey, 1) . 
            WHEN "BtnPrev":U   THEN DO:
                InsertTool ("Start":U, "Navigation":U, pcToolKey, 2) . 
                InsertTool (?, "TabItemToolbar":U, pcToolKey, 2) . 
            END.
            WHEN "BtnNext":U   THEN DO:
                InsertTool ("Start":U, "Navigation":U, pcToolKey, 3) . 
                InsertTool (?, "TabItemToolbar":U, pcToolKey, 3) . 
            END.
            WHEN "BtnLast":U   THEN InsertTool ("Start":U, "Navigation":U, pcToolKey, 4) . 

            WHEN "btnSearch":U THEN InsertTool ("Start":U, "Search":U, pcToolKey, 1) . 
            
            WHEN "m_Orderlines":U       THEN InsertTool ("Options":U, "View":U, pcToolKey, 1) .
            WHEN "m_Customer_Details":U THEN InsertTool ("Options":U, "View":U, pcToolKey, 2) .
            
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Inserts a Tool in the definition                                                                        
        Notes:                                         
        @param pcTabKey The key of the containing Ribbon Tab
        @param pcGroupKey The key of the containing Ribbon Group 
        @param pcToolKey The key of the Tool to create       
        @param piToolSort The sort weight of the tool to create                                
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID InsertTool (pcTabKey  AS CHARACTER,
                                      pcGroupKey AS CHARACTER,
                                      pcToolKey AS CHARACTER,
                                      piToolSort AS INTEGER):

        IF NOT CAN-FIND (ttTabs WHERE ttTabs.TabKey = pcTabKey) THEN 
            InsertTab (pcTabKey) .

        IF NOT CAN-FIND (ttGroups WHERE ttGroups.TabKey   = pcTabKey  
                                    AND ttGroups.GroupKey = pcGroupKey) THEN 
            InsertGroup (pcTabKey, pcGroupKey) .

        CREATE ttTools.
        ASSIGN ttTools.TabKey   = pcTabKey 
               ttTools.GroupKey = pcGroupKey
               ttTools.ToolKey  = pcToolKey
               ttTools.ToolSort = piToolSort . 

    END METHOD . 

END CLASS.
