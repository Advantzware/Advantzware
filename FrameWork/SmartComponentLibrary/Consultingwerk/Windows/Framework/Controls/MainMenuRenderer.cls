/**********************************************************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
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
    File        : MainMenuRenderer
    Purpose     : Renders the Main Menu and Ribbon to be displayed from
                  SmartFramework. The reference implementation can be seen
                  in Consultingwerk.Windows.Framework.Reference.MainMenuForm.
    Syntax      :
    Description :
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Wed May 25 16:52:01 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                            FROM PROPATH .
USING Consultingwerk.Forms.*                      FROM PROPATH .
USING Consultingwerk.Framework.*                  FROM PROPATH .
USING Consultingwerk.Framework.Session.*          FROM PROPATH .
USING Consultingwerk.Util.*                       FROM PROPATH .
USING Consultingwerk.Windows.Framework.*          FROM PROPATH .
USING Consultingwerk.Windows.Framework.Controls.* FROM PROPATH .
USING Infragistics.Win.UltraWinExplorerBar.*      FROM ASSEMBLY .
USING Infragistics.Win.UltraWinToolbars.*         FROM ASSEMBLY .
USING Progress.Lang.*                             FROM PROPATH .
USING System.Windows.Forms.*                      FROM ASSEMBLY .

CLASS Consultingwerk.Windows.Framework.Controls.MainMenuRenderer
    IMPLEMENTS IMainMenuRenderer:

&IF DEFINED (SmartFramework) NE 0 &THEN
    { Consultingwerk/SmartFramework/Authorization/dsMenuGroup.i }
    { Consultingwerk/SmartFramework/Menu/dsMenu.i }
&ENDIF

    DEFINE PRIVATE VARIABLE oForm AS BaseForm NO-UNDO .

    /*------------------------------------------------------------------------------
        Purpose: Comma delimeted list of constant ExplorerBarGroups that do not get
                 removed if the user is not authorized to see them
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ConstantExplorerBarGroups AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Defines sorting of MenuTreeViewControl. When menu should be sorted
                 by alphabet and not by menu structure provider
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY MenuTreeViewControlSortAlphabetical AS LOGICAL INITIAL FALSE NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Indicates if the list of last recently used MenuItems should be rendered
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY RenderRecentGroup AS LOGICAL INITIAL TRUE NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Constructor for the MainMenuRenderer class
     * Notes:
     * @param poForm The BaseForm instance to use to load and store settings
     */
    CONSTRUCTOR PUBLIC MainMenuRenderer (poForm AS BaseForm):

        ASSIGN oForm = poForm .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the MainMenuRenderer class
     * Notes:
     */
    CONSTRUCTOR PUBLIC MainMenuRenderer ():

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Initializes an ExplorerBarGroup with the SmartFramework MenuTreeViewControl
        Notes:
        @param pcGroupKey The key of the Group to initialize
        @param pcGroupTitle The title of the group to initialize
        @param pcRootNodeKey The key of the RootNode for the Menu
        @param pcSmallImageKey The key of the small Group Image
        @param pcLargeImageKey The key of the large Group Image
        @param piPosition The position of the Explorer Bar Group or ?
        @param poUltraExplorerBar The ultraExplorerBar instance that owns the group
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID InitializeExplorerBarGroup (pcGroupKey AS CHARACTER,
                                                      pcGroupTitle AS CHARACTER,
                                                      pcRootNodeKey AS CHARACTER,
                                                      pcSmallImageKey AS CHARACTER,
                                                      pcLargeImageKey AS CHARACTER,
                                                      piPosition AS INTEGER,
                                                      poUltraExplorerBar AS UltraExplorerBar):

        THIS-OBJECT:InitializeExplorerBarGroup (pcGroupKey,
                                                pcGroupTitle,
                                                pcRootNodeKey,
                                                pcSmallImageKey,
                                                pcLargeImageKey,
                                                piPosition,
                                                TRUE,
                                                poUltraExplorerBar) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Initializes an ExplorerBarGroup with the SmartFramework MenuTreeViewControl
        Notes:
        @param pcGroupKey The key of the Group to initialize
        @param pcGroupTitle The title of the group to initialize
        @param pcRootNodeKey The key of the RootNode for the Menu
        @param pcSmallImageKey The key of the small Group Image
        @param pcLargeImageKey The key of the large Group Image
        @param piPosition The position of the Explorer Bar Group or ?
        @param plRenderRootNode Logical value indicating if the RootNode should be rendered
        @param poUltraExplorerBar The ultraExplorerBar instance that owns the group
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID InitializeExplorerBarGroup (pcGroupKey AS CHARACTER,
                                                      pcGroupTitle AS CHARACTER,
                                                      pcRootNodeKey AS CHARACTER,
                                                      pcSmallImageKey AS CHARACTER,
                                                      pcLargeImageKey AS CHARACTER,
                                                      piPosition AS INTEGER,
                                                      plRenderRootNode AS LOGICAL,
                                                      poUltraExplorerBar AS UltraExplorerBar):

        DEFINE VARIABLE oMenuTreeViewControl AS MenuTreeViewControl   NO-UNDO.
        DEFINE VARIABLE oGroup               AS UltraExplorerBarGroup NO-UNDO .
        DEFINE VARIABLE oImageProvider       AS IImageProvider        NO-UNDO .

        oMenuTreeViewControl = NEW MenuTreeViewControl ().

        ASSIGN
            oMenuTreeViewControl:BackColor      = System.Drawing.Color:White
            oMenuTreeViewControl:Form           = oForm
            oMenuTreeViewControl:RootNodeKey    = pcRootNodeKey
            oMenuTreeViewControl:RenderRootNode = plRenderRootNode
            oMenuTreeViewControl:SettingsKey    = pcRootNodeKey .

        /* Overwrite the menu structure provider sorting with alphabetical order */
        oMenuTreeViewControl:Sorted = THIS-OBJECT:MenuTreeViewControlSortAlphabetical.

        IF piPosition = ? THEN
            oGroup = poUltraExplorerBar:Groups:Add (pcGroupKey, pcGroupTitle) .
        ELSE
            oGroup = poUltraExplorerBar:Groups:Insert (piPosition, pcGroupKey, pcGroupTitle) .

        oGroup:Settings:Style = GroupStyle:ControlContainer .
        oGroup:Container:Controls:Add (oMenuTreeViewControl) .
        oMenuTreeViewControl:Dock = DockStyle:Fill.

        oImageProvider = {Consultingwerk/get-service.i Consultingwerk.Windows.Framework.IImageProvider} .

        IF VALID-OBJECT (oImageProvider) THEN DO:
            oGroup:Settings:AppearancesSmall:HeaderAppearance:Image = oImageProvider:ImageForKey
                        (pcSmallImageKey) .
            oGroup:Settings:AppearancesLarge:HeaderAppearance:Image = oImageProvider:ImageForKey
                        (pcLargeImageKey) .
        END.

    END METHOD .

&IF DEFINED (SmartFramework) NE 0 &THEN
    /*------------------------------------------------------------------------------
        Purpose: Initializes an ExplorerBarGroup with the native Explorer Bar Items
        Notes:
        @param pcGroupKey The key of the Group to initialize
        @param pcGroupTitle The title of the group to initialize
        @param pcRootNodeKey The key of the RootNode for the Menu
        @param pcSmallImageKey The key of the small Group Image
        @param pcLargeImageKey The key of the large Group Image
        @param piPosition The position of the Explorer Bar Group or ?
        @param poUltraExplorerBar The ultraExplorerBar instance that owns the group
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID InitializeNativeExplorerBarGroup (pcGroupKey AS CHARACTER,
                                                            pcGroupTitle AS CHARACTER,
                                                            pcRootNodeKey AS CHARACTER,
                                                            pcSmallImageKey AS CHARACTER,
                                                            pcLargeImageKey AS CHARACTER,
                                                            piPosition AS INTEGER,
                                                            poUltraExplorerBar AS UltraExplorerBar):

        DEFINE VARIABLE oController AS MenuStructureExplorerBarGroupController NO-UNDO .

        oController = NEW MenuStructureExplorerBarGroupController (poUltraExplorerBar,
                                                                   THIS-OBJECT,
                                                                   pcGroupKey,
                                                                   pcGroupTitle,
                                                                   pcRootNodeKey,
                                                                   pcSmallImageKey,
                                                                   pcLargeImageKey,
                                                                   piPosition) .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Initializes an ExplorerBarGroup with recently used menu items
        Notes:
        @param pcGroupKey The key of the Group to initialize
        @param pcGroupTitle The title of the group to initialize
        @param pcSmallImageKey The key of the small Group Image
        @param pcLargeImageKey The key of the large Group Image
        @param piPosition The position of the Explorer Bar Group or ?
        @param poUltraExplorerBar The ultraExplorerBar instance that owns the group
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID InitializeRecentlyUsedMenuItemsGroup (pcGroupKey AS CHARACTER,
                                                                pcGroupTitle AS CHARACTER,
                                                                pcSmallImageKey AS CHARACTER,
                                                                pcLargeImageKey AS CHARACTER,
                                                                piPosition AS INTEGER,
                                                                poUltraExplorerBar AS UltraExplorerBar):

        DEFINE VARIABLE oController AS LastRecentlyUsedExplorerBarGroupController NO-UNDO .

        oController = NEW LastRecentlyUsedExplorerBarGroupController (poUltraExplorerBar,
                                                                      THIS-OBJECT,
                                                                      pcGroupKey,
                                                                      pcGroupTitle,
                                                                      pcSmallImageKey,
                                                                      pcLargeImageKey,
                                                                      piPosition) .
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Initializes a MenuStructureRibbonTabController instance
        Notes:
        @param pcMenuStructureGuid The Guid of the Menu Structure to render
        @param poUltraToolbarsManager The UltraToolbarsManager instance that owns the RibonTab
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID InitializeRibbonTab (pcMenuStructureGuid AS CHARACTER,
                                               poUltraToolbarsManager AS UltraToolbarsManager):

        DEFINE VARIABLE oController AS MenuStructureRibbonTabController NO-UNDO .

        oController = NEW MenuStructureRibbonTabController (poUltraToolbarsManager,
                                                            THIS-OBJECT,
                                                            pcMenuStructureGuid) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Initializes an ExplorerBarGroup with the SmartFramework MenuTaskListControl
        Notes:
        @param pcGroupKey The key of the Group to initialize
        @param pcGroupTitle The title of the group to initialize
        @param pcRootNodeKey The key of the RootNode for the Menu
        @param pcSmallImageKey The key of the small Group Image
        @param pcLargeImageKey The key of the large Group Image
        @param piPosition The position of the Explorer Bar Group or ?
        @param poUltraExplorerBar The ultraExplorerBar instance that owns the group
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID InitializeTaskList (pcGroupKey AS CHARACTER,
                                              pcGroupTitle AS CHARACTER,
                                              pcRootNodeKey AS CHARACTER,
                                              pcSmallImageKey AS CHARACTER,
                                              pcLargeImageKey AS CHARACTER,
                                              piPosition AS INTEGER,
                                              poUltraExplorerBar AS UltraExplorerBar):

        DEFINE VARIABLE oMenuTaskListControl AS MenuTaskListControl   NO-UNDO.
        DEFINE VARIABLE oGroup               AS UltraExplorerBarGroup NO-UNDO .
        DEFINE VARIABLE oImageProvider       AS IImageProvider        NO-UNDO .

        oMenuTaskListControl = NEW MenuTaskListControl ().

        oMenuTaskListControl:BackColor = System.Drawing.Color:White.
        oMenuTaskListControl:RootNodeKey = pcRootNodeKey .

        IF piPosition = ? THEN
            oGroup = poUltraExplorerBar:Groups:Add (pcGroupKey, pcGroupTitle) .
        ELSE
            oGroup = poUltraExplorerBar:Groups:Insert (piPosition, pcGroupKey, pcGroupTitle) .

        oGroup:Settings:Style = GroupStyle:ControlContainer .
        oGroup:Container:Controls:Add (oMenuTaskListControl) .
        oMenuTaskListControl:Dock = DockStyle:Fill.

        oImageProvider = {Consultingwerk/get-service.i Consultingwerk.Windows.Framework.IImageProvider} .

        IF VALID-OBJECT (oImageProvider) THEN DO:
            oGroup:Settings:AppearancesSmall:HeaderAppearance:Image = oImageProvider:ImageForKey
                        (pcSmallImageKey) .
            oGroup:Settings:AppearancesLarge:HeaderAppearance:Image = oImageProvider:ImageForKey
                        (pcLargeImageKey) .
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Cleanup existing groups if the user is no longer authorized
        Notes:
        @param poUltraExplorerBar The ultraExplorerBar instance that owns the group
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID RemoveGroupsIfNotAuthorized (poUltraExplorerBar AS UltraExplorerBar):

        {Consultingwerk/foreach.i UltraExplorerBarGroup oGroup in poUltraExplorerBar:Groups '' groupLoop}
            /* Skip fixed ExplorerBarGroups */
            IF (oGroup:Key = "Recent":U AND
                THIS-OBJECT:RenderRecentGroup) OR
                CAN-DO (THIS-OBJECT:ConstantExplorerBarGroups, oGroup:Key) THEN
                NEXT groupLoop .

            FIND FIRST eSmartMenu WHERE eSmartMenu.MenuGuid = oGroup:Key NO-LOCK NO-ERROR.

            IF NOT AVAILABLE eSmartMenu THEN
                poUltraExplorerBar:Groups:Remove (oGroup).
        END.

    END METHOD .
&ENDIF

    /*------------------------------------------------------------------------------
        Purpose: Removes an existing UltraExplorerBarGroup to be enable reloading it.
                 The old index or of the group or the current index value to be added
                 is returned, to have the group at the same position again.
        Notes:
        @param pcMenuGuid The key of the group to be removed
        @param poUltraExplorerBar The ultraExplorerBar instance that owns the group
        @param piIndex The current index when adding new groups to the UltraExplorerBar
        @return The index to insert the group to
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED INTEGER RemoveGroupToReload (pcMenuGuid AS CHARACTER,
                                                  poUltraExplorerBar AS UltraExplorerBar,
                                                  piIndex AS INTEGER):

        DEFINE VARIABLE oGroup    AS UltraExplorerBarGroup NO-UNDO .
        DEFINE VARIABLE iPosition AS INTEGER               NO-UNDO .

        IF poUltraExplorerBar:Groups:Exists (pcMenuGuid) THEN DO:
           oGroup = poUltraExplorerBar:Groups[pcMenuGuid].
           iPosition = oGroup:Index.
           poUltraExplorerBar:Groups:Remove(oGroup).
        END.
        ELSE
            iPosition = piIndex.

        RETURN iPosition.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Render or refreshes the SmartFramework Menu displayed in an
                 ultraExplorerBar and ultraToolbarsManager instance
        Notes:
        @param poUltraExplorerBar The ultraExplorerBar instance that owns the group
        @param poUltraToolbarsManager The UltraToolbarsManager instance that owns the RibonTab
        @param poForm The form instance the menu is rendered to.
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RenderMenuStructure (poUltraExplorerBar AS UltraExplorerBar,
                                            poUltraToolbarsManager AS UltraToolbarsManager,
                                            poForm AS Form):

        DEFINE VARIABLE i         AS INTEGER               NO-UNDO INIT 0.
        DEFINE VARIABLE iPosition AS INTEGER               NO-UNDO .
        DEFINE VARIABLE oGroup    AS UltraExplorerBarGroup NO-UNDO .

&IF DEFINED (SmartFramework) NE 0 &THEN
        poUltraExplorerBar:SuspendLayout() .

        Consultingwerk.Windows.API.Win32:LockWindowUpdate (poForm) .

        IF THIS-OBJECT:RenderRecentGroup THEN DO:
            IF NOT poUltraExplorerBar:Groups:Exists("Recent":U) THEN
                THIS-OBJECT:InitializeRecentlyUsedMenuItemsGroup ("Recent":U,
                                                                  "Recent"{&TRAN},
                                                                  "Consultingwerk/Windows/Framework/Images/step_16.png":U,
                                                                  "Consultingwerk/Windows/Framework/Images/step_32.png":U,
                                                                  i,
                                                                  poUltraExplorerBar) .

            i = i + 1 .
        END.

        DEFINE VARIABLE oGroups AS CharacterHolder NO-UNDO .

        IF SessionManager:UserGroupKeys > "":U THEN DO:
            oGroups = NEW CharacterHolder (SessionManager:UserGroupKeys) .

            FrameworkSettings:ServiceAdapter:InvokeMethod ("":U,
                                                           "Consultingwerk.SmartFramework.Menu.MenuBusinessEntity":U,
                                                           "GetMenusForGroups":U,
                                                           INPUT-OUTPUT DATASET dsMenu,
                                                           oGroups) .

            RemoveGroupsIfNotAuthorized (poUltraExplorerBar).

            FOR EACH eSmartMenu WHERE eSmartMenu.MenuStyleCode = "":U
                                   OR eSmartMenu.MenuStyleCode = "TREE":U
                                   OR eSmartMenu.MenuStyleCode = "EXPLORERBAR":U
                                   OR eSmartMenu.MenuStyleCode = "TASKLIST":U
                                BY eSmartMenu.MenuSequence ON ERROR UNDO, THROW:

                CASE eSmartMenu.MenuStyleCode :
                    WHEN "":U OR
                    WHEN "TREE":U THEN DO:
                        IF NOT poUltraExplorerBar:Groups:Exists (eSmartMenu.MenuGuid) THEN DO:
                            THIS-OBJECT:InitializeExplorerBarGroup (eSmartMenu.MenuGuid,
                                                                    eSmartMenu.MenuName,
                                                                    eSmartMenu.MenuGuid,
                                                                    eSmartMenu.MenuSmallImage,
                                                                    eSmartMenu.MenuLargeImage,
                                                                    i,
                                                                    FALSE,
                                                                    poUltraExplorerBar) .
                        END.
                        ELSE DO:
                            oGroup = poUltraExplorerBar:Groups[eSmartMenu.MenuGuid].

                            IF VALID-OBJECT (oGroup:Container) AND
                               oGroup:Container:Controls:Count > 0 AND
                               TYPE-OF (oGroup:Container:Controls[0],
                                        MenuTreeViewControl) THEN DO ON ERROR UNDO, THROW:

                                    CAST (oGroup:Container:Controls[0],
                                          MenuTreeViewControl):LoadMenu () .

                                CATCH ple AS Progress.Lang.Error :
                                    ErrorHelper:ShowErrorMessage (ple).
                                END CATCH.
                            END.
                        END.
                    END.
                    WHEN "EXPLORERBAR":U THEN DO:
                        iPosition = RemoveGroupToReload (eSmartMenu.MenuGuid,
                                                         poUltraExplorerBar,
                                                         i).

                        THIS-OBJECT:InitializeNativeExplorerBarGroup (eSmartMenu.MenuGuid,
                                                                      eSmartMenu.MenuName,
                                                                      eSmartMenu.MenuGuid,
                                                                      eSmartMenu.MenuSmallImage,
                                                                      eSmartMenu.MenuLargeImage,
                                                                      i,
                                                                      poUltraExplorerBar) .
                    END.
                    OTHERWISE DO:
                        iPosition = RemoveGroupToReload (eSmartMenu.MenuGuid,
                                                         poUltraExplorerBar,
                                                         i).

                        THIS-OBJECT:InitializeTaskList (eSmartMenu.MenuGuid,
                                                        eSmartMenu.MenuName,
                                                        eSmartMenu.MenuGuid,
                                                        eSmartMenu.MenuSmallImage,
                                                        eSmartMenu.MenuLargeImage,
                                                        i,
                                                        poUltraExplorerBar) .
                    END.
                END CASE.

                i = i + 1 .
            END.

            FOR EACH eSmartMenu WHERE eSmartMenu.MenuStyleCode = "RIBBON":U
                    BY eSmartMenu.MenuSequence ON ERROR UNDO, THROW:

                THIS-OBJECT:InitializeRibbonTab (eSmartMenu.MenuGuid,
                                                 poUltraToolbarsManager) .

            END.

        END.

        Consultingwerk.Windows.API.Win32:LockWindowUpdate (0) .

        poUltraExplorerBar:ResumeLayout(TRUE) .
&ENDIF

    END METHOD .

END CLASS.
