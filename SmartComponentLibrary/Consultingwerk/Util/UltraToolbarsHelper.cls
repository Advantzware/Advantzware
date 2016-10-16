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
    File        : UltraToolbarsHelper
    Purpose     :
    Syntax      : Static methods only, Protected constructor to disallow
                  instance creation
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Dec 08 11:38:42 CET 2008
    Notes       : This version of the UltraToolbarsHelper is dependent
                  on the very generic Interface type
                  Consultingwerk.Framework.IFormWithUltraToolbarsManager

                  This file is intended for GUI for .NET only
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW .

{ Consultingwerk/products.i }

USING Consultingwerk.*                      FROM PROPATH .
USING Consultingwerk.Assertion.*            FROM PROPATH .
USING Consultingwerk.Framework.*            FROM PROPATH .
USING Consultingwerk.Util.*                 FROM PROPATH .
&IF DEFINED (WinKit) NE 0 &THEN
USING Consultingwerk.WindowIntegrationKit.* FROM PROPATH .
&ENDIF
&IF DEFINED (DotNetAccessible) NE 0 &THEN
USING Infragistics.Win.UltraWinToolbars.*   FROM ASSEMBLY .
USING Progress.Lang.*                       FROM ASSEMBLY .
&ENDIF

CLASS Consultingwerk.Util.UltraToolbarsHelper:

    {Consultingwerk/Util/TempTables/ttWidgetsByLocation.i}

&IF DEFINED (WinKit) NE 0 AND DEFINED (DotNetAccessible) NE 0 &THEN
    DEFINE PRIVATE STATIC VARIABLE oToolImageProvider                AS Consultingwerk.WindowIntegrationKit.IToolImageProvider               NO-UNDO .
    DEFINE PRIVATE STATIC VARIABLE oSubmenuCustomizer                AS Consultingwerk.WindowIntegrationKit.ISubmenuCustomizer               NO-UNDO .
    DEFINE PRIVATE STATIC VARIABLE oCustomWidgetRenderer             AS Consultingwerk.WindowIntegrationKit.ICustomToolbarWidgetRenderer     NO-UNDO .
    DEFINE PRIVATE STATIC VARIABLE oRibbonButtonCustomizer           AS Consultingwerk.WindowIntegrationKit.IRibbonButtonCustomizer          NO-UNDO .
    DEFINE PRIVATE STATIC VARIABLE oRefreshToolsFilter               AS Consultingwerk.WindowIntegrationKit.IRefreshToolsFilter              NO-UNDO .
    DEFINE PRIVATE STATIC VARIABLE oMenuItemFilter                   AS Consultingwerk.WindowIntegrationKit.IMenuItemFilter                  NO-UNDO .
    DEFINE PRIVATE STATIC VARIABLE oViewHideToolbarButtonsFilter     AS Consultingwerk.WindowIntegrationKit.IViewHideToolbarButtonsFilter    NO-UNDO .
    DEFINE PRIVATE STATIC VARIABLE oLinkButtonsAndToolsFilter        AS Consultingwerk.WindowIntegrationKit.ILinkButtonsAndToolsFilter       NO-UNDO .
    DEFINE PRIVATE STATIC VARIABLE oHideToolbarButtonsFilter         AS Consultingwerk.WindowIntegrationKit.IHideToolbarButtonsFilter        NO-UNDO .
    DEFINE PRIVATE STATIC VARIABLE oEnableToolsLikeButtonsFilter     AS Consultingwerk.WindowIntegrationKit.IEnableToolsLikeButtonsFilter    NO-UNDO .
    DEFINE PRIVATE STATIC VARIABLE oBuildRibbonGroupFromFrameFilter  AS Consultingwerk.WindowIntegrationKit.IBuildRibbonGroupFromFrameFilter NO-UNDO .
    DEFINE PRIVATE STATIC VARIABLE oBuildToolbarFromFrameFilter      AS Consultingwerk.WindowIntegrationKit.IBuildToolbarFromFrameFilter     NO-UNDO .
    DEFINE PRIVATE STATIC VARIABLE oChangeRibbonToolCaptionFilter    AS Consultingwerk.WindowIntegrationKit.IChangeRibbonToolCaptionFilter   NO-UNDO .
&ENDIF

    /**
     * Purpose: Initializes the static instance of the UltraToolbarsManager
     * Notes:
     */
    CONSTRUCTOR STATIC UltraToolbarsHelper ():

&IF DEFINED (WinKit) NE 0 AND DEFINED (DotNetAccessible) NE 0 &THEN
        UltraToolbarsHelper:EvaluateCustomToolImageProvider () .
        UltraToolbarsHelper:EvaluateSubmenuCustomizer () .
        UltraToolbarsHelper:EvaluateCustomWidgetRenderer () .
        UltraToolbarsHelper:EvaluateRibbonButtonCustomizer () .
        UltraToolbarsHelper:EvaluateRefreshToolsFilter () .
        UltraToolbarsHelper:EvaluateMenuItemFilter () .
        UltraToolbarsHelper:EvaluateViewHideToolbarButtonsFilter () .
        UltraToolbarsHelper:EvaluateLinkButtonsAndToolsFilter () .
        UltraToolbarsHelper:EvaluateHideToolbarButtonsFilter () .
        UltraToolbarsHelper:EvaluateEnableToolsLikeButtonsFilter () .
        UltraToolbarsHelper:EvaluateBuildRibbonGroupFromFrameFilter () .
        UltraToolbarsHelper:EvaluateBuildToolbarFromFrameFilter () .
&ENDIF
    END CONSTRUCTOR.

    /**
     * Purpose: Disallow instance creation
     * Notes:
     */
    CONSTRUCTOR PROTECTED UltraToolbarsHelper ():
        SUPER ().

    END CONSTRUCTOR.

&IF DEFINED (DotNetAccessible) NE 0 &THEN

    /**
     * Purpose: Gets and sets the name of Menu items in a menu structure that
     *          should be rendered as the ApplicationMenuFooter Tools
     * Notes:
     */
    DEFINE PUBLIC STATIC PROPERTY ApplicationMenuFooterTools AS CHARACTER NO-UNDO
        INIT "m_Ende":U
    GET.
    SET.

    /**
     * Purpose: Optionally prefix ButtonTools generated for Menu-Items with the
     *          Submenu's Name to ensure uniqueness
     * Notes:
     */
    DEFINE PUBLIC STATIC PROPERTY PrefixMenuItemsWithMenuName AS LOGICAL INITIAL FALSE NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Indicates that we are currently refreshing tools and that some changed
     *          events do not need to be propagated to the ABL widgets.
     * Notes:
     */
    DEFINE PUBLIC STATIC PROPERTY RefreshingToolbar AS LOGICAL INITIAL FALSE NO-UNDO
    GET.
    PRIVATE SET.

    /**
     * Purpose: Returns the maximum between two ABL BUTTON Widgets before the routine
     *          BuildToolbarFromFrame starts a new Group of ButtonTool's
     * Notes:
     */
    DEFINE PUBLIC STATIC PROPERTY ToolbarButtonMaxDistance AS DECIMAL INITIAL 0 NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Activates the first RibbonTab of the current MDI Child
     * Notes:   Leverages the MdiMerging capabilities of the Infragistics ribbon
     * @param poParentToolbarsManager The UltraToolbarsManager of the MDI Container
     */
    METHOD PUBLIC STATIC VOID ActivateFirstMdiChildRibbonTab (poParentToolbarsManager AS UltraToolbarsManager):

        IF NOT VALID-OBJECT (poParentToolbarsManager:ActiveMdiChildManager) THEN
            RETURN .

        IF poParentToolbarsManager:ActiveMdiChildManager:Ribbon:Tabs:Count > 0 THEN .
        ELSE RETURN .

        poParentToolbarsManager:Ribbon:SelectedTab = poParentToolbarsManager:ActiveMdiChildManager:Ribbon:Tabs[0]:AttachedParentTab .

    END METHOD .

    /**
     * Purpose: Renders the Tools listed in the ApplicationMenuFooterTools property
     *          in the Ribbon's ApplicationMenu:FooterToolbar
     * Notes:
     * @param poForm The Form owning the UltraToolbarsManager that contains the Tools
     */
    METHOD PUBLIC STATIC VOID BuildApplicationFooterMenu (poForm AS IFormWithUltraToolbarsManager):

        DEFINE VARIABLE i            AS INTEGER     NO-UNDO .
        DEFINE VARIABLE oToolItemRef AS ToolBase    NO-UNDO .

        /* Mike Fechner, Consultingwerk Ltd. 15.12.2009
           Add selected Tools to the Application menu footer */
        DO i = 1 TO NUM-ENTRIES (UltraToolbarsHelper:ApplicationMenuFooterTools):
            IF poForm:ToolbarsManager:Tools:Exists (ENTRY (i, UltraToolbarsHelper:ApplicationMenuFooterTools)) THEN DO:
                oToolItemRef = NEW ButtonTool (ENTRY (i, UltraToolbarsHelper:ApplicationMenuFooterTools)) .
                oToolItemRef:InstanceProps:DisplayStyle = ToolDisplayStyle:ImageAndText .

                poForm:ToolbarsManager:Ribbon:ApplicationMenu:FooterToolbar:Tools:Add (oToolItemRef) .
            END.
        END.

    END METHOD.

    /**
     * Purpose: Renders the context menu of an ABL Widget on an Infragistics
     *          UltraToolbarsManager and registers if for the given .NET Control
     * Notes:   Silently exits, when there is not POPUP-MENU defined for the Widget
     * @param poToolbarsForm The Infragistics IFormWithUltraToolbarsManager instance that contains the UltraToolbarsManager
     * @param phWidget The ABL Widget that ownes the ABL POPUP-MENU
     * @param poControl The .NET Control that should receive the Infragistics Context Menu
     */
    METHOD PUBLIC STATIC VOID BuildContextMenu (poToolbarsForm AS IFormWithUltraToolbarsManager,
                                                phWidget AS HANDLE,
                                                poControl AS System.Windows.Forms.Control):

        DEFINE VARIABLE cContextMenuKey  AS CHARACTER            NO-UNDO.
        DEFINE VARIABLE oPopupMenu       AS PopupMenuTool        NO-UNDO .
        DEFINE VARIABLE oToolbarsManager AS UltraToolbarsManager NO-UNDO .

        {Consultingwerk/Assertion/HandleAssert/ValidHandle.i phWidget """WIDGET"":U"} .

        IF NOT VALID-HANDLE (phWidget:POPUP-MENU) THEN
            RETURN.

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poToolbarsForm """ToolbarsForm"":U"} .
        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poToolbarsForm:ToolbarsManager """UltraToolbarsManager"":U"} .

        ASSIGN oToolbarsManager =  poToolbarsForm:ToolbarsManager
               cContextMenuKey  = SUBSTITUTE ("context-menu-&1":U,
                                             STRING (phWidget:POPUP-MENU)).

        IF oToolbarsManager:Tools:Exists (cContextMenuKey) THEN
            ASSIGN oPopupMenu = CAST (oToolbarsManager:Tools[cContextMenuKey],
                                      PopupMenuTool) .
        ELSE DO:
            oPopupMenu = NEW PopupMenuTool (cContextMenuKey) .
            oPopupMenu:Tag = STRING (phWidget:POPUP-MENU).
            oPopupMenu:SharedProps:Caption = "Context Menu":U .
            oToolbarsManager:Tools:Add (oPopupMenu) .
        END.

        Consultingwerk.Util.UltraToolbarsHelper:BuildSubMenuItems (phWidget:POPUP-MENU,
                                                                   oPopupMenu,
                                                                   poToolbarsForm) .

        oToolbarsManager:SetContextMenuUltra (poControl, cContextMenuKey) .

    END METHOD.

    /**
     * Purpose: Builds a Context Menu (PopupMenuTool) structure with the menu structure
     *          of an ABL menu-bar
     * Notes:
     * @param poForm The Form owning the UltraToolbarsManager that contains the Ribbon
     * @param poPopupMenuRoot The reference to the PopupMenuTool that should received the menustructure
     */
    METHOD PUBLIC STATIC VOID BuildContextMenuFromMenuBar (poForm AS IFormWithUltraToolbarsManager,
                                                           poPopupMenuRoot AS PopupMenuTool):

        UltraToolbarsHelper:PopulateToolsCollectionFromMenuBar (poForm, poPopupMenuRoot:Tools) .

    END METHOD.

    /**
     * Purpose: Builds a Ribbon ApplicationMenu structure with the structure
     *          of an ABL menu-bar
     * Notes:
     * @param poForm The Form owning the UltraToolbarsManager that contains the Ribbon
     */
    METHOD PUBLIC STATIC VOID BuildRibbonApplicationMenu (poForm AS IFormWithUltraToolbarsManager):

        UltraToolbarsHelper:PopulateToolsCollectionFromMenuBar (poForm,
                                                                poForm:ToolbarsManager:Ribbon:ApplicationMenu:ToolAreaLeft:Tools) .

    END METHOD.

    /**
     * Purpose: Renders an ABL MENU-BAR structure as a Ribbon
     * Notes:   The Handle to the MENU-BAR needs to be accessible from the poForm:MENU-BAR reference
     *          First sub menu will be rendered in ApplicationMenu (Left)
     *          Additional sub menus will be rendered as RibbonTabs
     * @param poForm The Form owning the UltraToolbarsManager that contains the Ribbon
     */
    METHOD PUBLIC STATIC VOID BuildRibbonFromMenuBar (poForm AS IFormWithUltraToolbarsManager):

        DEFINE VARIABLE oRibbonTab     AS RibbonTab     NO-UNDO .
        DEFINE VARIABLE oRibbonGroup   AS RibbonGroup   NO-UNDO .
        DEFINE VARIABLE hMenuBar       AS HANDLE        NO-UNDO .
        DEFINE VARIABLE hMenuItem      AS HANDLE        NO-UNDO .
        DEFINE VARIABLE oToolItem      AS ToolBase      NO-UNDO .

        DEFINE VARIABLE oPopupMenuItem AS PopupMenuTool NO-UNDO .

        DEFINE VARIABLE lRulePending   AS LOGICAL       NO-UNDO .

        ASSIGN hMenuBar  = poForm:MENU-BAR
               hMenuItem = hMenuBar:FIRST-CHILD .

        /* First sub menu will be rendered in ApplicationMenu (Left) */
        BuildToolsCollection (hMenuItem,
                              poForm:ToolbarsManager:Ribbon:ApplicationMenu:ToolAreaLeft:Tools,
                              poForm) .

        /* next sub menus will be rendered as RibbonTabs */
        ASSIGN hMenuItem = hMenuItem:NEXT-SIBLING .

        /* Build a single Ribbon Tab and a single RibbonGroup for the menu-bar */

        oRibbonTab = NEW RibbonTab ("MenuRibbonTab":U, "Menu"{&TRAN}) .

        poForm:ToolbarsManager:Ribbon:Tabs:Add (oRibbonTab) .

        /* Start with first RibbonGroup */
        oRibbonGroup = NEW RibbonGroup ("MenuRibbonGroup":U, "Menu"{&TRAN}) .

        oRibbonTab:Groups:Add (oRibbonGroup) .

        DO WHILE VALID-HANDLE(hMenuItem):
            CASE hMenuItem:TYPE:
                WHEN "MENU-ITEM":U THEN DO:
                    CASE hMenuItem:SUBTYPE:
                        WHEN "NORMAL":U THEN DO:
                            oToolItem = CreateMenuItemButtonTool (hMenuItem, oRibbonGroup:Tools, poForm) .

                            IF lRulePending THEN
                                ASSIGN oRibbonGroup:Tools[oToolItem:Key]:InstanceProps:IsFirstInGroup = TRUE
                                       lRulePending = FALSE .
                        END.
                        WHEN "RULE":U THEN
                            ASSIGN lRulePending = TRUE .
                    END CASE .
                END.
                WHEN "SUB-MENU":U THEN DO:
                    oPopupMenuItem = CreateSubMenuPopupMenu (hMenuItem, oRibbonGroup:Tools, poForm) .

                    BuildSubMenuItems (hMenuItem, oPopupMenuItem, poForm) .

                    /* PreferredSizeOnRibbon: Large */
                    oRibbonGroup:Tools[oPopupMenuItem:Key]:InstanceProps:PreferredSizeOnRibbon = RibbonToolSize:LARGE .
                END.
            END CASE .

            hMenuItem = hMenuItem:NEXT-SIBLING .
        END.

        UltraToolbarsHelper:BuildApplicationFooterMenu (poForm) .

    END METHOD .

    /**
     * Purpose: Builds a RibbonGroup for BUTTON widgets in an ABL FRAME
     * Notes:
     * @param phFrame The HANDLE of the ABL FRAME WIDGET containing the BUTTON widgets
     * @param poToolbarsManager The reference to the UltraToolbarsManager that contains the Ribbon
     * @param poRibbonTab The reference to the RibbonTab that should contain the new RibbonGroup
     * @param pcRibbonGroupKey The key for the new RibbonTab object
     * @param pcRibbonGroupCaption The caption of the new RibbonTab object
     * @param plSkipExisting Flag indicating if Tools already available in the ToolbarsManager should be skipped
     * @param plHideButtons Flag indicating if the BUTTON widgets should be hidden
     */
    METHOD PUBLIC STATIC VOID BuildRibbonGroupFromFrame (phFrame AS HANDLE,
                                                         poToolbarsManager AS UltraToolbarsManager,
                                                         poRibbonTab AS RibbonTab,
                                                         pcRibbonGroupKey AS CHARACTER,
                                                         pcRibbonGroupCaption AS CHARACTER,
                                                         plSkipExisting AS LOGICAL,
                                                         plHideButtons AS LOGICAL):

        UltraToolbarsHelper:BuildRibbonGroupFromFrame(phFrame,
                                                      poToolbarsManager,
                                                      poRibbonTab,
                                                      pcRibbonGroupKey,
                                                      pcRibbonGroupCaption,
                                                      plSkipExisting,
                                                      plHideButtons,
                                                      FALSE,
                                                      0,
                                                      "":U,
                                                      "":U) .
    END METHOD .

    /**
     * Purpose: Builds a RibbonGroup for BUTTON widgets in an ABL FRAME
     * Notes:
     * @param phFrame The HANDLE of the ABL FRAME WIDGET containing the BUTTON widgets
     * @param poToolbarsManager The reference to the UltraToolbarsManager that contains the Ribbon
     * @param poRibbonTab The reference to the RibbonTab that should contain the new RibbonGroup
     * @param pcRibbonGroupKey The key for the new RibbonTab object
     * @param pcRibbonGroupCaption The caption of the new RibbonTab object
     * @param plSkipExisting Flag indicating if Tools already available in the ToolbarsManager should be skipped
     * @param plHideButtons Flag indicating if the BUTTON widgets should be hidden
     * @param plSkipInvisibleButtons Skip Buttons that are not visible right now
     */
    METHOD PUBLIC STATIC VOID BuildRibbonGroupFromFrame (phFrame AS HANDLE,
                                                         poToolbarsManager AS UltraToolbarsManager,
                                                         poRibbonTab AS RibbonTab,
                                                         pcRibbonGroupKey AS CHARACTER,
                                                         pcRibbonGroupCaption AS CHARACTER,
                                                         plSkipExisting AS LOGICAL,
                                                         plHideButtons AS LOGICAL,
                                                         plSkipInvisibleButtons AS LOGICAL):

        UltraToolbarsHelper:BuildRibbonGroupFromFrame(phFrame,
                                                      poToolbarsManager,
                                                      poRibbonTab,
                                                      pcRibbonGroupKey,
                                                      pcRibbonGroupCaption,
                                                      plSkipExisting,
                                                      plHideButtons,
                                                      plSkipInvisibleButtons,
                                                      0,
                                                      "":U,
                                                      "":U) .
    END METHOD .

    /**
     * Purpose: Builds a RibbonGroup for BUTTON widgets in an ABL FRAME
     * Notes:
     * @param phFrame The HANDLE of the ABL FRAME WIDGET containing the BUTTON widgets
     * @param poToolbarsManager The reference to the UltraToolbarsManager that contains the Ribbon
     * @param poRibbonTab The reference to the RibbonTab that should contain the new RibbonGroup
     * @param pcRibbonGroupKey The key for the new RibbonTab object
     * @param pcRibbonGroupCaption The caption of the new RibbonTab object
     * @param plSkipExisting Flag indicating if Tools already available in the ToolbarsManager should be skipped
     * @param plHideButtons Flag indicating if the BUTTON widgets should be hidden
     * @param pdeLimitRow The topmost Row (in ABL character units) for BUTTON widgets that should be renders in the RibbonGroup
     * @return The RibbonGroup that was created for the Buttons in the FRAME
     */
    METHOD PUBLIC STATIC RibbonGroup BuildRibbonGroupFromFrame (phFrame AS HANDLE,
                                                                poToolbarsManager AS UltraToolbarsManager,
                                                                poRibbonTab AS RibbonTab,
                                                                pcRibbonGroupKey AS CHARACTER,
                                                                pcRibbonGroupCaption AS CHARACTER,
                                                                plSkipExisting AS LOGICAL,
                                                                plHideButtons AS LOGICAL,
                                                                pdeLimitRow AS DECIMAL):

        RETURN UltraToolbarsHelper:BuildRibbonGroupFromFrame(phFrame,
                                                             poToolbarsManager,
                                                             poRibbonTab,
                                                             pcRibbonGroupKey,
                                                             pcRibbonGroupCaption,
                                                             plSkipExisting,
                                                             plHideButtons,
                                                             FALSE,
                                                             pdeLimitRow,
                                                             "":U,
                                                             "":U) .
    END METHOD .

    /**
     * Purpose: Builds a RibbonGroup for BUTTON widgets in an ABL FRAME
     * Notes:
     * @param phFrame The HANDLE of the ABL FRAME WIDGET containing the BUTTON widgets
     * @param poToolbarsManager The reference to the UltraToolbarsManager that contains the Ribbon
     * @param poRibbonTab The reference to the RibbonTab that should contain the new RibbonGroup
     * @param pcRibbonGroupKey The key for the new RibbonTab object
     * @param pcRibbonGroupCaption The caption of the new RibbonTab object
     * @param plSkipExisting Flag indicating if Tools already available in the ToolbarsManager should be skipped
     * @param plHideButtons Flag indicating if the BUTTON widgets should be hidden
     * @param plSkipInvisibleButtons Skip Buttons that are not visible right now
     * @param pdeLimitRow The topmost Row (in ABL character units) for BUTTON widgets that should be renders in the RibbonGroup
     * @return The RibbonGroup that was created for the Buttons in the FRAME
     */
    METHOD PUBLIC STATIC RibbonGroup BuildRibbonGroupFromFrame (phFrame AS HANDLE,
                                                                poToolbarsManager AS UltraToolbarsManager,
                                                                poRibbonTab AS RibbonTab,
                                                                pcRibbonGroupKey AS CHARACTER,
                                                                pcRibbonGroupCaption AS CHARACTER,
                                                                plSkipExisting AS LOGICAL,
                                                                plHideButtons AS LOGICAL,
                                                                plSkipInvisibleButtons AS LOGICAL,
                                                                pdeLimitRow AS DECIMAL):

        RETURN UltraToolbarsHelper:BuildRibbonGroupFromFrame (phFrame,
                                                              poToolbarsManager,
                                                              poRibbonTab,
                                                              pcRibbonGroupKey,
                                                              pcRibbonGroupCaption,
                                                              plSkipExisting,
                                                              plHideButtons,
                                                              plSkipInvisibleButtons,
                                                              pdeLimitRow,
                                                              "":U,
                                                              "":U) .
    END METHOD .

    /**
     * Purpose: Builds a RibbonGroup for BUTTON widgets in an ABL FRAME
     * Notes:
     * @param phFrame The HANDLE of the ABL FRAME WIDGET containing the BUTTON widgets
     * @param poToolbarsManager The reference to the UltraToolbarsManager that contains the Ribbon
     * @param poRibbonTab The reference to the RibbonTab that should contain the new RibbonGroup
     * @param pcRibbonGroupKey The key for the new RibbonTab object
     * @param pcRibbonGroupCaption The caption of the new RibbonTab object
     * @param plSkipExisting Flag indicating if Tools already available in the ToolbarsManager should be skipped
     * @param plHideButtons Flag indicating if the BUTTON widgets should be hidden
     * @param plSkipInvisibleButtons Skip Buttons that are not visible right now
     * @param pdeLimitRow The topmost Row (in ABL character units) for BUTTON widgets that should be renders in the RibbonGroup
     * @param pcToolkeyPrefix The prefix to use for the Ribbon Tool Keys
     * @param pcToolkeySuffix The suffix to use for the Ribbon Tool Keys
     * @return The RibbonGroup that was created for the Buttons in the FRAME
     */
    METHOD PUBLIC STATIC RibbonGroup BuildRibbonGroupFromFrame (phFrame AS HANDLE,
                                                                poToolbarsManager AS UltraToolbarsManager,
                                                                poRibbonTab AS RibbonTab,
                                                                pcRibbonGroupKey AS CHARACTER,
                                                                pcRibbonGroupCaption AS CHARACTER,
                                                                plSkipExisting AS LOGICAL,
                                                                plHideButtons AS LOGICAL,
                                                                plSkipInvisibleButtons AS LOGICAL,
                                                                pdeLimitRow AS DECIMAL,
                                                                pcToolkeyPrefix AS CHARACTER,
                                                                pcToolkeySuffix AS CHARACTER):

        DEFINE VARIABLE hWidget   AS HANDLE          NO-UNDO .
        DEFINE VARIABLE oTool     AS ToolBase        NO-UNDO .
        DEFINE VARIABLE deLastCol AS DECIMAL  INIT ? NO-UNDO .
        DEFINE VARIABLE cName     AS CHARACTER       NO-UNDO .

        DEFINE VARIABLE oRibbonGroup AS RibbonGroup NO-UNDO .

        EMPTY TEMP-TABLE ttWidgetsByLocation .

        IF VALID-HANDLE (phFrame) AND phFrame:TYPE = "FRAME":U THEN .
        ELSE RETURN ? .

        /* Mike Fechner, Consultingwerk Ltd. 10.07.2009
           Search toolbar buttons: TYPE = "BUTTON", ROW needs to be <=pdeLimitRow .
           When pdeLimitRow = 0 or ? then use 9999 as limit (show match every button).

           When there is a space between buttons, create a new group. */

        IF pdeLimitRow > 0 THEN .
        ELSE pdeLimitRow = 9999.

&IF DEFINED (WinKit) &THEN
        IF VALID-OBJECT (oCustomWidgetRenderer) THEN
            oCustomWidgetRenderer:BeginRibbonGroup (poToolbarsManager,
                                                    oRibbonGroup) .
&ENDIF

        IF poRibbonTab:Groups:Exists (pcRibbonGroupKey) THEN
            oRibbonGroup = poRibbonTab:Groups[pcRibbonGroupKey] .
        ELSE
            oRibbonGroup = poRibbonTab:Groups:Add (pcRibbonGroupKey) .

        oRibbonGroup:Caption = pcRibbonGroupCaption .

        ASSIGN hWidget = phFrame:FIRST-CHILD:FIRST-CHILD .

        widgetloop: DO WHILE VALID-HANDLE (hWidget):
            IF hWidget:TYPE = "BUTTON":U AND hWidget:NAME > "":U AND hWidget:ROW <= pdeLimitRow THEN DO:

&IF DEFINED (WinKit) NE 0 &THEN
                IF VALID-OBJECT (oBuildRibbonGroupFromFrameFilter) AND
                   NOT oBuildRibbonGroupFromFrameFilter:FilterTool (hWidget) THEN DO:
                    ASSIGN hWidget = hWidget:NEXT-SIBLING .
                    NEXT .
                END.
&ENDIF
                IF plSkipExisting AND poToolbarsManager:Tools:Exists (hWidget:NAME) THEN DO:
                    poToolbarsManager:Tools[hWidget:NAME]:SharedProps:Enabled = hWidget:SENSITIVE .
                    ASSIGN hWidget = hWidget:NEXT-SIBLING .
                    NEXT widgetloop .
                END.

                IF plSkipInvisibleButtons AND hWidget:VISIBLE = FALSE THEN DO:
                    ASSIGN hWidget = hWidget:NEXT-SIBLING .
                    NEXT widgetloop .
                END.

                CREATE ttWidgetsByLocation .
                ASSIGN ttWidgetsByLocation.WidgetRow    = hWidget:ROW
                       ttWidgetsByLocation.WidgetColumn = hWidget:COLUMN
                       ttWidgetsByLocation.WidgetHandle = hWidget .
            END.

&IF DEFINED (WinKit) NE 0 &THEN
            ELSE IF VALID-OBJECT (oCustomWidgetRenderer) AND hWidget:ROW <= pdeLimitRow AND hWidget:NAME > "":U THEN DO:
                IF oCustomWidgetRenderer:FilterWidget (hWidget,
                                                       poToolbarsManager) THEN DO:

                    CREATE ttWidgetsByLocation .
                    ASSIGN ttWidgetsByLocation.WidgetRow    = hWidget:ROW
                           ttWidgetsByLocation.WidgetColumn = hWidget:COLUMN
                           ttWidgetsByLocation.WidgetHandle = hWidget .
                END.
            END.
&ENDIF
            ASSIGN hWidget = hWidget:NEXT-SIBLING .
        END.

        widgetbylocationloop: FOR EACH ttWidgetsByLocation USE-INDEX LocationByColumn:
            ASSIGN hWidget = ttWidgetsByLocation.WidgetHandle
                   cName   = SUBSTITUTE ("&1&2&3":U,
                                         pcToolkeyPrefix,
                                         hWidget:NAME,
                                         pcToolkeySuffix).

            IF hWidget:TYPE = "BUTTON":U AND hWidget:NAME > "":U AND hWidget:ROW <= pdeLimitRow THEN DO:

                IF plSkipInvisibleButtons AND hWidget:VISIBLE = FALSE THEN DO:
                    ASSIGN hWidget = hWidget:NEXT-SIBLING .
                    NEXT widgetbylocationloop .
                END.

                IF plSkipExisting AND poToolbarsManager:Tools:Exists (cName) THEN DO:

                    poToolbarsManager:Tools[cName]:SharedProps:Enabled = hWidget:SENSITIVE .
                    ASSIGN hWidget = hWidget:NEXT-SIBLING .
                    NEXT widgetbylocationloop .
                END.

                IF poToolbarsManager:Tools:Exists (cName) THEN
                    oTool = poToolbarsManager:Tools[cName] .
                ELSE DO:
                    IF CAN-QUERY (hWidget, "POPUP-MENU":U) AND VALID-HANDLE (hWidget:POPUP-MENU) THEN DO:
                        oTool = NEW PopupMenuTool (cName).
                        CAST (oTool, PopupMenuTool):DropDownArrowStyle = DropDownArrowStyle:Segmented .

                        IF TYPE-OF (poToolbarsManager:DockWithinContainer,
                                    IFormWithUltraToolbarsManager) THEN
                            UltraToolbarsHelper:BuildSubMenuItems (hWidget:POPUP-MENU,
                                                                   CAST (oTool, PopupMenuTool),
                                                                   CAST (poToolbarsManager:DockWithinContainer,
                                                                         IFormWithUltraToolbarsManager)) .

                    END.
                    ELSE
                        oTool = NEW ButtonTool (cName) .

                    poToolbarsManager:Tools:Add(oTool) .
                END.

                ASSIGN oTool:Tag                 = STRING(hWidget)
                       oTool:SharedProps:Caption = hWidget:LABEL
                       oTool:SharedProps:Enabled = hWidget:SENSITIVE .

                IF NOT oRibbonGroup:Tools:Exists (cName) THEN
                    oRibbonGroup:Tools:AddTool (cName).

                /* Mike Fechner, Consultingwerk Ltd. 27/01/2014
                   Widgets that are more than X cols away from each other should be seperated
                   by a tiny bar in the Ribbon Group */
                IF ToolbarButtonMaxDistance > 0 THEN
                IF deLastCol = ? OR (deLastCol + ToolbarButtonMaxDistance) < hWidget:COLUMN THEN
                    oRibbonGroup:Tools[cName]:InstanceProps:IsFirstInGroup = TRUE .

                ASSIGN deLastCol = hWidget:COL + hWidget:WIDTH .

                /* Mike Fechner, Consultingwerk Ltd. 23 Aug 2011
                   Image Handling in separate Routine now for easier reuse */
                IF LoadImage (hWidget, oTool, TRUE) = TRUE THEN
                    oRibbonGroup:Tools[cName]:InstanceProps:PreferredSizeOnRibbon = RibbonToolSize:Large .
                ELSE
                    oRibbonGroup:Tools[cName]:InstanceProps:PreferredSizeOnRibbon = RibbonToolSize:Normal.

                oRibbonGroup:Tools[cName]:InstanceProps:DisplayStyle = ToolDisplayStyle:ImageAndText .
                oRibbonGroup:Tools[cName]:InstanceProps:Caption = IF hWidget:LABEL > "":U THEN hWidget:LABEL ELSE hWidget:TOOLTIP  .

&IF DEFINED (WinKit) NE 0 &THEN
                IF VALID-OBJECT (oRibbonButtonCustomizer) AND TYPE-OF (oRibbonGroup:Tools[cName],
                                                                       ButtonTool) THEN
                    oRibbonButtonCustomizer:CustomizeRibbonButton (CAST (oRibbonGroup:Tools[cName],
                                                                         ButtonTool)) .
&ENDIF
                IF plHideButtons THEN
                    ASSIGN hWidget:VISIBLE = FALSE
                           hWidget:HIDDEN  = TRUE .
            END.
&IF DEFINED (WinKit) NE 0 &THEN
            ELSE IF VALID-OBJECT (oCustomWidgetRenderer) AND hWidget:ROW <= pdeLimitRow AND hWidget:NAME > "":U THEN DO:
                IF oCustomWidgetRenderer:FilterWidget (hWidget,
                                                       poToolbarsManager) THEN DO:

                  oCustomWidgetRenderer:RenderWidget (hWidget,
                                                      poToolbarsManager,
                                                      oRibbonGroup) .
                END.
            END.
&ENDIF

            ASSIGN hWidget = hWidget:NEXT-SIBLING .
        END .

&IF DEFINED (WinKit) &THEN
        IF VALID-OBJECT (oCustomWidgetRenderer) THEN
            oCustomWidgetRenderer:EndRibbonGroup (poToolbarsManager,
                                                  oRibbonGroup) .
&ENDIF

        RETURN oRibbonGroup .

        FINALLY:
            EMPTY TEMP-TABLE ttWidgetsByLocation .
        END FINALLY.

    END METHOD.

    /**
     * Purpose: Builds a Ribbon Groups for a list of Widget Handles
     * Notes:
     * @param poToolbarsManager The reference to the UltraToolbarsManager that contains the Ribbon
     * @param poRibbonTab The reference to the RibbonTab that should contain the new RibbonGroup
     * @param pcRibbonGroupKey The key for the new RibbonTab object
     * @param pcRibbonGroupCaption The caption of the new RibbonTab object
     * @param plSkipExisting Flag indicating if Tools already available in the ToolbarsManager should be skipped
     * @param plHideButtons Flag indicating if the BUTTON widgets should be hidden
     * @param pcWidgetHandleList A comma-delimited list of BUTTON widget handles that should be rendered as ButtonTools in the RibbonGroup
     * @return The RibbonGroup that was created for the Buttons in the list
     */
    METHOD PUBLIC STATIC RibbonGroup BuildRibbonGroupFromList (poToolbarsManager AS UltraToolbarsManager,
                                                               poRibbonTab AS RibbonTab,
                                                               pcRibbonGroupKey AS CHARACTER,
                                                               pcRibbonGroupCaption AS CHARACTER,
                                                               plSkipExisting AS LOGICAL,
                                                               plHideButtons AS LOGICAL,
                                                               pcWidgetHandleList AS CHARACTER):

        DEFINE VARIABLE hWidget AS HANDLE   NO-UNDO .
        DEFINE VARIABLE oTool   AS ToolBase NO-UNDO .
        DEFINE VARIABLE i       AS INTEGER  NO-UNDO .

        DEFINE VARIABLE oRibbonGroup AS RibbonGroup NO-UNDO .

&IF DEFINED (WinKit) &THEN
        IF VALID-OBJECT (oCustomWidgetRenderer) THEN
            oCustomWidgetRenderer:BeginRibbonGroup (poToolbarsManager,
                                                    oRibbonGroup) .
&ENDIF

        IF poRibbonTab:Groups:Exists (pcRibbonGroupKey) THEN
            oRibbonGroup = poRibbonTab:Groups[pcRibbonGroupKey] .
        ELSE
            oRibbonGroup = poRibbonTab:Groups:Add (pcRibbonGroupKey) .

        oRibbonGroup:Caption = pcRibbonGroupCaption .

        widgetloop: DO i = 1 TO NUM-ENTRIES (pcWidgetHandleList):

            ASSIGN hWidget = WIDGET-HANDLE (ENTRY (i, pcWidgetHandleList)) .

            IF NOT VALID-HANDLE (hWidget) THEN
                NEXT widgetloop .

            IF hWidget:TYPE = "BUTTON":U AND hWidget:NAME > "":U THEN DO:

                IF plSkipExisting AND poToolbarsManager:Tools:Exists (hWidget:NAME) THEN DO:

                    poToolbarsManager:Tools[hWidget:NAME]:SharedProps:Enabled = hWidget:SENSITIVE .
                    ASSIGN hWidget = hWidget:NEXT-SIBLING .
                    NEXT widgetloop .
                END.

                IF poToolbarsManager:Tools:Exists (hWidget:NAME) THEN
                    oTool = poToolbarsManager:Tools[hWidget:NAME] .
                ELSE DO:
                    IF CAN-QUERY (hWidget, "POPUP-MENU":U) AND VALID-HANDLE (hWidget:POPUP-MENU) THEN DO:
                        oTool = NEW PopupMenuTool (hWidget:NAME).
                        CAST (oTool, PopupMenuTool):DropDownArrowStyle = DropDownArrowStyle:Segmented .

                        IF TYPE-OF (poToolbarsManager:DockWithinContainer,
                                    IFormWithUltraToolbarsManager) THEN
                            UltraToolbarsHelper:BuildSubMenuItems (hWidget:POPUP-MENU,
                                                                   CAST (oTool, PopupMenuTool),
                                                                   CAST (poToolbarsManager:DockWithinContainer,
                                                                         IFormWithUltraToolbarsManager)) .

                    END.
                    ELSE
                        oTool = NEW ButtonTool (hWidget:NAME) .

                    poToolbarsManager:Tools:Add(oTool) .
                END.

                ASSIGN oTool:Tag                 = STRING(hWidget)
                       oTool:SharedProps:Caption = hWidget:LABEL
                       oTool:SharedProps:Enabled = hWidget:SENSITIVE
                       oTool:SharedProps:Visible = hWidget:VISIBLE .

                IF NOT oRibbonGroup:Tools:Exists (hWidget:NAME) THEN
                    oRibbonGroup:Tools:AddTool (hWidget:NAME).

                /* Mike Fechner, Consultingwerk Ltd. 23 Aug 2011
                   Image Handling in separate Routine now for easier reuse */
                IF LoadImage (hWidget, oTool, TRUE) = TRUE THEN
                    oRibbonGroup:Tools[hWidget:NAME]:InstanceProps:PreferredSizeOnRibbon = RibbonToolSize:Large .
                ELSE
                    oRibbonGroup:Tools[hWidget:NAME]:InstanceProps:PreferredSizeOnRibbon = RibbonToolSize:Normal.

                oRibbonGroup:Tools[hWidget:NAME]:InstanceProps:DisplayStyle = ToolDisplayStyle:ImageAndText .
                oRibbonGroup:Tools[hWidget:NAME]:InstanceProps:Caption = IF hWidget:LABEL > "":U THEN hWidget:LABEL ELSE hWidget:TOOLTIP  .

&IF DEFINED (WinKit) NE 0 &THEN
                IF VALID-OBJECT (oRibbonButtonCustomizer) AND TYPE-OF (oRibbonGroup:Tools[hWidget:NAME],
                                                                       ButtonTool) THEN
                    oRibbonButtonCustomizer:CustomizeRibbonButton (CAST (oRibbonGroup:Tools[hWidget:NAME],
                                                                         ButtonTool)) .
&ENDIF
                IF plHideButtons THEN
                    ASSIGN hWidget:VISIBLE = FALSE
                           hWidget:HIDDEN  = TRUE .
            END.
&IF DEFINED (WinKit) &THEN
            ELSE IF VALID-OBJECT (oCustomWidgetRenderer) AND hWidget:NAME > "":U THEN DO:
                IF oCustomWidgetRenderer:FilterWidget (hWidget,
                                                       poToolbarsManager) THEN DO:

                  oCustomWidgetRenderer:RenderWidget (hWidget,
                                                      poToolbarsManager,
                                                      oRibbonGroup) .
                END.
            END.
&ENDIF
        END .

&IF DEFINED (WinKit) &THEN
        IF VALID-OBJECT (oCustomWidgetRenderer) THEN
            oCustomWidgetRenderer:EndRibbonGroup (poToolbarsManager,
                                                  oRibbonGroup) .
&ENDIF

        RETURN oRibbonGroup .

    END METHOD.

    /**
     * Purpose: Renders an ABL MENU-BAR structure as a Ribbon
     * Notes:   The Handle to the MENU-BAR needs to be accessible from the poForm:MENU-BAR reference
     *          First sub menu will be rendered in ApplicationMenu (Left)
     *          Additional sub menus will be rendered as RibbonTabs
     *          Pull-down menus are supposed to habe a valid name
     * @param poForm The Form owning the UltraToolbarsManager that contains the Ribbon
     * @param pcSkipMenus A comma delimited list of menu names that will be skipped (main pull down level only)
     */
    METHOD PUBLIC STATIC VOID BuildRibbonTabsFromMenuBar (poForm AS IFormWithUltraToolbarsManager,
                                                          pcSkipMenus AS CHARACTER):

        /* Mike Fechner, Consultingwerk Ltd. 15.12.2009
           First menu item will be rendered as ApplicationMenu */

        DEFINE VARIABLE hMenuBar  AS HANDLE NO-UNDO.
        DEFINE VARIABLE hMenuItem AS HANDLE NO-UNDO.

        ASSIGN hMenuBar  = poForm:MENU-BAR
               hMenuItem = hMenuBar:FIRST-CHILD .

        /* First sub menu will be rendered in ApplicationMenu (Left) */
        IF hMenuItem:NAME > "":U THEN
            BuildToolsCollection (hMenuItem,
                                  poForm:ToolbarsManager:Ribbon:ApplicationMenu:ToolAreaLeft:Tools,
                                  poForm) .

        /* next sub menus will be renderes as RibbonTabs */
        ASSIGN hMenuItem = hMenuItem:NEXT-SIBLING .

        DO WHILE VALID-HANDLE (hMenuItem):
            IF hMenuItem:NAME > "":U AND
               (CharacterType:IsNullOrEmpty (pcSkipMenus) OR LOOKUP (hMenuItem:NAME, pcSkipMenus) = 0) THEN

                BuildRibbonTab (poForm, hMenuItem) .

            hMenuItem = hMenuItem:NEXT-SIBLING .
        END.

        BuildApplicationFooterMenu (poForm) .

    END METHOD.

    /**
     * Purpose: Renders an ABL MENU-BAR structure as a Ribbon
     * Notes:   The Handle to the MENU-BAR needs to be accessible from the poForm:MENU-BAR reference
     *          First sub menu will be rendered in ApplicationMenu (Left)
     *          Additional sub menus will be rendered as RibbonTabs
     * @param poForm The Form owning the UltraToolbarsManager that contains the Ribbon
     */
    METHOD PUBLIC STATIC VOID BuildRibbonTabsFromMenuBar (poForm AS IFormWithUltraToolbarsManager):

        UltraToolbarsHelper:BuildRibbonTabsFromMenuBar (poForm, "":U) .

    END METHOD.


    /**
     * Purpose: Builds a RibbonTab from an ABL SUBMENU widget
     * Notes:
     * @param poForm The reference to the Form that contains the UltraToolbarsManager
     * @param phSubMenu The HANDLE of the ABL SUBMENU widget
     * @return The RibbonTab
     */
    METHOD PUBLIC STATIC RibbonTab BuildRibbonTab (poForm AS IFormWithUltraToolbarsManager,
                                                   phSubMenu AS HANDLE):

        DEFINE VARIABLE oRibbonTab     AS RibbonTab     NO-UNDO .
        DEFINE VARIABLE oRibbonGroup   AS RibbonGroup   NO-UNDO .
        DEFINE VARIABLE oToolItem      AS ToolBase      NO-UNDO .
        DEFINE VARIABLE oPopupMenuItem AS PopupMenuTool NO-UNDO .

        DEFINE VARIABLE hChild         AS HANDLE        NO-UNDO .

        DEFINE VARIABLE cLabel         AS CHARACTER     NO-UNDO .

        IF NOT CAN-QUERY (phSubMenu, "FIRST-CHILD":U) THEN
            RETURN ? .

        IF CAN-QUERY (phSubMenu, "LABEL":U) THEN
            ASSIGN cLabel = phSubMenu:LABEL .
        ELSE
            ASSIGN cLabel = phSubMenu:NAME .

        oRibbonTab = NEW RibbonTab (phSubMenu:NAME, UltraToolbarsHelper:RemoveMenuAmpersand (cLabel)) .

        poForm:ToolbarsManager:Ribbon:Tabs:Add (oRibbonTab) .

        /* Start with first RibbonGroup */
        oRibbonGroup = NEW RibbonGroup (GUID, "":U) .

        oRibbonTab:Groups:Add (oRibbonGroup) .

        ASSIGN hChild = phSubMenu:FIRST-CHILD .

        DO WHILE VALID-HANDLE(hChild):
            CASE hChild:TYPE:
                WHEN "MENU-ITEM":U THEN DO:
                    CASE hChild:SUBTYPE:
                        WHEN "NORMAL":U THEN DO:
                            oToolItem = CreateMenuItemButtonTool (hChild, oRibbonGroup:Tools, poForm) .

                            IF hChild:PRIVATE-DATA MATCHES "LARGE":U THEN

                            IF hChild:ACCELERATOR > "":U THEN
                                SetAccelerator (oToolItem, hChild) .
                        END.
                        WHEN "RULE":U THEN DO:
                            /* Create new Group */
                            oRibbonGroup = NEW RibbonGroup (GUID, "":U) .
                            oRibbonTab:Groups:Add (oRibbonGroup) .
                        END.

                    END CASE .
                END.
                WHEN "SUB-MENU":U THEN DO:
                    oPopupMenuItem = CreateSubMenuPopupMenu (hChild, oRibbonGroup:Tools, poForm) .

                    BuildSubMenuItems (hChild, oPopupMenuItem, poForm) .
                END.
            END CASE .

            hChild = hChild:NEXT-SIBLING .
        END.

        RETURN oRibbonTab .

        CATCH e AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (e) .
        END CATCH.

    END METHOD.

    /**
     * Purpose: Builds the RootTool Instances based on an ABL MENU-BAR Widget
     * Notes:
     * @param poForm The Form owning the UltraToolbarsManager that should create the Tool Instnances for the MENU-BAR
     */
    METHOD PUBLIC STATIC VOID BuildRootToolInstanceFromMenuBar (poForm AS IFormWithUltraToolbarsManager):

        /* Check availbility of Menubar first. */
        IF NOT VALID-HANDLE (poForm:MENU-BAR) THEN
            RETURN .

        poForm:ToolbarsManager:BeginUpdate () .

        UltraToolbarsHelper:BuildRootToolInstanceFromMenu (poForm) .

        FINALLY:
            poForm:ToolbarsManager:EndUpdate (TRUE) .
        END FINALLY.

    END METHOD .

    /**
     * Purpose: Builds the RootTool Instances based on an ABL MENU-BAR Widget
     * Notes:
     * @param poForm The Form owning the UltraToolbarsManager that should create the Tool Instnances for the MENU-BAR
     */
    METHOD PUBLIC STATIC VOID BuildRootToolInstanceFromMenu (poForm AS IFormWithUltraToolbarsManager):

        DEFINE VARIABLE hChild         AS HANDLE        NO-UNDO.
        DEFINE VARIABLE oPopupMenuItem AS PopupMenuTool NO-UNDO .

        ASSIGN hChild = poForm:MENU-BAR:FIRST-CHILD .

        DO WHILE VALID-HANDLE(hChild):
            CASE hChild:TYPE:
                WHEN "MENU-ITEM":U THEN DO:
                    IF hChild:SUBTYPE = "NORMAL":U THEN DO:
                        UltraToolbarsHelper:CreateMenuItemRootButtonTool (hChild,
                                                                          poForm) .
                    END .
                END.
                WHEN "SUB-MENU":U THEN DO:
                    oPopupMenuItem = CreateSubMenuPopupMenu (hChild, ?, poForm) .

                    UltraToolbarsHelper:BuildSubMenuItems (hChild, oPopupMenuItem, poForm) .
                END.
            END CASE .

            hChild = hChild:NEXT-SIBLING .
        END.

    END METHOD .

    /**
     * Purpose: Builds the RootTool Instances based on Buttons in a Frame
     * Notes:   Useful to build the RootTool Instances in cases where the toolbar or
     *          ribbon is built using an IRibbonBuilder
     * @param phFrame The HANDLE of the ABL FRAME widget containing the BUTTON widgets
     * @param pdeLimitRow The topmost Row (in ABL character units) for BUTTON widgets that should be renders in the Toolbar
     * @param poToolbarsManager The reference to the UltraToolbarsManager that contains the Toolbars collection
     */
    METHOD PUBLIC STATIC VOID BuildRootToolInstancesFromFrame (phFrame AS HANDLE,
                                                               pdeLimitRow AS DECIMAL,
                                                               poToolbarsManager AS UltraToolbarsManager):

        DEFINE VARIABLE oButtonTool  AS ToolBase              NO-UNDO .

        DEFINE VARIABLE hFieldGroup  AS HANDLE                NO-UNDO .
        DEFINE VARIABLE hWidget      AS HANDLE                NO-UNDO .
        DEFINE VARIABLE oToolbar     AS UltraToolbar          NO-UNDO .

        /* Mike Fechner, Consultingwerk Ltd. 10.07.2009
           Search toolbar buttons: TYPE = "BUTTON", ROW needs to be <=pdeLimitRow .
           When pdeLimitRow = 0 or ? then use 9999 as limit (show match every button).

           When there is a space between buttons, create a new group. */

        poToolbarsManager:BeginUpdate() .

        IF pdeLimitRow > 0 THEN .
        ELSE pdeLimitRow = 9999.

        ASSIGN hFieldGroup = phFrame:FIRST-CHILD .

        ASSIGN hWidget = hFieldGroup:FIRST-CHILD .

        DO WHILE VALID-HANDLE (hWidget):
            IF NOT hWidget:NAME > "":U THEN DO:
                hWidget = hWidget:NEXT-SIBLING .
                NEXT .
            END.

            IF (hWidget:TYPE = "BUTTON":U AND
                hWidget:ROW <= pdeLimitRow)
&IF DEFINED (WinKit) &THEN
                OR
               (VALID-OBJECT (oCustomWidgetRenderer) AND
                hWidget:ROW <= pdeLimitRow AND
                oCustomWidgetRenderer:FilterWidget (hWidget,
                                                    poToolbarsManager))
&ENDIF
               THEN DO:
&IF DEFINED (WinKit) &THEN
                IF hWidget:TYPE <> "BUTTON":U AND VALID-OBJECT (oCustomWidgetRenderer) THEN DO:
                    oCustomWidgetRenderer:RenderWidget (hWidget,
                                                        poToolbarsManager,
                                                        oToolbar) /* invalid reference, but typed */.
                    hWidget = hWidget:NEXT-SIBLING .
                    NEXT .
                END.
&ENDIF
                IF NOT poToolbarsManager:Tools:Exists(hWidget:NAME) THEN DO:
                    IF CAN-QUERY (hWidget, "POPUP-MENU":U) AND VALID-HANDLE (hWidget:POPUP-MENU) THEN DO:
                        oButtonTool = NEW PopupMenuTool (hWidget:NAME).
                        CAST (oButtonTool, PopupMenuTool):DropDownArrowStyle = DropDownArrowStyle:Segmented .
                    END.
                    ELSE
                        oButtonTool = NEW ButtonTool (hWidget:NAME).

                    poToolbarsManager:Tools:Add (oButtonTool) .
                END.
                ELSE
                    oButtonTool = poToolbarsManager:Tools[hWidget:NAME] .

                oButtonTool:SharedProps:Enabled = hWidget:SENSITIVE .
                oButtonTool:SharedProps:Visible = hWidget:VISIBLE .
                oButtonTool:SharedProps:ToolTipText = REPLACE (hWidget:TOOLTIP,
                                                               CHR(13),
                                                               System.Environment:NewLine)  .

                IF hWidget:LABEL > "":U AND oButtonTool:SharedProps:Caption = "":U THEN
                    oButtonTool:SharedProps:Caption = hWidget:LABEL .

                /* Mike Fechner, Consultingwerk Ltd. 23 Aug 2011
                   Image Handling in separate Routine now for easier reuse */
                IF LoadImage (hWidget, oButtonTool, TRUE) = FALSE THEN
                    oButtonTool:SharedProps:DisplayStyle = ToolDisplayStyle:TextOnlyAlways .

                /* Mike Fechner, Consultingwerk Ltd. 10.07.2009
                   Let .NET Toolbar know the Progress widget Handle */
                oButtonTool:Tag = STRING(hWidget:HANDLE).
            END.

            hWidget = hWidget:NEXT-SIBLING .
        END.

        FINALLY:
              poToolbarsManager:EndUpdate (TRUE) .
        END FINALLY.

    END METHOD .

    /**
     * Purpose: Creates Infragistics Tools (ButtonTool, PopupMenuTool) for widgets
     *          in an ABL menu structure
     * Notes:   WINKIT-13 added capabilities to filter menu items based on IMenuItemFilter
     *          service
     * @param phParentMenu The HANDLE of the ABL menu structure
     * @param poToolsCollection The reference to the ToolsCollection that should receive the created tools
     * @param poForm The Form owning the UltraToolbarsManager that contains the ToolsCollection
     */
    METHOD PUBLIC STATIC VOID BuildToolsCollection (phParentMenu AS HANDLE,
                                                    poToolsCollection AS ToolsCollection,
                                                    poForm AS IFormWithUltraToolbarsManager):

        DEFINE VARIABLE hChild         AS HANDLE        NO-UNDO.
        DEFINE VARIABLE lRulePending   AS LOGICAL       NO-UNDO INIT TRUE .
        DEFINE VARIABLE oToolItem      AS ToolBase      NO-UNDO .
        DEFINE VARIABLE oPopupMenuItem AS PopupMenuTool NO-UNDO .

        ASSIGN hChild = phParentMenu:FIRST-CHILD .

        DO WHILE VALID-HANDLE(hChild):
            /* Mike Fechner, Consultingwerk Ltd. 15.10.2014
               WINKIT-13: Allow to filter out context menu items */
&IF DEFINED (WinKit) NE 0 &THEN
            IF VALID-OBJECT (oMenuItemFilter) AND oMenuItemFilter:FilterMenuItem (poForm, hChild) = FALSE THEN DO:
                hChild = hChild:NEXT-SIBLING .
                NEXT .
            END.
&ENDIF

            CASE hChild:TYPE:
                WHEN "MENU-ITEM":U THEN DO:
                    CASE hChild:SUBTYPE:
                        WHEN "NORMAL":U THEN DO:
                            oToolItem = CreateMenuItemButtonTool (hChild, poToolsCollection, poForm) .

                            IF lRulePending THEN
                                ASSIGN poToolsCollection:Item [oToolItem:Key]:InstanceProps:IsFirstInGroup = TRUE
                                       lRulePending = FALSE .

                            IF hChild:ACCELERATOR > "":U THEN
                                SetAccelerator (oToolItem, hChild) .
                        END.
                        WHEN "RULE":U THEN
                            ASSIGN lRulePending = TRUE .
                    END CASE .
                END.
                WHEN "SUB-MENU":U THEN DO:
                    oPopupMenuItem = CreateSubMenuPopupMenu (hChild, poToolsCollection, poForm) .

                    IF lRulePending THEN
                        ASSIGN poToolsCollection:Item [oPopupMenuItem:Key]:InstanceProps:IsFirstInGroup = TRUE
                               lRulePending = FALSE .

                    BuildSubMenuItems (hChild, oPopupMenuItem, poForm) .
                END.
            END CASE .

            hChild = hChild:NEXT-SIBLING .
        END.

    END METHOD .

    /**
     * Purpose: Recurses a SUB-MENU and builds toolbar items
     * Notes:
     * @param phMenu The HANDLE of the ABL menu widget
     * @param poParentMenuItem The reference to the PopupMenuTool that has been created for the phMenu widget
     * @param poForm The Form owning the UltraToolbarsManager that contains the Tools
     */
    METHOD PUBLIC STATIC VOID BuildSubMenuItems (phMenu AS HANDLE,
                                                 poParentMenuItem AS PopupMenuTool,
                                                 poForm AS IFormWithUltraToolbarsManager):

        BuildToolsCollection (phMenu,
                              poParentMenuItem:Tools,
                              poForm) .

    END METHOD.

    /**
     * Purpose: Create a popup menu for the menu-items and sub-menus in the menu-bar
     * Notes:
     * @param phMenu The HANDLE of the ABL menu widget
     * @param poToolbar The reference to the UltraToolbar created for the ABL menu widget
     * @param poForm The Form owning the UltraToolbarsManager that contains the Toolbar
     * @param plIncludeMenuItem Flag indicating of the MenuItems should be rendered as well
     */
    METHOD PUBLIC STATIC VOID BuildSubMenus (phMenu AS HANDLE,
                                             poToolbar AS UltraToolbar,
                                             poForm AS IFormWithUltraToolbarsManager,
                                             plIncludeMenuItem AS LOGICAL):

        DEFINE VARIABLE hChild         AS HANDLE        NO-UNDO.
        DEFINE VARIABLE lRulePending   AS LOGICAL       NO-UNDO INIT FALSE .
        DEFINE VARIABLE oToolItem      AS ToolBase      NO-UNDO .
        DEFINE VARIABLE oPopupMenuItem AS PopupMenuTool NO-UNDO .

        ASSIGN hChild = phMenu:FIRST-CHILD .

        DO WHILE VALID-HANDLE(hChild):
            CASE hChild:TYPE:
                WHEN "MENU-ITEM":U THEN DO:
                    CASE hChild:SUBTYPE:
                        WHEN "NORMAL":U THEN IF plIncludeMenuItem THEN DO:
                            oToolItem = CreateMenuItemButtonTool (hChild, poToolbar:Tools, poForm) .

                            IF lRulePending THEN
                                ASSIGN poToolbar:Tools[oToolItem:Key]:InstanceProps:IsFirstInGroup = TRUE
                                       lRulePending = FALSE .
                        END.
                        WHEN "RULE":U THEN
                            ASSIGN lRulePending = TRUE .
                    END CASE .
                END.
                WHEN "SUB-MENU":U THEN DO:
                    oPopupMenuItem = UltraToolbarsHelper:CreateSubMenuPopupMenu (hChild, poToolbar:Tools, poForm) .

                    UltraToolbarsHelper:BuildSubMenuItems (hChild, oPopupMenuItem, poForm) .
                END.
            END CASE .

            hChild = hChild:NEXT-SIBLING .
        END.

    END METHOD.

    /**
     * Purpose: Creates ButtonTools for the MENU-ITEMs in the Window MENU-BAR
     * Notes:
     * @param phMenu The HANDLE of the ABL menu widget
     * @param poToolbar The reference to the UltraToolbar created for the ABL menu widget
     * @param poForm The Form owning the UltraToolbarsManager that contains the Toolbar
     */
    METHOD PUBLIC STATIC VOID BuildToolbar (phMenu AS HANDLE,
                                            poToolbar AS UltraToolbar,
                                            poForm AS IFormWithUltraToolbarsManager):

        DEFINE VARIABLE hChild       AS HANDLE   NO-UNDO.
        DEFINE VARIABLE oToolItem    AS ToolBase NO-UNDO .

        ASSIGN hChild = phMenu:FIRST-CHILD .

        DO WHILE VALID-HANDLE(hChild):
            CASE hChild:TYPE:
                WHEN "MENU-ITEM":U THEN DO:
                    CASE hChild:SUBTYPE:
                        WHEN "NORMAL":U THEN
                            oToolItem = CreateMenuItemButtonTool (hChild, poToolbar:Tools, poForm) .
                    END CASE .
                END.

            END CASE .

            hChild = hChild:NEXT-SIBLING .
        END.

    END METHOD.

    /**
     * Purpose: Builds a Toolbar based on Buttons in a Frame
     * Notes:
     * @param phFrame The HANDLE of the ABL FRAME widget containing the BUTTON widgets
     * @param phWindow The HANDLE of the ABL WINDOW widget, used for assigning the Text of the Toolbar object
     * @param pdeLimitRow The topmost Row (in ABL character units) for BUTTON widgets that should be renders in the Toolbar
     * @param poToolbarsManager The reference to the UltraToolbarsManager that contains the Toolbars collection
     * @return The reference to the generated UltraToolbar instance
     */
    METHOD PUBLIC STATIC UltraToolbar BuildToolbarFromFrame (phFrame AS HANDLE,
                                                             phWindow AS HANDLE,
                                                             pdeLimitRow AS DECIMAL,
                                                             poToolbarsManager AS UltraToolbarsManager) :

        DEFINE VARIABLE oToolbar     AS UltraToolbar          NO-UNDO .
        DEFINE VARIABLE oButtonTool  AS ToolBase              NO-UNDO .
        DEFINE VARIABLE oButtonToolI AS ToolBase              NO-UNDO .

        DEFINE VARIABLE hFieldGroup  AS HANDLE                NO-UNDO .
        DEFINE VARIABLE hWidget      AS HANDLE                NO-UNDO .
        DEFINE VARIABLE deLastCol    AS DECIMAL        INIT ? NO-UNDO .

        EMPTY TEMP-TABLE ttWidgetsByLocation .

        poToolbarsManager:BeginUpdate() .

        IF NOT poToolbarsManager:Toolbars:Exists ("frame":U + STRING(phFrame)) THEN
            oToolbar = NEW UltraToolbar ("frame":U + STRING(phFrame)) .
        ELSE
            oToolbar = poToolbarsManager:Toolbars ["frame":U + STRING(phFrame)] .

        IF VALID-HANDLE (phWindow) THEN
            oToolbar:Text = phWindow:TITLE .

        oToolbar:DockedRow = 0 .
        oToolbar:Tag = phFrame:NAME.

&IF DEFINED (WinKit) &THEN
        IF VALID-OBJECT (oCustomWidgetRenderer) THEN
            oCustomWidgetRenderer:BeginToolbar (poToolbarsManager,
                                                oToolbar) .
&ENDIF

        /* Mike Fechner, Consultingwerk Ltd. 10.07.2009
           Search toolbar buttons: TYPE = "BUTTON", ROW needs to be <=pdeLimitRow .
           When pdeLimitRow = 0 or ? then use 9999 as limit (show match every button).

           When there is a space between buttons, create a new group. */

        IF pdeLimitRow > 0 THEN .
        ELSE pdeLimitRow = 9999.

        ASSIGN hFieldGroup = phFrame:FIRST-CHILD .

        ASSIGN hWidget = hFieldGroup:FIRST-CHILD .

        DO WHILE VALID-HANDLE (hWidget):
            IF hWidget:TYPE = "BUTTON":U AND hWidget:ROW <= pdeLimitRow AND hWidget:NAME > "":U
               AND NOT oToolbar:Tools:Exists(hWidget:NAME) THEN DO:

&IF DEFINED (WinKit) NE 0 &THEN
                IF VALID-OBJECT (oBuildToolbarFromFrameFilter) AND
                   NOT oBuildRibbonGroupFromFrameFilter:FilterTool (hWidget) THEN DO:
                    ASSIGN hWidget = hWidget:NEXT-SIBLING .
                    NEXT .
                END.
&ENDIF

                CREATE ttWidgetsByLocation .
                ASSIGN ttWidgetsByLocation.WidgetRow    = hWidget:ROW
                       ttWidgetsByLocation.WidgetColumn = hWidget:COLUMN
                       ttWidgetsByLocation.WidgetHandle = hWidget .
            END.
&IF DEFINED (WinKit) &THEN
            ELSE IF VALID-OBJECT (oCustomWidgetRenderer) AND hWidget:ROW <= pdeLimitRow AND hWidget:NAME > "":U THEN DO:
                IF oCustomWidgetRenderer:FilterWidget (hWidget,
                                                       poToolbarsManager) THEN DO:

                    CREATE ttWidgetsByLocation .
                    ASSIGN ttWidgetsByLocation.WidgetRow    = hWidget:ROW
                           ttWidgetsByLocation.WidgetColumn = hWidget:COLUMN
                           ttWidgetsByLocation.WidgetHandle = hWidget .
                END.
            END.
&ENDIF

            ASSIGN
                hWidget   = hWidget:NEXT-SIBLING .
        END.

        widgetbylocationloop: FOR EACH ttWidgetsByLocation USE-INDEX Location:
            ASSIGN hWidget = ttWidgetsByLocation.WidgetHandle .

&IF DEFINED (WinKit) &THEN
            IF hWidget:TYPE <> "BUTTON":U AND VALID-OBJECT (oCustomWidgetRenderer) THEN DO:
                oCustomWidgetRenderer:RenderWidget (hWidget,
                                                    poToolbarsManager,
                                                    oToolbar) .

                NEXT widgetbylocationloop.
            END.
&ENDIF

            IF NOT poToolbarsManager:Tools:Exists(hWidget:NAME) THEN DO:
                IF CAN-QUERY (hWidget, "POPUP-MENU":U) AND VALID-HANDLE (hWidget:POPUP-MENU) THEN DO:
                    oButtonTool = NEW PopupMenuTool (hWidget:NAME).
                    CAST (oButtonTool, PopupMenuTool):DropDownArrowStyle = DropDownArrowStyle:Segmented .
                END.
                ELSE
                    oButtonTool = NEW ButtonTool (hWidget:NAME).

                poToolbarsManager:Tools:Add (oButtonTool) .
            END.
            ELSE
                oButtonTool = poToolbarsManager:Tools[hWidget:NAME] .

            IF TYPE-OF (oButtonTool, PopupMenuTool) THEN DO:
                oButtonToolI = NEW PopupMenuTool (hWidget:NAME).

                BuildSubMenuItems (hWidget:POPUP-MENU,
                                   CAST (oButtonTool, PopupMenuTool),
                                   CAST (poToolbarsManager:DockWithinContainer,
                                         IFormWithUltraToolbarsManager)) .
            END.
            ELSE
                oButtonToolI = NEW ButtonTool (hWidget:NAME).

            oButtonTool:SharedProps:Enabled = hWidget:SENSITIVE .
            oButtonTool:SharedProps:Visible = hWidget:VISIBLE .
            oButtonTool:SharedProps:ToolTipText = REPLACE (hWidget:TOOLTIP,
                                                           CHR(13),
                                                           System.Environment:NewLine)  .

            IF hWidget:LABEL > "":U AND oButtonTool:SharedProps:Caption = "":U THEN
                oButtonTool:SharedProps:Caption = hWidget:LABEL .

            /* Mike Fechner, Consultingwerk Ltd. 23 Aug 2011
               Image Handling in separate Routine now for easier reuse */
            IF LoadImage (hWidget, oButtonTool, TRUE) = FALSE THEN
                oButtonTool:SharedProps:DisplayStyle = ToolDisplayStyle:TextOnlyAlways .

            /* Mike Fechner, Consultingwerk Ltd. 10.07.2009
               Separator before next button ? */
            IF deLastCol = ? OR (deLastCol + ToolbarButtonMaxDistance) < hWidget:COLUMN THEN
                oButtonToolI:InstanceProps:IsFirstInGroup = TRUE .

            /* Mike Fechner, Consultingwerk Ltd. 10.07.2009
               Let .NET Toolbar know the Progress widget Handle */
            oButtonTool:Tag = STRING(hWidget:HANDLE).

            /* Mike Fechner, Consultingwerk Ltd. 10.07.2009
               Add to UltraToolbarsManager and the Toolbar */
            oToolbar:Tools:Add (oButtonToolI) .

            deLastCol = hWidget:COL + hWidget:WIDTH .
        END.

&IF DEFINED (WinKit) &THEN
        IF VALID-OBJECT (oCustomWidgetRenderer) THEN
            oCustomWidgetRenderer:EndToolbar (poToolbarsManager,
                                              oToolbar) .
&ENDIF

        IF oToolbar:Tools:Count > 0 AND NOT poToolbarsManager:Toolbars:Contains (oToolbar) THEN
            poToolbarsManager:Toolbars:Add(oToolbar) .

        RETURN oToolbar .

        FINALLY:
            EMPTY TEMP-TABLE ttWidgetsByLocation .

            poToolbarsManager:EndUpdate (TRUE) .
        END FINALLY.

    END METHOD .

    /**
     * Purpose: Builds a Toolbar based on Buttons in a Frame
     * Notes:
     * @param phFrame The HANDLE of the ABL FRAME widget containing the BUTTON widgets
     * @param pdeLimitRow The topmost Row (in ABL character units) for BUTTON widgets that should be renders in the Toolbar
     * @param poToolbarsManager The reference to the UltraToolbarsManager that contains the Toolbars collection
     * @return The reference to the generated UltraToolbar instance
     */
    METHOD PUBLIC STATIC UltraToolbar BuildToolbarFromFrame (phFrame AS HANDLE,
                                                             pdeLimitRow AS DECIMAL,
                                                             poToolbarsManager AS UltraToolbarsManager) :

         RETURN BuildToolbarFromFrame (phFrame,
                                       WidgetHelper:FindWindow(phFrame),
                                       pdeLimitRow,
                                       poToolbarsManager) .

    END METHOD .

    /**
     * Purpose: Builds a Toolbar based on Buttons in a Frame
     * Notes:
     * @param phFrame The HANDLE of the ABL FRAME widget containing the BUTTON widgets
     * @param phWindow The HANDLE of the ABL WINDOW widget, used for assigning the Text of the Toolbar object
     * @param poToolbarsManager The reference to the UltraToolbarsManager that contains the Toolbars collection
     * @return The reference to the generated UltraToolbar instance
     */
    METHOD PUBLIC STATIC UltraToolbar BuildToolbarFromFrame (phFrame AS HANDLE,
                                                             phWindow AS HANDLE,
                                                             poToolbarsManager AS UltraToolbarsManager) :

         RETURN BuildToolbarFromFrame (phFrame,
                                       phWindow,
                                       0,
                                       poToolbarsManager) .

    END METHOD .

    /**
     * Purpose: Builds a Toolbar based on Buttons in a Frame
     * Notes:
     * @param phFrame The HANDLE of the ABL FRAME widget containing the BUTTON widgets
     * @param poToolbarsManager The reference to the UltraToolbarsManager that contains the Toolbars collection
     * @return The reference to the generated UltraToolbar instance
     */
    METHOD PUBLIC STATIC UltraToolbar BuildToolbarFromFrame (phFrame AS HANDLE,
                                                             poToolbarsManager AS UltraToolbarsManager) :

         RETURN BuildToolbarFromFrame (phFrame,
                                       WidgetHelper:FindWindow(phFrame),
                                       0,
                                       poToolbarsManager) .

    END METHOD .

    /**
     * Purpose: Builds a Toolbar based on Buttons in a Frame
     * Notes:
     * @param phFrame The HANDLE of the ABL FRAME widget containing the BUTTON widgets
     * @param phWindow The HANDLE of the ABL WINDOW widget, used for assigning the Text of the Toolbar object
     * @param pdeLimitRow The topmost Row (in ABL character units) for BUTTON widgets that should be renders in the Toolbar
     * @param poForm The Form owning the UltraToolbarsManager that should create the Toolbar
     * @return The reference to the generated UltraToolbar instance
     */
    METHOD PUBLIC STATIC UltraToolbar BuildToolbarFromFrame (phFrame AS HANDLE,
                                                             phWindow AS HANDLE,
                                                             pdeLimitRow AS DECIMAL,
                                                             poForm AS IFormWithUltraToolbarsManager) :

         RETURN BuildToolbarFromFrame (phFrame,
                                       phWindow,
                                       pdeLimitRow,
                                       poForm:ToolbarsManager) .

    END METHOD .

    /**
     * Purpose: Builds a Toolbar based on Buttons in a Frame
     * Notes:
     * @param phFrame The HANDLE of the ABL FRAME widget containing the BUTTON widgets
     * @param pdeLimitRow The topmost Row (in ABL character units) for BUTTON widgets that should be renders in the Toolbar
     * @param poForm The Form owning the UltraToolbarsManager that should create the Toolbar
     * @return The reference to the generated UltraToolbar instance
     */
    METHOD PUBLIC STATIC UltraToolbar BuildToolbarFromFrame (phFrame AS HANDLE,
                                                             pdeLimitRow AS DECIMAL,
                                                             poForm AS IFormWithUltraToolbarsManager) :

             RETURN BuildToolbarFromFrame (phFrame,
                                           WidgetHelper:FindWindow(phFrame),
                                           pdeLimitRow,
                                           poForm:ToolbarsManager) .

    END METHOD .

    /**
     * Purpose: Builds a Toolbar based on Buttons in a Frame
     * Notes:
     * @param phFrame The HANDLE of the ABL FRAME widget containing the BUTTON widgets
     * @param phWindow The HANDLE of the ABL WINDOW widget, used for assigning the Text of the Toolbar object
     * @param poForm The Form owning the UltraToolbarsManager that should create the Toolbar
     * @return The reference to the generated UltraToolbar instance
     */
    METHOD PUBLIC STATIC UltraToolbar BuildToolbarFromFrame (phFrame AS HANDLE,
                                                             phWindow AS HANDLE,
                                                             poForm AS IFormWithUltraToolbarsManager) :

         RETURN BuildToolbarFromFrame (phFrame,
                                       phWindow,
                                       0,
                                       poForm:ToolbarsManager) .

    END METHOD .

    /**
     * Purpose: Builds a Toolbar based on Buttons in a Frame
     * Notes:
     * @param phFrame The HANDLE of the ABL FRAME widget containing the BUTTON widgets
     * @param poForm The Form owning the UltraToolbarsManager that should create the Toolbar
     * @return The reference to the generated UltraToolbar instance
     */
    METHOD PUBLIC STATIC UltraToolbar BuildToolbarFromFrame (phFrame AS HANDLE,
                                                             poForm AS IFormWithUltraToolbarsManager) :

         RETURN BuildToolbarFromFrame (phFrame,
                                       WidgetHelper:FindWindow(phFrame),
                                       0,
                                       poForm:ToolbarsManager) .

    END METHOD .

    /**
     * Purpose: Builds a Toolbar from a comma delimited list of Button Handles
     * Notes:
     * @param phFrame The HANDLE of the ABL FRAME widget containing the BUTTON widgets
     * @param poForm The Form owning the UltraToolbarsManager that should create the Toolbar
     * @param pcButtonList A comma-delimited list of ABL BUTTON widget HANDLEs
     * @return The reference to the generated UltraToolbar instance
     */
    METHOD PUBLIC STATIC UltraToolbar BuildToolbarFromList (phFrame      AS HANDLE,
                                                            poForm       AS IFormWithUltraToolbarsManager,
                                                            pcButtonList AS CHARACTER) :

         RETURN BuildToolbarFromList (phFrame,
                                      WidgetHelper:FindWindow(phFrame),
                                      pcButtonList,
                                      poForm:ToolbarsManager) .

    END METHOD .

    /**
     * Purpose: Builds a Toolbar from a comma delimited list of Button Handles
     * Notes:
     * @param phFrame The HANDLE of the ABL FRAME widget containing the BUTTON widgets
     * @param phWindow The HANDLE of the ABL WINDOW widget, used for assigning the Text of the Toolbar object
     * @param pcButtonList A comma-delimited list of ABL BUTTON widget HANDLEs
     * @param poToolbarsManager The reference to the UltraToolbarsManager
     * @return The reference to the generated UltraToolbar instance
     */
    METHOD PUBLIC STATIC UltraToolbar BuildToolbarFromList (phFrame AS HANDLE,
                                                            phWindow AS HANDLE,
                                                            pcButtonList AS CHARACTER,
                                                            poToolbarsManager AS UltraToolbarsManager):

        RETURN UltraToolbarsHelper:BuildToolbarFromList ("frame":U + STRING(phFrame),
                                                         phWindow:TITLE,
                                                         BOX(STRING (phFrame)),
                                                         STRING (phFrame),
                                                         pcButtonList,
                                                         poToolbarsManager) .

    END METHOD .

    /**
     * Purpose: Builds a Toolbar from a comma delimited list of Button Handles
     * Notes:
     * @param pcToolbarKey The key for the generated toolbar
     * @param pcToolbarTitle The title for the generated toolbar
     * @param poToolbarTag The tag for the generated toolbar
     * @param pcToolKeyPrefix The prefix to use for the generated tools
     * @param pcButtonList A comma-delimited list of ABL BUTTON widget HANDLEs
     * @param poToolbarsManager The reference to the UltraToolbarsManager
     * @return The reference to the generated UltraToolbar instance
     */
    METHOD PUBLIC STATIC UltraToolbar BuildToolbarFromList (pcToolbarKey AS CHARACTER,
                                                            pcToolbarTitle AS CHARACTER,
                                                            poToolbarTag AS System.Object,
                                                            pcToolKeyPrefix AS CHARACTER,
                                                            pcButtonList AS CHARACTER,
                                                            poToolbarsManager AS UltraToolbarsManager) :

        RETURN UltraToolbarsHelper:BuildToolbarFromList (pcToolbarKey,
                                                         pcToolbarTitle,
                                                         poToolbarTag,
                                                         pcToolKeyPrefix,
                                                         "":U,
                                                         pcButtonList,
                                                         poToolbarsManager) .

    END METHOD .

    /**
     * Purpose: Builds a Toolbar from a comma delimited list of Button Handles
     * Notes:
     * @param pcToolbarKey The key for the generated toolbar
     * @param pcToolbarTitle The title for the generated toolbar
     * @param poToolbarTag The tag for the generated toolbar
     * @param pcToolKeyPrefix The prefix to use for the generated tools
     * @param pcToolKeySuffix The suffix to use for the generated tools
     * @param pcButtonList A comma-delimited list of ABL BUTTON widget HANDLEs
     * @param poToolbarsManager The reference to the UltraToolbarsManager
     * @return The reference to the generated UltraToolbar instance
     */
    METHOD PUBLIC STATIC UltraToolbar BuildToolbarFromList (pcToolbarKey AS CHARACTER,
                                                            pcToolbarTitle AS CHARACTER,
                                                            poToolbarTag AS System.Object,
                                                            pcToolKeyPrefix AS CHARACTER,
                                                            pcToolKeySuffix AS CHARACTER,
                                                            pcButtonList AS CHARACTER,
                                                            poToolbarsManager AS UltraToolbarsManager) :

        DEFINE VARIABLE oToolbar     AS UltraToolbar          NO-UNDO .
        DEFINE VARIABLE oButtonTool  AS ToolBase              NO-UNDO .
        DEFINE VARIABLE oButtonToolI AS ToolBase              NO-UNDO .

        DEFINE VARIABLE hWidget      AS HANDLE                NO-UNDO .
        DEFINE VARIABLE deLastCol    AS DECIMAL        INIT ? NO-UNDO .
        DEFINE VARIABLE iCount       AS INTEGER               NO-UNDO .
        DEFINE VARIABLE cToolKey     AS CHARACTER             NO-UNDO .

        EMPTY TEMP-TABLE ttWidgetsByLocation .
        /* If the button list is empty then process nothing */
        IF pcButtonList EQ "":U THEN
            RETURN ? .

        IF poToolbarsManager:Toolbars:Exists (pcToolbarKey) AND
           poToolbarsManager:Toolbars [pcToolbarKey]:Visible EQ FALSE
        THEN DO:
            poToolbarsManager:Toolbars [pcToolbarKey]:Visible = TRUE.
            RETURN poToolbarsManager:Toolbars [pcToolbarKey].
        END.

        /* Toolbar Settings */
        oToolbar = NEW UltraToolbar (pcToolbarKey) .

        IF pcToolbarTitle > "":U THEN
            oToolbar:TEXT = pcToolbarTitle .

        oToolbar:DockedRow = 0 .
        oToolbar:Tag = poToolbarTag .

        DO iCount = 1 TO NUM-ENTRIES(pcButtonList):
            ASSIGN hWidget = HANDLE(ENTRY(iCount, pcButtonList)).

            IF VALID-HANDLE (hWidget) THEN DO:
                IF hWidget:TYPE = "BUTTON":U AND hWidget:NAME > "":U THEN DO:

                    CREATE ttWidgetsByLocation .
                    ASSIGN ttWidgetsByLocation.WidgetRow    = hWidget:ROW
                           ttWidgetsByLocation.WidgetColumn = hWidget:COLUMN
                           ttWidgetsByLocation.WidgetHandle = hWidget .
                END.
            END.
        END.

        FOR EACH ttWidgetsByLocation USE-INDEX Location:

            ASSIGN
              hWidget   = ttWidgetsByLocation.WidgetHandle
              cToolKey  = SUBSTITUTE ("&1&2&3":U,
                                         pcToolkeyPrefix,
                                         hWidget:NAME,
                                         pcToolkeySuffix).

            IF NOT poToolbarsManager:Tools:Exists(cToolKey) THEN DO:
                IF CAN-QUERY (hWidget, "POPUP-MENU":U) AND VALID-HANDLE (hWidget:POPUP-MENU) THEN DO:
                    oButtonTool = NEW PopupMenuTool (cToolKey).
                    CAST (oButtonTool, PopupMenuTool):DropDownArrowStyle = DropDownArrowStyle:Segmented .
                END.
                ELSE
                    oButtonTool = NEW ButtonTool (cToolKey).

                poToolbarsManager:Tools:Add (oButtonTool) .
            END.
            ELSE
                oButtonTool = CAST (poToolbarsManager:Tools[cToolKey],
                                    ButtonTool) .

            IF TYPE-OF (oButtonTool, PopupMenuTool) THEN DO:
                oButtonToolI = NEW PopupMenuTool (cToolKey).

                BuildSubMenuItems (hWidget:POPUP-MENU,
                                   CAST (oButtonTool, PopupMenuTool),
                                   CAST (poToolbarsManager:DockWithinContainer,
                                         IFormWithUltraToolbarsManager)) .
            END.
            ELSE
                oButtonToolI = NEW ButtonTool (cToolKey).

            oButtonTool:SharedProps:Enabled = hWidget:SENSITIVE .
            oButtonTool:SharedProps:ToolTipText = REPLACE (hWidget:TOOLTIP,
                                                           CHR(13),
                                                           System.Environment:NewLine)  .

            IF hWidget:LABEL > "":U AND oButtonTool:SharedProps:Caption = "":U THEN
                oButtonTool:SharedProps:Caption = hWidget:LABEL .

            /* Mike Fechner, Consultingwerk Ltd. 23 Aug 2011
               Image Handling in separate Routine now for easier reuse */
            IF LoadImage (hWidget, oButtonTool, TRUE) = FALSE THEN
                oButtonTool:SharedProps:DisplayStyle = ToolDisplayStyle:TextOnlyAlways .

            /* Separator before next button ? */
            IF deLastCol = ? OR deLastCol < hWidget:COL THEN
                oButtonToolI:InstanceProps:IsFirstInGroup = TRUE .

            /* Let .NET Toolbar know the Progress widget Handle */
            oButtonTool:Tag = STRING(hWidget:HANDLE).

            /* Add to UltraToolbarsManager and the Toolbar */
            oToolbar:Tools:Add (oButtonToolI) .

            deLastCol = hWidget:COL + hWidget:WIDTH .

        END.

        IF oToolbar:Tools:Count > 0 THEN
            poToolbarsManager:Toolbars:Add(oToolbar) .

        RETURN oToolbar .

        FINALLY:
            EMPTY TEMP-TABLE ttWidgetsByLocation .
        END FINALLY.

    END METHOD .

    /**
     * Purpose: Renders the MENU-BAR widget of an ABL WINDOW widget as the main menu tool of an UltraToolbarsManager instance
     * Notes:
     * @param poForm The Form owning the UltraToolbarsManager that should create the Main Menu Toolbar
     */
    METHOD PUBLIC STATIC VOID BuildUltraToolbarMenu (poForm AS IFormWithUltraToolbarsManager):

        DEFINE VARIABLE hMenubar AS HANDLE       NO-UNDO .
        DEFINE VARIABLE oToolbar AS UltraToolbar NO-UNDO .

        ASSIGN hMenubar = poForm:MENU-BAR .

        /* Check availbility of Menubar first. */
        IF NOT VALID-HANDLE(hMenubar) THEN
            RETURN .

        /* Check the availbility of the UltraToolbarsManager */
        IF NOT VALID-OBJECT(poForm:ToolbarsManager) THEN
            RETURN .

        poForm:ToolbarsManager:BeginUpdate() .

        oToolbar = CreateMenubarToolbar (poForm) .

        UltraToolbarsHelper:BuildSubMenus (hMenubar, oToolbar, poForm, NOT poForm:MenuItemsAsToolbar) .

        IF poForm:MenuItemsAsToolbar THEN DO:
            oToolbar = CreateToolbar ("Toolbar":U, poForm) .

            BuildToolbar (hMenubar, oToolbar, poForm) .
        END.

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

        FINALLY:
          IF VALID-OBJECT (poForm) AND VALID-OBJECT (poForm:ToolbarsManager) THEN
              poForm:ToolbarsManager:EndUpdate (TRUE) .
        END FINALLY.

    END METHOD.

    /**
     * Purpose: Cleans up a List of ContextualTabs typically created by the method
     *          MergeContextualTabGroups
     * Notes:
     * @param poToolbarsManager The UltraToolbarsManager instance that contains the tools to be removed
     * @param poList A System.Collections.IList containing the ToolBase, RibbonTabs and ContextualTabGroups
     */
    METHOD PUBLIC STATIC VOID CleanUpMergedContextualTabGroups (poToolbarsManager AS UltraToolbarsManager,
                                                                poList AS System.Collections.IList):

        {Consultingwerk/foreach.i System.Object oObject in poList}

            IF TYPE-OF (oObject, ContextualTabGroup) THEN
                CAST (oObject, ContextualTabGroup):Visible = FALSE  .

            IF TYPE-OF (oObject, ContextualTabGroup) THEN
                poToolbarsManager:Ribbon:ContextualTabGroups:Remove (CAST (oObject, ContextualTabGroup)) .

            IF TYPE-OF (oObject, RibbonTab) THEN
                poToolbarsManager:Ribbon:Tabs:Remove (CAST (oObject, RibbonTab)) .

            IF TYPE-OF (oObject, ToolBase) THEN
                poToolbarsManager:Tools:Remove (CAST (oObject, ToolBase)) .

            DELETE OBJECT oObject .
        END.

        poToolbarsManager:RefreshMerge () .

    END METHOD .

    /**
     * Purpose: Converts a RibbonTab to a Toolbar
     * Notes:
     * @param poToolbarsManager The reference to the UltraToolbarsManager that contains the Ribbon
     * @param pcRibbonTab The RibbonTab that should be converted to a Toolbar
     */
    METHOD PUBLIC STATIC VOID ConvertRibbonToToolbars (poToolbarsManager AS UltraToolbarsManager,
                                                       pcRibbonTab AS CHARACTER):

        DEFINE VARIABLE oRibbonTab   AS RibbonTab    NO-UNDO .
        DEFINE VARIABLE oRibbonGroup AS RibbonGroup  NO-UNDO .
        DEFINE VARIABLE i            AS INTEGER      NO-UNDO .
        DEFINE VARIABLE j            AS INTEGER      NO-UNDO .
        DEFINE VARIABLE oToolbar     AS UltraToolbar NO-UNDO .

        poToolbarsManager:Ribbon:Visible = FALSE .

        ASSIGN oRibbonTab = poToolbarsManager:Ribbon:Tabs[pcRibbonTab] .

        DO i = 0 TO oRibbonTab:Groups:Count - 1:
            oRibbonGroup = oRibbonTab:Groups[i] .

            oToolbar = NEW UltraToolbar (oRibbonGroup:Key) .

            oToolbar:Text = oRibbonGroup:Caption + " ":U + poToolbarsManager:DockWithinContainer:Text .

            oToolbar:DockedRow = 1 .
            oToolbar:VISIBLE = oRibbonGroup:VISIBLE .

            poToolbarsManager:Toolbars:Add (oToolbar) .

            DO j = 0 TO oRibbonGroup:Tools:Count - 1:
                oToolbar:Tools:InsertTool (j, oRibbonGroup:Tools[j]:Key) .
            END.
        END.

    END METHOD.

    /**
     * Purpose: Converts a RibbonTab to a Toolbar
     * Notes:
     * @param poForm The Form owning the UltraToolbarsManager that contains the Ribbon
     * @param pcRibbonTab The RibbonTab that should be converted to a Toolbar
     */
    METHOD PUBLIC STATIC VOID ConvertRibbonToToolbars (poForm AS IFormWithUltraToolbarsManager,
                                                       pcRibbonTab AS CHARACTER):

        UltraToolbarsHelper:ConvertRibbonToToolbars (poForm:ToolbarsManager, pcRibbonTab) .

    END METHOD.

    /**
     * Purpose: Creates a Button Tool (RootTool only) in the UltraToolbarsManager
     *          based on the properties of a ABL Button Widget
     * Notes:
     * @param poToolbarsManager The reference to the UltraToolbarsManager that contains the Ribbon
     * @param phButton The HANDLE of the ABL BUTTON widget
     * @return The reference to the ButtonTool that was created to represent the ABL BUTTON widget
     */
    METHOD PUBLIC STATIC ButtonTool CreateButtonToolFromButton (poToolbarsManager AS UltraToolbarsManager,
                                                                phButton AS HANDLE):

        DEFINE VARIABLE oButtonTool AS ButtonTool            NO-UNDO .

        /* Button Tool */
        IF NOT poToolbarsManager:Tools:Exists(phButton:NAME) THEN DO:
            oButtonTool = NEW ButtonTool (phButton:NAME).

            poToolbarsManager:Tools:Add (oButtonTool) .
        END.
        ELSE
            oButtonTool = CAST (poToolbarsManager:Tools[phButton:NAME],
                                ButtonTool) .

        oButtonTool:SharedProps:Caption = phButton:LABEL .
        oButtonTool:Tag = STRING (phButton) .

        oButtonTool:SharedProps:Enabled = phButton:SENSITIVE .
        oButtonTool:SharedProps:ToolTipText = REPLACE (phButton:TOOLTIP,
                                                       CHR(13),
                                                       System.Environment:NewLine)  .

        /* Mike Fechner, Consultingwerk Ltd. 23 Aug 2011
           Image Handling in separate Routine now for easier reuse */
        IF LoadImage (phButton, oButtonTool, TRUE) = FALSE THEN
                oButtonTool:SharedProps:DisplayStyle = ToolDisplayStyle:TextOnlyAlways .

        RETURN oButtonTool .

    END METHOD.

    /**
     * Purpose: Creates a new Menubar Toolbar in the UltraToolbar
     * Notes:   Checks if a Menubar Toolbar existis and returns that reference if
     *          so. Used by BuildUltraToolbarMenu
     * @param poForm The Form owning the UltraToolbarsManager that should create the main menu Toolbar
     * @return The UltraToolbar that should be used as the main menu
     */
    METHOD PUBLIC STATIC UltraToolbar CreateMenubarToolbar (poForm AS IFormWithUltraToolbarsManager):

        DEFINE VARIABLE oToolbar AS UltraToolbar NO-UNDO.

        oToolbar = UltraToolbarsHelper:GetMainMenuToolbar (poForm:ToolbarsManager) .

        IF VALID-OBJECT (oToolbar) THEN
            RETURN oToolbar .

        oToolbar = NEW UltraToolbar ("MENU-BAR":U) .

        ASSIGN oToolbar:DockedColumn = 0
               oToolbar:DockedRow = 0
               oToolbar:IsMainMenuBar = NOT poForm:MenuBarAsToolbar .

        poForm:ToolbarsManager:Toolbars:Add (oToolbar) .

        RETURN oToolbar.

    END METHOD.

    /**
     * Purpose: Creates a ButtonTool for a MENU-ITEM widget
     * Notes:   The MENU-ITEM needs have a valid name to proceed. Handling of
     *          MENU-ITEM's without a name can be customized using an implementation
     *          of the Consultingwerk.WindowIntegrationKit.IUnnamedWidgetHandler
     *          interface. The default is to raise an error.
     * @param phMenuItem The MenuItem to create a ButtonTool for
     * @param poToolsCollection The collection (in a Toolbar, a PopupMenuTool, a RibbonGroup, ...) an instance tool should be created
     * @param poForm The Form that owns the UltraToolbarsManager
     * @return The reference to the ButtonTool that was created for the MENU-ITEM widget
     */
    METHOD PUBLIC STATIC ButtonTool CreateMenuItemButtonTool (phMenuItem AS HANDLE,
                                                              poToolsCollection AS ToolsCollectionBase,
                                                              poForm AS IFormWithUltraToolbarsManager):

        RETURN CreateMenuItemButtonTool (phMenuItem,
                                         poToolsCollection,
                                         poForm,
                                         "":U) .

    END METHOD .

    /**
     * Purpose: Creates a ButtonTool for a MENU-ITEM widget
     * Notes:   The MENU-ITEM needs have a valid name to proceed. Handling of
     *          MENU-ITEM's without a name can be customized using an implementation
     *          of the Consultingwerk.WindowIntegrationKit.IUnnamedWidgetHandler
     *          interface. The default is to raise an error.
     * @param phMenuItem The MenuItem to create a ButtonTool for
     * @param poToolsCollection The collection (in a Toolbar, a PopupMenuTool, a RibbonGroup, ...) an instance tool should be created
     * @param poForm The Form that owns the UltraToolbarsManager
     * @param pcToolKeySuffix The suffix to append to the tool's key
     * @return The reference to the ButtonTool that was created for the MENU-ITEM widget
     */
    METHOD PUBLIC STATIC ButtonTool CreateMenuItemButtonTool (phMenuItem AS HANDLE,
                                                              poToolsCollection AS ToolsCollectionBase,
                                                              poForm AS IFormWithUltraToolbarsManager,
                                                              pcToolKeySuffix AS CHARACTER):

        DEFINE VARIABLE oToolItem             AS ButtonTool            NO-UNDO .
        DEFINE VARIABLE oToolItemRef          AS ButtonTool            NO-UNDO .

        DEFINE VARIABLE cToolName             AS CHARACTER             NO-UNDO.
        DEFINE VARIABLE cOwnerName            AS CHARACTER             NO-UNDO.
        &IF DEFINED (WinKit) NE 0 &THEN
        DEFINE VARIABLE oUnnamedWidgetHandler AS IUnnamedWidgetHandler NO-UNDO .
        &ENDIF

        /* Mike Fechner, Consultingwerk Ltd. 30.06.2011
           Meaningfull error message when a Widget has no name
           (o.k. for Progress, problem with Infragistics) */
        /* Mike Fechner, Consultingwerk Ltd. 25.11.2011
           Handling of Unnamed widgets not in IUnnamedWidgetHandler */
        IF phMenuItem:NAME > "":U THEN
            ASSIGN cToolName = phMenuItem:NAME .
        ELSE DO:
        &IF DEFINED (WinKit) NE 0 &THEN
            ASSIGN oUnnamedWidgetHandler = {Consultingwerk/get-service.i
                                                Consultingwerk.WindowIntegrationKit.IUnnamedWidgetHandler
                                                "NEW UnnamedWidgetHandler ()"} .

            cToolName = oUnnamedWidgetHandler:HandleUnnamedWidget (phMenuItem,
                                                                   phMenuItem:INSTANTIATING-PROCEDURE,
                                                                   poForm:ToolbarsManager) .
        &ELSE
            UNDO, THROW NEW AppError (SUBSTITUTE ("Unable to render widget as Infragistics ButtonTool. Name may not be empty.&1" +
                                                       "Widget-Type: &5&1Dynamic Widget: &2&1Creator: &3&1Label: &4",
                                                       System.Environment:NewLine,
                                                       phMenuItem:DYNAMIC,
                                                       (IF VALID-HANDLE (phMenuItem:INSTANTIATING-PROCEDURE) THEN phMenuItem:INSTANTIATING-PROCEDURE:FILE-NAME ELSE "?":U),
                                                       phMenuItem:LABEL,
                                                       phMenuItem:TYPE), 0) .
        &ENDIF

        END.

        IF Consultingwerk.Util.UltraToolbarsHelper:PrefixMenuItemsWithMenuName THEN DO:
            IF TYPE-OF (poToolsCollection:Owner, ToolBase) THEN
               ASSIGN cOwnerName = CAST (poToolsCollection:Owner, ToolBase):Key .
            ELSE IF TYPE-OF (poToolsCollection:Owner, RibbonGroup) THEN
               ASSIGN cOwnerName = CAST (poToolsCollection:Owner, RibbonGroup):Key .
            ELSE IF TYPE-OF (poToolsCollection:Owner, UltraToolbar) THEN
               ASSIGN cOwnerName = CAST (poToolsCollection:Owner, UltraToolbar):Key .
            ELSE IF TYPE-OF (poToolsCollection:Owner, ApplicationMenuArea) THEN DO:
                IF poToolsCollection:Owner = poToolsCollection:ToolbarsManager:Ribbon:ApplicationMenu:ToolAreaLeft THEN
                    ASSIGN cOwnerName = "Ribbon:ApplicationMenu:ToolAreaLeft":U .
                ELSE
                    ASSIGN cOwnerName = "Ribbon:ApplicationMenu:ToolAreaRight":U .
            END.

            IF cOwnerName > "":U THEN
                ASSIGN cToolName = cOwnerName + ":":U + cToolName .
        END.

        IF pcToolKeySuffix > "":U THEN
            ASSIGN cToolName = cToolName + pcToolKeySuffix .

        /* Mike Fechner, Consultingwerk Ltd. 12.05.2009
           Only add the base tool item, if not already in the Tool collection
           of the UltraToolbarsManager */
        IF NOT poForm:ToolbarsManager:Tools:Exists (cToolName) THEN DO:
            IF CAN-QUERY (phMenuItem, "TOGGLE-BOX":U) AND phMenuItem:TOGGLE-BOX = TRUE THEN DO:
                oToolItem = NEW StateButtonTool (cToolName) .

                CAST (oToolItem, StateButtonTool):MenuDisplayStyle = StateButtonMenuDisplayStyle:DisplayCheckmark .
                CAST (oToolItem, StateButtonTool):ToolbarDisplayStyle = StateButtonToolbarDisplayStyle:Button .
                CAST (oToolItem, StateButtonTool):Checked = phMenuItem:CHECKED .
            END.
            ELSE
                oToolItem = NEW ButtonTool (cToolName) .

            ASSIGN
                oToolItem:Tag = STRING(phMenuItem)
                oToolItem:SharedProps:Caption = phMenuItem:LABEL
                oToolItem:SharedProps:Enabled = phMenuItem:SENSITIVE .

            /* Mike Fechner, Consultingwerk Ltd. 27.03.2012
               Moved the call to add the new item to the Tools collection
               to before the call to the custom ToolImageProvider class so
               that this class could - if required - call back to
               poToolItem:ToolbarsManager:DockedWithinContainer ... */
            poForm:ToolbarsManager:Tools:Add (oToolItem) .

            &IF DEFINED (WinKit) NE 0 &THEN
            /* Mike Fechner, Consultingwerk Ltd. 24.08.2011
               IToolImageProvider is part of WinKit, so the WinKit is required
               to compile this */

            /* Mike Fechner, Consultingwerk Ltd. 24.08.2011
               When a custom ToolImageProvider is used, this might actuall
               find images for MENU-ITEMs as well */
            IF VALID-OBJECT (oToolImageProvider) THEN DO:
                IF oToolImageProvider:LoadImage (phMenuItem, oToolItem) THEN
                    oToolItem:SharedProps:DisplayStyle = ToolDisplayStyle:ImageOnlyOnToolbars .
                ELSE
                    oToolItem:SharedProps:DisplayStyle = ToolDisplayStyle:TextOnlyAlways .
            END.
            ELSE
            &ELSE
            oToolItem:SharedProps:DisplayStyle = ToolDisplayStyle:TextOnlyInMenus .
            &ENDIF

            IF CAN-QUERY (phMenuItem, "TOOLTIP":U) THEN
                oToolItem:SharedProps:ToolTipText = REPLACE (phMenuItem:TOOLTIP,
                                                             CHR(13),
                                                             System.Environment:NewLine)  .

            IF oToolItem:Key = oToolItem:SharedProps:Caption AND oToolItem:SharedProps:ToolTipText > "":U THEN
                oToolItem:SharedProps:Caption = oToolItem:SharedProps:ToolTipText .
        END.
        ELSE DO:
            oToolItem = CAST(poForm:ToolbarsManager:Tools[cToolName], ButtonTool) .

            /* Mike Fechner, Consultingwerk Ltd. 04.04.2010
               Tag should point to the new MENU-ITEM so that the latest menu item
               used to build this tool is active (received OnChoose event) */

            oToolItem:Tag = STRING (phMenuItem) .
            oToolItem:SharedProps:Enabled = phMenuItem:SENSITIVE .

            /* By default don't set again when there is already a caption,
               optionally let the service decide */

            IF oToolItem:SharedProps:Caption > "":U
                AND NOT (VALID-OBJECT (oChangeRibbonToolCaptionFilter) AND oChangeRibbonToolCaptionFilter:ChangeToolLabel (phMenuItem))

                THEN .
            ELSE oToolItem:SharedProps:Caption = phMenuItem:LABEL .
        END.

        UltraToolbarsHelper:SetAccelerator (oToolItem, phMenuItem) .

        IF NOT poToolsCollection:Exists(cToolName) THEN DO:
            IF CAN-QUERY (phMenuItem, "TOGGLE-BOX":U) AND phMenuItem:TOGGLE-BOX = TRUE THEN
                oToolItemRef = NEW StateButtonTool (cToolName) .
            ELSE
            oToolItemRef = NEW ButtonTool (cToolName) .

            oToolItemRef:Tag = STRING(phMenuItem).

            IF oToolItem:SharedProps:Caption <> phMenuItem:LABEL THEN
                oToolItemRef:InstanceProps:Caption = phMenuItem:LABEL .

            poToolsCollection:Add(oToolItemRef) .
        END.

        RETURN oToolItem .

    END METHOD.

    /**
     * Purpose: Creates a Root ButtonTool for a MENU-ITEM widget
     * Notes:   The MENU-ITEM needs have a valid name to proceed. Handling of
     *          MENU-ITEM's without a name can be customized using an implementation
     *          of the Consultingwerk.WindowIntegrationKit.IUnnamedWidgetHandler
     *          interface. The default is to raise an error.
     * @param phMenuItem The MenuItem to create a ButtonTool for
     * @param poForm The reference to the Form with the reference to the UltraToolbarsManager
     * @return The reference to the ButtonTool that was created for the MENU-ITEM widget
     */
    METHOD PUBLIC STATIC ToolBase CreateMenuItemRootButtonTool (phMenuItem AS HANDLE,
                                                                poForm AS IFormWithUltraToolbarsManager):

        DEFINE VARIABLE oToolItem             AS ButtonTool            NO-UNDO .

        DEFINE VARIABLE cToolName             AS CHARACTER             NO-UNDO.
        &IF DEFINED (WinKit) NE 0 &THEN
        DEFINE VARIABLE oUnnamedWidgetHandler AS IUnnamedWidgetHandler NO-UNDO .
        &ENDIF

        /* Mike Fechner, Consultingwerk Ltd. 30.06.2011
           Meaningfull error message when a Widget has no name
           (o.k. for Progress, problem with Infragistics) */
        /* Mike Fechner, Consultingwerk Ltd. 25.11.2011
           Handling of Unnamed widgets not in IUnnamedWidgetHandler */
        IF phMenuItem:NAME > "":U THEN
            ASSIGN cToolName = phMenuItem:NAME .
        ELSE DO:
        &IF DEFINED (WinKit) NE 0 &THEN
            ASSIGN oUnnamedWidgetHandler = {Consultingwerk/get-service.i
                                                Consultingwerk.WindowIntegrationKit.IUnnamedWidgetHandler
                                                "NEW UnnamedWidgetHandler ()"} .

            cToolName = oUnnamedWidgetHandler:HandleUnnamedWidget (phMenuItem,
                                                                   phMenuItem:INSTANTIATING-PROCEDURE,
                                                                   poForm:ToolbarsManager) .
        &ELSE
            UNDO, THROW NEW AppError (SUBSTITUTE ("Unable to render widget as Infragistics ButtonTool. Name may not be empty.&1" +
                                                       "Widget-Type: &5&1Dynamic Widget: &2&1Creator: &3&1Label: &4",
                                                       System.Environment:NewLine,
                                                       phMenuItem:DYNAMIC,
                                                       (IF VALID-HANDLE (phMenuItem:INSTANTIATING-PROCEDURE) THEN phMenuItem:INSTANTIATING-PROCEDURE:FILE-NAME ELSE "?":U),
                                                       phMenuItem:LABEL,
                                                       phMenuItem:TYPE), 0) .
        &ENDIF

        END.

        /* Mike Fechner, Consultingwerk Ltd. 12.05.2009
           Only add the base tool item, if not already in the Tool collection
           of the UltraToolbarsManager */
        IF NOT poForm:ToolbarsManager:Tools:Exists (cToolName) THEN DO:
            IF CAN-QUERY (phMenuItem, "TOGGLE-BOX":U) AND phMenuItem:TOGGLE-BOX = TRUE THEN DO:
                oToolItem = NEW StateButtonTool (cToolName) .

                CAST (oToolItem, StateButtonTool):MenuDisplayStyle = StateButtonMenuDisplayStyle:DisplayCheckmark .
                CAST (oToolItem, StateButtonTool):ToolbarDisplayStyle = StateButtonToolbarDisplayStyle:Button .
                CAST (oToolItem, StateButtonTool):Checked = phMenuItem:CHECKED .
            END.
            ELSE
                oToolItem = NEW ButtonTool (cToolName) .

            ASSIGN
                oToolItem:Tag = STRING(phMenuItem)
                oToolItem:SharedProps:Caption = phMenuItem:LABEL
                oToolItem:SharedProps:Enabled = phMenuItem:SENSITIVE .

            /* Mike Fechner, Consultingwerk Ltd. 27.03.2012
               Moved the call to add the new item to the Tools collection
               to before the call to the custom ToolImageProvider class so
               that this class could - if required - call back to
               poToolItem:ToolbarsManager:DockedWithinContainer ... */
            poForm:ToolbarsManager:Tools:Add (oToolItem) .

            &IF DEFINED (WinKit) NE 0 &THEN
            /* Mike Fechner, Consultingwerk Ltd. 24.08.2011
               IToolImageProvider is part of WinKit, so the WinKit is required
               to compile this */

            /* Mike Fechner, Consultingwerk Ltd. 24.08.2011
               When a custom ToolImageProvider is used, this might actuall
               find images for MENU-ITEMs as well */
            IF VALID-OBJECT (oToolImageProvider) THEN DO:
                IF oToolImageProvider:LoadImage (phMenuItem, oToolItem) THEN
                    oToolItem:SharedProps:DisplayStyle = ToolDisplayStyle:ImageOnlyOnToolbars .
                ELSE
                    oToolItem:SharedProps:DisplayStyle = ToolDisplayStyle:TextOnlyAlways .
            END.
            ELSE
            &ELSE
            oToolItem:SharedProps:DisplayStyle = ToolDisplayStyle:TextOnlyInMenus .
            &ENDIF

            IF CAN-QUERY (phMenuItem, "TOOLTIP":U) THEN
                oToolItem:SharedProps:ToolTipText = REPLACE (phMenuItem:TOOLTIP,
                                                             CHR(13),
                                                             System.Environment:NewLine)  .

            IF oToolItem:Key = oToolItem:SharedProps:Caption AND oToolItem:SharedProps:ToolTipText > "":U THEN
                oToolItem:SharedProps:Caption = oToolItem:SharedProps:ToolTipText .
        END.
        ELSE DO:
            oToolItem = CAST(poForm:ToolbarsManager:Tools[cToolName], ButtonTool) .

            /* Mike Fechner, Consultingwerk Ltd. 04.04.2010
               Tag should point to the new MENU-ITEM so that the latest menu item
               used to build this tool is active (received OnChoose event) */

            oToolItem:Tag = STRING (phMenuItem) .
            oToolItem:SharedProps:Enabled = phMenuItem:SENSITIVE .

            IF oToolItem:SharedProps:Caption > "":U THEN .
            ELSE oToolItem:SharedProps:Caption = phMenuItem:LABEL .
        END.

        RETURN oToolItem .

    END METHOD .

    /**
     * Purpose: Creates a State Button Tool for a TOGGLE-BOX widget
     * Notes:   The TOGGLE-BOX needs have a valid name to proceed. Handling of
     *          widgets without a name can be customized using an implementation
     *          of the Consultingwerk.WindowIntegrationKit.IUnnamedWidgetHandler
     *          interface. The default is to raise an error.
     *          Defaults to create no glyph
     * @param phWidget The Widget to create a ButtonTool for
     * @param poToolsCollection The collection (in a Toolbar, a PopupMenuTool, a RibbonGroup, ...) an instance tool should be created
     * @param poForm The Form that owns the UltraToolbarsManager
     * @param pcToolKeySuffix The suffix to append to the tool's key
     * @return The reference to the StateButtonTool that was created for the MENU-ITEM widget
     */
    METHOD PUBLIC STATIC StateButtonTool CreateStateButtonToolFromToggleBox (phWidget AS HANDLE,
                                                                             poToolsCollection AS ToolsCollectionBase,
                                                                             poForm AS IFormWithUltraToolbarsManager,
                                                                             pcToolKeySuffix AS CHARACTER):

        UltraToolbarsHelper:CreateStateButtonToolFromToggleBox (phWidget,
                                                                poToolsCollection,
                                                                poForm,
                                                                pcToolKeySuffix,
                                                                FALSE) .
    END METHOD .

    /**
     * Purpose: Creates a State Button Tool for a TOGGLE-BOX widget
     * Notes:   The TOGGLE-BOX needs have a valid name to proceed. Handling of
     *          widgets without a name can be customized using an implementation
     *          of the Consultingwerk.WindowIntegrationKit.IUnnamedWidgetHandler
     *          interface. The default is to raise an error.
     * @param phWidget The Widget to create a ButtonTool for
     * @param poToolsCollection The collection (in a Toolbar, a PopupMenuTool, a RibbonGroup, ...) an instance tool should be created
     * @param poForm The Form that owns the UltraToolbarsManager
     * @param pcToolKeySuffix The suffix to append to the tool's key
     * @param plUseGlyph Logical value indicating if a Glyph (checkbox) or push down button style StateButtonTool should be created
     * @return The reference to the StateButtonTool that was created for the MENU-ITEM widget
     */
    METHOD PUBLIC STATIC StateButtonTool CreateStateButtonToolFromToggleBox (phWidget AS HANDLE,
                                                                             poToolsCollection AS ToolsCollectionBase,
                                                                             poForm AS IFormWithUltraToolbarsManager,
                                                                             pcToolKeySuffix AS CHARACTER,
                                                                             plUseGlyph AS LOGICAL):

        DEFINE VARIABLE oToolItem             AS StateButtonTool       NO-UNDO .
        DEFINE VARIABLE oToolItemRef          AS StateButtonTool       NO-UNDO .

        DEFINE VARIABLE cToolName             AS CHARACTER             NO-UNDO.
        DEFINE VARIABLE cOwnerName            AS CHARACTER             NO-UNDO.
        &IF DEFINED (WinKit) NE 0 &THEN
        DEFINE VARIABLE oUnnamedWidgetHandler AS IUnnamedWidgetHandler NO-UNDO .
        &ENDIF

        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phWidget WidgetTypeEnum:ToggleBox} .

        /* Mike Fechner, Consultingwerk Ltd. 30.06.2011
           Meaningfull error message when a Widget has no name
           (o.k. for Progress, problem with Infragistics) */
        /* Mike Fechner, Consultingwerk Ltd. 25.11.2011
           Handling of Unnamed widgets not in IUnnamedWidgetHandler */
        IF phWidget:NAME > "":U THEN
            ASSIGN cToolName = phWidget:NAME .
        ELSE DO:
        &IF DEFINED (WinKit) NE 0 &THEN
            ASSIGN oUnnamedWidgetHandler = {Consultingwerk/get-service.i
                                                Consultingwerk.WindowIntegrationKit.IUnnamedWidgetHandler
                                                "NEW UnnamedWidgetHandler ()"} .

            cToolName = oUnnamedWidgetHandler:HandleUnnamedWidget (phWidget,
                                                                   phWidget:INSTANTIATING-PROCEDURE,
                                                                   poForm:ToolbarsManager) .
        &ELSE
            UNDO, THROW NEW AppError (SUBSTITUTE ("Unable to render widget as Infragistics ButtonTool. Name may not be empty.&1" +
                                                       "Widget-Type: &5&1Dynamic Widget: &2&1Creator: &3&1Label: &4",
                                                       System.Environment:NewLine,
                                                       phWidget:DYNAMIC,
                                                       (IF VALID-HANDLE (phWidget:INSTANTIATING-PROCEDURE) THEN phWidget:INSTANTIATING-PROCEDURE:FILE-NAME ELSE "?":U),
                                                       phWidget:LABEL,
                                                       phWidget:TYPE), 0) .
        &ENDIF

        END.

        IF Consultingwerk.Util.UltraToolbarsHelper:PrefixMenuItemsWithMenuName THEN DO:
            IF TYPE-OF (poToolsCollection:Owner, ToolBase) THEN
               ASSIGN cOwnerName = CAST (poToolsCollection:Owner, ToolBase):Key .
            ELSE IF TYPE-OF (poToolsCollection:Owner, RibbonGroup) THEN
               ASSIGN cOwnerName = CAST (poToolsCollection:Owner, RibbonGroup):Key .
            ELSE IF TYPE-OF (poToolsCollection:Owner, UltraToolbar) THEN
               ASSIGN cOwnerName = CAST (poToolsCollection:Owner, UltraToolbar):Key .
            ELSE IF TYPE-OF (poToolsCollection:Owner, ApplicationMenuArea) THEN DO:
                IF poToolsCollection:Owner = poToolsCollection:ToolbarsManager:Ribbon:ApplicationMenu:ToolAreaLeft THEN
                    ASSIGN cOwnerName = "Ribbon:ApplicationMenu:ToolAreaLeft":U .
                ELSE
                    ASSIGN cOwnerName = "Ribbon:ApplicationMenu:ToolAreaRight":U .
            END.

            IF cOwnerName > "":U THEN
                ASSIGN cToolName = cOwnerName + ":":U + cToolName .
        END.

        IF pcToolKeySuffix > "":U THEN
            ASSIGN cToolName = cToolName + pcToolKeySuffix .

        /* Mike Fechner, Consultingwerk Ltd. 12.05.2009
           Only add the base tool item, if not already in the Tool collection
           of the UltraToolbarsManager */
        IF NOT poForm:ToolbarsManager:Tools:Exists (cToolName) THEN DO:
            oToolItem = NEW StateButtonTool (cToolName) .

            oToolItem:MenuDisplayStyle = StateButtonMenuDisplayStyle:DisplayCheckmark .
            oToolItem:ToolbarDisplayStyle = StateButtonToolbarDisplayStyle:Button .
            oToolItem:Checked = phWidget:CHECKED .

            ASSIGN
                oToolItem:Tag = STRING(phWidget)
                oToolItem:SharedProps:Caption = phWidget:LABEL
                oToolItem:SharedProps:Enabled = phWidget:SENSITIVE .

            /* Mike Fechner, Consultingwerk Ltd. 27.03.2012
               Moved the call to add the new item to the Tools collection
               to before the call to the custom ToolImageProvider class so
               that this class could - if required - call back to
               poToolItem:ToolbarsManager:DockedWithinContainer ... */
            poForm:ToolbarsManager:Tools:Add (oToolItem) .

            &IF DEFINED (WinKit) NE 0 &THEN
            /* Mike Fechner, Consultingwerk Ltd. 24.08.2011
               IToolImageProvider is part of WinKit, so the WinKit is required
               to compile this */

            /* Mike Fechner, Consultingwerk Ltd. 24.08.2011
               When a custom ToolImageProvider is used, this might actuall
               find images for MENU-ITEMs as well */
            IF VALID-OBJECT (oToolImageProvider) THEN DO:
                IF oToolImageProvider:LoadImage (phWidget, oToolItem) THEN
                    oToolItem:SharedProps:DisplayStyle = ToolDisplayStyle:ImageOnlyOnToolbars .
                ELSE
                    oToolItem:SharedProps:DisplayStyle = ToolDisplayStyle:TextOnlyAlways .
            END.
            ELSE
            &ELSE
            oToolItem:SharedProps:DisplayStyle = ToolDisplayStyle:TextOnlyInMenus .
            &ENDIF

            IF plUseGlyph THEN
                oToolItem:ToolbarDisplayStyle = StateButtonToolbarDisplayStyle:Glyph .
            ELSE
                oToolItem:ToolbarDisplayStyle = StateButtonToolbarDisplayStyle:Button .

            oToolItem:SharedProps:ToolTipText = REPLACE (phWidget:TOOLTIP,
                                                         CHR(13),
                                                         System.Environment:NewLine)  .

            IF oToolItem:Key = oToolItem:SharedProps:Caption AND oToolItem:SharedProps:ToolTipText > "":U THEN
                oToolItem:SharedProps:Caption = oToolItem:SharedProps:ToolTipText .
        END.
        ELSE DO:
            oToolItem = CAST(poForm:ToolbarsManager:Tools[cToolName], StateButtonTool) .

            /* Mike Fechner, Consultingwerk Ltd. 04.04.2010
               Tag should point to the new MENU-ITEM so that the latest menu item
               used to build this tool is active (received OnChoose event) */

            oToolItem:Tag = STRING (phWidget) .
            oToolItem:SharedProps:Enabled = phWidget:SENSITIVE .

            IF oToolItem:SharedProps:Caption > "":U THEN .
            ELSE oToolItem:SharedProps:Caption = phWidget:LABEL .
        END.

        UltraToolbarsHelper:SetAccelerator (oToolItem, phWidget) .

        IF NOT poToolsCollection:Exists(cToolName) THEN DO:
            oToolItemRef = NEW StateButtonTool (cToolName) .

            oToolItemRef:Tag = STRING(phWidget).

            IF oToolItem:SharedProps:Caption <> phWidget:LABEL THEN
                oToolItemRef:InstanceProps:Caption = phWidget:LABEL .

            poToolsCollection:Add(oToolItemRef) .
        END.

        RETURN oToolItem .

    END METHOD .

    /**
     * Purpose: Creates a PopupMenuTool for an ABL SUB-MENU widget
     * Notes:
     * @param phMenuItem The HANDLE of the ABL SUB-MENU widget
     * @param poToolsCollection The collection (in a Toolbar, a PopupMenuTool, a RibbonGroup, ...) an instance tool should be created
     * @param poForm The Form that owns the UltraToolbarsManager
     * @return The PopupMenuTool that represents the ABL SUB-MENU widget
     */
    METHOD PUBLIC STATIC PopupMenuTool CreateSubMenuPopupMenu (phMenuItem AS HANDLE,
                                                               poToolsCollection AS ToolsCollectionBase,
                                                               poForm AS IFormWithUltraToolbarsManager):

        DEFINE VARIABLE oToolItem             AS PopupMenuTool         NO-UNDO .
        DEFINE VARIABLE oToolItemRef          AS PopupMenuTool         NO-UNDO .
        DEFINE VARIABLE cToolName             AS CHARACTER             NO-UNDO .
&IF DEFINED (WinKit) &THEN
        DEFINE VARIABLE oUnnamedWidgetHandler AS IUnnamedWidgetHandler NO-UNDO .
&ENDIF

        IF phMenuItem:NAME > "":U THEN .
        ELSE DO:
        &IF DEFINED (WinKit) NE 0 &THEN
            ASSIGN oUnnamedWidgetHandler = {Consultingwerk/get-service.i
                                                Consultingwerk.WindowIntegrationKit.IUnnamedWidgetHandler
                                                "NEW UnnamedWidgetHandler ()"} .

            cToolName = oUnnamedWidgetHandler:HandleUnnamedWidget (phMenuItem,
                                                                   phMenuItem:INSTANTIATING-PROCEDURE,
                                                                   poForm:ToolbarsManager) .
        &ELSE
            UNDO, THROW NEW AppError (SUBSTITUTE ("Unable to render widget as Infragistics ButtonTool. Name may not be empty.&1" +
                                                       "Widget-Type: &5&1Dynamic Widget: &2&1Creator: &3&1Label: &4",
                                                       System.Environment:NewLine,
                                                       phMenuItem:DYNAMIC,
                                                       (IF VALID-HANDLE (phMenuItem:INSTANTIATING-PROCEDURE) THEN phMenuItem:INSTANTIATING-PROCEDURE:FILE-NAME ELSE "?":U),
                                                       phMenuItem:LABEL,
                                                       phMenuItem:TYPE), 0) .
        &ENDIF
        END.

        ASSIGN cToolName = "mnu_":U + phMenuItem:NAME .

        IF poForm:ToolbarsManager:Tools:Exists (cToolName) THEN
            oToolItem = CAST(poForm:ToolbarsManager:Tools [cToolName], PopupMenuTool) .
        ELSE
            oToolItem = NEW PopupMenuTool (cToolName) .

        ASSIGN
            oToolItem:Tag = STRING(phMenuItem)
            oToolItem:SharedProps:Caption = phMenuItem:LABEL
            oToolItem:SharedProps:DisplayStyle = ToolDisplayStyle:TextOnlyInMenus
            oToolItem:SharedProps:Enabled = phMenuItem:SENSITIVE .

        IF phMenuItem:NAME = "mnuhelp":U THEN
            oToolItem:SharedProps:MergeOrder = 150 .

        &IF DEFINED (WinKit) NE 0 &THEN
        /* Mike Fechner, Consultingwerk Ltd. 24.08.2011
           ISubmenuCustomizer is part of WinKit, so the WinKit is required
           to compile this */

        /* Mike Fechner, Consultingwerk Ltd. 24.08.2011
           When a custom ToolImageProvider is used, this might actuall
           find images for MENU-ITEMs as well */
        IF VALID-OBJECT (oSubmenuCustomizer) THEN
            oSubmenuCustomizer:AssignPopupMenuToolProperties (phMenuItem,
                                                              oToolItem) .
        &ENDIF

        IF NOT poForm:ToolbarsManager:Tools:Exists (cToolName) THEN
            poForm:ToolbarsManager:Tools:Add(oToolItem) .

        IF VALID-OBJECT (poToolsCollection) AND NOT poToolsCollection:Exists (cToolName) THEN DO:
            oToolItemRef = NEW PopupMenuTool (cToolName) .
            oToolItemRef:Tag = STRING(phMenuItem).

            poToolsCollection:Add(oToolItemRef) .
        END.

        RETURN oToolItem .

    END METHOD.

    /**
     * Purpose: Creates a TextBox Tool (RootTool only) in the UltraToolbarsManager
     *          based on the properties of an ABL Fill-In Widget
     * Notes:
     * @param poToolbarsManager The reference to the UltraToolbarsManager that should contain the TextBoxTool
     * @param phFillIn The HANDLE of the FILL-IN widget that should be the source for the TextBoxTool
     * @return The TextBoxTool that represents the FILL-IN widget
     */
    METHOD PUBLIC STATIC TextBoxTool CreateTextBoxToolFromFillIn (poToolbarsManager AS UltraToolbarsManager,
                                                                  phFillIn AS HANDLE):

        DEFINE VARIABLE oTextBoxTool AS TextBoxTool NO-UNDO .

        /* TextBox Tool */
        IF NOT poToolbarsManager:Tools:Exists(phFillIn:NAME) THEN DO:
            oTextBoxTool = NEW TextBoxTool (phFillIn:NAME).

            poToolbarsManager:Tools:Add (oTextBoxTool) .
        END.
        ELSE
            oTextBoxTool = CAST (poToolbarsManager:Tools[phFillIn:NAME],
                                 TextBoxTool) .

        oTextBoxTool:SharedProps:Caption = phFillIn:LABEL .
        oTextBoxTool:Tag = STRING (phFillIn) .
        oTextBoxTool:Text = phFillIn:SCREEN-VALUE .

        oTextBoxTool:SharedProps:Enabled = phFillIn:SENSITIVE .
        oTextBoxTool:SharedProps:ToolTipText = phFillIn:TOOLTIP .

        RETURN oTextBoxTool .

    END METHOD.

    /**
     * Purpose: Creates a new Tooblar in the UltraToolbar
     * Notes:
     * @param pcToolbarKey The key for the new Toolbar
     * @param poForm The Form owning the UltraToolbarsManager that should create the Toolbar
     * @return The reference to the creates UltraToolbar instance
     */
    METHOD PUBLIC STATIC UltraToolbar CreateToolbar (pcToolbarKey AS CHARACTER,
                                                     poForm AS IFormWithUltraToolbarsManager):

        RETURN CreateToolbar (pcToolbarKey, poForm:ToolbarsManager).

    END METHOD.

    /**
     * Purpose: Creates a new Tooblar in the UltraToolbar
     * Notes:
     * @param pcToolbarKey The key for the new Toolbar
     * @param poToolbarsManager The UltraToolbarsManager that should create the Toolbar
     * @return The reference to the creates UltraToolbar instance
     */
    METHOD PUBLIC STATIC UltraToolbar CreateToolbar (pcToolbarKey AS CHARACTER,
                                                     poToolbarsManager AS UltraToolbarsManager):

        DEFINE VARIABLE oToolbar AS UltraToolbar NO-UNDO.

        IF poToolbarsManager:Toolbars:Exists (pcToolbarKey) THEN
            RETURN poToolbarsManager:Toolbars[pcToolbarKey] .

        oToolbar = NEW UltraToolbar (pcToolbarKey) .

        ASSIGN oToolbar:DockedColumn = 0
               oToolbar:DockedRow = 1
               oToolbar:IsMainMenuBar = FALSE .

        poToolbarsManager:Toolbars:Add(oToolbar) .

        RETURN oToolbar.

    END METHOD.

    /**
     * Purpose: Disables the Tool referenced by the given key
     * Notes:
     * @param poToolbarsManager The reference to the UltraToolbarsManager that contains the Tool
     * @param pcToolKey The key of the Tool in the UltraToolbarsManager Tools Collection
     */
    METHOD PUBLIC STATIC VOID DisableTool (poToolbarsManager AS UltraToolbarsManager,
                                           pcToolKey AS CHARACTER):

        IF NOT poToolbarsManager:Tools:Exists (pcToolKey) THEN
            UNDO, THROW NEW AppError (SUBSTITUTE ("Invalid ToolKey: &1"{&TRAN}, pcToolKey), 0).

        poToolbarsManager:Tools[pcToolKey]:SharedProps:Enabled = FALSE .

    END METHOD.

    /**
     * Purpose: Duplicates a Tool (clones the tool)
     * Notes:   The new key will be based on a substitute expression and &1 for the original tool key
     * @param poTool The tool to clone
     * @param pcKeySubstitute The substitue expression for the tool key
     * @return The ToolBase object which is the duplicate
     */
    METHOD PUBLIC STATIC ToolBase DuplicateTool (poTool AS ToolBase,
                                                 pcKeySubstitute AS CHARACTER):

        DEFINE VARIABLE oNewTool AS Infragistics.Win.UltraWinToolbars.ToolBase NO-UNDO .

        DEFINE VARIABLE oTypes  AS "System.Type[]":U   NO-UNDO.
        DEFINE VARIABLE oValues AS "System.Object[]":U NO-UNDO.

        ASSIGN oTypes  = {Consultingwerk/new-array.i System.Type 1} .
               oValues = {Consultingwerk/new-array.i System.Object 1} .

        oTypes:SetValue (Progress.Util.TypeHelper:GetType ("System.String":U), 0) .
        oValues:SetValue (BOX (SUBSTITUTE (pcKeySubstitute, poTool:Key)), 0).

        oNewTool = CAST (poTool:GetType ():GetConstructor (oTypes):Invoke (oValues),
                         Infragistics.Win.UltraWinToolbars.ToolBase)  .

        ReflectionHelper:CopyInstanceProperties (poTool,
                                                 oNewTool,
                                                 "Key":U) .

        ReflectionHelper:CopyInstanceProperties (poTool:SharedProps,
                                                 oNewTool:SharedProps,
                                                 "":U) .

        ReflectionHelper:CopyInstanceProperties (poTool:SharedProps:AppearancesLarge,
                                                 oNewTool:SharedProps:AppearancesLarge,
                                                 "":U) .

        ReflectionHelper:CopyInstanceProperties (poTool:SharedProps:AppearancesSmall,
                                                 oNewTool:SharedProps:AppearancesSmall,
                                                 "":U) .

        RETURN oNewTool .

    END METHOD .

    /**
     * Purpose: Enables the Tool referenced by the given key
     * Notes:
     * @param poToolbarsManager The reference to the UltraToolbarsManager that contains the Tool
     * @param pcToolKey The key of the Tool in the UltraToolbarsManager Tools Collection
     */
    METHOD PUBLIC STATIC VOID EnableTool (poToolbarsManager AS UltraToolbarsManager,
                                          pcToolKey AS CHARACTER):

        IF NOT poToolbarsManager:Tools:Exists (pcToolKey) THEN
            UNDO, THROW NEW AppError (SUBSTITUTE ("Invalid ToolKey: &1"{&TRAN}, pcToolKey), 0).

        poToolbarsManager:Tools[pcToolKey]:SharedProps:Enabled = TRUE .

    END METHOD.

    /**
     * Purpose: Enables/Disables the left entries of the Ribbon's ApplicationMenu
     * Notes:
     * @param poToolbar The reference to the UltraToolbarsManager that contains the Ribbon
     * @param plEnabled Flag indicating if the Ribbon ApplicationMenu's tools should be enabled
     */
    METHOD PUBLIC STATIC VOID EnableApplicationMenu (poToolbar AS UltraToolbarsManager,
                                                     plEnabled AS LOGICAL):

        {Consultingwerk/foreach.i Infragistics.Win.UltraWinToolbars.ToolBase oToolbase in poToolbar:Ribbon:ApplicationMenu:ToolAreaLeft:Tools}
            ASSIGN oToolbase:SharedProps:Enabled = plEnabled .
        END.

    END METHOD.

    /**
     * Purpose: Enables/Disables the tools in a PopupMenuTool
     * Notes:
     * @param oPopupMenu The reference to the PopupMenuTool that contains the Tools
     * @param plEnabled Flag indicating if the tools should be enabled (true) or disabled (false)
     */
    METHOD PUBLIC STATIC VOID EnablePopupMenu (oPopupMenu AS PopupMenuTool,
                                               plEnabled AS LOGICAL):

        {Consultingwerk/foreach.i ToolBase oTool in oPopupMenu:Tools}
            ASSIGN oTool:SharedProps:Enabled = plEnabled .
        END.

    END METHOD.

    /**
     * Purpose: Enables Tools in an UltraToolbarsManager based on Buttons in a
     *          Frame
     * Notes:
     * @param phFrame The HANDLE of the ABL FRAME WIDGET containing the BUTTON Widgets
     * @param poForm The Form owning the UltraToolbarsManager that contains the ButtonTools
     */
    METHOD PUBLIC STATIC VOID EnableToolsLikeButtons (phFrame AS HANDLE,
                                                      poForm AS IFormWithUltraToolbarsManager):

        IF VALID-OBJECT (poForm) THEN
            EnableToolsLikeButtons (phFrame, poForm:ToolbarsManager) .

    END METHOD .

    /**
     * Purpose: Enabled Tools in an UltraToolbarsManager based on Buttons in a
     *          Frame
     * Notes:
     * @param phFrame The HANDLE of the ABL FRAME WIDGET containing the BUTTON Widgets
     * @param poToolbarsManager The reference to the UltraToolbarsManager that contains the ButtonTools
     */
    METHOD PUBLIC STATIC VOID EnableToolsLikeButtons (phFrame AS HANDLE,
                                                      poToolbarsManager AS UltraToolbarsManager):

        DEFINE VARIABLE hWidget AS HANDLE   NO-UNDO .
        DEFINE VARIABLE oTool   AS ToolBase NO-UNDO .

        IF VALID-HANDLE (phFrame) AND phFrame:TYPE = "FRAME":U THEN .
        ELSE RETURN .

        ASSIGN hWidget = phFrame:FIRST-CHILD:FIRST-CHILD .

        DO WHILE VALID-HANDLE (hWidget):
&IF DEFINED (WinKit) NE 0 &THEN
            IF VALID-OBJECT (oEnableToolsLikeButtonsFilter) AND
               NOT oEnableToolsLikeButtonsFilter:FilterTool (hWidget) THEN DO:
                ASSIGN hWidget = hWidget:NEXT-SIBLING .
                NEXT .
            END.
&ENDIF
            IF hWidget:TYPE = "BUTTON":U AND hWidget:NAME > "":U AND
               poToolbarsManager:Tools:Exists(hWidget:NAME) THEN DO:

                ASSIGN oTool                     = poToolbarsManager:Tools[hWidget:NAME]
                       oTool:SharedProps:Enabled = hWidget:SENSITIVE .

                /* Disable hidden tools */
                IF hWidget:HIDDEN THEN
                    oTool:SharedProps:Enabled = FALSE .
            END.

            ASSIGN hWidget = hWidget:NEXT-SIBLING .
        END .

    END METHOD.

    /**
     * Purpose: Remove all Toolbars, Tools and Toolinstances from an
     *          UltraToolbarsManager
     * Notes:
     * @param poToolbarsManager The reference to the UltraToolbarsManager that contains the Toolbars Collections
     */
    METHOD PUBLIC STATIC VOID EraseToolbarsCollections (poToolbarsManager AS UltraToolbarsManager):

        DEFINE VARIABLE i AS INTEGER NO-UNDO.
        DEFINE VARIABLE j AS INTEGER NO-UNDO.

        IF NOT VALID-OBJECT (poToolbarsManager) THEN
            RETURN .

        DO i = poToolbarsManager:Toolbars:Count - 1 TO 0 BY -1:
            UltraToolbarsHelper:EraseToolsCollection (poToolbarsManager:Toolbars[i]:Tools) .

            DELETE OBJECT poToolbarsManager:Toolbars[i] .
        END.

        DO i = poToolbarsManager:Ribbon:Tabs:Count - 1 TO 0 BY -1:
            DO j = poToolbarsManager:Ribbon:Tabs[i]:Groups:Count - 1 TO 0 BY -1:
                UltraToolbarsHelper:EraseToolsCollection (poToolbarsManager:Ribbon:Tabs[i]:Groups[j]:Tools) .

                DELETE OBJECT poToolbarsManager:Ribbon:Tabs[i]:Groups[j] .
            END.

            poToolbarsManager:Ribbon:Tabs[i]:Groups:Clear () .
            DELETE OBJECT poToolbarsManager:Ribbon:Tabs[i] .
        END.

        poToolbarsManager:Ribbon:Tabs:Clear () .

        UltraToolbarsHelper:EraseToolsCollection (poToolbarsManager:Ribbon:ApplicationMenu:ToolAreaLeft:Tools) .
        UltraToolbarsHelper:EraseToolsCollection (poToolbarsManager:Ribbon:ApplicationMenu:ToolAreaRight:Tools) .
        UltraToolbarsHelper:EraseToolsCollection (poToolbarsManager:Ribbon:QuickAccessToolbar:Tools) .

        UltraToolbarsHelper:EraseToolsCollection (poToolbarsManager:Tools) .

        poToolbarsManager:Toolbars:Clear () .
        poToolbarsManager:Tools:Clear () .

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /**
     * Purpose: Erases a ToolsCollectionBase object by deleting all contained objects
     * Notes:
     * @param poTools The reference to the ToolsCollectionBase object containing the tools that should be deleted
     */
    METHOD PUBLIC STATIC VOID EraseToolsCollection (poTools AS ToolsCollectionBase):

        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        DO i = poTools:Count - 1 TO 0 BY -1:
            DELETE OBJECT poTools:Item[i] .
        END.

        poTools:Clear () .

    END METHOD.

    /**
     * Purpose: Evaluates a IMenuItemFilter implementation
     * Notes:   Allows to ensure that the UltraToolbarsHelper is using a custom
     *          IMenuItemFilter service after it has been registered in the
     *          ServiceContainer
     */
    METHOD PUBLIC STATIC VOID EvaluateMenuItemFilter ():

&IF DEFINED (WinKit) NE 0 &THEN
        oMenuItemFilter = {Consultingwerk/get-service.i Consultingwerk.WindowIntegrationKit.IMenuItemFilter} .
&ENDIF

    END METHOD.

    /**
     * Purpose: Evaluates a IRefreshToolsFilter implementation
     * Notes:   Allows to ensure that the UltraToolbarsHelper is using a custom
     *          IRefreshToolsFilter service after it has been registered in the
     *          ServiceContainer
     */
    METHOD PUBLIC STATIC VOID EvaluateRefreshToolsFilter ():

&IF DEFINED (WinKit) NE 0 &THEN
        oRefreshToolsFilter = {Consultingwerk/get-service.i Consultingwerk.WindowIntegrationKit.IRefreshToolsFilter} .
&ENDIF

    END METHOD.

    /**
     * Purpose: Evaluates an IViewHideToolbarButtonsFilter implementation
     * Notes:   Allows to ensure that the UltraToolbarsHelper is using a custom
     *          IViewHideToolbarButtonsFilter service after it has been registered in the
     *          ServiceContainer
     */
    METHOD PUBLIC STATIC VOID EvaluateViewHideToolbarButtonsFilter ():

&IF DEFINED (WinKit) NE 0 &THEN
        oViewHideToolbarButtonsFilter = {Consultingwerk/get-service.i Consultingwerk.WindowIntegrationKit.IViewHideToolbarButtonsFilter} .
&ENDIF

    END METHOD .

    /**
     * Purpose: Evaluates an ILinkButtonsAndToolsFilter implementation
     * Notes:   Allows to ensure that the UltraToolbarsHelper is using a custom
     *          IViewHideToolbarButtonsFilter service after it has been registered in the
     *          ServiceContainer
     */
    METHOD PUBLIC STATIC VOID EvaluateLinkButtonsAndToolsFilter ():

&IF DEFINED (WinKit) NE 0 &THEN
        oLinkButtonsAndToolsFilter = {Consultingwerk/get-service.i Consultingwerk.WindowIntegrationKit.ILinkButtonsAndToolsFilter} .
&ENDIF

    END METHOD .

    /**
     * Purpose: Evaluates an IHideToolbarButtonsFilter implementation
     * Notes:   Allows to ensure that the UltraToolbarsHelper is using a custom
     *          IViewHideToolbarButtonsFilter service after it has been registered in the
     *          ServiceContainer
     */
    METHOD PUBLIC STATIC VOID EvaluateHideToolbarButtonsFilter ():

&IF DEFINED (WinKit) NE 0 &THEN
        oHideToolbarButtonsFilter = {Consultingwerk/get-service.i Consultingwerk.WindowIntegrationKit.IHideToolbarButtonsFilter} .
&ENDIF

    END METHOD .

    /**
     * Purpose: Evaluates an IEnableToolsLikeButtonsFilter implementation
     * Notes:   Allows to ensure that the UltraToolbarsHelper is using a custom
     *          IViewHideToolbarButtonsFilter service after it has been registered in the
     *          ServiceContainer
     */
    METHOD PUBLIC STATIC VOID EvaluateEnableToolsLikeButtonsFilter ():

&IF DEFINED (WinKit) NE 0 &THEN
        oEnableToolsLikeButtonsFilter = {Consultingwerk/get-service.i Consultingwerk.WindowIntegrationKit.IEnableToolsLikeButtonsFilter} .
&ENDIF

    END METHOD .

    /**
     * Purpose: Evaluates an IBuildRibbonGroupFromFrameFilter implementation
     * Notes:   Allows to ensure that the UltraToolbarsHelper is using a custom
     *          IViewHideToolbarButtonsFilter service after it has been registered in the
     *          ServiceContainer
     */
    METHOD PUBLIC STATIC VOID EvaluateBuildRibbonGroupFromFrameFilter ():

&IF DEFINED (WinKit) NE 0 &THEN
        oBuildRibbonGroupFromFrameFilter = {Consultingwerk/get-service.i Consultingwerk.WindowIntegrationKit.IBuildRibbonGroupFromFrameFilter} .
&ENDIF

    END METHOD .

    /**
     * Purpose: Evaluates an IBuildToolbarFromFrameFilter  implementation
     * Notes:   Allows to ensure that the UltraToolbarsHelper is using a custom
     *          IViewHideToolbarButtonsFilter service after it has been registered in the
     *          ServiceContainer
     */
    METHOD PUBLIC STATIC VOID EvaluateBuildToolbarFromFrameFilter ():

&IF DEFINED (WinKit) NE 0 &THEN
        oBuildToolbarFromFrameFilter = {Consultingwerk/get-service.i Consultingwerk.WindowIntegrationKit.IBuildToolbarFromFrameFilter} .
&ENDIF

    END METHOD .

    /**
     * Purpose: Locates the first visible Toolbar that is not a menu-bar
     * Notes:
     * @param poToolbarsManager The reference to the UltraToolbarsManager that contains the Toolbar
     * @return The reference to the first visible UltraToolbar object of the UltraToolbarsManager
     */
    METHOD PUBLIC STATIC UltraToolbar FindFirstToolbar (poToolbarsManager AS UltraToolbarsManager):

        DEFINE VARIABLE oToolbar AS UltraToolbar NO-UNDO .
        DEFINE VARIABLE i        AS INTEGER      NO-UNDO .

        DO i = 0 TO poToolbarsManager:Toolbars:Count - 1:

            oToolbar = poToolbarsManager:Toolbars [i] .

            IF NOT oToolbar:IsMainMenuBar AND oToolbar:Visible THEN
                RETURN oToolbar .
        END.

        RETURN ? .

    END METHOD.

    /**
     * Purpose: Returns the reference to an UltraToolbarsManager ToolBase instance
     *          that represents a Progress Widget by comparing the Tag property
     *          of the Tool to the widget handle
     * Notes:
     * @param phWidget The associated widget handle
     * @param poToolbarsManager The UltraToolbarsManager that manages to tools
     * @return The ToolBase the represents the widget
     */
    METHOD PUBLIC STATIC ToolBase FindToolFromWidgetHandle (phWidget AS HANDLE,
                                                            poToolbarsManager AS UltraToolbarsManager):

        {Consultingwerk/Assertion/HandleAssert/ValidHandle.i phWidget} .
        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poToolbarsManager} .

        {Consultingwerk/foreach.i ToolBase oTool in poToolbarsManager:Tools}

             IF VALID-OBJECT (oTool:Tag) AND UNBOX (oTool:Tag) = phWidget THEN
                 RETURN oTool .
        END.

        RETURN ? .

    END METHOD.

    /**
     * Purpose: Returns the reference to the ButtonTool for the given key
     * Notes:
     * @param poToolbarsManager The reference to the UltraToolbarsManager that contains the StateButton Tool
     * @param pcToolKey The key of the Button Tool in the UltraToolbarsManager Tools Collection
     * @return Object of type ButtonTool for the given key
     */
    METHOD PUBLIC STATIC ButtonTool GetButtonTool (poToolbarsManager AS UltraToolbarsManager,
                                                   pcToolKey AS CHARACTER):

        IF NOT poToolbarsManager:Tools:Exists (pcToolKey) THEN
            UNDO, THROW NEW AppError (SUBSTITUTE ("Invalid ToolKey: &1"{&TRAN}, pcToolKey), 0).

        IF NOT TYPE-OF (poToolbarsManager:Tools[pcToolKey],
                        ButtonTool) THEN
            UNDO, THROW NEW AppError (SUBSTITUTE ("Tool &1 is not a ButtonTool."{&TRAN}, pcToolKey), 0).

        RETURN CAST (poToolbarsManager:Tools[pcToolKey],
                     ButtonTool).

    END METHOD.

    /**
     * Purpose: Returns the reference to the MainMenuBar of the passed in UltraToolbarsManager
     *          instance
     * Notes:
     * @param poToolbarsManager The UltraToolbarsManager that contains the MainMenuBar
     * @return The reference to the MainMenuBar with the IsMainMenuBar flaf set to true of ? when there is none
     */
    METHOD PUBLIC STATIC UltraToolbar GetMainMenuToolbar (poToolbarsManager AS UltraToolbarsManager):

        {Consultingwerk/foreach.i UltraToolbar oToolbar in poToolbarsManager:Toolbars}

            IF oToolbar:IsMainMenuBar THEN
                RETURN oToolbar .
        END.

        RETURN ? .

    END METHOD.

    /**
     * Purpose:  Returns a comma-delimited list of Tools of the current Ribbon
     *           QuickAccessTools
     * Notes:
     * @param poToolbarsManager The reference to the UltraToolbarsManager instance
     * @return The comma delimited list (CHARACTER) of tool keys on the Ribbon Quick Access Toolbar
     */
    METHOD PUBLIC STATIC CHARACTER GetQuickAccessToolbarTools (poToolbarsManager AS UltraToolbarsManager):

        DEFINE VARIABLE cList AS CHARACTER NO-UNDO.

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poToolbarsManager} .

        {Consultingwerk/foreach.i ToolBase oTool in poToolbarsManager:Ribbon:QuickAccessToolbar:Tools}
            ASSIGN cList = cList + ",":U + oTool:Key .
        END .

        RETURN TRIM (cList, ",":U) .

    END METHOD.

    /**
     * Purpose: Returns a reference to the StateButtonTool for the given key
     * Notes:
     * @param poToolbarsManager The reference to the UltraToolbarsManager that contains the StateButton Tool
     * @param pcToolKey The key of the StateButton Tool in the UltraToolbarsManager Tools Collection
     * @return Object of type StateButtonTool for the given key
     */
    METHOD PUBLIC STATIC StateButtonTool GetStateButtonTool (poToolbarsManager AS UltraToolbarsManager,
                                                             pcToolKey AS CHARACTER):

        IF NOT poToolbarsManager:Tools:Exists (pcToolKey) THEN
            UNDO, THROW NEW AppError (SUBSTITUTE ("Invalid ToolKey: &1"{&TRAN}, pcToolKey), 0).

        IF NOT TYPE-OF (poToolbarsManager:Tools[pcToolKey],
                        StateButtonTool) THEN
            UNDO, THROW NEW AppError (SUBSTITUTE ("Tool &1 is not a StateButtonTool."{&TRAN}, pcToolKey), 0).

        RETURN CAST (poToolbarsManager:Tools[pcToolKey],
                     StateButtonTool) .

  END METHOD.

    /**
     * Purpose: Returns the Checked property of a StateButtonTool
     * Notes:
     * @param poToolbarsManager The reference to the UltraToolbarsManager that contains the StateButton Tool
     * @param pcToolKey The key of the StateButton Tool in the UltraToolbarsManager Tools Collection
     * @return Logical value indicating of the StateButtonTool is checked or not
     */
    METHOD PUBLIC STATIC LOGICAL GetStateButtonToolChecked (poToolbarsManager AS UltraToolbarsManager,
                                                            pcToolKey AS CHARACTER):

        DEFINE VARIABLE oStateButtonTool AS StateButtonTool NO-UNDO .

        IF NOT poToolbarsManager:Tools:Exists (pcToolKey) THEN
            UNDO, THROW NEW AppError (SUBSTITUTE ("Invalid ToolKey: &1"{&TRAN}, pcToolKey), 0) .

        IF NOT TYPE-OF (poToolbarsManager:Tools[pcToolKey],
                        StateButtonTool) THEN
            UNDO, THROW NEW AppError (SUBSTITUTE ("Tool &1 is not a StateButtonTool."{&TRAN}, pcToolKey), 0) .

        oStateButtonTool = CAST (poToolbarsManager:Tools[pcToolKey],
                                 StateButtonTool) .

        RETURN oStateButtonTool:Checked .

    END METHOD.

    /**
     * Purpose: Returns the selected option (key) of a StateButtonTool's optionset
     * Notes:
     * @param poToolbarsManager The reference to the UltraToolbarsManager that contains the StateButton Tool
     * @param pcToolKey The key of the StateButton Tool in the UltraToolbarsManager Tools Collection
     * @return Character value indicating the key of the selected option
     */
    METHOD PUBLIC STATIC CHARACTER GetStateButtonToolSelectedKey (poToolbarsManager AS UltraToolbarsManager,
                                                                  pcToolKey AS CHARACTER):

        DEFINE VARIABLE oStateButtonTool AS StateButtonTool NO-UNDO .

        oStateButtonTool = GetStateButtonTool (poToolbarsManager, pcToolKey).

         IF NOT VALID-OBJECT (oStateButtonTool:OptionSet) THEN
             UNDO, THROW NEW AppError (SUBSTITUTE ("Tool &1 is not member of an OptionSet."{&TRAN}, pcToolKey), 0) .

        RETURN oStateButtonTool:OptionSet:SelectedTool:Key.

    END METHOD.

    /**
     * Purpose: Hides a physical Toolbar within the Toolbar Container
     * Notes:
     * @param phFrame The HANDLE of the ABL FRAME widget that contains the BUTTON Widgets
     * @param poForm The reference to the Form with the UltraToolbarsManager
     * @param plHide Flag indicating if the Toolbar object should be hidden
     */
    METHOD PUBLIC STATIC VOID HideToolbarFrame (phFrame AS HANDLE,
                                                poForm  AS IFormWithUltraToolbarsManager,
                                                plHide  AS LOGICAL):

        HideToolbarFrame (phFrame,
                          poForm:ToolbarsManager,
                          plHide) .

    END METHOD.

    /**
     * Purpose: Hides a physical Toolbar within the Toolbar Container
     * Notes:
     * @param phFrame The HANDLE of the ABL FRAME widget that contains the BUTTON Widgets
     * @param poToolbarsManager The reference to the UltraToolbarsManager that contains the Toolbar
     * @param plHide Flag indicating if the Toolbar object should be hidden
     */
    METHOD PUBLIC STATIC VOID HideToolbarFrame (phFrame AS HANDLE,
                                                poToolbarsManager AS UltraToolbarsManager,
                                                plHide AS LOGICAL):

        IF poToolbarsManager:Toolbars:Exists ("frame":U + STRING(phFrame)) THEN
            poToolbarsManager:Toolbars ["frame":U + STRING(phFrame)]:VISIBLE = NOT plHide.

    END METHOD.

    /**
     * Purpose: Hides Toolbars Buttons when the buttons in the frame with the
     *          Same name are not visible or hidden.
     * Notes:
     * @param phFrame The HANDLE of the ABL FRAME widget that contains the BUTTON Widgets
     * @param poToolbarsManager The reference to the UltraToolbarsManager that contains the Tools
     */
    METHOD PUBLIC STATIC VOID HideToolbarButtons (phFrame AS HANDLE,
                                                  poToolbarsManager AS UltraToolbarsManager):

        DEFINE VARIABLE hWidget          AS HANDLE              NO-UNDO .
        DEFINE VARIABLE oTool            AS ToolBase            NO-UNDO .
        DEFINE VARIABLE i                AS INTEGER             NO-UNDO .
        DEFINE VARIABLE oToolInstance    AS ToolBase            NO-UNDO .
        DEFINE VARIABLE oToolsCollection AS ToolsCollectionBase NO-UNDO .

        IF VALID-HANDLE (phFrame) AND phFrame:TYPE = "FRAME":U THEN .
        ELSE RETURN .

        ASSIGN hWidget = phFrame:FIRST-CHILD:FIRST-CHILD .

        DO WHILE VALID-HANDLE (hWidget):
&IF DEFINED (WinKit) NE 0 &THEN
            IF VALID-OBJECT (oHideToolbarButtonsFilter) AND
               NOT oHideToolbarButtonsFilter:FilterTool (hWidget) THEN DO:
                ASSIGN hWidget = hWidget:NEXT-SIBLING .
                NEXT .
            END.
&ENDIF
            IF hWidget:TYPE = "BUTTON":U AND hWidget:NAME > "":U AND
               poToolbarsManager:Tools:Exists (hWidget:NAME) THEN DO:

                IF hWidget:HIDDEN OR NOT hWidget:VISIBLE THEN DO:
                    ASSIGN oTool                     = poToolbarsManager:Tools[hWidget:NAME]
                           oTool:SharedProps:Visible = FALSE .

                    DO i = (oTool:SharedProps:ToolInstances:Count - 1) TO 0 BY -1:

                        ASSIGN oToolInstance = oTool:SharedProps:ToolInstances [i] .

                        /* Mike Fechner, Consultingwerk Ltd. 15.12.2009
                           Tool instances on a ribbon need to be deleted */
                        IF oToolInstance:OwnerIsRibbonGroup THEN DO:
                            oToolsCollection = oToolInstance:ParentCollection .

                            oToolsCollection:Remove (oToolInstance) .

                            IF oToolsCollection:Count = 0 AND TYPE-OF (oToolsCollection:Owner, RibbonGroup) THEN
                                CAST(oToolsCollection:Owner, RibbonGroup):Visible = FALSE .
                        END.
                    END.
                END.
            END.

            ASSIGN hWidget = hWidget:NEXT-SIBLING .
        END .

    END METHOD.

    /**
     * Purpose: Hides Toolbars Buttons when the buttons in the frame with the
     *          Same name are not visible or hidden.
     * Notes:
     * @param phFrame The HANDLE of the ABL FRAME widget that contains the BUTTON Widgets
     * @param poForm The Form owning the UltraToolbarsManager that contains the Tools
     */
    METHOD PUBLIC STATIC VOID HideToolbarButtons (phFrame AS HANDLE,
                                                  poForm AS IFormWithUltraToolbarsManager):

        HideToolbarButtons (phFrame,
                            poForm:ToolbarsManager) .

    END METHOD.

    /**
     * Purpose: Evaluates a custom IToolImageProvider implementation
     * Notes:   Allows to ensure that the UltraToolbarsHelper is using a custom
     *          IToolImageProvider service after it has been registered in the
     *          ServiceContainer
     */
    METHOD PUBLIC STATIC VOID EvaluateCustomToolImageProvider ():

&IF DEFINED (WinKit) NE 0 AND DEFINED (DotNetAccessible) NE 0 &THEN
        ASSIGN oToolImageProvider = {Consultingwerk/get-service.i Consultingwerk.WindowIntegrationKit.IToolImageProvider} .
&ENDIF
    END METHOD.

    /**
     * Purpose: Evaluates a custom ICustomToolbarWidgetRenderer implementation
     * Notes:   Allows to ensure that the UltraToolbarsHelper is using a custom
     *          ICustomToolbarWidgetRenderer service after it has been registered in the
     *          ServiceContainer
     */
    METHOD PUBLIC STATIC VOID EvaluateCustomWidgetRenderer ():

&IF DEFINED (WinKit) NE 0 AND DEFINED (DotNetAccessible) NE 0 &THEN
        ASSIGN oCustomWidgetRenderer = {Consultingwerk/get-service.i Consultingwerk.WindowIntegrationKit.ICustomToolbarWidgetRenderer} .
&ENDIF
    END METHOD.

    /**
     * Purpose: Evaluates a custom IToolImageProvider implementation
     * Notes:   Allows to ensure that the UltraToolbarsHelper is using a custom
     *          IToolImageProvider service after it has been registered in the
     *          ServiceContainer
     */
    METHOD PUBLIC STATIC VOID EvaluateSubmenuCustomizer ():

&IF DEFINED (WinKit) NE 0 AND DEFINED (DotNetAccessible) NE 0 &THEN
        ASSIGN oSubmenuCustomizer = {Consultingwerk/get-service.i Consultingwerk.WindowIntegrationKit.ISubmenuCustomizer} .
&ENDIF
    END METHOD.

    /**
     * Purpose: Evaluates a custom IRibbonButtonCustomizer implementation
     * Notes:   Allows to ensure that the UltraToolbarsHelper is using a custom
     *          IRibbonButtonCustomizer service after it has been registered in the
     *          ServiceContainer
     */
    METHOD PUBLIC STATIC VOID EvaluateRibbonButtonCustomizer ():

&IF DEFINED (WinKit) NE 0 AND DEFINED (DotNetAccessible) NE 0 &THEN
        ASSIGN oRibbonButtonCustomizer = {Consultingwerk/get-service.i Consultingwerk.WindowIntegrationKit.IRibbonButtonCustomizer} .
&ENDIF
    END METHOD.

    /**
     * Purpose: Links Buttons with Tools in an UltraTooblarsManager
     * Notes:   Link is established by assigning the Tag property of the Tool
     *          the Handle of the Button. Optionally the Button will be hidden.
     * @param pcButtonList A comma-delimited list of button handles to link
     * @param poToolbarsManager The UltraToolbarsManager instance to link the buttons to
     * @param plHideButtons True to hide the buttons, False to leave the buttons unchanged
     */
    METHOD PUBLIC STATIC VOID LinkButtonsAndTools (pcButtonList AS CHARACTER,
                                                   poToolbarsManager AS UltraToolbarsManager,
                                                   plHideButtons AS LOGICAL):

        DEFINE VARIABLE hWidget AS HANDLE   NO-UNDO .
        DEFINE VARIABLE i       AS INTEGER  NO-UNDO.
        DEFINE VARIABLE oTool   AS ToolBase NO-UNDO .

        DO i = 1 TO NUM-ENTRIES (pcButtonList):
            ASSIGN hWidget = WIDGET-HANDLE (ENTRY (i, pcButtonList)) .

&IF DEFINED (WinKit) NE 0 &THEN
            IF VALID-OBJECT (oLinkButtonsAndToolsFilter) AND
               NOT oLinkButtonsAndToolsFilter:FilterTool (hWidget) THEN DO:
                ASSIGN hWidget = hWidget:NEXT-SIBLING .
                NEXT .
            END.
&ENDIF

            IF VALID-HANDLE (hWidget) AND hWidget:TYPE = "BUTTON":U AND hWidget:NAME > "":U AND
               poToolbarsManager:Tools:Exists (hWidget:NAME) THEN DO:

                ASSIGN oTool                     = poToolbarsManager:Tools[hWidget:NAME]
                       oTool:Tag                 = STRING(hWidget)
                       oTool:SharedProps:Enabled = hWidget:SENSITIVE .

                IF plHideButtons THEN
                    /* Mike Fechner, Consultingwerk Ltd. 10.11.2010
                       1*1 size of buttons works like hiding the button + HIDDEN / VISIBLE
                       can still be used for existing program logic */
                    /* Mike Fechner, Consultingwerk Ltd. 14.03.2016
                       WINKIT-17 - Buttons should by moved to top left as well */
                    ASSIGN hWidget:WIDTH-PIXELS  = 1
                           hWidget:HEIGHT-PIXELS = 1
                           hWidget:X             = 1
                           hWidget:Y             = 1
                           hWidget:TAB-STOP      = FALSE.  /*DG01 remove tabstop attrib*/
            END.
        END .

    END METHOD.

    /**
     * Purpose: Links Buttons in a Frame with Tools in an UltraTooblarsManager
     * Notes:   Link is established by assigning the Tag property of the Tool
     *          the Handle of the Button. By default the Button will be hidden.
     * @param pcButtonList A comma-delimited list of button handles to link
     * @param poToolbarsManager The UltraToolbarsManager instance to link the buttons to
     */
    METHOD PUBLIC STATIC VOID LinkButtonsAndTools (pcButtonList AS CHARACTER,
                                                   poToolbarsManager AS UltraToolbarsManager):

        LinkButtonsAndTools (pcButtonList, poToolbarsManager, TRUE) .

    END METHOD.

    /**
     * Purpose: Links Buttons in a Frame with Tools in an UltraTooblarsManager
     * Notes:   Link is established by assigning the Tag property of the Tool
     *          the Handle of the Button. Optionally the Button will be hidden.
     * @param phFrame The HANDLE of the ABL FRAME widget that contains the BUTTON Widgets
     * @param poToolbarsManager The reference to the UltraToolbarsManager that contains the Toolbar
     * @param plHideButtons Flag indicating if the ABL BUTTON widgets should be hidden after linking them
     */
    METHOD PUBLIC STATIC VOID LinkButtonsAndTools (phFrame AS HANDLE,
                                                   poToolbarsManager AS UltraToolbarsManager,
                                                   plHideButtons AS LOGICAL):

        DEFINE VARIABLE hWidget AS HANDLE   NO-UNDO .
        DEFINE VARIABLE oTool   AS ToolBase NO-UNDO .

        IF VALID-HANDLE (phFrame) AND phFrame:TYPE = "FRAME":U THEN . /* if type = frame then continue on!!! otherwise exit */
        ELSE RETURN .

        ASSIGN hWidget = phFrame:FIRST-CHILD:FIRST-CHILD .

        DO WHILE VALID-HANDLE (hWidget) ON ERROR UNDO, THROW:
&IF DEFINED (WinKit) NE 0 &THEN
            IF VALID-OBJECT (oLinkButtonsAndToolsFilter) AND
               NOT oLinkButtonsAndToolsFilter:FilterTool (hWidget) THEN DO:
                ASSIGN hWidget = hWidget:NEXT-SIBLING .
                NEXT .
            END.
&ENDIF
            IF hWidget:TYPE = "BUTTON":U AND hWidget:NAME > "":U THEN
                IF poToolbarsManager:Tools:Exists(hWidget:NAME) THEN DO:

                ASSIGN oTool                     = poToolbarsManager:Tools[hWidget:NAME]
                       oTool:Tag                 = STRING(hWidget)
                       oTool:SharedProps:Enabled = hWidget:SENSITIVE .

                IF plHideButtons THEN
                    /* Mike Fechner, Consultingwerk Ltd. 10.11.2010
                       1*1 size of buttons works like hiding the button + HIDDEN / VISIBLE
                       can still be used for existing program logic */
                    /* Mike Fechner, Consultingwerk Ltd. 14.03.2016
                       WINKIT-17 - Buttons should by moved to top left as well */
                    ASSIGN hWidget:WIDTH-PIXELS  = 1
                           hWidget:HEIGHT-PIXELS = 1
                           hWidget:X             = 1
                           hWidget:Y             = 1
                           hWidget:TAB-STOP      = FALSE.  /*DG01 remove tabstop attrib*/
            END.

            ASSIGN hWidget = hWidget:NEXT-SIBLING .
        END .

    END METHOD.

    /**
     * Purpose: Links Buttons in a Frame with Tools in an UltraTooblarsManager
     * Notes:   Link is established by assigning the Tag property of the Tool
     *          the Handle of the Button. By default the Button will be hidden.
     * @param phFrame The HANDLE of the ABL FRAME widget that contains the BUTTON Widgets
     * @param poToolbarsManager The reference to the UltraToolbarsManager that contains the Toolbar
     */
    METHOD PUBLIC STATIC VOID LinkButtonsAndTools (phFrame AS HANDLE,
                                                   poToolbarsManager AS UltraToolbarsManager):

        LinkButtonsAndTools (phFrame,poToolbarsManager, TRUE) .

    END METHOD.

    /**
     * Purpose: Links Buttons in a Frame with Tools in an UltraTooblarsManager
     * Notes:   Link is established by assigning the Tag property of the Tool
     *          the Handle of the Button. Optionally the Button will be hidden.
     * @param phFrame The HANDLE of the ABL FRAME widget that contains the BUTTON Widgets
     * @param poForm The Form owning the UltraToolbarsManager that contains the Tools
     * @param plHideButtons Flag indicating if the ABL BUTTON widgets should be hidden after linking them
     */
    METHOD PUBLIC STATIC VOID LinkButtonsAndTools (phFrame AS HANDLE,
                                                   poForm AS IFormWithUltraToolbarsManager,
                                                   plHideButtons AS LOGICAL):

        LinkButtonsAndTools (phFrame, poForm:ToolbarsManager, plHideButtons) .

    END METHOD .

    /**
     * Purpose: Links Buttons in a Frame with Tools in an UltraTooblarsManager
     * Notes:   Link is established by assigning the Tag property of the Tool
     *          the Handle of the Button. By default the Button will be hidden.
     *          Defaults to Hiding the ABL Buttons
     * @param phFrame The HANDLE of the ABL FRAME widget that contains the BUTTON Widgets
     * @param poForm The Form owning the UltraToolbarsManager that contains the Tools
     */
    METHOD PUBLIC STATIC VOID LinkButtonsAndTools (phFrame AS HANDLE,
                                                   poForm AS IFormWithUltraToolbarsManager):

        LinkButtonsAndTools (phFrame,poForm:ToolbarsManager, TRUE) .

    END METHOD.


    /**
     * Purpose: Links Menu Items in a SUB-MENU with Tools in an UltraTooblarsManager
     * Notes:   Link is established by assigning the Tag property of the Tool
     *          the Handle of the Button. By default the Button will be hidden.
     * @param phMenu The HANDLE of the ABL SUB-MENU Widget
     * @param poToolbarsManager The reference to the UltraToolbarsManager that contains the Tools
     */
    METHOD PUBLIC STATIC VOID LinkContextMenuItems (phMenu AS HANDLE,
                                                    poToolbarsManager AS UltraToolbarsManager):

        DEFINE VARIABLE hWidget AS HANDLE   NO-UNDO .
        DEFINE VARIABLE oTool   AS ToolBase NO-UNDO .

        IF VALID-HANDLE (phMenu) AND (phMenu:TYPE = "MENU":U OR hWidget:TYPE = "SUBMENU":U) THEN .
        ELSE RETURN .

        ASSIGN hWidget = phMenu:FIRST-CHILD .

        DO WHILE VALID-HANDLE (hWidget):
            IF hWidget:TYPE = "MENU-ITEM":U AND hWidget:NAME > "":U THEN DO:

                IF hWidget:SUBTYPE = "RULE":U THEN DO:
                    hWidget = hWidget:NEXT-SIBLING .

                    NEXT .
                END.

                IF NOT poToolbarsManager:Tools:Exists(hWidget:NAME) THEN DO:
                    hWidget = hWidget:NEXT-SIBLING .

                    NEXT .
                END.

                ASSIGN oTool                     = poToolbarsManager:Tools[hWidget:NAME]
                       oTool:Tag                 = STRING(hWidget)
                       oTool:SharedProps:Enabled = hWidget:SENSITIVE .
            END.
            ELSE IF hWidget:TYPE = "MENU":U OR hWidget:TYPE = "SUBMENU":U THEN
                LinkContextMenuItems (hWidget, poToolbarsManager) .

            ASSIGN hWidget = hWidget:NEXT-SIBLING .
        END .
    END METHOD.

    /**
     * Purpose: Links Menu Items in a SUB-MENU with Tools in an UltraTooblarsManager
     * Notes:   Link is established by assigning the Tag property of the Tool
     *          the Handle of the Button. By default the Button will be hidden.
     * @param phMenu The HANDLE of the ABL SUB-MENU Widget
     * @param poForm The Form owning the UltraToolbarsManager that contains the Tools
     */
    METHOD PUBLIC STATIC VOID LinkContextMenuItems (phMenu AS HANDLE,
                                                    poForm AS IFormWithUltraToolbarsManager):

        LinkContextMenuItems (phMenu, poForm:ToolbarsManager) .

    END METHOD.

    /**
     * Purpose: Standard routine to load an image to a Tool
     * Notes:   Can be customized using an IToolImageProvider Implementation.
     *          When called from a custom service, the plUseCustomService parameter
     *          can be used to avoid infinitive loops
     * @param phWidget The HANDLE of the ABL WIDGET that contains the image
     * @param poButtonTool The ToolBase reference that should receive the image
     * @param plUseCustomService Flag that indicates if the ToolImageProvider should be used to load the images
     * @return Logical value indicating if an image was loaded
     */
    METHOD PUBLIC STATIC LOGICAL LoadImage (phWidget AS HANDLE,
                                            poButtonTool AS ToolBase,
                                            plUseCustomService AS LOGICAL):

        DEFINE VARIABLE oImage  AS System.Drawing.Bitmap NO-UNDO .
        DEFINE VARIABLE lReturn AS LOGICAL               NO-UNDO INIT FALSE .

        DEFINE VARIABLE oIcon   AS System.Drawing.Icon   NO-UNDO .

        &IF DEFINED (WinKit) NE 0 &THEN
        /* Mike Fechner, Consultingwerk Ltd. 24.08.2011
           IToolImageProvider is part of WinKit, so the WinKit is required
           to compile this (which is not available when just the core framework
           classes are provided) */

        /* Mike Fechner, Consultingwerk Ltd. 24.08.2011
           As the custom service might call back into the UltraToolbarsHelper:LoadImage
           for default handling, plUseCustomService is a way to avoid infinitive loops */
        IF plUseCustomService AND VALID-OBJECT (oToolImageProvider) THEN DO:
            ASSIGN lReturn = oToolImageProvider:LoadImage (phWidget, poButtonTool) .

            IF VALID-OBJECT (poButtonTool:SharedProps:AppearancesSmall:Appearance) AND
               VALID-OBJECT (poButtonTool:SharedProps:AppearancesSmall:Appearance:Image) AND
               /* Image may be System.String -> key in ImageList */
               TYPE-OF (poButtonTool:SharedProps:AppearancesSmall:Appearance:Image,
                        System.Drawing.Image) THEN

               /* Mike Fechner, Consultingwerk Ltd. 09.09.2011
                  Store the ABL widget's IMAGE (filename) in the Tag of the .NET Image */
               CAST(poButtonTool:SharedProps:AppearancesSmall:Appearance:Image,
                    System.Drawing.Image):Tag = phWidget:IMAGE .

            RETURN lReturn .
        END.
        &ENDIF

        IF NOT CAN-QUERY (phWidget, "IMAGE":U) THEN
            RETURN FALSE .

        IF phWidget:IMAGE > "":U THEN DO:
            FILE-INFO:FILE-NAME = phWidget:IMAGE .

            IF FILE-INFO:FULL-PATHNAME > "":U AND

               /* Mike Fechner, Consultingwerk Ltd. 10.07.2009
                  .NET routines cannot load images from procedure library */
               NOT FILE-INFO:FULL-PATHNAME MATCHES "*<<*":U THEN DO:

                IF phWidget:IMAGE MATCHES "*.ico":U THEN DO:
                    oIcon = NEW System.Drawing.Icon (FILE-INFO:FULL-PATHNAME) .

                    ASSIGN oImage = oIcon:ToBitmap () .
                END.
                ELSE
                    oImage = Consultingwerk.Util.ImageHelper:MakeTransparent(Consultingwerk.Util.ImageHelper:Load (FILE-INFO:FULL-PATHNAME)).

                poButtonTool:SharedProps:AppearancesSmall:Appearance = NEW Infragistics.Win.Appearance () .

                oImage:Tag = BOX (phWidget:IMAGE) .

                poButtonTool:SharedProps:AppearancesSmall:Appearance:Image = oImage .

                lReturn = TRUE .
            END.
            ELSE
                poButtonTool:SharedProps:DisplayStyle = ToolDisplayStyle:TextOnlyAlways .
        END.
        ELSE
            poButtonTool:SharedProps:DisplayStyle = ToolDisplayStyle:TextOnlyAlways .

        RETURN lReturn .

    END METHOD.

    /**
     * Purpose: Adds all Ribbon Tabs of the UltraToolbarsManager to the first
     *          Contextual Tab Group of the UltraToolbarsManager
     * Notes:   Useful to conditionally make Ribbon Tabs contextual (MDI Child)
     * @param poToolbarsManager The reference to the UltraToolbarsManager
     */
    METHOD PUBLIC STATIC VOID MakeTabsContextual (poToolbarsManager AS UltraToolbarsManager):

        DEFINE VARIABLE oContextualRibbonTab AS ContextualTabGroup NO-UNDO .

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poToolbarsManager """poToolbarsManager"":U"} .

        IF poToolbarsManager:Ribbon:ContextualTabGroups:Count = 0 THEN
            RETURN .

        ASSIGN oContextualRibbonTab = poToolbarsManager:Ribbon:ContextualTabGroups[0]
               oContextualRibbonTab:Visible = TRUE .

        {Consultingwerk/foreach.i RibbonTab oTab in poToolbarsManager:Ribbon:Tabs}

            IF NOT VALID-OBJECT (oTab:ContextualTabGroup) THEN
                oContextualRibbonTab:Tabs:Add (oTab) .

        END.

    END METHOD.

    /**
     * Purpose: Merges the contextual Tabs of one UltraToolbarsManager instance into
     *          another
     * Notes:   See the method CleanUpMergedContextualTabGroups for the cleaning up of
     *          the Tools, Tabs and ContextTualTabGroups instantiated by this routine
     * @param poSourceToolbarsManager The source UltraToolbarsManager instance to copy the contextual Tabs from
     * @param poTargetToolbarsManager The target UltraToolbarsManager instance to copy the contextual Tabs to
     * @param pcPrefix The prefix to use for naming the keys for the ContextualTabGroup, the Tabs, the Groups and the Tools
     * @return An System.Collections.IList containing the references to the generated objects for later clean up
     */
    METHOD PUBLIC STATIC System.Collections.IList MergeContextualTabGroups (poSourceToolbarsManager AS UltraToolbarsManager,
                                                                            poTargetToolbarsManager AS UltraToolbarsManager,
                                                                            pcPrefix AS CHARACTER):

        DEFINE VARIABLE oNewTabGroup    AS ContextualTabGroup NO-UNDO .
        DEFINE VARIABLE oNewRibbonTab   AS RibbonTab          NO-UNDO .
        DEFINE VARIABLE oNewRibbonGroup AS RibbonGroup        NO-UNDO .
        DEFINE VARIABLE oNewTool        AS ToolBase           NO-UNDO .
        DEFINE VARIABLE oRootTool       AS ToolBase           NO-UNDO .
        DEFINE VARIABLE oFirstTabGroup  AS ContextualTabGroup NO-UNDO.

        DEFINE VARIABLE cToolKey AS CHARACTER NO-UNDO.

        DEFINE VARIABLE oList AS System.Collections.IList NO-UNDO .

        ASSIGN oList = NEW System.Collections.ArrayList () .

        {Consultingwerk/foreach.i ContextualTabGroup oTabGroup in poSourceToolbarsManager:Ribbon:ContextualTabGroups}

            oNewTabGroup = poTargetToolbarsManager:Ribbon:ContextualTabGroups:Add (SUBSTITUTE ("&1&2":U, pcPrefix, oTabGroup:Key)) .

            IF NOT VALID-OBJECT (oFirstTabGroup) THEN
                oFirstTabGroup = oNewTabGroup .

            oList:Add (oNewTabGroup) .

            ReflectionHelper:CopyInstanceProperties (oTabGroup, oNewTabGroup, "Key":U) .

            {Consultingwerk/foreach.i RibbonTab oRibbonTab in oTabGroup:Tabs}

                oNewRibbonTab = poTargetToolbarsManager:Ribbon:Tabs:Add (SUBSTITUTE ("&1&2":U, pcPrefix, oRibbonTab:Key)) .

                oList:Add (oNewRibbonTab) .

                ReflectionHelper:CopyInstanceProperties (oRibbonTab, oNewRibbonTab, "Key":U) .

                oNewTabGroup:Tabs:Add (oNewRibbonTab) .

                {Consultingwerk/foreach.i RibbonGroup oRibbonGroup in oRibbonTab:Groups}

                    oNewRibbonGroup = oNewRibbonTab:Groups:Add (SUBSTITUTE ("&1&2":U, pcPrefix, oRibbonGroup:Key)) .

                    oList:Add (oNewRibbonGroup) .

                    ReflectionHelper:CopyInstanceProperties (oRibbonGroup, oNewRibbonGroup, "Key":U) .

                    {Consultingwerk/foreach.i ToolBase oToolBase in oRibbonGroup:Tools}

                        ASSIGN oRootTool = oToolBase:SharedProps:RootTool
                               cToolKey  = SUBSTITUTE ("&1&2":U, pcPrefix, oToolBase:Key) .

                        IF NOT poTargetToolbarsManager:Tools:Exists (cToolKey) THEN DO:
                            oNewTool = DuplicateTool (oRootTool, SUBSTITUTE ("&1&&1":U, pcPrefix)) .

                            oList:Add (oNewTool) .

                            poTargetToolbarsManager:Tools:Add (oNewTool) .
                        END.

                        oNewTool = oNewRibbonGroup:Tools:AddTool (cToolKey) .

                        ReflectionHelper:CopyInstanceProperties (oToolBase:InstanceProps, oNewTool:InstanceProps, "":U) .
                    END.
                END.
            END.
        END.

        IF VALID-OBJECT (oFirstTabGroup) AND oFirstTabGroup:Tabs:Count > 0 THEN DO:
            IF VALID-OBJECT (poTargetToolbarsManager:MdiParentManager) THEN DO:
                poTargetToolbarsManager:RefreshMerge () .

                poTargetToolbarsManager:MdiParentManager:Ribbon:SelectedTab = oFirstTabGroup:Tabs[0]:AttachedParentTab  .
            END.
            ELSE
                poTargetToolbarsManager:Ribbon:SelectedTab = oFirstTabGroup:Tabs[0] .
        END.

        RETURN oList .

    END METHOD .

    /**
     * Purpose: Merges two tools
     * Notes:   This method is intended for merging a rendered menu-item (i.e. toggle-box)
     *          and rendered toolbar buttons. The TargetTool will be used as a place
     *          holder, defining the position in the target toolbars. The source
     *          tool will become "decorated" with the images from the target
     *          tool.
     * @param poToolbarsManager The reference to the UltraToolbarsManager that contains the Toolbar
     * @param pcSourceTool The Key of the source tool
     * @param pcTargetTool The Key of the target tool
     */
    METHOD PUBLIC STATIC VOID MergeTools (poToolbarsManager AS UltraToolbarsManager,
                                          pcSourceTool AS CHARACTER,
                                          pcTargetTool AS CHARACTER):

        DEFINE VARIABLE oSourceTool AS ToolBase NO-UNDO .
        DEFINE VARIABLE oTargetTool AS ToolBase NO-UNDO .
        DEFINE VARIABLE oToolInst   AS ToolBase NO-UNDO .

        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        ASSIGN oSourceTool = poToolbarsManager:Tools [pcSourceTool]
               oTargetTool = poToolbarsManager:Tools [pcTargetTool] .

        IF NOT VALID-OBJECT (oSourceTool) THEN
            UNDO, THROW NEW AppError ("Invalid Sourcetool."{&TRAN}, 0) .

        IF NOT VALID-OBJECT (oTargetTool) THEN
            UNDO, THROW NEW AppError ("Invalid Targettool."{&TRAN}, 0) .

        IF VALID-OBJECT (oTargetTool:SharedProps:AppearancesSmall:Appearance:Image) THEN
            oSourceTool:SharedProps:AppearancesSmall:Appearance:Image = oTargetTool:SharedProps:AppearancesSmall:Appearance:Image .

        IF VALID-OBJECT (oTargetTool:SharedProps:AppearancesLarge:Appearance:Image) THEN
            oSourceTool:SharedProps:AppearancesLarge:Appearance:Image = oTargetTool:SharedProps:AppearancesLarge:Appearance:Image .

        DO i = 0 TO oTargetTool:SharedProps:ToolInstances:Count - 1:

            oToolInst = oTargetTool:SharedProps:ToolInstances[i] .

            /* Insert Target Tool Instance */
            CAST (oToolInst:ParentCollection, ToolsCollection):InsertTool (oToolInst:Index, pcSourceTool) .
        END.

        DO i = oTargetTool:SharedProps:ToolInstances:Count - 1 TO 0 BY -1:

            oToolInst = oTargetTool:SharedProps:ToolInstances[i] .

            oToolInst:ParentCollection:Remove (oToolInst) .
        END.

    END METHOD.

    /**
     * Purpose: Popuplates a ToolsCollection with a menu structure from an ABL
     *          Menu Bar
     * Notes:
     * @param poForm The Form owning the Menu Bar and the UltraToolbarsManager
     * @param poTools The ToolsCollection to add the tools to
     */
    METHOD PUBLIC STATIC VOID PopulateToolsCollectionFromMenuBar (poForm AS IFormWithUltraToolbarsManager,
                                                                  poTools AS ToolsCollection):

        DEFINE VARIABLE hMenubar       AS HANDLE        NO-UNDO.
        DEFINE VARIABLE hChild         AS HANDLE        NO-UNDO.
        DEFINE VARIABLE lRulePending   AS LOGICAL       NO-UNDO INIT FALSE .
        DEFINE VARIABLE oToolItem      AS ToolBase      NO-UNDO .
        DEFINE VARIABLE oPopupMenuItem AS PopupMenuTool NO-UNDO .

        ASSIGN hMenubar = poForm:MENU-BAR .

        /* Check availbility of Menubar first. */
        IF NOT VALID-HANDLE (hMenubar) THEN
            RETURN .

        /* Check the availbility of the UltraToolbarsManager */
        IF NOT VALID-OBJECT(poForm:ToolbarsManager) THEN
            RETURN .

        poForm:ToolbarsManager:BeginUpdate() .

        ASSIGN hChild = hMenubar:FIRST-CHILD .

        DO WHILE VALID-HANDLE(hChild):
            CASE hChild:TYPE:
                WHEN "MENU-ITEM":U THEN DO:
                    CASE hChild:SUBTYPE:
                        WHEN "NORMAL":U THEN DO:
                            oToolItem = CreateMenuItemButtonTool (hChild, poTools, poForm) .

                            IF lRulePending THEN
                                ASSIGN poTools:Item [oToolItem:Key]:InstanceProps:IsFirstInGroup = TRUE
                                       lRulePending = FALSE .
                        END.
                        WHEN "RULE":U THEN
                            ASSIGN lRulePending = TRUE .
                    END CASE .
                END.
                WHEN "SUB-MENU":U THEN DO:
                    oPopupMenuItem = CreateSubMenuPopupMenu (hChild, poTools, poForm) .

                    BuildSubMenuItems (hChild, oPopupMenuItem, poForm) .
                END.
            END CASE .

            hChild = hChild:NEXT-SIBLING .
        END.

        FINALLY:
          IF VALID-OBJECT (poForm) AND VALID-OBJECT (poForm:ToolbarsManager) THEN
              poForm:ToolbarsManager:EndUpdate (TRUE) .
        END FINALLY.

    END METHOD.

    /**
     * Purpose: Refreshes the status of Tools in a List of ContextualTabs typically
     *          created by the method MergeContextualTabGroups
     * Notes:
     * @param poSourceToolbarsManager The UltraToolbarsManager instance that contains the orignal/source tools
     * @param poList A System.Collections.IList containing the ToolBase, RibbonTabs and ContextualTabGroups
     * @param pcPrefix The prefix used for naming the keys for the Tools in the list
     */
    METHOD PUBLIC STATIC VOID RefreshMergedContextualTabGroups (poSourceToolbarsManager AS UltraToolbarsManager,
                                                                poList AS System.Collections.IList,
                                                                pcPrefix AS CHARACTER):

        DEFINE VARIABLE oSourceTool AS ToolBase  NO-UNDO .
        DEFINE VARIABLE oTargetTool AS ToolBase  NO-UNDO .
        DEFINE VARIABLE cToolKey    AS CHARACTER NO-UNDO .

        {Consultingwerk/foreach.i System.Object oObject in poList}

            IF NOT TYPE-OF (oObject, ToolBase) THEN
                NEXT .

            ASSIGN oTargetTool = CAST (oObject, ToolBase) .

            IF LENGTH (oTargetTool:Key) < LENGTH (pcPrefix) OR
               NOT oTargetTool:Key BEGINS pcPrefix THEN
                NEXT .

            cToolKey = SUBSTRING (oTargetTool:Key, LENGTH (pcPrefix) + 1) .

            IF NOT poSourceToolbarsManager:Tools:Exists (cToolKey) THEN
                NEXT .

            oSourceTool = poSourceToolbarsManager:Tools [cToolKey] .

            ASSIGN oTargetTool:SharedProps:Enabled     = oSourceTool:SharedProps:Enabled
                   oTargetTool:SharedProps:Visible     = oSourceTool:SharedProps:Visible
                   oTargetTool:SharedProps:Caption     = oSourceTool:SharedProps:Caption
                   oTargetTool:SharedProps:ToolTipText = oSourceTool:SharedProps:ToolTipText  .

            IF TYPE-OF (oTargetTool, TextBoxTool) AND TYPE-OF (oSourceTool, TextBoxTool) THEN
                CAST (oTargetTool, TextBoxTool):Text = CAST (oSourceTool, TextBoxTool):Text .

            IF TYPE-OF (oTargetTool, ComboBoxTool) AND TYPE-OF (oSourceTool, ComboBoxTool) THEN
                CAST (oTargetTool, ComboBoxTool):Text = CAST (oSourceTool, ComboBoxTool):Text .
        END.

    END METHOD.

    /**
     * Purpose: Refreshs Menu items and Toolbar items based on the ABL widget
     *          status
     * Notes:
     * @param poToolbarsManager The reference to the UltraToolbarsManager that contains the ButtonTools
     */
    METHOD PUBLIC STATIC VOID RefreshTools (poToolbarsManager AS UltraToolbarsManager):

        UltraToolbarsHelper:RefreshTools (poToolbarsManager, TRUE) .

    END METHOD .

    /**
     * Purpose: Refreshs Menu items and Toolbar items based on the ABL widget
     *          status
     * Notes:
     * @param poToolbarsManager The reference to the UltraToolbarsManager that contains the ButtonTools
     * @param plIncludeVisible Logical value indicating if the Visible Attribute should be applied as well
     */
    METHOD PUBLIC STATIC VOID RefreshTools (poToolbarsManager AS UltraToolbarsManager,
                                            plIncludeVisible  AS LOGICAL):

        DEFINE VARIABLE hWidget      AS HANDLE                NO-UNDO .
        DEFINE VARIABLE lReloadImage AS LOGICAL               NO-UNDO .

        ASSIGN UltraToolbarsHelper:RefreshingToolbar = TRUE .

        poToolbarsManager:BeginUpdate () .

        {Consultingwerk/foreach.i ToolBase oTool in poToolbarsManager:Tools}

            ASSIGN hWidget = WIDGET-HANDLE (UNBOX(oTool:Tag)) NO-ERROR .

            IF VALID-HANDLE (hWidget) THEN DO:

&IF DEFINED (WinKit) NE 0 &THEN
                IF VALID-OBJECT (oRefreshToolsFilter) AND
                   NOT oRefreshToolsFilter:FilterTool (oTool, hWidget) THEN
                    NEXT .
&ENDIF
                ASSIGN oTool:SharedProps:Enabled = hWidget:SENSITIVE  .

                IF hWidget:TYPE = "BUTTON":U AND plIncludeVisible = TRUE THEN
                   oTool:SharedProps:Visible = hWidget:VISIBLE .

                /* Mike Fechner, Consultingwerk Ltd. 23 Aug 2011
                   Change label only when it's not just the case
                   that is changing, for the Ribbon we should
                   actually never be changing the Caption of a tool */
                /* Mike Fechner, Consultingwerk Ltd. 07.06.2012
                   Don't update the Caption with the LABEL of a Radio-set */
                IF hWidget:TYPE <> WidgetTypeEnum:RadioSet AND CAN-QUERY (hWidget, "LABEL":U) AND (oTool:SharedProps:Caption <> hWidget:LABEL) AND
                   (poToolbarsManager:Ribbon:Visible = FALSE
                   AND (NOT VALID-OBJECT (oChangeRibbonToolCaptionFilter) OR oChangeRibbonToolCaptionFilter:ChangeToolLabel (hWidget)))
                   THEN
                       oTool:SharedProps:Caption = hWidget:LABEL .

                IF CAN-QUERY (hWidget, "TOOLTIP":U) THEN
                    oTool:SharedProps:ToolTipText = REPLACE (hWidget:TOOLTIP,
                                                             CHR(13),
                                                             System.Environment:NewLine)  .

                IF CAN-QUERY (hWidget, "CHECKED":U) AND TYPE-OF (oTool, StateButtonTool) THEN
                    CAST (oTool, StateButtonTool):Checked = hWidget:CHECKED .

                IF CAN-QUERY (hWidget, "SCREEN-VALUE":U) AND TYPE-OF (oTool, TextBoxTool) THEN
                    CAST (oTool, TextBoxTool):Text = hWidget:SCREEN-VALUE .

                IF CAN-QUERY (hWidget, "SCREEN-VALUE":U) AND TYPE-OF (oTool, ComboBoxTool) THEN
                    CAST (oTool, ComboBoxTool):Text = hWidget:SCREEN-VALUE .

                /* Change image required? */
                ASSIGN lReloadImage = FALSE .

                /* Mike Fechner, Consultingwerk Ltd. 26.04.2012
                   Optinally have the check for change images in the ToolImageProvider implementation */
&IF DEFINED (WinKit) &THEN
                IF VALID-OBJECT (oToolImageProvider) THEN DO:
                    ASSIGN lReloadImage = oToolImageProvider:CheckReloadImage (hWidget,
                                                                             oTool) .
                    /* Leave back to original rule */
                    IF lReloadImage = ? THEN
                        ASSIGN lReloadImage = CAN-QUERY (hWidget, "IMAGE":U) AND hWidget:IMAGE > "":U .
                END.
                ELSE
&ENDIF
                    ASSIGN lReloadImage = CAN-QUERY (hWidget, "IMAGE":U) AND hWidget:IMAGE > "":U .

                IF lReloadImage THEN DO:
                    /* Mike Fechner, Consultingwerk Ltd. 17.06.2010
                       When an image has previously been loaded (from a
                       toolbar design, don't load the ABL button image) */
                    IF NOT VALID-OBJECT (oTool:SharedProps:AppearancesSmall:Appearance) OR
                       NOT VALID-OBJECT (oTool:SharedProps:AppearancesSmall:Appearance:Image) OR

                       UNBOX (CAST (oTool:SharedProps:AppearancesSmall:Appearance:Image, System.Drawing.Image):Tag) <> hWidget:IMAGE

                        THEN
                            LoadImage (hWidget, oTool, TRUE) .
                END.

&IF DEFINED (WinKit) NE 0 &THEN
                IF VALID-OBJECT (oRibbonButtonCustomizer) THEN DO:
                    IF TYPE-OF (oTool, ButtonTool) THEN
                        oRibbonButtonCustomizer:RefreshRibbonButton (CAST (oTool, ButtonTool)) .
                END.
&ENDIF
            END.
        END.

        FINALLY:
            poToolbarsManager:EndUpdate (TRUE) .

            ASSIGN UltraToolbarsHelper:RefreshingToolbar = FALSE .
        END FINALLY.

    END METHOD .

    /**
     * Purpose: Refreshs Menu items and Toolbar items based on the ABL widget
     *          status
     * Notes:
     * @param poForm The Form owning the UltraToolbarsManager that contains the Main Menu Toolbar
     */
    METHOD PUBLIC STATIC VOID RefreshTools (poForm AS IFormWithUltraToolbarsManager):

        UltraToolbarsHelper:RefreshTools (poForm:ToolbarsManager, TRUE) .

    END METHOD.

    /**
     * Purpose: Removes the menu-bar and all sub-menus from the UltraToolbarsManager
     * Notes:   Used to rebuild the menu-bar (i.e. when different tab-folder pages
     *          contain different menu items
     * @param poForm The Form owning the UltraToolbarsManager that contains the Main Menu Toolbar
     */
    METHOD PUBLIC STATIC VOID RemoveMenu (poForm AS IFormWithUltraToolbarsManager):

        DEFINE VARIABLE oToolbarsManager AS UltraToolbarsManager NO-UNDO .
        DEFINE VARIABLE oToolbar         AS UltraToolbar         NO-UNDO.
        DEFINE VARIABLE i                AS INTEGER              NO-UNDO.

        ASSIGN oToolbarsManager = poForm:ToolbarsManager .

        IF NOT VALID-OBJECT (oToolbarsManager) THEN
            RETURN .

        toolbarsearch: DO i = 0 TO oToolbarsManager:Toolbars:Count - 1:
            IF oToolbarsManager:Toolbars[i]:IsMainMenuBar THEN DO:
                oToolbar = oToolbarsManager:Toolbars[i] .

                LEAVE toolbarsearch .
            END.
        END.

        IF NOT VALID-OBJECT (oToolbar) THEN
            RETURN .

        RemoveSubMenus (oToolbar:Tools) .

    END METHOD.

    /**
     * Purpose: Removes single Ampersand Charcaters from a MENU-ITEM Label
     * Notes:   && will be kept
     * @param pcString The source string
     * @return The string without single & characters
     */
    METHOD PUBLIC STATIC CHARACTER RemoveMenuAmpersand (pcString AS CHARACTER):

        RETURN REPLACE (REPLACE (REPLACE (pcString, "&&":U, CHR(1)), "&":U, "":U), CHR (1), "&&":U) .

    END METHOD.

    /**
     * Purpose: Recursively clears all sub menus (PopupMenuTool instances) from a
     *          Tools collections
     * Notes:
     * @param poTools The ToolsCollection containing the PopupMeuTools to be removed
     */
    METHOD PUBLIC STATIC VOID RemoveSubMenus (poTools AS ToolsCollection):

        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        DO i = 0 TO poTools:Count - 1:

            IF TYPE-OF (poTools:Item[i], PopupMenuTool) THEN
                RemoveSubMenus (CAST (poTools:Item[i], PopupMenuTool):Tools) .
        END .

        poTools:Clear() .

    END METHOD.

    /**
     * Purpose: Removes a specific Toolbar from the Toolbar Manager
     * Notes:   The Toolbar is referenced by the ABL FRAME widget that contained the BUTTON Widgets
     * @param phFrame The HANDLE of the ABL FRAME WIDGET containing the BUTTON widgets
     * @param poForm The Form owning the UltraToolbarsManager that contains the Toolbar
     */
    METHOD PUBLIC STATIC VOID RemoveToolbarFrame (phFrame AS HANDLE,
                                                  poForm  AS IFormWithUltraToolbarsManager ):

        RemoveToolbarFrame (phFrame,
                            poForm:ToolbarsManager) .

    END METHOD.

    /**
     * Purpose: Removes a specific Toolbar from the Toolbar Manager
     * Notes:   The Toolbar is referenced by the ABL FRAME widget that contained the BUTTON Widgets
     * @param phFrame The HANDLE of the ABL FRAME WIDGET containing the BUTTON Widgets
     * @param poToolbarsManager The reference to the UltraToolbarsManager that contains the Toolbar
     */
    METHOD PUBLIC STATIC VOID RemoveToolbarFrame (phFrame AS HANDLE,
                                                  poToolbarsManager AS UltraToolbarsManager ):

        IF poToolbarsManager:Toolbars:Exists ("frame":U + STRING(phFrame)) THEN DO:

            UltraToolbarsHelper:EraseToolsCollection (poToolbarsManager:Toolbars ["frame":U + STRING(phFrame)]:Tools) .
            poToolbarsManager:Toolbars ["frame":U + STRING(phFrame)]:Tools:Clear().
            poToolbarsManager:Toolbars ["frame":U + STRING(phFrame)]:VISIBLE = FALSE.
            DELETE OBJECT poToolbarsManager:Toolbars ["frame":U + STRING(phFrame)] .
            poToolbarsManager:Toolbars ["frame":U + STRING(phFrame)]:Dispose().
        END.

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /**
     * Purpose:  Restored a previouly stored comma-delimited list of Tools of the
     *           Ribbon Quick Access Tools
     * Notes:
     * @param poToolbarsManager The reference to the UltraToolbarsManager instance
     * @param pcTools The comma delimited list (CHARACTER) of tool keys on the Ribbon Quick Access Toolbar
     */
    METHOD PUBLIC STATIC VOID RestoreQuickAccessToolbarTools (poToolbarsManager AS UltraToolbarsManager,
                                                              pcTools AS CHARACTER):

        DEFINE VARIABLE i    AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cKey AS CHARACTER NO-UNDO.

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poToolbarsManager} .

        poToolbarsManager:Ribbon:QuickAccessToolbar:Tools:Clear () .

        DO i = 1 TO NUM-ENTRIES (pcTools):
            ASSIGN cKey = ENTRY (i, pcTools) .

            IF poToolbarsManager:Tools:Exists (cKey) THEN
                poToolbarsManager:Ribbon:QuickAccessToolbar:Tools:AddTool (cKey) .
        END .

    END METHOD.

    /**
     * Purpose: Assigns an accelerator (keyboard mnemonic) to a menu item button tool
     * Notes:   Uses the ShortcutFromAccelerator Method to match the ACCELERATOR to a Shortcut
     * @param poTool The reference to the tool (ToolBase reference)
     * @param phMenuItem The reference to the ABL MENU-ITEM Widget
     */
    METHOD PROTECTED STATIC VOID SetAccelerator (poTool AS ToolBase, phMenuItem AS HANDLE):

        DEFINE VARIABLE oShortcut AS System.Windows.Forms.Shortcut NO-UNDO .

        IF NOT VALID-HANDLE (phMenuItem) OR NOT CAN-QUERY (phMenuItem, "ACCELERATOR":U) THEN
            RETURN .

        IF phMenuItem:ACCELERATOR > "":U THEN .
        ELSE
            RETURN .

        oShortcut = KeyboardHelper:ShortcutFromAccelerator (phMenuItem:ACCELERATOR) .

        IF VALID-OBJECT (oShortcut) THEN
           ASSIGN poTool:SharedProps:Shortcut = oShortcut
                  phMenuItem:ACCELERATOR      = "":U .

    END METHOD.

    /**
     * Purpose: Sets the Checked property of a StateButtonTool
     * Notes:
     * @param poToolbarsManager The reference to the UltraToolbarsManager that contains the StateButton Tool
     * @param pcToolKey The key of the StateButton Tool in the UltraToolbarsManager Tools Collection
     * @param plChecked The value to assign to the Checked property of the StateButton Tool
     */
    METHOD PUBLIC STATIC VOID SetStateButtonToolChecked (poToolbarsManager AS UltraToolbarsManager,
                                                         pcToolKey AS CHARACTER,
                                                         plChecked AS LOGICAL):

        DEFINE VARIABLE oStateButtonTool AS StateButtonTool NO-UNDO .

        IF NOT poToolbarsManager:Tools:Exists (pcToolKey) THEN
            UNDO, THROW NEW AppError (SUBSTITUTE ("Invalid ToolKey: &1"{&TRAN}, pcToolKey), 0) .

        IF NOT TYPE-OF (poToolbarsManager:Tools[pcToolKey],
                        StateButtonTool) THEN
            UNDO, THROW NEW AppError (SUBSTITUTE ("Tool &1 is not a StateButtonTool."{&TRAN}, pcToolKey), 0) .

        oStateButtonTool = CAST (poToolbarsManager:Tools[pcToolKey],
                                 StateButtonTool) .

        oStateButtonTool:Checked = plChecked .

    END METHOD.

    /**
     * Purpose: Translates a Progress MENU-ITEM Accelerator into a .NET
     *          System.Windows.Forms.Shortcut enumeration member
     * Notes:   Method now implemented in the KeyboardHelper class
     *          Note, not all possible Progress Accelerators are available/supported
     *          as .NET Shortcuts, http://blog.consultingwerk.de/consultingwerkblog/2011/10/create-custom-shortcuts-not-contained-in-the-system-windows-forms-shortcut-enumeration/
     * @param pcAccelerator The ABL keyboard accelerator
     * @return The System.Windows.Forms.Shortcut value that matches the ABL keyboard accelerator
     */
    METHOD PUBLIC STATIC System.Windows.Forms.Shortcut ShortcutFromAccelerator (pcAccelerator AS CHARACTER):

        RETURN KeyboardHelper:ShortcutFromAccelerator (pcAccelerator) .

    END METHOD.

    /**
     * Purpose: Toggles the Visibility of the Toolbars. Makes them only visible, when
     *          at least a single Tool is visible.
     * Notes:
     * @param poToolbarsManager The UltraToolbarsManager that contains the Toolbars
     */
    METHOD PUBLIC STATIC VOID ToggleVisibleToolbars (poToolbarsManager AS UltraToolbarsManager):

        DEFINE VARIABLE lVisible AS LOGICAL NO-UNDO.

        {Consultingwerk/foreach.i UltraToolbar oToolbar in poToolbarsManager:Toolbars}

            lVisible = FALSE .
            {Consultingwerk/foreach.i ToolBase oTool in oToolbar:Tools}

                IF oTool:SharedProps:Visible THEN DO:
                    lVisible = TRUE .
                    LEAVE  .
                END.
             END.

             oToolbar:Visible = lVisible .
        END.

    END METHOD .

    /**
     * Purpose: Views and hides Buttons in an UltraToolbarsManager ToolbarBand
     *          based on visibility of Buttons in a FRAME
     * Notes:
     * @param phFrame The handle to the FRAME that contains the BUTTON widgets
     * @param poToolbarsManager The reference to the UltraToolbarsManager that contains the Button Tools
     */
    METHOD PUBLIC STATIC VOID ViewHideToolbarButtons (phFrame AS HANDLE,
                                                      poToolbarsManager AS UltraToolbarsManager):

        DEFINE VARIABLE oToolbar     AS UltraToolbar          NO-UNDO .
        DEFINE VARIABLE oButtonTool  AS ToolBase              NO-UNDO .
        DEFINE VARIABLE hWidget      AS HANDLE                NO-UNDO .

        IF NOT poToolbarsManager:Toolbars:Exists (("frame":U + STRING(phFrame))) THEN
            UNDO, THROW NEW AppError ("Toolbarband for Frame not found."{&TRAN}, 0) .
        ELSE
            oToolbar = poToolbarsManager:Toolbars [("frame":U + STRING(phFrame))] .

        oToolbar:Visible = phFrame:VISIBLE .

        ASSIGN
            hWidget = phFrame:FIRST-CHILD:FIRST-CHILD .

        DO WHILE VALID-HANDLE (hWidget):
            IF hWidget:TYPE = "BUTTON":U THEN DO:

&IF DEFINED (WinKit) NE 0 &THEN
                IF VALID-OBJECT (oViewHideToolbarButtonsFilter) AND
                   NOT oViewHideToolbarButtonsFilter:FilterTool (hWidget) THEN DO:
                    ASSIGN hWidget = hWidget:NEXT-SIBLING .
                    NEXT .
                END.
&ENDIF

                IF NOT poToolbarsManager:Tools:Exists(hWidget:NAME) THEN
                DO:
                    ASSIGN
                        hWidget = hWidget:NEXT-SIBLING .

                    NEXT .
                END.

                oButtonTool = poToolbarsManager:Tools[hWidget:NAME] .

                oButtonTool:SharedProps:Visible = hWidget:VISIBLE .
            END. /* Button */

            ASSIGN
                hWidget = hWidget:NEXT-SIBLING .
        END. /* VALID-HANDLE */

    END METHOD.
&ENDIF
END CLASS.
