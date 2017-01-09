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
    File        : SmartUpdatePanel
    Purpose     : Panel with TableIO and Navigation Buttons
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Dec 22 14:31:24 CET 2009
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.Collections.*                                   FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.*                                    FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*                                    FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.*                          FROM PROPATH .
USING Consultingwerk.SmartComponents.Interfaces.*                              FROM PROPATH .
USING Consultingwerk.SmartComponents.Resources.*                               FROM PROPATH .
USING Consultingwerk.Util.*                                                    FROM PROPATH .
USING Infragistics.Win.UltraWinToolbars.*                                      FROM ASSEMBLY .
USING Progress.Lang.*                                                          FROM ASSEMBLY .
USING Progress.Windows.UserControl                                             FROM ASSEMBLY .
USING Consultingwerk.SmartComponents.Interfaces.Design.IDesignNavigationSource FROM ASSEMBLY .
USING Consultingwerk.SmartComponents.Interfaces.Design.IDesignTableIOSource    FROM ASSEMBLY .
USING System.ComponentModel.*                                                  FROM ASSEMBLY .

{Consultingwerk/products.i}

CLASS Consultingwerk.SmartComponents.Implementation.SmartPanelController
    INHERITS SmartUserControl
    IMPLEMENTS ISmartNavigationSource, ISmartTableIOSource, IDoNotCheckOkToClose,
               ICustomTypeDescriptor,
               ISmartToolbarControl,
               Consultingwerk.SmartComponents.Interfaces.Design.IDesignNavigationSource,
               Consultingwerk.SmartComponents.Interfaces.Design.IDesignTableIOSource :

    DEFINE PRIVATE VARIABLE m_SmartUpdatePanel_Toolbars_Dock_Area_Top AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SmartUpdatePanel_Toolbars_Dock_Area_Right AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SmartUpdatePanel_Toolbars_Dock_Area_Left AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_SmartUpdatePanel_Toolbars_Dock_Area_Bottom AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraToolbarsManager1 AS Infragistics.Win.UltraWinToolbars.UltraToolbarsManager NO-UNDO.

    /* Mike Fechner, Consultingwerk Ltd. 16.05.2010
       Core implementation of ICustomTypeDescriptor */
    {Consultingwerk/SmartComponents/Base/ICustomTypeDescriptor.i}

    /* Mike Fechner, Consultingwerk Ltd. 03.09.2013
       Static only on 10.2B to workaround too-many-temp-table issue
       Not static on 11.x to avoid issues with the combination of static
       and hybrids on 11.2 and 11.3, OE defect OE00240707
       http://knowledgebase.progress.com/articles/Article/000042229?popup=true */
    DEFINE PRIVATE
    &IF PROVERSION BEGINS "10.2":U &THEN
    STATIC
    &ENDIF TEMP-TABLE ttSmartNavigationTarget NO-UNDO
        FIELD RecordOwner             AS Progress.Lang.Object
        FIELD SmartNavigationTarget   AS Progress.Lang.Object
        FIELD SmartNavigationPosition AS CHARACTER
        INDEX RecordOwner IS UNIQUE RecordOwner SmartNavigationTarget.
        .

    DEFINE VARIABLE oController AS SmartToolbarStateController NO-UNDO .

    /*------------------------------------------------------------------------------
        Purpose: Returns a reference to the current active SmartNavigationTarget
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ActiveSmartNavigationTarget AS ISmartNavigationTarget NO-UNDO
    GET:
        RETURN oController:ActiveSmartNavigationTarget .
    END GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns a reference to the current active SmartTableIOTarget
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ActiveSmartTableIOTarget AS ISmartTableIOTarget NO-UNDO
    GET:
        RETURN oController:ActiveSmartTableIOTarget .
    END GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the List of TableIOTargetControl instances
        Notes:   See http://jira.consultingwerkcloud.com/browse/SCL-442 for details
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ListTableIOTargetControl AS ListTableIOTargetControl NO-UNDO
    GET:
        RETURN oController:ListTableIOTargetControl .
    END GET .

    /*------------------------------------------------------------------------------
        Purpose: Allows access to the Toolinstances of the Navigation Toolbar
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY NavigationTools AS ToolsCollection NO-UNDO
    GET:
        RETURN ultraToolbarsManager1:Toolbars["NavigationToolbar":U]:Tools .
    END GET .

    /*------------------------------------------------------------------------------
        Purpose: Allows access to the Toolinstances of the Navigation Toolbar
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY RootTools AS RootToolsCollection NO-UNDO
    GET:
        RETURN ultraToolbarsManager1:Tools .
    END GET .

    /*------------------------------------------------------------------------------
        Purpose: Controls the visibility of the Navigation Toolbar
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ShowNavigationTools AS LOGICAL INITIAL TRUE NO-UNDO
    GET:
        RETURN ultraToolbarsManager1:Toolbars["NavigationToolbar":U]:Visible .
    END.
    SET (arg AS LOGICAL):
        ultraToolbarsManager1:Toolbars["NavigationToolbar":U]:Visible = arg .
        ultraToolbarsManager1:Tools["blank":U]:SharedProps:Visible = arg .
    END.

    /*------------------------------------------------------------------------------
        Purpose: Controls the visibility of the TableIO Toolbar
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ShowTableIOTools AS LOGICAL INITIAL TRUE NO-UNDO
    GET:
        RETURN ultraToolbarsManager1:Toolbars["TableIOToolbar":U]:Visible .
    END.
    SET (arg AS LOGICAL):
        ultraToolbarsManager1:Toolbars["TableIOToolbar":U]:Visible = arg .
    END.

    /*------------------------------------------------------------------------------
        Purpose: Invokes the SmartPanelDesigner Dialog
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmartPanelDesigner AS LOGICAL INITIAL FALSE NO-UNDO
    GET:
        RETURN FALSE.
    END.
    SET (arg AS LOGICAL):

    END SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the List of SmartTableIOTargets as a generic List
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmartTableIOTargets AS ListISmartTableIOTarget NO-UNDO
    GET:
        RETURN oController:SmartTableIOTargets .
    END GET.

    /*------------------------------------------------------------------------------
        Purpose: Allows access to the Toolinstances of the Navigation Toolbar
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY TableIOTools AS ToolsCollection NO-UNDO
    GET:
        RETURN ultraToolbarsManager1:Toolbars["TableIOToolbar":U]:Tools .
    END GET.


    /*------------------------------------------------------------------------------
        Purpose: Allows access to the Toolbars collection
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Toolbars AS ToolbarsCollection NO-UNDO
    GET():
        RETURN ultraToolbarsManager1:Toolbars .
    END GET .

    /*------------------------------------------------------------------------------
        Purpose: Allows to control the Visible Navigation tools and their relative order
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY VisibleNavigationTools AS CHARACTER NO-UNDO
        INIT "NavigationFirst,NavigationPrev,NavigationNext,NavigationLast":U
    GET.
    SET (arg AS CHARACTER):
        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        ASSIGN arg = TRIM (TRIM(arg, " ":U), ",":U) .

        DO i = 1 TO NUM-ENTRIES (arg):
            IF NOT ListHelper:EntryIsInList (ENTRY(i, arg), "NavigationFirst,NavigationPrev,NavigationNext,NavigationLast":U) THEN
                UNDO, THROW NEW AppError ("Allowed values are: NavigationFirst,NavigationPrev,NavigationNext,NavigationLast":U, 0) .
        END .

        THIS-OBJECT:NavigationTools:Clear () .

        DO i = 1 TO NUM-ENTRIES(arg):
            THIS-OBJECT:NavigationTools:AddTool (ENTRY(i, arg)) .
        END.

        VisibleNavigationTools = arg .

    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Allows to control the Visible TableIO tools and their relative order
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY VisibleTableIOTools AS CHARACTER NO-UNDO
       INIT "TableIOAdd,TableIOCopy,TableIOUpdate,TableIOSave,TableIOCancel,TableIODelete":U
    GET.
    SET (arg AS CHARACTER):
        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        ASSIGN arg = TRIM (TRIM(arg, " ":U), ",":U) .

        DO i = 1 TO NUM-ENTRIES (arg):
            IF NOT ListHelper:EntryIsInList (ENTRY(i, arg), "TableIOAdd,TableIOCopy,TableIOUpdate,TableIOSave,TableIOCancel,TableIODelete":U) THEN
                UNDO, THROW NEW AppError ("Allowed values are: TableIOAdd,TableIOCopy,TableIOUpdate,TableIOSave,TableIOCancel,TableIODelete":U, 0) .
        END .

        THIS-OBJECT:TableIOTools:Clear () .

        DO i = 1 TO NUM-ENTRIES(arg):
            THIS-OBJECT:TableIOTools:AddTool (ENTRY(i, arg)) .
        END.

        THIS-OBJECT:TableIOTools:AddTool ("blank":U) .

        VisibleTableIOTools = arg .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartPanelController class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartPanelController ():

        SUPER().
        InitializeComponent().

        InitializeCaptionOfTools ().

        ASSIGN oController = NEW SmartToolbarStateController (THIS-OBJECT) .

        THIS-OBJECT:ultraToolbarsManager1:BeginUpdate () .

        DO ON ERROR UNDO, THROW:
            DisableTools ("NavigationFirst,NavigationPrev,NavigationNext,NavigationLast":U) .
            DisableTools ("TableIOAdd,TableIOCopy,TableIOUpdate,TableIOSave,TableIOCancel,TableIODelete":U) .

            FINALLY:
                THIS-OBJECT:ultraToolbarsManager1:EndUpdate () .
            END FINALLY.
        END.

        IF THIS-OBJECT:DesignTime THEN
            SetDesignerProperties () .

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Begins a series of updates to the Control
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID BeginControlUpdate ():

        THIS-OBJECT:ultraToolbarsManager1:BeginUpdate () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Ends a series of updates to the Control
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID EndControlUpdate ():

        THIS-OBJECT:ultraToolbarsManager1:EndUpdate () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns a CharacterList of all ToolKeys in the Toolbar
        Notes:
        @return The CharacterList of all ToolKeys in the Toolbar
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CharacterList GetAllToolKeys ():

        DEFINE VARIABLE oList AS CharacterList NO-UNDO .

        oList = NEW CharacterList ().

        {Consultingwerk/foreach.i Infragistics.Win.UltraWinToolbars.ToolBase oTool in THIS-OBJECT:ultraToolbarsManager1:Tools}
            oList:Add (oTool:Key) .
        END.

        RETURN oList .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the type name of the Form that contains the Toolbar
        Notes:
        @return The type name of the Form that contains the Toolbar
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetFormTypeName ():

        DEFINE VARIABLE oForm AS System.Windows.Forms.Form     NO-UNDO .

        oForm = THIS-OBJECT:FindForm () .

        IF NOT VALID-OBJECT (oForm) THEN
            RETURN ? .

        RETURN oForm:GetType():FullName .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Allows to permanently disable TableIO Tools in the SmartToolbarController
        Notes:   Useful for applying security settings to the toolbar
                 http://confluence.consultingwerkcloud.com/wiki/pages/viewpage.action?pageId=722342
                 See http://jira.consultingwerkcloud.com/browse/SCL-442 for details on
                 extending this for custom buttons
        @param poSmartTableIOTarget The reference to the ISmartTableIOTarget for this an action should be disabled
        @param poTableIOAction The TableIOActionEnum member identifying the action to enable/disable
        @return Logical value indicating if the tool should be disabled. Set to FALSE to enable the tool
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL GetTableIOActionDisabled (poSmartTableIOTarget AS ISmartTableIOTarget,
                                                    poTableIOAction AS TableIOActionEnum):

        RETURN oController:GetTableIOActionDisabled(poSmartTableIOTarget, poTableIOAction) .

    END METHOD .

    /**
     * Purpose: Initializes the Visual Design
     * Notes:
     */
    @InitializeComponent.
    METHOD PRIVATE VOID InitializeComponent ():

        /* NOTE: The following method is automatically generated.

        We strongly suggest that the contents of this method only be modified using the
        Visual Designer to avoid any incompatible modifications.

        Modifying the contents of this method using a code editor will invalidate any support for this file. */
        THIS-OBJECT:components = NEW System.ComponentModel.Container().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraToolbar1 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar1 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("TableIOToolbar":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool2 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool2 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOAdd":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool3 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool3 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCopy":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool4 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool4 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOUpdate":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool5 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool5 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool6 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool6 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCancel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool7 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool7 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIODelete":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool23 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool23 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("blank":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraToolbar2 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.
        ultraToolbar2 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("NavigationToolbar":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool14 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool14 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationFirst":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool15 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool15 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationPrev":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool16 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool16 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationNext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool17 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool17 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationLast":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool1 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool1 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOAdd":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.SmartComponents.Implementation.SmartPanelController":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool8 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool8 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCopy":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool9 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool9 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOUpdate":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
        appearance3 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool10 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool10 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
        appearance4 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool11 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool11 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCancel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance5 AS Infragistics.Win.Appearance NO-UNDO.
        appearance5 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool12 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool12 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIODelete":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance6 AS Infragistics.Win.Appearance NO-UNDO.
        appearance6 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool13 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool13 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationFirst":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance7 AS Infragistics.Win.Appearance NO-UNDO.
        appearance7 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool18 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool18 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationPrev":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance8 AS Infragistics.Win.Appearance NO-UNDO.
        appearance8 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool19 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool19 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationNext":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance9 AS Infragistics.Win.Appearance NO-UNDO.
        appearance9 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool20 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool20 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationLast":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance10 AS Infragistics.Win.Appearance NO-UNDO.
        appearance10 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool22 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool22 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("blank":U).
        THIS-OBJECT:ultraToolbarsManager1 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsManager(THIS-OBJECT:components).
        THIS-OBJECT:m_SmartUpdatePanel_Toolbars_Dock_Area_Right = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_SmartUpdatePanel_Toolbars_Dock_Area_Left = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_SmartUpdatePanel_Toolbars_Dock_Area_Bottom = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_SmartUpdatePanel_Toolbars_Dock_Area_Top = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        CAST(THIS-OBJECT:ultraToolbarsManager1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraToolbarsManager1 */
        /*  */
        THIS-OBJECT:ultraToolbarsManager1:DesignerFlags = 1.
        THIS-OBJECT:ultraToolbarsManager1:DockWithinContainer = THIS-OBJECT.
        THIS-OBJECT:ultraToolbarsManager1:ShowFullMenusDelay = 500.
        THIS-OBJECT:ultraToolbarsManager1:Style = Infragistics.Win.UltraWinToolbars.ToolbarStyle:Office2007.
        ultraToolbar1:DockedColumn = 0.
        ultraToolbar1:DockedRow = 0.
        buttonTool2:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 7 NO-UNDO.
        arrayvar0[1] = buttonTool2.
        arrayvar0[2] = buttonTool3.
        arrayvar0[3] = buttonTool4.
        arrayvar0[4] = buttonTool5.
        arrayvar0[5] = buttonTool6.
        arrayvar0[6] = buttonTool7.
        arrayvar0[7] = buttonTool23.
        ultraToolbar1:NonInheritedTools:AddRange(arrayvar0).
        ultraToolbar1:Text = "TableIO Toolbar":U.
        ultraToolbar2:DockedColumn = 1.
        ultraToolbar2:DockedRow = 0.
        buttonTool14:InstanceProps:IsFirstInGroup = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 4 NO-UNDO.
        arrayvar1[1] = buttonTool14.
        arrayvar1[2] = buttonTool15.
        arrayvar1[3] = buttonTool16.
        arrayvar1[4] = buttonTool17.
        ultraToolbar2:NonInheritedTools:AddRange(arrayvar1).
        ultraToolbar2:Text = "Navigation Toolbar":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinToolbars.UltraToolbar EXTENT 2 NO-UNDO.
        arrayvar2[1] = ultraToolbar1.
        arrayvar2[2] = ultraToolbar2.
        THIS-OBJECT:ultraToolbarsManager1:Toolbars:AddRange(arrayvar2).
        THIS-OBJECT:ultraToolbarsManager1:ToolbarSettings:AllowCustomize = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:ultraToolbarsManager1:ToolbarSettings:AllowDockBottom = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:ultraToolbarsManager1:ToolbarSettings:AllowDockLeft = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:ultraToolbarsManager1:ToolbarSettings:AllowDockRight = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:ultraToolbarsManager1:ToolbarSettings:AllowDockTop = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:ultraToolbarsManager1:ToolbarSettings:AllowFloating = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:ultraToolbarsManager1:ToolbarSettings:AllowHiding = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:ultraToolbarsManager1:ToolbarSettings:FillEntireRow = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:ultraToolbarsManager1:ToolbarSettings:GrabHandleStyle = Infragistics.Win.UltraWinToolbars.GrabHandleStyle:None.
        appearance1:Image = CAST(resources:GetObject("appearance1.Image":U), System.Object).
        buttonTool1:SharedPropsInternal:AppearancesSmall:Appearance = appearance1.
        buttonTool1:SharedPropsInternal:Caption = "Add record":U.
        buttonTool1:SharedPropsInternal:CustomizerCaption = "Add":U.
        buttonTool1:SharedPropsInternal:Enabled = FALSE.
        buttonTool1:SharedPropsInternal:ToolTipText = "Add record":U.
        appearance2:Image = CAST(resources:GetObject("appearance2.Image":U), System.Object).
        buttonTool8:SharedPropsInternal:AppearancesSmall:Appearance = appearance2.
        buttonTool8:SharedPropsInternal:Caption = "Copy record":U.
        buttonTool8:SharedPropsInternal:CustomizerCaption = "Copy":U.
        buttonTool8:SharedPropsInternal:Enabled = FALSE.
        buttonTool8:SharedPropsInternal:ToolTipText = "Copy record":U.
        appearance3:Image = CAST(resources:GetObject("appearance3.Image":U), System.Object).
        buttonTool9:SharedPropsInternal:AppearancesSmall:Appearance = appearance3.
        buttonTool9:SharedPropsInternal:Caption = "Update record":U.
        buttonTool9:SharedPropsInternal:CustomizerCaption = "Update":U.
        buttonTool9:SharedPropsInternal:Enabled = FALSE.
        buttonTool9:SharedPropsInternal:ToolTipText = "Update record":U.
        appearance4:Image = CAST(resources:GetObject("appearance4.Image":U), System.Object).
        buttonTool10:SharedPropsInternal:AppearancesSmall:Appearance = appearance4.
        buttonTool10:SharedPropsInternal:Caption = "Save changes":U.
        buttonTool10:SharedPropsInternal:CustomizerCaption = "Save":U.
        buttonTool10:SharedPropsInternal:Enabled = FALSE.
        buttonTool10:SharedPropsInternal:ToolTipText = "Save changes":U.
        appearance5:Image = CAST(resources:GetObject("appearance5.Image":U), System.Object).
        buttonTool11:SharedPropsInternal:AppearancesSmall:Appearance = appearance5.
        buttonTool11:SharedPropsInternal:Caption = "Cancel update":U.
        buttonTool11:SharedPropsInternal:CustomizerCaption = "Cancel":U.
        buttonTool11:SharedPropsInternal:Enabled = FALSE.
        buttonTool11:SharedPropsInternal:ToolTipText = "Cancel update":U.
        appearance6:Image = CAST(resources:GetObject("appearance6.Image":U), System.Object).
        buttonTool12:SharedPropsInternal:AppearancesSmall:Appearance = appearance6.
        buttonTool12:SharedPropsInternal:Caption = "Delete record":U.
        buttonTool12:SharedPropsInternal:CustomizerCaption = "Delete":U.
        buttonTool12:SharedPropsInternal:Enabled = FALSE.
        buttonTool12:SharedPropsInternal:ToolTipText = "Delete record":U.
        appearance7:Image = CAST(resources:GetObject("appearance7.Image":U), System.Object).
        buttonTool13:SharedPropsInternal:AppearancesSmall:Appearance = appearance7.
        buttonTool13:SharedPropsInternal:Caption = "First record":U.
        buttonTool13:SharedPropsInternal:CustomizerCaption = "First":U.
        buttonTool13:SharedPropsInternal:Enabled = FALSE.
        buttonTool13:SharedPropsInternal:ToolTipText = "First record":U.
        appearance8:Image = CAST(resources:GetObject("appearance8.Image":U), System.Object).
        buttonTool18:SharedPropsInternal:AppearancesSmall:Appearance = appearance8.
        buttonTool18:SharedPropsInternal:Caption = "Previous record":U.
        buttonTool18:SharedPropsInternal:CustomizerCaption = "Previous":U.
        buttonTool18:SharedPropsInternal:Enabled = FALSE.
        buttonTool18:SharedPropsInternal:ToolTipText = "Previous record":U.
        appearance9:Image = CAST(resources:GetObject("appearance9.Image":U), System.Object).
        buttonTool19:SharedPropsInternal:AppearancesSmall:Appearance = appearance9.
        buttonTool19:SharedPropsInternal:Caption = "Next record":U.
        buttonTool19:SharedPropsInternal:CustomizerCaption = "Next":U.
        buttonTool19:SharedPropsInternal:Enabled = FALSE.
        buttonTool19:SharedPropsInternal:ToolTipText = "Next record":U.
        appearance10:Image = CAST(resources:GetObject("appearance10.Image":U), System.Object).
        buttonTool20:SharedPropsInternal:AppearancesSmall:Appearance = appearance10.
        buttonTool20:SharedPropsInternal:Caption = "Last record":U.
        buttonTool20:SharedPropsInternal:CustomizerCaption = "Last":U.
        buttonTool20:SharedPropsInternal:Enabled = FALSE.
        buttonTool20:SharedPropsInternal:ToolTipText = "Last record":U.
        buttonTool22:SharedPropsInternal:DisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:TextOnlyAlways.
        buttonTool22:SharedPropsInternal:Enabled = FALSE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar3 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 11 NO-UNDO.
        arrayvar3[1] = buttonTool1.
        arrayvar3[2] = buttonTool8.
        arrayvar3[3] = buttonTool9.
        arrayvar3[4] = buttonTool10.
        arrayvar3[5] = buttonTool11.
        arrayvar3[6] = buttonTool12.
        arrayvar3[7] = buttonTool13.
        arrayvar3[8] = buttonTool18.
        arrayvar3[9] = buttonTool19.
        arrayvar3[10] = buttonTool20.
        arrayvar3[11] = buttonTool22.
        THIS-OBJECT:ultraToolbarsManager1:Tools:AddRange(arrayvar3).
        THIS-OBJECT:ultraToolbarsManager1:ToolClick:Subscribe(THIS-OBJECT:ToolClickHandler).
        /*  */
        /* m_SmartUpdatePanel_Toolbars_Dock_Area_Right */
        /*  */
        THIS-OBJECT:m_SmartUpdatePanel_Toolbars_Dock_Area_Right:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_SmartUpdatePanel_Toolbars_Dock_Area_Right:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_SmartUpdatePanel_Toolbars_Dock_Area_Right:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Right.
        THIS-OBJECT:m_SmartUpdatePanel_Toolbars_Dock_Area_Right:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_SmartUpdatePanel_Toolbars_Dock_Area_Right:Location = NEW System.Drawing.Point(867, 28).
        THIS-OBJECT:m_SmartUpdatePanel_Toolbars_Dock_Area_Right:Name = "m_SmartUpdatePanel_Toolbars_Dock_Area_Right":U.
        THIS-OBJECT:m_SmartUpdatePanel_Toolbars_Dock_Area_Right:Size = NEW System.Drawing.Size(0, 32).
        THIS-OBJECT:m_SmartUpdatePanel_Toolbars_Dock_Area_Right:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_SmartUpdatePanel_Toolbars_Dock_Area_Left */
        /*  */
        THIS-OBJECT:m_SmartUpdatePanel_Toolbars_Dock_Area_Left:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_SmartUpdatePanel_Toolbars_Dock_Area_Left:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_SmartUpdatePanel_Toolbars_Dock_Area_Left:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Left.
        THIS-OBJECT:m_SmartUpdatePanel_Toolbars_Dock_Area_Left:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_SmartUpdatePanel_Toolbars_Dock_Area_Left:Location = NEW System.Drawing.Point(0, 28).
        THIS-OBJECT:m_SmartUpdatePanel_Toolbars_Dock_Area_Left:Name = "m_SmartUpdatePanel_Toolbars_Dock_Area_Left":U.
        THIS-OBJECT:m_SmartUpdatePanel_Toolbars_Dock_Area_Left:Size = NEW System.Drawing.Size(0, 32).
        THIS-OBJECT:m_SmartUpdatePanel_Toolbars_Dock_Area_Left:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_SmartUpdatePanel_Toolbars_Dock_Area_Bottom */
        /*  */
        THIS-OBJECT:m_SmartUpdatePanel_Toolbars_Dock_Area_Bottom:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_SmartUpdatePanel_Toolbars_Dock_Area_Bottom:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_SmartUpdatePanel_Toolbars_Dock_Area_Bottom:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Bottom.
        THIS-OBJECT:m_SmartUpdatePanel_Toolbars_Dock_Area_Bottom:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_SmartUpdatePanel_Toolbars_Dock_Area_Bottom:Location = NEW System.Drawing.Point(0, 60).
        THIS-OBJECT:m_SmartUpdatePanel_Toolbars_Dock_Area_Bottom:Name = "m_SmartUpdatePanel_Toolbars_Dock_Area_Bottom":U.
        THIS-OBJECT:m_SmartUpdatePanel_Toolbars_Dock_Area_Bottom:Size = NEW System.Drawing.Size(867, 0).
        THIS-OBJECT:m_SmartUpdatePanel_Toolbars_Dock_Area_Bottom:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_SmartUpdatePanel_Toolbars_Dock_Area_Top */
        /*  */
        THIS-OBJECT:m_SmartUpdatePanel_Toolbars_Dock_Area_Top:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_SmartUpdatePanel_Toolbars_Dock_Area_Top:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        THIS-OBJECT:m_SmartUpdatePanel_Toolbars_Dock_Area_Top:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Top.
        THIS-OBJECT:m_SmartUpdatePanel_Toolbars_Dock_Area_Top:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_SmartUpdatePanel_Toolbars_Dock_Area_Top:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:m_SmartUpdatePanel_Toolbars_Dock_Area_Top:Name = "m_SmartUpdatePanel_Toolbars_Dock_Area_Top":U.
        THIS-OBJECT:m_SmartUpdatePanel_Toolbars_Dock_Area_Top:Size = NEW System.Drawing.Size(867, 28).
        THIS-OBJECT:m_SmartUpdatePanel_Toolbars_Dock_Area_Top:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* SmartPanelController */
        /*  */
        THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SmartUpdatePanel_Toolbars_Dock_Area_Left).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SmartUpdatePanel_Toolbars_Dock_Area_Right).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SmartUpdatePanel_Toolbars_Dock_Area_Bottom).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_SmartUpdatePanel_Toolbars_Dock_Area_Top).
        THIS-OBJECT:Name = "SmartPanelController":U.
        THIS-OBJECT:Size = NEW System.Drawing.Size(867, 60).
        CAST(THIS-OBJECT:ultraToolbarsManager1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the SizeChanged event
        Notes:
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID OnSizeChanged (e AS System.EventArgs):

        SUPER:OnSizeChanged(INPUT e).

        IF VALID-OBJECT (THIS-OBJECT:ultraToolbarsManager1) AND
           VALID-OBJECT (THIS-OBJECT:ultraToolbarsManager1:Toolbars[0]) THEN DO:

            THIS-OBJECT:Size = NEW System.Drawing.Size (THIS-OBJECT:Width,
                                                        THIS-OBJECT:ultraToolbarsManager1:Toolbars[0]:Height) .
            THIS-OBJECT:MinimumSize = NEW System.Drawing.Size (100,
                                                               THIS-OBJECT:ultraToolbarsManager1:Toolbars[0]:Height) .
            THIS-OBJECT:MaximumSize = NEW System.Drawing.Size (5000,
                                                               THIS-OBJECT:ultraToolbarsManager1:Toolbars[0]:Height) .

        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Assign customized Caption CustomizerCaption and ToolTipText to the
        Notes:   Tools of the Toolbar.
                 Invoked by the Method OnEndInit
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID InitializeCaptionOfTools ():

        IF THIS-OBJECT:ultraToolbarsManager1:Toolbars:Exists ("NavigationToolbar":U) THEN
            THIS-OBJECT:ultraToolbarsManager1:Toolbars:Item["NavigationToolbar":U]:Text = SmartComponentLibraryCustomizer:SmartToolbarController_NavigationToolBar_Text.

        IF THIS-OBJECT:ultraToolbarsManager1:Tools:Exists ("NavigationFirst":U) THEN DO:
            THIS-OBJECT:ultraToolbarsManager1:Tools["NavigationFirst":U]:SharedProps:Caption = SmartComponentLibraryCustomizer:SmartToolbarController_NavigationToolNavigationFirst_ButtonCaption .
            THIS-OBJECT:ultraToolbarsManager1:Tools["NavigationFirst":U]:SharedProps:CustomizerCaption = SmartComponentLibraryCustomizer:SmartToolbarController_NavigationToolNavigationFirst_ButtonCustomizerCaption .
            THIS-OBJECT:ultraToolbarsManager1:Tools["NavigationFirst":U]:SharedProps:ToolTipText = SmartComponentLibraryCustomizer:SmartToolbarController_NavigationToolNavigationFirst_ButtonToolTipText .
        END.
        IF THIS-OBJECT:ultraToolbarsManager1:Tools:Exists ("NavigationPrev":U) THEN DO:
            THIS-OBJECT:ultraToolbarsManager1:Tools["NavigationPrev":U]:SharedProps:Caption = SmartComponentLibraryCustomizer:SmartToolbarController_NavigationToolNavigationPrev_ButtonCaption .
            THIS-OBJECT:ultraToolbarsManager1:Tools["NavigationPrev":U]:SharedProps:CustomizerCaption = SmartComponentLibraryCustomizer:SmartToolbarController_NavigationToolNavigationPrev_ButtonCustomizerCaption .
            THIS-OBJECT:ultraToolbarsManager1:Tools["NavigationPrev":U]:SharedProps:ToolTipText = SmartComponentLibraryCustomizer:SmartToolbarController_NavigationToolNavigationPrev_ButtonToolTipText .
        END.
        IF THIS-OBJECT:ultraToolbarsManager1:Tools:Exists ("NavigationNext":U) THEN DO:
            THIS-OBJECT:ultraToolbarsManager1:Tools["NavigationNext":U]:SharedProps:Caption = SmartComponentLibraryCustomizer:SmartToolbarController_NavigationToolNavigationNext_ButtonCaption .
            THIS-OBJECT:ultraToolbarsManager1:Tools["NavigationNext":U]:SharedProps:CustomizerCaption = SmartComponentLibraryCustomizer:SmartToolbarController_NavigationToolNavigationNext_ButtonCustomizerCaption .
            THIS-OBJECT:ultraToolbarsManager1:Tools["NavigationNext":U]:SharedProps:ToolTipText = SmartComponentLibraryCustomizer:SmartToolbarController_NavigationToolNavigationNext_ButtonToolTipText .
        END.
        IF THIS-OBJECT:ultraToolbarsManager1:Tools:Exists ("NavigationLast":U) THEN DO:
            THIS-OBJECT:ultraToolbarsManager1:Tools["NavigationLast":U]:SharedProps:Caption = SmartComponentLibraryCustomizer:SmartToolbarController_NavigationToolNavigationLast_ButtonCaption .
            THIS-OBJECT:ultraToolbarsManager1:Tools["NavigationLast":U]:SharedProps:CustomizerCaption = SmartComponentLibraryCustomizer:SmartToolbarController_NavigationToolNavigationLast_ButtonCustomizerCaption .
            THIS-OBJECT:ultraToolbarsManager1:Tools["NavigationLast":U]:SharedProps:ToolTipText = SmartComponentLibraryCustomizer:SmartToolbarController_NavigationToolNavigationLast_ButtonToolTipText .
        END.

        IF THIS-OBJECT:ultraToolbarsManager1:Toolbars:Exists ("TableIOToolbar":U) THEN
            THIS-OBJECT:ultraToolbarsManager1:Toolbars:Item["TableIOToolbar":U]:Text = SmartComponentLibraryCustomizer:SmartToolbarController_TableIOToolBar_Text.

        IF THIS-OBJECT:ultraToolbarsManager1:Tools:Exists ("TableIOAdd":U) THEN DO:
            THIS-OBJECT:ultraToolbarsManager1:Tools["TableIOAdd":U]:SharedProps:Caption = SmartComponentLibraryCustomizer:SmartToolbarController_TableIOToolTableIOAdd_ButtonCaption .
            THIS-OBJECT:ultraToolbarsManager1:Tools["TableIOAdd":U]:SharedProps:CustomizerCaption = SmartComponentLibraryCustomizer:SmartToolbarController_TableIOToolTableIOAdd_ButtonCustomizerCaption .
            THIS-OBJECT:ultraToolbarsManager1:Tools["TableIOAdd":U]:SharedProps:ToolTipText = SmartComponentLibraryCustomizer:SmartToolbarController_TableIOToolTableIOAdd_ButtonToolTipText .
        END.
        IF THIS-OBJECT:ultraToolbarsManager1:Tools:Exists ("TableIOCopy":U) THEN DO:
            THIS-OBJECT:ultraToolbarsManager1:Tools["TableIOCopy":U]:SharedProps:Caption = SmartComponentLibraryCustomizer:SmartToolbarController_TableIOToolTableIOCopy_ButtonCaption .
            THIS-OBJECT:ultraToolbarsManager1:Tools["TableIOCopy":U]:SharedProps:CustomizerCaption = SmartComponentLibraryCustomizer:SmartToolbarController_TableIOToolTableIOCopy_ButtonCustomizerCaption .
            THIS-OBJECT:ultraToolbarsManager1:Tools["TableIOCopy":U]:SharedProps:ToolTipText = SmartComponentLibraryCustomizer:SmartToolbarController_TableIOToolTableIOCopy_ButtonToolTipText .
        END.
        IF THIS-OBJECT:ultraToolbarsManager1:Tools:Exists ("TableIOUpdate":U) THEN DO:
            THIS-OBJECT:ultraToolbarsManager1:Tools["TableIOUpdate":U]:SharedProps:Caption = SmartComponentLibraryCustomizer:SmartToolbarController_TableIOToolTableIOUpdate_ButtonCaption .
            THIS-OBJECT:ultraToolbarsManager1:Tools["TableIOUpdate":U]:SharedProps:CustomizerCaption = SmartComponentLibraryCustomizer:SmartToolbarController_TableIOToolTableIOUpdate_ButtonCustomizerCaption .
            THIS-OBJECT:ultraToolbarsManager1:Tools["TableIOUpdate":U]:SharedProps:ToolTipText = SmartComponentLibraryCustomizer:SmartToolbarController_TableIOToolTableIOUpdate_ButtonToolTipText .
        END.
        IF THIS-OBJECT:ultraToolbarsManager1:Tools:Exists ("TableIOSave":U) THEN DO:
            THIS-OBJECT:ultraToolbarsManager1:Tools["TableIOSave":U]:SharedProps:Caption = SmartComponentLibraryCustomizer:SmartToolbarController_TableIOToolTableIOSave_ButtonCaption .
            THIS-OBJECT:ultraToolbarsManager1:Tools["TableIOSave":U]:SharedProps:CustomizerCaption = SmartComponentLibraryCustomizer:SmartToolbarController_TableIOToolTableIOSave_ButtonCustomizerCaption .
            THIS-OBJECT:ultraToolbarsManager1:Tools["TableIOSave":U]:SharedProps:ToolTipText = SmartComponentLibraryCustomizer:SmartToolbarController_TableIOToolTableIOSave_ButtonToolTipText .
        END.
        IF THIS-OBJECT:ultraToolbarsManager1:Tools:Exists ("TableIOCancel":U) THEN DO:
            THIS-OBJECT:ultraToolbarsManager1:Tools["TableIOCancel":U]:SharedProps:Caption = SmartComponentLibraryCustomizer:SmartToolbarController_TableIOToolTableIOCancel_ButtonCaption .
            THIS-OBJECT:ultraToolbarsManager1:Tools["TableIOCancel":U]:SharedProps:CustomizerCaption = SmartComponentLibraryCustomizer:SmartToolbarController_TableIOToolTableIOCancel_ButtonCustomizerCaption .
            THIS-OBJECT:ultraToolbarsManager1:Tools["TableIOCancel":U]:SharedProps:ToolTipText = SmartComponentLibraryCustomizer:SmartToolbarController_TableIOToolTableIOCancel_ButtonToolTipText .
        END.
        IF THIS-OBJECT:ultraToolbarsManager1:Tools:Exists ("TableIODelete":U) THEN DO:
            THIS-OBJECT:ultraToolbarsManager1:Tools["TableIODelete":U]:SharedProps:Caption = SmartComponentLibraryCustomizer:SmartToolbarController_TableIOToolTableIODelete_ButtonCaption .
            THIS-OBJECT:ultraToolbarsManager1:Tools["TableIODelete":U]:SharedProps:CustomizerCaption = SmartComponentLibraryCustomizer:SmartToolbarController_TableIOToolTableIODelete_ButtonCustomizerCaption .
            THIS-OBJECT:ultraToolbarsManager1:Tools["TableIODelete":U]:SharedProps:ToolTipText = SmartComponentLibraryCustomizer:SmartToolbarController_TableIOToolTableIODelete_ButtonToolTipText .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns if the given tool is restricted by the IToolbarItemSecurityService
        Notes:   Requires EndInit() to be executed first, not applicable to the
                 TableIO Tools as they are maintaind on a per SmartTableIOTarget base
        @param pcToolKey The key of the tool to test
        @return Logical value indicating of the given tool is restricted by the
                IToolbarItemSecurityService service
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL IsToolRestricted (pcToolKey AS CHARACTER):

        RETURN oController:IsToolRestricted (pcToolKey) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns a new instance of the TableIOTargetControl class
        Notes:
        @return The reference to the new instance of the TableIOTargetControl class
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC TableIOTargetControl InvokeNewTableIOTargetControl ():

        RETURN NewTableIOTargetControl () .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Factory method for instances of the TableIOTargetControl class
        Notes:   Allows customers to return an extended version of the class in a custom
                 variant of the SmartToolbarController class
        @return The new instance of the TableIOTargetControl class
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED TableIOTargetControl NewTableIOTargetControl ():

        RETURN NEW TableIOTargetControl () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event Handler method for Designer Verbs
        Notes:   This method is intended to be overridden
        @param pcVerbText The text indicating the verb that was clicked
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID OnVerbClicked (pcVerbText AS CHARACTER):

        SUPER:OnVerbClicked (pcVerbText).

        CASE pcVerbText:
            WHEN "SmartPanel Designer":U THEN
                THIS-OBJECT:ShowSmartPanelDesigner() .
        END CASE .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Performs Toolbar Security
        Notes:
        @param poSmartTableIOTarget The ISmartTableIOTarget to perform authorization for
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID PerformToolbarSecurity (poSmartTableIOTarget AS ISmartTableIOTarget):

        oController:PerformToolbarSecurity (poSmartTableIOTarget) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Performs Toolbar Security for non standard tools
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID PerformToolbarSecurity ():

        oController:PerformToolbarSecurity () .

    END METHOD .

    /*------------------------------------------------------------------------------
       Purpose: Raises the ActiveSmartCommitTargetChanged event
       Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RaiseActiveSmartCommitTargetChanged ():

        /*THIS-OBJECT:OnActiveSmartCommitTargetChanged (System.EventArgs:Empty) .*/

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Registers a SmartNavigationTarget with the SmartNavigationSource.
        Notes:   This Method is invoked by the SmartDataAdapter in the setter of
                 the Property SmartNavigationSource.
                 Enforced by the Interface ISmartNavigationSource.
        @param poSmartNavigationTarget The ISmartNavigationTarget to register
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RegisterSmartNavigationTarget (poSmartNavigationTarget AS ISmartNavigationTarget):

        oController:RegisterSmartNavigationTarget (poSmartNavigationTarget) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Deregisters a SmartNavigationTarget from the SmartNavigationSource.
        Notes:
        @param poSmartNavigationTarget The ISmartNavigationTarget to deregister
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DeregisterSmartNavigationTarget (poSmartNavigationTarget AS ISmartNavigationTarget):

        oController:DeregisterSmartNavigationTarget (poSmartNavigationTarget) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Activate the given SmartNavigationTarget if it is registered with
                 the SmartNavigationSource.
        Notes:   This Method has to be called if one Toolbar handels two or more
                 SmartDataSources at a time.
                 Enforced by Interface ISmartNavigationSource.
        @param poSmartNavigationTarget The ISmartNavigationTarget instance to activate
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ActivateSmartNavigationTarget (poSmartNavigationTarget AS ISmartNavigationTarget):

        oController:ActivateSmartNavigationTarget (poSmartNavigationTarget) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Creates a ButtonTool with Key, Caption, Customizer Caption and
                 Image.
        Notes:   This Method is executed by CreateSmartNavigationTools and
                 CreateSmartTableIOTools when adding Navigation and TableIO Toolbars
                 to the SmartToolbarController.
                 Used to create the default Navigation and TableIO tools
        @param pcButtonKey Key used to identify the Button
        @param pcButtonCaption The Caption of the Button
        @param pcButtonCustomizerCaption The CustomizerCaption of the Button
        @param pcButtonImage The Image of the Button
        @return The reference to the new ButtonTool
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC Infragistics.Win.UltraWinToolbars.ButtonTool CreateButtonTool (pcButtonKey AS CHARACTER,
                                                                                 pcButtonCaption AS CHARACTER,
                                                                                 pcButtonCustomizerCaption AS CHARACTER,
                                                                                 pcButtonImage AS CHARACTER):

        DEFINE VARIABLE oButton            AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        DEFINE VARIABLE buttonAppearance   AS Infragistics.Win.Appearance                  NO-UNDO.
        DEFINE VARIABLE image              AS System.Drawing.Bitmap                        NO-UNDO.

        oButton = NEW Infragistics.Win.UltraWinToolbars.ButtonTool(pcButtonKey).

        oButton:SharedProps:Caption = pcButtonCaption .
        oButton:SharedProps:CustomizerCaption = pcButtonCustomizerCaption .

        FILE-INFORMATION:FILE-NAME = pcButtonImage .
        IF FILE-INFORMATION:FULL-PATHNAME <> ? THEN DO:
            image = Consultingwerk.Util.ImageHelper:Load (FILE-INFORMATION:FULL-PATHNAME) .
            image:MakeTransparent() .

            buttonAppearance = NEW Infragistics.Win.Appearance().
            buttonAppearance:Image = image .

            oButton:SharedProps:AppearancesSmall:Appearance = buttonAppearance.
        END.

        RETURN oButton.

    END METHOD.

     /*------------------------------------------------------------------------------
        Purpose: Disables the tool with the given Key when it exists in this instance.
                 Invoked by DisableTools.
        Notes:
        @param  pcToolKey Key of the Tool ButtonTool to be disabled
        @return True if the Tool exists, False if the Tool does not Exist
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL DisableControlTool (pcToolKey AS CHARACTER):

        IF THIS-OBJECT:ultraToolbarsManager1:Tools:Exists(pcToolKey) THEN DO:
            THIS-OBJECT:ultraToolbarsManager1:Tools[pcToolKey]:SharedProps:Enabled = FALSE .

            RETURN TRUE .
        END.

        RETURN FALSE .

    END METHOD .

     /*------------------------------------------------------------------------------
        Purpose: Disables the tool with the given Key when it exists in this instance.
                 Invoked by DisableTools.
        Notes:
        @param  pcToolKey Key of the Tool ButtonTool to be disabled
        @return True if the Tool exists, False if the Tool does not Exist
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL DisableTool (pcToolKey AS CHARACTER):

        RETURN oController:DisableTool (pcToolKey) .

    END METHOD.


    /*------------------------------------------------------------------------------
        Purpose: Disables a list of tools when they exist in this instance.
                 This Method is called from within the Toolbar depending on what
                 action has been performed and what actions are possible with the
                 current SmartTableIOTarget and SmartNavigationTarget.
        Notes:
        @param pcToolKeys Comma delimeted list of ButtonTool Keys to be disabled
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DisableTools (pcToolKeys AS CHARACTER):

        oController:DisableTools (pcToolKeys) .

    END METHOD.

     /*------------------------------------------------------------------------------
        Purpose: Enables the tool with the given Key when it exists in this instance.
        Notes:   Invoked by the SmartToolbarStateController:EnableTool. Does not check
                 for any further authentication
        @param  pcToolKey Key of the Tool ButtonTool to be enabled
        @return True if the Tool exists, False if the Tool does not Exist
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL EnableControlTool (pcToolKey AS CHARACTER):

        IF THIS-OBJECT:ultraToolbarsManager1:Tools:Exists(pcToolKey) THEN DO:
            THIS-OBJECT:ultraToolbarsManager1:Tools[pcToolKey]:SharedProps:Enabled = TRUE .

            RETURN TRUE .
        END.

        RETURN FALSE .

    END METHOD.

     /*------------------------------------------------------------------------------
        Purpose: Enables the tool with the given Key when it exists in this instance.
        Notes:   Invoked by EnableTools.
        @param  pcToolKey Key of the Tool ButtonTool to be enabled
        @return True if the Tool exists, False if the Tool does not Exist
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL EnableTool (pcToolKey AS CHARACTER):

        RETURN oController:EnableTool (pcToolKey) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Enables a list of tools when they exist in this instance.
        Notes:   This Method is called from within the Toolbar depending on what
                 action has been performed and what actions are possible with the
                 current SmartTableIOTarget and SmartNavigationTarget.
        @param pcToolKeys Comma delimeted list of ButtonTool Keys to be disabled
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID EnableTools (pcToolKeys AS CHARACTER):

        oController:EnableTools (pcToolKeys) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns if the Toolbar has a tool with the given key
        Notes:
        @param pcToolKey The key of the tool to check for
        @return Logical value indicating if the tool has a key with the given name
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL HasTool (pcToolKey AS CHARACTER):

        RETURN THIS-OBJECT:ultraToolbarsManager1:Tools:Exists (pcToolKey) .

    END METHOD .

     /*------------------------------------------------------------------------------
        Purpose: Registers a SmartTableIOTarget with the SmartTableIOSource.
        Notes:   This Method is invoked by the SmartDataAdapter in the setter of
                 the Property SmartTableIOSource.
                 Enforced by the Interface ISmartTableIOSource.
        @param poSmartTableIOTarget The ISmartTableIOTarget to register
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RegisterSmartTableIOTarget (poSmartTableIOTarget AS ISmartTableIOTarget):

        oController:RegisterSmartTableIOTarget (poSmartTableIOTarget) .

    END METHOD.

     /*------------------------------------------------------------------------------
        Purpose: Deregisters a SmartTableIOTarget with the SmartTableIOSource.
        Notes:
        @param poSmartTableIOTarget The ISmartTableIOTarget to deregister
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DeregisterSmartTableIOTarget (poSmartTableIOTarget AS ISmartTableIOTarget):

        oController:DeregisterSmartTableIOTarget (poSmartTableIOTarget) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Activate the given SmartTableIOTarget if it is registered as a
                 SmartTableIOSource.
        Notes:   This Method has to be called if one Toolbar handels two or more
                 SmartDataSources at a time.
                 Enforced by the Interface ISmartTableIOSource.
        @param poSmartTableIOTarget The ISmartTableIOTarget to activate
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ActivateSmartTableIOTarget (poSmartTableIOTarget AS ISmartTableIOTarget):

        oController:ActivateSmartTableIOTarget (poSmartTableIOTarget) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Sets the Visual Designer Properties
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID SetDesignerProperties ():

        THIS-OBJECT:DesignerVerbs = "SmartPanel Designer":U .
        THIS-OBJECT:NonBrowsableProperties = "SmartPanelDesigner":U .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Allows to permanently disable TableIO Tools in the SmartToolbarController
        Notes:   Useful for applying security settings to the toolbar
                 http://confluence.consultingwerkcloud.com/wiki/pages/viewpage.action?pageId=722342
                 See http://jira.consultingwerkcloud.com/browse/SCL-442 for details on
                 extending this for custom buttons
        @param poSmartTableIOTarget The reference to the ISmartTableIOTarget for this an action should be disabled
        @param poTableIOAction The TableIOActionEnum member identifying the action to enable/disable
        @param plDisabled Logical value indicating if the tool should be disabled. Set to FALSE to enable the tool
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SetTableIOActionDisabled (poSmartTableIOTarget AS ISmartTableIOTarget,
                                                 poTableIOAction AS TableIOActionEnum,
                                                 plDisabled AS LOGICAL):

        oController:SetTableIOActionDisabled (poSmartTableIOTarget,
                                              poTableIOAction,
                                              plDisabled) .
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Allows to permanently disable TableIO Tools in the SmartToolbarController
        Notes:   Useful for applying security settings to the toolbar
                 http://confluence.consultingwerkcloud.com/wiki/pages/viewpage.action?pageId=722342
                 See http://jira.consultingwerkcloud.com/browse/SCL-442 for details on
                 extending this for custom buttons
        @param poSmartTableIOTarget The reference to the ISmartTableIOTarget for this an action should be disabled
        @param poTableIOAction The TableIOActionEnum member identifying the action to enable/disable
        @param poTableIOAction2 The TableIOActionEnum member identifying the action to enable/disable
        @param plDisabled Logical value indicating if the tool should be disabled. Set to FALSE to enable the tool
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SetTableIOActionDisabled (poSmartTableIOTarget AS ISmartTableIOTarget,
                                                 poTableIOAction AS TableIOActionEnum,
                                                 poTableIOAction2 AS TableIOActionEnum,
                                                 plDisabled AS LOGICAL):

        oController:SetTableIOActionDisabled (poSmartTableIOTarget,
                                              poTableIOAction,
                                              poTableIOAction2,
                                              plDisabled) .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Allows to permanently disable TableIO Tools in the SmartToolbarController
        Notes:   Useful for applying security settings to the toolbar
                 http://confluence.consultingwerkcloud.com/wiki/pages/viewpage.action?pageId=722342
                 See http://jira.consultingwerkcloud.com/browse/SCL-442 for details on
                 extending this for custom buttons
        @param poSmartTableIOTarget The reference to the ISmartTableIOTarget for this an action should be disabled
        @param poTableIOAction The TableIOActionEnum member identifying the action to enable/disable
        @param poTableIOAction2 The TableIOActionEnum member identifying the action to enable/disable
        @param poTableIOAction3 The TableIOActionEnum member identifying the action to enable/disable
        @param plDisabled Logical value indicating if the tool should be disabled. Set to FALSE to enable the tool
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SetTableIOActionDisabled (poSmartTableIOTarget AS ISmartTableIOTarget,
                                                 poTableIOAction AS TableIOActionEnum,
                                                 poTableIOAction2 AS TableIOActionEnum,
                                                 poTableIOAction3 AS TableIOActionEnum,
                                                 plDisabled AS LOGICAL):

        oController:SetTableIOActionDisabled (poSmartTableIOTarget,
                                              poTableIOAction,
                                              poTableIOAction2,
                                              poTableIOAction3,
                                              plDisabled) .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Allows to permanently disable TableIO Tools in the SmartToolbarController
        Notes:   Useful for applying security settings to the toolbar
                 http://confluence.consultingwerkcloud.com/wiki/pages/viewpage.action?pageId=722342
                 See http://jira.consultingwerkcloud.com/browse/SCL-442 for details on
                 extending this for custom buttons
        @param poSmartTableIOTarget The reference to the ISmartTableIOTarget for this an action should be disabled
        @param poTableIOAction The TableIOActionEnum member identifying the action to enable/disable
        @param poTableIOAction2 The TableIOActionEnum member identifying the action to enable/disable
        @param poTableIOAction3 The TableIOActionEnum member identifying the action to enable/disable
        @param poTableIOAction4 The TableIOActionEnum member identifying the action to enable/disable
        @param plDisabled Logical value indicating if the tool should be disabled. Set to FALSE to enable the tool
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SetTableIOActionDisabled (poSmartTableIOTarget AS ISmartTableIOTarget,
                                                 poTableIOAction AS TableIOActionEnum,
                                                 poTableIOAction2 AS TableIOActionEnum,
                                                 poTableIOAction3 AS TableIOActionEnum,
                                                 poTableIOAction4 AS TableIOActionEnum,
                                                 plDisabled AS LOGICAL):

        oController:SetTableIOActionDisabled (poSmartTableIOTarget,
                                              poTableIOAction,
                                              poTableIOAction2,
                                              poTableIOAction3,
                                              poTableIOAction4,
                                              plDisabled) .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Allows to permanently disable TableIO Tools in the SmartToolbarController
        Notes:   Useful for applying security settings to the toolbar
                 http://confluence.consultingwerkcloud.com/wiki/pages/viewpage.action?pageId=722342
                 See http://jira.consultingwerkcloud.com/browse/SCL-442 for details on
                 extending this for custom buttons
        @param poSmartTableIOTarget The reference to the ISmartTableIOTarget for this an action should be disabled
        @param poTableIOAction The TableIOActionEnum member identifying the action to enable/disable
        @param poTableIOAction2 The TableIOActionEnum member identifying the action to enable/disable
        @param poTableIOAction3 The TableIOActionEnum member identifying the action to enable/disable
        @param poTableIOAction4 The TableIOActionEnum member identifying the action to enable/disable
        @param poTableIOAction5 The TableIOActionEnum member identifying the action to enable/disable
        @param plDisabled Logical value indicating if the tool should be disabled. Set to FALSE to enable the tool
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SetTableIOActionDisabled (poSmartTableIOTarget AS ISmartTableIOTarget,
                                                 poTableIOAction AS TableIOActionEnum,
                                                 poTableIOAction2 AS TableIOActionEnum,
                                                 poTableIOAction3 AS TableIOActionEnum,
                                                 poTableIOAction4 AS TableIOActionEnum,
                                                 poTableIOAction5 AS TableIOActionEnum,
                                                 plDisabled AS LOGICAL):

        oController:SetTableIOActionDisabled (poSmartTableIOTarget,
                                              poTableIOAction,
                                              poTableIOAction2,
                                              poTableIOAction3,
                                              poTableIOAction4,
                                              poTableIOAction5,
                                              plDisabled) .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Shows the Designer Dialog
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ShowSmartPanelDesigner ():

        DEFINE VARIABLE cNavigationTools AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cTableIOTools    AS CHARACTER NO-UNDO .

        DEFINE VARIABLE cMessages        AS CHARACTER NO-UNDO .
        DEFINE VARIABLE i                AS INTEGER   NO-UNDO .

        IF NOT THIS-OBJECT:DesignTime THEN
            RETURN .

        ASSIGN cNavigationTools = THIS-OBJECT:VisibleNavigationTools
               cTableIOTools    = THIS-OBJECT:VisibleTableIOTools .

        /* Mike Fechner, Consultingwerk Ltd. 05.01.2010
           Access to Dialoges in Consultingwerk.SmartComponents.Design.dll
           outsourced to .p file, this removes the runtime dependency on the design
           time assembly.
           .R File is launched, to prevent runtime compile */
        IF SEARCH ("Consultingwerk/SmartComponents/Support/SmartPanelControllerDesignerForm_ShowDesignerDialog.r":U) = ?
            THEN DO:

            MESSAGE "Unable to invoke Consultingwerk.SmartComponents.Design.dll"{&TRAN} SKIP(1)
                    "Consultingwerk/SmartComponents/Support/SmartPanelControllerDesignerForm_ShowDesignerDialog.r":U SKIP
                    "could not be found!"{&TRAN}
                VIEW-AS ALERT-BOX ERROR TITLE "SmartPanelController"{&TRAN} .

            RETURN .
        END.

        RUN Consultingwerk/SmartComponents/Support/SmartPanelControllerDesignerForm_ShowDesignerDialog.r
           (INPUT-OUTPUT cNavigationTools,
            INPUT-OUTPUT cTableIOTools) NO-ERROR .

        IF ERROR-STATUS:ERROR THEN DO:
            DO i = 1 TO ERROR-STATUS:NUM-MESSAGES:
                ASSIGN cMessages = cMessages +
                                   (IF i > 1 THEN CHR(13) ELSE "":U) +
                                   ERROR-STATUS:GET-MESSAGE (i) .
            END.

            MESSAGE "Unable to invoke Consultingwerk.SmartComponents.Design.dll"{&TRAN} SKIP(1)
                    cMessages
                VIEW-AS ALERT-BOX ERROR TITLE "SmartPanelController"{&TRAN} .

            RETURN .
        END.

        ASSIGN THIS-OBJECT:VisibleNavigationTools = cNavigationTools
               THIS-OBJECT:VisibleTableIOTools    = cTableIOTools    .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ToolClick event of the ultraToolbarsManager1
        Notes:   Depending on which ButtonTool has been clicked the corresponding
                 action is performed by calling a routine in the active
                 SmartNavigationTarget or SmartTableIOTarget.
                 This Method is executed as the Handler if the ToolClick Event of
                 the base class is fired.
        @param sender The reference to the object that raised the event
        @param e The Infragistics.Win.UltraWinToolbars.ToolClickEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ToolClickHandler (sender AS System.Object,
                                            e AS ToolClickEventArgs):

        oController:HandleToolClick (e:Tool:Key) .

        /* Mike Fechner, Consultingwerk Ltd. 27.12.2009
           This is considered a UI event handler, so there should be a CATCH block */
        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the SmartNavigationTargetPositionChanged event of the SmartNavigationTarget
        Notes:
        @param sender The reference to the object that raised the evnet
        @param e The SmartNavigationTargetPositionChangedEventArgs instance with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID UpdateSmartNavigationTargetPosition (sender AS System.Object,
                                                            e AS SmartNavigationTargetPositionChangedEventArgs):

        oController:UpdateSmartNavigationTargetPosition (sender,
                                                         e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Handles the update of a Position from the SmartNavigationTarget.
                 This Method is called from the SmartDataAdapter derived Classes
                 if the PositionChangedHandler is executed or from inside the
                 ISmartNavigationSource implementing class when the Method
                 UpdateSmartNavigationTargetState or RegisterSmartNavigationTarget
                 are executed.
        Notes:   Enforced by Interface ISmartNavigationSource.
        @param poSmartNavigationTarget The reference to the current SmartNavigationTarget
        @param pcPosition The RecordPositionEnum member indicating the new navigation target position
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID UpdateSmartNavigationTargetPosition (poSmartNavigationTarget AS ISmartNavigationTarget,
                                                               pcPosition AS CHARACTER):

        oController:UpdateSmartNavigationTargetPosition (poSmartNavigationTarget,
                                                         pcPosition)  .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the SmartNavigationTargetStateChanged event
        Notes:
        @param sender The reference to the object that raised the event
        @param e The SmartNavigationTargetStateChangedEventArgs instance with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID UpdateSmartNavigationTargetState (sender AS System.Object,
                                                         e AS SmartNavigationTargetStateChangedEventArgs):

        oController:UpdateSmartNavigationTargetState (sender,
                                                      e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Handles the update of a State from the SmartNavigationTarget.
                 This Method is called from the SmartDataAdapter derived Classes
                 if the Property setter of SmartDataSourceState is executed.
        Notes:   Enforced by Interface ISmartNavigationSource.
        @param poSmartNavigationTarget The reference to the current SmartNavigationTarget
        @param pcState The RecordPositionEnum member indicating the new navigation target state, Updating or Navigatable
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID UpdateSmartNavigationTargetState (poSmartNavigationTarget AS ISmartNavigationTarget,
                                                            pcState AS CHARACTER):

        oController:UpdateSmartNavigationTargetState (poSmartNavigationTarget,
                                                      pcState) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the SmartTableIOStateChange event of the TableIO
                 Target
        Notes:
        @param sender The reference to the object that raised the event
        @param e The SmartTableIOStateChangedEventArgs instance with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID UpdateSmartTableIOState (sender AS System.Object,
                                                   e AS SmartTableIOStateChangedEventArgs):

        oController:UpdateSmartTableIOState (sender,
                                             e) .

    END METHOD.

     /*------------------------------------------------------------------------------
        Purpose: Handles the update of a State from the SmartTableIOTarget.
                 This Method is called from the SmartViewerControl derived Classes
                 if the Property setter of SmartTableIOState is executed.
        Notes:   Enforced by Interface ISmartTableIOSource.
        @param poSmartTableIOTarget The reference to the current ISmartTableIOTarget
        @param pcSmartTableIOState The current state of the SmartTableIOTarget instance, e.g. AddCopyAllowed, FieldsDisabled, FieldsEnabled, ModifyingData, NoDataSource
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID UpdateSmartTableIOState (poSmartTableIOTarget AS ISmartTableIOTarget,
                                                   pcSmartTableIOState AS CHARACTER):

        oController:UpdateSmartTableIOState (poSmartTableIOTarget,
                                             pcSmartTableIOState) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Updates the Buttons based on the TransactionState of the current SmartCommitTarget
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID UpdateTransactionState ():

        oController:UpdateTransactionState () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SmartPanelController class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartPanelController ( ):

        /* Mike Fechner, Consultingwerk Ltd. 24.01.2010
           Only delete ToolbarsCollection when not in Visual Designer
           to avoid conflicts when re-opening layouts. */
        IF NOT THIS-OBJECT:DesignTime THEN
            Consultingwerk.Util.UltraToolbarsHelper:EraseToolbarsCollections (ultraToolbarsManager1) .

        ultraToolbarsManager1:Dispose () .

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
