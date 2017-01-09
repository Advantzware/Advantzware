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
    File        : SmartToolbarController
    Purpose     : Main Toolbar/Ribbon Component of the SmartComponent Library
                  Provides controls for NavigationSource, TableIOSource
                  and CommitSource functions
    Syntax      :
    Description : Extents the Infragistics UltraToolbarsManager class
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Jan 17 14:42:31 CET 2009
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Exceptions.*                     FROM PROPATH .
USING Consultingwerk.Framework.Authorization.*        FROM PROPATH .
USING Consultingwerk.Framework.Collections.*          FROM PROPATH .
USING Consultingwerk.SmartComponents.*                FROM ASSEMBLY .
USING Consultingwerk.SmartComponents.Base.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.SmartComponents.Interfaces.*     FROM PROPATH .
USING Consultingwerk.SmartComponents.Resources.*      FROM PROPATH .
USING Consultingwerk.Windows.Util.Forms.*             FROM PROPATH .
USING Infragistics.Win.UltraWinToolbars.*             FROM ASSEMBLY .
USING System.ComponentModel.*                         FROM ASSEMBLY .
USING System.ComponentModel.Design.*                  FROM ASSEMBLY .
USING Progress.Lang.*                                 FROM ASSEMBLY .
USING Consultingwerk.Studio.ClassDocumentation.* FROM PROPATH.

CLASS Consultingwerk.SmartComponents.Implementation.SmartToolbarController
    INHERITS UltraToolbarsManager
    IMPLEMENTS ISmartNavigationSource, ISmartTableIOSource, ISmartCommitSource,
               Consultingwerk.SmartComponents.Interfaces.Design.IDesignNavigationSource ,
               Consultingwerk.SmartComponents.Interfaces.Design.IDesignTableIOSource,
               Consultingwerk.SmartComponents.Interfaces.Design.IDesignCommitSource,
               ICustomTypeDescriptor, IHasDesignerVerbs, ISmartToolbarControl:

    DEFINE VARIABLE lInitialized AS LOGICAL NO-UNDO INIT FALSE . /* SCL-41 */

    DEFINE VARIABLE oController AS SmartToolbarStateController NO-UNDO .

    /* Mike Fechner, Consultingwerk Ltd. 16.05.2010
       Core implementation of ICustomTypeDescriptor */
    {Consultingwerk/SmartComponents/Base/ICustomTypeDescriptor.i}
    {Consultingwerk/SmartComponents/Base/CreateVerbs.i}

    /*------------------------------------------------------------------------------
        Purpose: Indicates that the ActiveSmartCommitTarget property has changed
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT ActiveSmartCommitTargetChanged DELEGATE System.EventHandler.

    /*------------------------------------------------------------------------------
        Purpose: Returns a reference to the current active SmartCommitTarget
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ActiveSmartCommitTarget AS ISmartCommitTarget NO-UNDO
    GET:
        RETURN oController:ActiveSmartCommitTarget .
    END.
    PROTECTED SET (arg AS ISmartCommitTarget):
        ASSIGN oController:ActiveSmartCommitTarget = arg .
    END SET.

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
        Purpose: This property is supposed to be set ONCE in the Visual Designer
                 to create the default set of Navigation Tools
        Notes:   In 10.2A a PUBLIC PROPERTY seems to be the only interaction possible
                 between custom ABL code and the Visual Designer. So this property
                 allows a developer to add the Tools required for the Navigation
                 Interface. This property is supposed to be "pressed" only once
                 (double click in the Property grid will set it to True, but the
                 property value will actually not change!).
                 Adding the Tools in the Visual Designer seems to be the only
                 possibility to have the tools available (for customization) in the
                 Visual Designer and not cause double tool key conflicts - as the
                 code generated for the SmartToolbarController instance
                 (UltraToolbarsManager) will add all Tools/Toolbars at runtime without
                 checking if they might have been created by the constructor etc.
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY AddNavigationTools AS LOGICAL NO-UNDO INIT FALSE
    GET:
        RETURN FALSE .
    END GET.
    SET (arg AS LOGICAL):
        IF arg = FALSE THEN
            RETURN .

        MESSAGE SmartComponentLibraryCustomizer:SmartToolbarController_AddNavigationTools
            VIEW-AS ALERT-BOX INFORMATION TITLE SmartComponentLibraryCustomizer:SmartToolbarController_AddNavigationTools_Title.

        CreateSmartNavigationTools () .
    END.

    /*------------------------------------------------------------------------------
        Purpose: This property is supposed to be set ONCE in the Visual Designer
                 to create the default set of TableIO Tools
        Notes:   In 10.2A a PUBLIC PROPERTY seems to be the only interaction possible
                 between custom ABL code and the Visual Designer. So this property
                 allows a developer to add the Tools required for the Navigation
                 Interface. This property is supposed to be "pressed" only once
                 (double click in the Property grid will set it to True, but the
                 property value will actually not change!).
                 Adding the Tools in the Visual Designer seems to be the only
                 possibility to have the tools available (for customization) in the
                 Visual Designer and not cause double tool key conflicts - as the
                 code generated for the SmartToolbarController instance
                 (UltraToolbarsManager) will add all Tools/Toolbars at runtime without
                 checking if they might have been created by the constructor etc.
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY AddTableIOTools AS LOGICAL NO-UNDO INIT FALSE
    GET:
        RETURN FALSE .
    END GET.
    SET (arg AS LOGICAL):
        IF arg = FALSE THEN
            RETURN .

        MESSAGE SmartComponentLibraryCustomizer:SmartToolbarController_AddTableIOTools
            VIEW-AS ALERT-BOX INFORMATION TITLE SmartComponentLibraryCustomizer:SmartToolbarController_AddTableIOTools.

        CreateSmartTableIOTools () .
    END.

    /*------------------------------------------------------------------------------
        Purpose: Propery flags if the current instance of the SmartBindingSource
                 is running at design time or runtime.
        Notes:   Set during constructor using a check at the
                 System.ComponentModel.LicenseManager
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DesignTime AS LOGICAL NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Property that is used to generate Designer Verbs (actions for the
                 Visual Designer)
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PROTECTED PROPERTY DesignerVerbs AS CHARACTER NO-UNDO
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Property that is used to mark properties (comma-delimited list) that
                 will be excluded from the .NET TypeDescriptor properties list
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PROTECTED PROPERTY HiddenProperties AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the List of TableIOTargetControl instances
        Notes:   See http://jira.consultingwerkcloud.com/browse/SCL-442 for details
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ListTableIOTargetControl AS ListTableIOTargetControl NO-UNDO
    GET:
        RETURN oController:ListTableIOTargetControl .
    END GET .

    /*------------------------------------------------------------------------------
        Purpose: Property that is used to mark properties (comma-delimited list) that
                 should not show up in the Visual Designers Property Grid
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PROTECTED PROPERTY NonBrowsableProperties AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the List of SmartTableIOTargets as a generic List
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmartTableIOTargets AS ListISmartTableIOTarget NO-UNDO
    GET:
        RETURN oController:SmartTableIOTargets .
    END GET.

     /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartToolbarController class
        Notes:
        @param poContainer The System.ComponentModel.IContainer used as the Container for this component
    ------------------------------------------------------------------------------*/
     CONSTRUCTOR PUBLIC SmartToolbarController (poContainer AS System.ComponentModel.IContainer):

        SUPER (poContainer).

        THIS-OBJECT:DesignTime = Progress.Util.EnumHelper:AreEqual(System.ComponentModel.LicenseManager:UsageMode,
                                                                   System.ComponentModel.LicenseUsageMode:Designtime) .

        ASSIGN oController = NEW SmartToolbarStateController (THIS-OBJECT) .

        /* Mike Fechner, Consultingwerk Ltd. 24.01.2010
           Only subscribe to Disposed event when not in Visual Designer
           to avoid conflicts when re-opening layouts. */
        IF NOT THIS-OBJECT:DesignTime THEN
            THIS-OBJECT:Disposed:Subscribe (DisposedHandler) .
        ELSE
            THIS-OBJECT:Disposed:Subscribe (DesignTimeDisposedHandler) .

        SetDesignerProperties () .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartToolbarController class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartToolbarController ():
        SUPER ().

        THIS-OBJECT:DesignTime = Progress.Util.EnumHelper:AreEqual(System.ComponentModel.LicenseManager:UsageMode,
                                                                   System.ComponentModel.LicenseUsageMode:Designtime) .

        ASSIGN oController = NEW SmartToolbarStateController (THIS-OBJECT) .

        /* Mike Fechner, Consultingwerk Ltd. 24.01.2010
           Only subscribe to Disposed event when not in Visual Designer
           to avoid conflicts when re-opening layouts. */
        IF NOT THIS-OBJECT:DesignTime THEN
            THIS-OBJECT:Disposed:Subscribe (DisposedHandler) .
        ELSE
            THIS-OBJECT:Disposed:Subscribe (DesignTimeDisposedHandler) .

        SetDesignerProperties () .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Begins a series of updates to the Control
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID BeginControlUpdate ():

        THIS-OBJECT:BeginUpdate () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Creates the default tools for the SmartCommitSource.
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID CreateSmartTransactionTools ():

        DEFINE VARIABLE toolarray AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.

        DEFINE VARIABLE buttonTool1 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        DEFINE VARIABLE buttonTool2 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.

        DEFINE VARIABLE ultraToolbar1 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.

        /* The tool navigation definitions */
        IF NOT THIS-OBJECT:Tools:Exists("TransactionCommit":U) THEN
            THIS-OBJECT:Tools:Add (CreateButtonTool("TransactionCommit":U,
                                                    SmartComponentLibraryCustomizer:SmartToolbarController_TransactionToolTransactionCommit_ButtonCaption,
                                                    SmartComponentLibraryCustomizer:SmartToolbarController_TransactionToolTransactionCommit_ButtonCustomizerCaption,
                                                    "Consultingwerk/SmartComponents/Images/table_ok_16.png":U,
                                                    "Consultingwerk/SmartComponents/Images/table_ok_32.png":U)) .

        IF NOT THIS-OBJECT:Tools:Exists("TransactionUndo":U) THEN
            THIS-OBJECT:Tools:Add (CreateButtonTool("TransactionUndo":U,
                                                    SmartComponentLibraryCustomizer:SmartToolbarController_TransactionToolTransactionUndo_ButtonCaption,
                                                    SmartComponentLibraryCustomizer:SmartToolbarController_TransactionToolTransactionUndo_ButtonCustomizerCaption,
                                                    "Consultingwerk/SmartComponents/Images/table_error_16.png":U,
                                                    "Consultingwerk/SmartComponents/Images/table_error_32.png":U)) .

        IF NOT THIS-OBJECT:Toolbars:Exists("TransactionToolbar":U) THEN DO:
            /* Create the navigation toolbar */
            ultraToolbar1 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar ("TransactionToolbar":U).
            ultraToolbar1:Text = SmartComponentLibraryCustomizer:SmartToolbarController_TransactionToolBar_Text.
            ultraToolbar1:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Top.

            /* Create the tool instances on the navigation toolbar */
            buttonTool1 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TransactionCommit":U).
            buttonTool2 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TransactionUndo":U).

            buttonTool1:InstanceProps:IsFirstInGroup = TRUE.

            toolarray[1] = buttonTool1.
            toolarray[2] = buttonTool2.

            ultraToolbar1:Tools:AddRange(toolarray) .

            THIS-OBJECT:Toolbars:Add(ultraToolbar1) .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Registers a new SmartCommitTarget
        Notes:
        @param poTarget The ISmartCommitTraget to add to this SmartToolbarController
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DeregisterSmartCommitTarget (poTarget AS ISmartCommitTarget):

        oController:DeregisterSmartCommitTarget (poTarget) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose:  Event handler for the Disposed Event of this Componetns
        Notes:    Removes links form this SmartToolbarController during design time
                  to allow linking of the targets to another component after this
                  component is removed
        @param sender The object that raised this event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID DesignTimeDisposedHandler (sender AS System.Object,
                                                     e AS System.EventArgs):

        THIS-OBJECT:Disposed:Unsubscribe (DesignTimeDisposedHandler) .

        oController:DesignTimeDisposedHandler () .

        @SuppressUnusedWarnings.
        CATCH err AS Progress.Lang.Error :
            /* ignore all errors from above */
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose:  Event handler for the Disposed Event of this Componetns
        Notes:
        @param sender The object that raised this event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID DisposedHandler (sender AS System.Object,
                                           e AS System.EventArgs):

        THIS-OBJECT:Disposed:Unsubscribe (DisposedHandler) .

        Consultingwerk.Util.ErrorHelper:ErrorMessageDialogAllowed = FALSE .

/*        Consultingwerk.Util.UltraToolbarsHelper:EraseToolbarsCollections (THIS-OBJECT) .*/

        DELETE OBJECT THIS-OBJECT .

        FINALLY:
            Consultingwerk.Util.ErrorHelper:ErrorMessageDialogAllowed = TRUE .
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Ends a series of updates to the Control
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID EndControlUpdate ():

        THIS-OBJECT:EndUpdate () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the type name of the Form that contains the Toolbar
        Notes:
        @return The type name of the Form that contains the Toolbar
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetFormTypeName ():

        DEFINE VARIABLE oControl                      AS System.Windows.Forms.Control  NO-UNDO .
        DEFINE VARIABLE oForm                         AS System.Windows.Forms.Form     NO-UNDO .

        oControl = THIS-OBJECT:DockWithinContainer .

        IF NOT VALID-OBJECT (oControl) THEN
            RETURN ? .

        IF TYPE-OF (oControl, System.Windows.Forms.Form) THEN
            oForm = CAST (oControl, System.Windows.Forms.Form) .
        ELSE
            oForm = oControl:FindForm () .

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

    /*------------------------------------------------------------------------------
        Purpose: Returns a CharacterList of all ToolKeys in the Toolbar
        Notes:
        @return The CharacterList of all ToolKeys in the Toolbar
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CharacterList GetAllToolKeys ():

        DEFINE VARIABLE oList AS CharacterList NO-UNDO .

        oList = NEW CharacterList ().

        {Consultingwerk/foreach.i Infragistics.Win.UltraWinToolbars.ToolBase oTool in THIS-OBJECT:Tools}
            oList:Add (oTool:Key) .
        END.

        RETURN oList .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Assign customized Caption CustomizerCaption and ToolTipText to the
                 Tools of the Toolbar.
                 Invoked by the Method OnEndInit
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID InitializeCaptionOfTools ():

        IF THIS-OBJECT:Toolbars:Exists ("NavigationToolbar":U) THEN
            THIS-OBJECT:Toolbars:Item["NavigationToolbar":U]:Text = SmartComponentLibraryCustomizer:SmartToolbarController_NavigationToolBar_Text.

        IF THIS-OBJECT:Tools:Exists ("NavigationFirst":U) THEN DO:
            THIS-OBJECT:Tools["NavigationFirst":U]:SharedProps:Caption = SmartComponentLibraryCustomizer:SmartToolbarController_NavigationToolNavigationFirst_ButtonCaption .
            THIS-OBJECT:Tools["NavigationFirst":U]:SharedProps:CustomizerCaption = SmartComponentLibraryCustomizer:SmartToolbarController_NavigationToolNavigationFirst_ButtonCustomizerCaption .
            THIS-OBJECT:Tools["NavigationFirst":U]:SharedProps:ToolTipText = SmartComponentLibraryCustomizer:SmartToolbarController_NavigationToolNavigationFirst_ButtonToolTipText .
        END.
        IF THIS-OBJECT:Tools:Exists ("NavigationPrev":U) THEN DO:
            THIS-OBJECT:Tools["NavigationPrev":U]:SharedProps:Caption = SmartComponentLibraryCustomizer:SmartToolbarController_NavigationToolNavigationPrev_ButtonCaption .
            THIS-OBJECT:Tools["NavigationPrev":U]:SharedProps:CustomizerCaption = SmartComponentLibraryCustomizer:SmartToolbarController_NavigationToolNavigationPrev_ButtonCustomizerCaption .
            THIS-OBJECT:Tools["NavigationPrev":U]:SharedProps:ToolTipText = SmartComponentLibraryCustomizer:SmartToolbarController_NavigationToolNavigationPrev_ButtonToolTipText .
        END.
        IF THIS-OBJECT:Tools:Exists ("NavigationNext":U) THEN DO:
            THIS-OBJECT:Tools["NavigationNext":U]:SharedProps:Caption = SmartComponentLibraryCustomizer:SmartToolbarController_NavigationToolNavigationNext_ButtonCaption .
            THIS-OBJECT:Tools["NavigationNext":U]:SharedProps:CustomizerCaption = SmartComponentLibraryCustomizer:SmartToolbarController_NavigationToolNavigationNext_ButtonCustomizerCaption .
            THIS-OBJECT:Tools["NavigationNext":U]:SharedProps:ToolTipText = SmartComponentLibraryCustomizer:SmartToolbarController_NavigationToolNavigationNext_ButtonToolTipText .
        END.
        IF THIS-OBJECT:Tools:Exists ("NavigationLast":U) THEN DO:
            THIS-OBJECT:Tools["NavigationLast":U]:SharedProps:Caption = SmartComponentLibraryCustomizer:SmartToolbarController_NavigationToolNavigationLast_ButtonCaption .
            THIS-OBJECT:Tools["NavigationLast":U]:SharedProps:CustomizerCaption = SmartComponentLibraryCustomizer:SmartToolbarController_NavigationToolNavigationLast_ButtonCustomizerCaption .
            THIS-OBJECT:Tools["NavigationLast":U]:SharedProps:ToolTipText = SmartComponentLibraryCustomizer:SmartToolbarController_NavigationToolNavigationLast_ButtonToolTipText .
        END.

        IF THIS-OBJECT:Toolbars:Exists ("TableIOToolbar":U) THEN
            THIS-OBJECT:Toolbars:Item["TableIOToolbar":U]:Text = SmartComponentLibraryCustomizer:SmartToolbarController_TableIOToolBar_Text.

        IF THIS-OBJECT:Tools:Exists ("TableIOAdd":U) THEN DO:
            THIS-OBJECT:Tools["TableIOAdd":U]:SharedProps:Caption = SmartComponentLibraryCustomizer:SmartToolbarController_TableIOToolTableIOAdd_ButtonCaption .
            THIS-OBJECT:Tools["TableIOAdd":U]:SharedProps:CustomizerCaption = SmartComponentLibraryCustomizer:SmartToolbarController_TableIOToolTableIOAdd_ButtonCustomizerCaption .
            THIS-OBJECT:Tools["TableIOAdd":U]:SharedProps:ToolTipText = SmartComponentLibraryCustomizer:SmartToolbarController_TableIOToolTableIOAdd_ButtonToolTipText .
        END.
        IF THIS-OBJECT:Tools:Exists ("TableIOCopy":U) THEN DO:
            THIS-OBJECT:Tools["TableIOCopy":U]:SharedProps:Caption = SmartComponentLibraryCustomizer:SmartToolbarController_TableIOToolTableIOCopy_ButtonCaption .
            THIS-OBJECT:Tools["TableIOCopy":U]:SharedProps:CustomizerCaption = SmartComponentLibraryCustomizer:SmartToolbarController_TableIOToolTableIOCopy_ButtonCustomizerCaption .
            THIS-OBJECT:Tools["TableIOCopy":U]:SharedProps:ToolTipText = SmartComponentLibraryCustomizer:SmartToolbarController_TableIOToolTableIOCopy_ButtonToolTipText .
        END.
        IF THIS-OBJECT:Tools:Exists ("TableIOUpdate":U) THEN DO:
            THIS-OBJECT:Tools["TableIOUpdate":U]:SharedProps:Caption = SmartComponentLibraryCustomizer:SmartToolbarController_TableIOToolTableIOUpdate_ButtonCaption .
            THIS-OBJECT:Tools["TableIOUpdate":U]:SharedProps:CustomizerCaption = SmartComponentLibraryCustomizer:SmartToolbarController_TableIOToolTableIOUpdate_ButtonCustomizerCaption .
            THIS-OBJECT:Tools["TableIOUpdate":U]:SharedProps:ToolTipText = SmartComponentLibraryCustomizer:SmartToolbarController_TableIOToolTableIOUpdate_ButtonToolTipText .
        END.
        IF THIS-OBJECT:Tools:Exists ("TableIOSave":U) THEN DO:
            THIS-OBJECT:Tools["TableIOSave":U]:SharedProps:Caption = SmartComponentLibraryCustomizer:SmartToolbarController_TableIOToolTableIOSave_ButtonCaption .
            THIS-OBJECT:Tools["TableIOSave":U]:SharedProps:CustomizerCaption = SmartComponentLibraryCustomizer:SmartToolbarController_TableIOToolTableIOSave_ButtonCustomizerCaption .
            THIS-OBJECT:Tools["TableIOSave":U]:SharedProps:ToolTipText = SmartComponentLibraryCustomizer:SmartToolbarController_TableIOToolTableIOSave_ButtonToolTipText .
        END.
        IF THIS-OBJECT:Tools:Exists ("TableIOCancel":U) THEN DO:
            THIS-OBJECT:Tools["TableIOCancel":U]:SharedProps:Caption = SmartComponentLibraryCustomizer:SmartToolbarController_TableIOToolTableIOCancel_ButtonCaption .
            THIS-OBJECT:Tools["TableIOCancel":U]:SharedProps:CustomizerCaption = SmartComponentLibraryCustomizer:SmartToolbarController_TableIOToolTableIOCancel_ButtonCustomizerCaption .
            THIS-OBJECT:Tools["TableIOCancel":U]:SharedProps:ToolTipText = SmartComponentLibraryCustomizer:SmartToolbarController_TableIOToolTableIOCancel_ButtonToolTipText .
        END.
        IF THIS-OBJECT:Tools:Exists ("TableIODelete":U) THEN DO:
            THIS-OBJECT:Tools["TableIODelete":U]:SharedProps:Caption = SmartComponentLibraryCustomizer:SmartToolbarController_TableIOToolTableIODelete_ButtonCaption .
            THIS-OBJECT:Tools["TableIODelete":U]:SharedProps:CustomizerCaption = SmartComponentLibraryCustomizer:SmartToolbarController_TableIOToolTableIODelete_ButtonCustomizerCaption .
            THIS-OBJECT:Tools["TableIODelete":U]:SharedProps:ToolTipText = SmartComponentLibraryCustomizer:SmartToolbarController_TableIOToolTableIODelete_ButtonToolTipText .
        END.

        IF THIS-OBJECT:Toolbars:Exists ("TransactionToolbar":U) THEN
            THIS-OBJECT:Toolbars:Item["TransactionToolbar":U]:Text = SmartComponentLibraryCustomizer:SmartToolbarController_TransactionToolBar_Text.

        IF THIS-OBJECT:Tools:Exists ("TransactionCommit":U) THEN DO:
            THIS-OBJECT:Tools["TransactionCommit":U]:SharedProps:Caption = SmartComponentLibraryCustomizer:SmartToolbarController_TransactionToolTransactionCommit_ButtonCaption .
            THIS-OBJECT:Tools["TransactionCommit":U]:SharedProps:CustomizerCaption = SmartComponentLibraryCustomizer:SmartToolbarController_TransactionToolTransactionCommit_ButtonCustomizerCaption .
            THIS-OBJECT:Tools["TransactionCommit":U]:SharedProps:ToolTipText = SmartComponentLibraryCustomizer:SmartToolbarController_TransactionToolTransactionCommit_ButtonToolTipText .
        END.
        IF THIS-OBJECT:Tools:Exists ("TransactionUndo":U) THEN DO:
            THIS-OBJECT:Tools["TransactionUndo":U]:SharedProps:Caption = SmartComponentLibraryCustomizer:SmartToolbarController_TransactionToolTransactionUndo_ButtonCaption .
            THIS-OBJECT:Tools["TransactionUndo":U]:SharedProps:CustomizerCaption = SmartComponentLibraryCustomizer:SmartToolbarController_TransactionToolTransactionUndo_ButtonCustomizerCaption .
            THIS-OBJECT:Tools["TransactionUndo":U]:SharedProps:ToolTipText = SmartComponentLibraryCustomizer:SmartToolbarController_TransactionToolTransactionUndo_ButtonToolTipText .
        END.

        /* Marko Rüterbories, Consultingwerk Ltd. 13.01.2014
           Added functionality to load customized captions for the default RibbonGroups */
        {Consultingwerk/foreach.i Infragistics.Win.UltraWinToolbars.RibbonTab oRibbonTab in THIS-OBJECT:Ribbon:Tabs}
            IF oRibbonTab:Groups:Exists ("Navigation":U) THEN
                oRibbonTab:Groups["Navigation":U]:Caption = SmartComponentLibraryCustomizer:SmartToolbarController_Navigation_GroupCaption.
            IF oRibbonTab:Groups:Exists ("Maintenance":U) THEN
                oRibbonTab:Groups["Maintenance":U]:Caption = SmartComponentLibraryCustomizer:SmartToolbarController_Maintenance_GroupCaption.
            IF oRibbonTab:Groups:Exists ("Transaction":U) THEN
                oRibbonTab:Groups["Transaction":U]:Caption = SmartComponentLibraryCustomizer:SmartToolbarController_Transaction_GroupCaption.
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
        Purpose: Loads a default Ribbon Design
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID LoadDefaultRibbonConfiguration ():

        IF NOT THIS-OBJECT:DesignTime THEN
            RETURN .

        FILE-INFORMATION:FILE-NAME = "Consultingwerk/SmartComponents/Resources/RibbonTemplate.xml":U .

        IF FILE-INFORMATION:FULL-PATHNAME > "":U THEN
            THIS-OBJECT:LoadFromXml(FILE-INFORMATION:FULL-PATHNAME) .
        ELSE
            Consultingwerk.Util.MessageFormHelper:ShowMessage
                ('The file "Consultingwerk/SmartComponents/Resources/RibbonTemplate.xml" cannot be found.',
                 "SmartToolbarController",
                 "Make sure it is available on your propath!",
                 MessageFormImages:ImageWarning) .

    END METHOD.

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
        Purpose: Raises the ActiveSmartCommitTargetChanged event
        Notes:
        @param e The System.EventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnActiveSmartCommitTargetChanged (e AS System.EventArgs):

        IF NOT VALID-OBJECT (e) THEN
            e = System.EventArgs:Empty .

        THIS-OBJECT:ActiveSmartCommitTargetChanged:Publish (THIS-OBJECT, e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event Handler method for Designer Verbs
        Notes:   This method is intended to be overridden
        @param pcVerbText The text indicating the verb that was clicked
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID OnVerbClicked (pcVerbText AS CHARACTER):

        CASE pcVerbText:
            WHEN "Add TableIO Tools":U THEN
                CreateSmartTableIOTools () .
            WHEN "Add Transaction Tools":U THEN
                CreateSmartTransactionTools () .
            WHEN "Add Navigation Tools":U THEN
                CreateSmartNavigationTools () .
            WHEN "Default Ribbon Configuration":U THEN
                LoadDefaultRibbonConfiguration () .
        END CASE .

    END METHOD .

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

        THIS-OBJECT:OnActiveSmartCommitTargetChanged (System.EventArgs:Empty) .

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
                 This Method is executed by CreateSmartNavigationTools and
                 CreateSmartTableIOTools when adding Navigation and TableIO Toolbars
                 to the SmartToolbarController.
        Notes:   Used to create the default Navigation and TableIO tools
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
        RETURN THIS-OBJECT:CreateButtonTool (pcButtonKey,
                                             pcButtonCaption,
                                             pcButtonCustomizerCaption,
                                             pcButtonImage,
                                             ?) .

    END METHOD .

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
        @param pcLargeButtonImage The large Image of the Button
        @return The reference to the new ButtonTool
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC Infragistics.Win.UltraWinToolbars.ButtonTool CreateButtonTool (pcButtonKey AS CHARACTER,
                                                                                 pcButtonCaption AS CHARACTER,
                                                                                 pcButtonCustomizerCaption AS CHARACTER,
                                                                                 pcButtonImage AS CHARACTER,
                                                                                 pcLargeButtonImage AS CHARACTER):

        DEFINE VARIABLE oButton     AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        DEFINE VARIABLE oAppearance AS Infragistics.Win.Appearance                  NO-UNDO.
        DEFINE VARIABLE oImage      AS System.Drawing.Bitmap                        NO-UNDO.

        oButton = NEW Infragistics.Win.UltraWinToolbars.ButtonTool (pcButtonKey).

        oButton:SharedProps:Caption = pcButtonCaption .
        oButton:SharedProps:CustomizerCaption = pcButtonCustomizerCaption .

        FILE-INFORMATION:FILE-NAME = pcButtonImage .
        IF FILE-INFORMATION:FULL-PATHNAME <> ? THEN DO:
            oImage = Consultingwerk.Util.ImageHelper:Load (FILE-INFORMATION:FULL-PATHNAME) .
            oImage:MakeTransparent() .

            oAppearance = NEW Infragistics.Win.Appearance().
            oAppearance:Image = oImage .

            oButton:SharedProps:AppearancesSmall:Appearance = oAppearance .
        END.

        FILE-INFORMATION:FILE-NAME = pcLargeButtonImage .
        IF FILE-INFORMATION:FULL-PATHNAME <> ? THEN DO:
            oImage = Consultingwerk.Util.ImageHelper:Load (FILE-INFORMATION:FULL-PATHNAME) .
            oImage:MakeTransparent() .

            oAppearance = NEW Infragistics.Win.Appearance().
            oAppearance:Image = oImage .

            oButton:SharedProps:AppearancesLarge:Appearance = oAppearance .
        END.

        RETURN oButton.

    END METHOD.

     /*------------------------------------------------------------------------------
        Purpose: Creates the default tools for the SmarNavigationSource.
                 Invoked by the Property setter for AddNavigationTools.
        Notes:   All ButtonTools for Navigation purposes are created and added to
                 a new instance of Infragistics.Win.UltraWinToolbars.UltraToolbar.
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID CreateSmartNavigationTools ():

        DEFINE VARIABLE toolarray AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 4 NO-UNDO.

        DEFINE VARIABLE buttonTool1 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        DEFINE VARIABLE buttonTool2 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        DEFINE VARIABLE buttonTool3 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        DEFINE VARIABLE buttonTool4 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.

        DEFINE VARIABLE ultraToolbar1 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.

        /* The tool navigation definitions */
        IF NOT THIS-OBJECT:Tools:Exists("NavigationFirst":U) THEN
            THIS-OBJECT:Tools:Add (CreateButtonTool("NavigationFirst":U,
                                                    SmartComponentLibraryCustomizer:SmartToolbarController_NavigationToolNavigationFirst_ButtonCaption,
                                                    SmartComponentLibraryCustomizer:SmartToolbarController_NavigationToolNavigationFirst_ButtonCustomizerCaption,
                                                    "Consultingwerk/SmartComponents/Images/first.gif":U)) .

        IF NOT THIS-OBJECT:Tools:Exists("NavigationPrev":U) THEN
            THIS-OBJECT:Tools:Add (CreateButtonTool("NavigationPrev":U,
                                                    SmartComponentLibraryCustomizer:SmartToolbarController_NavigationToolNavigationPrev_ButtonCaption,
                                                    SmartComponentLibraryCustomizer:SmartToolbarController_NavigationToolNavigationPrev_ButtonCustomizerCaption,
                                                    "Consultingwerk/SmartComponents/Images/prev.gif":U)) .

        IF NOT THIS-OBJECT:Tools:Exists("NavigationNext":U) THEN
            THIS-OBJECT:Tools:Add (CreateButtonTool("NavigationNext":U,
                                                    SmartComponentLibraryCustomizer:SmartToolbarController_NavigationToolNavigationNext_ButtonCaption,
                                                    SmartComponentLibraryCustomizer:SmartToolbarController_NavigationToolNavigationNext_ButtonCustomizerCaption,
                                                    "Consultingwerk/SmartComponents/Images/next.gif":U)) .

        IF NOT THIS-OBJECT:Tools:Exists("NavigationLast":U) THEN
            THIS-OBJECT:Tools:Add (CreateButtonTool("NavigationLast":U,
                                                    SmartComponentLibraryCustomizer:SmartToolbarController_NavigationToolNavigationLast_ButtonCaption,
                                                    SmartComponentLibraryCustomizer:SmartToolbarController_NavigationToolNavigationLast_ButtonCustomizerCaption,
                                                    "Consultingwerk/SmartComponents/Images/last.gif":U)) .

        IF NOT THIS-OBJECT:Toolbars:Exists("NavigationToolbar":U) THEN DO:
            /* Create the navigation toolbar */
            ultraToolbar1 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("NavigationToolbar":U).
            ultraToolbar1:Text = SmartComponentLibraryCustomizer:SmartToolbarController_NavigationToolBar_Text.
            ultraToolbar1:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Top.


            /* Create the tool instances on the navigation toolbar */
            buttonTool1 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationFirst":U).
            buttonTool2 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationPrev":U).
            buttonTool3 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationNext":U).
            buttonTool4 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("NavigationLast":U).

            buttonTool1:InstanceProps:IsFirstInGroup = TRUE.

            toolarray[1] = buttonTool1.
            toolarray[2] = buttonTool2.
            toolarray[3] = buttonTool3.
            toolarray[4] = buttonTool4.

            ultraToolbar1:Tools:AddRange(toolarray) .

            THIS-OBJECT:Toolbars:Add(ultraToolbar1) .
        END.

    END METHOD.

     /*------------------------------------------------------------------------------
        Purpose: Creates the default tools for the SmartTableIOSource.
                 Invoked by the Property setter for AddTableIOTools.
        Notes:   All ButtonTools for TableIO purposes are created and added to
                 a new instance of Infragistics.Win.UltraWinToolbars.UltraToolbar.
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID CreateSmartTableIOTools ():

        DEFINE VARIABLE toolarray AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 6 NO-UNDO.

        DEFINE VARIABLE buttonTool1 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        DEFINE VARIABLE buttonTool2 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        DEFINE VARIABLE buttonTool3 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        DEFINE VARIABLE buttonTool4 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        DEFINE VARIABLE buttonTool5 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        DEFINE VARIABLE buttonTool6 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.

        DEFINE VARIABLE ultraToolbar1 AS Infragistics.Win.UltraWinToolbars.UltraToolbar NO-UNDO.

        /* The tool tableIO definitions */
        IF NOT THIS-OBJECT:Tools:Exists("TableIOAdd":U) THEN
            THIS-OBJECT:Tools:Add (CreateButtonTool("TableIOAdd":U,
                                                    SmartComponentLibraryCustomizer:SmartToolbarController_TableIOToolTableIOAdd_ButtonCaption,
                                                    SmartComponentLibraryCustomizer:SmartToolbarController_TableIOToolTableIOAdd_ButtonCustomizerCaption,
                                                    "Consultingwerk/SmartComponents/Images/add.gif":U)) .

        IF NOT THIS-OBJECT:Tools:Exists("TableIOCopy":U) THEN
            THIS-OBJECT:Tools:Add (CreateButtonTool("TableIOCopy":U,
                                                    SmartComponentLibraryCustomizer:SmartToolbarController_TableIOToolTableIOCopy_ButtonCaption,
                                                    SmartComponentLibraryCustomizer:SmartToolbarController_TableIOToolTableIOCopy_ButtonCustomizerCaption,
                                                    "Consultingwerk/SmartComponents/Images/copyrec.gif":U)) .

        IF NOT THIS-OBJECT:Tools:Exists("TableIOUpdate":U) THEN
            THIS-OBJECT:Tools:Add (CreateButtonTool("TableIOUpdate":U,
                                                    SmartComponentLibraryCustomizer:SmartToolbarController_TableIOToolTableIOUpdate_ButtonCaption,
                                                    SmartComponentLibraryCustomizer:SmartToolbarController_TableIOToolTableIOUpdate_ButtonCustomizerCaption,
                                                    "Consultingwerk/SmartComponents/Images/update.gif":U)) .

        IF NOT THIS-OBJECT:Tools:Exists("TableIOSave":U) THEN
            THIS-OBJECT:Tools:Add (CreateButtonTool("TableIOSave":U,
                                                    SmartComponentLibraryCustomizer:SmartToolbarController_TableIOToolTableIOSave_ButtonCaption,
                                                    SmartComponentLibraryCustomizer:SmartToolbarController_TableIOToolTableIOSave_ButtonCustomizerCaption,
                                                    "Consultingwerk/SmartComponents/Images/saverec.gif":U)) .

        IF NOT THIS-OBJECT:Tools:Exists("TableIOCancel":U) THEN
            THIS-OBJECT:Tools:Add (CreateButtonTool("TableIOCancel":U,
                                                    SmartComponentLibraryCustomizer:SmartToolbarController_TableIOToolTableIOCancel_ButtonCaption,
                                                    SmartComponentLibraryCustomizer:SmartToolbarController_TableIOToolTableIOCancel_ButtonCustomizerCaption,
                                                    "Consultingwerk/SmartComponents/Images/cancel.gif":U)) .

        IF NOT THIS-OBJECT:Tools:Exists("TableIODelete":U) THEN
            THIS-OBJECT:Tools:Add (CreateButtonTool("TableIODelete":U,
                                                    SmartComponentLibraryCustomizer:SmartToolbarController_TableIOToolTableIODelete_ButtonCaption,
                                                    SmartComponentLibraryCustomizer:SmartToolbarController_TableIOToolTableIODelete_ButtonCustomizerCaption,
                                                    "Consultingwerk/SmartComponents/Images/deleterec.gif":U)) .

        IF NOT THIS-OBJECT:Toolbars:Exists("TableIOToolbar":U) THEN DO:
            /* Create the navigation toolbar */
            ultraToolbar1 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbar("TableIOToolbar":U).
            ultraToolbar1:Text = SmartComponentLibraryCustomizer:SmartToolbarController_TableIOToolBar_Text.
            ultraToolbar1:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Top.


            /* Create the tool instances on the navigation toolbar */
            buttonTool1 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOAdd":U).
            buttonTool2 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCopy":U).
            buttonTool3 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOUpdate":U).
            buttonTool4 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOSave":U).
            buttonTool5 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIOCancel":U).
            buttonTool6 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("TableIODelete":U).

            buttonTool1:InstanceProps:IsFirstInGroup = TRUE.

            toolarray[1] = buttonTool1.
            toolarray[2] = buttonTool2.
            toolarray[3] = buttonTool3.
            toolarray[4] = buttonTool4.
            toolarray[5] = buttonTool5.
            toolarray[6] = buttonTool6.

            ultraToolbar1:Tools:AddRange(toolarray) .

            THIS-OBJECT:Toolbars:Add(ultraToolbar1) .
        END.

        RETURN.

    END METHOD.

     /*------------------------------------------------------------------------------
        Purpose: Disables the tool with the given Key when it exists in this instance.
                 Invoked by DisableTools.
        Notes:
        @param  pcToolKey Key of the Tool ButtonTool to be disabled
        @return True if the Tool exists, False if the Tool does not Exist
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL DisableControlTool (pcToolKey AS CHARACTER):

        IF THIS-OBJECT:Tools:Exists(pcToolKey) THEN DO:
            THIS-OBJECT:Tools[pcToolKey]:SharedProps:Enabled = FALSE .

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

        IF THIS-OBJECT:Tools:Exists(pcToolKey) THEN DO:
            THIS-OBJECT:Tools[pcToolKey]:SharedProps:Enabled = TRUE .

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

        RETURN THIS-OBJECT:Tools:Exists (pcToolKey) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Invoked during the System.ComponentModel.ISupportInitialize.EndInit
                 of the component (base class).
        Notes:   Set Navigation and TableIO tools disabled after initialization.
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID OnEndInit ():

        SUPER:OnEndInit().

        IF NOT THIS-OBJECT:DesignTime THEN DO:

            /* Mike Fechner, Consultingwerk Ltd. 11.05.2014
               SCL-41: Perform initialization only once in case
               Visual Inheritance is used in the containing Form.
               Both the Base Form and the Child Form may invoke
               the UltraToolbarsManager's EndInit method */
            IF NOT lInitialized THEN DO ON ERROR UNDO, THROW:

                THIS-OBJECT:BeginUpdate() .

                InitializeCaptionOfTools ().

                DisableTools ("NavigationFirst,NavigationPrev,NavigationNext,NavigationLast":U) .

                IF VALID-OBJECT (THIS-OBJECT:ActiveSmartTableIOTarget) AND
                    THIS-OBJECT:ActiveSmartTableIOTarget:SmartTableIOState > "":U THEN

                    THIS-OBJECT:UpdateSmartTableIOState (THIS-OBJECT:ActiveSmartTableIOTarget,
                                                         THIS-OBJECT:ActiveSmartTableIOTarget:SmartTableIOState) .
                ELSE
                    DisableTools ("TableIOAdd,TableIOCopy,TableIOUpdate,TableIOSave,TableIOCancel,TableIODelete":U) .

                DisableTools ("TransactionCommit,TransactionUndo":U) .

                /* Mike Fechner, Consultingwerk Ltd. 23.04.2014
                   Perform security check for non standard tools */
                THIS-OBJECT:PerformToolbarSecurity () .

                FINALLY:
                    THIS-OBJECT:EndUpdate () .

                    ASSIGN lInitialized = TRUE .
                END FINALLY.
            END.

        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Registers a new SmartCommitTarget
        Notes:
        @param poTarget The ISmartCommitTarget to register
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RegisterSmartCommitTarget (poTarget AS ISmartCommitTarget):

        oController:RegisterSmartCommitTarget (poTarget) .

    END METHOD.

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
        Purpose: Sets Properties to customize Visual Designer behaviour
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID SetDesignerProperties ():

        THIS-OBJECT:DesignerVerbs = "Add TableIO Tools,Add Navigation Tools,Add Transaction Tools,Default Ribbon Configuration":U .

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
        Purpose: Event handler for the SmartTransactionStateChanged event of the SmartCommitTarget
        Notes:
        @param sender The object the raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID SmartTransactionStateChangedHandler (sender AS System.Object,
                                                               e AS System.EventArgs):

        oController:SmartTransactionStateChangedHandler (sender,
                                                         e) .

    END METHOD.

     /*------------------------------------------------------------------------------
        Purpose: Raises the ToolClick event
        Notes:   Invokes HandleToolClick with the key of the tool that was clicked
        @param e The Infragistics.Win.UltraWinToolbars.ToolClickEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnToolClick (e AS ToolClickEventArgs):

        IF e:Tool:Key = "Exit":U THEN DO:
            IF VALID-OBJECT (THIS-OBJECT:DockWithinContainer) AND
               TYPE-OF (THIS-OBJECT:DockWithinContainer, System.Windows.Forms.Form) THEN

                CAST (THIS-OBJECT:DockWithinContainer, System.Windows.Forms.Form):Close () .
                RETURN .
        END.

        SUPER:OnToolClick (e) .

        oController:HandleToolClick (e:Tool:Key) .

        /* Mike Fechner, Consultingwerk Ltd. 27.12.2009
           This is considered a UI event handler, so there should be a CATCH block */
        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD .

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
        Purpose: Destructor of the SmartToolbarController class
        Notes:   Deletes the associated ttSmartNavigationTarget and
                 ttSmartTableIOTarget records
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartToolbarController ():

        IF NOT THIS-OBJECT:DesignTime THEN DO:

            DELETE OBJECT oController .

            IF NOT THIS-OBJECT:DesignTime THEN
                Consultingwerk.Util.UltraToolbarsHelper:EraseToolbarsCollections (THIS-OBJECT) .

            IF VALID-OBJECT (oDesignerVerbHelper) THEN
                DELETE OBJECT oDesignerVerbHelper .
        END.

    END DESTRUCTOR.

END CLASS.
