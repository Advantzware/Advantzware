/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : SmartRadRibbonBarController
    Purpose     : Enhances RabRibbonBar (or RadRibbonForm) with runtime
                  and design time functionality of the SmartComponent Library
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Apr 26 17:54:41 CEST 2015
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Assertion.*                      FROM PROPATH .
USING Consultingwerk.Framework.Collections.*          FROM PROPATH .
USING Consultingwerk.SmartComponents.*                FROM ASSEMBLY .
USING Consultingwerk.SmartComponents.Base.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.SmartComponents.Interfaces.*     FROM PROPATH .
USING Consultingwerk.SmartComponents.Resources.*      FROM PROPATH .
USING Consultingwerk.SmartComponents.Support.*        FROM PROPATH .
USING Consultingwerk.Util.*                           FROM PROPATH .
USING Progress.Lang.*                                 FROM PROPATH .
USING System.ComponentModel.*                         FROM ASSEMBLY .
USING System.ComponentModel.Design.*                  FROM ASSEMBLY .
USING System.Windows.Forms.*                          FROM ASSEMBLY .
USING Telerik.WinControls.*                           FROM ASSEMBLY .
USING Telerik.WinControls.UI.*                        FROM ASSEMBLY .

CLASS Consultingwerk.SmartComponents.Implementation.SmartRadRibbonBarController
    INHERITS SmartComponent
    IMPLEMENTS ISmartNavigationSource, ISmartTableIOSource, ISmartCommitSource,
               Consultingwerk.SmartComponents.Interfaces.Design.IDesignNavigationSource ,
               Consultingwerk.SmartComponents.Interfaces.Design.IDesignTableIOSource,
               Consultingwerk.SmartComponents.Interfaces.Design.IDesignCommitSource,
               ISmartToolbarControl, ISupportInitialize:

    DEFINE VARIABLE lInitialized AS LOGICAL NO-UNDO INIT FALSE . /* SCL-41 */

    DEFINE VARIABLE oController  AS SmartToolbarStateController NO-UNDO .
    DEFINE VARIABLE oSmartItems  AS "System.Collections.Generic.List<RadItem>" NO-UNDO .

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
        Purpose: Returns the List of TableIOTargetControl instances
        Notes:   See http://jira.consultingwerkcloud.com/browse/SCL-442 for details
    ------------------------------------------------------------------------------*/
    DEFINE PROTECTED PROPERTY ListTableIOTargetControl AS ListTableIOTargetControl NO-UNDO
    GET:
        RETURN oController:ListTableIOTargetControl .
    END GET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the List of SmartTableIOTargets as a generic List
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmartTableIOTargets AS ListISmartTableIOTarget NO-UNDO
    GET:
        RETURN oController:SmartTableIOTargets .
    END GET.


    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the reference to the RadRibbonBar that is controlled
                 by this instance of the SmartRadRibbonBarController
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY RadRibbonBar AS Telerik.WinControls.UI.RadRibbonBar NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the SmartRadRibbonBarController class
        Notes:   Set's the DesignTime property using the LicenseManager. According to
                 http://dotnetfacts.blogspot.com/2009/01/identifying-run-time-and-design-mode.html
                 that is only possible during the constructor of the class.
        @param poContainer The IContainer to add the SmartBusinessEntityAdapter to.
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartRadRibbonBarController (poContainer AS System.ComponentModel.IContainer):
        SUPER (poContainer) .

        ASSIGN oController = NEW SmartToolbarStateController (THIS-OBJECT) .

        /* Mike Fechner, Consultingwerk Ltd. 24.01.2010
           Only subscribe to Disposed event when not in Visual Designer
           to avoid conflicts when re-opening layouts. */
        IF NOT THIS-OBJECT:DesignTime THEN
            THIS-OBJECT:Disposed:Subscribe (DisposedHandler) .
        ELSE
            THIS-OBJECT:Disposed:Subscribe (DesignTimeDisposedHandler) .

        THIS-OBJECT:SetDesignerProperties () .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the SmartRadRibbonBarController class. Set's the
                 DesignTime property using the LicenseManager.
        Notes:   According to
                 http://dotnetfacts.blogspot.com/2009/01/identifying-run-time-and-design-mode.html
                 that is only possible during the constructor of the class.
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartRadRibbonBarController ():
        SUPER ().

        ASSIGN oController = NEW SmartToolbarStateController (THIS-OBJECT) .

        /* Mike Fechner, Consultingwerk Ltd. 24.01.2010
           Only subscribe to Disposed event when not in Visual Designer
           to avoid conflicts when re-opening layouts. */
        IF NOT THIS-OBJECT:DesignTime THEN
            THIS-OBJECT:Disposed:Subscribe (DisposedHandler) .
        ELSE
            THIS-OBJECT:Disposed:Subscribe (DesignTimeDisposedHandler) .

        THIS-OBJECT:SetDesignerProperties () .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Adds the Default Ribbon Elements
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID AddDefaultRibbonElements ():

        IF NOT THIS-OBJECT:DesignTime THEN
            UNDO, THROW NEW AppError ("The method AddDefaultRibbonElements is only supported at design time.":U, 0) .

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i THIS-OBJECT:RadRibbonBar """RadRibbonBar"":U"} .

        THIS-OBJECT:CreateNavigationGroup () .
        THIS-OBJECT:CreateTableIOGroup () .
        THIS-OBJECT:CreateTransactionGroup () .

        DesignerHelper:RaiseComponentChanged (THIS-OBJECT, "":U, ?, ?) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Begins a series of updates to the Control
        Notes:   Not supported by the RadRibbonBar
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID BeginControlUpdate ():

/*        THIS-OBJECT:BeginUpdate () .*/

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Begins component initialization
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID BeginInit ():

        /* noop */

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Creates the Default Ribbon Tab
        Notes:
        @return The new RibbonTab
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC RibbonTab CreateDefaultRibbonTab ():

        DEFINE VARIABLE oRibbonTab   AS RibbonTab         NO-UNDO .

        oRibbonTab = CAST (THIS-OBJECT:CreateDesignTimeObjectInstance (Progress.Util.TypeHelper:GetType ("Telerik.WinControls.UI.RibbonTab":U)),
                           RibbonTab) .

        oRibbonTab:Name = "Start":U .
        oRibbonTab:Tag = "Start":U .
        oRibbonTab:Text = "Start"{&TRAN} .

        THIS-OBJECT:RadRibbonBar:CommandTabs:Insert (0, oRibbonTab) .

        RETURN oRibbonTab .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Creates a design time instance of the given System.Type
        Notes:
        @param poType The reference to the type to create an instance of
        @return The reference to the new object instance
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED System.Object CreateDesignTimeObjectInstance (poType AS System.Type):

        IF NOT THIS-OBJECT:DesignTime THEN
            UNDO, THROW NEW AppError ("The method CreateDesignTimeObjectInstance is only supported at design time.":U, 0) .

        RETURN CAST (THIS-OBJECT:Container, IDesignerHost):CreateComponent (poType) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Creates the Default Navigation Ribbon Group
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID CreateNavigationGroup ():

        DEFINE VARIABLE oRibbonTab   AS RibbonTab         NO-UNDO .
        DEFINE VARIABLE oRibbonGroup AS RadRibbonBarGroup NO-UNDO .

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i THIS-OBJECT:RadRibbonBar """RadRibbonBar"":U"} .

        IF NOT ControlHelper:ExistsByTag (THIS-OBJECT:RadRibbonBar:CommandTabs, "Start":U) THEN
            oRibbonTab = THIS-OBJECT:CreateDefaultRibbonTab () .
        ELSE
            oRibbonTab = CAST (ControlHelper:FindByTag (THIS-OBJECT:RadRibbonBar:CommandTabs, "Start":U), RibbonTab) .

        /* Do we already have a "Navigation" Ribbon group */
        IF ControlHelper:ExistsByTag (oRibbonTab:Items, "Navigation":U) THEN
            oRibbonGroup = CAST (ControlHelper:FindByTag (oRibbonTab:Items, "Navigation":U), RadRibbonBarGroup) .
        ELSE DO:
            oRibbonGroup = CAST (THIS-OBJECT:CreateDesignTimeObjectInstance (Progress.Util.TypeHelper:GetType ("Telerik.WinControls.UI.RadRibbonBarGroup":U)),
                                 RadRibbonBarGroup) .

            oRibbonTab:Items:Add (oRibbonGroup) .
            oRibbonGroup:Text = SmartComponentLibraryCustomizer:SmartToolbarController_Navigation_GroupCaption .
            oRibbonGroup:Name = "Navigation":U .
            oRibbonGroup:Tag = "Navigation":U .
        END.

        THIS-OBJECT:CreateRibbonButton (oRibbonGroup,
                                        "NavigationFirst":U,
                                        SmartComponentLibraryCustomizer:SmartToolbarController_NavigationToolNavigationFirst_ButtonCaption,
                                        "Consultingwerk/SmartComponents/Images/first_32.png":U,
                                        "Consultingwerk/SmartComponents/Images/first.png":U) .

        THIS-OBJECT:CreateRibbonButton (oRibbonGroup,
                                        "NavigationPrev":U,
                                        SmartComponentLibraryCustomizer:SmartToolbarController_NavigationToolNavigationPrev_ButtonCaption,
                                        "Consultingwerk/SmartComponents/Images/prev_32.png":U,
                                        "Consultingwerk/SmartComponents/Images/prev.png":U) .

        THIS-OBJECT:CreateRibbonButton (oRibbonGroup,
                                        "NavigationNext":U,
                                        SmartComponentLibraryCustomizer:SmartToolbarController_NavigationToolNavigationNext_ButtonCaption,
                                        "Consultingwerk/SmartComponents/Images/next_32.png":U,
                                        "Consultingwerk/SmartComponents/Images/next.png":U) .

        THIS-OBJECT:CreateRibbonButton (oRibbonGroup,
                                        "NavigationLast":U,
                                        SmartComponentLibraryCustomizer:SmartToolbarController_NavigationToolNavigationLast_ButtonCaption,
                                        "Consultingwerk/SmartComponents/Images/last_32.png":U,
                                        "Consultingwerk/SmartComponents/Images/last.png":U) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Creates the Default TableIO Ribbon Group
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID CreateTableIOGroup ():

        DEFINE VARIABLE oRibbonTab   AS RibbonTab         NO-UNDO .
        DEFINE VARIABLE oRibbonGroup AS RadRibbonBarGroup NO-UNDO .

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i THIS-OBJECT:RadRibbonBar """RadRibbonBar"":U"} .

        IF NOT ControlHelper:ExistsByTag (THIS-OBJECT:RadRibbonBar:CommandTabs, "Start":U) THEN
            oRibbonTab = THIS-OBJECT:CreateDefaultRibbonTab () .
        ELSE
            oRibbonTab = CAST (ControlHelper:FindByTag (THIS-OBJECT:RadRibbonBar:CommandTabs, "Start":U), RibbonTab) .

        /* Do we already have a "Navigation" Ribbon group */
        IF ControlHelper:ExistsByTag (oRibbonTab:Items, "TableIO":U) THEN
            oRibbonGroup = CAST (ControlHelper:FindByTag (oRibbonTab:Items, "TableIO":U), RadRibbonBarGroup) .
        ELSE DO:
            oRibbonGroup = CAST (THIS-OBJECT:CreateDesignTimeObjectInstance (Progress.Util.TypeHelper:GetType ("Telerik.WinControls.UI.RadRibbonBarGroup":U)),
                                 RadRibbonBarGroup) .

            oRibbonTab:Items:Add (oRibbonGroup) .
            oRibbonGroup:Text = SmartComponentLibraryCustomizer:SmartToolbarController_Maintenance_GroupCaption .
            oRibbonGroup:Name = "TableIO":U .
            oRibbonGroup:Tag = "TableIO":U .
        END.

        THIS-OBJECT:CreateRibbonButton (oRibbonGroup,
                                        "TableIOAdd":U,
                                        SmartComponentLibraryCustomizer:SmartToolbarController_TableIOToolTableIOAdd_ButtonCaption,
                                        "Consultingwerk/SmartComponents/Images/add_32.png":U,
                                        "Consultingwerk/SmartComponents/Images/add.png":U) .

        THIS-OBJECT:CreateRibbonButton (oRibbonGroup,
                                        "TableIOCopy":U,
                                        SmartComponentLibraryCustomizer:SmartToolbarController_TableIOToolTableIOCopy_ButtonCaption,
                                        "Consultingwerk/SmartComponents/Images/copyrec_32.png":U,
                                        "Consultingwerk/SmartComponents/Images/copyrec.png":U) .

        THIS-OBJECT:CreateRibbonButton (oRibbonGroup,
                                        "TableIOUpdate":U,
                                        SmartComponentLibraryCustomizer:SmartToolbarController_TableIOToolTableIOUpdate_ButtonCaption,
                                        "Consultingwerk/SmartComponents/Images/update_32.png":U,
                                        "Consultingwerk/SmartComponents/Images/update.png":U) .

        THIS-OBJECT:CreateRibbonButton (oRibbonGroup,
                                        "TableIOSave":U,
                                        SmartComponentLibraryCustomizer:SmartToolbarController_TableIOToolTableIOSave_ButtonCaption,
                                        "Consultingwerk/SmartComponents/Images/saverec_32.png":U,
                                        "Consultingwerk/SmartComponents/Images/saverec.png":U) .

        THIS-OBJECT:CreateRibbonButton (oRibbonGroup,
                                        "TableIOCancel":U,
                                        SmartComponentLibraryCustomizer:SmartToolbarController_TableIOToolTableIOCancel_ButtonCaption,
                                        "Consultingwerk/SmartComponents/Images/cancel_32.png":U,
                                        "Consultingwerk/SmartComponents/Images/cancel.png":U) .

        THIS-OBJECT:CreateRibbonButton (oRibbonGroup,
                                        "TableIODelete":U,
                                        SmartComponentLibraryCustomizer:SmartToolbarController_TableIOToolTableIODelete_ButtonCaption,
                                        "Consultingwerk/SmartComponents/Images/deleterec_32.png":U,
                                        "Consultingwerk/SmartComponents/Images/deleterec.png":U) .
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Creates the Default Transaction Ribbon Group
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID CreateTransactionGroup ():

        DEFINE VARIABLE oRibbonTab   AS RibbonTab         NO-UNDO .
        DEFINE VARIABLE oRibbonGroup AS RadRibbonBarGroup NO-UNDO .

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i THIS-OBJECT:RadRibbonBar """RadRibbonBar"":U"} .

        IF NOT ControlHelper:ExistsByTag (THIS-OBJECT:RadRibbonBar:CommandTabs, "Start":U) THEN
            oRibbonTab = THIS-OBJECT:CreateDefaultRibbonTab () .
        ELSE
            oRibbonTab = CAST (ControlHelper:FindByTag (THIS-OBJECT:RadRibbonBar:CommandTabs, "Start":U), RibbonTab) .

        /* Do we already have a "Navigation" Ribbon group */
        IF ControlHelper:ExistsByTag (oRibbonTab:Items, "Transaction":U) THEN
            oRibbonGroup = CAST (ControlHelper:FindByTag (oRibbonTab:Items, "Transaction":U), RadRibbonBarGroup) .
        ELSE DO:
            oRibbonGroup = CAST (THIS-OBJECT:CreateDesignTimeObjectInstance (Progress.Util.TypeHelper:GetType ("Telerik.WinControls.UI.RadRibbonBarGroup":U)),
                                 RadRibbonBarGroup) .

            oRibbonTab:Items:Add (oRibbonGroup) .
            oRibbonGroup:Text = SmartComponentLibraryCustomizer:SmartToolbarController_Transaction_GroupCaption .
            oRibbonGroup:Name = "Transaction":U .
            oRibbonGroup:Tag = "Transaction":U .
        END.

        THIS-OBJECT:CreateRibbonButton (oRibbonGroup,
                                        "TransactionCommit":U,
                                        SmartComponentLibraryCustomizer:SmartToolbarController_TransactionToolTransactionCommit_ButtonCaption,
                                        "Consultingwerk/SmartComponents/Images/table_ok_32.png":U,
                                        "Consultingwerk/SmartComponents/Images/table_ok_16.png":U) .

        THIS-OBJECT:CreateRibbonButton (oRibbonGroup,
                                        "TransactionUndo":U,
                                        SmartComponentLibraryCustomizer:SmartToolbarController_TransactionToolTransactionUndo_ButtonCaption,
                                        "Consultingwerk/SmartComponents/Images/table_error_32.png":U,
                                        "Consultingwerk/SmartComponents/Images/table_error_16.png":U) .
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Creates a Ribbon Button at Design Tiem
        Notes:
        @param poRibbonGroup The RibbonGroup to add the button to
        @param pcName The name of the button to create
        @param pcCaption The caption to assign to the Button
        @param pcLargeImage The large image file name to load into the button
        @param pcSmallImage The small image file name to load into the button
        @return The reference to the Ribbon Button that was created
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED RadButtonElement CreateRibbonButton (poRibbonGroup AS RadRibbonBarGroup,
                                                          pcName AS CHARACTER,
                                                          pcCaption AS CHARACTER,
                                                          pcLargeImage AS CHARACTER,
                                                          pcSmallImage AS CHARACTER) .

        DEFINE VARIABLE oButton AS RadButtonElement      NO-UNDO .
        DEFINE VARIABLE oImage  AS System.Drawing.Bitmap NO-UNDO .

        IF ControlHelper:ExistsByTag (poRibbonGroup:Items, pcName) THEN
            oButton = CAST (ControlHelper:FindByTag (poRibbonGroup:Items, pcName), RadButtonElement) .
        ELSE
            oButton = CAST (THIS-OBJECT:CreateDesignTimeObjectInstance (Progress.Util.TypeHelper:GetType ("Telerik.WinControls.UI.RadButtonElement":U)),
                            RadButtonElement) .

        IF pcLargeImage > "":U THEN DO:
            oImage = ImageHelper:Load (pcLargeImage) .
            oButton:Image = oImage .
        END.

        IF pcSmallImage > "":U THEN DO:
            oImage = ImageHelper:Load (pcSmallImage) .
            oButton:SmallImage = oImage .
        END.

        oButton:ImageAlignment = System.Drawing.ContentAlignment:MiddleCenter.
        oButton:Name = pcName .
        oButton:Tag = pcName .
        oButton:Text = pcCaption .
        oButton:TextAlignment = System.Drawing.ContentAlignment:MiddleCenter.
        oButton:TextImageRelation = System.Windows.Forms.TextImageRelation:ImageAboveText.

        poRibbonGroup:Items:Add (oButton) .

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

        {Consultingwerk/foreach.i RadItem oItem in oSmartItems}
            oItem:Click:Unsubscribe (ItemClickHandler) .
        END.

        DELETE OBJECT THIS-OBJECT .

        FINALLY:
            Consultingwerk.Util.ErrorHelper:ErrorMessageDialogAllowed = TRUE .
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Ends a series of updates to the Control
        Notes:   Not supported by the RadRibbonBar
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID EndControlUpdate ():

/*        THIS-OBJECT:EndUpdate () .*/

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the type name of the Form that contains the Toolbar
        Notes:
        @return The type name of the Form that contains the Toolbar
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetFormTypeName ():

        DEFINE VARIABLE oControl                      AS System.Windows.Forms.Control  NO-UNDO .
        DEFINE VARIABLE oForm                         AS System.Windows.Forms.Form     NO-UNDO .

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i THIS-OBJECT:RadRibbonBar """RadRibbonBar"":U"} .

        oControl = THIS-OBJECT:RadRibbonBar:FindForm() .

        IF NOT VALID-OBJECT (oControl) THEN
            RETURN ? .

        IF TYPE-OF (oControl, System.Windows.Forms.Form) THEN
            oForm = CAST (oControl, System.Windows.Forms.Form) .
        ELSE
            oForm = oControl:FindForm () .

        IF NOT VALID-OBJECT (oForm) THEN
            RETURN ? .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns a CharacterList of all ToolKeys in the Toolbar
        Notes:
        @return The CharacterList of all ToolKeys in the Toolbar
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CharacterList GetAllToolKeys ():

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i THIS-OBJECT:RadRibbonBar """RadRibbonBar"":U"} .

        RETURN RadRibbonBarHelper:GetAllTagValues (THIS-OBJECT:RadRibbonBar) .

    END METHOD .

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
        Purpose: Event handler for the Click event of the Items
        Notes:
        @param sender The reference to the Item that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ItemClickHandler (sender AS System.Object,
                                          e AS System.EventArgs):

        DEFINE VARIABLE oItem AS RadItem   NO-UNDO .
        DEFINE VARIABLE cTag  AS CHARACTER NO-UNDO.

        ASSIGN oItem = CAST (sender, RadItem)
               cTag  = UNBOX (oItem:Tag).

        oController:HandleToolClick (cTag) .

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
        @param pcVerbText The label of the verb the developer clicked on in the Visual Designer
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID OnVerbClicked (pcVerbText AS CHARACTER):

        SUPER:OnVerbClicked (pcVerbText).

        CASE pcVerbText:
            WHEN "Add Default Ribbon Elements":U THEN
                THIS-OBJECT:AddDefaultRibbonElements () .
        END CASE .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

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
        Purpose: Disables the tool with the given Key when it exists in this instance.
                 Invoked by DisableTools.
        Notes:
        @param  pcToolKey Key of the Tool ButtonTool to be disabled
        @return True if the Tool exists, False if the Tool does not Exist
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL DisableControlTool (pcToolKey AS CHARACTER):

        DEFINE VARIABLE oList AS "System.Collections.Generic.List<RadItem>" NO-UNDO .

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i THIS-OBJECT:RadRibbonBar """RadRibbonBar"":U"} .

        oList = RadRibbonBarHelper:GetAllItemsWithTag (THIS-OBJECT:RadRibbonBar,
                                                       pcToolKey) .

        IF oList:Count = 0 THEN
            RETURN FALSE .

        {Consultingwerk/foreach.i RadItem oItem in oList}
            oItem:Enabled = FALSE .
        END.

        RETURN TRUE .

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

        DEFINE VARIABLE oList AS "System.Collections.Generic.List<RadItem>" NO-UNDO .

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i THIS-OBJECT:RadRibbonBar """RadRibbonBar"":U"} .

        oList = RadRibbonBarHelper:GetAllItemsWithTag (THIS-OBJECT:RadRibbonBar,
                                                       pcToolKey) .

        IF oList:Count = 0 THEN
            RETURN FALSE .

        {Consultingwerk/foreach.i RadItem oItem in oList}
            oItem:Enabled = TRUE .
        END.

        RETURN TRUE .

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

        DEFINE VARIABLE oList AS "System.Collections.Generic.List<RadItem>" NO-UNDO .

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i THIS-OBJECT:RadRibbonBar """RadRibbonBar"":U"} .

        oList = RadRibbonBarHelper:GetAllItemsWithTag (THIS-OBJECT:RadRibbonBar,
                                                       pcToolKey) .

        RETURN oList:Count > 0 .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Invoked during the System.ComponentModel.ISupportInitialize.EndInit
                 of the component (base class).
        Notes:   Set Navigation and TableIO tools disabled after initialization.
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID EndInit ():

        IF NOT THIS-OBJECT:DesignTime THEN DO:

            /* Mike Fechner, Consultingwerk Ltd. 11.05.2014
               SCL-41: Perform initialization only once in case
               Visual Inheritance is used in the containing Form.
               Both the Base Form and the Child Form may invoke
               the UltraToolbarsManager's EndInit method */
            IF NOT lInitialized THEN DO ON ERROR UNDO, THROW:

/*                InitializeCaptionOfTools ().*/
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

                /* Mike Fechner, Consultingwerk Ltd. 02.05.2015
                   Subscribe to Click event of all "Smart" RibbonBar Items */
                oSmartItems = RadRibbonBarHelper:GetAllItemsWithTag (THIS-OBJECT:RadRibbonBar,
                                                                     "NavigationFirst,NavigationPrev,NavigationNext,NavigationLast,":U +
                                                                     "TableIOAdd,TableIOCopy,TableIOUpdate,TableIOSave,TableIOCancel,TableIODelete,":U +
                                                                     "TransactionCommit,TransactionUndo":U) .

                {Consultingwerk/foreach.i RadItem oItem in oSmartItems}
                    oItem:Click:Subscribe (ItemClickHandler) .
                END.

                FINALLY:
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

        THIS-OBJECT:DesignerVerbs = "Add Default Ribbon Elements":U .

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
        Purpose: Destructor for the SmartRadRibbonBarController class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartRadRibbonBarController ():

        IF NOT THIS-OBJECT:DesignTime THEN DO:

            DELETE OBJECT oController .

        END.

    END DESTRUCTOR.

END CLASS.
