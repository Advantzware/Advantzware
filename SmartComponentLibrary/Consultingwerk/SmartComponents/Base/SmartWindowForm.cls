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
    File        : SmartWindowForm
    Purpose     : Base class for SmartComponent Library Forms
    Syntax      :
    Description :
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Wed Jun 17 08:58:39 CEST 2009
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.*                      FROM PROPATH .
USING Consultingwerk.Framework.Enum.*                 FROM PROPATH .
USING Consultingwerk.SmartComponents.*                FROM ASSEMBLY .
USING Consultingwerk.SmartComponents.Base.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.SmartComponents.Interfaces.*     FROM PROPATH .
USING Consultingwerk.SmartComponents.Support.*        FROM PROPATH  .
USING Consultingwerk.Util.*                           FROM PROPATH .
USING Infragistics.Win.UltraWinToolbars.*             FROM ASSEMBLY .
USING Progress.Lang.*                                 FROM ASSEMBLY .
USING System.ComponentModel.*                         FROM ASSEMBLY .
USING System.ComponentModel.Design.*                  FROM ASSEMBLY .
USING System.Windows.Forms.*                          FROM ASSEMBLY .
USING Progress.Windows.Form                           FROM ASSEMBLY .

{Consultingwerk/products.i}

CLASS Consultingwerk.SmartComponents.Base.SmartWindowForm
    INHERITS Consultingwerk.Forms.BaseForm
    IMPLEMENTS ISmartWindowForm:

    DEFINE VARIABLE lInOnDeactivate  AS LOGICAL                    NO-UNDO INITIAL FALSE .
    DEFINE VARIABLE oDeactivateTimer AS System.Windows.Forms.Timer NO-UNDO .

    DEFINE VARIABLE oController      AS SmartWindowFormController  NO-UNDO .

    /*------------------------------------------------------------------------------
        Purpose: Raised whenever the ActiveSmartSmartControl property has changed
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT ActiveSmartControlChanged  DELEGATE System.EventHandler.

    /*------------------------------------------------------------------------------
        Purpose: Raised when the ExternalDataSource property changes
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT ExternalDataSourceChanged DELEGATE System.EventHandler .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the ExtenalNavigationSource property changes
        Notes:
        @param sender The object that raised the OnExternalNavigationTargetChanged event
        @param e The System.EventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT ExternalNavigationTargetChanged SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                                        e AS System.EventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Raised when the PrimaryDataSource property changes
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT PrimaryDataSourceChanged DELEGATE System.EventHandler .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the PrimaryDataTarget property changes
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT PrimaryDataTargetChanged DELEGATE System.EventHandler .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the PrimaryNavigationSource property changes
        Notes:
        @param sender The object that raised the PrimaryNavigationSourceChanged event
        @param e The System.EventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT PrimaryNavigationSourceChanged SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                                       e AS System.EventArgs).

    /*------------------------------------------------------------------------------
        Purpose: A reference to the last recently activated (focussed) SmartControl
                 such as a SmartViewerControl or SmartBrowserControl
        Notes:   Propaty SET is private as the Method ActivateSmartControl is the
                 proper way to set this property from anywhere else
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ActiveSmartControl AS System.Windows.Forms.Control NO-UNDO
    GET.
    PRIVATE SET (arg AS System.Windows.Forms.Control):
        THIS-OBJECT:ActiveSmartControl = arg .

        OnActiveSmartControlChanged (System.EventArgs:Empty) .
    END.

    /*------------------------------------------------------------------------------
        Purpose: A DataSource that is passed in from the launching Container (Form)
                 to this Form. The ExternalDataSource will typically be passed on
                 to the internal PrimaryDataTarget
        Notes:   Used for PASS-THROUGH data links
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ExternalDataSource AS ISmartDataSource NO-UNDO
    GET.
    SET (arg AS ISmartDataSource):
        THIS-OBJECT:ExternalDataSource = arg .

        OnExternalDataSourceChanged (System.EventArgs:Empty) .
    END.

    /*------------------------------------------------------------------------------
        Purpose: A NavigationTarget that is passed in from the launching Container (Form)
                 to this Form. The ExternalNavigationtarget will typically be passed on
                 to the internal PrimaryNavigationSource
        Notes:   Used for PASS-THROUGH navigation links
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ExternalNavigationTarget AS ISmartNavigationTarget NO-UNDO
    GET.
    SET (arg AS ISmartNavigationTarget):
        THIS-OBJECT:ExternalNavigationTarget = arg .

        OnExternalNavigationTargetChanged (System.EventArgs:Empty) .
    END.

    /*------------------------------------------------------------------------------
        Purpose: A reference to the primary data source in this Form
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY PrimaryDataSource AS Consultingwerk.SmartComponents.Interfaces.Design.IDesignDataSource NO-UNDO
    GET.
    SET (arg AS Consultingwerk.SmartComponents.Interfaces.Design.IDesignDataSource):
        THIS-OBJECT:PrimaryDataSource = arg .

        OnPrimaryDataSourceChanged (System.EventArgs:Empty) .
    END.

    /*------------------------------------------------------------------------------
        Purpose: A reference to the primary data target in this Form
        Notes:   Used for PASS-THROUGH data links
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY PrimaryDataTarget AS Consultingwerk.SmartComponents.Interfaces.Design.IDesignDataTarget NO-UNDO
    GET.
    SET (arg AS Consultingwerk.SmartComponents.Interfaces.Design.IDesignDataTarget):
        THIS-OBJECT:PrimaryDataTarget = arg .

        OnPrimaryDataTargetChanged (System.EventArgs:Empty) .
    END.

    /*------------------------------------------------------------------------------
        Purpose: A reference to the primary navigation source in this Form
        Notes:   Used for PASS-THROUGH navigation links
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY PrimaryNavigationSource AS Consultingwerk.SmartComponents.Interfaces.Design.IDesignNavigationSource NO-UNDO
    GET.
    SET (arg AS Consultingwerk.SmartComponents.Interfaces.Design.IDesignNavigationSource):
        THIS-OBJECT:PrimaryNavigationSource = arg .

        OnPrimaryNavigationSourceChanged (System.EventArgs:Empty) .
    END.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if the Form asks contained Controls to Save when the
                 Form is deactivated and the Focus remains within the Application
        Notes:   May be combined with PromptForSaveOnApplicationLeave
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY PromptForSaveOnFormDeactivate AS LOGICAL INITIAL FALSE NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if the Form asks contained Controls to Save when the
                 Form is deactivated and the Focus leaves the Application
        Notes:   May be combined with PromptForSaveOnFormDeactivate
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY PromptForSaveOnApplicationLeave AS LOGICAL INITIAL FALSE NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartWindowForm class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartWindowForm ():
        SUPER ().

        ASSIGN oController = NEW SmartWindowFormController (THIS-OBJECT) .

        IF THIS-OBJECT:DesignTime THEN
            THIS-OBJECT:SetDesignerProperties () .
        ELSE DO:
            THIS-OBJECT:Deactivate:Subscribe (DeactivateHandler) .

            THIS-OBJECT:oController:InitializeFrameworkBootstrap () .
        END.

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Activates the Links between the active control and the
                 SmartToolbarController
        Notes:   When the ActiveControl is a read-only Browse, we attempt to activate
                 the TableIO Link on another Data-Target of the Browsers SmartDataAdapter
                 That could be a Viewer with is linked to the same adapter
        @param poActiveControl The Control where the Links should be activated
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ActivateLinks (poActiveControl AS System.Windows.Forms.Control):

        oController:ActivateLinks (poActiveControl) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Marks a SmartViewerControl or SmartBrowserControl as Active
        Notes:
        @param poControl The reference to the Control that has become active
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ActivateSmartControl (poControl AS System.Windows.Forms.Control):

        IF VALID-OBJECT (poControl) AND
          (NOT TYPE-OF (poControl, SmartViewerControl) AND
           NOT TYPE-OF (poControl, SmartBrowserControl)) THEN

           UNDO, THROW NEW AppError (SUBSTITUTE ("Unable to set Control of type &1 as Active SmartControl."{&TRAN},
                                                 poControl:GetType():FullName), 0) .

        THIS-OBJECT:ActiveSmartControl = poControl .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Deactivate event
        Notes:   Enables the times for the PromptForSaveOnFormDeactivate or
                 PromptForSaveOnApplicationLeave. Used to be OnDeactivate method
                 override, but that did lead to crashes at some customers on 11.1.0.005
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID DeactivateHandler (sender AS System.Object,
                                           e AS System.EventArgs):


        IF THIS-OBJECT:PromptForSaveOnFormDeactivate = TRUE OR
           THIS-OBJECT:PromptForSaveOnApplicationLeave = TRUE THEN

            IF NOT lInOnDeactivate THEN DO ON ERROR UNDO, THROW:
                ASSIGN lInOnDeactivate = TRUE .

                oDeactivateTimer = NEW System.Windows.Forms.Timer () .
                oDeactivateTimer:Interval = 1 .
                oDeactivateTimer:Tick:Subscribe (THIS-OBJECT:DeactivateTimerTick) .
                oDeactivateTimer:Enabled  = TRUE .

            END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Handles the Tick of the Deactivate Timer component
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID DeactivateTimerTick (sender AS System.Object,
                                             e AS System.EventArgs):

        DEFINE VARIABLE oActiveForm         AS System.Windows.Forms.Form NO-UNDO .

        oDeactivateTimer:Tick:Unsubscribe (THIS-OBJECT:DeactivateTimerTick) .
        oDeactivateTimer:Enabled = FALSE .

        oActiveForm = System.Windows.Forms.Form:ActiveForm .

        /* Mike Fechner, Consultingwerk Ltd. 06.08.2013
           Perform no action, when the current Form is Modal, this might
           be when starting a MessageForm or Lookup */
        IF VALID-OBJECT (oActiveForm) AND oActiveForm:Modal THEN
            RETURN .

        /* Mike Fechner, Consultingwerk Ltd. 06.08.2013
           PromptForSaveOnFormDeactivate requires a valid new active Form */
        IF THIS-OBJECT:PromptForSaveOnFormDeactivate = TRUE AND
            THIS-OBJECT:PromptForSaveOnApplicationLeave = FALSE AND
            NOT VALID-OBJECT (oActiveForm) THEN RETURN .

        IF THIS-OBJECT:PromptForSaveOnApplicationLeave = TRUE AND
            THIS-OBJECT:PromptForSaveOnFormDeactivate = FALSE AND
            VALID-OBJECT (oActiveForm) THEN RETURN .

        /* Mike Fechner, Consultingwerk Ltd. 06.08.2013
           PromptForSaveOnFormDeactivate required a valid new active Form */
        IF THIS-OBJECT:PromptForSaveOnApplicationLeave = TRUE AND
           NOT THIS-OBJECT:PromptForSaveOnFormDeactivate = TRUE AND
           VALID-OBJECT (oActiveForm) THEN
              RETURN .


        IF VALID-OBJECT (oActiveForm) AND VALID-OBJECT (oActiveForm:ActiveMdiChild) THEN
            oActiveForm = oActiveForm:ActiveMdiChild .

        IF THIS-OBJECT:RecurseSaveChangesOnDeactivateForm (THIS-OBJECT:Controls, TRUE) = TRUE THEN DO:

            IF VALID-OBJECT (oActiveForm) AND oActiveForm <> System.Windows.Forms.Form:ActiveForm THEN DO:
                IF VALID-OBJECT (oActiveForm:MdiParent) THEN
                    oActiveForm:MdiParent:Activate () .

                oActiveForm:Activate () .
            END.
        END.

        FINALLY:
            ASSIGN oDeactivateTimer = ? .
                   lInOnDeactivate  = FALSE .

        END FINALLY.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Add linked SmartDataAdapter instances to a List
        Notes:   Used by GetLinkedDataAdpaters (poStart)
        @param poStart The reference to a SmartDataAdapter to use as the starting point
        @param poList The list of SmartDataAdapters to add to
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID GetLinkedDataAdapters (poStart AS SmartDataAdapter,
                                                 poList AS ListSmartDataAdapter):

        oController:GetLinkedDataAdapters (poStart,
                                           poList) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns a List of Data Adapter instances in the Form
        Notes:   Returns the PrimaryDataTarget (when that's a Data Adapter) and
                 the PrimaryDataSource or the poStart SmartDataAdapter instance
                 as well as all their DataTarget instances
        @param poStart An optional reference to a SmartDataAdapter to use as the starting point
        @return The List of Data Adapter instances
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC ListSmartDataAdapter GetLinkedDataAdapters (poStart AS SmartDataAdapter):

        RETURN oController:GetLinkedDataAdapters (poStart) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the ISmartToolbarControl of the ISmartWindowForm
        Notes:
        @return The reference to the ISmartToolbarControl of the ISmartWindowForm
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC ISmartToolbarControl GetToolbar ():

        DEFINE VARIABLE oToolbar                AS Infragistics.Win.UltraWinToolbars.UltraToolbarsManager NO-UNDO .

        oToolbar = Infragistics.Win.UltraWinToolbars.UltraToolbarsManager:FromForm (THIS-OBJECT) .

        IF NOT VALID-OBJECT (oToolbar) THEN
            RETURN ? .

        IF TYPE-OF (oToolbar, SmartToolbarController) THEN
            RETURN CAST (oToolbar, ISmartToolbarControl)  .

        RETURN ? .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns a List of Data Adapter instances in the Form
        Notes:   Returns the PrimaryDataTarget (when that's a Data Adapter) and
                 the PrimaryDataSource as well as all their DataTarget instances
        @return The List of Data Adapter instances
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC ListSmartDataAdapter GetLinkedDataAdapters ():

        RETURN oController:GetLinkedDataAdapters () .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the ActiveSmartControlChanged event
        Notes:
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnActiveSmartControlChanged (e AS System.EventArgs):

        IF NOT VALID-OBJECT (e) THEN
            e = System.EventArgs:Empty .

        THIS-OBJECT:ActiveSmartControlChanged:Publish (THIS-OBJECT, e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the ExternalDataSourceChanged event
        Notes:
        @param e The System.EventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnExternalDataSourceChanged (e AS System.EventArgs):

        IF NOT VALID-OBJECT (e) THEN
            e = System.EventArgs:Empty .

        THIS-OBJECT:ExternalDataSourceChanged:Publish (THIS-OBJECT, e) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the OnExternalNavigationTargetChanged
        Notes:
        @param e The System.EventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnExternalNavigationTargetChanged (e AS System.EventArgs):

        IF NOT VALID-OBJECT (e) THEN
            e = System.EventArgs:Empty .

        THIS-OBJECT:ExternalNavigationTargetChanged:Publish (THIS-OBJECT, e) .

    END METHOD .

     /*------------------------------------------------------------------------------
        Purpose: Checks wheather it is ok to close the form. If not the FormClosing
                 process is canceled.
        Notes:
        @param e The FormClosingEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID OnFormClosing (INPUT e AS System.Windows.Forms.FormClosingEventArgs):

        SUPER:OnFormClosing(e).

        oController:FormClosingHandler (e) .

        /* Mike Fechner, Consultingwerk Ltd. 21.01.2014
           When we're closing this Form, we don't need the Deactivate Handler
           If we're not unsubscribing here, we might receive an Invalid handle
           error while closing the Form */
        IF NOT e:Cancel THEN
            THIS-OBJECT:Deactivate:Unsubscribe (DeactivateHandler) NO-ERROR .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the Load event
        Notes:   Passes though the ExternalDataSource to the PrimaryDataTarget
        @param e The FormClosingEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnLoad (e AS System.EventArgs):

        IF NOT THIS-OBJECT:DesignTime THEN

            oController:LoadHandler() .

        SUPER:OnLoad (e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the PrimaryDataSourceChanged event
        Notes:
        @param e The System.EventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnPrimaryDataSourceChanged (e AS System.EventArgs):

        IF NOT VALID-OBJECT (e) THEN
            e = System.EventArgs:Empty .

        THIS-OBJECT:PrimaryDataSourceChanged:Publish (THIS-OBJECT, e) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the PrimaryDataTargetChanged event
        Notes:
        @param e The System.EventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnPrimaryDataTargetChanged (e AS System.EventArgs):

        IF NOT VALID-OBJECT (e) THEN
            e = System.EventArgs:Empty .

        THIS-OBJECT:PrimaryDataTargetChanged:Publish (THIS-OBJECT, e) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the PrimaryNavigationSourceChanged
        Notes:
        @param e The System.EventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnPrimaryNavigationSourceChanged (e AS System.EventArgs):


        IF NOT VALID-OBJECT (e) THEN
            e = System.EventArgs:Empty .

        THIS-OBJECT:PrimaryNavigationSourceChanged:Publish (THIS-OBJECT, e) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Internal event handler
        Notes:   Invoked when the SmartToolbarController property changes
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnSmartToolbarControllerChanged ():

        RETURN.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Shows the Prompt to save pending transactions to the backend while
                 closing the Form
        Notes:   Can be overridden by customers to change behaviour
        @return The DialogResultEnum value indicating "Yes" to save changes, "No" to not save changes and "Cancel" to not close the Form
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC DialogResultEnum PromptForSaveOfPendingChangesOnClose ():

        RETURN oController:PromptForSaveOfPendingChangesOnClose () .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Recursivly ask all Controls implementing the ICheckOkToClose Interface
                 wheather it is ok to close the form.
        Notes:
        @param poControls The System.Windows.Forms.Control+ControlCollection Collection of Controls to iterate
        @param plShowMessage When TRUE a message is Shown explaining why it is not ok to close the parent form.
        @param poCloseReason The System.Windows.Forms.CloseReason passed from then OnFormClosing Method of the parent form giving the reason who closes the form.
        @return Logical value indicating if the Form can be closed or not.
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL RecurseCheckIsOkToClose (INPUT poControls    AS System.Windows.Forms.Control+ControlCollection,
                                                   INPUT plShowMessage AS LOGICAL,
                                                   INPUT poCloseReason AS System.Windows.Forms.CloseReason):

        RETURN oController:RecurseCheckIsOkToClose (poControls,
                                                    plShowMessage,
                                                    poCloseReason) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Recursively checks all contained controls to save changes on
                 Deactivate of the Form
        Notes:
        @param poControls The System.Windows.Forms.Control+ControlCollection Collection of Controls to iterate
        @param plShowMessage When TRUE a message is Shown explaining why it is not ok to close the parent form.
        @return Logical value indicating if it's o.k. to move to the Activated window
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED LOGICAL RecurseSaveChangesOnDeactivateForm (poControls    AS System.Windows.Forms.Control+ControlCollection,
                                                                 plShowMessage AS LOGICAL):

        RETURN oController:RecurseSaveChangesOnDeactivateForm (poControls,
                                                               plShowMessage) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Sets Properties to customize Visual Designer behaviour
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID SetDesignerProperties ():

        THIS-OBJECT:NonBrowsableProperties = TRIM (THIS-OBJECT:NonBrowsableProperties +
                                             ",ActiveSmartControl":U, ",":U) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SmartWindowForm class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartWindowForm ():

        oController:RemoveDataLinks () .

        GarbageCollectorHelper:DeleteObject (oController) .

    END DESTRUCTOR .

END CLASS.
