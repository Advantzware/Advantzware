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
    File        : SmartLookup
    Purpose     : Abstract base class for Lookup Controls
    Syntax      :
    Description :
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Thu Mar 12 12:51:55 CET 2009
    Notes       : Extends an Infragistics UltraText Editor with Lookup
                  functionality. This base class is responsible for the
                  generic handling of the Lookup functionality (based on
                  values entered in the Control or raising the Lookup
                  Dialog). The base class is not responsible for the
                  data access itself. This is implemented in the
                  child classes of this class.
                  The SmartLookup can be used as an embedded
                  Editor for an UltraGrid Control's column. When doing so,
                  the OwningGrid property of the SmartLookup property
                  needs to by set to the reference of the UltraGrid.
                  When the Controls in the "LookupControls" list are
                  used as Embedded Editors to an UltraGrid it is required
                  to data bind those Controls to the same binding source
                  fields as the grid cells are that use those embedded
                  editors
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Exceptions.*                      FROM PROPATH.
USING Consultingwerk.SmartComponents.*                 FROM ASSEMBLY .
USING Consultingwerk.SmartComponents.Base.*            FROM PROPATH .
USING Consultingwerk.SmartComponents.Interfaces.*      FROM PROPATH .
USING Consultingwerk.Util.*                            FROM PROPATH .
USING Infragistics.Win.UltraWinGrid.*                  FROM ASSEMBLY .
USING Infragistics.Win.UltraWinEditors.UltraTextEditor FROM ASSEMBLY .
USING Infragistics.Win.UltraWinEditors.*               FROM ASSEMBLY .
USING Progress.Lang.*                                  FROM ASSEMBLY .
USING System.ComponentModel.*                          FROM ASSEMBLY .
USING System.ComponentModel.Design.*                   FROM ASSEMBLY .
USING System.Windows.Forms.*                           FROM ASSEMBLY .

{Consultingwerk/products.i}

CLASS Consultingwerk.SmartComponents.Base.SmartLookup
    INHERITS UltraTextEditor
    IMPLEMENTS ICustomTypeDescriptor, IHasDesignerVerbs, IEmbeddedSmartEditor
    ABSTRACT:

    /* Mike Fechner, Consultingwerk Ltd. 19.05.2010
       Core implementation of ICustomTypeDescriptor */
    {Consultingwerk/SmartComponents/Base/ICustomTypeDescriptor.i}
    {Consultingwerk/SmartComponents/Base/CreateVerbs.i}

    DEFINE VARIABLE cLookupBrowserFields AS CHARACTER                                     NO-UNDO.
    DEFINE VARIABLE oLookupTimer         AS System.Windows.Forms.Timer                    NO-UNDO.

    /* Logical value indicating that the lookup activity is pending - used when the
       Timer is not used (SCL-884 - ability to disable the Lookup timer */
    DEFINE VARIABLE lPendingLookup AS LOGICAL NO-UNDO INIT FALSE .

    /*------------------------------------------------------------------------------
        Purpose: Event raised after the lookup has performed a search
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT AfterPerformLookup DELEGATE System.EventHandler.

    /*------------------------------------------------------------------------------
        Purpose: Event raised before the lookup performs a search
        Notes:   The cancellable event is raised before the Lookup action is performed.
                 The event still allows to modify properties relevant to the Lookup
                 (such as modifying the LookupQueryString)
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT BeforePerformLookup DELEGATE System.ComponentModel.CancelEventHandler .

    /*------------------------------------------------------------------------------
        Purpose: Event raised when the Lookup Dialog's browser is performing it's
                 layout
        Notes:
        @param sender The reference to the object that raised the event
        @param e The reference to the InitializeLookupBrowserLayoutEventArgs with data for this event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT InitializeLookupBrowserLayout SIGNATURE VOID
            (sender AS System.Object,
             e AS InitializeLookupBrowserLayoutEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Event raised when the Lookup has returned a record
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT LookupComplete DELEGATE System.EventHandler.

    /*------------------------------------------------------------------------------
        Purpose: Event raised before the Lookup Dialog is shown
        Notes:   The cancelable event is raised before the Lookup Dialog is
                 instantiated and shown. This event still allows to modify properties
                 relevant for the Lookup Dialog.
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT ShowLookupDialog DELEGATE System.ComponentModel.CancelEventHandler .

    /*------------------------------------------------------------------------------
        Purpose: This Property is used to perform the Action to add a LookupButton to
                 the Control.
                 The Value of this Property is not changed but the Button is added to
                 the Control when temporarily set to TRUE.
        Notes:   Workaround to be able perform actions from Propertygrid.
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY AddLookupButton AS LOGICAL INITIAL FALSE NO-UNDO
    GET.
    SET(INPUT arg AS LOGICAL):
        IF arg = FALSE THEN
            RETURN.

        AddLookupEditorButton ().
    END SET.

    /*------------------------------------------------------------------------------
        Purpose: Property that is used to generate Designer Verbs (actions for the
                 Visual Designer)
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PROTECTED PROPERTY DesignerVerbs AS CHARACTER NO-UNDO
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Propery flags if the current instance of the SmartBindingSource
                 is running at design time or runtime.
        Notes:   Set during constructor using a check at the
                 System.ComponentModel.LicenseManager
    ------------------------------------------------------------------------------*/
    DEFINE PROTECTED PROPERTY DesignTime AS LOGICAL NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Property that is used to mark properties (comma-delimited list) that
                 will be excluded from the .NET TypeDescriptor properties list
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PROTECTED PROPERTY HiddenProperties AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Columns (comma delimited list of Column Names) not to be shown in the
                 LookupBrowser.
        Notes:   The value of this Property is defined during DesignTime.
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupBrowserExcludeFields AS CHARACTER NO-UNDO INIT "":U
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Columns (comma delimited list of Column Names) to be shown in the
                 LookupBrowser.
        Notes:   The value of this Property is defined during DesignTime.
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupBrowserFields AS CHARACTER NO-UNDO INIT "":U
    GET:
        RETURN cLookupBrowserFields.
    END GET.
    SET(INPUT arg AS CHARACTER):
        cLookupBrowserFields = arg.
    END SET.

    /*------------------------------------------------------------------------------
        Purpose: Controls if a clear is pending - that is if a previous PerformLookup
                 did not return a record - but did not clear the key value. In that
                 case the key value needs to be cleared on LostFocus
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PROTECTED PROPERTY LookupClearPending AS LOGICAL NO-UNDO INIT FALSE
    GET.
    SET.

      /*------------------------------------------------------------------------------
        Purpose: The value of the field which is returned by the SmartLookup.
                 The value of this Property is defined during DesignTime.
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupKeyField AS CHARACTER NO-UNDO INIT "":U
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: A comma seperated list of Column names. The value of those fields
                 gets filled into the corresponding Controls of the Property
                 LookupControls.
                 The value of this Property is defined during DesignTime.
        Notes:   Corresponding to the LookupControls
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupFields AS CHARACTER NO-UNDO INIT "":U
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: A comma seperated list of Control names. The value of the corresponding
                 Columns gets filled into those Controls.
                 The value of this Property is defined during DesignTime.
        Notes:   Corresponding to the LookupFields
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupControls AS CHARACTER NO-UNDO INIT "":U
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Dialog title for the LookupDialogForm.
                 The value of this Property is defined during DesignTime.
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupDialogTitle AS CHARACTER INITIAL "Lookup":U NO-UNDO
    GET.
    SET.

     /*------------------------------------------------------------------------------
        Purpose: The instance of the System.Windows.Forms.Timer component used by
                 this lookup to implement the delayed search
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupTimer AS System.Windows.Forms.Timer NO-UNDO
    GET():
        IF THIS-OBJECT:LookupTimerDisabled = TRUE THEN
            RETURN ? .

        IF NOT VALID-OBJECT (oLookupTimer) THEN
            InitializeTimer () .

        RETURN oLookupTimer .
    END GET.

     /*------------------------------------------------------------------------------
        Purpose: The number of msecs after a key has been pressed in a SmartLookup
                 until the entered value is looked up. This allows the user to enter
                 multiple keys before the value is looked up (in a data source).
        Notes:   The value of this Property is defined during DesignTime.
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupTimerDelay AS INTEGER NO-UNDO INIT 750
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Allows to completely disable the lookup timer functioanlity
        Notes:   When this is set to true, the LookupTimerDelay is ignored
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupTimerDisabled AS LOGICAL INITIAL FALSE NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Property that is used to mark properties (comma-delimited list) that
                 should not show up in the Visual Designers Property Grid
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PROTECTED PROPERTY NonBrowsableProperties AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the reference to the grid that owns this SmartLookup
                 as an Embedded Editor Control for a cell
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY OwningGrid AS UltraGrid NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the SmartLookup class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartLookup ():
        SUPER ().

        THIS-OBJECT:DesignTime = Progress.Util.EnumHelper:AreEqual(System.ComponentModel.LicenseManager:UsageMode,
                                                                   System.ComponentModel.LicenseUsageMode:Designtime) .

        IF THIS-OBJECT:DesignTime THEN
            THIS-OBJECT:SetDesignerProperties () .
        ELSE
            THIS-OBJECT:Disposed:Subscribe (DisposedHandler) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: This Method is used to add a LookupButton to the Control and sets
                 Key and Text Properties.
        Notes:   Invoked by the Property setter of AddLookupButton or the Designer Verb
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID AddLookupEditorButton ():

        DEFINE VARIABLE editorButtonLookup AS EditorButton NO-UNDO.

        IF NOT THIS-OBJECT:ButtonsRight:Exists ("ShowLookupDialog":U) THEN DO:
            editorButtonLookup = NEW Infragistics.Win.UltraWinEditors.EditorButton (THIS-OBJECT:Name + "ShowLookupDialog":U).
            editorButtonLookup:Key = "ShowLookupDialog":U .
            editorButtonLookup:Text = "...":U.

            THIS-OBJECT:ButtonsRight:Add (editorButtonLookup).
        END.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Assigns a Character Value to a Cell
        Notes:
        @param poCell The UltraGridCell to assign the value to
        @param pcValue The value to assign to the cell
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID AssignValueToCell (poCell AS UltraGridCell,
                                             pcValue AS CHARACTER):

        DEFINE VARIABLE cPromptChar AS CHARACTER NO-UNDO INIT " ":U.
        DEFINE VARIABLE cDataType   AS CHARACTER NO-UNDO .

        ASSIGN cDataType = DataTypeHelper:ToAblDataType (poCell:Column:DataType) .

        IF VALID-OBJECT (poCell:EditorComponentResolved) AND
           (cDataType = "DECIMAL":U OR cDataType = "INT64":U OR cDataType = "INTEGER":U OR cDataType = "DATETIME":U) THEN DO:

            IF ReflectionHelper:HasProperty (poCell:EditorComponentResolved, "PromptChar":U) THEN
                ASSIGN cPromptChar = UNBOX (ReflectionHelper:GetPropertyValue (poCell:EditorComponentResolved, "PromptChar":U)) .

            IF cPromptChar = "":U THEN
                ASSIGN cPromptChar = " ":U .
        END.

        CASE cDataType:
            WHEN "DATETIME":U THEN
                poCell:Value = BOX (DataTypeHelper:ToDateTime (REPLACE (pcValue, cPromptChar, "":U))) .
            WHEN "DECIMAL":U THEN
                poCell:Value = BOX (DataTypeHelper:ToDecimal (REPLACE (pcValue, cPromptChar, "":U))) .
            WHEN "INT64":U THEN
                poCell:Value = BOX (DataTypeHelper:ToInt64 (REPLACE (pcValue, cPromptChar, "":U))) .
            WHEN "INTEGER":U THEN
                poCell:Value = BOX (DataTypeHelper:ToInteger (REPLACE (pcValue, cPromptChar, "":U))) .
            WHEN "LOGICAL":U THEN
                poCell:Value = BOX (DataTypeHelper:ToLogical (pcValue)) .
            OTHERWISE
                poCell:Value = pcValue .
        END CASE .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Disposed event of the SmartLookup instance
        Notes:   Not subscribed during development, stops the Lookup Timer if it
                 should still be active
        @param sender The reference to the object that raised this event
        @param e The System.EventArgs with no data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID DisposedHandler (sender AS System.Object,
                                           e AS System.EventArgs):

        THIS-OBJECT:StopTimer() .

    END METHOD .

     /*------------------------------------------------------------------------------
        Purpose: Handles F3 key to invoke ShowLookupDialog method.
                 This Method is executed when the KeyDown Event of the base class is
                 fired.
        Notes:
        @param e System.Windows.Forms.KeyEventArgs
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID HandleKeyDown (e AS KeyEventArgs):

        IF NOT VALID-OBJECT (THIS-OBJECT:OwningGrid) THEN DO:
            IF e:Handled = FALSE AND Progress.Util.EnumHelper:AreEqual (e:KeyCode, SmartLookupSettings:ShortcutKey)
                AND THIS-OBJECT:Enabled THEN DO:

                ShowLookupDialog () .

                e:Handled = TRUE .
            END.

            IF e:Handled = FALSE AND THIS-OBJECT:Focused AND Progress.Util.EnumHelper:AreEqual (e:KeyCode, Keys:Delete)
                AND THIS-OBJECT:Enabled THEN DO:

                IF THIS-OBJECT:LookupTimerDisabled = FALSE THEN
                    THIS-OBJECT:StartTimer() .
                ELSE
                    ASSIGN lPendingLookup = TRUE .
            END.
        END.
        ELSE DO:
            IF e:Handled = FALSE AND Progress.Util.EnumHelper:AreEqual (e:KeyCode, SmartLookupSettings:ShortcutKey)
                AND VALID-OBJECT (THIS-OBJECT:OwningGrid:ActiveCell) AND THIS-OBJECT:OwningGrid:ActiveCell:IsInEditMode THEN DO:

                ShowLookupDialog () .

                e:Handled = TRUE .
            END.

            IF e:Handled = FALSE AND
               VALID-OBJECT (THIS-OBJECT:OwningGrid:ActiveCell) AND
               THIS-OBJECT:OwningGrid:ActiveCell:EditorComponentResolved = THIS-OBJECT AND
               Progress.Util.EnumHelper:AreEqual (e:KeyCode, Keys:Delete) AND
               THIS-OBJECT:Enabled THEN DO:

                IF THIS-OBJECT:LookupTimerDisabled = FALSE THEN
                    THIS-OBJECT:StartTimer() .
                ELSE
                    ASSIGN lPendingLookup = TRUE .
            END.
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the Invokes the EditorButtonClick event
        Notes:
        @param e The EditorButtonEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnEditorButtonClick (e AS Infragistics.Win.UltraWinEditors.EditorButtonEventArgs):

        CASE e:Button:Key:
            WHEN "ShowLookupDialog":U
                THEN ShowLookupDialog ().
        END CASE.

        SUPER:OnEditorButtonClick (e) .

        CATCH err AS Progress.Lang.Error:
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Initializes the Timer Component
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID InitializeTimer ():

        IF THIS-OBJECT:LookupTimerDisabled = TRUE THEN
            RETURN .

        oLookupTimer = NEW System.Windows.Forms.Timer () .
        oLookupTimer:Enabled = FALSE .
        oLookupTimer:Tick:Subscribe (LookupTimerTick) .

    END METHOD.

     /*------------------------------------------------------------------------------
        Purpose: Handles F3 key to invoke ShowLookupDialog method.
                 This Method is executed when the KeyDown Event of the base class is
                 fired.
        Notes:
        @param e System.Windows.Forms.KeyEventArgs
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnKeyDown (e AS KeyEventArgs):

        SUPER:OnKeyDown (e) .

        THIS-OBJECT:HandleKeyDown (e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Handles the InitializeLayout event of the Lookup Browser
        Notes:
        @param sender The reference to the object that raised the event
        @param e The InitializeLayoutEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID LookupBrowserInitializeLayoutHandler (sender AS System.Object,
                                                                e AS InitializeLayoutEventArgs):

        DEFINE VARIABLE e2 AS InitializeLookupBrowserLayoutEventArgs NO-UNDO .

        ASSIGN e2 = NEW InitializeLookupBrowserLayoutEventArgs (e:Layout,
                                                                CAST (sender, UltraGrid)) .

        OnInitializeLookupBrowserLayout (e2) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event Handler for the Tick event of the Lookup Timer Component
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID LookupTimerTick (sender AS System.Object,
                                           e AS System.EventArgs):

        DEFINE VARIABLE lLostFocus AS LOGICAL NO-UNDO.

        IF THIS-OBJECT:LookupTimerDisabled = FALSE THEN DO:
            THIS-OBJECT:LookupClearPending = FALSE .
            THIS-OBJECT:LookupTimer:Stop () .
            THIS-OBJECT:LookupTimer:Enabled = FALSE .
        END.

        ASSIGN lLostFocus = NOT (VALID-OBJECT (e) AND VALID-OBJECT (sender)) .

        PerformLookup (lLostFocus) .

        CATCH err AS Progress.Lang.Error:
            Consultingwerk.Util.ErrorHelper:ShowErrorMessageBox (err).
        END CATCH.

        FINALLY:
            ASSIGN lPendingLookup    = FALSE
                   .

        END FINALLY.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the OnAfterPerformLookup event
        Notes:   Invoked after the Lookup has fetched data
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnAfterPerformLookup (e AS System.EventArgs):

        IF NOT VALID-OBJECT (e) THEN
            e = System.EventArgs:Empty .

        THIS-OBJECT:AfterPerformLookup:Publish (THIS-OBJECT, e) .

        /* Mike Fechner, Consultingwerk Ltd. 24.11.2014
           SCL-111 : Usage of the SmartLookup (or derived type) as an embedded
                     editor */
        IF VALID-OBJECT (THIS-OBJECT:OwningGrid) THEN DO:
            THIS-OBJECT:AssignValueToCell (THIS-OBJECT:OwningGrid:ActiveCell, THIS-OBJECT:Text) .

            IF THIS-OBJECT:LookupControls > "":U THEN
                THIS-OBJECT:UpdateCellsWithEmbeddedEditors () .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the BeforePerformLookup event
        Notes:   Cancellable event, raised when the user leaves the Lookup field or
                 after changing the Text in the Lookup Control and waiting for the
                 LookupTimer to fire
        @param e The CancelEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnBeforePerformLookup (e AS System.ComponentModel.CancelEventArgs):

        IF NOT VALID-OBJECT (e) THEN
            e = NEW System.ComponentModel.CancelEventArgs () .

        THIS-OBJECT:BeforePerformLookup:Publish (THIS-OBJECT, e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the EnabledChanged event
        Notes:   Stops the Lookup timer when the Control get's disabled
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID OnEnabledChanged (INPUT e AS System.EventArgs):

        IF THIS-OBJECT:Enabled = FALSE THEN DO:
            IF THIS-OBJECT:LookupTimerDisabled = TRUE THEN
                ASSIGN lPendingLookup = FALSE .
            ELSE IF VALID-OBJECT (oLookupTimer) THEN
            oLookupTimer:Stop () .
        END.

        SUPER:OnEnabledChanged(e).

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the InitializeLookupBrowserLayout event
        Notes:
        @param e The InitializeLookupBrowserLayoutEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnInitializeLookupBrowserLayout (e AS InitializeLookupBrowserLayoutEventArgs):

        IF NOT VALID-OBJECT (e) THEN
            UNDO, THROW NEW AppError ("Unable to raise the InitializeLookupBrowserLayout event without an event argument."{&TRAN}, 0) .

        THIS-OBJECT:InitializeLookupBrowserLayout:Publish (THIS-OBJECT, e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the LookupCompleted event
        Notes:   Invokes the LookupCompleted call back in the SmartViewerControl
                 (when ILookupCompleteCallback is implemented).
                 Thid Method is called by ShowLookupDialog in a SmartLookup derived
                 class to perform the callback to the container.
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnLookupCompleted (e AS System.EventArgs):

        DEFINE VARIABLE oParent       AS System.Windows.Forms.Control      NO-UNDO.

        IF NOT VALID-OBJECT (e) THEN
          ASSIGN e = System.EventArgs:Empty .

        /* Mike Fechner, Consultingwerk Ltd. 21.08.2009
           Raise event */
        THIS-OBJECT:LookupComplete:Publish (THIS-OBJECT, e) .

        /* Mike Fechner, Consultingwerk Ltd. 21.08.2009
           Seek SmartViewerControl which is parenting this lookup control */
        oParent = THIS-OBJECT:Parent .

        ParentLookup: DO WHILE VALID-OBJECT(oParent):
            IF TYPE-OF (oParent, SmartViewerControl) THEN
                LEAVE ParentLookup .

            oParent = oParent:Parent .
        END.

        IF VALID-OBJECT(oParent) AND TYPE-OF(oParent, ILookupCompleteCallback) THEN
            CAST(oParent, ILookupCompleteCallback):LookupCompleteCallback (THIS-OBJECT) .

        /* Mike Fechner, Consultingwerk Ltd. 24.11.2014
           SCL-111 : Usage of the SmartLookup (or derived type) as an embedded
                     editor */
        IF VALID-OBJECT (THIS-OBJECT:OwningGrid) THEN DO:

            THIS-OBJECT:AssignValueToCell (THIS-OBJECT:OwningGrid:ActiveCell, THIS-OBJECT:Text) .

            IF THIS-OBJECT:LookupControls > "":U THEN
                THIS-OBJECT:UpdateCellsWithEmbeddedEditors () .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the KeyPress event
        Notes:   Starts the Lookup Timer when the Control has Focus
        @param e The KeyPressEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID OnKeyPress (e AS System.Windows.Forms.KeyPressEventArgs):

        SUPER:OnKeyPress (e).

        IF THIS-OBJECT:Focused THEN
            THIS-OBJECT:StartTimer () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the Leave event
        Notes:   When the Control Leave event ocurs the TimerTick event is
                 simulated, so that the lookup will be performed
        @param e Default .NET System.EventArgs (empty event argument)
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID OnLeave (e AS System.EventArgs):

        /* Mike Fechner, Consultingwerk Ltd. 12.12.2011
           Previously we were using the OnLostFocus event method here, but according to
           http://msdn.microsoft.com/en-us/library/system.windows.forms.control.lostfocus.aspx
           Instead the Enter and Leave events should be used for all controls */

        IF lPendingLookup OR (VALID-OBJECT (THIS-OBJECT:LookupTimer) AND THIS-OBJECT:LookupTimer:Enabled) THEN DO:
            THIS-OBJECT:LookupClearPending = FALSE .
            THIS-OBJECT:LookupTimerTick (?, ?) .

            ASSIGN lPendingLookup = FALSE .
        END .

        IF THIS-OBJECT:LookupClearPending THEN
            PerformLookup (TRUE) .

        SUPER:OnLeave (e).

        CATCH err AS Progress.Lang.Error:
            Consultingwerk.Util.ErrorHelper:ShowErrorMessageBox (err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the ShowLookupDialog event
        Notes:   Cancellable event
        @param e Default .NET CancelEventArgs (cancellable event argument)
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnShowLookupDialog (e AS System.ComponentModel.CancelEventArgs):

        IF NOT VALID-OBJECT (e) THEN
            e = NEW System.ComponentModel.CancelEventArgs () .

        THIS-OBJECT:ShowLookupDialog:Publish (THIS-OBJECT, e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the Value changed event
        Notes:   Sets the Lookup pending property when not using the Lookup timer
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID OnValueChanged (e AS System.EventArgs):

        SUPER:OnValueChanged (e) .

        IF THIS-OBJECT:LookupTimerDisabled = TRUE AND THIS-OBJECT:Focused THEN
             ASSIGN lPendingLookup = TRUE .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event Handler method for Designer Verbs
        Notes:   This method is intended to be overridden
        @param pcVerbText The name (Label) of the Designer Verb that was invoked
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID OnVerbClicked (pcVerbText AS CHARACTER):

        Consultingwerk.Util.ErrorHelper:ErrorMessageDialogAllowed = FALSE .

        CASE pcVerbText:
            WHEN "Add Lookup Button":U THEN DO:
                THIS-OBJECT:AddLookupEditorButton () .
            END.
        END CASE .

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

        FINALLY:
            Consultingwerk.Util.ErrorHelper:ErrorMessageDialogAllowed = TRUE .
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Performs the KeyDown event for an embedded SmartLookup
        Notes:   Only supported when the lookup is used as an embedded editor
        @param e The KeyEventArgs of the original UltraGrid event
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID PerformKeyDown (e AS KeyEventArgs):

        IF NOT VALID-OBJECT (THIS-OBJECT:OwningGrid) THEN
            UNDO, THROW NEW NotSupportedException ("PerformKeyDown":U, THIS-OBJECT:GetClass():TypeName) .

        THIS-OBJECT:HandleKeyDown (e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Performs the KeyPress event for an embedded SmartLookup
        Notes:   Only supported when the lookup is used as an embedded editor
        @param e The KeyPressEventArgs of the original UltraGrid event
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID PerformKeyPress (e AS KeyPressEventArgs) :

        IF NOT VALID-OBJECT (THIS-OBJECT:OwningGrid) THEN
            UNDO, THROW NEW NotSupportedException ("PerformKeyPress":U, THIS-OBJECT:GetClass():TypeName) .

        IF e:Handled = FALSE AND
           VALID-OBJECT (THIS-OBJECT:OwningGrid:ActiveCell) AND
           THIS-OBJECT:OwningGrid:ActiveCell:IsInEditMode THEN DO:

            IF NOT THIS-OBJECT:LookupTimerDisabled THEN
                THIS-OBJECT:StartTimer () .
            ELSE
                ASSIGN lPendingLookup = TRUE .
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Performs the Leave (BeforeCellDeactivate) event for an embedded SmartLookup
        Notes:   Only supported when the lookup is used as an embedded editor
        @param e The CancelEventArgs of the original UltraGrid event
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID PerformLeave (e AS CancelEventArgs):

        IF NOT VALID-OBJECT (THIS-OBJECT:OwningGrid) THEN
            UNDO, THROW NEW NotSupportedException ("PerformKeyPress":U, THIS-OBJECT:GetClass():TypeName) .

        IF e:Cancel = FALSE AND
           VALID-OBJECT (THIS-OBJECT:OwningGrid:ActiveCell) THEN DO:

            /* Mike Fechner, Consultingwerk Ltd. 24.11.2014
               SCL-111 : Usage of the SmartLookup (or derived type) as an embedded
                         editor */
            IF (VALID-OBJECT (THIS-OBJECT:LookupTimer) AND THIS-OBJECT:LookupTimer:Enabled) OR
               (THIS-OBJECT:LookupTimerDisabled AND lPendingLookup = TRUE) THEN DO:
                THIS-OBJECT:LookupClearPending = FALSE .
                THIS-OBJECT:LookupTimerTick (?, ?) .
            END .

            IF THIS-OBJECT:LookupClearPending THEN
                PerformLookup (TRUE) .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Performs the lookup functioanlity, performs a call to the data source
                 to retrieve the record based on the field value the user entered
                 in the lookup control
        Notes:   Abstract method that needs to be overidden in a more specialized
                 Lookup class, public method, that always assumes that the lookup has
                 no longer focus. Calls into PerformLookup (TRUE).
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC ABSTRACT VOID PerformLookup () .

    /*------------------------------------------------------------------------------
        Purpose: Performs the lookup functioanlity, performs a call to the data source
                 to retrieve the record based on the field value the user entered
                 in the lookup control
        Notes:   Abstract method that needs to be overidden in a more specialized
                 Lookup class
        @param plLostFocus Logical value indicating if the PerformLookup was invoked due to a loss of focus
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED ABSTRACT VOID PerformLookup (plLostFocus AS LOGICAL) .

    /*------------------------------------------------------------------------------
        Purpose: Calls PerformLookup only if the LookupTimer is currently enabled
        Notes:   Called by the SmartViewerControl at the begin of a Save operations
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID PerformLookupIfActive ():

        IF lPendingLookup OR (VALID-OBJECT (oLookupTimer) AND oLookupTimer:Enabled = TRUE) THEN DO:
            PerformLookup (TRUE) .

            lPendingLookup = FALSE .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Sets Properties to customize Visual Designer behaviour
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID SetDesignerProperties ():

        ASSIGN
            THIS-OBJECT:DesignerVerbs          = "Add Lookup Button":U .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Shows the Lookup Dialog
        Notes:   This Method needs to be overridden in a SmartLookup derived class.
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED ABSTRACT VOID ShowLookupDialog ().

    /*------------------------------------------------------------------------------
        Purpose: Prototype of the method GetLookupRegistryKey which can be used inside
                 the LookupDialog class to set and store f.e. size and position of the
                 Form.
        Notes:   This Method needs to be overridden in a SmartLookup derived class to
                 return a valid registry key name.
        @return The Lookup Registry Key
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetLookupRegistryKey ():

        RETURN "":U.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Activates the Lookup Timer Component
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID StartTimer ():

        IF THIS-OBJECT:LookupTimerDisabled = TRUE THEN
            RETURN .

        IF THIS-OBJECT:LookupTimerDelay > 0 THEN DO:
            THIS-OBJECT:LookupTimer:Interval = THIS-OBJECT:LookupTimerDelay .
            THIS-OBJECT:LookupTimer:Enabled = FALSE .
            THIS-OBJECT:LookupTimer:Enabled = TRUE .
            THIS-OBJECT:LookupTimer:Start () .
        END.
        ELSE
            THIS-OBJECT:PerformLookup (FALSE) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Deactivates the Lookup Timer Component
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID StopTimer ():

        ASSIGN lPendingLookup = FALSE .

        IF NOT VALID-OBJECT (oLookupTimer) THEN
            RETURN .

        THIS-OBJECT:LookupTimer:Enabled = FALSE .
        THIS-OBJECT:LookupTimer:Stop () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Updates cells in a Grid with values from embedded editors in case
                 the lookup is owned by a grid
        Notes:   This method requires that the embedded editors are data bound to binding
                 source columns
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID UpdateCellsWithEmbeddedEditors ():

        DEFINE VARIABLE oControl      AS System.Windows.Forms.Control      NO-UNDO .
        DEFINE VARIABLE oParent       AS System.Windows.Forms.Control      NO-UNDO .
        DEFINE VARIABLE i             AS INTEGER                           NO-UNDO .
        DEFINE VARIABLE cColumn       AS CHARACTER                         NO-UNDO .

        oParent = THIS-OBJECT:Parent .

        ParentLookup: DO WHILE VALID-OBJECT(oParent):
            IF TYPE-OF (oParent, SmartViewerControl) THEN
                LEAVE ParentLookup .

            oParent = oParent:Parent .
        END.

        IF VALID-OBJECT(oParent) THEN
            DO i = 1 TO NUM-ENTRIES (THIS-OBJECT:LookupControls):
                ASSIGN oControl = CAST(oParent, SmartViewerControl):GetViewerField (ENTRY(i, THIS-OBJECT:LookupControls)) .

                IF VALID-OBJECT(oControl) THEN DO:

                    ASSIGN cColumn = BindingSourceHelper:BindingFieldName (oControl) .

                    IF cColumn > "":U AND THIS-OBJECT:OwningGrid:DisplayLayout:Bands[0]:Columns:Exists (cColumn) THEN DO:

                        IF TYPE-OF (oControl, UltraCheckEditor) THEN
                            THIS-OBJECT:AssignValueToCell (THIS-OBJECT:OwningGrid:ActiveRow:Cells [cColumn],
                                                           STRING ( CAST(oControl, UltraCheckEditor):CheckedValue)) .
                        ELSE
                            THIS-OBJECT:AssignValueToCell (THIS-OBJECT:OwningGrid:ActiveRow:Cells [cColumn], oControl:Text) .

                    END.
                END.
            END.
        ELSE
            DO i = 1 TO NUM-ENTRIES(THIS-OBJECT:LookupControls):
                IF THIS-OBJECT:Parent:Controls:ContainsKey(ENTRY(i, THIS-OBJECT:LookupControls)) THEN DO:
                    oControl = THIS-OBJECT:Parent:Controls[ENTRY(i, THIS-OBJECT:LookupControls)] .

                    ASSIGN cColumn = BindingSourceHelper:BindingFieldName (oControl) .

                    IF cColumn > "":U AND THIS-OBJECT:OwningGrid:DisplayLayout:Bands[0]:Columns:Exists (cColumn) THEN DO:

                        IF TYPE-OF (oControl, UltraCheckEditor) THEN
                            THIS-OBJECT:AssignValueToCell (THIS-OBJECT:OwningGrid:ActiveRow:Cells [cColumn],
                                                           STRING (CAST (oControl, UltraCheckEditor):CheckedValue)) .
                        ELSE
                            THIS-OBJECT:AssignValueToCell (THIS-OBJECT:OwningGrid:ActiveRow:Cells [cColumn], oControl:Text) .

                    END.
                END.
            END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SmartLookup class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartLookup ():

        IF VALID-OBJECT (oLookupTimer) THEN
            oLookupTimer:Tick:Unsubscribe (LookupTimerTick) NO-ERROR .

        GarbageCollectorHelper:DeleteObject (oLookupTimer) .

    END DESTRUCTOR .

END CLASS.
