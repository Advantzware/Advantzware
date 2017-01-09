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
    File        : SmartViewerControl
    Purpose     : Base class for SmartComponent Library Viewer Controls
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Jan 17 16:57:43 CET 2009
    Notes       : This class is meant to be ABSTRACT but the .NET Visual
                  Designer framework cannot design classes that inherit
                  from abstract base classes.
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{ Consultingwerk/products.i }

USING Consultingwerk.* .                              /* FROM PROPATH is not possible here, then the .Interfaces.Design would not be accessible */
USING Consultingwerk.Assertion.*                         FROM PROPATH .
USING Consultingwerk.Exceptions.*                        FROM PROPATH .
USING Consultingwerk.Framework.*                         FROM PROPATH .
USING Consultingwerk.Framework.Enum.*                    FROM PROPATH .
USING Consultingwerk.Framework.Authorization.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.*              FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*              FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.*    FROM PROPATH .
USING Consultingwerk.SmartComponents.Interfaces.* .   /* FROM PROPATH is not possible here, then the .Interfaces.Design would not be accessible */
USING Consultingwerk.SmartComponents.Interfaces.Design.* FROM ASSEMBLY .
USING Consultingwerk.SmartComponents.Resources.*         FROM PROPATH .
USING Consultingwerk.Util.*                              FROM PROPATH .
USING Consultingwerk.Util.Forms.*                        FROM PROPATH .
USING Consultingwerk.Windows.Util.Forms.*                FROM PROPATH .
USING Infragistics.Win.Misc.*                            FROM ASSEMBLY .
USING Infragistics.Win.UltraWinEditors.*                 FROM ASSEMBLY .
USING Infragistics.Win.UltraWinTabControl.*              FROM ASSEMBLY .
USING Progress.Lang.*                                    FROM PROPATH .
USING Progress.Util.*                                    FROM ASSEMBLY .
USING System.ComponentModel.*                            FROM ASSEMBLY .
USING System.ComponentModel.Design.*                     FROM ASSEMBLY .
USING System.Windows.Forms.*                             FROM ASSEMBLY .

CLASS Consultingwerk.SmartComponents.Base.SmartViewerControl
    INHERITS SmartUserControl
    IMPLEMENTS ISmartTableIOTarget, ISmartDataTarget, ISmartGroupAssignSource,
               ISmartGroupAssignTarget, ICheckOkToClose,
               Consultingwerk.SmartComponents.Interfaces.ISmartViewerControl,
               Consultingwerk.SmartComponents.Interfaces.Design.IDesignGroupAssignSource,
               Consultingwerk.SmartComponents.Interfaces.Design.IDesignDataTarget,
               Consultingwerk.SmartComponents.Interfaces.Design.IDesignViewerControl,
               ISupportInitialize, ISaveChangesOnDeactivateForm:

    DEFINE         VARIABLE oSmartGroupAssignSource AS ISmartGroupAssignSource     NO-UNDO .
    DEFINE PRIVATE VARIABLE oSmartDataSource        AS ISmartDataSource            NO-UNDO .
    DEFINE PRIVATE VARIABLE oSmartTableIOSource     AS ISmartTableIOSource         NO-UNDO .
    DEFINE PRIVATE VARIABLE cSmartTableIOState      AS CHARACTER                   NO-UNDO .
    DEFINE PRIVATE VARIABLE lEventsSubscribed       AS LOGICAL                     INITIAL FALSE NO-UNDO .
    DEFINE PRIVATE VARIABLE oEditorDropDownControl  AS Control                     NO-UNDO .
    DEFINE PRIVATE VARIABLE oBindingSource          AS Progress.Data.BindingSource NO-UNDO .

    DEFINE VARIABLE lDesignBindingSourceDisposed AS LOGICAL                        NO-UNDO INITIAL FALSE .
    DEFINE VARIABLE oDesignBindingSource         AS Progress.Data.BindingSource    NO-UNDO .

    DEFINE VARIABLE lInCreateRecord AS LOGICAL NO-UNDO INITIAL FALSE .

    DEFINE PRIVATE STATIC TEMP-TABLE ttSmartGroupAssignTarget NO-UNDO
        FIELD RecordOwner            AS Progress.Lang.Object
        FIELD SmartGroupAssignTarget AS Progress.Lang.Object
        INDEX RecordOwner RecordOwner SmartGroupAssignTarget
        .

    DEFINE PRIVATE STATIC TEMP-TABLE ttEnableFields NO-UNDO
        FIELD RecordOwner        AS Progress.Lang.Object
        FIELD ChildControl       AS Progress.Lang.Object
        FIELD EnableMode         AS CHARACTER            /* ADD, UPDATE, NONE, ALWAYS */
        FIELD SecurityEnableMode AS CHARACTER            /* Influenced by IFieldSecurityProvider */
        INDEX RecordOwner RecordOwner EnableMode
        INDEX ChildControl IS UNIQUE ChildControl
        .

    DEFINE PRIVATE STATIC TEMP-TABLE ttSavedViewerValues NO-UNDO
        FIELD RecordOwner   AS Progress.Lang.Object
        FIELD ChildControl  AS Progress.Lang.Object
        FIELD PropertyName  AS CHARACTER
        FIELD PropertyValue AS Progress.Lang.Object
        INDEX RecordOwner RecordOwner ChildControl PropertyName
        .

    DEFINE PRIVATE STATIC TEMP-TABLE ttTabOrder NO-UNDO
        FIELD RecordOwner   AS Progress.Lang.Object
        FIELD ChildControl  AS Progress.Lang.Object
        FIELD TabOrder      AS INTEGER
        INDEX RecordOwner RecordOwner TabOrder .

    DEFINE PRIVATE VARIABLE oSmartTableIOSources AS Consultingwerk.Framework.Base.List NO-UNDO .

    DEFINE VARIABLE oTextChangedHandler    AS "System.Collections.Generic.List<System.Windows.Forms.Control>":U NO-UNDO .
    DEFINE VARIABLE oCheckChangedMSHandler AS "System.Collections.Generic.List<System.Windows.Forms.CheckBox>":U NO-UNDO .
    DEFINE VARIABLE oCheckChangedIGHandler AS "System.Collections.Generic.List<Infragistics.Win.UltraWinEditors.UltraCheckEditor>":U NO-UNDO .

    DEFINE VARIABLE oFieldAuthorizationProvider AS IFieldAuthorizationProvider NO-UNDO .

    {Consultingwerk/SmartComponents/Base/ttViewerBoundControls.i &REFERENCE-ONLY=REFERENCE-ONLY}

    /**
     * Purpose: The Constructor initializes a new SmartViewerControl
     *          Subscribes handlers for events
     * Notes:
     */
    CONSTRUCTOR PUBLIC SmartViewerControl ():
        SUPER ().

        oSmartTableIOSources = NEW Consultingwerk.Framework.Base.List () .

        THIS-OBJECT:ParentChanged:Subscribe (ParentChangedHandler) .
        THIS-OBJECT:Leave:Subscribe (LeaveHandler) .

        oTextChangedHandler    = NEW "System.Collections.Generic.List<System.Windows.Forms.Control>":U () .
        oCheckChangedMSHandler = NEW "System.Collections.Generic.List<System.Windows.Forms.CheckBox>":U () .
        oCheckChangedIGHandler = NEW "System.Collections.Generic.List<Infragistics.Win.UltraWinEditors.UltraCheckEditor>":U () .

        THIS-OBJECT:SmartGroupAssignTargets = NEW ListISmartGroupAssignTarget () .

        oFieldAuthorizationProvider = {Consultingwerk/get-service.i Consultingwerk.Framework.Authorization.IFieldAuthorizationProvider} .

        THIS-OBJECT:OptionalFeatures = NEW SmartViewerOptionalFeatures () .

    END CONSTRUCTOR.

    /**
     * Purpose: Raised when the value of the AddedRecordModified property changes
     * Notes:
     */
    DEFINE PUBLIC EVENT AddedRecordModifiedChanged DELEGATE System.EventHandler .

    /**
     * Purpose: Event fired when the AddingRecord Property is changed
     * Notes:
     */
    DEFINE PUBLIC EVENT AddingRecordChanged DELEGATE System.EventHandler.

    /**
     * Purpose: Event fired when the viewer has successfully started an Add
     *          operation
     * Notes:   This event is defined using a .NET delegate, so it will show in
     *          the PropertyGrid of the VisualDesigner
     */
    DEFINE PUBLIC EVENT AfterAddRecord  DELEGATE System.EventHandler.

    /**
     * Purpose: Event fired when the viewer has successfully cancelled an update
     * Notes:   This event is defined using a .NET delegate, so it will show in
     *          the PropertyGrid of the VisualDesigner
     */
    DEFINE PUBLIC EVENT AfterCancelUpdate DELEGATE System.EventHandler.

    /**
     * Purpose: Event fired when the viewer has successfully started a Copy operation
     * Notes:   This event is defined using a .NET delegate, so it will show in
     *          the PropertyGrid of the VisualDesigner
     */
    DEFINE PUBLIC EVENT AfterCopyRecord  DELEGATE System.EventHandler.

    /**
     * Purpose: Event fired when the viewer has successfully deleted a record.
     * Notes:   This event is defined using a .NET delegate, so it will show in
     *          the PropertyGrid of the VisualDesigner
     */
    DEFINE PUBLIC EVENT AfterDeleteRecord  DELEGATE System.EventHandler.

    /**
     * Purpose: Raised after the fields have been enabled
     * Notes:
     */
    DEFINE PUBLIC EVENT AfterFieldsEnabled DELEGATE System.EventHandler .

    /**
     * Purpose: Event fired when the viewer has successfully saved changes
     * Notes:   This event is defined using a .NET delegate, so it will show in
     *          the PropertyGrid of the VisualDesigner
     */
    DEFINE PUBLIC EVENT AfterSaveChanges DELEGATE System.EventHandler.

    /**
     * Purpose: Event fired when the Viewer starts adding a new record.
     *          This event is cancelable
     * Notes:   This event is defined using a .NET delegate, so it will show in
     *          the PropertyGrid of the VisualDesigner
     */
    DEFINE PUBLIC EVENT BeforeAddRecord  DELEGATE System.ComponentModel.CancelEventHandler.

    /**
     * Purpose: Event fired when the Viewer begins cancelling an update.
     *          This event is cancellable
     * Notes:   This event is defined using a .NET delegate, so it will show in
     *          the PropertyGrid of the VisualDesigner
     */
    DEFINE PUBLIC EVENT BeforeCancelUpdate  DELEGATE System.ComponentModel.CancelEventHandler.

    /**
     * Purpose: Event fired when the Viewer begins copying a record.
     *          This event is cancellable
     * Notes:   This event is defined using a .NET delegate, so it will show in
     *          the PropertyGrid of the VisualDesigner
     */
    DEFINE PUBLIC EVENT BeforeCopyRecord  DELEGATE System.ComponentModel.CancelEventHandler.

    /**
     * Purpose: Event fired when the Viewer starts deleting a record.
     *          This event is cancellable
     * Notes:   This event is defined using a .NET delegate, so it will show in
     *          the PropertyGrid of the VisualDesigner
     */
    DEFINE PUBLIC EVENT BeforeDeleteRecord  DELEGATE System.ComponentModel.CancelEventHandler.

    /**
     * Purpose: Raised before the fields are enabled
     * Notes:
     * @param sender The object that raised the BeforeEnableFields event
     * @param e The CancelableEventArgs with the data for the event
     */
    DEFINE PUBLIC EVENT BeforeEnableFields SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                           e AS CancelableEventArgs).

    /**
     * Purpose: Event fired when the Viewer starts saving a record.
     *          This event is cancellable
     * Notes:   This event is defined using a .NET delegate, so it will show in
     *          the PropertyGrid of the VisualDesigner
     */
    DEFINE PUBLIC EVENT BeforeSaveChanges  DELEGATE System.ComponentModel.CancelEventHandler.

    /**
     * Purpose: Raised when the CopyingRecord property has changed
     * Notes:
     */
    DEFINE PUBLIC EVENT CopyingRecordChanged DELEGATE System.EventHandler .

    /**
     * Purpose: Event fired when the fields of this viewer become disabled
     * Notes:   This event is defined using a .NET delegate, so it will show in
     *          the PropertyGrid of the VisualDesigner
     */
    DEFINE PUBLIC EVENT FieldsDisabled DELEGATE System.EventHandler.

    /**
     * Purpose: Event fired when the fields of this viewer become enabled
     * Notes:   This event is defined using a .NET delegate, so it will show in
     *          the PropertyGrid of the VisualDesigner
     */
    DEFINE PUBLIC EVENT FieldsEnabled DELEGATE System.EventHandler.

    /**
     * Purpose: Event fired when the FocusControlOnAdd property changes
     * Notes:
     */
    DEFINE PUBLIC EVENT FocusControlOnAddChanged DELEGATE System.EventHandler.

    /**
     * Purpose: Event fired when the FocusControlOnUpdate property changes
     * Notes:
     */
    DEFINE PUBLIC EVENT FocusControlOnUpdateChanged DELEGATE System.EventHandler.

    /**
     * Purpose: Event fired when the SmartDataSource Property is changed
     * Notes:
     */
    DEFINE PUBLIC EVENT SmartDataSourceChanged DELEGATE System.EventHandler.

    /**
     * Purpose: Event fired when the SmartGroupAssignSource Property is changed
     * Notes:
     */
    DEFINE PUBLIC EVENT SmartGroupAssignSourceChanged DELEGATE System.EventHandler.

    /**
     * Purpose: Event fired when the SmartTableIOSource Property is changed
     * Notes:
     */
    DEFINE PUBLIC EVENT SmartTableIOSourceChanged DELEGATE System.EventHandler.

    /**
     * Purpose: Event fired whenever the SmartTableIOState property has changed
     * Notes:
     * @param sender The object that raised the event
     * @param e The SmartTableIOStateChangedEventArgs with the data for this event
     */
    DEFINE PUBLIC EVENT SmartTableIOStateChanged SIGNATURE VOID (sender AS System.Object,
                                                                 e AS SmartTableIOStateChangedEventArgs).

    /**
     * Purpose: Raised when the Viewer performs Visual Validation
     * Notes:   May be used by Subscribers to corrent screen values before validation
     */
    DEFINE PUBLIC EVENT VisualValidating DELEGATE System.EventHandler .

    /**
     * Purpose: Gets and sets if the current Grid row should be Activated (highlighted)
     *          when starting to update in the Grid
     * Notes:
     */
    DEFINE PUBLIC PROPERTY ActivateCurrentGridRowDuringEdit AS LOGICAL INITIAL TRUE NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Returns if a record being added was already modified by the user
     * Notes:
     */
    DEFINE PUBLIC PROPERTY AddedRecordModified AS LOGICAL INITIAL FALSE NO-UNDO
    GET.
    PROTECTED SET (arg AS LOGICAL):
        DEFINE VARIABLE lChanged AS LOGICAL NO-UNDO INITIAL FALSE .

        IF arg <> THIS-OBJECT:AddedRecordModified THEN
            lChanged = TRUE .

        THIS-OBJECT:AddedRecordModified = arg .

        IF lChanged THEN
           OnAddedRecordModifiedChanged (System.EventArgs:Empty) .
    END SET .

    /**
     * Purpose: Flag that indicates if we are currently in Add Mode.
     *          When CreateRecord () is called the value of the Property changes to
     *          TRUE. CancelUpdate ()or during initialization the call of
     *          EvaluateTableIOState () resets the value of the Property to FALSE.
     * Notes:   To Differenciate from Modifying data
     */
    DEFINE PUBLIC PROPERTY AddingRecord AS LOGICAL INITIAL FALSE NO-UNDO
    GET.
    PROTECTED SET (arg AS LOGICAL):
        DEFINE VARIABLE lChanged    AS LOGICAL                 NO-UNDO INITIAL FALSE .

        IF arg <> THIS-OBJECT:AddingRecord THEN
            ASSIGN lChanged = TRUE .

        THIS-OBJECT:AddingRecord = arg .

        IF lChanged THEN
            OnAddingRecordChanged (System.EventArgs:Empty) .
    END.

    /**
     * Purpose: Controls if a Form containing this SmartViewerControl instance can
     *          be closed without a prompt to the user when a record has just been
     *          created but the user has not yet changed a single field
     * Notes:
     */
    DEFINE PUBLIC PROPERTY AllowCloseOfUnmodifiedNewRecord AS LOGICAL INITIAL TRUE NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Returns if the Viewer is currently cancelling the update
     * Notes:   Useful in Leave event handlers of the fields contained in the
     *          SmartViewerControl
     */
    DEFINE PUBLIC PROPERTY CancellingUpdate AS LOGICAL INITIAL FALSE NO-UNDO
    GET.
    PRIVATE SET.

    /**
     * Purpose: Returns if the Viewer is currently copying a record
     * Notes:   Only true when AddingRecord is also true
     */
    DEFINE PUBLIC PROPERTY CopyingRecord AS LOGICAL INITIAL FALSE NO-UNDO
    GET .
    PROTECTED SET (arg AS LOGICAL):
        DEFINE VARIABLE lChanged AS LOGICAL NO-UNDO INITIAL FALSE .

        IF arg <> THIS-OBJECT:CopyingRecord THEN
            ASSIGN lChanged = TRUE .

        THIS-OBJECT:CopyingRecord = arg .

        IF lChanged THEN
            OnCopyingRecordChanged (System.EventArgs:Empty) .
    END.

    /**
     * Purpose: Returns if the EventHandlers that would enter Edit mode (TextChanged,
     *          CheckChanged) are activated or not
     * Notes:
     */
    DEFINE PUBLIC PROPERTY EventHandlerActive AS LOGICAL INITIAL FALSE NO-UNDO
    GET.
    PROTECTED SET.

    /**
     * Purpose: Get/Set the Control that will automatically become focussed on Add
     *          or Copy
     * Notes:   This property is filled automatically during initialization
     */
    DEFINE PUBLIC PROPERTY FocusControlOnAdd AS Control NO-UNDO
    GET.
    SET (arg AS Control):
        DEFINE VARIABLE lChanged    AS LOGICAL                 NO-UNDO INITIAL FALSE .

        IF NOT VALID-OBJECT (arg) AND NOT THIS-OBJECT:DesignTime THEN
            RETURN .

        IF arg <> THIS-OBJECT:FocusControlOnAdd THEN
            ASSIGN lChanged = TRUE .

        THIS-OBJECT:FocusControlOnAdd = arg .

        IF lChanged THEN
            OnFocusControlOnAddChanged (System.EventArgs:Empty) .

    END SET .

    /**
     * Purpose: Get/Set the Control that will automatically become focussed on Update
     * Notes:   This property is filled automatically during initialization
     */
    DEFINE PUBLIC PROPERTY FocusControlOnUpdate AS Control NO-UNDO
    GET.
    SET (arg AS Control):
        DEFINE VARIABLE lChanged    AS LOGICAL                 NO-UNDO INITIAL FALSE .

        IF NOT VALID-OBJECT (arg) AND NOT THIS-OBJECT:DesignTime THEN
            RETURN .

        IF arg <> THIS-OBJECT:FocusControlOnUpdate THEN
            ASSIGN lChanged = TRUE .

        THIS-OBJECT:FocusControlOnUpdate = arg .

        IF lChanged THEN
            OnFocusControlOnUpdateChanged (System.EventArgs:Empty) .

    END SET .

    /**
     * Purpose: Indicated that the SmartViewerControl is currrently being
     *          Initialized
     * Notes:
     */
    DEFINE PUBLIC PROPERTY Initializing AS LOGICAL NO-UNDO INITIAL FALSE
    GET.
    PRIVATE SET.

    /**
     * Purpose: Represents the SmartDataSource property casted to a .NET Interface
     * Notes:   Implementation of Interface in Consultingwerk.SmartComponents.dll
     *          SCL-725 - code moved from LinkDataSource.i directly into class file
     */
    DEFINE PUBLIC PROPERTY LinkDataSource AS Consultingwerk.SmartComponents.Interfaces.Design.IDesignDataSource NO-UNDO
    GET:
        IF TYPE-OF (THIS-OBJECT:SmartDataSource, Consultingwerk.SmartComponents.Interfaces.Design.IDesignDataSource) THEN
            RETURN CAST (THIS-OBJECT:SmartDataSource, Consultingwerk.SmartComponents.Interfaces.Design.IDesignDataSource) .
    END GET .
    SET (arg AS Consultingwerk.SmartComponents.Interfaces.Design.IDesignDataSource):
        IF NOT VALID-OBJECT (arg) OR TYPE-OF (arg, Consultingwerk.SmartComponents.Interfaces.ISmartDataSource) THEN
            ASSIGN THIS-OBJECT:SmartDataSource = CAST (arg, Consultingwerk.SmartComponents.Interfaces.ISmartDataSource) .
    END.

    /**
     * Purpose: Represents the SmartGroupAssignSource property casted to a .NET Interface
     * Notes:   Implementation of Interface in Consultingwerk.SmartComponents.dll
     *          SCL-725 - code moved from LinkGroupAssignSource.i directly into class file
     */
    DEFINE PUBLIC PROPERTY LinkGroupAssignSource AS Consultingwerk.SmartComponents.Interfaces.Design.IDesignGroupAssignSource NO-UNDO
    GET:
        IF TYPE-OF (THIS-OBJECT:SmartGroupAssignSource, Consultingwerk.SmartComponents.Interfaces.Design.IDesignGroupAssignSource) THEN
            RETURN CAST (THIS-OBJECT:SmartGroupAssignSource, Consultingwerk.SmartComponents.Interfaces.Design.IDesignGroupAssignSource) .
    END GET .
    SET (arg AS Consultingwerk.SmartComponents.Interfaces.Design.IDesignGroupAssignSource):
        IF NOT VALID-OBJECT (arg) OR TYPE-OF (arg, Consultingwerk.SmartComponents.Interfaces.ISmartGroupAssignSource) THEN
            ASSIGN THIS-OBJECT:SmartGroupAssignSource = CAST (arg, Consultingwerk.SmartComponents.Interfaces.ISmartGroupAssignSource) .
    END.

    /**
     * Purpose: Represents the SmartTableIOSource property casted to a .NET Interface
     * Notes:   Implementation of Interface in Consultingwerk.SmartComponents.dll
     *          SCL-725 - code moved from LinkTableIOSource.i directly into class file
     */
    DEFINE PUBLIC PROPERTY LinkTableIOSource AS Consultingwerk.SmartComponents.Interfaces.Design.IDesignTableIOSource NO-UNDO
    GET:
        IF TYPE-OF (THIS-OBJECT:SmartTableIOSource, Consultingwerk.SmartComponents.Interfaces.Design.IDesignTableIOSource) THEN
            RETURN CAST (THIS-OBJECT:SmartTableIOSource, Consultingwerk.SmartComponents.Interfaces.Design.IDesignTableIOSource) .
    END GET .
    SET (arg AS Consultingwerk.SmartComponents.Interfaces.Design.IDesignTableIOSource):
        IF NOT VALID-OBJECT (arg) OR TYPE-OF (arg, Consultingwerk.SmartComponents.Interfaces.ISmartTableIOSource) THEN
            ASSIGN THIS-OBJECT:SmartTableIOSource = CAST (arg, Consultingwerk.SmartComponents.Interfaces.ISmartTableIOSource) .
    END.

    /**
     *  Purpose: Reference to the SmartViewerOptionalFeaturesInstance that allows
     *           to toggle on or off optional features of the SmartViewerControl
     *  Notes:
     */
    DEFINE PROTECTED PROPERTY OptionalFeatures AS SmartViewerOptionalFeatures NO-UNDO
    GET.
    PRIVATE SET.

    /**
     * Purpose: Gets and sets if Infragistics Text Editor Controls should be set
     *          ReadOnly instead of Enabled = FALSE when update is not active
     * Notes:   Effective for Controls inheriting from Infragistics.Win.UltraWinEditors.UltraWinEditorMaskedControlBase
     *          and  Infragistics.Win.UltraWinEditors.TextEditorControlBase
     */
    DEFINE PUBLIC PROPERTY ReadOnlyDisabled AS LOGICAL INITIAL FALSE NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Controls if the Viewer's field remain Enabled after Save or Cancel
     * Notes:   When the user is using the SaveMode the fields are basically always
     *          enabled as long as there is a record available. As soon as the user
     *          starts entering values in the fields of the viewer, the update
     *          operation is starting, resulting in a change of enabled buttons in the
     *          viewer and the disablement of linked grids.
     */
    DEFINE PUBLIC PROPERTY SaveMode AS LOGICAL NO-UNDO INITIAL FALSE
    GET.
    SET.

    /**
     * Purpose: Returns if the Viewer is currently saving the changes
     * Notes:   Useful in Leave event handlers of the fields contained in the
     *          SmartViewerControl
     */
    DEFINE PUBLIC PROPERTY SavingChanges AS LOGICAL INITIAL FALSE NO-UNDO
    GET.
    PRIVATE SET.

    /**
     * Purpose: Gets and sets the Toolbar Security Key
     * Notes:
     */
    DEFINE PUBLIC PROPERTY SecurityKey AS CHARACTER NO-UNDO INITIAL "":U
    GET.
    SET.

    /**
     * Purpose: Get/Set the SmartDataSource of the current Class. The SmartDataSource
     *          can be set to any class implementing the Interface ISmartDataSource.
     *          This Property is set by the developer when initializing a Control
     *          implementing the Interface ISmartDataTarget.
     *          Enforced by Interface ISmartDataTarget.
     * Notes:   If a new SmartDataSource is set the DataBinding of all fields is updated.
     */
    DEFINE PUBLIC PROPERTY SmartDataSource AS ISmartDataSource NO-UNDO
    GET:
        RETURN oSmartDataSource .
    END GET.
    SET (INPUT arg AS ISmartDataSource):
        DEFINE VARIABLE lChanged    AS LOGICAL                 NO-UNDO INITIAL FALSE .

        IF arg <> THIS-OBJECT:SmartDataSource THEN
            ASSIGN lChanged = TRUE .

        /* Mike Fechner, Consultingwerk Ltd. 27.12.2009
           During Initialization of the Container (Form), just keep the
           reference to the SmartDataSource, EndInit will set this property again
           to invoke binding etc. */
        IF THIS-OBJECT:Initializing THEN
            ASSIGN oSmartDataSource = arg .

        ELSE DO:
            IF VALID-OBJECT (arg) THEN
            DO:
                oSmartDataSource = arg.

                AttachBindingSource (arg:BindingSource).

                /* Register the Viewer (SmartDataTarget) with the SmartDataSource */
                oSmartDataSource:RegisterSmartDataTarget (THIS-OBJECT).
            END.
            ELSE
            DO:
                AttachBindingSource (?) .

                /* Deregister SmartDataTarget */
                IF VALID-OBJECT (oSmartDataSource) THEN
                    oSmartDataSource:DeregisterSmartDataTarget (THIS-OBJECT) .

                oSmartDataSource = ? .
            END.
        END.

        IF lChanged THEN
            OnSmartDataSourceChanged (System.EventArgs:Empty) .

        /* Mike Fechner, Consultingwerk Ltd. 23.01.2012
           When setting the SmartDataSource outside of the Containers InitializeComponent method
           we need to make sure we are updating the TableIOState */
        IF NOT THIS-OBJECT:Initializing THEN
            THIS-OBJECT:EvaluateTableIOState () .

    END SET.

    /**
     * Purpose: Activates the current row in the linked Grid
     * Notes:
     */
    METHOD PROTECTED VOID ActivateCurrentGridRow ():

        DEFINE VARIABLE oGrid AS ISmartDataBrowser NO-UNDO .

        IF NOT VALID-OBJECT (THIS-OBJECT:SmartDataSource) OR
           NOT TYPE-OF (THIS-OBJECT:SmartDataSource, SmartDataAdapter) THEN
            RETURN .

        oGrid = CAST (THIS-OBJECT:SmartDataSource, SmartDataAdapter):FindLinkedSmartDataBrowser () .

        IF VALID-OBJECT (oGrid) THEN
            oGrid:ActivateCurrentRow() .

    END METHOD.

    /**
     * Purpose: Activates the Event Handlers for the contained controls
     * Notes:
     */
    METHOD PUBLIC VOID ActivateEventHandler ():

        DEFINE VARIABLE oForm AS System.Windows.Forms.Form NO-UNDO .

        IF THIS-OBJECT:EventHandlerActive OR THIS-OBJECT:DesignTime THEN
            RETURN .

        {Consultingwerk/foreach.i Control oControl in oTextChangedHandler}
            oControl:TextChanged:Subscribe (TextChangedEventHandler) NO-ERROR .
        END.

        {Consultingwerk/foreach.i UltraCheckEditor oCheckEditor in oCheckChangedIGHandler}
            oCheckEditor:CheckedChanged:Subscribe (TextChangedEventHandler) NO-ERROR .
        END.

        {Consultingwerk/foreach.i CheckBox oCheckbox in oCheckChangedMSHandler}
            oCheckBox:CheckedChanged:Subscribe (TextChangedEventHandler) NO-ERROR .
        END.

        oForm = THIS-OBJECT:FindForm () .

        IF VALID-OBJECT (oForm) THEN DO:
            oForm:Deactivate:Subscribe (FormDeactivatedHandler) NO-ERROR .
            oForm:FormClosed:Subscribe (FormClosedHandler) NO-ERROR .
        END.

        FINALLY:
            THIS-OBJECT:EventHandlerActive = TRUE .
        END FINALLY.

    END METHOD.

    /**
     * Purpose: Adds a(nother) SmartTableIOSource to the SmartViewerControl
     *          instance
     * Notes:   Enables multiple SmartTableIOSources to the SmartViewerControl
     * @param poSmartTableIOSource The SmartTableIOSource instance to add to the list of TableIO Sources of this Control
     */
    METHOD PUBLIC VOID AddSmartTableIOSource (poSmartTableIOSource AS ISmartTableIOSource):

        /* Mike Fechner, Consultingwerk Ltd. 22.12.2009
           Register as first SmartTableIOSource */
        IF NOT VALID-OBJECT (THIS-OBJECT:SmartTableIOSource) THEN DO:
            THIS-OBJECT:SmartTableIOSource = poSmartTableIOSource .
            RETURN .
        END.

        oSmartTableIOSources:Add (poSmartTableIOSource) .

        poSmartTableIOSource:RegisterSmartTableIOTarget (THIS-OBJECT) .

    END METHOD.

    /**
     * Purpose: Set's focus into a field that is related to an error
     * Notes:   Proteced method, so behaviour can be customized
     *          Returns the focussed Control, so that visual hints may be added
     * @param pcFieldName The name of the field to apply Focus to
     * @return The Control that the focus was applied to
     */
    METHOD PROTECTED System.Windows.Forms.Control ApplyFocusToErrorField (pcFieldName AS CHARACTER):

        DEFINE VARIABLE oControl AS System.Windows.Forms.Control NO-UNDO .
        DEFINE VARIABLE oTarget  AS ISmartGroupAssignTarget      NO-UNDO .

        IF pcFieldName > "":U THEN
        DO:
            oControl = RecurseLocateBindingControl (THIS-OBJECT, pcFieldName) .

            IF VALID-OBJECT(oControl) THEN DO:
                THIS-OBJECT:ControlFocus (oControl) .

                RETURN oControl .
            END.

            FOR EACH ttSmartGroupAssignTarget WHERE ttSmartGroupAssignTarget.RecordOwner = THIS-OBJECT:
                oTarget = CAST (ttSmartGroupAssignTarget.SmartGroupAssignTarget,
                                ISmartGroupAssignTarget) .

                oControl = oTarget:GetBoundControl (pcFieldName) .

                IF VALID-OBJECT (oControl) THEN DO:
                    oTarget:EnsureVisible () .
                    THIS-OBJECT:ControlFocus (oControl) .

                    RETURN oControl .
                END.
            END.

            RETURN ? .
        END.

        RETURN ? .

    END METHOD.

    /**
     * Purpose: Set's focus into a field that is related to an error
     * Notes:   Proteced method, so behaviour can be customized
     *          Returns the focussed Control, so that visual hints may be added
     * @param e The IFieldValidationError instance that contains the information about which Contrl/Field to focus
     * @return The Control that the focus was applied to
     */
    METHOD PROTECTED System.Windows.Forms.Control ApplyFocusToErrorField (e AS IFieldValidationError):

        DEFINE VARIABLE oControl AS System.Windows.Forms.Control NO-UNDO .

        IF VALID-OBJECT (e:FocusControl) THEN
        DO:
            THIS-OBJECT:ControlFocus (e:FocusControl) .

            RETURN e:FocusControl .
        END.
        ELSE IF e:FocusControlName > "":U THEN
        DO:
            oControl = RecurseLocateBindingControl (THIS-OBJECT, e:FocusControlName) .

            IF VALID-OBJECT(oControl) THEN
                THIS-OBJECT:ControlFocus (oControl) .

            RETURN oControl .
        END.

    END METHOD.

    /**
     * Purpose: This Method attaches a new BindingSource given as the Parameter
     *          poBindingSource to the Objects of the current Class. The new
     *          BindingSource may have been created by a DataAdapter or reused from a
     *          SmartBrowserControl
     * Notes:   Enforced by Interface ISmartDataTarget.
     *          It is called by the SmartDataAdapter Class when creating a new
     *          BindingSource or when a new SmartDataSource is registered to the
     *          Class implementing the Interface ISmartDataTarget.
     *          This method is also called from the SmartViewerControl destructor (while
     *          a form is closed) to de-attach from the Binding Source. In this case
     *          the poBindingSource reference is passed in as the unknown value.
     * @param poBindingSource The BindingSource to be attached to the Controls contained in the Viewer
    */
    METHOD PUBLIC VOID AttachBindingSource (poBindingSource AS Progress.Data.BindingSource):

        /* Mike Fechner, Consultingwerk Ltd. 06.11.2010
           No binding source should also not allow the toobar buttons to be active */
        IF NOT VALID-OBJECT(poBindingSource) THEN DO:
            THIS-OBJECT:SmartTableIOState = TableIOStateEnum:NoDataSource .
            RETURN.
        END.

        IF THIS-OBJECT:DesignTime THEN
            RETURN .

        /* Replace databindings with bindings to the binding source of the ISmartDataSource */
        RecurseReplaceBindingSource (THIS-OBJECT, poBindingSource) .

        /* Mike Fechner, Consultingwerk Ltd. 27.12.2009
           After the first time an external BindingSource is attached, we do not need
           the design time bindingsource anymore */
        IF NOT lDesignBindingSourceDisposed AND VALID-OBJECT (oDesignBindingSource) THEN DO:
            oDesignBindingSource:Dispose() .

            IF VALID-OBJECT (oDesignBindingSource) THEN
                DELETE OBJECT oDesignBindingSource .

            ASSIGN lDesignBindingSourceDisposed = TRUE .
        END.

        EvaluateTableIOState () .

        IF VALID-OBJECT (oBindingSource) THEN
            oBindingSource:PositionChanged:Unsubscribe (BindingSourcePositionChanged) NO-ERROR .

        ASSIGN
            oBindingSource = poBindingSource .

        oBindingSource:PositionChanged:Subscribe (BindingSourcePositionChanged) .

    END METHOD.

    /**
     * Purpose: Returns the list of SmartGroupAssignTargets as a generic list
     * Notes:
     */
    DEFINE PUBLIC PROPERTY SmartGroupAssignTargets AS ListISmartGroupAssignTarget NO-UNDO
    GET.
    PRIVATE SET.

    /**
     * Purpose: Get and Set the current SmartTableIOSource of the SmartViewerControl.
     *          This Property is set by the developer when initializing a Control
     *          implementing the Interface ISmartDataTarget.
     *          Enforced by Interface ISmartDataTarget.
     * Notes:   Cannot be changed while the SmartViewerControl is in ModifyingData mode.
     */
    DEFINE PUBLIC PROPERTY SmartTableIOSource AS ISmartTableIOSource NO-UNDO
    GET:
        RETURN oSmartTableIOSource .
    END.
    SET (arg AS ISmartTableIOSource):
        DEFINE VARIABLE lChanged AS LOGICAL NO-UNDO INITIAL FALSE .

        IF arg <> THIS-OBJECT:SmartTableIOSource THEN
            ASSIGN lChanged = TRUE .

        IF VALID-OBJECT (arg) THEN
        DO:
            /* Only one of SmartTableIOSource and SmartGroupAssignSource can be set at a time. */
            IF VALID-OBJECT (THIS-OBJECT:SmartGroupAssignSource) THEN
                UNDO, THROW NEW AppError ("A SmartViewerControl can only have a SmartTableIOSource or a SmartGroupAssignSource"{&TRAN}, 0).

            /* When setting a SmartTableIOSource, check current SmartTableIOState first */
            IF VALID-OBJECT (oSmartTableIOSource) THEN
                UNDO, THROW NEW Progress.Lang.AppError ("Use AddSmartTableIOSource method to add another SmartTableUISource"{&TRAN}, 0) .

            /* Currently - by default, disable the fields.
               this will be controlled by a property later
               do this first, as a SmartTableIO state will be set to Disabled */
            THIS-OBJECT:DisableFields () .

            /* Register the Viewer (SmartTableIOTarget) with the SmartTableIOSource */
            oSmartTableIOSource = arg .

            AddSmartTableIOSource (arg) .
        END.
        ELSE
        DO:
            IF VALID-OBJECT (oSmartTableIOSource) THEN
                RemoveSmartTableIOSource (oSmartTableIOSource) .

            oSmartTableIOSource = ? .
        END.

        IF lChanged THEN
            OnSmartTableIOSourceChanged (System.EventArgs:Empty) .
    END.

    /**
     * Purpose: The SmartTableIOState property reflects the operationmode of the
     *          SmartViewerControl.
     *          Value changes of this Property are only made in the Class
     *          SmartViewerControl.
     *          Enforced by Interface ISmartDataTarget or ISmartGroupAssignTarget.
     * Notes:   Possible values (TableIOStateEnum):
     *          - AddCopyAllowed
     *          - FieldsDisabled
     *          - FieldsEnabled
     *          - ModifyingData
     *          - NoDataSource
     *          - NoParentRecordAvailable
     */
    DEFINE PUBLIC PROPERTY SmartTableIOState AS CHARACTER NO-UNDO
    GET:
        IF THIS-OBJECT:DesignTime THEN
            RETURN "":U .

        RETURN cSmartTableIOState .
    END GET.
    PUBLIC SET (arg AS CHARACTER):
        /* Mike Fechner, Consultingwerk Ltd. 17.11.2015
           SCL-1102 - allow empty SmartTableIOState in Visual Designer
           SCL-1112 - allow empty SamrtTableIOState between BeginInit() and EndInit() */
        IF arg = "":U AND (THIS-OBJECT:Initializing OR THIS-OBJECT:DesignTime) THEN
            RETURN .

        /* Allow empty string during initialization - due to OEA generating code
           to set the property, see above */
        IF NOT VALID-OBJECT(oSmartTableIOSource) AND arg = "":U THEN .
        ELSE

            IF NOT TableIOStateEnum:IsValid (arg) THEN
                UNDO, THROW NEW Progress.Lang.AppError
                    ("Possible values for SmartTableIOState are: AddCopyAllowed, FieldsDisabled, FieldsEnabled, ModifyingData, NoDataSource, NoRecordAvailable, NoParentRecordAvailable"{&TRAN}) .

        IF arg <> TableIOStateEnum:ModifyingData AND
            cSmartTableIOState = TableIOStateEnum:ModifyingData AND
            VALID-OBJECT(oSmartDataSource) THEN
            oSmartDataSource:SmartDataSourceState = DataSourceStateEnum:Navigatable .

        IF arg = cSmartTableIOState THEN
            RETURN .

        ASSIGN
            cSmartTableIOState = arg .

        IF arg <> "":U THEN
            OnSmartTableIOStateChanged (NEW SmartTableIOStateChangedEventArgs (arg)) .
    END.

    /**
     * Purpose: Property to register a SmartGroupAssignSource for the SmartViewerControl
     *          a backward handshake to the Source is done.
     *          A SmartGroupAssignSource can be every class implementing the Interface
     *          ISmartGroupAssignSource.
     *          This Property is set programatically by the developer.
     * Notes:
     */
    DEFINE PUBLIC PROPERTY SmartGroupAssignSource AS ISmartGroupAssignSource NO-UNDO
    GET:
        RETURN oSmartGroupAssignSource .
    END GET.
    SET (arg AS ISmartGroupAssignSource):
        DEFINE VARIABLE oTempSource AS ISmartGroupAssignSource NO-UNDO.
        DEFINE VARIABLE lChanged    AS LOGICAL                 NO-UNDO INITIAL FALSE .

        IF arg <> THIS-OBJECT:SmartGroupAssignSource THEN
            ASSIGN lChanged = TRUE .

        IF VALID-OBJECT (arg) THEN
        DO:
            /* Only one of SmartTableIOSource and SmartGroupAssignSource can be set at a time. */
            IF VALID-OBJECT(THIS-OBJECT:SmartTableIOSource) THEN
                UNDO, THROW NEW AppError ("A SmartViewerControl can only have a SmartTableIOSource or a SmartGroupAssignSource"{&TRAN}, 0).

            /* A Viewer can not be it's own GroupAssignSource */
            IF arg = THIS-OBJECT THEN
                UNDO, THROW NEW AppError ("A SmartViewerControl can not be it's own SmartGroupAssignSource!"{&TRAN}, 0).

            /* Detect loops 1 -> 2, 2 -> 3, !3 -> 1! */
            IF TYPE-OF (arg, ISmartGroupAssignTarget) AND
                VALID-OBJECT(CAST(arg, ISmartGroupAssignTarget):SmartGroupAssignSource) THEN
            DO:
                oTempSource = CAST(arg, ISmartGroupAssignTarget):SmartGroupAssignSource.

                DO WHILE VALID-OBJECT (oTempSource):
                    IF oTempSource = THIS-OBJECT THEN
                        UNDO, THROW NEW AppError ("Cyclic SmartGroupAssign link detected! "{&TRAN} +
                                                  CAST(CAST(arg, Progress.Lang.Object), System.Windows.Forms.Control):Name + " -> ":U +
                                                  THIS-OBJECT:Name, 0).

                    IF TYPE-OF (oTempSource, ISmartGroupAssignTarget) THEN
                        oTempSource = CAST(oTempSource, ISmartGroupAssignTarget):SmartGroupAssignSource.
                END.
            END.

            ASSIGN
                oSmartGroupAssignSource = arg .

            /* Currently - by default, disable the fields.
               this will be controlled by a property later
               do this first, as a SmartTableIO state will be set to Disabled */
            THIS-OBJECT:DisableFields() .

            arg:RegisterSmartGroupAssignTarget(THIS-OBJECT) .

            arg:SmartTableIOStateChanged:Subscribe(GroupAssignSmartTableIOStateChangedHandler) .
        END.
        ELSE
        DO:
            IF VALID-OBJECT (oSmartGroupAssignSource) THEN DO:
                oSmartGroupAssignSource:DeregisterSmartGroupAssignTarget (THIS-OBJECT) .

                oSmartGroupAssignSource:SmartTableIOStateChanged:Unsubscribe(GroupAssignSmartTableIOStateChangedHandler) .
            END.

            oSmartGroupAssignSource = ? .

            THIS-OBJECT:DisableFields() .
        END.

        IF lChanged THEN
            OnSmartGroupAssignSourceChanged (System.EventArgs:Empty) .

    END.

    /**
     * Purpose: Gets and sets if text TextChangedHandler is (temporarily) suspended
     * Notes:   The TextChangedHandler controls setting the SmartTableIOState of the
     *          SmartViewerControl to modifying data. There may be situations in
     *          custom control developement where this might need to be turned off
     *          temporarily. Setting this flag to TRUE is less work, than calling
     *          the DeactivateEventHandler and ActivateEventHandler
     */
    DEFINE PROTECTED PROPERTY TextChangedHandlerSuspended AS LOGICAL INITIAL FALSE NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Returns the Update State of the Viewer
     * Notes:   Possible values are Add, Copy, Update or View
     */
    DEFINE PUBLIC PROPERTY UpdateState AS UpdateStateEnum NO-UNDO
    GET:
        IF THIS-OBJECT:SmartTableIOState = TableIOStateEnum:ModifyingData THEN DO:

           IF THIS-OBJECT:AddingRecord = TRUE THEN DO:
               IF THIS-OBJECT:CopyingRecord THEN
                   RETURN UpdateStateEnum:Copy .
               ELSE
                   RETURN UpdateStateEnum:Add .
           END.
           ELSE
               RETURN UpdateStateEnum:Update .
        END.
        ELSE
            RETURN UpdateStateEnum:View .
    END .

    /**
     * Purpose: Gets and sets the class name of the ViewerLogic class
     * Notes:   This property should only be set for independent SmartViewerControl
     *          instances of the source of a group assign link. The value of the
     *          property will be ignored for group assign targets
     *          The class named here, needs to implement the
     *          Consultingwerk.SmartComponents.Implementation.ISmartViewerLogic
     *          interface
     */
    DEFINE PUBLIC PROPERTY ViewerLogicClass AS CHARACTER NO-UNDO INITIAL "":U
    GET.
    SET.

    /**
     * Purpose: Returns the reference to the ISmartViewerLogic instance that provides
     *          logic for this instance
     * Notes:
     */
    DEFINE PUBLIC PROPERTY ViewerLogicObject AS ISmartViewerLogic NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Add a new record to the SmartDataSource. The Method CreateRecord of
     *          SmartViewerControl gets called to Handle this Task.
     *          This Method is called by the Toolbar Classes SmartToolbarController
     *          or SmartToolStripContainer by pressing the AddRecord Tool.
     *          Enforced by Interface ISmartDataTarget.
     * Notes:
     */
    METHOD PUBLIC VOID AddRecord ():

        DEFINE VARIABLE e AS System.ComponentModel.CancelEventArgs NO-UNDO .
        e = NEW System.ComponentModel.CancelEventArgs () .

        OnBeforeAddRecord (e) .

        IF VALID-OBJECT (e) AND e:Cancel THEN
            RETURN .

        THIS-OBJECT:EnsureVisible () .

        THIS-OBJECT:CreateRecord (FALSE) . /* FALSE = Add new record */

        OnAfterAddRecord (System.EventArgs:Empty) .

    END METHOD.

    /**
     * Purpose: Invoked at the beginning of a Containers InitializeComponents
     *          method
     * Notes:   Enforced by System.ComponentModel.ISupportInitialize
     *          See http://msdn.microsoft.com/de-de/library/system.componentmodel.isupportinitialize.begininit(VS.80).aspx
     */
    METHOD PUBLIC VOID BeginInit ():

        ASSIGN THIS-OBJECT:Initializing = TRUE  .

    END METHOD.

    /**
     * Purpose: Event handler for the PositionChanged event of the BindingSource
     *          this Viewer is attached to
     * Notes:   Cancels the pending timer of all Lookups contained on the Viewer
     * @param sender The reference to the object that raised the event
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PROTECTED VOID BindingSourcePositionChanged (sender AS System.Object,
                                                        e AS System.EventArgs):

        RecurseLookupStopTimer (THIS-OBJECT) .

    END METHOD.

    /**
     * Purpose: Reverts the changes to the current record.
     * Notes:   This Method is called by the Toolbar Classes SmartToolbarController
     *          or SmartToolStripContainer by pressing the corresponding Tool.
     *          Enforced by Interface ISmartDataTarget.
     */
    METHOD PUBLIC VOID CancelUpdate ():

        DEFINE VARIABLE e AS System.ComponentModel.CancelEventArgs NO-UNDO .

        THIS-OBJECT:CancellingUpdate = TRUE .

        /* Stop pending Lookup Timers */
        RecurseLookupStopTimer (THIS-OBJECT) .

        e = NEW System.ComponentModel.CancelEventArgs () .
        OnBeforeCancelUpdate (e) .

        IF VALID-OBJECT (e) AND e:Cancel THEN
            RETURN .

        /* Marko Rüterbories, Consultingwerk Ltd. 17.06.2016
           SCL-1359 SmartViewerControl: Canceling a CancelUpdate may not reset the
                    AddingRecord and CopyingRecord properties */
        DO ON ERROR UNDO, THROW:

            /* Cancel pending Add operation */
            IF THIS-OBJECT:AddingRecord = TRUE THEN
                oSmartDataSource:CancelCreateRecord () .
            ELSE
            /* Mike Fechner, Consultingwerk Ltd. 24.10.2011
               Check for valid BindingSource first */
               IF VALID-OBJECT (oSmartDataSource) AND VALID-OBJECT (oSmartDataSource:BindingSource) THEN
                    oSmartDataSource:BindingSource:Refresh () .

            /* Fields will only be disabled depending on the SaveMode only */
            IF THIS-OBJECT:SaveMode THEN
                THIS-OBJECT:EnableFields (Consultingwerk.SmartComponents.Enum.EnableFieldsEnum:Update) .
            ELSE
                THIS-OBJECT:DisableFields() .

            OnAfterCancelUpdate (System.EventArgs:Empty) .

            FINALLY:
                /* No matter if we have been in Add mode or not, CancelUpdate is able
                   to set AddingRecord to FALSE */
                ASSIGN THIS-OBJECT:AddingRecord  = FALSE
                       THIS-OBJECT:CopyingRecord = FALSE .

                THIS-OBJECT:EvaluateTableIOState () .
            END FINALLY.

        END.

        FINALLY:
            THIS-OBJECT:CancellingUpdate = FALSE .
        END FINALLY.

    END METHOD.

    /**
     * Purpose: Calls the Focus() method for a .NET Control
     * Notes:   SCL-659 : A customer was setting the SET-WAIT-STATE("GENERAL") using
     *          the WaitStateManager around all TableIO Operations - with the consequence,
     *          that all Focus() operations on
     *          - error message putting focus to error field
     *          - FocusControlOnAdd, etc.
     *          were not working anymore. Seems that .NET Controls ignore the Focus()
     *          during the WAIT-STATE. In order to workaround those situation we have
     *          moved all calls to the Focus() method of Viewer Controls to the
     *          overriable method ControlFocus() that receives the Control reference
     *          as the input parameter. This overridable method can be extended by
     *          customers to make sure no WAIT-STATE is active anymore before the
     *          Control:Focus() is called.
     * @param poControl The reference to the .NET Control that should receive Focus
     * @return Logical value indicating if the input focus request was successful
     */
    METHOD PROTECTED LOGICAL ControlFocus (poControl AS System.Windows.Forms.Control):

        IF VALID-OBJECT (poControl) THEN
            RETURN poControl:Focus () .

        RETURN FALSE .

    END METHOD.

    /**
     * Purpose: Copy the current record to a new one. The Method CreateRecord of
     *          SmartViewerControl gets called to Handle this Task.
     *          This Method is called by the Toolbar Classes SmartToolbarController
     *          or SmartToolStripContainer by pressing the corresponding Tool.
     *          Enforced by Interface ISmartDataTarget.
     * Notes:
     */
    METHOD PUBLIC VOID CopyRecord ():

        DEFINE VARIABLE e AS System.ComponentModel.CancelEventArgs NO-UNDO .

        e = NEW System.ComponentModel.CancelEventArgs () .

        OnBeforeCopyRecord (e) .

        IF VALID-OBJECT (e) AND e:Cancel THEN
            RETURN .

        IF THIS-OBJECT:ActivateCurrentGridRowDuringEdit THEN
            THIS-OBJECT:ActivateCurrentGridRow() .

        ASSIGN THIS-OBJECT:CopyingRecord = TRUE .

        THIS-OBJECT:EnsureVisible () .

        THIS-OBJECT:CreateRecord (TRUE) . /* TRUE = Copy existing record*/

        OnAfterCopyRecord (System.EventArgs:Empty) .

    END METHOD.

    /**
     * Purpose: Creates a new record in the viewer.
     *          This Method is called by AddRecord and CopyRecord from the current
     *          Class but may be overridden.
     * Notes:
     * @param plCopy Logical value indicating if the method has been called from AddRecord or CopyRecord, FALSE = Add new record, TRUE = Copy existing record
     */
    METHOD PROTECTED VOID CreateRecord (plCopy AS LOGICAL):

        DEFINE VARIABLE cSourceRecordKey AS CHARACTER NO-UNDO INITIAL ? .

        ASSIGN lInCreateRecord = TRUE .

        /* The SmartDataSource needs to create a new row and reposition the
           BindingSource - this need the be done using event handlers in the
           binding source */

        IF plCopy THEN DO:
            StoreValuesForCopy () .

            /* Mike Fechner, Consultingwerk Ltd. 16.12.2013
               Store the RecordKey of the current record, so that we can
               store this in the new record */
            IF TYPE-OF (THIS-OBJECT:SmartDataSource, IProvidesRecordKey) AND
               TYPE-OF (THIS-OBJECT:SmartDataSource, IStoresCopiedFrom) AND
               CAST (THIS-OBJECT:SmartDataSource, IStoresCopiedFrom):HasCopiedFromField () THEN

                ASSIGN cSourceRecordKey = CAST (THIS-OBJECT:SmartDataSource, IProvidesRecordKey):GetCurrentRecordKey () .
        END .


        oSmartDataSource:CreateRecord () .

        THIS-OBJECT:EnableFields (EnableFieldsEnum:Add) .

        THIS-OBJECT:AddingRecord = TRUE .
        THIS-OBJECT:AddedRecordModified = FALSE .

        THIS-OBJECT:SmartTableIOState = TableIOStateEnum:ModifyingData .

        /* Tell the SmartDataSource that the user has started to update data. */
        IF VALID-OBJECT(oSmartDataSource) THEN
            oSmartDataSource:BeginUpdateState (THIS-OBJECT).


        IF plCopy THEN DO:
            RestoreValuesForCopy () .

            IF cSourceRecordKey <> ? AND
               TYPE-OF (THIS-OBJECT:SmartDataSource, IStoresCopiedFrom) THEN

               CAST (THIS-OBJECT:SmartDataSource, IStoresCopiedFrom):StoreCopiedFrom (cSourceRecordKey) .
        END.

        CATCH cancelex AS Consultingwerk.Exceptions.CancelException:
            /* When there's a message with a Cancel Exception, show it */
            IF cancelex:NumMessages > 0 THEN
                ErrorHelper:ShowErrorMessage (cancelex,
                                              SmartComponentLibraryCustomizer:SmartViewerControl_UnableToCreateRecord) .

            /* Mike Fechner, Consultingwerk Ltd. 16.02.2016
               SCL-1205 When SaveMode = TRUE, get back into Update mode */
            IF THIS-OBJECT:SaveMode AND THIS-OBJECT:SmartDataSource:Available THEN
                THIS-OBJECT:EnableFields (EnableFieldsEnum:Update) .
            ELSE
                THIS-OBJECT:DisableFields () .

            THIS-OBJECT:AddingRecord = FALSE .
            THIS-OBJECT:AddedRecordModified = FALSE .
        END CATCH.

        CATCH e AS Progress.Lang.Error :
            ErrorHelper:ShowErrorMessage (e, SmartComponentLibraryCustomizer:SmartViewerControl_UnableToCreateRecord) .

            /* Mike Fechner, Consultingwerk Ltd. 16.02.2016
               SCL-1205 When SaveMode = TRUE, get back into Update mode */
            IF THIS-OBJECT:SaveMode AND THIS-OBJECT:SmartDataSource:Available THEN
                THIS-OBJECT:EnableFields (EnableFieldsEnum:Update) .
            ELSE
                THIS-OBJECT:DisableFields () .

            THIS-OBJECT:AddingRecord = FALSE .
            THIS-OBJECT:AddedRecordModified = FALSE .
        END CATCH.

        FINALLY:
            ASSIGN lInCreateRecord = FALSE .

            /* Mike Fechner, Consultingwerk Ltd. 29.08.2011
               For proparse, we need an intermediate variable here - otherwise this would
               cause parsing errors. */
            DEFINE VARIABLE oThisObject AS Progress.Lang.Object NO-UNDO .
            oThisObject = THIS-OBJECT .

            FOR EACH ttSavedViewerValues WHERE ttSavedViewerValues.RecordOwner = oThisObject :
                DELETE ttSavedViewerValues.
            END.
        END FINALLY.

    END METHOD.

    /**
     * Purpose: Overridable method that can be used to perform custom event
     *          subscriptions
     * Notes:   Empty method, needs to be overridden in a custom viewer class
     * @param poControl The reference to the Control
     */
    METHOD PROTECTED VOID CustomInitializeControlHandler (poControl AS Control):

        /* noop */

    END METHOD .

    /**
     * Purpose: Deactivates the Event Handlers for the contained controls
     * Notes:
     */
    METHOD PUBLIC VOID DeactivateEventHandler ():

        DEFINE VARIABLE oForm AS System.Windows.Forms.Form NO-UNDO .

        IF THIS-OBJECT:EventHandlerActive = FALSE OR THIS-OBJECT:DesignTime THEN
            RETURN .

        {Consultingwerk/foreach.i Control oControl in oTextChangedHandler}
            oControl:TextChanged:Unsubscribe (TextChangedEventHandler) NO-ERROR .
        END.

        {Consultingwerk/foreach.i UltraCheckEditor oCheckEditor in oCheckChangedIGHandler}
            oCheckEditor:CheckedChanged:Unsubscribe (TextChangedEventHandler) NO-ERROR .
        END.

        {Consultingwerk/foreach.i CheckBox oCheckbox in oCheckChangedMSHandler}
            oCheckBox:CheckedChanged:Unsubscribe (TextChangedEventHandler) NO-ERROR .
        END.

        oForm = THIS-OBJECT:FindForm () .

        IF VALID-OBJECT (oForm) THEN DO:
            oForm:Deactivate:Unsubscribe (FormDeactivatedHandler) NO-ERROR .
            oForm:FormClosed:Unsubscribe (FormClosedHandler) NO-ERROR .
        END.

        FINALLY:
            THIS-OBJECT:EventHandlerActive = FALSE .
        END FINALLY.

    END METHOD.

    /**
     * Purpose: Delete the current record.
     * Notes:   The Method DeleteRow of the SmartDataSource is called to perform
     *          this Task. This Method is called by the Toolbar Classes SmartToolbarController
     *          or SmartToolStripContainer by pressing the corresponding Tool.
     *          Enforced by Interface ISmartDataTarget.
     */
    METHOD PUBLIC VOID DeleteRecord ():

        DEFINE VARIABLE e             AS System.ComponentModel.CancelEventArgs NO-UNDO .
        DEFINE VARIABLE oDialogResult AS DialogResultEnum                      NO-UNDO .
        DEFINE VARIABLE cTitle        AS CHARACTER                             NO-UNDO .

        THIS-OBJECT:EnsureVisible () .

        IF THIS-OBJECT:ActivateCurrentGridRowDuringEdit THEN
            THIS-OBJECT:ActivateCurrentGridRow() .

        IF VALID-OBJECT(oSmartDataSource) THEN DO:
            e = NEW System.ComponentModel.CancelEventArgs () .

            OnBeforeDeleteRecord (e) .

            IF VALID-OBJECT (e) AND e:Cancel THEN
                RETURN .

            IF SmartComponentLibraryCustomizer:SmartViewerControl_DeletePrompt > "":U THEN DO:
                ASSIGN cTitle = THIS-OBJECT:FindForm():Text NO-ERROR .

                IF cTitle > "":U THEN .
                ELSE
                    ASSIGN cTitle = "":U .

                oDialogResult = MessageFormHelper:AskQuestion (SmartComponentLibraryCustomizer:SmartViewerControl_DeletePrompt,
                                                               cTitle) .
                IF oDialogResult <> DialogResultEnum:DialogResultYes THEN
                    RETURN .
            END.

            oSmartDataSource:DeleteRow() .

            OnAfterDeleteRecord (System.EventArgs:Empty) .
        END.

        CATCH err AS Progress.Lang.Error :
            DisplayError(err).

            /* If the Error implements the Interface IFieldValidationError
               Set the focus to the Field causing the error */
            IF TYPE-OF(err, IFieldValidationError) THEN
            DO:
                ApplyFocusToErrorField (CAST(err, IFieldValidationError)) .
            END.
        END CATCH.

        /* Mike Fechner, Consultingwerk Ltd. 06.12.2011
           Bug 2580: After deletion, when SaveMode = TRUE we need to evaluate the TableIOState */
        FINALLY:
            IF VALID-OBJECT (THIS-OBJECT:SmartDataSource) AND VALID-OBJECT (THIS-OBJECT:SmartDataSource:BindingSource) THEN
                IF THIS-OBJECT:SmartDataSource:BindingSource:Count > 0 THEN .
                ELSE DO:
                    IF THIS-OBJECT:SaveMode THEN
                        THIS-OBJECT:DisableFields() .

                        THIS-OBJECT:EvaluateTableIOState() .
            END.
        END FINALLY.
    END METHOD.

    /**
     * Purpose: Disable all enabled controls of the SmartViewerControl.
     *          This Method is called from inside the SmartViewerControl.
     *          Enforced by Interface ISmartDataTarget or ISmartGroupAssignTarget.
     * Notes:   Private version of Disable Fields that allows to disable fields (no
     *          record available and SaveMode = TRUE) without raising the event that
     *          the TableIOState was changed
     * @param plAssignTableIOState Logical value indicating if the SmartTableIOState should be
     */
    METHOD PRIVATE VOID DisableFields (plAssignTableIOState AS LOGICAL):

        IF plAssignTableIOState = TRUE THEN
            THIS-OBJECT:SmartTableIOState = TableIOStateEnum:FieldsDisabled .

        FOR EACH ttEnableFields WHERE (ttEnableFields.RecordOwner = THIS-OBJECT
                                   AND ttEnableFields.EnableMode  = ControlEnabledEnum:Add)
                                   OR (ttEnableFields.RecordOwner = THIS-OBJECT
                                   AND ttEnableFields.EnableMode  = ControlEnabledEnum:Update)
                                   OR (ttEnableFields.RecordOwner = THIS-OBJECT
                                   AND ttEnableFields.EnableMode  = ControlEnabledEnum:UpdateOnly):

            IF NOT VALID-OBJECT(ttEnableFields.ChildControl) THEN
                NEXT.

            /* Mike Fechner, Consultingwerk Ltd. 20.05.2009
               Bug 1787, Bug 1741: TYPE-OF validation in the form of
               IF TYPE-OF (ttEnableFields.ChildControl, Control) fails. I believe this
               is caused by a core issue and should be reviewed on a later release.
               Actually there is no strong need for the TYPE-OF validation, since the
               input signature to the method SetControlEnabled guarantees that we only
               receive references to System.Windows.Forms.Control childs. So the CASE
               is not expected to fail. */

            /*IF TYPE-OF(ttEnableFields.ChildControl, Control) THEN*/
            /* Mike Fechner, Consultingwerk Ltd. 02.09.2010
               Editors (= Multi-Line TextBoxes) should be set to read-only, not Disabled */
            IF TYPE-OF (ttEnableFields.ChildControl, Infragistics.Win.UltraWinEditors.UltraTextEditor) AND
                CAST (ttEnableFields.ChildControl, Infragistics.Win.UltraWinEditors.UltraTextEditor):MultiLine = TRUE THEN
                     CAST (ttEnableFields.ChildControl, Infragistics.Win.UltraWinEditors.UltraTextEditor):ReadOnly = TRUE .
            ELSE
            IF TYPE-OF (ttEnableFields.ChildControl, System.Windows.Forms.TextBox) AND
                CAST (ttEnableFields.ChildControl, System.Windows.Forms.TextBox):MultiLine = TRUE THEN
                     ASSIGN CAST (ttEnableFields.ChildControl, System.Windows.Forms.TextBox):ReadOnly = TRUE .
            ELSE IF THIS-OBJECT:ReadOnlyDisabled AND
                    TYPE-OF (ttEnableFields.ChildControl, Infragistics.Win.UltraWinEditors.UltraWinEditorMaskedControlBase) THEN
                 ASSIGN CAST (ttEnableFields.ChildControl, Infragistics.Win.UltraWinEditors.UltraWinEditorMaskedControlBase):ReadOnly = TRUE
                        CAST (ttEnableFields.ChildControl, Infragistics.Win.UltraWinEditors.UltraWinEditorMaskedControlBase):Enabled  = TRUE .
            ELSE IF THIS-OBJECT:ReadOnlyDisabled AND
                     TYPE-OF (ttEnableFields.ChildControl, Infragistics.Win.UltraWinEditors.UltraWinEditorMaskedControlBase) THEN
                 ASSIGN CAST (ttEnableFields.ChildControl, Infragistics.Win.UltraWinEditors.UltraWinEditorMaskedControlBase):ReadOnly = TRUE
                        CAST (ttEnableFields.ChildControl, Infragistics.Win.UltraWinEditors.UltraWinEditorMaskedControlBase):Enabled  = TRUE.
            ELSE IF THIS-OBJECT:ReadOnlyDisabled AND
                     TYPE-OF (ttEnableFields.ChildControl, Infragistics.Win.UltraWinGrid.UltraCombo) THEN
                 ASSIGN CAST (ttEnableFields.ChildControl, Infragistics.Win.UltraWinGrid.UltraCombo):ReadOnly = TRUE
                        CAST (ttEnableFields.ChildControl, Infragistics.Win.UltraWinGrid.UltraCombo):Enabled  = TRUE  .
            ELSE IF THIS-OBJECT:ReadOnlyDisabled AND
                    TYPE-OF (ttEnableFields.ChildControl, Infragistics.Win.UltraWinEditors.TextEditorControlBase) THEN
                 ASSIGN CAST (ttEnableFields.ChildControl, Infragistics.Win.UltraWinEditors.TextEditorControlBase):ReadOnly = TRUE
                        CAST (ttEnableFields.ChildControl, Infragistics.Win.UltraWinEditors.TextEditorControlBase):Enabled  = TRUE .
            ELSE
                CAST(ttEnableFields.ChildControl, Control):Enabled = FALSE  .
        END.

        /* DisableFields in every registered SmartGroupAssignTarget */
        FOR EACH ttSmartGroupAssignTarget WHERE ttSmartGroupAssignTarget.RecordOwner = THIS-OBJECT NO-LOCK:
            IF NOT VALID-OBJECT (ttSmartGroupAssignTarget.SmartGroupAssignTarget) THEN
                MessageFormHelper:ShowMessage ("An invalid SmartGroupAssignTarget reference was detected during DisableFields."{&TRAN},
                                               (IF VALID-OBJECT (THIS-OBJECT:FindForm()) THEN THIS-OBJECT:FindForm():Text ELSE "SmartViewerControl":U),
                                               ErrorHelper:CurrentStacktrace,
                                               Consultingwerk.Windows.Util.Forms.MessageFormImages:ImageError) .
            ELSE
                CAST(ttSmartGroupAssignTarget.SmartGroupAssignTarget, ISmartGroupAssignTarget):DisableFields().
        END.

        OnFieldsDisabled (System.EventArgs:Empty) .

    END METHOD .

    /**
     * Purpose: Disable all enabled controls of the SmartViewerControl.
     *          This Method is called from inside the SmartViewerControl.
     *          Enforced by Interface ISmartDataTarget or ISmartGroupAssignTarget.
     * Notes:
     */
    METHOD PUBLIC VOID DisableFields ():

        THIS-OBJECT:DisableFields (TRUE) .

    END METHOD.

    /**
     * Purpose: Event handler for the AfterCloseUp event of an
     *          UltraDateTimeEditor. Clears the reference to the currently opened
     *          DataTime popup editor in the variable oEditorDropDownControl
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PROTECTED VOID EditorDropDownAfterCloseUpHandler (sender AS System.Object,
                                                             e AS System.EventArgs):

        DEFINE VARIABLE oDateTimeEditor AS UltraDateTimeEditor NO-UNDO .

        SetOpenedEditorDropDownControl (?) .

        oDateTimeEditor = CAST (sender, UltraDateTimeEditor) .

        IF THIS-OBJECT:SmartTableIOState = TableIOStateEnum:FieldsEnabled AND
           oDateTimeEditor:Enabled AND
           VALID-OBJECT (oDateTimeEditor:DataBindings["Value":U]) AND
           VALID-OBJECT (THIS-OBJECT:SmartDataSource) AND
           VALID-OBJECT (THIS-OBJECT:SmartDataSource:BindingSource) THEN DO:

            THIS-OBJECT:ControlFocus (THIS-OBJECT) .
            THIS-OBJECT:TextChangedEventHandler (sender, System.EventArgs:Empty) .
        END.

    END METHOD.

    /**
     * Purpose: Event handler for the AfterDropDown event of an
     *          UltraDateTimeEditor. Stores a reference to the currently opened
     *          DataTime popup editor in the variable oEditorDropDownControl
     *          Required to be able to start the update process in the
     *          method TextChangedHandler (in this class)
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PROTECTED VOID EditorDropDownAfterDropDownHandler (sender AS System.Object,
                                                              e AS System.EventArgs):

        SetOpenedEditorDropDownControl (CAST(sender, Control)) .

    END METHOD.

    /**
     * Purpose: Enables controls of the SmartViewerControl
     * Notes:   This Method is called by the Toolbar Classes SmartToolbarController
     *          or SmartToolStripContainer by pressing the Tools for Add or Edit Data.
     *          Enforced by Interface ISmartDataTarget or ISmartGroupAssignTarget.
     * @param pcFields The EnableFieldsEnum value indicating if the fields are enabled for Add or Update
     */
    METHOD PUBLIC VOID EnableFields (INPUT pcFields AS CHARACTER):

        DEFINE VARIABLE lAdding              AS LOGICAL             NO-UNDO.
        DEFINE VARIABLE lEnabled             AS LOGICAL             NO-UNDO.
        DEFINE VARIABLE oCancelableEventArgs AS CancelableEventArgs NO-UNDO .

        ASSIGN oCancelableEventArgs = NEW CancelableEventArgs () .

        THIS-OBJECT:OnBeforeEnableFields (oCancelableEventArgs) .

        IF VALID-OBJECT (oCancelableEventArgs) AND oCancelableEventArgs:Cancel THEN
            RETURN .

        IF VALID-OBJECT (THIS-OBJECT:SmartTableIOSource) THEN
           THIS-OBJECT:EnsureVisible () .

        IF THIS-OBJECT:ActivateCurrentGridRowDuringEdit THEN
            THIS-OBJECT:ActivateCurrentGridRow() .

        THIS-OBJECT:SmartTableIOState = TableIOStateEnum:FieldsEnabled .

        IF (pcFields = EnableFieldsEnum:Add) THEN
            ASSIGN lAdding = TRUE .

        FOR EACH ttEnableFields WHERE ttEnableFields.RecordOwner = THIS-OBJECT:

            IF NOT VALID-OBJECT(ttEnableFields.ChildControl) THEN
                NEXT.

            ASSIGN lEnabled = IsControlEnabled (ttEnableFields.SecurityEnableMode,
                                                ttEnableFields.EnableMode,
                                                lAdding) .

            /* Mike Fechner, Consultingwerk Ltd. 20.05.2009
               Bug 1787, Bug 1741: TYPE-OF validation in the form of
               IF TYPE-OF (ttEnableFields.ChildControl, Control) fails. I believe this
               is caused by a core issue and should be reviewed on a later release.
               Actually there is no strong need for the TYPE-OF validation, since the
               input signature to the method SetControlEnabled guarantees that we only
               receive references to System.Windows.Forms.Control childs. So the CASE
               is not expected to fail. */

            /* Mike Fechner, Consultingwerk Ltd. 23.03.2016
               SCL-1163: When ReadOnlyDisabled then Enabled should by TRUE and ReadOnly false
               for those Controls that have a ReadOnly property */
            IF lEnabled = FALSE AND THIS-OBJECT:ReadOnlyDisabled = TRUE THEN DO:
                IF TYPE-OF (ttEnableFields.ChildControl, Infragistics.Win.UltraWinEditors.UltraWinEditorMaskedControlBase) OR
                   TYPE-OF (ttEnableFields.ChildControl, Infragistics.Win.UltraWinGrid.UltraCombo) OR
                   TYPE-OF (ttEnableFields.ChildControl, Infragistics.Win.UltraWinEditors.TextEditorControlBase) OR
                   TYPE-OF (ttEnableFields.ChildControl, System.Windows.Forms.TextBox) THEN

                    CAST(ttEnableFields.ChildControl, Control):Enabled = TRUE .
                ELSE
                    CAST(ttEnableFields.ChildControl, Control):Enabled = lEnabled .
            END .
            ELSE
                CAST(ttEnableFields.ChildControl, Control):Enabled = lEnabled .

            /* Mike Fechner, Consultingwerk Ltd. 02.09.2010
               Editors (= Multi-Line TextBoxes) should be set to read-only, not Disabled */
            IF TYPE-OF (ttEnableFields.ChildControl, Infragistics.Win.UltraWinEditors.UltraTextEditor) AND
                  CAST (ttEnableFields.ChildControl, Infragistics.Win.UltraWinEditors.UltraTextEditor):MultiLine = TRUE THEN
                       CAST (ttEnableFields.ChildControl, Infragistics.Win.UltraWinEditors.UltraTextEditor):ReadOnly = NOT lEnabled .
            ELSE IF THIS-OBJECT:ReadOnlyDisabled AND
                     TYPE-OF (ttEnableFields.ChildControl, Infragistics.Win.UltraWinEditors.UltraWinEditorMaskedControlBase) THEN
                 CAST (ttEnableFields.ChildControl, Infragistics.Win.UltraWinEditors.UltraWinEditorMaskedControlBase):ReadOnly = NOT lEnabled .
            ELSE IF THIS-OBJECT:ReadOnlyDisabled AND
                     TYPE-OF (ttEnableFields.ChildControl, Infragistics.Win.UltraWinGrid.UltraCombo) THEN
                 CAST (ttEnableFields.ChildControl, Infragistics.Win.UltraWinGrid.UltraCombo):ReadOnly = NOT lEnabled .
            ELSE IF THIS-OBJECT:ReadOnlyDisabled AND
                     TYPE-OF (ttEnableFields.ChildControl, Infragistics.Win.UltraWinEditors.TextEditorControlBase) THEN
                 CAST (ttEnableFields.ChildControl, Infragistics.Win.UltraWinEditors.TextEditorControlBase):ReadOnly = NOT lEnabled .
            ELSE
            IF TYPE-OF (ttEnableFields.ChildControl, System.Windows.Forms.TextBox) AND
                CAST (ttEnableFields.ChildControl, System.Windows.Forms.TextBox):MultiLine = TRUE THEN
                    CAST (ttEnableFields.ChildControl, System.Windows.Forms.TextBox):ReadOnly = NOT lEnabled .
        END.

        /* EnableFields in every registered SmartGroupAssignTarget */
        FOR EACH ttSmartGroupAssignTarget WHERE ttSmartGroupAssignTarget.RecordOwner = THIS-OBJECT NO-LOCK:
            IF NOT VALID-OBJECT (ttSmartGroupAssignTarget.SmartGroupAssignTarget) THEN
                MessageFormHelper:ShowMessage ("An invalid SmartGroupAssignTarget reference was detected during EnableFields."{&TRAN},
                                               (IF VALID-OBJECT (THIS-OBJECT:FindForm()) THEN THIS-OBJECT:FindForm():Text ELSE "SmartViewerControl":U),
                                               ErrorHelper:CurrentStacktrace,
                                               Consultingwerk.Windows.Util.Forms.MessageFormImages:ImageError) .
            ELSE
                CAST(ttSmartGroupAssignTarget.SmartGroupAssignTarget, ISmartGroupAssignTarget):EnableFields (pcFields).
        END.

        /* Mike Fechner, Consultingwerk Ltd. 18.09.2009
           Focus Control */
        IF VALID-OBJECT (THIS-OBJECT:SmartTableIOSource) THEN DO:
            IF pcFields = EnableFieldsEnum:Add THEN DO:
                IF VALID-OBJECT (THIS-OBJECT:FocusControlOnAdd) THEN
                    THIS-OBJECT:ControlFocus (THIS-OBJECT:FocusControlOnAdd) .
                ELSE THIS-OBJECT:FocusFirstEnabledEditorControl (THIS-OBJECT) .
            END.
            ELSE IF pcFields = EnableFieldsEnum:Update THEN DO:
                IF VALID-OBJECT (THIS-OBJECT:FocusControlOnUpdate) THEN
                    THIS-OBJECT:ControlFocus (THIS-OBJECT:FocusControlOnUpdate) .
                ELSE THIS-OBJECT:FocusFirstEnabledEditorControl (THIS-OBJECT) .
            END.
        END.

        /* Mike Fechner, Consultingwerk Ltd. 02.06.2015
           SCL-829 - optionally directly into update state */
        IF VALID-OBJECT (THIS-OBJECT:OptionalFeatures) AND THIS-OBJECT:OptionalFeatures:BeginUpdateStateOnEnableFields = TRUE THEN DO:
            IF THIS-OBJECT:ActivateCurrentGridRowDuringEdit THEN
                THIS-OBJECT:ActivateCurrentGridRow() .

            THIS-OBJECT:SmartTableIOState = TableIOStateEnum:ModifyingData .

            /* Tell the SmartDataSource that the user has started to update data. */
            IF VALID-OBJECT(THIS-OBJECT:SmartDataSource) THEN
                THIS-OBJECT:SmartDataSource:BeginUpdateState (THIS-OBJECT).
        END.

        OnFieldsEnabled (System.EventArgs:Empty) .
        OnAfterFieldsEnabled (System.EventArgs:Empty) .

    END METHOD.

    /**
     * Purpose: Invoked at the end of a Containers InitializeComponents method
     * Notes:   Enforced by System.ComponentModel.ISupportInitialize
     *          See http://msdn.microsoft.com/de-de/library/system.componentmodel.isupportinitialize.endinit(VS.80).aspx
     */
    METHOD PUBLIC VOID EndInit ():

       ASSIGN THIS-OBJECT:Initializing = FALSE .

       IF VALID-OBJECT (oSmartDataSource) THEN
          ASSIGN THIS-OBJECT:SmartDataSource = oSmartDataSource .

       THIS-OBJECT:DisableFields() .

       THIS-OBJECT:EvaluateTableIOState () .

    END METHOD.

    /**
     * Purpose: Ensures that this SmartViewerControl is visible
     * Notes:
     */
    METHOD PUBLIC VOID EnsureVisible ():

        DEFINE VARIABLE oForm           AS System.Windows.Forms.Form                               NO-UNDO .
        DEFINE VARIABLE oTabPageControl AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO .

        oForm = THIS-OBJECT:ParentForm .

        IF VALID-OBJECT (oForm) THEN DO:
           IF Progress.Util.EnumHelper:AreEqual(oForm:WindowState, FormWindowState:Minimized) THEN
               oForm:WindowState = System.Windows.Forms.FormWindowState:Normal .

           THIS-OBJECT:ParentForm:Activate() .
         END.

        IF THIS-OBJECT:Visible THEN
            RETURN .

        /* Mike Fechner, Consultingwerk Ltd. 31.12.2009
           Check for UltraTabPage as parent */
        ASSIGN oTabPageControl = CAST (FormHelper:FindParentOfType (THIS-OBJECT,
                                                                    TypeHelper:GetType ("Infragistics.Win.UltraWinTabControl.UltraTabPageControl":U)),
                                       Infragistics.Win.UltraWinTabControl.UltraTabPageControl) .

        IF VALID-OBJECT (oTabPageControl) THEN DO:
            oTabPageControl:Tab:Selected = TRUE .
        END.

    END METHOD.

    /**
     * Purpose: Event handler for the Enter event of an EditorButtonControlBase
     *          derived Class. Selects all content from the Controls Text Property
     *          in the UI.
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PRIVATE VOID EnterEditorControlHandler (sender AS CLASS System.Object,
                                                   e AS CLASS System.EventArgs):

        IF TYPE-OF (sender, Infragistics.Win.UltraWinEditors.UltraWinEditorMaskedControlBase) THEN
            CAST (sender, Infragistics.Win.UltraWinEditors.UltraWinEditorMaskedControlBase):SelectAll ().

        ELSE IF TYPE-OF (sender, Infragistics.Win.UltraWinEditors.TextEditorControlBase) THEN
                CAST (sender, Infragistics.Win.UltraWinEditors.TextEditorControlBase):SelectAll ().

            ELSE IF TYPE-OF (sender, System.Windows.Forms.TextBoxBase) THEN
                    CAST (sender, System.Windows.Forms.TextBoxBase):SelectAll ().

    END METHOD.

    /**
     * Purpose: This Method sets the Property SmartTableIOState to a value according
     *          to the SmartDataSources state of the current class. Possible values
     *          for SmartTableIOState are defined in the TableIOStateEnum.
     * Notes:   This Method is called by the SmartDataAdapter when the
     *          PositionChangedHandler is fired or inside the SmartViewerControl if
     *          a new BindingSource is attached.
     *          Enforced by ISmartDataTarget
     */
    METHOD PUBLIC VOID EvaluateTableIOState ():

        /* Mike Fechner, Consultingwerk Ltd. 12.02.2014
           Don't evaluate a new TableIO State, when the BindingSource's PositionChanged handler
           was actually invoked from the creation of a new record. */
        IF lInCreateRecord OR THIS-OBJECT:SavingChanges THEN
            RETURN .

        /* Validate SmartTableIOState on init */
        /* Check if data is available */
        IF VALID-OBJECT (oSmartDataSource) AND VALID-OBJECT (oSmartDataSource:BindingSource) THEN
        DO:
            IF oSmartDataSource:BindingSource:Count > 0 THEN
            DO:
                /* Don't set TableIO State to FieldsDisabled, when fields are enabled */
                IF THIS-OBJECT:SmartTableIOState <> TableIOStateEnum:FieldsEnabled THEN
                DO:
                    /* Mike Fechner, Consultingwerk Ltd. 01.11.2010
                       SaveMode */
                    IF THIS-OBJECT:SaveMode THEN
                        THIS-OBJECT:EnableFields (EnableFieldsEnum:Update) .
                    ELSE
                        THIS-OBJECT:SmartTableIOState = TableIOStateEnum:FieldsDisabled .

                    ASSIGN THIS-OBJECT:AddingRecord  = FALSE
                           THIS-OBJECT:CopyingRecord = FALSE .
                END.
            END.
            ELSE DO:
                IF TYPE-OF (oSmartDataSource, SmartDataAdapter) AND
                   VALID-OBJECT (CAST (oSmartDataSource, SmartDataAdapter):SmartDataSource) AND
                   TYPE-OF (CAST (oSmartDataSource, SmartDataAdapter):SmartDataSource, SmartDataAdapter) AND

                   CAST (CAST (oSmartDataSource, SmartDataAdapter):SmartDataSource, SmartDataAdapter):RecordPosition = RecordPositionEnum:NoRecordAvailable THEN

                    THIS-OBJECT:SmartTableIOState = TableIOStateEnum:NoParentRecordAvailable .
                ELSE
                    THIS-OBJECT:SmartTableIOState = TableIOStateEnum:NoRecordAvailable .
            END.
        END.
        ELSE THIS-OBJECT:SmartTableIOState = TableIOStateEnum:NoDataSource .

        /* Mike Fechner, Consultingwerk Ltd. 22.06.2015
           SCL-871 - when there is no record available, we need to disable the fields */
        IF THIS-OBJECT:SmartTableIOState = TableIOStateEnum:NoRecordAvailable AND
           THIS-OBJECT:SaveMode = TRUE THEN
            THIS-OBJECT:DisableFields (FALSE) .

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /**
     * Purpose: Populates the ttViewerBoundControls temp-table
     * Notes:   Internal method, populates records for a single control container at
     *          a time
     * @param poContainer The reference to the current control container
     */
    METHOD PROTECTED VOID FillBoundControlTable (poContainer AS System.Windows.Forms.Control):

        DEFINE VARIABLE cPropertyName       AS CHARACTER                               NO-UNDO .
        DEFINE VARIABLE oBindingsCollection AS System.Windows.Forms.BindingsCollection NO-UNDO .
        DEFINE VARIABLE j                   AS INTEGER                                 NO-UNDO .

        {Consultingwerk/foreach.i System.Windows.Forms.Control oControl in poContainer:Controls}

            ASSIGN oBindingsCollection = oControl:DataBindings .

            IF oBindingsCollection:Count > 0 THEN DO:
                DO j = (oBindingsCollection:Count - 1) TO 0 BY -1:
                    CREATE ttViewerBoundControls.
                    ASSIGN cPropertyName                                 = oBindingsCollection:Item[j]:PropertyName
                           ttViewerBoundControls.ControlReference        = oControl
                           ttViewerBoundControls.ViewerControl           = THIS-OBJECT
                           ttViewerBoundControls.BindingSourceColumnName = oControl:DataBindings[cPropertyName]:BindingMemberInfo:BindingField
                           ttViewerBoundControls.PropertyName            = cPropertyName .

                    RELEASE ttViewerBoundControls .
                END.
            END.

            IF oControl:Controls:Count > 0 THEN
                FillBoundControlTable (oControl) .
        END.

    END METHOD .

&IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN
    /**
     * Purpose: Attempts to locate the BindingSource that was used in a SmartViewerControl
     *          at Design Time
     * Notes:   This method may only be called before the SmartViewerControl:SmartDataSource
     *          property has been assigned a value to as the Design Time BindingSource
     *          will be disposed when attaching to the SmartDataSource's BindingSource
     *          at runtime.
     *          This method is helpful when building tools that may be able to query
     *          properties such as the EntityName and EntityTable from the Design Time
     *          BindingSource
     *          Supported from OpenEdge
     * @return The reference to the design time binding source or ?
     */
    METHOD PUBLIC Progress.Data.BindingSource FindDesignTimeBindingSource ():

        {Consultingwerk/foreach.i System.ComponentModel.IContainer oContainer in THIS-OBJECT:ComponentsCollection}

            {Consultingwerk/foreach.i System.ComponentModel.IComponent oComponent in oContainer:Components}

                IF TYPE-OF (oComponent, Progress.Data.BindingSource) THEN

                    RETURN CAST (oComponent, Progress.Data.BindingSource).
            END.
        END.

        RETURN ? .

    END METHOD .
&ENDIF

    /**
     * Purpose: Focusses the first enabled EditorControl of the given Container
     *          Control
     * Notes:   DEFINE PRIVATE STATIC TEMP-TABLE ttTabOrder NO-UNDO
     *              FIELD RecordOwner   AS Progress.Lang.Object
     *              FIELD ChildControl  AS Progress.Lang.Object
     *              FIELD TabOrder      AS INTEGER
     *              INDEX RecordOwner RecordOwner TabOrder .
     * @param poControl The reference to the container control
     * @return Logical value indicating the success of this method
     */
    METHOD PUBLIC LOGICAL FocusFirstEnabledEditorControl (poControl AS Control):

        DEFINE VARIABLE oChildControl AS Control NO-UNDO .

        DEFINE BUFFER bttTabOrder FOR ttTabOrder .

        {Consultingwerk/foreach.i Control oControl in poControl:Controls}

            IF NOT oControl:Enabled OR
               NOT oControl:Visible OR
               NOT oControl:TabStop THEN
                NEXT .

            CREATE bttTabOrder .
            ASSIGN bttTabOrder.RecordOwner  = poControl
                   bttTabOrder.ChildControl = oControl
                   bttTabOrder.TabOrder     = oControl:TabIndex .
        END.

        FOR EACH bttTabOrder WHERE bttTabOrder.RecordOwner = poControl BY bttTabOrder.TabOrder:
            oChildControl = CAST(bttTabOrder.ChildControl, Control) .

            IF oChildControl:Controls:Count > 0 THEN
                IF THIS-OBJECT:FocusFirstEnabledEditorControl (oChildControl) THEN
                    RETURN TRUE .

            IF THIS-OBJECT:IsEditorControl(oChildControl) AND THIS-OBJECT:ControlFocus (oChildControl) THEN
                RETURN TRUE .
        END.

        RETURN FALSE .

        FINALLY:
            ASSIGN oChildControl = ? .

            FOR EACH bttTabOrder WHERE bttTabOrder.RecordOwner = poControl:
                DELETE bttTabOrder .
            END.
        END FINALLY.
    END METHOD.

    /**
     * Purpose: Event handler for the Activate event of the owning Form
     * Notes:   Notes, this Event Handler is only subscribed to the Activate event
     *          of the containing Form when the Viewer's event handlers are activated,
     *          i.o.w the Viewer is active.
     * @param sender The reference to the object that raised the event
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PROTECTED VOID FormActivatedHandler (sender AS System.Object,
                                                e AS System.EventArgs):

        IF THIS-OBJECT:DesignTime THEN
            RETURN .

        ActivateEventHandler () .

        THIS-OBJECT:FindForm ():Activated:Unsubscribe (FormActivatedHandler) NO-ERROR .

    END METHOD.

    /**
     * Purpose: Handles the FormClosed event of the containing Form
     * Notes:   Notes, this Event Handler is only subscribed to the FormClosed event
     *          of the containing Form when the Viewer's event handlers are activated,
     *          iow the Viewer is active.
     * @param sender The reference to the object that raised the event
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PROTECTED VOID FormClosedHandler (sender AS System.Object,
                                             e AS System.EventArgs):

        DeactivateEventHandler () .

    END METHOD.

    /**
     * Purpose: Handles the FormClosed event of the containing Form
     * Notes:   Notes, this Event Handler is always subscribed to the FormClosed event
     *          of the containing Form
     * @param sender The reference to the object that raised the event
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PRIVATE VOID FormClosedHandler2 (sender AS System.Object,
                                            e AS System.EventArgs):

        IF VALID-OBJECT (sender) AND TYPE-OF (sender, System.Windows.Forms.Form) THEN
            CAST (sender, System.Windows.Forms.Form):FormClosed:Unsubscribe (FormClosedHandler2) .

        THIS-OBJECT:Leave:Unsubscribe (LeaveHandler) .

        IF VALID-OBJECT (THIS-OBJECT:ViewerLogicObject) AND
           THIS-OBJECT:ViewerLogicObject:Owner = THIS-OBJECT THEN DO:

            THIS-OBJECT:ViewerLogicObject:Shutdown () .

            DELETE OBJECT THIS-OBJECT:ViewerLogicObject .
            THIS-OBJECT:ViewerLogicObject = ? .
        END.

    END METHOD .

    /**
     * Purpose: Event handler for the Deactivate event of the owning Form
     * Notes:   Notes, this Event Handler is only subscribed to the Deactivated event
     *          of the containing Form when the Viewer's event handlers are activated,
     *          iow the Viewer is active.
     * @param sender The reference to the object that raised the event
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PROTECTED VOID FormDeactivatedHandler (sender AS System.Object,
                                                  e AS System.EventArgs):

        IF THIS-OBJECT:DesignTime THEN
            RETURN .

        DeactivateEventHandler () .

        THIS-OBJECT:FindForm ():Activated:Subscribe (FormActivatedHandler) NO-ERROR .

    END METHOD.

    /**
     * Purpose: Returns the reference to the Control that is bound to the named field
     *          of the binding source
     * Notes:
     * @param pcBindingPropertyName The name of the field in the ProBindingSource
     * @return The reference to the resulting Control
     */
    METHOD PUBLIC System.Windows.Forms.Control GetBoundControl (pcBindingPropertyName AS CHARACTER):

        RETURN THIS-OBJECT:RecurseLocateBindingControl (THIS-OBJECT, pcBindingPropertyName) .

    END METHOD .

    /**
     * Purpose: Returns the list of Controls that is bound to the named field
     *          of the binding source
     * Notes:   Returns all Controls in the viewer and also GroupAssignLinked viewers
     * @param pcBindingPropertyName The name of the field in the ProBindingSource
     * @return The List of bound Controls
     */
    METHOD PUBLIC "System.Collections.Generic.List<System.Windows.Forms.Control>" GetBoundControls (pcBindingPropertyName AS CHARACTER):

        DEFINE VARIABLE oList  AS "System.Collections.Generic.List<System.Windows.Forms.Control>" NO-UNDO .
        DEFINE VARIABLE oList2 AS "System.Collections.Generic.List<System.Windows.Forms.Control>" NO-UNDO .

        oList = NEW "System.Collections.Generic.List<System.Windows.Forms.Control>" () .

        THIS-OBJECT:RecurseLocateBindingControls (THIS-OBJECT, oList, pcBindingPropertyName) .

        {Consultingwerk/foreachABL.i ISmartGroupAssignTarget oTarget in THIS-OBJECT:SmartGroupAssignTargets}

            IF VALID-OBJECT (oTarget) AND TYPE-OF (oTarget, SmartViewerControl) THEN DO ON ERROR UNDO, THROW:

                oList2 = CAST (oTarget, SmartViewerControl):GetBoundControls (pcBindingPropertyName) .

                {Consultingwerk/foreach.i System.Windows.Forms.Control oControl in oList2}

                    oList:Add (oControl) .
                END.

                FINALLY:
                    GarbageCollectorHelper:DeleteObject (oList2) .
                END FINALLY.
            END.
        END.

        RETURN oList .

    END METHOD .

    /**
     * Purpose: Populates a temp-table with information about all bound controls
     *          in the SmartViewerControl
     * Notes:   Also processes GroupAssignTarget Viewers
     * @param ttViewerBoundControls INPUT-OUTPUT TABLE with the data binding information, from Consultingwerk\SmartComponents\Base\ttViewerBoundControls.i
     */
    METHOD PUBLIC VOID GetBoundControlTable (INPUT-OUTPUT TABLE FOR ttViewerBoundControls):

        THIS-OBJECT:FillBoundControlTable (THIS-OBJECT) .

        {Consultingwerk/foreachABL.i ISmartGroupAssignTarget oTarget in THIS-OBJECT:SmartGroupAssignTargets}

            IF VALID-OBJECT (oTarget) AND TYPE-OF (oTarget, SmartViewerControl) THEN DO ON ERROR UNDO, THROW:

                CAST (oTarget, SmartViewerControl):GetBoundControlTable (INPUT-OUTPUT TABLE ttViewerBoundControls BY-REFERENCE) .

            END.
        END.

    END METHOD .

    /**
     * Purpose: Returns the Enable mode for a Control from the list of Controlthat will be enabled
     *          and disabled by the Viewer based on the current Viewer state
     * @param poControl The Control for which the Enable mode should be returned
     * @return The CHARACTER value representing the Control Enabled mode
     */
    METHOD PUBLIC CHARACTER GetControlEnabled (poControl AS Control):

        FIND ttEnableFields WHERE ttEnableFields.ChildControl = poControl
                              AND ttEnableFields.RecordOwner  = THIS-OBJECT NO-ERROR .

        IF AVAILABLE ttEnableFields THEN
            RETURN ttEnableFields.EnableMode .

        RETURN ? .

    END METHOD .

    /**
     * Purpose: Returns the security Enable mode for a Control from the list of Controls
     *          that will be enabled and disabled by the Viewer based on the current Viewer
     *          state
     * @param poControl The Control for which the Enable mode should be returned
     * @return The CHARACTER value representing the security Control Enabled mode
     */
    METHOD PUBLIC CHARACTER GetControlSecurityEnabled (poControl AS Control):

        FIND ttEnableFields WHERE ttEnableFields.ChildControl = poControl
                              AND ttEnableFields.RecordOwner  = THIS-OBJECT NO-ERROR .

        IF AVAILABLE ttEnableFields THEN
            RETURN ttEnableFields.SecurityEnableMode .

        RETURN ? .

    END METHOD .

     /**
      * Purpose: Returns the reference to a Control in the Viewer identified by the Controls
      *          Name. This Method starts a recursive search for the Control.
      * Notes:   The Method is called by the SmartLookupControls to set values to
      *          the Controls after the Lookup Dialog has been closed.
      * @param pcControlName Name of the Control to search
      * @return The reference to the Control or the unknown value
      */
    METHOD PUBLIC System.Windows.Forms.Control GetViewerField (pcControlName AS CHARACTER):

        IF THIS-OBJECT:Controls:ContainsKey (pcControlName) THEN
            RETURN THIS-OBJECT:Controls [pcControlName] .
        ELSE
            RETURN GetViewerField (pcControlName, THIS-OBJECT) .

    END METHOD.

    /**
     * Purpose: Recursively searches for the Control with the given name
     * Notes:   This Method is called by the Method GetViewerField (pcControlName AS CHARACTER)
     *          of the SmartViewerControl Class.
     * @param pcControlName Name of the Control to search
     * @param poControl The parent control to search in
     * @return The reference to the Control or the unknown value
     */
    METHOD PROTECTED System.Windows.Forms.Control GetViewerField (pcControlName AS CHARACTER,
                                                                  poControl AS Control):

        DEFINE VARIABLE i        AS INTEGER NO-UNDO.
        DEFINE VARIABLE oControl AS Control NO-UNDO .

        IF poControl:Controls:ContainsKey (pcControlName) THEN
            RETURN poControl:Controls[pcControlName] .


        DO i = 0 TO poControl:Controls:Count - 1:
            oControl = GetViewerField (pcControlName, poControl:Controls[i]) .

            IF VALID-OBJECT(oControl) THEN
                RETURN oControl .
        END.

        RETURN ? .

    END METHOD.

    /**
     * Purpose: Returns the reference to the SmartGroupAssignTarget that contains
     *          a Control bound to the given field name
     * Notes:
     * @param pcFieldName The name of the field
     * @return The reference to the SmartGroupAssignTarget that contains a Control bound to the given field name
     */
    METHOD PROTECTED ISmartGroupAssignTarget GetViewerForBoundControl (pcFieldName AS CHARACTER):

        DEFINE VARIABLE oControl AS System.Windows.Forms.Control NO-UNDO .
        DEFINE VARIABLE oTarget  AS ISmartGroupAssignTarget      NO-UNDO .

        IF pcFieldName > "":U THEN
        DO:
            oControl = RecurseLocateBindingControl (THIS-OBJECT, pcFieldName) .

            IF VALID-OBJECT(oControl) THEN
                RETURN THIS-OBJECT .

            FOR EACH ttSmartGroupAssignTarget WHERE ttSmartGroupAssignTarget.RecordOwner = THIS-OBJECT:
                oTarget = CAST (ttSmartGroupAssignTarget.SmartGroupAssignTarget,
                                ISmartGroupAssignTarget) .

                oControl = oTarget:GetBoundControl (pcFieldName) .

                IF VALID-OBJECT (oControl) THEN
                    RETURN oTarget .
            END.

            RETURN ? .
        END.

        RETURN ? .

    END METHOD.

    /**
     * Purpose: Event handler for the SmartTableIOStateChanged event of the
     *          Groupassign Source
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The SmartTableIOStateChangedEventArgs with the data for this event
     */
    METHOD PROTECTED VOID GroupAssignSmartTableIOStateChangedHandler (sender AS System.Object,
                                                                      e AS SmartTableIOStateChangedEventArgs):

        THIS-OBJECT:SmartTableIOState  = e:SmartTableIOState .

    END METHOD.

    /**
     * Purpose: Handles an error raised by validation routines
     * Notes:   Displays the Error message and focusses the field that caused the error
     * @param poError The reference to the validation error instance
     */
    METHOD PUBLIC VOID HandleValidationError (poError AS Progress.Lang.Error):

        DEFINE VARIABLE oGroupAssignTarget AS ISmartGroupAssignTarget NO-UNDO .

        IF poError:NumMessages > 0 AND NUM-ENTRIES (poError:GetMessage (1), CHR (4)) > 1 THEN
            oGroupAssignTarget = THIS-OBJECT:GetViewerForBoundControl (ENTRY (2, poError:GetMessage (1), CHR (4))) .

        IF NOT VALID-OBJECT (oGroupAssignTarget) THEN
            oGroupAssignTarget = THIS-OBJECT .

        oGroupAssignTarget:EnsureVisible () .

        THIS-OBJECT:DisplayError (poError, FALSE).

        /* If the Error implements the Interface IFieldValidationError
           Set the focus to the Field causing the error */
        IF TYPE-OF(poError, IFieldValidationError) THEN
            THIS-OBJECT:ApplyFocusToErrorField (CAST(poError, IFieldValidationError)) .
        ELSE IF poError:NumMessages > 0 AND NUM-ENTRIES (poError:GetMessage (1), CHR (4)) > 1 THEN
            THIS-OBJECT:ApplyFocusToErrorField (ENTRY (2, poError:GetMessage (1), CHR (4))) .

        /* Mike Fechner, Consultingwerk Ltd. 14.04.2016
           SCL-1117: Reinstantiate SmartTableIOState property, to receive correct Toolbar State */
        IF (THIS-OBJECT:SmartTableIOState = TableIOStateEnum:FieldsDisabled OR
            THIS-OBJECT:SmartTableIOState = TableIOStateEnum:FieldsEnabled) AND
           TYPE-OF (poError, ValidationException) AND
           VALID-OBJECT (CAST (poError, ValidationException):RecordIdentifier) THEN DO:

            CASE CAST (poError, ValidationException):RecordIdentifier:RowState:

                WHEN ROW-MODIFIED OR WHEN ROW-CREATED THEN
                    THIS-OBJECT:SmartTableIOState = TableIOStateEnum:ModifyingData .

            END CASE .
        END.

    END METHOD.

    /**
     * Purpose: Returns logical value indicating if the Viewer contains a Control
     *          that is bound to the named field
     * Notes:   Optionally includes Group Assigned viewers
     * @param pcBindingPropertyName The name of the field in the ProBindingSource
     * @param plIncludeGroupAssignTargets Logical value indicating if the method should also look in GroupAssign Target Viewers
     * @return Logical value indicating if a bound control exists
     */
    METHOD PUBLIC LOGICAL HasBoundControl (pcBindingPropertyName AS CHARACTER,
                                           plIncludeGroupAssignTargets AS LOGICAL):

        IF VALID-OBJECT (THIS-OBJECT:RecurseLocateBindingControl (THIS-OBJECT,
                                                                  pcBindingPropertyName)) THEN
            RETURN TRUE .

        IF plIncludeGroupAssignTargets = TRUE THEN DO:

            {Consultingwerk/foreachABL.i ISmartGroupAssignTarget oTarget in THIS-OBJECT:SmartGroupAssignTargets}

                IF VALID-OBJECT (oTarget) AND TYPE-OF (oTarget, SmartViewerControl) THEN DO:

                    IF CAST (oTarget, SmartViewerControl):HasBoundControl (pcBindingPropertyName,
                                                                           plIncludeGroupAssignTargets) = TRUE THEN
                        RETURN TRUE .
                END.
            END.
        END.

        RETURN FALSE .

    END METHOD .

    /**
     * Purpose: Subscribe the EventHandler(s) to events of all controls added to
     *          the SmartViewerControl. The RecurseInitializeControlEventHandler is
     *          used to Handle this Task.
     *          This Method is called every Time a new Control is added to the
     *          SmartViewerControl.
     * Notes:
     */
    METHOD PROTECTED VOID InitializeControlEventHandler ():

        /* Mike Fechner, Consultingwerk Ltd. 27.07.2011
           Do not initialize the event handler in the
           Visual Designer. This would just have an negative
           impact on the loading time. */
        IF THIS-OBJECT:DesignTime = FALSE THEN
            RecurseInitializeControlEventHandler (THIS-OBJECT) .

    END METHOD.

    /**
     * Purpose: Initializes the Viewer Logic Object
     * Notes:   The SmartViewerControl will initialize the ViewerLogicClass during
     *          EndInit when the property ViewerLogicClass is set and the Viewer
     *          does not have a SmartGroupAssignSource
     *          The Viewer Logic Object will also not initialized when the ViewerLogicObject
     *          instance is already set
     *          Registers the ViewerLogicObject also for the SmartGroupAssignTargets
     */
    METHOD PROTECTED VOID InitializeViewerLogicObject ():

        DEFINE VARIABLE oLogicObject AS ISmartViewerLogic  NO-UNDO .
        DEFINE VARIABLE oChildViewer AS SmartViewerControl NO-UNDO .

        {Consultingwerk/Assertion/Assert/NotNullOrEmpty.i THIS-OBJECT:ViewerLogicClass "'ViewerLogicClass':U"} .
        ObjectAssert:IsNotValid (THIS-OBJECT:SmartGroupAssignSource, "SmartGroupAssignSource":U) .
        ObjectAssert:IsNotValid (THIS-OBJECT:ViewerLogicObject, "ViewerLogicObject":U) .

        oLogicObject = DYNAMIC-NEW (THIS-OBJECT:ViewerLogicClass) () .
        oLogicObject:Owner = THIS-OBJECT .

        /* Handshaking */
        oLogicObject:RegisterSmartViewerControl (THIS-OBJECT) .

        ASSIGN THIS-OBJECT:ViewerLogicObject = oLogicObject .

        {Consultingwerk/foreachABL.i ISmartGroupAssignTarget oGroupAssignTarget in THIS-OBJECT:SmartGroupAssignTargets}

            IF TYPE-OF (oGroupAssignTarget, SmartViewerControl) THEN DO:

                oChildViewer = CAST (oGroupAssignTarget, SmartViewerControl) .

                IF NOT VALID-OBJECT (oChildViewer:ViewerLogicObject) THEN
                    oLogicObject:RegisterSmartViewerControl (oChildViewer) .
            END.
        END.

        oLogicObject:Initialize () .

        CATCH err AS Progress.Lang.Error:
            UNDO, THROW NEW InitializeViewerLogicObjectException (err,
                                                                  THIS-OBJECT:ViewerLogicClass) .
        END CATCH.

    END METHOD .

    /**
     * Purpose: Determines if a Control should be enabeld based on the SecurityEnableMode
     *          and EnableMode
     * Notes:
     * @param pcSecurityEnableMode The security enable mode of the current control
     * @param pcEnableMode The enable mode of the current control
     * @param plAdd Logical value indicating if the viewer is in Add (true) or Update (false) mode
     * @return Logical value indicating if the control is supposed to be enabeld or not
     */
    METHOD PROTECTED LOGICAL IsControlEnabled (pcSecurityEnableMode AS CHARACTER,
                                               pcEnableMode AS CHARACTER,
                                               plAdd AS LOGICAL):

        DEFINE VARIABLE lSecurityEnabled AS LOGICAL NO-UNDO INITIAL FALSE .
        DEFINE VARIABLE lEnabled         AS LOGICAL NO-UNDO INITIAL FALSE .

        IF pcSecurityEnableMode = ControlEnabledEnum:Add THEN
            ASSIGN lSecurityEnabled = plAdd .

        ELSE IF pcSecurityEnableMode = ControlEnabledEnum:Always THEN
            ASSIGN lSecurityEnabled = TRUE .

        ELSE IF pcSecurityEnableMode = ControlEnabledEnum:Never THEN
            ASSIGN lSecurityEnabled = FALSE .

        ELSE IF pcSecurityEnableMode = ControlEnabledEnum:Update THEN
            ASSIGN lSecurityEnabled = TRUE .

        ELSE IF pcSecurityEnableMode = ControlEnabledEnum:UpdateOnly THEN
            ASSIGN lSecurityEnabled = NOT plAdd .


        IF pcEnableMode = ControlEnabledEnum:Add THEN
            ASSIGN lEnabled = plAdd .

        ELSE IF pcEnableMode = ControlEnabledEnum:Always THEN
            ASSIGN lEnabled = TRUE .

        ELSE IF pcEnableMode = ControlEnabledEnum:Never THEN
            ASSIGN lEnabled = FALSE .

        ELSE IF pcEnableMode = ControlEnabledEnum:Update THEN
            ASSIGN lEnabled = TRUE .

        ELSE IF pcEnableMode = ControlEnabledEnum:UpdateOnly THEN
            ASSIGN lEnabled = NOT plAdd .

        RETURN lSecurityEnabled AND lEnabled .

    END METHOD .

    /**
     * Purpose: Returns TRUE is the Controls is recognized as an Editor Control
     * Notes:   That is a Control the user normally enters Text
     * @param poControl The reference to the Control to test
     * @return Logical valud indicating if the Control is a known Editor Control
     */
    METHOD PUBLIC LOGICAL IsEditorControl (poControl AS Control):

        RETURN TYPE-OF (poControl, Infragistics.Win.UltraWinEditors.EditorButtonControlBase) OR
               TYPE-OF (poControl, Infragistics.Win.UltraWinEditors.UltraCheckEditor) OR
               TYPE-OF (poControl, Infragistics.Win.UltraWinGrid.UltraDropDownBase) .

    END METHOD.

    /**
     * Purpose: Returns if the current (screen-value) of a Control in the SmartViewerControl
     *          derived class has changed in comparison to the underlying buffer vaule
     * Notes:   Currently compares based on Value, Text, CheckedState or LookupKeyValue
     *          bound values
     * @param poControl The reference to the Control (type is System.Object for convenience of passing in the sender of a .NET event handler)
     * @return Logical value indicating if the current value has changed
     */
    METHOD PROTECTED LOGICAL IsValueChanged (poControl AS System.Object):

        DEFINE VARIABLE cBindingProperties AS CHARACTER                    NO-UNDO EXTENT 4
              INITIAL ["Value":U, "Text":U, "CheckedState":U, "LookupKeyValue":U].
        DEFINE VARIABLE oControl           AS System.Windows.Forms.Control NO-UNDO .
        DEFINE VARIABLE cPropertyName      AS CHARACTER                    NO-UNDO .
        DEFINE VARIABLE oBinding           AS System.Windows.Forms.Binding NO-UNDO .
        DEFINE VARIABLE oScreenValue       AS System.Object                NO-UNDO .
        DEFINE VARIABLE cScreenValue       AS CHARACTER                    NO-UNDO .
        DEFINE VARIABLE cFieldName         AS CHARACTER                    NO-UNDO .
        DEFINE VARIABLE i                  AS INTEGER                      NO-UNDO .

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poControl """poControl"":U"} .
        {Consultingwerk/Assertion/ObjectAssert/IsValid.i THIS-OBJECT:SmartDataSource """SmartDataSource"":U"} .

        ASSIGN oControl = CAST (poControl, System.Windows.Forms.Control) .

        DO i = 1 TO EXTENT (cBindingProperties):
            ASSIGN cPropertyName = cBindingProperties [i] .

            oBinding = oControl:DataBindings[cPropertyName] NO-ERROR .

            IF NOT VALID-OBJECT (oBinding) THEN
                NEXT .

            ASSIGN cFieldName   = oBinding:BindingMemberInfo:BindingField
                   oScreenValue = CAST (oBinding:DataSource,
                                        Progress.Data.BindingSource):InputValue [cFieldName]
                   cScreenValue = STRING (UNBOX (oScreenValue)).

            IF cScreenValue <> THIS-OBJECT:SmartDataSource:GetFieldValues (cFieldName) THEN
                RETURN TRUE .
            ELSE
                RETURN FALSE .
        END.

        UNDO, THROW NEW AppError (SUBSTITUTE ("The Control &1 does not have any of the supported data bindings for this method (Value, Text, CheckedState, LookupKeyValue)."{&TRAN},
                                              oControl:Name),
                                  0) .

    END METHOD .

    /**
     * Purpose: Raises the AddingRecordChanged event
     * Notes:
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PROTECTED VOID OnAddingRecordChanged (e AS System.EventArgs):

        IF NOT VALID-OBJECT (e) THEN
            e = System.EventArgs:Empty .

        THIS-OBJECT:AddingRecordChanged:Publish (THIS-OBJECT, e) .

    END METHOD.

    /**
     * Purpose: Raises the AddedRecordModifiedChanged
     * Notes:
     * @param e The System.EventArgs with the data for the event
     */
    METHOD PROTECTED VOID OnAddedRecordModifiedChanged (e AS System.EventArgs):

        IF NOT VALID-OBJECT (e) THEN
            e = System.EventArgs:Empty .

        THIS-OBJECT:AddedRecordModifiedChanged:Publish (THIS-OBJECT, e) .

    END METHOD .

    /**
     * Purpose: Raises the AfterAddRecord event
     * Notes:
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PROTECTED VOID OnAfterAddRecord (e AS System.EventArgs):

        IF NOT VALID-OBJECT (e) THEN
            e = System.EventArgs:Empty .

        THIS-OBJECT:AfterAddRecord:Publish (THIS-OBJECT, e) .

    END METHOD.

    /**
     * Purpose: Raises the AfterCancelUpdate event
     * Notes:
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PROTECTED VOID OnAfterCancelUpdate (e AS System.EventArgs):

        IF NOT VALID-OBJECT (e) THEN
            e = System.EventArgs:Empty .

        THIS-OBJECT:AfterCancelUpdate:Publish (THIS-OBJECT, e) .

    END METHOD.

    /**
     * Purpose: Raises the AfterCopyRecord event
     * Notes:
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PROTECTED VOID OnAfterCopyRecord (e AS System.EventArgs):

        IF NOT VALID-OBJECT (e) THEN
            e = System.EventArgs:Empty .

        THIS-OBJECT:AfterCopyRecord:Publish (THIS-OBJECT, e) .

    END METHOD.

    /**
     * Purpose: Raises the AfterDeleteRecord event
     * Notes:
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PROTECTED VOID OnAfterDeleteRecord (e AS System.EventArgs):

        IF NOT VALID-OBJECT (e) THEN
            e = System.EventArgs:Empty .

        THIS-OBJECT:AfterDeleteRecord:Publish (THIS-OBJECT, e) .

    END METHOD.

    /**
     * Purpose: Raises the AfterFieldsEnabled
     * Notes:
     * @param e The System.EventArgs with the data for the event
     */
    METHOD PROTECTED VOID OnAfterFieldsEnabled (e AS System.EventArgs):

        IF NOT VALID-OBJECT (e) THEN
            e = System.EventArgs:Empty .

        THIS-OBJECT:AfterFieldsEnabled:Publish (THIS-OBJECT, e) .

    END METHOD .

    /**
     * Purpose: Raises the AfterSaveChanges event
     * Notes:
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PROTECTED VOID OnAfterSaveChanges (e AS System.EventArgs):

        IF NOT VALID-OBJECT (e) THEN
            e = System.EventArgs:Empty .

        THIS-OBJECT:AfterSaveChanges:Publish (THIS-OBJECT, e) .

    END METHOD.

    /**
     * Purpose: Raises the BeforeAddRecord event
     * Notes:
     * @param e The CancelEventArgs with the data for this event
     */
    METHOD PROTECTED VOID OnBeforeAddRecord (e AS System.ComponentModel.CancelEventArgs):

        IF NOT VALID-OBJECT (e) THEN
            UNDO, THROW NEW AppError ("This event needs a valid CancelEventArgs object."{&TRAN}, 0) .

        THIS-OBJECT:BeforeAddRecord:Publish (THIS-OBJECT, e) .

    END METHOD.

    /**
     * Purpose: Raises the BeforeCancelUpdate event
     * Notes:
     * @param e The CancelEventArgs with the data for this event
     */
    METHOD PROTECTED VOID OnBeforeCancelUpdate (e AS System.ComponentModel.CancelEventArgs):

        IF NOT VALID-OBJECT (e) THEN
            UNDO, THROW NEW AppError ("This event needs a valid CancelEventArgs object."{&TRAN}, 0) .

        THIS-OBJECT:BeforeCancelUpdate:Publish (THIS-OBJECT, e) .

    END METHOD.

    /**
     * Purpose: Raises the BeforeCopyRecord event
     * Notes:
     * @param e The CancelEventArgs with the data for this event
     */
    METHOD PROTECTED VOID OnBeforeCopyRecord (e AS System.ComponentModel.CancelEventArgs):

        IF NOT VALID-OBJECT (e) THEN
            UNDO, THROW NEW AppError ("This event needs a valid CancelEventArgs object."{&TRAN}, 0) .

        THIS-OBJECT:BeforeCopyRecord:Publish (THIS-OBJECT, e) .

    END METHOD.

    /**
     * Purpose: Raises the BeforeDeleteRecord event
     * Notes:
     * @param e The CancelEventArgs with the data for this event
     */
    METHOD PROTECTED VOID OnBeforeDeleteRecord (e AS System.ComponentModel.CancelEventArgs):

        IF NOT VALID-OBJECT (e) THEN
            UNDO, THROW NEW AppError ("This event needs a valid CancelEventArgs object."{&TRAN}, 0) .

        THIS-OBJECT:BeforeDeleteRecord:Publish (THIS-OBJECT, e) .

    END METHOD.

    /**
     * Purpose: Raises the BeforeEnableFields
     * Notes:
     * @param e The CancelableEventArgs with the data for the event
     */
    METHOD PROTECTED VOID OnBeforeEnableFields (e AS CancelableEventArgs):

        Consultingwerk.Assertion.EventArgsAssert:IsValid (e, "BeforeEnableFields":U) .

        THIS-OBJECT:BeforeEnableFields:Publish (THIS-OBJECT, e) .

    END METHOD .

    /**
     * Purpose: Raises the BeforeSaveChanges event
     * Notes:
     * @param e The CancelEventArgs with the data for this event
     */
    METHOD PROTECTED VOID OnBeforeSaveChanges (e AS System.ComponentModel.CancelEventArgs):

        IF NOT VALID-OBJECT (e) THEN
            UNDO, THROW NEW AppError ("This event needs a valid CancelEventArgs object."{&TRAN}, 0) .

        THIS-OBJECT:BeforeSaveChanges:Publish (THIS-OBJECT, e) .

    END METHOD.

    /**
     * Purpose: Raises the FieldsDisabled event.
     * Notes:
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PROTECTED VOID OnFieldsDisabled (e AS System.EventArgs):

        THIS-OBJECT:FieldsDisabled:Publish (THIS-OBJECT, e) .

    END METHOD.

    /**
     * Purpose: Raises the FieldsEnabled event.
     * Notes:
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PROTECTED VOID OnFieldsEnabled (e AS System.EventArgs):

        THIS-OBJECT:FieldsEnabled:Publish (THIS-OBJECT, e) .

    END METHOD.

    /**
     * Purpose: Raises the FocusControlOnAddChanged event
     * Notes:
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PROTECTED VOID OnFocusControlOnAddChanged (e AS System.EventArgs):

        IF NOT VALID-OBJECT (e) THEN
            e = System.EventArgs:Empty .

        THIS-OBJECT:FocusControlOnAddChanged:Publish (THIS-OBJECT, e) .

    END METHOD.

    /**
     * Purpose: Raises the FocusControlOnUpdateChanged event
     * Notes:
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PROTECTED VOID OnFocusControlOnUpdateChanged (e AS System.EventArgs):

        IF NOT VALID-OBJECT (e) THEN
            e = System.EventArgs:Empty .

        THIS-OBJECT:FocusControlOnUpdateChanged:Publish (THIS-OBJECT, e) .

    END METHOD.

    /**
     * Purpose: Raises the CopyingRecordChanged
     * Notes:
     * @param e The System.EventArgs with the data for the event
     */
    METHOD PROTECTED VOID OnCopyingRecordChanged (e AS System.EventArgs):

        IF NOT VALID-OBJECT (e) THEN
            e = System.EventArgs:Empty .

        THIS-OBJECT:CopyingRecordChanged:Publish (THIS-OBJECT, e) .

    END METHOD .

    /**
     * Purpose: Raises the Enter event
     * Notes:
     * @param e The System.EventArgs with the data for this event
     */
    METHOD OVERRIDE PROTECTED VOID OnEnter (e AS System.EventArgs):

        DEFINE VARIABLE oForm AS System.Windows.Forms.Form NO-UNDO .

        SUPER:OnEnter(INPUT e).

        /* Mike Fechner, Consultingwerk Ltd. 14.10.2011
           While closing of a Form the FindForm() method may raise a
           System.NullReferenceException. When trown, there is no need to
           Activate the event handler as the Form is about to be closed. */
        DO ON ERROR UNDO, THROW:
            oForm = THIS-OBJECT:FindForm() .

            @SuppressUnusedWarnings.
            CATCH ex AS System.NullReferenceException:
                RETURN .
            END CATCH.
        END.

        IF VALID-OBJECT (oForm) AND TYPE-OF (oForm, ISmartWindowForm) THEN
            CAST (oForm, ISmartWindowForm):ActivateSmartControl (THIS-OBJECT) .

        ActivateEventHandler () .

    END METHOD.

    /**
     * Purpose: Event handler for the Leave event
     * Notes:   No longer overriding the OnLeave method as that cause unhandled
     *          .NET Exceptions when in update mode and the containing Form was closed
     * @param sender The reference to the object that raised the event
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PRIVATE VOID LeaveHandler (sender AS System.Object,
                                      e AS System.EventArgs):

        DeactivateEventHandler () .

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /**
     * Purpose: Raises the Load event
     * Notes:   Initializes the ViewerLogic class if property is set
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PROTECTED OVERRIDE VOID OnLoad (e AS System.EventArgs):

        DEFINE VARIABLE oForm AS System.Windows.Forms.Form NO-UNDO .

        SUPER:OnLoad (e) .

        oForm = THIS-OBJECT:FindForm () .

        IF VALID-OBJECT (oForm) AND NOT THIS-OBJECT:DesignTime THEN
            oForm:FormClosed:Subscribe (FormClosedHandler2) NO-ERROR .

        IF THIS-OBJECT:ViewerLogicClass > "":U AND
           NOT VALID-OBJECT (THIS-OBJECT:SmartGroupAssignSource) AND

           /* SCL-569 - don't attempt to re-initialize the viewer logic object
              in case for Viewer is re-Load'ed caused by terminal server changing
              UI style. */
           NOT VALID-OBJECT (THIS-OBJECT:ViewerLogicObject) THEN

           THIS-OBJECT:InitializeViewerLogicObject () .

        CATCH err AS Progress.Lang.Error:
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD .

    /**
     * Purpose: Publishes the SmartGroupAssignSourceChanged event
     * Notes:
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PROTECTED VOID OnSmartDataSourceChanged (e AS System.EventArgs):

        IF NOT VALID-OBJECT (e) THEN
            e = System.EventArgs:Empty .

        THIS-OBJECT:SmartDataSourceChanged:Publish (THIS-OBJECT, e) .

    END METHOD.

    /**
     * Purpose: Publishes the SmartGroupAssignSourceChanged event
     * Notes:
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PROTECTED VOID OnSmartGroupAssignSourceChanged (e AS System.EventArgs):

        IF NOT VALID-OBJECT (e) THEN
            e = System.EventArgs:Empty .

        THIS-OBJECT:SmartGroupAssignSourceChanged:Publish (THIS-OBJECT, e) .

    END METHOD.

    /**
     * Purpose: Publishes the SmartGroupAssignSourceChanged event
     * Notes:
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PROTECTED VOID OnSmartTableIOSourceChanged (e AS System.EventArgs):

        IF NOT VALID-OBJECT (e) THEN
            e = System.EventArgs:Empty .

        THIS-OBJECT:SmartTableIOSourceChanged:Publish (THIS-OBJECT, e) .

    END METHOD.


    /**
     * Purpose: Publishes the SmartTableIOStateChanged event
     * Notes:
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PROTECTED VOID OnSmartTableIOStateChanged (e AS SmartTableIOStateChangedEventArgs):

        IF NOT VALID-OBJECT (e) THEN
          ASSIGN e = NEW SmartTableIOStateChangedEventArgs (THIS-OBJECT:SmartTableIOState) .

        THIS-OBJECT:SmartTableIOStateChanged:Publish (THIS-OBJECT, e) .

    END METHOD.

    /**
     * Purpose: Raises the VisualValidating event
     * Notes:
     * @param e The System.EventArgs with the data for the event
     */
    METHOD PROTECTED VOID OnVisualValidating (e AS System.EventArgs):

        IF NOT VALID-OBJECT (e) THEN
            e = System.EventArgs:Empty .

        THIS-OBJECT:VisualValidating:Publish (THIS-OBJECT, e) .

    END METHOD .

    /**
     * Purpose: Event handler to the ParentChanged event
     *          Add EventHandlers to Child Controls inside the SmartViewerControl.
     *          This Method is Executed when the SmartViewerControl is added to a Form.
     * Notes:   Executed only one time.
     * @param sender The reference to the object that raised the event
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PRIVATE VOID ParentChangedHandler (sender AS System.Object,
                                              e AS System.EventArgs):

        IF lEventsSubscribed THEN
            RETURN .

        InitializeControlEventHandler () .

        IF THIS-OBJECT:DesignTime THEN
            DisableFields().

        FINALLY:
            lEventsSubscribed = TRUE .
        END FINALLY.

    END METHOD.

    /**
     * Purpose: Subscribe the EventHandler(s) to events of all controls added to
     *          the SmartViewerControl.
     *          This Method is called by the Method InitializeControlEventHandler ()
     *          of the SmartViewerControl.
     * Notes:
     * @param poParentControl The The Parent Control (THIS-OBJECT), nested Container
     */
    METHOD PROTECTED VOID RecurseInitializeControlEventHandler (poParentControl AS Control):

        {Consultingwerk/foreach.i Control oControl in poParentControl:Controls}

            /* Skip certain Controls */
            IF TYPE-OF(oControl, System.Windows.Forms.Label) OR
                TYPE-OF(oControl, UltraLabel) THEN
                 NEXT .

            /* Mike Fechner, Consultinwerk Ltd. 12.10.2011
               Basic Container Controls should always be enabled */
            IF TYPE-OF (oControl, UltraTabControl) OR
               TYPE-OF (oControl, SplitContainer) OR
               TYPE-OF (oControl, Panel) OR
               TYPE-OF (oControl, UltraPanel) OR
               TYPE-OF (oControl, UltraGroupBox) OR
               TYPE-OF (oControl, UltraExpandableGroupBox) THEN DO:

                SetControlEnabled (oControl, ControlEnabledEnum:Always) .

                /* Recursion */
                IF oControl:Controls:Count > 0 THEN
                    RecurseInitializeControlEventHandler (oControl) .

                NEXT .
            END.

            /* Mike Fechner, Consultingwerk Ltd. 18.09.2009
               When iterating the Controls during initialization and the
               ControlEnalbed mode has not been set, default to Update */
            /* Mike Fechner, Consultingwerk Ltd. 15.09.2010
               Controls that have a ReadOnly property (and this can only be tested
               using reflection as there is no common base type with a ReadOnly
               property) will not be enabled/disabled by the SmartComponent
               Library */
            IF NOT UNBOX (Consultingwerk.Util.ReflectionHelper:GetPropertyValue(oControl, "ReadOnly":U)) = TRUE AND
               oControl:Enabled AND NOT CAN-FIND (ttEnableFields WHERE ttEnableFields.ChildControl = oControl) THEN
                SetControlEnabled (oControl, ControlEnabledEnum:Update) .

            /* Mike Fechner, Consultingwerk Ltd. 06.07.2009
               Bug 1887: CheckedChanged instead of TextChanged for CheckBoxes, Infragistics and Microsoft */
            IF TYPE-OF (oControl, Infragistics.Win.UltraWinEditors.UltraCheckEditor) THEN
                oCheckChangedIGHandler:Add (CAST (oControl, Infragistics.Win.UltraWinEditors.UltraCheckEditor)) .
            ELSE IF TYPE-OF (oControl, System.Windows.Forms.CheckBox) THEN
                oCheckChangedMSHandler:Add (CAST(oControl, System.Windows.Forms.CheckBox)) .
            ELSE
                oTextChangedHandler:Add (oControl) .

            /* UltraDateTimeEditor event subscription */
            IF TYPE-OF (oControl, Infragistics.Win.UltraWinEditors.UltraDateTimeEditor) THEN
            DO:
                CAST (oControl, Infragistics.Win.UltraWinEditors.UltraDateTimeEditor):AfterCloseUp:Subscribe (EditorDropDownAfterCloseUpHandler) .
                CAST (oControl, Infragistics.Win.UltraWinEditors.UltraDateTimeEditor):AfterDropDown:Subscribe (EditorDropDownAfterDropDownHandler) .
            END.

            /* Enter event subscription */
            IF TYPE-OF (oControl, Infragistics.Win.UltraWinEditors.UltraWinEditorMaskedControlBase) THEN
                CAST (oControl, Infragistics.Win.UltraWinEditors.UltraWinEditorMaskedControlBase):Enter:Subscribe (EnterEditorControlHandler).
            ELSE IF TYPE-OF (oControl, Infragistics.Win.UltraWinEditors.TextEditorControlBase) THEN
                    CAST (oControl, Infragistics.Win.UltraWinEditors.TextEditorControlBase):Enter:Subscribe (EnterEditorControlHandler).
                ELSE IF TYPE-OF (oControl, System.Windows.Forms.TextBoxBase) THEN
                        CAST (oControl, System.Windows.Forms.TextBoxBase):Enter:Subscribe (EnterEditorControlHandler).

            /* Mike Fechner, Consultingwerk Ltd. 19.05.2015
               SCL-814: Optionally call CustomInitializeControlHandler */
            IF THIS-OBJECT:OptionalFeatures:CallCustomInitializeControlHandler THEN
                THIS-OBJECT:CustomInitializeControlHandler (oControl) .

            /* Set Binding udpate mode, set Change @18192 */
            {Consultingwerk/foreach.i Binding oBinding in oControl:DataBindings}
                oBinding:DataSourceUpdateMode = DataSourceUpdateMode:OnPropertyChanged .
            END.

            /* Recursion */
            IF oControl:Controls:Count > 0 THEN
                RecurseInitializeControlEventHandler (oControl) .
        END.

    END METHOD.

    /**
     * Purpose: Recurses through the Controls in this SmartViewerControl and nested
     *          controls searching for a Control that is bound to the named
     *          BindingProperty (a field attached to the ProBindingSource).
     * Notes:
     * @param poParentControl The reference to the ParentControl
     * @param pcBindingPropertyName The name of the field in the ProBindingSource
     * @return The reference to the resulting Control
     */
    METHOD PROTECTED System.Windows.Forms.Control RecurseLocateBindingControl (poParentControl AS System.Windows.Forms.Control,
                                                                               pcBindingPropertyName AS CHARACTER):

        DEFINE VARIABLE i                   AS INTEGER                                 NO-UNDO.
        DEFINE VARIABLE j                   AS INTEGER                                 NO-UNDO.
        DEFINE VARIABLE oControl            AS System.Windows.Forms.Control            NO-UNDO .
        DEFINE VARIABLE oBindingsCollection AS System.Windows.Forms.BindingsCollection NO-UNDO .
        DEFINE VARIABLE cPropertyName       AS CHARACTER                               NO-UNDO.
        DEFINE VARIABLE cBindingField       AS CHARACTER                               NO-UNDO.
        DEFINE VARIABLE oResult             AS System.Windows.Forms.Control            NO-UNDO .

        DO i = 0 TO poParentControl:Controls:Count - 1:
            ASSIGN
                oControl            = poParentControl:Controls[i]
                oBindingsCollection = oControl:DataBindings .

            /* Recurse Container Controls */
            IF oControl:Controls:Count > 0 THEN
            DO:
                oResult = RecurseLocateBindingControl (oControl, pcBindingPropertyName) .

                IF VALID-OBJECT (oResult) THEN
                    RETURN oResult .
            END.

            IF oBindingsCollection:Count = 0 THEN
                NEXT .

            DO j = (oBindingsCollection:Count - 1) TO 0 BY -1:
                ASSIGN
                    cPropertyName = oBindingsCollection:Item[j]:PropertyName
                    cBindingField = oControl:DataBindings[cPropertyName]:BindingMemberInfo:BindingField .

                IF cBindingField = pcBindingPropertyName THEN
                    RETURN oControl .
            END.
        END.

        RETURN ? .

        FINALLY:
            ASSIGN
                oBindingsCollection = ?
                oControl            = ? .
        END FINALLY.

    END METHOD.

    /**
     * Purpose: Recurses through the Controls in this SmartViewerControl and nested
     *          controls searching for all Controls that are bound to the named
     *          BindingProperty (a field attached to the ProBindingSource).
     * Notes:   The controls will be added to the list
     * @param poParentControl The reference to the ParentControl
     * @param poList The list to add the control references to
     * @param pcBindingPropertyName The name of the field in the ProBindingSource
     */
    METHOD PROTECTED VOID RecurseLocateBindingControls (poParentControl AS System.Windows.Forms.Control,
                                                        poList AS "System.Collections.Generic.List<System.Windows.Forms.Control>",
                                                        pcBindingPropertyName AS CHARACTER):

        DEFINE VARIABLE i                   AS INTEGER                                 NO-UNDO.
        DEFINE VARIABLE j                   AS INTEGER                                 NO-UNDO.
        DEFINE VARIABLE oControl            AS System.Windows.Forms.Control            NO-UNDO .
        DEFINE VARIABLE oBindingsCollection AS System.Windows.Forms.BindingsCollection NO-UNDO .
        DEFINE VARIABLE cPropertyName       AS CHARACTER                               NO-UNDO.
        DEFINE VARIABLE cBindingField       AS CHARACTER                               NO-UNDO.

        controlLoop: DO i = 0 TO poParentControl:Controls:Count - 1:
            ASSIGN
                oControl            = poParentControl:Controls[i]
                oBindingsCollection = oControl:DataBindings .

            /* Recurse Container Controls */
            IF oControl:Controls:Count > 0 THEN
                RecurseLocateBindingControls (oControl, poList, pcBindingPropertyName) .

            IF oBindingsCollection:Count = 0 THEN
                NEXT .

            DO j = (oBindingsCollection:Count - 1) TO 0 BY -1:
                ASSIGN
                    cPropertyName = oBindingsCollection:Item[j]:PropertyName
                    cBindingField = oControl:DataBindings[cPropertyName]:BindingMemberInfo:BindingField .

                IF cBindingField = pcBindingPropertyName THEN DO:
                    poList:Add (oControl) .
                    NEXT controlLoop .
                END.
            END.
        END.

        FINALLY:
            ASSIGN
                oBindingsCollection = ?
                oControl            = ? .
        END FINALLY.

    END METHOD.

    /**
     * Purpose: Performs the Lookup operation in all contained SmartLookup Controls
     * Notes:   Called at the begin of a Save operations
     * @param poParentControl The parent Control for the current iteration
     */
    METHOD PROTECTED VOID RecursePerformLookup (poParentControl AS Control):

        {Consultingwerk/foreach.i System.Windows.Forms.Control oControl in poParentControl:Controls}

            IF TYPE-OF (oControl, SmartLookup) THEN
                CAST (oControl, SmartLookup):PerformLookupIfActive () .

            IF oControl:Controls:Count > 0 THEN
                RecursePerformLookup (oControl) .
        END.

    END METHOD.

    /**
     * Purpose: Internal method to replace the data bindings of all contained
     *          Controls with the actual binding source provided by a SmartDataSource.
     * Notes:   Initially called by the Method AttachBindingSource () of the
     *          SmartViewerControl.
     *          This method also applies field security from the IFieldAuthorizationProvider
     *          implementation. When a Control is hidden, the label is also hidden. The
     *          Label for a Control is located using the GetNextControl method of the
     *          parent control, thus it's required that the label is standing exactly
     *          before the Control in the Tab-Order of the Viewer.
     * @param poParentControl Current Container to perform Action on
     * @param poBindingSource New BindingSource Instance passed to the Class
     */
    METHOD PROTECTED VOID RecurseReplaceBindingSource (poParentControl AS Control,
                                                       poBindingSource AS Progress.Data.BindingSource):

        DEFINE VARIABLE i                   AS INTEGER                                 NO-UNDO .
        DEFINE VARIABLE j                   AS INTEGER                                 NO-UNDO .
        DEFINE VARIABLE oControl            AS System.Windows.Forms.Control            NO-UNDO .
        DEFINE VARIABLE oBindingsCollection AS System.Windows.Forms.BindingsCollection NO-UNDO .
        DEFINE VARIABLE cPropertyName       AS CHARACTER                               NO-UNDO .
        DEFINE VARIABLE cBindingField       AS CHARACTER                               NO-UNDO .
        DEFINE VARIABLE cBufferFieldName    AS CHARACTER                               NO-UNDO .
        DEFINE VARIABLE oRestricted         AS FieldRestrictionEnum                    NO-UNDO .
        DEFINE VARIABLE oLabel              AS System.Windows.Forms.Control            NO-UNDO .

        DO i = 0 TO poParentControl:Controls:Count - 1:
            ASSIGN
                oControl            = poParentControl:Controls[i]
                oBindingsCollection = oControl:DataBindings .

            /* Recurse Container Controls */
            IF oControl:Controls:Count > 0 THEN
                RecurseReplaceBindingSource (oControl, poBindingSource) .

            IF oBindingsCollection:Count = 0 THEN
                NEXT .

            fieldLoop: DO j = (oBindingsCollection:Count - 1) TO 0 BY -1:
                ASSIGN
                    cPropertyName = oBindingsCollection:Item[j]:PropertyName
                    cBindingField = oControl:DataBindings[cPropertyName]:BindingMemberInfo:BindingField .

                IF VALID-OBJECT (oFieldAuthorizationProvider) THEN DO:

                    IF cBindingField > "":U THEN DO ON ERROR UNDO, THROW:
                        IF VALID-HANDLE (poBindingSource:HANDLE) THEN
                        cBufferFieldName = BindingSourceHelper:BindingBufferFieldName (poBindingSource,
                                                                                       cBindingField) .
                        ELSE
                            IF VALID-OBJECT (THIS-OBJECT:SmartDataSource) AND
                               TYPE-OF (THIS-OBJECT:SmartDataSource, SmartDatasetAdapter) AND
                               VALID-HANDLE (CAST (THIS-OBJECT:SmartDataSource, Consultingwerk.SmartComponents.Base.SmartDatasetAdapter):Dataset) AND
                               CAST (THIS-OBJECT:SmartDataSource, Consultingwerk.SmartComponents.Base.SmartDatasetAdapter):EntityTable > "":U THEN

                            cBufferFieldName = BindingSourceHelper:BindingBufferFieldName (CAST (THIS-OBJECT:SmartDataSource, Consultingwerk.SmartComponents.Base.SmartDatasetAdapter):dataset,
                                                                                           cBindingField,

                                                                                           TRIM (SUBSTITUTE ("&1,&2":U,
                                                                                                             CAST (THIS-OBJECT:SmartDataSource, Consultingwerk.SmartComponents.Base.SmartDatasetAdapter):EntityTable,
                                                                                                             CAST (THIS-OBJECT:SmartDataSource, Consultingwerk.SmartComponents.Base.SmartDatasetAdapter):EntityView),
                                                                                                 ",":U)) .
                        ELSE
                            NEXT fieldLoop .

                        oRestricted = oFieldAuthorizationProvider:IsFieldRestricted (THIS-OBJECT,
                                                                                     cBufferFieldName,
                                                                                     THIS-OBJECT:SecurityKey) .

                        CASE oRestricted:
                            WHEN FieldRestrictionEnum:Disabled THEN
                                THIS-OBJECT:SetControlSecurityEnabled (oControl, ControlEnabledEnum:Never) .

                            WHEN FieldRestrictionEnum:AddOnly THEN
                                THIS-OBJECT:SetControlSecurityEnabled (oControl, ControlEnabledEnum:Add) .

                            WHEN FieldRestrictionEnum:UpdateOnly THEN
                                THIS-OBJECT:SetControlSecurityEnabled (oControl, ControlEnabledEnum:UpdateOnly) .

                            WHEN FieldRestrictionEnum:Hidden THEN DO:
                                oLabel = poParentControl:GetNextControl (oControl, FALSE) .

                                IF TYPE-OF (oLabel, System.Windows.Forms.Label) OR
                                   TYPE-OF (oLabel, Infragistics.Win.Misc.UltraLabel) THEN

                                    oLabel:Visible = FALSE .

                                oControl:Visible = FALSE .
                            END.
                        END CASE .

                        CATCH err AS Progress.Lang.Error:
                            /* ignore errors from BindingSourceHelper:BindingBufferFieldName */
                        END CATCH.
                    END.
                END.

                IF NOT lDesignBindingSourceDisposed AND NOT VALID-OBJECT (oDesignBindingSource) AND
                    VALID-OBJECT (oBindingsCollection:Item[j]:DataSource) AND
                    TYPE-OF (oBindingsCollection:Item[j]:DataSource, Progress.Data.BindingSource) THEN

                    ASSIGN oDesignBindingSource = CAST (oBindingsCollection:Item[j]:DataSource, Progress.Data.BindingSource).


                oControl:DataBindings:Remove(oControl:DataBindings[cPropertyName]) .

                IF VALID-OBJECT (poBindingSource) THEN DO ON ERROR UNDO, THROW:
                    oControl:DataBindings:Add (cPropertyName,
                                               poBindingSource,
                                               cBindingField,
                                               TRUE,
                                               DataSourceUpdateMode:OnPropertyChanged) .

                    CATCH err AS Progress.Lang.Error :
                        MESSAGE err:GetMessage(1) SKIP (2)
                                SUBSTITUTE ("SmartViewerControl: &1 Control: &2":U, THIS-OBJECT:GetType():FullName, oControl:Name) SKIP
                                SUBSTITUTE ("Propertyname: &1":U, cPropertyName) SKIP
                                SUBSTITUTE ("Fieldname: &1":U, cBindingfield)
                            VIEW-AS ALERT-BOX ERROR
                            BUTTONS OK-CANCEL
                            TITLE "Error while reassining ProBindingSource":U
                            UPDATE lOk AS LOGICAL.

                        IF NOT lOk THEN
                            RETURN .
                    END CATCH.
            END.
        END.
        END.

        FINALLY:
            ASSIGN
                oBindingsCollection = ?
                oControl            = ? .
        END FINALLY.

    END METHOD.

    /**
     * Purpose: Recurese Restore the Current Values of the Controls on the Viewer
     *          after a new DataRow is created.
     *          This Method is called by the internal Method RestoreValuesForCopy ().
     *          Parameters:
     * Notes:
     * @param poParentControl The parent control of the current iteration
     */
    METHOD PROTECTED VOID RecurseRestoreCurrentValues (poParentControl AS Control):

        DEFINE VARIABLE oControl            AS System.Windows.Forms.Control            NO-UNDO .
        DEFINE VARIABLE oBindingsCollection AS System.Windows.Forms.BindingsCollection NO-UNDO .
        DEFINE VARIABLE cPropertyName       AS CHARACTER                               NO-UNDO .
        DEFINE VARIABLE oType               AS System.Type                             NO-UNDO .
        DEFINE VARIABLE oProperty           AS System.Reflection.PropertyInfo          NO-UNDO .

        DEFINE VARIABLE i                   AS INTEGER                                 NO-UNDO .
        DEFINE VARIABLE j                   AS INTEGER                                 NO-UNDO .

        DO i = 0 TO poParentControl:Controls:Count - 1:
            /* Write Changes to the current field */
            ASSIGN
                oControl            = poParentControl:Controls[i]
                oBindingsCollection = oControl:DataBindings.

            inner: DO j = 0 TO oControl:DataBindings:Count - 1:
                ASSIGN
                    cPropertyName = oBindingsCollection:Item[j]:PropertyName
                    oType         = oControl:GetType()
                    oProperty     = oType:GetProperty (cPropertyName) .

                IF NOT VALID-OBJECT(oProperty) THEN
                    NEXT inner.

                FIND ttSavedViewerValues WHERE ttSavedViewerValues.RecordOwner  = THIS-OBJECT
                    AND ttSavedViewerValues.ChildControl = oControl
                    AND ttSavedViewerValues.PropertyName = cPropertyName
                    NO-ERROR .

                IF NOT AVAILABLE ttSavedViewerValues THEN
                    NEXT inner.

                oProperty:SetValue(oControl, CAST(ttSavedViewerValues.PropertyValue, System.Object), ?) .
                oControl:DataBindings[cPropertyName]:WriteValue().

                DELETE ttSavedViewerValues .
            END.

            IF oControl:Controls:Count > 0 THEN
                RecurseRestoreCurrentValues (oControl) .
        END.

    END METHOD.

    /**
     * Purpose: Stops the current timer of all Lookups
     * Notes:   Invoked when a PositionChanged event occurs
     * @param poParentControl The parent Control of the current iteration
     */
    METHOD PROTECTED VOID RecurseLookupStopTimer (poParentControl AS System.Windows.Forms.Control):

        DEFINE VARIABLE oControl AS System.Windows.Forms.Control NO-UNDO .
        DEFINE VARIABLE i        AS INTEGER                      NO-UNDO .

        DO i = 0 TO poParentControl:Controls:Count - 1:
            /* Write Changes to the current field */
            ASSIGN
                oControl = poParentControl:Controls[i].

            IF TYPE-OF (oControl, SmartLookup) THEN
                CAST (oControl, SmartLookup):StopTimer () .

            IF oControl:Controls:Count > 0 THEN
                RecurseLookupStopTimer (oControl) .
        END.
    END METHOD.

    /**
     * Purpose: Recurese Store the Current Values of the Controls on the Viewer
     *          before a new DataRow is created.
     *          This Method is called by the internal Method StoreValuesForCopy ().
     * Notes:
     * @param poParentControl The parent Control of the current iteration
     */
    METHOD PROTECTED VOID RecurseStoreCurrentValues (poParentControl AS Control):

        DEFINE VARIABLE oControl            AS System.Windows.Forms.Control            NO-UNDO .
        DEFINE VARIABLE oBindingsCollection AS System.Windows.Forms.BindingsCollection NO-UNDO .
        DEFINE VARIABLE cPropertyName       AS CHARACTER                               NO-UNDO .
        DEFINE VARIABLE oType               AS System.Type                             NO-UNDO .
        DEFINE VARIABLE oProperty           AS System.Reflection.PropertyInfo          NO-UNDO .

        DEFINE VARIABLE i                   AS INTEGER                                 NO-UNDO .
        DEFINE VARIABLE j                   AS INTEGER                                 NO-UNDO .

        DO i = 0 TO poParentControl:Controls:Count - 1:
            /* Write Changes to the current field */
            ASSIGN
                oControl            = poParentControl:Controls[i]
                oBindingsCollection = oControl:DataBindings.

            inner: DO j = 0 TO oControl:DataBindings:Count - 1:
                ASSIGN
                    cPropertyName = oBindingsCollection:Item[j]:PropertyName
                    oType         = oControl:GetType()
                    oProperty     = oType:GetProperty (cPropertyName) .

                IF NOT VALID-OBJECT(oProperty) THEN
                    NEXT inner.

                CREATE ttSavedViewerValues .
                ASSIGN
                    ttSavedViewerValues.RecordOwner   = THIS-OBJECT
                    ttSavedViewerValues.ChildControl  = oControl
                    ttSavedViewerValues.PropertyName  = cPropertyName
                    ttSavedViewerValues.PropertyValue = oProperty:GetValue (oControl, ?) .
            END.

            IF oControl:Controls:Count > 0 THEN
                RecurseStoreCurrentValues (oControl) .
        END.

    END METHOD.

    /**
     * Purpose: Removes a Control from the list of Controls that will be enabled
     *          and disabled by the Viewer based on the current Viewer state
     * @param poControl The Control which shall be removed from the list of managed Controls
     */
    METHOD PROTECTED VOID RemoveControlEnabled (poControl AS Control):

        FIND ttEnableFields WHERE ttEnableFields.ChildControl = poControl
                              AND ttEnableFields.RecordOwner  = THIS-OBJECT NO-ERROR .

        IF AVAILABLE ttEnableFields THEN
            DELETE ttEnableFields .

    END METHOD .

    /**
     * Purpose: Removes multiple Controls from the list of Controls that will be enabled
     *          and disabled by the Viewer based on the current Viewer state
     * @param poControl1 The first Control which shall be removed from the list of managed Controls
     * @param poControl2 The second Control which shall be removed from the list of managed Controls
     */
    METHOD PROTECTED VOID RemoveControlEnabled (poControl1 AS Control,
                                                poControl2 AS Control):

        THIS-OBJECT:RemoveControlEnabled (poControl1) .
        THIS-OBJECT:RemoveControlEnabled (poControl2) .

    END METHOD .

    /**
     * Purpose: Removes multiple Controls from the list of Controls that will be enabled
     *          and disabled by the Viewer based on the current Viewer state
     * @param poControl1 The first Control which shall be removed from the list of managed Controls
     * @param poControl2 The second Control which shall be removed from the list of managed Controls
     * @param poControl3 The third Control which shall be removed from the list of managed Controls
     */
    METHOD PROTECTED VOID RemoveControlEnabled (poControl1 AS Control,
                                                poControl2 AS Control,
                                                poControl3 AS Control):

        THIS-OBJECT:RemoveControlEnabled (poControl1) .
        THIS-OBJECT:RemoveControlEnabled (poControl2) .
        THIS-OBJECT:RemoveControlEnabled (poControl3) .

    END METHOD .

    /**
     * Purpose: Removes multiple Controls from the list of Controls that will be enabled
     *          and disabled by the Viewer based on the current Viewer state
     * @param poControl1 The first Control which shall be removed from the list of managed Controls
     * @param poControl2 The second Control which shall be removed from the list of managed Controls
     * @param poControl3 The third Control which shall be removed from the list of managed Controls
     * @param poControl4 The fourth Control which shall be removed from the list of managed Controls
     */
    METHOD PROTECTED VOID RemoveControlEnabled (poControl1 AS Control,
                                                poControl2 AS Control,
                                                poControl3 AS Control,
                                                poControl4 AS Control):

        THIS-OBJECT:RemoveControlEnabled (poControl1) .
        THIS-OBJECT:RemoveControlEnabled (poControl2) .
        THIS-OBJECT:RemoveControlEnabled (poControl3) .
        THIS-OBJECT:RemoveControlEnabled (poControl4) .

    END METHOD .

    /**
     * Purpose: Removes multiple Controls from the list of Controls that will be enabled
     *          and disabled by the Viewer based on the current Viewer state
     * @param poControl1 The first Control which shall be removed from the list of managed Controls
     * @param poControl2 The second Control which shall be removed from the list of managed Controls
     * @param poControl3 The third Control which shall be removed from the list of managed Controls
     * @param poControl4 The fourth Control which shall be removed from the list of managed Controls
     * @param poControl5 The fifth Control which shall be removed from the list of managed Controls
     */
    METHOD PROTECTED VOID RemoveControlEnabled (poControl1 AS Control,
                                                poControl2 AS Control,
                                                poControl3 AS Control,
                                                poControl4 AS Control,
                                                poControl5 AS Control):

        THIS-OBJECT:RemoveControlEnabled (poControl1) .
        THIS-OBJECT:RemoveControlEnabled (poControl2) .
        THIS-OBJECT:RemoveControlEnabled (poControl3) .
        THIS-OBJECT:RemoveControlEnabled (poControl4) .
        THIS-OBJECT:RemoveControlEnabled (poControl5) .

    END METHOD .

    /**
     * Purpose: Removes a SmartTableIOSource from the SmartDataAdapter
     *          instance
     * Notes:   Enables multiple SmartTableIOSource to the SmartDataAdapter.
     * @param poSmartTableIOSource The SmartTableIOSource to remove from the Viewer
     */
    METHOD PUBLIC VOID RemoveSmartTableIOSource (poSmartTableIOSource AS ISmartTableIOSource):

        DEFINE VARIABLE lDeleteDefault AS LOGICAL NO-UNDO INITIAL FALSE .

        IF poSmartTableIOSource = oSmartTableIOSource THEN
            lDeleteDefault = TRUE .

         IF VALID-OBJECT (poSmartTableIOSource) THEN
            poSmartTableIOSource:DeregisterSmartTableIOTarget (THIS-OBJECT) .

        IF lDeleteDefault THEN
            ASSIGN oSmartTableIOSource = ? .

    END METHOD.

    /**
     * Purpose: Restore the Current Values of all Controls on a SmartViewerControl
     *          and in all SmartGroupAssignTargets.
     *          This Method is called by CreateRecord from the current Class.
     *          Enforced by Interface ISmartGroupAssignTarget.
     * Notes:
     */
    METHOD PUBLIC VOID RestoreValuesForCopy ():

        RecurseRestoreCurrentValues (THIS-OBJECT) .

        FOR EACH ttSmartGroupAssignTarget WHERE ttSmartGroupAssignTarget.RecordOwner = THIS-OBJECT:
            CAST(ttSmartGroupAssignTarget.SmartGroupAssignTarget, ISmartGroupAssignTarget):RestoreValuesForCopy () .
        END.

    END METHOD.

    /**
     * Purpose: Saves the current changes to the database.
     * Notes:   This Method is called by the Toolbar Classes SmartToolbarController
     *          or SmartToolStripContainer by pressing the corresponding Tool.
     *          Enforced by Interface ISmartDataTarget or ISmartGroupAssignTarget.
     */
    METHOD PUBLIC VOID SaveChanges ():

        DEFINE VARIABLE e AS System.ComponentModel.CancelEventArgs NO-UNDO .

        THIS-OBJECT:SavingChanges = TRUE .

        RecursePerformLookup (THIS-OBJECT) .

        e = NEW System.ComponentModel.CancelEventArgs () .
        OnBeforeSaveChanges (e) .

        IF VALID-OBJECT (e) AND e:Cancel THEN
            RETURN .

        /* Mike Fechner, Consultingwerk Ltd. 20.06.2013
           Raise the VisualValidating event, so that subscribers might do LEAVE of fields first */
        THIS-OBJECT:OnVisualValidating (System.EventArgs:Empty) .

        /* Call visual validation code */
        THIS-OBJECT:VisualValidate().

        /* Perform the Control's validation */
        IF THIS-OBJECT:ValidateChildren() = FALSE THEN DO:

            IF VALID-OBJECT(oSmartGroupAssignSource) THEN
                UNDO, THROW NEW ControlValidateFailedException () .
            ELSE
                RETURN .
        END .

        /* Save changes in every registered SmartGroupAssignTarget */
        FOR EACH ttSmartGroupAssignTarget WHERE ttSmartGroupAssignTarget.RecordOwner = THIS-OBJECT NO-LOCK ON ERROR UNDO, THROW:
            CAST(ttSmartGroupAssignTarget.SmartGroupAssignTarget, ISmartGroupAssignTarget):SaveChanges().

            /* Mike Fechner, Consultingwerk Ltd. 25.09.2013
               When a Group assign target raises the ControlValidateFailedException
               then we stop saving from the Viewer */
            @SuppressUnusedWarnings.
            CATCH valex AS ControlValidateFailedException:
                RETURN .
            END CATCH.
        END.

        IF VALID-OBJECT(oSmartGroupAssignSource) THEN
            RETURN.

        /* Save record in SmartDataSource */
        oSmartDataSource:UpdateRow () .

        /* Fields will only be disabled in case of success. Any error from above
           will be catched by the CATCH block below */
        IF THIS-OBJECT:SaveMode THEN
            THIS-OBJECT:EnableFields (Consultingwerk.SmartComponents.Enum.EnableFieldsEnum:Update) .
        ELSE
            THIS-OBJECT:DisableFields() .

        OnAfterSaveChanges (System.EventArgs:Empty) .

        /* Mike Fechner, Consultingwerk Ltd. 12.08.2011
           When a save operation is completed, this Viewer is no longer in Add mode
           Without this assignment, the first record in a query would disappear when
           it has been edited directly after the creation and the edit is cancelled.
           This is because the CancelUpdate would invoke the CancelCreateRecord in
           the DataSource */
        ASSIGN THIS-OBJECT:AddingRecord  = FALSE
               THIS-OBJECT:CopyingRecord = FALSE .

        /* Mike Fechner, Consultingwerk Ltd. 10.12.2013
           Silently ignoring the CancelException thrown by the SmartDatasetAdapter
           when the BeforeUpdateRecord or BeforeAssignRecord events have been
           cancelled */
        @SuppressUnusedWarnings.
        CATCH cex AS CancelException:
            RETURN .
        END CATCH.

        CATCH err AS Progress.Lang.Error :
            IF VALID-OBJECT(oSmartGroupAssignSource) THEN
                UNDO, THROW err.
            ELSE
            DO:
                THIS-OBJECT:HandleValidationError (err) .
            END.
        END CATCH.

        FINALLY:
            THIS-OBJECT:SavingChanges = FALSE .
        END FINALLY.

    END METHOD.

    /**
     * Purpose: Display ErrorMessages shows a predefined Error Dialog and is called
     *          from the Method SaveChanges () of the SmartViewerControl Class.
     * Notes:
     * @param e The error instance to visualize
     */
    METHOD PROTECTED VOID DisplayError (e AS Progress.Lang.Error) :

        THIS-OBJECT:DisplayError (e, TRUE) .

    END METHOD .

    /**
     * Purpose: Display ErrorMessages shows a predefined Error Dialog and is called
     *          from the Method SaveChanges () of the SmartViewerControl Class.
     * Notes:
     * @param e The error instance to visualize
     * @param plEnsureVisible Logical flag indicating if the viewer should ensure it's visible
     */
    METHOD PROTECTED VOID DisplayError (e AS Progress.Lang.Error,
                                        plEnsureVisible AS LOGICAL):

        DEFINE VARIABLE cFormTitle AS CHARACTER NO-UNDO.

        /* Mike Fechner, Consultingwerk Ltd. 01.01.2010
           Ensure that the viewer is visible */
        IF plEnsureVisible THEN
            THIS-OBJECT:EnsureVisible () .

        /* Mike Fechner, Consultingwerk Ltd. 06.11.2009
           Add the Title of the Form containing this Viewer to the
           message box. */
        IF VALID-OBJECT (THIS-OBJECT:FindForm()) THEN DO:
            ASSIGN cFormTitle = THIS-OBJECT:FindForm():Text .

            IF FrameworkSettings:DebugMode THEN DO:
                IF cFormTitle > "":U THEN
                    ASSIGN cFormTitle = cFormTitle + " / ":U + ErrorHelper:ErrorTitle (e) .
                ELSE
                    ASSIGN cFormTitle = ErrorHelper:ErrorTypeName(e) .
            END.
        END.

        IF cFormTitle > "":U THEN
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (e, cFormTitle) .
        ELSE
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (e) .

    END METHOD.

    /**
     * Purpose: Saves pending changes when a Form is deactivated
     * Notes:
     * @param plShowMessage When TRUE a message is asking the user if values should be saved. When FALSE values will be saved silently
     * @return True when the user did choose not to save or the save was successful, False when the control remains in Update state
     */
    METHOD PUBLIC LOGICAL SaveChangesOnDeactivateForm (plShowMessage AS LOGICAL):

        DEFINE VARIABLE oResult   AS DialogResultEnum NO-UNDO.

        /* Mike Fechner, Consultingwerk Ltd. 21.02.2010
           A GroupAssign Target does not take the decision, this will (potentially)
           be run in the GroupAssign Source */
        IF VALID-OBJECT(oSmartGroupAssignSource) THEN
            RETURN TRUE .

        IF THIS-OBJECT:SmartTableIOState = TableIOStateEnum:ModifyingData THEN
        DO:
            IF plShowMessage THEN DO:
                THIS-OBJECT:FindForm():Activate () .

                oResult = Consultingwerk.Util.MessageFormHelper:ShowMessage
                    (SmartComponentLibraryCustomizer:SmartViewerControl_UpdateActiveWhileFormDeactivate,
                     THIS-OBJECT:FindForm():Text,
                     "":U,
                     MessageFormImages:ImageQuestion,
                     MessageFormDefaults:LabelYesNo,
                     MessageFormDefaults:GetDialogResultYesNo()) .

                IF oResult:Equals (DialogResultEnum:DialogResultNo) THEN
                    RETURN TRUE .
            END.

            THIS-OBJECT:SaveChanges() .

            IF THIS-OBJECT:SmartTableIOState = TableIOStateEnum:ModifyingData THEN
                RETURN FALSE .
        END.

        RETURN TRUE .

    END METHOD .

    /**
     * Purpose: Sets the Enable Mode based on Security Constraints for a given Control
     * Notes:   Possible Values for pcEnabled (ControlEnabledEnum):
     *          Add, Always, Never, Update, UpdateOnly
     * @param poControl The Control which shall be set to the passed in Mode
     * @param pcEnabled Mode to be set as defined in ControlEnabledEnum
     */
    METHOD PRIVATE VOID SetControlSecurityEnabled (poControl AS Control, pcEnabled AS CHARACTER):

        DEFINE BUFFER bttEnableFields FOR ttEnableFields .

        IF pcEnabled <> ControlEnabledEnum:Add AND
            pcEnabled <> ControlEnabledEnum:Always AND
            pcEnabled <> ControlEnabledEnum:Never AND
            pcEnabled <> ControlEnabledEnum:Update AND
            pcEnabled <> ControlEnabledEnum:UpdateOnly THEN

            UNDO, THROW NEW AppError ("Invalid ControlEnabled Mode "{&TRAN} + pcEnabled, 0) .

        FIND bttEnableFields WHERE bttEnableFields.ChildControl = poControl
            NO-ERROR .

        IF NOT AVAILABLE bttEnableFields THEN
        DO:
            CREATE bttEnableFields.
            ASSIGN
                bttEnableFields.ChildControl       = poControl
                bttEnableFields.RecordOwner        = THIS-OBJECT
                bttEnableFields.SecurityEnableMode = ControlEnabledEnum:Always
                bttEnableFields.EnableMode         = ControlEnabledEnum:Update .
        END.

        ASSIGN
            bttEnableFields.SecurityEnableMode = pcEnabled .

        /* Mike Fechner, Consultingwerk Ltd. 07.08.2014
           Apply Enabled mode to Control */
        IF THIS-OBJECT:SmartTableIOState <> TableIOStateEnum:FieldsDisabled THEN

            poControl:Enabled = IsControlEnabled (bttEnableFields.SecurityEnableMode,
                                                  bttEnableFields.EnableMode,
                                                  THIS-OBJECT:AddingRecord) .

        RELEASE bttEnableFields .

    END METHOD .

    /**
     * Purpose: Sets the Enable Mode for a given Control inside the SmartViewerControl.
     *          Called by the Method InitializeControlEventHandler () of a
     *          SmartViewerControl derived Class.
     * Notes:   Possible Values for pcEnabled (ControlEnabledEnum):
     *          Add, Always, Never, Update, UpdateOnly
     * @param poControl The Control which shall be set to the passed in Mode
     * @param pcEnabled Mode to be set as defined in ControlEnabledEnum
     */
    METHOD PUBLIC VOID SetControlEnabled (poControl AS Control, pcEnabled AS CHARACTER):

        DEFINE BUFFER bttEnableFields FOR ttEnableFields .

        DEFINE VARIABLE i        AS INTEGER NO-UNDO .
        DEFINE VARIABLE lEnabled AS LOGICAL NO-UNDO .

        IF pcEnabled <> ControlEnabledEnum:Add AND
            pcEnabled <> ControlEnabledEnum:Always AND
            pcEnabled <> ControlEnabledEnum:Never AND
            pcEnabled <> ControlEnabledEnum:Update AND
            pcEnabled <> ControlEnabledEnum:UpdateOnly THEN

            UNDO, THROW NEW AppError ("Invalid ControlEnabled Mode "{&TRAN} + pcEnabled, 0) .

        FIND bttEnableFields WHERE bttEnableFields.ChildControl = poControl
            NO-ERROR .

        IF NOT AVAILABLE bttEnableFields THEN
        DO:
            CREATE bttEnableFields.
            ASSIGN
                bttEnableFields.ChildControl       = poControl
                bttEnableFields.RecordOwner        = THIS-OBJECT
                bttEnableFields.SecurityEnableMode = ControlEnabledEnum:Always
                bttEnableFields.EnableMode         = ControlEnabledEnum:Update .
        END.

        ASSIGN
            bttEnableFields.EnableMode = pcEnabled .

        /* Mike Fechner, Consultingwerk Ltd. 07.08.2014
           Apply Enabled mode to Control */
        IF THIS-OBJECT:SmartTableIOState <> TableIOStateEnum:FieldsDisabled THEN DO:

            ASSIGN lEnabled = THIS-OBJECT:IsControlEnabled (bttEnableFields.SecurityEnableMode,
                                                            bttEnableFields.EnableMode,
                                                            THIS-OBJECT:AddingRecord) .

            /* Mike Fechner, Consultingwerk Ltd. 23.03.2016
               SCL-1163: When ReadOnlyDisabled then Enabled should by TRUE and ReadOnly false
               for those Controls that have a ReadOnly property */
            IF lEnabled = FALSE AND THIS-OBJECT:ReadOnlyDisabled = TRUE THEN DO:
                IF TYPE-OF (bttEnableFields.ChildControl, Infragistics.Win.UltraWinEditors.UltraWinEditorMaskedControlBase) OR
                   TYPE-OF (bttEnableFields.ChildControl, Infragistics.Win.UltraWinEditors.TextEditorControlBase) OR
                   TYPE-OF (bttEnableFields.ChildControl, System.Windows.Forms.TextBox) THEN
                CAST(bttEnableFields.ChildControl, Control):Enabled = TRUE .
            END .
            ELSE
                CAST(bttEnableFields.ChildControl, Control):Enabled = lEnabled .

            /* Mike Fechner, Consultingwerk Ltd. 02.09.2010
               Editors (= Multi-Line TextBoxes) should be set to read-only, not Disabled */
            IF TYPE-OF (bttEnableFields.ChildControl, Infragistics.Win.UltraWinEditors.UltraTextEditor) AND
                  CAST (bttEnableFields.ChildControl, Infragistics.Win.UltraWinEditors.UltraTextEditor):MultiLine = TRUE THEN
                       CAST (bttEnableFields.ChildControl, Infragistics.Win.UltraWinEditors.UltraTextEditor):ReadOnly = NOT lEnabled .
            ELSE IF THIS-OBJECT:ReadOnlyDisabled AND
                     TYPE-OF (bttEnableFields.ChildControl, Infragistics.Win.UltraWinEditors.UltraWinEditorMaskedControlBase) THEN
                 CAST (bttEnableFields.ChildControl, Infragistics.Win.UltraWinEditors.UltraWinEditorMaskedControlBase):ReadOnly = NOT lEnabled .
            ELSE IF THIS-OBJECT:ReadOnlyDisabled AND
                     TYPE-OF (bttEnableFields.ChildControl, Infragistics.Win.UltraWinGrid.UltraCombo) THEN
                 CAST (bttEnableFields.ChildControl, Infragistics.Win.UltraWinGrid.UltraCombo):ReadOnly = NOT lEnabled .
            ELSE IF THIS-OBJECT:ReadOnlyDisabled AND
                     TYPE-OF (bttEnableFields.ChildControl, Infragistics.Win.UltraWinEditors.TextEditorControlBase) THEN
                 CAST (bttEnableFields.ChildControl, Infragistics.Win.UltraWinEditors.TextEditorControlBase):ReadOnly = NOT lEnabled .
            ELSE
            IF TYPE-OF (bttEnableFields.ChildControl, System.Windows.Forms.TextBox) AND
                CAST (bttEnableFields.ChildControl, System.Windows.Forms.TextBox):MultiLine = TRUE THEN
                    CAST (bttEnableFields.ChildControl, System.Windows.Forms.TextBox):ReadOnly = NOT lEnabled .
        END.

        RELEASE bttEnableFields .

        /* Mike Fechner, Consultingwerk Ltd. 02.01.2010
           Simplify usage of UltraTabControls on Viewers: Set the same ControlEnabled State
           for the UltraTabPageControls */
        IF TYPE-OF (poControl, Infragistics.Win.UltraWinTabControl.UltraTabControl) THEN DO:
            DO i = 0 TO CAST (poControl, Infragistics.Win.UltraWinTabControl.UltraTabControl):Controls:Count - 1:
                THIS-OBJECT:SetControlEnabled (CAST (poControl, Infragistics.Win.UltraWinTabControl.UltraTabControl):Controls[i], pcEnabled) .
            END.
        END.

    END METHOD.

    /**
     * Purpose: Sets the Enable Mode for two given Controls inside the SmartViewerControl.
     *          Called by the Method InitializeControlEventHandler () of a
     *          SmartViewerControl derived Class.
     * Notes:   Possible Values for pcEnabled (ControlEnabledEnum):
     *          Add, Always, Never, Update, UpdateOnly
     * @param poControl1 The first Control which shall be set to the passed in Mode
     * @param poControl2 The second Control which shall be set to the passed in Mode
     * @param pcEnabled Mode to be set as defined in ControlEnabledEnum
     */
    METHOD PUBLIC VOID SetControlEnabled (poControl1 AS Control,
                                          poControl2 AS Control,
                                          pcEnabled AS CHARACTER):

        SetControlEnabled (poControl1, pcEnabled) .
        SetControlEnabled (poControl2, pcEnabled) .

    END METHOD .

    /**
     * Purpose: Sets the Enable Mode for three given Controls inside the SmartViewerControl.
     *          Called by the Method InitializeControlEventHandler () of a
     *          SmartViewerControl derived Class.
     * Notes:   Possible Values for pcEnabled (ControlEnabledEnum):
     *          Add, Always, Never, Update, UpdateOnly
     * @param poControl1 The first Control which shall be set to the passed in Mode
     * @param poControl2 The second Control which shall be set to the passed in Mode
     * @param poControl3 The third Control which shall be set to the passed in Mode
     * @param pcEnabled Mode to be set as defined in ControlEnabledEnum
     */
    METHOD PUBLIC VOID SetControlEnabled (poControl1 AS Control,
                                          poControl2 AS Control,
                                          poControl3 AS Control,
                                          pcEnabled AS CHARACTER):

        SetControlEnabled (poControl1, pcEnabled) .
        SetControlEnabled (poControl2, pcEnabled) .
        SetControlEnabled (poControl3, pcEnabled) .

    END METHOD .

    /**
     * Purpose: Sets the Enable Mode for four given Controls inside the SmartViewerControl.
     *          Called by the Method InitializeControlEventHandler () of a
     *          SmartViewerControl derived Class.
     * Notes:   Possible Values for pcEnabled (ControlEnabledEnum):
     *          Add, Always, Never, Update, UpdateOnly
     * @param poControl1 The first Control which shall be set to the passed in Mode
     * @param poControl2 The second Control which shall be set to the passed in Mode
     * @param poControl3 The third Control which shall be set to the passed in Mode
     * @param poControl4 The forth Control which shall be set to the passed in Mode
     * @param pcEnabled Mode to be set as defined in ControlEnabledEnum
     */
    METHOD PUBLIC VOID SetControlEnabled (poControl1 AS Control,
                                          poControl2 AS Control,
                                          poControl3 AS Control,
                                          poControl4 AS Control,
                                          pcEnabled AS CHARACTER):

        SetControlEnabled (poControl1, pcEnabled) .
        SetControlEnabled (poControl2, pcEnabled) .
        SetControlEnabled (poControl3, pcEnabled) .
        SetControlEnabled (poControl4, pcEnabled) .

    END METHOD .

    /**
     * Purpose: Sets the Enable Mode for five given Controls inside the SmartViewerControl.
     *          Called by the Method InitializeControlEventHandler () of a
     *          SmartViewerControl derived Class.
     * Notes:   Possible Values for pcEnabled (ControlEnabledEnum):
     *          Add, Always, Never, Update, UpdateOnly
     * @param poControl1 The first Control which shall be set to the passed in Mode
     * @param poControl2 The second Control which shall be set to the passed in Mode
     * @param poControl3 The third Control which shall be set to the passed in Mode
     * @param poControl4 The forth Control which shall be set to the passed in Mode
     * @param poControl5 The fifth Control which shall be set to the passed in Mode
     * @param pcEnabled Mode to be set as defined in ControlEnabledEnum
     */
    METHOD PUBLIC VOID SetControlEnabled (poControl1 AS Control,
                                          poControl2 AS Control,
                                          poControl3 AS Control,
                                          poControl4 AS Control,
                                          poControl5 AS Control,
                                          pcEnabled AS CHARACTER):

        SetControlEnabled (poControl1, pcEnabled) .
        SetControlEnabled (poControl2, pcEnabled) .
        SetControlEnabled (poControl3, pcEnabled) .
        SetControlEnabled (poControl4, pcEnabled) .
        SetControlEnabled (poControl5, pcEnabled) .

    END METHOD .

    /**
     * Purpose: Sets the Enable Mode for the given Controls inside the SmartViewerControl
     * Notes:   Possible Values for pcEnabled (ControlEnabledEnum):
     *          Add, Always, Never, Update, UpdateOnly
     * @param poControls The array of Control which shall be set to the passed in Mode
     * @param pcEnabled Mode to be set as defined in ControlEnabledEnum
     */
    METHOD PUBLIC VOID SetControlEnabled (poControls AS Control EXTENT,
                                          pcEnabled  AS CHARACTER):

        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        DO i = 1 TO EXTENT (poControls):
            THIS-OBJECT:SetControlEnabled (poControls [i], pcEnabled) .
        END.

    END METHOD .

    /**
     * Purpose: Sets the TableIOState to ModifyingData
     * Notes:   Useful when modifying Viewer values through custom event handlers
     */
    METHOD PUBLIC VOID SetModifyingState():

        IF THIS-OBJECT:SmartTableIOState = TableIOStateEnum:FieldsEnabled THEN DO:

            THIS-OBJECT:SmartTableIOState = TableIOStateEnum:ModifyingData .

            /* Tell the SmartDataSource that the user has started to update data. */
            IF VALID-OBJECT(THIS-OBJECT:SmartDataSource) THEN
                THIS-OBJECT:SmartDataSource:BeginUpdateState (THIS-OBJECT).
        END.

    END METHOD.

    /**
     * Purpose: Set's the current opened EditorDropDownControl
     * Notes:   See Bug 1878 for details
     * @param poControl The reference to the Control that currently has the list dropped down
     */
    METHOD PUBLIC VOID SetOpenedEditorDropDownControl (poControl AS Control):

        IF VALID-OBJECT (oSmartGroupAssignSource) THEN
            oSmartGroupAssignSource:SetOpenedEditorDropDownControl (poControl) .

        oEditorDropDownControl = poControl .

    END METHOD.

    /**
     * Purpose: Store the Current Values of all Controls on a SmartViewerControl
     *          and do the same in all SmartGroupAssignTargets.
     *          This Method is called by CreateRecord from the current Class.
     *          Enforced by Interface ISmartGroupAssignTarget.
     * Notes:
     */
    METHOD PUBLIC VOID StoreValuesForCopy ():

        RecurseStoreCurrentValues (THIS-OBJECT) .

        FOR EACH ttSmartGroupAssignTarget WHERE ttSmartGroupAssignTarget.RecordOwner = THIS-OBJECT:
            CAST(ttSmartGroupAssignTarget.SmartGroupAssignTarget, ISmartGroupAssignTarget):StoreValuesForCopy () .
        END.

    END METHOD.

    /**
     * Purpose: Validate data entered into the DataFields
     * Notes:   This is a placeholder that can be overridden in a viewer control
     *          inheriting from SmartViewerControl
     *          Due to limitations with the .NET Visual Designer environment, this
     *          method cannot be market ABSTRACT
     */
    METHOD PUBLIC VOID VisualValidate () :
        /* NOOP */
    END METHOD.

    /**
     * Purpose: Event handler for the TextChangedEvent event of Editor Controls on
     *          the SmartViewerControl.
     *          Initiate switching to the ModifyinData Mode if the user enters data.
     * Notes:   This Method is executed every time data changes in the EditorControls
     *          of a SmartViewerControl.
     *          Enforced by Interface ISmartGroupAssignSource.
     * @param sender The reference to the object that raised the event
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PUBLIC VOID TextChangedEventHandler (sender AS System.Object,
                                                e AS System.EventArgs):

       /* Mike Fechner, Consultingwerk Ltd. 13.02.2015
          SCL-662 : Allow to turn off the processing in this event handler
                    (see property for further notes) */
       IF THIS-OBJECT:TextChangedHandlerSuspended = TRUE THEN
           RETURN .

        /* Mike Fechner, Consultingwerk Ltd. 27.11.2014
           SCL-561 : Avoid setting Data Modified to early during screen initialization
                     when a field is focussed before loading data
                     Ideally there would be a better way to exclude TextChanged beeing fired
                     when the BindingSource updates the bound control - but there does not
                     seem to be one. So far we always relied on the Control beeing focussed.
                     However CCE has cases (see SCL-561) where the Control is focussed when
                     the Query is opened and thus we did initialize the Update state without
                     a change caused by the user. */
        /* Mike Fechner, Consultingwerk Ltd. 13.02.2015
           SCL-661 : Need to look for System.Windows.Forms.Binding.PushData */
        IF BindingSourceHelper:IsBindingChangingValue() THEN
            RETURN .

        IF VALID-OBJECT(oSmartGroupAssignSource) THEN
        DO:
            oSmartGroupAssignSource:TextChangedEventHandler (sender, e).
            RETURN.
        END.

        IF THIS-OBJECT:AddingRecord AND THIS-OBJECT:AddedRecordModified = FALSE THEN DO:

            IF VALID-OBJECT(sender) AND
                TYPE-OF(sender, System.Windows.Forms.Control) AND
                CAST(sender, System.Windows.Forms.Control):Enabled = FALSE THEN RETURN .

            IF CAST(sender, System.Windows.Forms.Control):Focused OR
               CAST(sender, System.Windows.Forms.Control) = oEditorDropDownControl THEN

                THIS-OBJECT:AddedRecordModified = TRUE .
        END.

        IF THIS-OBJECT:SmartTableIOState <> TableIOStateEnum:FieldsEnabled THEN
            RETURN .

        IF VALID-OBJECT(sender) AND
            TYPE-OF(sender, System.Windows.Forms.Control) AND
            CAST(sender, System.Windows.Forms.Control):Enabled = FALSE THEN
                RETURN .

        /* Only switch into ModifyingData Mode if the focused control itself initiated
           the change due to a manual change via keyboard.
           03.06.2009: Alternative is that the current control has an EditorDropDown
                       dialogue opened (like the UltraDateTimeEditor) */
        IF CAST(sender, System.Windows.Forms.Control):Focused OR
           CAST(sender, System.Windows.Forms.Control) = oEditorDropDownControl THEN DO:

            IF THIS-OBJECT:ActivateCurrentGridRowDuringEdit THEN
                THIS-OBJECT:ActivateCurrentGridRow() .

            THIS-OBJECT:SmartTableIOState = TableIOStateEnum:ModifyingData .

            /* Tell the SmartDataSource that the user has started to update data. */
            IF VALID-OBJECT(oSmartDataSource) THEN
                oSmartDataSource:BeginUpdateState (THIS-OBJECT).
        END.
    END METHOD.

    /**
     * Purpose: Register a SmartDataTarget with the SmartGroupAssignTarget
     *          This Method is executed every time a new SmartGroupAssignSource is
     *          registered to the corresponding Property of a Class implementing the
     *          Interface ISmartGroupAssignSource.
     *          Enforced by Interface ISmartGroupAssignSource.
     * Notes:
     * @param poSmartGroupAssignTarget The SmartGroupAssignTarget to register with this SmartViewerControl instance
     */
    METHOD PUBLIC VOID RegisterSmartGroupAssignTarget (poSmartGroupAssignTarget AS ISmartGroupAssignTarget):

        CREATE ttSmartGroupAssignTarget.

        ASSIGN
            ttSmartGroupAssignTarget.RecordOwner            = THIS-OBJECT
            ttSmartGroupAssignTarget.SmartGroupAssignTarget = poSmartGroupAssignTarget
            .

        RELEASE ttSmartGroupAssignTarget.

        THIS-OBJECT:SmartGroupAssignTargets:Add (poSmartGroupAssignTarget) .

    END METHOD.

    /**
     * Purpose: Deregister a SmartDataTarget with the SmartGroupAssignTarget
     *          Enforced by Interface ISmartGroupAssignSource.
     * Notes:
     * @param poSmartGroupAssignTarget The SmartGroupAssignTarget to deregister from this SmartViewerControl instance
     */
    METHOD PUBLIC VOID DeregisterSmartGroupAssignTarget (poSmartGroupAssignTarget AS ISmartGroupAssignTarget):

        FIND ttSmartGroupAssignTarget WHERE ttSmartGroupAssignTarget.RecordOwner            = THIS-OBJECT
                                        AND ttSmartGroupAssignTarget.SmartGroupAssignTarget = poSmartGroupAssignTarget
                NO-ERROR .

        DELETE ttSmartGroupAssignTarget.

        IF THIS-OBJECT:SmartGroupAssignTargets:Contains (poSmartGroupAssignTarget) THEN
            THIS-OBJECT:SmartGroupAssignTargets:Remove (poSmartGroupAssignTarget) .

    END METHOD.

    /**
     * Purpose: Destructor of the SmartViewerContorl class
     * Notes:   Deletes the associated ttEnableFields and ttSmartGroupAssignTarget
     *          records
     */
    DESTRUCTOR PUBLIC SmartViewerControl ( ):

        DEFINE VARIABLE i       AS INTEGER              NO-UNDO .
        DEFINE VARIABLE iCount  AS INTEGER              NO-UNDO .
        DEFINE VARIABLE oObject AS Progress.Lang.Object NO-UNDO .

        THIS-OBJECT:SmartDataSource = ? .
        THIS-OBJECT:SmartGroupAssignSource = ? .

        oTextChangedHandler:Clear () .
        oCheckChangedIGHandler:Clear () .
        oCheckChangedMSHandler:Clear () .

        ASSIGN iCount = oSmartTableIOSources:Count .

        /* Mike Fechner, Consultingwerk Ltd. 27.12.2009
           Remove link to SmartTableIOSources (if they are still available) */
        DO i = 1 TO iCount:
            /* might already have been deleted on the .NET side */
            oObject = oSmartTableIOSources:GetItem(i) NO-ERROR .

            IF VALID-OBJECT(oObject) THEN
                RemoveSmartTableIOSource (CAST(oObject, ISmartTableIOSource)) .
        END.

        DELETE OBJECT oSmartTableIOSources .

        FOR EACH ttEnableFields WHERE ttEnableFields.RecordOwner = THIS-OBJECT:
            DELETE ttEnableFields .
        END.

        FOR EACH ttSmartGroupAssignTarget WHERE ttSmartGroupAssignTarget.RecordOwner = THIS-OBJECT:
            DELETE ttSmartGroupAssignTarget.
        END.

        FOR EACH ttSavedViewerValues WHERE ttSavedViewerValues.RecordOwner = THIS-OBJECT:
            DELETE ttSavedViewerValues.
        END.

        THIS-OBJECT:SmartGroupAssignTargets:Clear () .

    END DESTRUCTOR.

    /**
     * Purpose: Check if it is ok to close the parent form.
     *          Returns TRUE when the form can be closed. Otherwise returns FALSE.
     *          Enforced by the Interface ICheckOkToClose.
     * Notes:
     * @param plShowMessage When TRUE a message is Shown explaining why it is not ok to close the parent form
     * @param poCloseReason The System.Windows.Forms.CloseReason passed from then OnFormClosing Method of the parent form giving the reason why the form is closed
     * @return Logical value indicating if the parent Form may be closed
     */
    METHOD PUBLIC LOGICAL IsOkToClose (plShowMessage AS LOGICAL,
                                       poCloseReason AS System.Windows.Forms.CloseReason):

        DEFINE VARIABLE lOk       AS LOGICAL          NO-UNDO.
        DEFINE VARIABLE lPrevious AS LOGICAL          NO-UNDO.
        DEFINE VARIABLE oResult   AS DialogResultEnum NO-UNDO.

        /* Mike Fechner, Consultingwerk Ltd. 21.02.2010
           A GroupAssign Target does not take the decision, this will (potentially)
           be run in the GroupAssign Source */
        IF VALID-OBJECT(oSmartGroupAssignSource) THEN
            RETURN TRUE .

        /* Mike Fechner, Consultingwerk Ltd. 31.10.2013
           No prompt when a added record has not yet been modified by the user */
        IF THIS-OBJECT:AllowCloseOfUnmodifiedNewRecord AND
           THIS-OBJECT:AddingRecord AND
           NOT THIS-OBJECT:AddedRecordModified = TRUE THEN DO:
            THIS-OBJECT:CancelUpdate() .
            RETURN TRUE .
        END.

        IF THIS-OBJECT:SmartTableIOState = TableIOStateEnum:ModifyingData THEN
        DO:
            IF plShowMessage THEN DO:
                oResult = Consultingwerk.Util.MessageFormHelper:ShowMessage
                    (SmartComponentLibraryCustomizer:SmartViewerControl_UpdateActiveWhileFormClosing,
                     SmartComponentLibraryCustomizer:SmartViewerControl_UpdateActiveWhileFormClosing_Title,
                     "":U,
                     MessageFormImages:ImageQuestion,
                     MessageFormDefaults:LabelYesNoCancel,
                     MessageFormDefaults:GetDialogResultYesNoCancel()) .

                IF oResult:Equals (DialogResultEnum:DialogResultYes) THEN
                    lOk = TRUE .
                ELSE IF oResult:Equals (DialogResultEnum:DialogResultNo) THEN
                    lOk = FALSE .
                ELSE IF oResult:Equals (DialogResultEnum:DialogResultCancel) THEN
                    lOk = ? .
            END.

            IF lOk = TRUE THEN DO ON ERROR UNDO, THROW:
                ASSIGN lPrevious = Consultingwerk.Util.ErrorHelper:ErrorMessageDialogAllowed .

                Consultingwerk.Util.ErrorHelper:ErrorMessageDialogAllowed = FALSE .

                THIS-OBJECT:SaveChanges() .

                /* Mike Fechner, Consultingwerk Ltd. 21.02.2010
                   Check if still in update mode */
                IF THIS-OBJECT:SmartTableIOState = TableIOStateEnum:ModifyingData THEN
                    RETURN FALSE .
                ELSE
                    RETURN TRUE .

                FINALLY:
                    Consultingwerk.Util.ErrorHelper:ErrorMessageDialogAllowed = lPrevious .
                END FINALLY.

            END.
            IF lOk = FALSE THEN DO:
                THIS-OBJECT:CancelUpdate() .

                RETURN TRUE .
            END.

            RETURN FALSE.
        END.
        ELSE
            RETURN TRUE.

    END METHOD.

END CLASS.
