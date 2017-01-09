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
    File        : SmartUpdatableBrowser
    Purpose     : Updatable browser implementation
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Jul 16 14:36:37 CEST 2010
    Notes       : The SmartUpdatableBrowser can use a SmartLookup
                  (SmartBusinessEntityLookup) Control as an embedded editor
                  by assigning a columns EditorComponent property to an
                  instance of a lookup control.
                  The communication between the SmartUpdatabelBrowser and
                  the SmartBusinessEntityLookup is mediated through the
                  IEmbeddedSmartEditor interface. The SmartLookup needs to
                  be referencing the Grid using the OwningGrid property
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                                /*FROM PROPATH*/ .
USING Consultingwerk.Exceptions.*                     FROM PROPATH .
USING Consultingwerk.Framework.*                      FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*           FROM PROPATH .
USING Consultingwerk.Framework.Enum.*                 FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.SmartComponents.Interfaces.*     /*FROM PROPATH*/ .
USING Consultingwerk.SmartComponents.Resources.*      FROM PROPATH .
USING Consultingwerk.Util.*                           FROM PROPATH .
USING Consultingwerk.Util.Forms.*                     FROM PROPATH .
USING Consultingwerk.Windows.Util.Forms.*             FROM PROPATH .
USING Infragistics.Win.UltraWinGrid.*                 FROM ASSEMBLY .
USING Progress.Lang.*                                 FROM PROPATH .
USING Progress.Util.*                                 FROM ASSEMBLY .
USING System.Windows.Forms.*                          FROM ASSEMBLY .

CLASS Consultingwerk.SmartComponents.Implementation.SmartUpdatableBrowser
    INHERITS SmartDataBrowser
    IMPLEMENTS ISmartTableIOTarget, /*ISmartGroupAssignSource,*/ ICheckOkToClose,
               ISaveChangesOnDeactivateForm:
               /*Consultingwerk.SmartComponents.Interfaces.Design.IDesignGroupAssignSource*/

    DEFINE PRIVATE VARIABLE oSmartTableIOSource    AS ISmartTableIOSource                NO-UNDO .
    DEFINE PRIVATE VARIABLE oSmartTableIOSources   AS Consultingwerk.Framework.Base.List NO-UNDO .
    DEFINE PRIVATE VARIABLE cSmartTableIOState     AS CHARACTER                          NO-UNDO .
    DEFINE PRIVATE VARIABLE lDisableFieldsOnSave   AS LOGICAL  INIT TRUE                 NO-UNDO .
    DEFINE PRIVATE VARIABLE lDeactivateRow         AS LOGICAL  INIT FALSE                NO-UNDO .
    DEFINE PRIVATE VARIABLE lSaving                AS LOGICAL  INIT FALSE                NO-UNDO .
    DEFINE PRIVATE VARIABLE lCancelling            AS LOGICAL  INIT FALSE                NO-UNDO .
    DEFINE PRIVATE VARIABLE oEditorDropDownControl AS Control                            NO-UNDO .

    DEFINE PRIVATE VARIABLE oStoredValues AS "System.Collections.Generic.Dictionary<System.String, System.Object>":U NO-UNDO .

    /* Mike Fechner, Consultingwerk Ltd. 03.09.2013
       Static only on 10.2B to workaround too-many-temp-table issue
       Not static on 11.x to avoid issues with the combination of static
       and hybrids on 11.2 and 11.3, OE defect OE00240707
       http://knowledgebase.progress.com/articles/Article/000042229?popup=true */
    DEFINE PRIVATE
    &IF PROVERSION BEGINS "10.2":U &THEN
    STATIC
    &ENDIF TEMP-TABLE ttSmartGroupAssignTarget NO-UNDO
        FIELD RecordOwner            AS Progress.Lang.Object
        FIELD SmartGroupAssignTarget AS Progress.Lang.Object
        INDEX RecordOwner RecordOwner SmartGroupAssignTarget
        .

    /*------------------------------------------------------------------------------
        Purpose: Event fired when the AddingRecord Property is changed
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT AddingRecordChanged DELEGATE System.EventHandler.

    /*------------------------------------------------------------------------------
        Purpose: Event fired when the viewer has successfully started an Add
                 operation
        Notes:   This event is defined using a .NET delegate, so it will show in
                 the PropertyGrid of the VisualDesigner
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT AfterAddRecord  DELEGATE System.EventHandler.

    /*------------------------------------------------------------------------------
        Purpose: Event fired when the viewer has successfully cancelled an update
        Notes:   This event is defined using a .NET delegate, so it will show in
                 the PropertyGrid of the VisualDesigner
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT AfterCancelUpdate DELEGATE System.EventHandler.

    /*------------------------------------------------------------------------------
        Purpose: Event fired when the viewer has successfully started a Copy operation
        Notes:   This event is defined using a .NET delegate, so it will show in
                 the PropertyGrid of the VisualDesigner
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT AfterCopyRecord  DELEGATE System.EventHandler.

    /*------------------------------------------------------------------------------
        Purpose: Raised after the fields have been enabled
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT AfterFieldsEnabled DELEGATE System.EventHandler .

    /*------------------------------------------------------------------------------
        Purpose: Event fired when the viewer has successfully saved changes
        Notes:   This event is defined using a .NET delegate, so it will show in
                 the PropertyGrid of the VisualDesigner
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT AfterSaveChanges  DELEGATE System.EventHandler.

    /*------------------------------------------------------------------------------
        Purpose: Event fired when the Viewer starts adding a new record.
                 This event is cancelable
        Notes:   This event is defined using a .NET delegate, so it will show in
                 the PropertyGrid of the VisualDesigner
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT BeforeAddRecord  DELEGATE System.ComponentModel.CancelEventHandler.

    /*------------------------------------------------------------------------------
        Purpose: Event fired when the Viewer begins cancelling an update.
                 This event is cancellable
        Notes:   This event is defined using a .NET delegate, so it will show in
                 the PropertyGrid of the VisualDesigner
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT BeforeCancelUpdate  DELEGATE System.ComponentModel.CancelEventHandler.

    /*------------------------------------------------------------------------------
        Purpose: Event fired when the Viewer begins copying a record.
                 This event is cancellable
        Notes:   This event is defined using a .NET delegate, so it will show in
                 the PropertyGrid of the VisualDesigner
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT BeforeCopyRecord  DELEGATE System.ComponentModel.CancelEventHandler.

    /*------------------------------------------------------------------------------
        Purpose: Raised before the fields are enabled
        Notes:
        @param sender The object that raised the BeforeEnableFields event
        @param e The CancelableEventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT BeforeEnableFields SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                           e AS CancelableEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Event fired when the Viewer starts saving a record.
                 This event is cancellable
        Notes:   This event is defined using a .NET delegate, so it will show in
                 the PropertyGrid of the VisualDesigner
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT BeforeSaveChanges  DELEGATE System.ComponentModel.CancelEventHandler.

    /*------------------------------------------------------------------------------
        Purpose: Raised when the CopyingRecord property has changed
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT CopyingRecordChanged DELEGATE System.EventHandler .

    /*------------------------------------------------------------------------------
        Purpose: Event fired when the columns of this browser become disabled
        Notes:   This event is defined using a .NET delegate, so it will show in
                 the PropertyGrid of the VisualDesigner
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT FieldsDisabled DELEGATE System.EventHandler.

    /*------------------------------------------------------------------------------
        Purpose: Event fired when the columns of this browser become enabled
        Notes:   This event is defined using a .NET delegate, so it will show in
                 the PropertyGrid of the VisualDesigner
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT FieldsEnabled DELEGATE System.EventHandler.

    /*------------------------------------------------------------------------------
        Purpose: Event fired when the SmartTableIOSource Property is changed
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT SmartTableIOSourceChanged DELEGATE System.EventHandler.

    /*------------------------------------------------------------------------------
        Purpose: Event fired when the SmartTableIOSource Property is changed
        Notes:
        @param sender The reference to the object that raised the event
        @param e The SmartTableIOStateChangedEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT SmartTableIOStateChanged SIGNATURE VOID
                   (sender AS System.Object,
                    e AS SmartTableIOStateChangedEventArgs).

    /*------------------------------------------------------------------------------
       Purpose: Flag that indicates if we are currently in Add Mode.
                When CreateRecord () is called the value of the Property changes to
                TRUE. CancelUpdate ()or during initialization the call of
                EvaluateTableIOState () resets the value of the Property to FALSE.
       Notes:   To Differenciate from Modifying data
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY AddingRecord AS LOGICAL INITIAL FALSE NO-UNDO
    GET.
    PROTECTED SET (arg AS LOGICAL):
        DEFINE VARIABLE lChanged    AS LOGICAL                 NO-UNDO INIT FALSE .

        IF arg <> THIS-OBJECT:AddingRecord THEN
            ASSIGN lChanged = TRUE .

        THIS-OBJECT:AddingRecord = arg .

        IF lChanged THEN
            OnAddingRecordChanged (System.EventArgs:Empty) .

    END.

    /*------------------------------------------------------------------------------
       Purpose: Support for Template Row to add new records
       Notes:   SCL-826, supported values as No (default), FixedAddRowOnTop and
                FixedAddRowOnBottom
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY AllowAddNew AS AllowAddNew NO-UNDO
    GET:
        IF VALID-OBJECT (THIS-OBJECT:AllowAddNew) THEN
            RETURN THIS-OBJECT:AllowAddNew .
        ELSE
            RETURN Infragistics.Win.UltraWinGrid.AllowAddNew:No .
    END GET .
    SET (arg AS AllowAddNew):
        IF NOT VALID-OBJECT (arg) THEN DO:
            ASSIGN AllowAddNew = Infragistics.Win.UltraWinGrid.AllowAddNew:No .
        END.
        ELSE DO:
            IF EnumHelper:AreNotEqual (arg, Infragistics.Win.UltraWinGrid.AllowAddNew:No) AND
               EnumHelper:AreNotEqual (arg, Infragistics.Win.UltraWinGrid.AllowAddNew:FixedAddRowOnTop) AND
               EnumHelper:AreNotEqual (arg, Infragistics.Win.UltraWinGrid.AllowAddNew:FixedAddRowOnBottom) THEN

                UNDO, THROW NEW InvalidValueException (arg:ToString(), "AllowAddNew":U) .

            ASSIGN AllowAddNew = arg .
        END.
    END SET .

    /**
     * Purpose: Toggles if the SmartUpdatableBrowser should enable the same fields when
     *          editing an uncommitted add (that is an added record, that was saved and
     *          is then edited again before committing to the backend) as during the
     *          initial add
     * Notes:   SCL-1340, only supported when the SmartDataSource is a SmartBusinessEntityAdapter
     *          or SmartDatasetChildAdapter (SmartDatasetAdapter derived).
     */
    DEFINE PUBLIC PROPERTY AllowEditOfUncommittedAdd AS LOGICAL NO-UNDO INITIAL FALSE
    GET.
    SET.

    /*------------------------------------------------------------------------------
       Purpose: Flag if closing a cell'S drop down list begins edit mode event
                when no change has been made by the user
       Notes:   SCL-1124
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BeginEditModeOnCellListCloseUp AS LOGICAL INITIAL TRUE NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns if the Viewer is currently copying a record
        Notes:   Only true when AddingRecord is also true
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY CopyingRecord AS LOGICAL INITIAL FALSE NO-UNDO
    GET .
    PROTECTED SET (arg AS LOGICAL):
        DEFINE VARIABLE lChanged AS LOGICAL NO-UNDO INIT FALSE .

        IF arg <> THIS-OBJECT:CopyingRecord THEN
            ASSIGN lChanged = TRUE .

        THIS-OBJECT:CopyingRecord = arg .

        IF lChanged THEN
            OnCopyingRecordChanged (System.EventArgs:Empty) .
    END.

    /*------------------------------------------------------------------------------
        Purpose: Get and Set the keys of grid that are disabled during add
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DisabledColumnsForAdd AS CHARACTER NO-UNDO INIT "":U
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Get and Set the keys of grid that are disabled during update
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DisabledColumnsForUpdate AS CHARACTER NO-UNDO INIT "":U
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Get and Set the list of grid columns that are excluded from the
                 RestoreValuesForCopy. Column values in this list will not be copied
        Notes:   Comma delimited entries. Entries are column names, for format depends
                 on the BindingSource UseFullFieldName setting
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ExcludeColumnsFromCopy AS CHARACTER NO-UNDO INIT "":U
    GET.
    SET.

    /*------------------------------------------------------------------------------
       Purpose: Represents the SmartTableIOSource property casted to a .NET Interface
       Notes:   Implementation of Interface in Consultingwerk.SmartComponents.dll
                SCL-725 - code moved from LinkTableIOSource.i directly into class file
   ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LinkTableIOSource AS Consultingwerk.SmartComponents.Interfaces.Design.IDesignTableIOSource NO-UNDO
    GET:
        IF TYPE-OF (THIS-OBJECT:SmartTableIOSource, Consultingwerk.SmartComponents.Interfaces.Design.IDesignTableIOSource) THEN
            RETURN CAST (THIS-OBJECT:SmartTableIOSource, Consultingwerk.SmartComponents.Interfaces.Design.IDesignTableIOSource) .
    END GET .
    SET (arg AS Consultingwerk.SmartComponents.Interfaces.Design.IDesignTableIOSource):
        IF NOT VALID-OBJECT (arg) OR TYPE-OF (arg, Consultingwerk.SmartComponents.Interfaces.ISmartTableIOSource) THEN
            ASSIGN THIS-OBJECT:SmartTableIOSource = CAST (arg, Consultingwerk.SmartComponents.Interfaces.ISmartTableIOSource) .
    END.

    /*------------------------------------------------------------------------------
        Purpose: Allows to turn of the save changes prompt when the user leaves
                 (deactivates) a row with modifications
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY PromptForSaveValuesOnRowDeactivate AS LOGICAL NO-UNDO INIT TRUE
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Allows to turn on saving changes on leave of the SmartUpdatableBrowser
        Notes:   Allow wo fix UltraGrid behaviour of resetting pending changes on Leave
                 SCL-570
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SaveChangesOnLeave AS LOGICAL INITIAL FALSE NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
       Purpose: Controls if the Browsers's field remain Enabled after Save or Cancel
       Notes:   When the user is using the SaveMode the fields are basically always
                enabled as long as there is a record available. As soon as the user
                starts entering values in the fields of the viewer, the update
                operation is starting, resulting in a change of enabled buttons in the
                viewer and the disablement of linked grids.
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SaveMode AS LOGICAL NO-UNDO INIT FALSE
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Toolbar Security Key
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SecurityKey AS CHARACTER NO-UNDO INIT "":U
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Turns on tracking KeyDown events of the underlying UltraGrid Control
                 to allow "Excel-like" navigation between Cells during Update.
        Notes:   See Bug 2640
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SimpleKeyboardNavigationDuringUpdate AS LOGICAL INITIAL FALSE NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Controls if the SimpleKeyboardNavigationDuringUpdate feature does also
                 include the cursor-left and cursor-right (default). Setting this to
                 false allows cursor navigation within a cell during update which provides
                 a more natural update behaviour
        Notes:   See SCL-1082
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SimpleKeyboardNavigationDuringUpdateForLeftRight AS LOGICAL INITIAL TRUE NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: When using SimpleKeyboardNavigationDuringUpdate, this property allows
                 to force cursor key navigation, thus not raising the KeyDown event for
                 the Cursor-Up/Down/Left/Right keys
        Notes:   See SCL-277
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SimpleKeyboardNavigationDuringUpdateForced AS LOGICAL INITIAL FALSE NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Get and Set the current SmartTableIOSource of the SmartViewerControl.
                 This Property is set by the developer when initializing a Control
                 implementing the Interface ISmartDataTarget.
                 Enforced by Interface ISmartDataTarget.
        Notes:   Cannot be changed while the SmartViewerControl is in ModifyingData mode.
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmartTableIOSource AS ISmartTableIOSource NO-UNDO
    GET:
        RETURN oSmartTableIOSource .
    END.
    SET (arg AS ISmartTableIOSource):
        DEFINE VARIABLE lChanged AS LOGICAL NO-UNDO INIT FALSE .

        IF arg <> THIS-OBJECT:SmartTableIOSource THEN
            ASSIGN lChanged = TRUE .

        IF VALID-OBJECT (arg) THEN
        DO:
            /* When setting a SmartTableIOSource, check current SmartTableIOState first */
            IF VALID-OBJECT(oSmartTableIOSource) THEN
                UNDO, THROW NEW Progress.Lang.AppError ("Use AddSmartTableIOSource method to add another SmartTableUISource", 0) .

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

    /*------------------------------------------------------------------------------
        Purpose: The SmartTableIOState property reflects the operationmode of the
                 SmartViewerControl.
                 Value changes of this Property are only made in the Class
                 SmartViewerControl.
                 Enforced by Interface ISmartDataTarget or ISmartGroupAssignTarget.
        Notes:   Possible values (TableIOStateEnum):
                     AddCopyAllowed
                     FieldsDisabled
                     FieldsEnabled
                     ModifyingData
                     NoDataSource
                     NoParentRecordAvailable
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmartTableIOState AS CHARACTER NO-UNDO
    GET:
        RETURN cSmartTableIOState .
    END GET.
    PUBLIC SET (arg AS CHARACTER):
        /* Mike Fechner, Consultingwerk Ltd. 17.11.2015
           SCL-1102 - allow empty SmartTableIOState in Visual Designer */
        IF arg = "":U AND THIS-OBJECT:DesignTime THEN
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
            VALID-OBJECT(THIS-OBJECT:SmartDataSource) THEN
            THIS-OBJECT:SmartDataSource:SmartDataSourceState = DataSourceStateEnum:Navigatable .

        IF arg = cSmartTableIOState THEN
            RETURN .

        ASSIGN
            cSmartTableIOState = arg .

        IF arg <> "":U THEN
            OnSmartTableIOStateChanged (NEW SmartTableIOStateChangedEventArgs (arg)) .
    END.

    /*------------------------------------------------------------------------------
        Purpose: Returns the Update State of the Viewer
        Notes:   Possible values are Add, Copy, Update or View
    ------------------------------------------------------------------------------*/
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

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartUpdatableBrowser class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartUpdatableBrowser ():
        SUPER ().

        oSmartTableIOSources = NEW Consultingwerk.Framework.Base.List () .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Add a new record to the SmartDataSource. The Method CreateRecord of
                 SmartUpdatableBrowser gets called to Handle this Task.
                 This Method is called by the Toolbar Classes SmartToolbarController
                 or SmartToolStripContainer by pressing the AddRecord Tool.
                 Enforced by Interface ISmartDataTarget.
        Notes:
    ------------------------------------------------------------------------------*/
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

    /*------------------------------------------------------------------------------
        Purpose: Ensures the SmartUpdatableBrowser is in AddingRecord mode and the
                 right fields are enabled etc. after a new row was created by the
                 BindingSource
        Notes:   SCL-826
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID AddRecordFromBindingSource ():

        THIS-OBJECT:EnableFields (EnableFieldsEnum:Add, TRUE) .

        THIS-OBJECT:AddingRecord = TRUE .

        THIS-OBJECT:SmartTableIOState = TableIOStateEnum:ModifyingData .

        /* Tell the SmartDataSource that the user has started to update data. */
        IF VALID-OBJECT(THIS-OBJECT:SmartDataSource) THEN
            THIS-OBJECT:SmartDataSource:BeginUpdateState (THIS-OBJECT).

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Adds a(nother) SmartTableIOSource to the SmartDataAdapter
                 instance
        Notes:   Enables multiple SmartTableIOSources to the SmartViewerControl.
        @param poSmartTableIOSource The ISmartTableIOSource to add to this instance
    ------------------------------------------------------------------------------*/
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

    /*------------------------------------------------------------------------------
       Purpose: Reverts the changes to the current record.
                This Method is called by the Toolbar Classes SmartToolbarController
                or SmartToolStripContainer by pressing the corresponding Tool.
                Enforced by Interface ISmartDataTarget.
       Notes:
   ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID CancelUpdate ():

        DEFINE VARIABLE e AS System.ComponentModel.CancelEventArgs NO-UNDO .

        ASSIGN lCancelling = TRUE .

        e = NEW System.ComponentModel.CancelEventArgs () .
        OnBeforeCancelUpdate (e) .

        IF VALID-OBJECT (e) AND e:Cancel THEN
            RETURN .

        THIS-OBJECT:PerformAction (Infragistics.Win.UltraWinGrid.UltraGridAction:ExitEditMode) .

        /* Cancel pending Add operation */
        IF THIS-OBJECT:AddingRecord = TRUE THEN
            THIS-OBJECT:SmartDataSource:CancelCreateRecord () .
        ELSE
            THIS-OBJECT:SmartDataSource:BindingSource:Refresh () .

        /* Fields will only be disabled depending on the SaveMode only */
        IF THIS-OBJECT:SaveMode THEN
            THIS-OBJECT:EnableFields (Consultingwerk.SmartComponents.Enum.EnableFieldsEnum:Update, FALSE) .
        ELSE
            THIS-OBJECT:DisableFields() .

        OnAfterCancelUpdate (System.EventArgs:Empty) .

        FINALLY:
            /* No matter if we have been in Add mode or not, CancelUpdate is able
               to set AddingRecord to FALSE */
            ASSIGN THIS-OBJECT:AddingRecord  = FALSE
                   THIS-OBJECT:CopyingRecord = FALSE .

            THIS-OBJECT:EvaluateTableIOState () .

            lCancelling = FALSE .
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Clears the Filter Row
        Notes:   Method overridden in the SmartUpdatableBrowse to allow to save pending
                 updates before clearing the filter
        @param poUIElement The UIElement that was clicked to distinguish between clear filter row and a single filter cell
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID ClearFilterRow (poUIElement AS FilterClearButtonUIElement):

        DEFINE VARIABLE lOk AS LOGICAL NO-UNDO INIT FALSE .

        /* Mike Fechner, Consultingwerk Ltd. 19.07.2010
           WAIT-FOR Workaround, SCL-843 - only required on 10.2B */
        &IF PROVERSION BEGINS "10" &THEN
        Consultingwerk.Util.ErrorHelper:ErrorMessageDialogAllowed = FALSE .
        &ENDIF

        ASSIGN lDeactivateRow = FALSE  .

        IF THIS-OBJECT:SmartTableIOState = TableIOStateEnum:ModifyingData THEN DO:

            IF THIS-OBJECT:PromptForSaveValuesOnRowDeactivate = TRUE THEN
                ASSIGN lOk = THIS-OBJECT:PromptForSaveValuesOnRowDeactivate () .
            ELSE
                ASSIGN lOk = TRUE .

            IF lOk THEN DO:
                ASSIGN lDisableFieldsOnSave = FALSE .

                THIS-OBJECT:SaveChanges () .

                /* Mike Fechner, Consultingwerk Ltd. 18.10.2013
                   When we are still modifying the record after SaveChanges, we know,
                   that the save was not successful and should stay in the current row */
                IF THIS-OBJECT:SmartTableIOState = TableIOStateEnum:ModifyingData THEN
                    RETURN .

                THIS-OBJECT:SmartTableIOState = TableIOStateEnum:FieldsEnabled .
            END.
            ELSE DO:
                THIS-OBJECT:CancelUpdate() .
            END.
        END.

        SUPER:ClearFilterRow (poUIElement) .

        FINALLY:
            ASSIGN lDisableFieldsOnSave = TRUE
&IF PROVERSION BEGINS "10" &THEN
                   Consultingwerk.Util.ErrorHelper:ErrorMessageDialogAllowed = TRUE
&ENDIF
                    .
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Copy the current record to a new one. The Method CreateRecord of
                 SmartViewerControl gets called to Handle this Task.
                 This Method is called by the Toolbar Classes SmartToolbarController
                 or SmartToolStripContainer by pressing the corresponding Tool.
                 Enforced by Interface ISmartDataTarget.
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID CopyRecord ():

        DEFINE VARIABLE e AS System.ComponentModel.CancelEventArgs NO-UNDO .
        e = NEW System.ComponentModel.CancelEventArgs () .

        OnBeforeCopyRecord (e) .

        IF VALID-OBJECT (e) AND e:Cancel THEN
            RETURN .

        ASSIGN THIS-OBJECT:CopyingRecord = TRUE .

        THIS-OBJECT:EnsureVisible () .

        THIS-OBJECT:CreateRecord (TRUE) . /* TRUE = Copy existing record */

        OnAfterCopyRecord (System.EventArgs:Empty) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Creates a new record in the viewer.
                 This Method is called by AddRecord and CopyRecord from the current
                 Class but may be overridden.
        Notes:   The parameter plCopy tells the method if it has been called from
                 AddRecord or CopyRecord.
        @param plCopy Logical value, FALSE = Add new record, TRUE = Copy existing record
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID CreateRecord (plCopy AS LOGICAL):

        DEFINE VARIABLE cSourceRecordKey AS CHARACTER NO-UNDO INIT ? .

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

        THIS-OBJECT:SmartDataSource:CreateRecord () .

        THIS-OBJECT:EnableFields (EnableFieldsEnum:Add, TRUE) .

        THIS-OBJECT:AddingRecord = TRUE .

        THIS-OBJECT:SmartTableIOState = TableIOStateEnum:ModifyingData .

        /* Tell the SmartDataSource that the user has started to update data. */
        IF VALID-OBJECT(THIS-OBJECT:SmartDataSource) THEN
            THIS-OBJECT:SmartDataSource:BeginUpdateState (THIS-OBJECT).

        THIS-OBJECT:PerformAction (Infragistics.Win.UltraWinGrid.UltraGridAction:EnterEditMode) .

        IF plCopy THEN DO:
            RestoreValuesForCopy () .

            IF cSourceRecordKey <> ? AND
               TYPE-OF (THIS-OBJECT:SmartDataSource, IStoresCopiedFrom) THEN

               CAST (THIS-OBJECT:SmartDataSource, IStoresCopiedFrom):StoreCopiedFrom (cSourceRecordKey) .
        END.

        CATCH cancelex AS Consultingwerk.Exceptions.CancelException:
            /* When there's a message with a Cancel Exception, show it */
            IF cancelex:NumMessages > 0 THEN
                THIS-OBJECT:DisplayError (cancelex, SmartComponentLibraryCustomizer:SmartViewerControl_UnableToCreateRecord) .

            THIS-OBJECT:DisableFields () .
        END CATCH.

        CATCH e AS Progress.Lang.Error :
            THIS-OBJECT:DisplayError (e, SmartComponentLibraryCustomizer:SmartViewerControl_UnableToCreateRecord) .

            THIS-OBJECT:DisableFields () .
        END CATCH.

        FINALLY:
            IF VALID-OBJECT (oStoredValues) THEN
                DELETE OBJECT oStoredValues .
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Widens access to the DeleteRecord method of the SmartBrowserControl
        Notes:   ISmartTableIOTarget requires this method to be public
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC OVERRIDE VOID DeleteRecord ():

        SUPER:DeleteRecord() .

        /* Mike Fechner, Consultingwerk Ltd. 28.04.2015
           Silently ignoring the CancelException thrown by the SmartDatasetAdapter
           when the BeforeUpdateRecord or BeforeAssignRecord events have been
           cancelled */
        @SuppressUnusedWarnings.
        CATCH cex AS CancelException:
            RETURN .
        END CATCH.

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

    /*------------------------------------------------------------------------------
        Purpose: Deregister a SmartDataTarget with the SmartGroupAssignTarget
        Notes:   Enforced by Interface ISmartGroupAssignSource.
        @param poSmartGroupAssignTarget The ISmartGroupAssignTarget to remove from this instance
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DeregisterSmartGroupAssignTarget (poSmartGroupAssignTarget AS ISmartGroupAssignTarget):

        FIND ttSmartGroupAssignTarget
            WHERE ttSmartGroupAssignTarget.RecordOwner            = THIS-OBJECT
            AND ttSmartGroupAssignTarget.SmartGroupAssignTarget = poSmartGroupAssignTarget
            NO-ERROR .

        DELETE ttSmartGroupAssignTarget.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Disables the fields of the SmartUpdatableBrowser
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DisableFields ():

        THIS-OBJECT:SmartTableIOState = TableIOStateEnum:FieldsDisabled .

        THIS-OBJECT:DisplayLayout:Override:AllowAddNew = Infragistics.Win.UltraWinGrid.AllowAddNew:No.
        THIS-OBJECT:DisplayLayout:Override:AllowDelete = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:DisplayLayout:Override:AllowUpdate = Infragistics.Win.DefaultableBoolean:False.

        THIS-OBJECT:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:RowSelect .

        IF VALID-OBJECT (THIS-OBJECT:ActiveCell) THEN
            THIS-OBJECT:PerformAction (Infragistics.Win.UltraWinGrid.UltraGridAction:DeactivateCell) .

        /* DisableFields in every registered SmartGroupAssignTarget */
        FOR EACH ttSmartGroupAssignTarget WHERE ttSmartGroupAssignTarget.RecordOwner = THIS-OBJECT NO-LOCK:
            CAST(ttSmartGroupAssignTarget.SmartGroupAssignTarget, ISmartGroupAssignTarget):DisableFields().
        END.

        OnFieldsDisabled (System.EventArgs:Empty) .

    END METHOD.

    /*------------------------------------------------------------------------------
       Purpose: Disable the Controls during an Update.
       Notes:   This Method is called by the SmartDataAdapter by a call to
                BeginUpdateState () which is defined in the Interface ISmartDataSource.
                Enforced by Interface ISmartDataTarget.
                Do not disable this browser, when the browser itself initiated the
                Update mode
       @param poSmartDataTarget The SmartDataTarget that is currently in update state
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID DisableForUpdate (poSmartDataTarget AS Consultingwerk.SmartComponents.Interfaces.ISmartDataTarget):

        IF poSmartDataTarget = THIS-OBJECT THEN
            RETURN .

        IF VALID-OBJECT (THIS-OBJECT:SmartTableIOSource) THEN
            THIS-OBJECT:SmartTableIOState = TableIOStateEnum:NoDataSource .

        SUPER:DisableForUpdate (INPUT poSmartDataTarget).

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Display ErrorMessages shows a predefined Error Dialog and is called
                 from the Method SaveChanges () of the SmartUpdatableBrowser Class.
        Notes:
        @param e The error to display
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID DisplayError (e AS Progress.Lang.Error):

        THIS-OBJECT:DisplayError (e, ?) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Display ErrorMessages shows a predefined Error Dialog and is called
                 from the Method SaveChanges () of the SmartUpdatableBrowser Class.
        Notes:
        @param e The error to display
        @param pcMessageTitle The title to use for the error message
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID DisplayError (e AS Progress.Lang.Error,
                                        pcMessageTitle AS CHARACTER):

        DEFINE VARIABLE cFormTitle AS CHARACTER NO-UNDO.

        /* Mike Fechner, Consultingwerk Ltd. 01.01.2010
           Ensure that the grid is visible */
        THIS-OBJECT:EnsureVisible () .

        /* Mike Fechner, Consultingwerk Ltd. 06.11.2009
           Add the Title of the Form containing this Viewer to the
           message box. */
        IF CharacterType:IsNotNullOrEmpty (pcMessageTitle) THEN
            cFormTitle = pcMessageTitle .
        ELSE DO:
            IF VALID-OBJECT (THIS-OBJECT:FindForm()) THEN DO:
                ASSIGN cFormTitle = THIS-OBJECT:FindForm():Text .

                IF FrameworkSettings:DebugMode THEN DO:
                    IF cFormTitle > "":U THEN
                        ASSIGN cFormTitle = cFormTitle + " / ":U + ErrorHelper:ErrorTypeName(e) .
                    ELSE
                        ASSIGN cFormTitle = ErrorHelper:ErrorTypeName(e) .
                END.
            END.
        END.

        IF cFormTitle > "":U THEN
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (e, cFormTitle) .
        ELSE
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (e) .

    END METHOD.

    /*------------------------------------------------------------------------------
      Purpose: Enable all controls of the SmartUpdatableBrowser.
      Notes:   This Method is called by the Toolbar Classes SmartToolbarController
               or SmartToolStripContainer by pressing the Tools for Add or Edit Data.
               Enforced by Interface ISmartDataTarget or ISmartGroupAssignTarget.
      @param pcFields The EnableFieldsEnum value
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID EnableFields (pcFields AS CHARACTER):

        THIS-OBJECT:EnableFields (pcFields, TRUE) .

    END METHOD .

    /*------------------------------------------------------------------------------
      Purpose: Enable all controls of the SmartUpdatableBrowser.
      Notes:   This Method is called by the Toolbar Classes SmartToolbarController
               or SmartToolStripContainer by pressing the Tools for Add or Edit Data.
               Enforced by Interface ISmartDataTarget or ISmartGroupAssignTarget.
      @param pcFields The EnableFieldsEnum value
      @param plApplyFocus Apply Focus to the SmartUpdatableBrowser
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID EnableFields (pcFields AS CHARACTER,
                                        plApplyFocus AS LOGICAL):

        DEFINE VARIABLE i                     AS INTEGER             NO-UNDO .
        DEFINE VARIABLE cList                 AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE iFirstColumn          AS INTEGER             NO-UNDO INIT -1.
        DEFINE VARIABLE iFirstVisiblePosition AS INTEGER             NO-UNDO INIT 1000.
        DEFINE VARIABLE oChildRow             AS UltraGridRow        NO-UNDO .

        DEFINE VARIABLE oCancelableEventArgs  AS CancelableEventArgs NO-UNDO .

        ASSIGN oCancelableEventArgs = NEW CancelableEventArgs () .

        THIS-OBJECT:OnBeforeEnableFields (oCancelableEventArgs) .

        IF VALID-OBJECT (oCancelableEventArgs) AND oCancelableEventArgs:Cancel THEN
            RETURN .

        IF plApplyFocus AND VALID-OBJECT (THIS-OBJECT:SmartTableIOSource) THEN
           THIS-OBJECT:EnsureVisible () .

        IF NOT VALID-OBJECT (THIS-OBJECT:ActiveRow) AND THIS-OBJECT:SaveMode = FALSE THEN
            UNDO, THROW NEW AppError (SmartComponentLibraryCustomizer:SmartUpdatableBrowser_UnableToEnableFields_NoActiveRow, 0) .

        THIS-OBJECT:SmartTableIOState = TableIOStateEnum:FieldsEnabled .

        /* Mike Fechner, Consultingwerk Ltd. 19.04.2016
           SCL-826 - support for AllowAddNew when SaveMode = TRUE */
        IF THIS-OBJECT:SaveMode THEN
            ASSIGN THIS-OBJECT:DisplayLayout:Override:AllowAddNew = THIS-OBJECT:AllowAddNew .
        ELSE
            THIS-OBJECT:DisplayLayout:Override:AllowAddNew = Infragistics.Win.UltraWinGrid.AllowAddNew:No.

        THIS-OBJECT:DisplayLayout:Override:AllowDelete = Infragistics.Win.DefaultableBoolean:True.
        THIS-OBJECT:DisplayLayout:Override:AllowUpdate = Infragistics.Win.DefaultableBoolean:True.

        /* What happens when clicking on a Cell */
        THIS-OBJECT:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText .

        /* SCL-1340: Add is always Add */
        IF pcFields = EnableFieldsEnum:Add THEN
            cList = THIS-OBJECT:DisabledColumnsForAdd .
        ELSE DO:
            /* SCL-1340: But an Update of an uncommitted Add may be Add */
            IF THIS-OBJECT:AllowEditOfUncommittedAdd = TRUE AND
               TYPE-OF (THIS-OBJECT:SmartDataSource, SmartDatasetAdapter) AND
               CAST (THIS-OBJECT:SmartDataSource, SmartDatasetAdapter):IsRowUncommittedAdd() THEN

                ASSIGN cList = THIS-OBJECT:DisabledColumnsForAdd .
            ELSE
                ASSIGN cList = THIS-OBJECT:DisabledColumnsForUpdate .
        END.

        /* Set key columns to read-only */
        DO i = 0 TO THIS-OBJECT:DisplayLayout:Bands[0]:Columns:Count - 1:
            IF ListHelper:EntryIsInList (THIS-OBJECT:DisplayLayout:Bands[0]:Columns[i]:Key, cList) THEN
                THIS-OBJECT:DisplayLayout:Bands[0]:Columns[i]:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
            ELSE DO:
                THIS-OBJECT:DisplayLayout:Bands[0]:Columns[i]:CellActivation = Infragistics.Win.UltraWinGrid.Activation:AllowEdit.

                IF THIS-OBJECT:DisplayLayout:Bands[0]:Columns[i]:Hidden = FALSE AND
                   THIS-OBJECT:DisplayLayout:Bands[0]:Columns[i]:Header:VisiblePosition < iFirstVisiblePosition THEN
                    ASSIGN iFirstVisiblePosition = THIS-OBJECT:DisplayLayout:Bands[0]:Columns[i]:Header:VisiblePosition
                           iFirstColumn          = i .
            END.
        END.

        /* Put focus into an enabled cell and start editing */
        IF iFirstColumn >= 0 AND VALID-OBJECT (THIS-OBJECT:ActiveRow) THEN DO:
            IF THIS-OBJECT:ActiveRow:IsDataRow THEN
                THIS-OBJECT:ActiveCell = THIS-OBJECT:ActiveRow:Cells[iFirstColumn] .

            /* When the active row is a group by row, we should activate the first data row */
            ELSE IF THIS-OBJECT:ActiveRow:IsGroupByRow THEN DO:

                oChildRow = FindFirstChildDataRow (CAST (THIS-OBJECT:ActiveRow, UltraGridGroupByRow)) .

                IF VALID-OBJECT (oChildRow) THEN DO:
                    oChildRow:Activate ()  .
                    THIS-OBJECT:ActiveCell = THIS-OBJECT:ActiveRow:Cells[iFirstColumn] .
                END.
            END.
        END.

        IF plApplyFocus THEN
            THIS-OBJECT:Focus () .

        /* SCL-1515: Don't call EnterEditMode during an ActiveRowChange event
           as it might steal the Focus from the Grid cell */
        IF NOT System.Environment:StackTrace MATCHES
            "*at Infragistics.Win.UltraWinGrid.UltraGrid.OnActiveRowChange*":U AND
           VALID-OBJECT (THIS-OBJECT:ActiveRow) THEN

            THIS-OBJECT:PerformAction(Infragistics.Win.UltraWinGrid.UltraGridAction:EnterEditMode, FALSE, FALSE) .

        /* EnableFields in every registered SmartGroupAssignTarget */
        FOR EACH ttSmartGroupAssignTarget WHERE ttSmartGroupAssignTarget.RecordOwner = THIS-OBJECT NO-LOCK:
            CAST(ttSmartGroupAssignTarget.SmartGroupAssignTarget, ISmartGroupAssignTarget):EnableFields(pcFields).
        END.

        OnFieldsEnabled (System.EventArgs:Empty) .
        OnAfterFieldsEnabled (System.EventArgs:Empty) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Enable the Controls after an Update.
                 This Method is called by the SmartDataAdapter when setting the
                 SmartDataSourceState Property to Navigatable.
                 Enforced by Interface ISmartDataTarget.
        Notes:   Use this only in a Browser!
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID EnableAfterUpdate():

        SUPER:EnableAfterUpdate () .

        IF VALID-OBJECT (THIS-OBJECT:SmartTableIOSource) THEN
            THIS-OBJECT:EvaluateTableIOState() .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Ensures that this SmartUpdatableBrowser is visible
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID EnsureVisible ():

        DEFINE VARIABLE oForm           AS System.Windows.Forms.Form                               NO-UNDO .
        DEFINE VARIABLE oTabPageControl AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO .

        oForm = THIS-OBJECT:FindForm () .

        IF VALID-OBJECT (oForm) THEN DO:
           IF Progress.Util.EnumHelper:AreEqual(oForm:WindowState, FormWindowState:Minimized) THEN
               oForm:WindowState = System.Windows.Forms.FormWindowState:Normal .

           oForm:Activate() .
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

    /*------------------------------------------------------------------------------
       Purpose: This Method sets the Property SmartTableIOState to a value according
                to the SmartDataSources state of the current class. Possible values
                for SmartTableIOState are defined in the TableIOStateEnum.
       Notes:   This Method is called by the SmartDataAdapter when the
                PositionChangedHandler is fired or inside the SmartViewerControl if
                a new BindingSource is attached.
                Enforced by ISmartDataTarget
   ------------------------------------------------------------------------------*/
    METHOD PUBLIC OVERRIDE VOID EvaluateTableIOState ():

        /* Validate SmartTableIOState on init */
        /* Check if data is available */
        IF VALID-OBJECT (THIS-OBJECT:SmartDataSource) AND VALID-OBJECT (THIS-OBJECT:SmartDataSource:BindingSource) THEN
        DO:
            IF THIS-OBJECT:SmartDataSource:BindingSource:Count > 0 THEN
            DO:
                /* FilterRow should be similar to no record available  */
                IF VALID-OBJECT (THIS-OBJECT:ActiveRow) AND THIS-OBJECT:ActiveRow:IsFilterRow THEN
                    THIS-OBJECT:SmartTableIOState = TableIOStateEnum:NoRecordAvailable .

                /* Don't set TableIO State to FieldsDisabled, when fields are enabled */
                ELSE IF THIS-OBJECT:AddingRecord = FALSE AND THIS-OBJECT:SmartTableIOState <> TableIOStateEnum:FieldsEnabled THEN
                DO:
                    /* Mike Fechner, Consultingwerk Ltd. 01.11.2010
                       SaveMode */
                    IF THIS-OBJECT:SaveMode THEN
                        THIS-OBJECT:EnableFields (EnableFieldsEnum:Update, FALSE)  .
                    ELSE
                        THIS-OBJECT:SmartTableIOState = TableIOStateEnum:FieldsDisabled .

                    ASSIGN THIS-OBJECT:AddingRecord  = FALSE
                           THIS-OBJECT:CopyingRecord = FALSE .
                END.
            END.
            ELSE DO:
                IF TYPE-OF (THIS-OBJECT:SmartDataSource, SmartDataAdapter) AND
                   VALID-OBJECT (CAST (THIS-OBJECT:SmartDataSource, SmartDataAdapter):SmartDataSource) AND
                   TYPE-OF (CAST (THIS-OBJECT:SmartDataSource, SmartDataAdapter):SmartDataSource, SmartDataAdapter) AND

                   CAST (CAST (THIS-OBJECT:SmartDataSource, SmartDataAdapter):SmartDataSource, SmartDataAdapter):RecordPosition = RecordPositionEnum:NoRecordAvailable THEN

                    THIS-OBJECT:SmartTableIOState = TableIOStateEnum:NoParentRecordAvailable .
                ELSE
                    THIS-OBJECT:SmartTableIOState = TableIOStateEnum:NoRecordAvailable .
            END.
        END.
        ELSE THIS-OBJECT:SmartTableIOState = TableIOStateEnum:NoDataSource .

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Finds the first child row of a group by row that is a data row
        Notes:
        @param poGroupByRow The group by row to find the first child of
        @return The UltraGridRow that is a data row of ?
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC UltraGridRow FindFirstChildDataRow (poGroupByRow AS UltraGridGroupByRow):

        DEFINE VARIABLE oChildRow AS UltraGridRow NO-UNDO .

        IF poGroupByRow:Rows:Count = 0 THEN
            RETURN ? .

        oChildRow = poGroupByRow:Rows[0] .

        IF oChildRow:IsDataRow THEN
            RETURN oChildRow .
        IF oChildRow:IsGroupByRow THEN
            RETURN THIS-OBJECT:FindFirstChildDataRow (CAST (oChildRow, UltraGridGroupByRow)) .

        RETURN ? .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Handles an error raised by validation routines
        Notes:   Displays the Error message and focusses the field that caused the error
        @param poError The reference to the validation error instance
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID HandleValidationError (poError AS Progress.Lang.Error):

        DEFINE VARIABLE cErrorField AS CHARACTER NO-UNDO.

        THIS-OBJECT:DisplayError (poError).

        IF poError:NumMessages > 0 AND NUM-ENTRIES (poError:GetMessage (1), CHR (4)) > 1 THEN
            ASSIGN cErrorField = ENTRY (2, poError:GetMessage (1), CHR (4)) .

        IF cErrorField > "":U THEN DO:
            THIS-OBJECT:Focus () .

            THIS-OBJECT:ActivateCurrentRow() .

            IF THIS-OBJECT:DisplayLayout:Bands[0]:Columns:Exists (cErrorField) THEN DO:
                THIS-OBJECT:ActiveCell = THIS-OBJECT:ActiveRow:Cells[cErrorField] .
            END.

            THIS-OBJECT:PerformAction (Infragistics.Win.UltraWinGrid.UltraGridAction:EnterEditMode) .

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
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Check if it is ok to close the parent form.
        Notes:
        @param plShowMessage When TRUE a message is Shown explaining why it is not ok to close the parent form
        @param poCloseReason The System.Windows.Forms.CloseReason passed from then OnFormClosing Method of the parent form giving the reason why the form is closed
        @return Logical value indicating if the parent Form may be closed
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC OVERRIDE LOGICAL IsOkToClose (plShowMessage AS LOGICAL,
                                                poCloseReason AS System.Windows.Forms.CloseReason):

        DEFINE VARIABLE lOk       AS LOGICAL          NO-UNDO.
        DEFINE VARIABLE lPrevious AS LOGICAL          NO-UNDO.
        DEFINE VARIABLE oResult   AS DialogResultEnum NO-UNDO.

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

    /*------------------------------------------------------------------------------
        Purpose: Raises the AddingRecordChanged event
        Notes:
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnAddingRecordChanged (e AS System.EventArgs):

        IF NOT VALID-OBJECT (e) THEN
            e = System.EventArgs:Empty .

        THIS-OBJECT:AddingRecordChanged:Publish (THIS-OBJECT, e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the AfterAddRecord event
        Notes:
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnAfterAddRecord (e AS System.EventArgs):

        IF NOT VALID-OBJECT (e) THEN
            e = System.EventArgs:Empty .

        THIS-OBJECT:AfterAddRecord:Publish (THIS-OBJECT, e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the AfterCancelUpdate event
        Notes:
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnAfterCancelUpdate (e AS System.EventArgs):

        IF NOT VALID-OBJECT (e) THEN
            e = System.EventArgs:Empty .

        THIS-OBJECT:AfterCancelUpdate:Publish (THIS-OBJECT, e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the AfterCellUpdate event
        Notes:   Called after a cell has been updated
        @param e The CellEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnAfterCellUpdate (e AS CellEventArgs):

        SUPER:OnAfterCellUpdate (e) .

        IF e:Cell:IsDataCell THEN DO:
            IF THIS-OBJECT:SmartTableIOState <> TableIOStateEnum:FieldsEnabled THEN
                RETURN .

            THIS-OBJECT:SmartTableIOState = TableIOStateEnum:ModifyingData .

            /* Tell the SmartDataSource that the user has started to update data. */
            IF VALID-OBJECT (THIS-OBJECT:SmartDataSource) THEN
                THIS-OBJECT:SmartDataSource:BeginUpdateState (THIS-OBJECT).
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the AfterCellListCloseUp event
        Notes:   Called after a CellList is closed up
        @param e The CellEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnAfterCellListCloseUp (e AS CellEventArgs):

        SUPER:OnAfterCellListCloseUp (e) .

        IF THIS-OBJECT:BeginEditModeOnCellListCloseUp AND e:Cell:IsDataCell THEN DO:
            IF THIS-OBJECT:SmartTableIOState <> TableIOStateEnum:FieldsEnabled THEN
                RETURN .

            THIS-OBJECT:SmartTableIOState = TableIOStateEnum:ModifyingData .

            /* Tell the SmartDataSource that the user has started to update data. */
            IF VALID-OBJECT (THIS-OBJECT:SmartDataSource) THEN
                THIS-OBJECT:SmartDataSource:BeginUpdateState (THIS-OBJECT).
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the AfterCopyRecord event
        Notes:
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnAfterCopyRecord (e AS System.EventArgs):

        IF NOT VALID-OBJECT (e) THEN
            e = System.EventArgs:Empty .

        THIS-OBJECT:AfterCopyRecord:Publish (THIS-OBJECT, e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Called after a cell is activated
        Notes:   Deactivates cell after Cancel of Save during selection of a different
                 row
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnAfterCellActivate():

        SUPER:OnAfterCellActivate () .

        IF lDeactivateRow THEN DO:
            THIS-OBJECT:PerformAction (Infragistics.Win.UltraWinGrid.UltraGridAction:DeactivateCell) .
            ASSIGN lDeactivateRow = FALSE .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the AfterFieldsEnabled
        Notes:
        @param e The System.EventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnAfterFieldsEnabled (e AS System.EventArgs):

        IF NOT VALID-OBJECT (e) THEN
            e = System.EventArgs:Empty .

        THIS-OBJECT:AfterFieldsEnabled:Publish (THIS-OBJECT, e) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the AfterPerformAction event
        Notes:
        @param e The AfterUltraGridPerformActionEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnAfterPerformAction (e AS AfterUltraGridPerformActionEventArgs):

        IF Progress.Util.EnumHelper:AreEqual(e:UltraGridAction,
                                             Infragistics.Win.UltraWinGrid.UltraGridAction:UndoCell) THEN DO:

            THIS-OBJECT:CancelUpdate () .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
       Purpose: Called after a row is activated
       Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnAfterRowActivate ():

        SUPER:OnAfterRowActivate () .

        THIS-OBJECT:EvaluateTableIOState () .

        /* SCL-1340: When fields are enabled, re-evaluate the Add/Update for new row */
        IF THIS-OBJECT:SmartTableIOState = TableIOStateEnum:FieldsEnabled THEN
            THIS-OBJECT:EnableFields(EnableFieldsEnum:Update, FALSE) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the AfterSaveChanges event
        Notes:
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnAfterSaveChanges (e AS System.EventArgs):

        IF NOT VALID-OBJECT (e) THEN
            e = System.EventArgs:Empty .

        THIS-OBJECT:AfterSaveChanges:Publish (THIS-OBJECT, e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the BeforeAddRecord event
        Notes:
        @param e The CancelEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnBeforeAddRecord (e AS System.ComponentModel.CancelEventArgs):

        IF NOT VALID-OBJECT (e) THEN
            UNDO, THROW NEW AppError ("This event needs a valid CancelEventArgs object.", 0) .

        THIS-OBJECT:BeforeAddRecord:Publish (THIS-OBJECT, e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the BeforeCancelUpdate event
        Notes:
        @param e The CancelEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnBeforeCancelUpdate (e AS System.ComponentModel.CancelEventArgs):

        IF NOT VALID-OBJECT (e) THEN
            UNDO, THROW NEW AppError ("This event needs a valid CancelEventArgs object.", 0) .

        THIS-OBJECT:BeforeCancelUpdate:Publish (THIS-OBJECT, e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Called before a cell is deactivated
        Notes:
        @param e An System.ComponentModel.CancelEventArgs instance that contains the event data.
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnBeforeCellDeactivate (e AS System.ComponentModel.CancelEventArgs):

        SUPER:OnBeforeCellDeactivate (e) .

        IF e:Cancel THEN
            RETURN .

        IF VALID-OBJECT (THIS-OBJECT:ActiveCell) AND /*THIS-OBJECT:ActiveCell:IsInEditMode AND*/
           VALID-OBJECT (THIS-OBJECT:ActiveCell:EditorComponentResolved) AND
           TYPE-OF (THIS-OBJECT:ActiveCell:EditorComponentResolved, IEmbeddedSmartEditor) THEN

           CAST (THIS-OBJECT:ActiveCell:EditorComponentResolved, IEmbeddedSmartEditor):PerformLeave (e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the BeforeCopyRecord event
        Notes:
        @param e The CancelEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnBeforeCopyRecord (e AS System.ComponentModel.CancelEventArgs):

        IF NOT VALID-OBJECT (e) THEN
            UNDO, THROW NEW AppError ("This event needs a valid CancelEventArgs object.", 0) .

        THIS-OBJECT:BeforeCopyRecord:Publish (THIS-OBJECT, e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the BeforeEnableFields
        Notes:
        @param e The CancelableEventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnBeforeEnableFields (e AS CancelableEventArgs):

        Consultingwerk.Assertion.EventArgsAssert:IsValid (e, "BeforeEnableFields":U) .

        THIS-OBJECT:BeforeEnableFields:Publish (THIS-OBJECT, e) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the BeforeRowDeactivate event
        Notes:   Called before a row is deactivated
        @param e The CancelEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnBeforeRowDeactivate (e AS System.ComponentModel.CancelEventArgs):

        DEFINE VARIABLE lOk AS LOGICAL NO-UNDO INIT FALSE .

        /* Mike Fechner, Consultingwerk Ltd. 19.07.2010
           WAIT-FOR Workaround, SCL-843 - only required on 10.2B */
        &IF PROVERSION BEGINS "10" &THEN
        Consultingwerk.Util.ErrorHelper:ErrorMessageDialogAllowed = FALSE .
        &ENDIF

        ASSIGN lDeactivateRow = FALSE  .

        IF NOT lSaving AND
           NOT lCancelling AND
           THIS-OBJECT:SmartTableIOState = TableIOStateEnum:ModifyingData THEN DO:

            IF THIS-OBJECT:PromptForSaveValuesOnRowDeactivate = TRUE THEN
                ASSIGN lOk = THIS-OBJECT:PromptForSaveValuesOnRowDeactivate () .
            ELSE
                ASSIGN lOk = TRUE .

            IF lOk THEN DO:
                ASSIGN lDisableFieldsOnSave = FALSE .

                THIS-OBJECT:SaveChanges () .

                /* Mike Fechner, Consultingwerk Ltd. 18.10.2013
                   When we are still modifying the record after SaveChanges, we know,
                   that the save was not successful and should stay in the current row */
                IF THIS-OBJECT:SmartTableIOState = TableIOStateEnum:ModifyingData THEN DO:
                    e:Cancel = TRUE .
                    RETURN .
                END .

                THIS-OBJECT:SmartTableIOState = TableIOStateEnum:FieldsEnabled .
            END.
            ELSE DO:
                THIS-OBJECT:CancelUpdate() .
                ASSIGN lDeactivateRow = TRUE  .
            END.
        END.

        SUPER:OnBeforeRowDeactivate (e) .

        FINALLY:
            ASSIGN lDisableFieldsOnSave = TRUE
&IF PROVERSION BEGINS "10" &THEN
                   Consultingwerk.Util.ErrorHelper:ErrorMessageDialogAllowed = TRUE
&ENDIF
                    .
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the BeforeSaveChanges event
        Notes:
        @param e The CancelEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnBeforeSaveChanges (e AS System.ComponentModel.CancelEventArgs):

        IF NOT VALID-OBJECT (e) THEN
            UNDO, THROW NEW AppError ("This event needs a valid CancelEventArgs object.", 0) .

        THIS-OBJECT:BeforeSaveChanges:Publish (THIS-OBJECT, e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the CellChange event
        Notes:
        @param e The CellEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnCellChange (e AS Infragistics.Win.UltraWinGrid.CellEventArgs):

        SUPER:OnCellChange (e) .

        IF e:Cell:IsDataCell THEN DO:
            IF THIS-OBJECT:SmartTableIOState <> TableIOStateEnum:FieldsEnabled THEN
                RETURN .

            THIS-OBJECT:SmartTableIOState = TableIOStateEnum:ModifyingData .

            /* Tell the SmartDataSource that the user has started to update data. */
            IF VALID-OBJECT(THIS-OBJECT:SmartDataSource) THEN
                THIS-OBJECT:SmartDataSource:BeginUpdateState (THIS-OBJECT).
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the CopyingRecordChanged
        Notes:
        @param e The System.EventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnCopyingRecordChanged (e AS System.EventArgs):

        IF NOT VALID-OBJECT (e) THEN
            e = System.EventArgs:Empty .

        THIS-OBJECT:CopyingRecordChanged:Publish (THIS-OBJECT, e) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Invoked during the System.ComponentModel.ISupportInitialize.EndInit
                 of the component.
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnEndInit ():

        SUPER:OnEndInit() .

        THIS-OBJECT:DisableFields () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the FieldsDisabled event.
        Notes:
        @param e The System.EventArgs with the data for thie event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnFieldsDisabled (e AS System.EventArgs):

        THIS-OBJECT:FieldsDisabled:Publish (THIS-OBJECT, e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the FieldsEnabled event.
        Notes:
        @param e The System.EventArgs with the data for thie event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnFieldsEnabled (e AS System.EventArgs):

        THIS-OBJECT:FieldsEnabled:Publish (THIS-OBJECT, e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the Key Down Event
        Notes:   Bug 2640: Enable Excel-like navigation during update
                 SCL-277: Ability to force cursor key navigation (prevent spinning in
                          nummeric fields, etc.). The Grid will not handle the cursor
                          keys in an ActiveCell itself anymore.
        @param e The KeyEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnKeyDown (e AS System.Windows.Forms.KeyEventArgs):

        DEFINE VARIABLE lRaiseEvent AS LOGICAL NO-UNDO INIT TRUE .

        /* Mike Fechner, Consultingwerk Ltd. 06.07.2015
           SCL-899: Don't handle Enter or Return here for Filter Cells */
        IF VALID-OBJECT (THIS-OBJECT:ActiveCell) AND THIS-OBJECT:ActiveCell:IsFilterRowCell AND
            (Progress.Util.EnumHelper:AreEqual (e:KeyCode, System.Windows.Forms.Keys:Return)  OR
             Progress.Util.EnumHelper:AreEqual (e:KeyCode, System.Windows.Forms.Keys:Enter)) THEN DO:

            SUPER:OnKeyDown (e) .

            RETURN .
        END.

        IF THIS-OBJECT:SimpleKeyboardNavigationDuringUpdateForced AND
           THIS-OBJECT:SimpleKeyboardNavigationDuringUpdate AND
           VALID-OBJECT (THIS-OBJECT:ActiveCell) AND THIS-OBJECT:ActiveCell:IsInEditMode THEN DO:

                IF Progress.Util.EnumHelper:AreEqual (e:KeyCode, System.Windows.Forms.Keys:Down) OR
                   Progress.Util.EnumHelper:AreEqual (e:KeyCode, System.Windows.Forms.Keys:Enter) OR
                   Progress.Util.EnumHelper:AreEqual (e:KeyCode, System.Windows.Forms.Keys:Return) OR
                   (Progress.Util.EnumHelper:AreEqual (e:KeyCode, System.Windows.Forms.Keys:Up) AND THIS-OBJECT:SimpleKeyboardNavigationDuringUpdateForLeftRight) OR
                   (Progress.Util.EnumHelper:AreEqual (e:KeyCode, System.Windows.Forms.Keys:Left) AND THIS-OBJECT:SimpleKeyboardNavigationDuringUpdateForLeftRight) OR
                   Progress.Util.EnumHelper:AreEqual (e:KeyCode, System.Windows.Forms.Keys:Right) THEN

                   ASSIGN lRaiseEvent = FALSE .
        END .

        IF lRaiseEvent = TRUE THEN
            SUPER:OnKeyDown (e) .

        IF (NOT e:Handled) AND (THIS-OBJECT:SimpleKeyboardNavigationDuringUpdate) THEN DO:
            /* Mike Fechner, Consultingwerk Ltd. 29.04.2011
               Bug 2640: When a cell is on EditMode, enable cursor keys and Enter/Return
               for navigation to neighboring cells. e:Handled will be set so that the
               Grid itself will not perform it's default action. */
            IF VALID-OBJECT (THIS-OBJECT:ActiveCell) AND THIS-OBJECT:ActiveCell:IsInEditMode THEN DO:

                IF Progress.Util.EnumHelper:AreEqual (e:KeyCode, System.Windows.Forms.Keys:Down) OR
                   Progress.Util.EnumHelper:AreEqual (e:KeyCode, System.Windows.Forms.Keys:Enter) OR
                   Progress.Util.EnumHelper:AreEqual (e:KeyCode, System.Windows.Forms.Keys:Return) THEN DO:

                    THIS-OBJECT:PerformAction (Infragistics.Win.UltraWinGrid.UltraGridAction:BelowCell) .
                    THIS-OBJECT:PerformAction (Infragistics.Win.UltraWinGrid.UltraGridAction:EnterEditMode) .
                    e:Handled = TRUE .
                END.
                ELSE IF Progress.Util.EnumHelper:AreEqual (e:KeyCode, System.Windows.Forms.Keys:Up) THEN DO:

                    THIS-OBJECT:PerformAction (Infragistics.Win.UltraWinGrid.UltraGridAction:AboveCell) .
                    THIS-OBJECT:PerformAction (Infragistics.Win.UltraWinGrid.UltraGridAction:EnterEditMode) .
                    e:Handled = TRUE .
                END.
                ELSE IF Progress.Util.EnumHelper:AreEqual (e:KeyCode, System.Windows.Forms.Keys:Left) AND THIS-OBJECT:SimpleKeyboardNavigationDuringUpdateForLeftRight THEN DO:

                    THIS-OBJECT:PerformAction (Infragistics.Win.UltraWinGrid.UltraGridAction:PrevCell) .
                    THIS-OBJECT:PerformAction (Infragistics.Win.UltraWinGrid.UltraGridAction:EnterEditMode) .
                    e:Handled = TRUE .
                END.
                ELSE IF Progress.Util.EnumHelper:AreEqual (e:KeyCode, System.Windows.Forms.Keys:Right) AND THIS-OBJECT:SimpleKeyboardNavigationDuringUpdateForLeftRight THEN DO:

                    THIS-OBJECT:PerformAction (Infragistics.Win.UltraWinGrid.UltraGridAction:NextCell) .
                    THIS-OBJECT:PerformAction (Infragistics.Win.UltraWinGrid.UltraGridAction:EnterEditMode) .
                    e:Handled = TRUE .
                END.
            END.
        END.

        IF VALID-OBJECT (THIS-OBJECT:ActiveCell) AND THIS-OBJECT:ActiveCell:IsInEditMode AND
           VALID-OBJECT (THIS-OBJECT:ActiveCell:EditorComponentResolved) AND
           TYPE-OF (THIS-OBJECT:ActiveCell:EditorComponentResolved, IEmbeddedSmartEditor) THEN

           CAST (THIS-OBJECT:ActiveCell:EditorComponentResolved, IEmbeddedSmartEditor):PerformKeyDown (e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the KeyPress event
        Notes:   Starts the Lookup Timer when the Control has Focus
        @param e The KeyPressEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID OnKeyPress (e AS System.Windows.Forms.KeyPressEventArgs):

        SUPER:OnKeyPress (e).

        IF VALID-OBJECT (THIS-OBJECT:ActiveCell) AND THIS-OBJECT:ActiveCell:IsInEditMode AND
           VALID-OBJECT (THIS-OBJECT:ActiveCell:EditorComponentResolved) AND
           TYPE-OF (THIS-OBJECT:ActiveCell:EditorComponentResolved, IEmbeddedSmartEditor) THEN

           CAST (THIS-OBJECT:ActiveCell:EditorComponentResolved, IEmbeddedSmartEditor):PerformKeyPress (e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the Leave event
        Notes:   Allows invocation of SaveChanges when there are pending updates
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnLeave (e AS System.EventArgs):

        DEFINE VARIABLE oTargetControl AS System.windows.Forms.Control NO-UNDO .

        /* Mike Fechner, Consultingwerk Ltd. 28.11.2014
           SCL-570 */
        IF THIS-OBJECT:SaveChangesOnLeave AND
           THIS-OBJECT:SmartTableIOState = TableIOStateEnum:ModifyingData THEN DO:

            IF VALID-OBJECT (THIS-OBJECT:FindForm()) THEN
                oTargetControl = THIS-OBJECT:FindForm():ActiveControl .

            THIS-OBJECT:SaveChanges (FALSE) .

            IF VALID-OBJECT (oTargetControl) THEN
                oTargetControl:Focus() .
        END.

        SUPER:OnLeave (e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Publishes the SmartGroupAssignSourceChanged event
        Notes:
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnSmartTableIOSourceChanged (e AS System.EventArgs):

        IF NOT VALID-OBJECT (e) THEN
            e = System.EventArgs:Empty .

        THIS-OBJECT:SmartTableIOSourceChanged:Publish (THIS-OBJECT, e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Publishes the SmartTableIOStateChanged event
        Notes:
        @param e The SmartTableIOStateChangedEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnSmartTableIOStateChanged (e AS SmartTableIOStateChangedEventArgs):

        IF NOT VALID-OBJECT (e) THEN
          ASSIGN e = NEW SmartTableIOStateChangedEventArgs (THIS-OBJECT:SmartTableIOState) .

        THIS-OBJECT:SmartTableIOStateChanged:Publish (THIS-OBJECT, e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Performs lookup on the current grid row
        Notes:   Executed during SaveChanges
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID PerformLookups ():

        IF NOT VALID-OBJECT (THIS-OBJECT:ActiveRow) THEN
            RETURN .

        IF NOT THIS-OBJECT:ActiveRow:IsDataRow THEN
            RETURN .

        {Consultingwerk/foreach.i UltraGridCell oCell in THIS-OBJECT:ActiveRow:Cells}

            IF VALID-OBJECT (oCell:EditorComponentResolved) AND
               TYPE-OF (oCell:EditorComponentResolved, SmartLookup) THEN DO:

                CAST (oCell:EditorComponentResolved, SmartLookup):PerformLookupIfActive () .

            END.
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Prompt the user to save changes when selecting a different row
                 while editing a row.
        Notes:   This method is meant to be overridden to modify the behaviour
                 when the user selected a different row.
        @return Logical value indicating if the current values shoudl be saved
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED LOGICAL PromptForSaveValuesOnRowDeactivate ():

        DEFINE VARIABLE oDialogResult AS DialogResultEnum NO-UNDO .

        oDialogResult = MessageFormHelper:AskQuestion (SmartComponentLibraryCustomizer:SmartUpdatableBrowser_SaveRecordPromt_On_RowDeactivate) .

        IF oDialogResult:Equals (DialogResultEnum:DialogResultYes) THEN
            RETURN TRUE .
        ELSE
            RETURN FALSE .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Register a SmartDataTarget with the SmartGroupAssignTarget
        Notes:   This Method is executed every time a new SmartGroupAssignSource is
                 registered to the corresponding Property of a Class implementing the
                 Interface ISmartGroupAssignSource.
                 Enforced by Interface ISmartGroupAssignSource.
        @param poSmartGroupAssignTarget The ISmartGroupAssignTarget to register
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RegisterSmartGroupAssignTarget (poSmartGroupAssignTarget AS ISmartGroupAssignTarget):

        CREATE ttSmartGroupAssignTarget.

        ASSIGN
            ttSmartGroupAssignTarget.RecordOwner            = THIS-OBJECT
            ttSmartGroupAssignTarget.SmartGroupAssignTarget = poSmartGroupAssignTarget
            .

        RELEASE ttSmartGroupAssignTarget.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Removes a SmartTableIOSource from the SmartDataAdapter
                 instance
        Notes:   Enables multiple SmartTableIOSource to the SmartDataAdapter.
        @param poSmartTableIOSource The ISmartTableIOSource to remove from this instance
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RemoveSmartTableIOSource (poSmartTableIOSource AS ISmartTableIOSource):

        DEFINE VARIABLE lDeleteDefault AS LOGICAL NO-UNDO INIT FALSE .

        IF poSmartTableIOSource = oSmartTableIOSource THEN
            lDeleteDefault = TRUE .

        IF VALID-OBJECT (poSmartTableIOSource) THEN
            poSmartTableIOSource:DeregisterSmartTableIOTarget (THIS-OBJECT) .

        IF lDeleteDefault THEN
            ASSIGN oSmartTableIOSource = ? .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Restore the Current Values of all Controls on a SmartViewerControl
                 and in all SmartGroupAssignTargets.
        Notes:   This Method is called by CreateRecord from the current Class.
                 Enforced by Interface ISmartGroupAssignTarget.
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RestoreValuesForCopy ():

        DEFINE VARIABLE i      AS INTEGER       NO-UNDO.
        DEFINE VARIABLE cKey   AS CHARACTER     NO-UNDO.
        DEFINE VARIABLE oValue AS System.Object NO-UNDO .

        IF VALID-OBJECT (oStoredValues) then do:

            cols: DO i = 0 TO THIS-OBJECT:DisplayLayout:Bands[0]:Columns:Count - 1:

                ASSIGN cKey = THIS-OBJECT:DisplayLayout:Bands[0]:Columns[i]:Key .

                /* Mike Fechner, Consultingwerk Ltd. 27.11.2014
                   SCL-562 : Ability to exclude columns from copy */
                IF THIS-OBJECT:ExcludeColumnsFromCopy > "":U AND
                   LOOKUP (cKey, THIS-OBJECT:ExcludeColumnsFromCopy) > 0 THEN
                    NEXT .

                IF oStoredValues:ContainsKey (cKey) THEN DO:
                    oValue = oStoredValues[cKey] .

                    IF NOT VALID-OBJECT (oValue)
                       AND NOT EnumHelper:Equals (THIS-OBJECT:DisplayLayout:Bands[0]:Columns[i]:Nullable,
                                                  Nullable:Nothing)
                       AND NOT

                       THIS-OBJECT:DisplayLayout:Bands[0]:Columns[i]:DataType = TypeHelper:GetType ("System.String")

                       THEN DO:
                        /* See http://knowledgebase.progress.com/articles/Article/000036670 and
                           http://help.infragistics.com/Help/Doc/WinForms/2014.1/CLR4.0/html/Infragistics4.Win.UltraWinGrid.v14.1~Infragistics.Win.UltraWinGrid.Nullable.html
                           The UltraGrid will raise a Unable to convert from '(null)' System.In32.
                           warning/error when trying to assign ? to a cell's value. The grid's layout
                           should be using column:Nullable = Nullable:Nothing for a columns, where ?
                           is expected during copy. */
                        MESSAGE "Unable to copy unknown value for column" cKey "because" SKIP
                                "the grid columns Nullable property is not set to ~"Nothing~"."
                            VIEW-AS ALERT-BOX WARNING.

                        NEXT cols .
                    END.

                    THIS-OBJECT:ActiveRow:Cells[cKey]:Value = oValue.
                END.
            END.
        END.

        FOR EACH ttSmartGroupAssignTarget WHERE ttSmartGroupAssignTarget.RecordOwner = THIS-OBJECT:
            CAST(ttSmartGroupAssignTarget.SmartGroupAssignTarget, ISmartGroupAssignTarget):RestoreValuesForCopy () .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Saves the current changes to the database.
        Notes:   This Method is called by the Toolbar Classes SmartToolbarController
                 or SmartToolStripContainer by pressing the corresponding Tool.
                 Enforced by Interface ISmartDataTarget or ISmartGroupAssignTarget.
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SaveChanges ():

        THIS-OBJECT:SaveChanges (TRUE) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Saves the current changes to the database.
        Notes:   Internal method that allows to specify if or if not the Control is
                 supported to Focus() itself
        @param plEnforceFocus Logical value indicating if the Control should Focus() itself
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SaveChanges (plEnforceFocus AS LOGICAL):

        DEFINE VARIABLE e AS System.ComponentModel.CancelEventArgs NO-UNDO .

        ASSIGN lSaving = TRUE .

        /* Save changes in every registered SmartGroupAssignTarget */
        FOR EACH ttSmartGroupAssignTarget WHERE ttSmartGroupAssignTarget.RecordOwner = THIS-OBJECT NO-LOCK ON ERROR UNDO, THROW:
            CAST(ttSmartGroupAssignTarget.SmartGroupAssignTarget, ISmartGroupAssignTarget):SaveChanges().
        END.

        IF plEnforceFocus AND NOT THIS-OBJECT:Focused THEN
            THIS-OBJECT:Focus() .

        THIS-OBJECT:PerformLookups () .

        THIS-OBJECT:PerformAction (Infragistics.Win.UltraWinGrid.UltraGridAction:ExitEditMode) .

        e = NEW System.ComponentModel.CancelEventArgs () .
        OnBeforeSaveChanges (e) .

        IF VALID-OBJECT (e) AND e:Cancel THEN
            RETURN .

        /* Save record in SmartDataSource */
        THIS-OBJECT:SmartDataSource:UpdateRow () .

        /* Fields will only be disabled in case of success. Any error from above
           will be catched by the CATCH block below */
        IF lDisableFieldsOnSave AND THIS-OBJECT:SaveMode = FALSE THEN
            THIS-OBJECT:DisableFields() .
        ELSE
            THIS-OBJECT:SmartTableIOState = TableIOStateEnum:FieldsEnabled .

        IF THIS-OBJECT:SaveMode = TRUE THEN
            THIS-OBJECT:EnableFields (Consultingwerk.SmartComponents.Enum.EnableFieldsEnum:Update) .

        OnAfterSaveChanges (System.EventArgs:Empty) .

        /* Mike Fechner, Consultingwerk Ltd. 12.08.2011
           When a save operation is completed, this Browser is no longer in Add mode
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

        CATCH err AS Progress.Lang.Error:
            THIS-OBJECT:HandleValidationError (err) .
        END CATCH.

        FINALLY:
            ASSIGN lSaving = FALSE .
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Saves pending changes when a Form is deactivated
        Notes:
        @param plShowMessage When TRUE a message is asking the user if values should be saved. When FALSE values will be saved silently
        @return True when the user did choose not to save or the save was successful, False when the control remains in Update state
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL SaveChangesOnDeactivateForm (plShowMessage AS LOGICAL):

        DEFINE VARIABLE oResult   AS DialogResultEnum NO-UNDO.

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

    /*------------------------------------------------------------------------------
        Purpose: Sets the TableIOState to ModifyingData
        Notes:   Useful when modifying Grid cell values through event handlers
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SetModifyingState():

        IF THIS-OBJECT:SmartTableIOState = TableIOStateEnum:FieldsEnabled THEN DO:

            THIS-OBJECT:SmartTableIOState = TableIOStateEnum:ModifyingData .

            /* Tell the SmartDataSource that the user has started to update data. */
            IF VALID-OBJECT(THIS-OBJECT:SmartDataSource) THEN
                THIS-OBJECT:SmartDataSource:BeginUpdateState (THIS-OBJECT).
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Sets the Openend Drop Down Editor Control
        Notes:   See Bug 1878 for details
        @param poControl The reference to the currently opened EditorDropDownControl
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SetOpenedEditorDropDownControl (poControl AS Control):

        oEditorDropDownControl = poControl .

    END METHOD.

    /*------------------------------------------------------------------------------
       Purpose: Store the Current Values of all Controls on a SmartViewerControl
                and do the same in all SmartGroupAssignTargets.
       Notes:   This Method is called by CreateRecord from the current Class.
                Enforced by Interface ISmartGroupAssignTarget.
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID StoreValuesForCopy ():

        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        /* Mike Fechner, Consultingwerk Ltd. 20.11.2011
           Bug 2736: Compiler crashes while trying to compile SmartUpdatableBrowser
           The usage of the ASSIGN statement here does cause the ABL compiler to
           crash prowin32.exe in combination with annotating the string with :U */
        /*ASSIGN*/ oStoredValues = NEW "System.Collections.Generic.Dictionary<System.String, System.Object>":U () .

        DO i = 0 TO THIS-OBJECT:DisplayLayout:Bands[0]:Columns:Count - 1:
            oStoredValues:ADD (THIS-OBJECT:DisplayLayout:Bands[0]:Columns[i]:Key,
                               THIS-OBJECT:ActiveRow:Cells[THIS-OBJECT:DisplayLayout:Bands[0]:Columns[i]:Key]:Value) .
        END.

        FOR EACH ttSmartGroupAssignTarget WHERE ttSmartGroupAssignTarget.RecordOwner = THIS-OBJECT:
            CAST(ttSmartGroupAssignTarget.SmartGroupAssignTarget, ISmartGroupAssignTarget):StoreValuesForCopy () .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Initiate switching to the ModifyinData Mode if the user enters data.
        Notes:   This Method is executed every time data changes in the EditorControls
                 of a SmartViewerControl.
                 Event handler for the TextChangedEvent event of Editor Controls on
                 the SmartViewerControl.
                 Enforced by Interface ISmartGroupAssignSource.
        @param sender The reference to the object that raised this event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID TextChangedEventHandler (sender AS System.Object,
                                                e AS System.EventArgs):

        IF THIS-OBJECT:SmartTableIOState <> TableIOStateEnum:FieldsEnabled THEN
            RETURN .

        IF VALID-OBJECT(sender) AND
            TYPE-OF(sender, System.Windows.Forms.Control) AND
            CAST(sender, System.Windows.Forms.Control):Enabled = FALSE THEN RETURN .

        /* Only switch into ModifyingData Mode if the focused control itself initiated
           the change due to a manual change via keyboard.
           03.06.2009: Alternative is that the current control has an EditorDropDown
                       dialogue opened (like the UltraDateTimeEditor) */
        IF CAST(sender, System.Windows.Forms.Control):Focused OR
        CAST(sender, System.Windows.Forms.Control) = oEditorDropDownControl THEN
        DO:
            THIS-OBJECT:SmartTableIOState = TableIOStateEnum:ModifyingData .

            /* Tell the SmartDataSource that the user has started to update data. */
            IF VALID-OBJECT(THIS-OBJECT:SmartDataSource) THEN
                THIS-OBJECT:SmartDataSource:BeginUpdateState (THIS-OBJECT).
        END.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SmartUpdatableBrowser class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartUpdatableBrowser ():

        FOR EACH ttSmartGroupAssignTarget WHERE ttSmartGroupAssignTarget.RecordOwner = THIS-OBJECT:
            DELETE ttSmartGroupAssignTarget.
        END.

        IF VALID-OBJECT (oStoredValues) THEN
            DELETE OBJECT oStoredValues .

    END DESTRUCTOR.

END CLASS.
