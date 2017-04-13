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
    File        : MultilpleEntriesPickerEditor
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Nov 12 17:45:13 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Progress.Util.* FROM ASSEMBLY.
USING System.Windows.Forms.* FROM ASSEMBLY.
USING Consultingwerk.Util.*               FROM PROPATH .
USING Consultingwerk.Windows.Controls.*   FROM PROPATH .
USING Infragistics.Win.UltraWinEditors.*  FROM ASSEMBLY .
USING Infragistics.Win.UltraWinListView.* FROM ASSEMBLY.
USING Progress.Lang.*                     FROM PROPATH .

CLASS Consultingwerk.Windows.Controls.MultilpleEntriesPickerEditor
    INHERITS UltraTextEditor:

    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraListView1 AS Infragistics.Win.UltraWinListView.UltraListView NO-UNDO.

    DEFINE VARIABLE lFirstTime   AS LOGICAL NO-UNDO INIT TRUE .
    DEFINE VARIABLE lSettingList AS LOGICAL NO-UNDO INIT FALSE .

    /**
     * Purpose: Gets and sets the delimiter used
     * Notes:
     */
    DEFINE PUBLIC PROPERTY Delimiter AS CHARACTER NO-UNDO INIT ",":U
    GET.
    SET.

    /**
     * Purpose: Propery flags if the current instance of the SmartBindingSource
     *           is running at design time or runtime.
     *  Notes:   Set during constructor using a check at the
     *           System.ComponentModel.LicenseManager
     */
    DEFINE PUBLIC PROPERTY DesignTime AS LOGICAL NO-UNDO
    GET.
    PRIVATE SET.

    /**
     * Purpose: Gets and sets the list of possible values for the Control
     * Notes:
     */
    DEFINE PUBLIC PROPERTY ValueList AS CHARACTER NO-UNDO INIT "":U
    GET.
    SET.

    /**
     * Purpose: Constructor for the MultilpleEntriesPickerEditor class
     * Notes:
     */
    CONSTRUCTOR PUBLIC MultilpleEntriesPickerEditor ():

        SUPER().

        THIS-OBJECT:DesignTime = Progress.Util.EnumHelper:AreEqual(System.ComponentModel.LicenseManager:UsageMode,
                                                                   System.ComponentModel.LicenseUsageMode:Designtime) .

        InitializeComponent().

        IF NOT THIS-OBJECT:DesignTime THEN
            THIS-OBJECT:InitializeEditorButton () .

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

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
        THIS-OBJECT:ultraListView1 = NEW Infragistics.Win.UltraWinListView.UltraListView().
        CAST(THIS-OBJECT:ultraListView1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraListView1 */
        /*  */
        THIS-OBJECT:ultraListView1:ItemSettings:SelectionType = Infragistics.Win.UltraWinListView.SelectionType:Single.
        THIS-OBJECT:ultraListView1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:ultraListView1:Name = "ultraListView1":U.
        THIS-OBJECT:ultraListView1:Size = NEW System.Drawing.Size(121, 150).
        THIS-OBJECT:ultraListView1:TabIndex = 0.
        THIS-OBJECT:ultraListView1:Text = "ultraListView1":U.
        THIS-OBJECT:ultraListView1:View = Infragistics.Win.UltraWinListView.UltraListViewStyle:List.
        THIS-OBJECT:ultraListView1:ViewSettingsList:CheckBoxStyle = Infragistics.Win.UltraWinListView.CheckBoxStyle:CheckBox.
        THIS-OBJECT:ultraListView1:ViewSettingsList:ImageSize = NEW System.Drawing.Size(0, 0).
        THIS-OBJECT:ultraListView1:ViewSettingsList:MultiColumn = FALSE.
        THIS-OBJECT:ultraListView1:Visible = FALSE.
        THIS-OBJECT:ultraListView1:ItemCheckStateChanged:Subscribe(THIS-OBJECT:ultraListView1_ItemCheckStateChanged).
        /*  */
        /* MultilpleEntriesPickerEditor */
        /*  */
        THIS-OBJECT:Size = NEW System.Drawing.Size(100, 21).
        THIS-OBJECT:EditorButtonClick:Subscribe(THIS-OBJECT:MultilpleEntriesPickerEditor_EditorButtonClick).
        CAST(THIS-OBJECT:ultraListView1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD .

    /**
     * Purpose: Initializes the Drop Down Editor Button
     * Notes:
     */
    METHOD PROTECTED VOID InitializeEditorButton():

        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE dropDownEditorButton1 AS Infragistics.Win.UltraWinEditors.DropDownEditorButton NO-UNDO.
        dropDownEditorButton1 = NEW Infragistics.Win.UltraWinEditors.DropDownEditorButton().

        dropDownEditorButton1:ButtonStyle = Infragistics.Win.UIElementButtonStyle:Popup.
        dropDownEditorButton1:Control = THIS-OBJECT:ultraListView1.
        dropDownEditorButton1:DropDownResizeHandleStyle = Infragistics.Win.DropDownResizeHandleStyle:VerticalResize.
        THIS-OBJECT:ButtonsRight:Add(dropDownEditorButton1).

    END METHOD.

    /**
     * Purpose: Event handler for the EditorButtonClick  event of the
     *          MultilpleEntriesPickerEditor
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The EditorButtonEventArgs with the data for this event
     */
    @VisualDesigner.
    METHOD PRIVATE VOID MultilpleEntriesPickerEditor_EditorButtonClick (sender AS System.Object,
                                                                        e AS Infragistics.Win.UltraWinEditors.EditorButtonEventArgs):

        DEFINE VARIABLE i     AS INTEGER           NO-UNDO .
        DEFINE VARIABLE oItem AS UltraListViewItem NO-UNDO .
        DEFINE VARIABLE cKey  AS CHARACTER         NO-UNDO .

        ASSIGN lSettingList = TRUE .

        ultraListView1:Items:Clear () .

        DO i = 1 TO NUM-ENTRIES (THIS-OBJECT:ValueList, THIS-OBJECT:Delimiter):

            ASSIGN cKey = ENTRY (i, THIS-OBJECT:ValueList, THIS-OBJECT:Delimiter) .

            oItem = THIS-OBJECT:ultraListView1:Items:Add (cKey) .
            oItem:Value = BOX (cKey) .

            IF ListHelper:EntryIsInList(cKey, THIS-OBJECT:Text, THIS-OBJECT:Delimiter) THEN
                oItem:CheckState = CheckState:Checked .
            ELSE
                oItem:CheckState = CheckState:Unchecked .
        END.

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage(err) .
        END CATCH.

        FINALLY:
            ASSIGN lSettingList = FALSE .
        END FINALLY.

    END METHOD .

    /**
     * Purpose: Raises the VisibleChanged event
     * Notes:
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PROTECTED OVERRIDE VOID OnVisibleChanged (e AS System.EventArgs):

        SUPER:OnVisibleChanged (e) .

        IF lFirstTime THEN DO:
            ASSIGN lFirstTime = FALSE .

            ultraListView1:Width = THIS-OBJECT:Width .
        END.

    END METHOD.

    /**
     * Purpose: Event handler for the ItemCheckStateChanged event of the ultraListView1
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The ItemCheckStateChangedEventArgs with the data for this event
     */
    @VisualDesigner.
    METHOD PRIVATE VOID ultraListView1_ItemCheckStateChanged (sender AS System.Object,
                                                              e AS Infragistics.Win.UltraWinListView.ItemCheckStateChangedEventArgs):

        IF lSettingList = TRUE THEN
            RETURN .

        IF EnumHelper:AreEqual (e:Item:CheckState, CheckState:Checked) THEN DO:

            IF NOT ListHelper:EntryIsInList (e:Item:Key, THIS-OBJECT:Text, THIS-OBJECT:Delimiter) THEN
                THIS-OBJECT:Text = ListHelper:AddEntry (THIS-OBJECT:Text, e:Item:Key, THIS-OBJECT:Delimiter) .
        END.
        ELSE
            THIS-OBJECT:Text = ListHelper:Remove (THIS-OBJECT:Text, e:Item:Key) .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage(err) .
        END CATCH.

    END METHOD .

    /**
     * Purpose: Destructor for the MultilpleEntriesPickerEditor class
     * Notes:
     */
    DESTRUCTOR PUBLIC MultilpleEntriesPickerEditor ():

    END DESTRUCTOR .

END CLASS.
