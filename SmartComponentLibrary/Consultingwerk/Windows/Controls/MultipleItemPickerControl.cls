/**********************************************************************
 * Copyright (C) 2006-2012 by Consultingwerk Ltd. ("CW") -            *
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
    File        : MultipleItemPickerControl
    Purpose     : Control that allows selecting multiple items from a list
                  of items
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Aug 22 14:35:01 CEST 2014
    Notes       : SCL-417
                  This Control uses a TableLayoutPanel for layout purposes.
                  As this Control does not require to be localizable we
                  do not expect to see the issues described in SCL-205
                  here
  ----------------------------------------------------------------------*/


ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.* FROM PROPATH.
USING Consultingwerk.Assertion.* FROM PROPATH.
USING Consultingwerk.Framework.Collections.* FROM PROPATH.
USING Consultingwerk.Util.* FROM PROPATH.
USING Progress.Windows.* FROM ASSEMBLY.
USING Infragistics.Win.UltraWinListView.* FROM ASSEMBLY.

CLASS Consultingwerk.Windows.Controls.MultipleItemPickerControl INHERITS UserControl USE-WIDGET-POOL:

    DEFINE PRIVATE VARIABLE btnAdd AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE btnRemove AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE lvSelected AS Infragistics.Win.UltraWinListView.UltraListView NO-UNDO.
    DEFINE PRIVATE VARIABLE lvAvailable AS Infragistics.Win.UltraWinListView.UltraListView NO-UNDO.
    DEFINE PRIVATE VARIABLE lblSelected AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE lblAvailable AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE tableLayoutPanel1 AS System.Windows.Forms.TableLayoutPanel NO-UNDO.

    /**
     * Purpose: Raised whenever the List of selected items changes
     * Notes:
     */
    DEFINE PUBLIC EVENT SelectedItemsChanged DELEGATE System.EventHandler .

    /**
     * Purpose: Gets and sets the names of the fields that shall be used for the
     *          Display text of the items
     * Notes:   Comma delimited list
     */
    DEFINE PUBLIC PROPERTY DisplayFields AS CHARACTER NO-UNDO INIT "":U
    GET.
    SET.

    /**
     * Purpose: Gets and sets the pattern to be used (SUBSTITUTE function) to build
     *          the Display text of the items from the values of the fields in the
     *          DisplayFields property
     * Notes:
     */
    DEFINE PUBLIC PROPERTY DisplayFieldSubstitute AS CHARACTER NO-UNDO INIT "&1":U
    GET.
    SET.

    /**
     * Purpose: Gets and sets the name of the field that returns the Image Key
     * Notes:
     */
    DEFINE PUBLIC PROPERTY ImageKeyField AS CHARACTER NO-UNDO INIT "":U
    GET.
    SET.

    /**
     * Purpose: Gets and sets the name of the field that returns the Key Value
     * Notes:
     */
    DEFINE PUBLIC PROPERTY KeyField AS CHARACTER NO-UNDO INIT "":U
    GET.
    SET.

    /**
     * Purpose: Gets and sets the label for the available items
     * Notes:
     */
    DEFINE PUBLIC PROPERTY LabelAvailableItems AS CHARACTER NO-UNDO INIT "Available Items:":U
    GET:
        RETURN lblAvailable:Text .
    END GET.
    SET (arg AS CHARACTER):
        lblAvailable:Text = arg .
    END SET.

    /**
     * Purpose: Gets and sets the label for the selected items
     * Notes:
     */
    DEFINE PUBLIC PROPERTY LabelSelectedItems AS CHARACTER NO-UNDO INIT "Selected Items:":U
    GET:
        RETURN lblSelected:Text .
    END GET.
    SET (arg AS CHARACTER):
        lblSelected:Text = arg .
    END SET.

    /**
     * Purpose: Constructor for the MultipleItemPickerControl class
     * Notes:
     */
    CONSTRUCTOR PUBLIC MultipleItemPickerControl ():

        SUPER().
        InitializeComponent().

        lvAvailable:MainColumn:Sorting = Sorting:Ascending .
        lvSelected:MainColumn:Sorting = Sorting:Ascending .

        &IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN
        THIS-OBJECT:ComponentsCollection:Add (THIS-OBJECT:components).
        &ENDIF

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

    /**
     * Purpose: Adds selected items
     * Notes:
     */
    METHOD PROTECTED VOID AddSelectedItems ():

        DEFINE VARIABLE oItems AS "UltraListViewItem[]" NO-UNDO .

        oItems = {Consultingwerk/new-array.i Infragistics.Win.UltraWinListView.UltraListViewItem lvAvailable:SelectedItems:Count} .

        lvAvailable:SelectedItems:CopyTo (oItems, 0) .

        {Consultingwerk/foreach.i UltraListViewItem oItem in oItems}
            lvAvailable:Items:Remove (oItem) .
            lvSelected:Items:Add (oItem) .
        END.

        OnSelectedItemsChanged (System.EventArgs:Empty) .

    END METHOD.

    /**
     * Purpose: Event handler for the Click event of the btnAdd
     * Notes:
     * @param sender The reference to the Control that raised the event
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PRIVATE VOID btnAdd_Click (sender AS System.Object,
                                      e AS System.EventArgs):

        THIS-OBJECT:AddSelectedItems () .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage(err) .
        END CATCH.

    END METHOD.

    /**
     * Purpose: Event handler for the Click event of the btnRemove
     * Notes:
     * @param sender The reference to the Control that raised the event
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PRIVATE VOID btnRemove_Click (sender AS System.Object,
                                         e AS System.EventArgs):

        THIS-OBJECT:RemoveSelectedItems () .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage(err) .
        END CATCH.

    END METHOD.

    /**
     * Purpose: Returns the List of selected Items
     * Notes:   Uses a CHR(1) delimited CharacterList
     * @return The CharacterList with the seelcted items
     */
    METHOD PUBLIC CharacterList GetSelectedItems ():

        DEFINE VARIABLE oList AS CharacterList NO-UNDO.

        ASSIGN oList = NEW CharacterList ("":U, CHR (1)) .

        {Consultingwerk/foreach.i UltraListViewItem oItem in lvSelected:Items}
            oList:Add (oItem:Key) .
        END.

        RETURN oList.

    END METHOD.

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
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Controls.MultipleItemPickerControl":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        THIS-OBJECT:tableLayoutPanel1 = NEW System.Windows.Forms.TableLayoutPanel().
        THIS-OBJECT:lblAvailable = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:lblSelected = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:lvAvailable = NEW Infragistics.Win.UltraWinListView.UltraListView().
        THIS-OBJECT:lvSelected = NEW Infragistics.Win.UltraWinListView.UltraListView().
        THIS-OBJECT:btnAdd = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:btnRemove = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:tableLayoutPanel1:SuspendLayout().
        CAST(THIS-OBJECT:lvAvailable, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:lvSelected, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* tableLayoutPanel1 */
        /*  */
        THIS-OBJECT:tableLayoutPanel1:ColumnCount = 3.
        THIS-OBJECT:tableLayoutPanel1:ColumnStyles:Add(NEW System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType:Percent, Progress.Util.CastUtil:ToSingle(50))).
        THIS-OBJECT:tableLayoutPanel1:ColumnStyles:Add(NEW System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType:Absolute, Progress.Util.CastUtil:ToSingle(35))).
        THIS-OBJECT:tableLayoutPanel1:ColumnStyles:Add(NEW System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType:Percent, Progress.Util.CastUtil:ToSingle(50))).
        THIS-OBJECT:tableLayoutPanel1:Controls:Add(THIS-OBJECT:lblAvailable, 0, 0).
        THIS-OBJECT:tableLayoutPanel1:Controls:Add(THIS-OBJECT:lblSelected, 2, 0).
        THIS-OBJECT:tableLayoutPanel1:Controls:Add(THIS-OBJECT:lvAvailable, 0, 1).
        THIS-OBJECT:tableLayoutPanel1:Controls:Add(THIS-OBJECT:lvSelected, 2, 1).
        THIS-OBJECT:tableLayoutPanel1:Controls:Add(THIS-OBJECT:btnAdd, 1, 2).
        THIS-OBJECT:tableLayoutPanel1:Controls:Add(THIS-OBJECT:btnRemove, 1, 3).
        THIS-OBJECT:tableLayoutPanel1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:tableLayoutPanel1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:tableLayoutPanel1:Name = "tableLayoutPanel1":U.
        THIS-OBJECT:tableLayoutPanel1:RowCount = 5.
        THIS-OBJECT:tableLayoutPanel1:RowStyles:Add(NEW System.Windows.Forms.RowStyle()).
        THIS-OBJECT:tableLayoutPanel1:RowStyles:Add(NEW System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType:Percent, Progress.Util.CastUtil:ToSingle(50))).
        THIS-OBJECT:tableLayoutPanel1:RowStyles:Add(NEW System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType:Absolute, Progress.Util.CastUtil:ToSingle(35))).
        THIS-OBJECT:tableLayoutPanel1:RowStyles:Add(NEW System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType:Absolute, Progress.Util.CastUtil:ToSingle(35))).
        THIS-OBJECT:tableLayoutPanel1:RowStyles:Add(NEW System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType:Percent, Progress.Util.CastUtil:ToSingle(50))).
        THIS-OBJECT:tableLayoutPanel1:Size = NEW System.Drawing.Size(540, 226).
        THIS-OBJECT:tableLayoutPanel1:TabIndex = 0.
        /*  */
        /* lblAvailable */
        /*  */
        THIS-OBJECT:lblAvailable:AutoSize = TRUE.
        THIS-OBJECT:lblAvailable:Location = NEW System.Drawing.Point(3, 3).
        THIS-OBJECT:lblAvailable:Name = "lblAvailable":U.
        THIS-OBJECT:lblAvailable:Size = NEW System.Drawing.Size(84, 14).
        THIS-OBJECT:lblAvailable:TabIndex = 0.
        THIS-OBJECT:lblAvailable:Text = "Available Items:":U.
        /*  */
        /* lblSelected */
        /*  */
        THIS-OBJECT:lblSelected:AutoSize = TRUE.
        THIS-OBJECT:lblSelected:Location = NEW System.Drawing.Point(290, 3).
        THIS-OBJECT:lblSelected:Name = "lblSelected":U.
        THIS-OBJECT:lblSelected:Size = NEW System.Drawing.Size(82, 14).
        THIS-OBJECT:lblSelected:TabIndex = 1.
        THIS-OBJECT:lblSelected:Text = "Selected Items:":U.
        /*  */
        /* lvAvailable */
        /*  */
        THIS-OBJECT:lvAvailable:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:lvAvailable:Location = NEW System.Drawing.Point(3, 23).
        THIS-OBJECT:lvAvailable:Name = "lvAvailable":U.
        THIS-OBJECT:tableLayoutPanel1:SetRowSpan(THIS-OBJECT:lvAvailable, 4).
        THIS-OBJECT:lvAvailable:Size = NEW System.Drawing.Size(246, 200).
        THIS-OBJECT:lvAvailable:TabIndex = 2.
        THIS-OBJECT:lvAvailable:Text = "ultraListView1":U.
        THIS-OBJECT:lvAvailable:View = Infragistics.Win.UltraWinListView.UltraListViewStyle:List.
        THIS-OBJECT:lvAvailable:ViewSettingsList:MultiColumn = FALSE.
        THIS-OBJECT:lvAvailable:ItemDoubleClick:Subscribe(THIS-OBJECT:lvAvailable_ItemDoubleClick).
        THIS-OBJECT:lvAvailable:ItemSelectionChanged:Subscribe(THIS-OBJECT:lvAvailable_ItemSelectionChanged).
        /*  */
        /* lvSelected */
        /*  */
        THIS-OBJECT:lvSelected:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:lvSelected:Location = NEW System.Drawing.Point(290, 23).
        THIS-OBJECT:lvSelected:Name = "lvSelected":U.
        THIS-OBJECT:tableLayoutPanel1:SetRowSpan(THIS-OBJECT:lvSelected, 4).
        THIS-OBJECT:lvSelected:Size = NEW System.Drawing.Size(247, 200).
        THIS-OBJECT:lvSelected:TabIndex = 3.
        THIS-OBJECT:lvSelected:Text = "ultraListView2":U.
        THIS-OBJECT:lvSelected:View = Infragistics.Win.UltraWinListView.UltraListViewStyle:List.
        THIS-OBJECT:lvSelected:ViewSettingsList:MultiColumn = FALSE.
        THIS-OBJECT:lvSelected:ItemDoubleClick:Subscribe(THIS-OBJECT:lvSelected_ItemDoubleClick).
        THIS-OBJECT:lvSelected:ItemSelectionChanged:Subscribe(THIS-OBJECT:lvSelected_ItemSelectionChanged).
        /*  */
        /* btnAdd */
        /*  */
        appearance1:Image = CAST(resources:GetObject("appearance1.Image":U), System.Object).
        appearance1:ImageHAlign = Infragistics.Win.HAlign:Center.
        appearance1:ImageVAlign = Infragistics.Win.VAlign:Middle.
        THIS-OBJECT:btnAdd:Appearance = appearance1.
        THIS-OBJECT:btnAdd:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:btnAdd:Enabled = FALSE.
        THIS-OBJECT:btnAdd:Location = NEW System.Drawing.Point(255, 91).
        THIS-OBJECT:btnAdd:Name = "btnAdd":U.
        THIS-OBJECT:btnAdd:Size = NEW System.Drawing.Size(29, 29).
        THIS-OBJECT:btnAdd:TabIndex = 4.
        THIS-OBJECT:btnAdd:Click:Subscribe(THIS-OBJECT:btnAdd_Click).
        /*  */
        /* btnRemove */
        /*  */
        appearance2:Image = CAST(resources:GetObject("appearance2.Image":U), System.Object).
        appearance2:ImageHAlign = Infragistics.Win.HAlign:Center.
        appearance2:ImageVAlign = Infragistics.Win.VAlign:Middle.
        THIS-OBJECT:btnRemove:Appearance = appearance2.
        THIS-OBJECT:btnRemove:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:btnRemove:Enabled = FALSE.
        THIS-OBJECT:btnRemove:Location = NEW System.Drawing.Point(255, 126).
        THIS-OBJECT:btnRemove:Name = "btnRemove":U.
        THIS-OBJECT:btnRemove:Size = NEW System.Drawing.Size(29, 29).
        THIS-OBJECT:btnRemove:TabIndex = 5.
        THIS-OBJECT:btnRemove:Click:Subscribe(THIS-OBJECT:btnRemove_Click).
        /*  */
        /* MultipleItemPickerControl */
        /*  */
        THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tableLayoutPanel1).
        THIS-OBJECT:Name = "MultipleItemPickerControl":U.
        THIS-OBJECT:Size = NEW System.Drawing.Size(540, 226).
        THIS-OBJECT:tableLayoutPanel1:ResumeLayout(FALSE).
        THIS-OBJECT:tableLayoutPanel1:PerformLayout().
        CAST(THIS-OBJECT:lvAvailable, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:lvSelected, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /**
     * Purpose: Event handler for the ItemDoubleClick event of the lvAvailable
     * Notes:
     * @param sender The reference to the Control that raised the event
     * @param e The ItemDoubleClickEventArgs with the data for this event
     */
    METHOD PRIVATE VOID lvAvailable_ItemDoubleClick (sender AS System.Object,
                                                     e AS ItemDoubleClickEventArgs ):

        THIS-OBJECT:AddSelectedItems () .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage(err) .
        END CATCH.

    END METHOD.

    /**
     * Purpose: Event handler for the ItemSelectionChanged event of the lvAvailable
     * Notes:
     * @param sender The reference to the Control that raised the event
     * @param e The ItemSelectionChangedEventArgs with the data for this event
     */
    METHOD PRIVATE VOID lvAvailable_ItemSelectionChanged (sender AS System.Object,
                                                          e AS ItemSelectionChangedEventArgs):

        ASSIGN btnAdd:Enabled = lvAvailable:SelectedItems:Count > 0 .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage(err) .
        END CATCH.

    END METHOD.

    /**
     * Purpose: Event handler for the ItemDoubleClick event of the lvSelected
     * Notes:
     * @param sender The reference to the Control that raised the event
     * @param e The ItemDoubleClickEventArgs with the data for this event
     */
    METHOD PRIVATE VOID lvSelected_ItemDoubleClick (sender AS System.Object,
                                                    e AS ItemDoubleClickEventArgs):

        THIS-OBJECT:RemoveSelectedItems() .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage(err) .
        END CATCH.

    END METHOD.

    /**
     * Purpose: Event handler for the ItemSelectionChanged event of the lvSelected
     * Notes:
     * @param sender The reference to the Control that raised the event
     * @param e The ItemSelectionChangedEventArgs with the data for this event
     */
    METHOD PRIVATE VOID lvSelected_ItemSelectionChanged (sender AS System.Object,
                                                         e AS ItemSelectionChangedEventArgs):

        ASSIGN btnRemove:Enabled = lvSelected:SelectedItems:Count > 0 .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage(err) .
        END CATCH.

    END METHOD.

    /**
     * Purpose: Raises the SelectedItemsChanged
     * Notes:
     * @param e The System.EventArgs with the data for the event
     */
    METHOD PROTECTED VOID OnSelectedItemsChanged (e AS System.EventArgs):

        IF NOT VALID-OBJECT (e) THEN
            e = System.EventArgs:Empty .

        THIS-OBJECT:SelectedItemsChanged:Publish (THIS-OBJECT, e) .

    END METHOD .

    /**
     * Purpose: Removes selected items
     * Notes:
     */
    METHOD PROTECTED VOID RemoveSelectedItems ():

        DEFINE VARIABLE oItems AS "UltraListViewItem[]" NO-UNDO .

        oItems = {Consultingwerk/new-array.i Infragistics.Win.UltraWinListView.UltraListViewItem lvSelected:SelectedItems:Count} .

        lvSelected:SelectedItems:CopyTo (oItems, 0) .

        {Consultingwerk/foreach.i UltraListViewItem oItem in oItems}
            lvSelected:Items:Remove (oItem) .
            lvAvailable:Items:Add (oItem) .
        END.

        OnSelectedItemsChanged (System.EventArgs:Empty) .

    END METHOD.

    /**
     * Purpose: Initializes the Selection lists based on the given temp-table and
     *          CharcaterList of selected values
     * Notes:
     * @param phTempTable The handle (NOT TABLE-HANDLE) of the temp-table containing all values
     * @param poSelectedItems The CharacterList of selected items
     */
    METHOD PUBLIC VOID SetListItems (phTempTable AS HANDLE,
                                     poSelectedItems AS CharacterList):

        DEFINE VARIABLE hBuffer AS HANDLE            NO-UNDO .
        DEFINE VARIABLE hQuery  AS HANDLE            NO-UNDO .
        DEFINE VARIABLE oItem   AS UltraListViewItem NO-UNDO .

        DEFINE VARIABLE oImageProvider AS Consultingwerk.Windows.Framework.IImageProvider NO-UNDO .

        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phTempTable WidgetTypeEnum:TempTable} .
        ObjectAssert:IsValid (poSelectedItems, "poSelectedItems":U) .

        lvAvailable:Items:Clear () .
        lvSelected:Items:Clear () .

        CREATE BUFFER hBuffer FOR TABLE phTempTable .

        BufferAssert:HasField (hBuffer, THIS-OBJECT:KeyField) .

        IF THIS-OBJECT:ImageKeyField > "":U THEN DO:
            BufferAssert:HasField (hBuffer, THIS-OBJECT:ImageKeyField) .

            ASSIGN oImageProvider = {Consultingwerk/get-service.i Consultingwerk.Windows.Framework.IImageProvider
                                                                  "NEW Consultingwerk.Windows.Framework.ImageProvider ()"  } .

        END.

        hQuery = QueryHelper:CreatePreparedQuery (hBuffer) .

        DO WHILE NOT hQuery:QUERY-OFF-END ON ERROR UNDO, THROW:

            oItem = NEW UltraListViewItem (BufferHelper:BufferFieldSubstitute (hBuffer,
                                                                               THIS-OBJECT:DisplayFields,
                                                                               THIS-OBJECT:DisplayFieldSubstitute), ?, ?) .
            oItem:Key = hBuffer:BUFFER-FIELD (THIS-OBJECT:KeyField):BUFFER-VALUE .

            IF THIS-OBJECT:ImageKeyField > "":U THEN
                oItem:Appearance:Image = oImageProvider:ImageForKey (hBuffer:BUFFER-FIELD (THIS-OBJECT:ImageKeyField):BUFFER-VALUE) .

            IF NOT poSelectedItems:ContainsValue (oItem:Key) THEN
                lvAvailable:Items:Add (oItem) .
            ELSE
                lvSelected:Items:Add (oItem) .

            FINALLY:
                hQuery:GET-NEXT () .
            END FINALLY.
        END.

        FINALLY:
            GarbageCollectorHelper:DeleteObject (hQuery) .
            GarbageCollectorHelper:DeleteObject (hBuffer) .
        END FINALLY.

    END METHOD.

END CLASS.
