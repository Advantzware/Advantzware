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
    File        : IndexFieldsForm
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike
    Created     : Mon Oct 26 20:44:28 CET 2015
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Forms.*                           FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Services.* FROM PROPATH.
USING Consultingwerk.BusinessEntityDesigner.UI.*       FROM PROPATH .
USING Consultingwerk.Util.*                            FROM PROPATH .
USING Consultingwerk.Windows.Controls.*                FROM PROPATH .
USING Progress.Lang.*                                  FROM PROPATH .
USING Infragistics.Win.UltraWinListView.* FROM ASSEMBLY.
USING System.Runtime.InteropServices.* FROM ASSEMBLY.
USING System.Windows.Forms.* FROM ASSEMBLY.

CLASS Consultingwerk.BusinessEntityDesigner.UI.IndexFieldsForm
    INHERITS BaseForm:

    { Consultingwerk/BusinessEntityDesigner/Services/dsBusinessEntity.i &REFERENCE-ONLY=REFERENCE-ONLY}

    DEFINE PRIVATE VARIABLE btnRemove AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE btnAdd AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE lvIndexFields AS Infragistics.Win.UltraWinListView.UltraListView NO-UNDO.
    DEFINE PRIVATE VARIABLE lvBusinessEntityFields AS Infragistics.Win.UltraWinListView.UltraListView NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraButton2 AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraButton1 AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE panel1 AS System.Windows.Forms.Panel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraOptionSet1 AS Infragistics.Win.UltraWinEditors.UltraOptionSet NO-UNDO.

    DEFINE VARIABLE oUltraListViewOrderController AS UltraListViewOrderController NO-UNDO .
    DEFINE VARIABLE oUltraListViewLeftRightController AS UltraListViewLeftRightController NO-UNDO .

    /*------------------------------------------------------------------------------
        Purpose: Returns the Index Fields
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY IndexFields AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the Index Sort
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY IndexSort AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the IndexFieldsForm class
        Notes:
        @param pcBusinessEntityName The name of the business entity
        @param pcTempTableName The name of the temp-table
        @param pcIndexName The name of the index
        @param pcFieldNames The comma delimited list of index field
        @param pcFieldSort The current sort order of the index fields
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC IndexFieldsForm (pcBusinessEntityName AS CHARACTER,
                                        pcTempTableName AS CHARACTER,
                                        pcIndexName AS CHARACTER,
                                        pcFieldNames AS CHARACTER,
                                        pcFieldSort AS CHARACTER):

        DEFINE VARIABLE oImage AS System.Drawing.Image NO-UNDO .
        DEFINE VARIABLE hDataset AS HANDLE NO-UNDO.
        DEFINE VARIABLE oController AS BusinessEntityDatasetController NO-UNDO .

        SUPER().

        InitializeComponent().

        ASSIGN oImage                                               = ImageHelper:Load ("Consultingwerk/BusinessEntityDesigner/UI/Images/table2_selection_column.png":U)
               lvBusinessEntityFields:ItemSettings:Appearance:Image = oImage
               lvIndexFields:ItemSettings:Appearance:Image          = oImage
                .

        oController = {Consultingwerk/get-service.i Consultingwerk.BusinessEntityDesigner.Services.BusinessEntityDatasetController} .

        ASSIGN hDataset = oController:DatasetHandle .

        THIS-OBJECT:BindDataset (DATASET-HANDLE hDataset BIND) .

        THIS-OBJECT:PopulateListViews (pcBusinessEntityName,
                                       pcTempTableName,
                                       pcIndexName,
                                       pcFieldNames,
                                       pcFieldSort) .


        oUltraListViewOrderController = NEW UltraListViewOrderController (lvBusinessEntityFields) .

        oUltraListViewLeftRightController = NEW UltraListViewLeftRightController (lvBusinessEntityFields,
                                                                                  lvIndexFields,
                                                                                  btnAdd,
                                                                                  btnRemove)  .


    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Binds the Dataset Handle
        Notes:
        @param dsBusinessEntity The dataset to bind to
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID BindDataset (DATASET dsBusinessEntity BIND):

        /* noop */

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Client event of the Add button
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID btnAdd_Click (INPUT sender AS System.Object, INPUT e AS System.EventArgs):

        {Consultingwerk/foreach.i Infragistics.Win.UltraWinListView.UltraListViewItem oItem in lvBusinessEntityFields:SelectedItems}
            oItem:Value = SUBSTITUTE ("&1 (ASCENDING)":U, oItem:Key) .
        END.

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Client event of the Remove button
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID btnRemove_Click (sender AS System.Object,
                                         e AS System.EventArgs):

        {Consultingwerk/foreach.i Infragistics.Win.UltraWinListView.UltraListViewItem oItem in lvIndexFields:SelectedItems}
            oItem:Value = oItem:Key .
        END.

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

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
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem1 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem1 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem2 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem2 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.BusinessEntityDesigner.UI.IndexFieldsForm":U).
        THIS-OBJECT:btnRemove = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:btnAdd = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:lvIndexFields = NEW Infragistics.Win.UltraWinListView.UltraListView().
        THIS-OBJECT:lvBusinessEntityFields = NEW Infragistics.Win.UltraWinListView.UltraListView().
        THIS-OBJECT:ultraOptionSet1 = NEW Infragistics.Win.UltraWinEditors.UltraOptionSet().
        THIS-OBJECT:ultraButton1 = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:ultraButton2 = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:panel1 = NEW System.Windows.Forms.Panel().
        CAST(THIS-OBJECT:lvIndexFields, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:lvBusinessEntityFields, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraOptionSet1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:panel1:SuspendLayout().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* btnRemove */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = System.Windows.Forms.AnchorStyles:Top.
        THIS-OBJECT:btnRemove:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btnRemove:Location = NEW System.Drawing.Point(273, 131).
        THIS-OBJECT:btnRemove:Name = "btnRemove":U.
        THIS-OBJECT:btnRemove:Size = NEW System.Drawing.Size(58, 23).
        THIS-OBJECT:btnRemove:TabIndex = 10.
        THIS-OBJECT:btnRemove:Text = "<<":U.
        THIS-OBJECT:btnRemove:Click:Subscribe(THIS-OBJECT:btnRemove_Click).
        /*  */
        /* btnAdd */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = System.Windows.Forms.AnchorStyles:Top.
        THIS-OBJECT:btnAdd:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btnAdd:Location = NEW System.Drawing.Point(273, 102).
        THIS-OBJECT:btnAdd:Name = "btnAdd":U.
        THIS-OBJECT:btnAdd:Size = NEW System.Drawing.Size(58, 23).
        THIS-OBJECT:btnAdd:TabIndex = 9.
        THIS-OBJECT:btnAdd:Text = ">>":U.
        THIS-OBJECT:btnAdd:Click:Subscribe(THIS-OBJECT:btnAdd_Click).
        /*  */
        /* lvIndexFields */
        /*  */
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:lvIndexFields:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:lvIndexFields:Location = NEW System.Drawing.Point(337, 38).
        THIS-OBJECT:lvIndexFields:Name = "lvIndexFields":U.
        THIS-OBJECT:lvIndexFields:Size = NEW System.Drawing.Size(250, 295).
        THIS-OBJECT:lvIndexFields:TabIndex = 8.
        THIS-OBJECT:lvIndexFields:Text = "ultraListView2":U.
        THIS-OBJECT:lvIndexFields:View = Infragistics.Win.UltraWinListView.UltraListViewStyle:List.
        THIS-OBJECT:lvIndexFields:ViewSettingsList:MultiColumn = FALSE.
        THIS-OBJECT:lvIndexFields:ItemSelectionChanged:Subscribe(THIS-OBJECT:lvIndexFields_ItemSelectionChanged).
        /*  */
        /* lvBusinessEntityFields */
        /*  */
        DEFINE VARIABLE nestedvar3 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar3 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:lvBusinessEntityFields:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar3, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:lvBusinessEntityFields:Location = NEW System.Drawing.Point(17, 38).
        THIS-OBJECT:lvBusinessEntityFields:Name = "lvBusinessEntityFields":U.
        THIS-OBJECT:lvBusinessEntityFields:Size = NEW System.Drawing.Size(250, 295).
        THIS-OBJECT:lvBusinessEntityFields:TabIndex = 7.
        THIS-OBJECT:lvBusinessEntityFields:Text = "ultraListView1":U.
        THIS-OBJECT:lvBusinessEntityFields:View = Infragistics.Win.UltraWinListView.UltraListViewStyle:List.
        THIS-OBJECT:lvBusinessEntityFields:ViewSettingsList:MultiColumn = FALSE.
        /*  */
        /* ultraOptionSet1 */
        /*  */
        DEFINE VARIABLE nestedvar4 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar4 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:ultraOptionSet1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar4, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraOptionSet1:BorderStyle = Infragistics.Win.UIElementBorderStyle:None.
        THIS-OBJECT:ultraOptionSet1:CausesValidation = FALSE.
        valueListItem1:DataValue = "ASCENDING":U.
        valueListItem1:DisplayText = "Ascending":U.
        valueListItem2:DataValue = "DESCENDING":U.
        valueListItem2:DisplayText = "Descending":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.ValueListItem EXTENT 2 NO-UNDO.
        arrayvar0[1] = valueListItem1.
        arrayvar0[2] = valueListItem2.
        THIS-OBJECT:ultraOptionSet1:Items:AddRange(arrayvar0).
        THIS-OBJECT:ultraOptionSet1:Location = NEW System.Drawing.Point(337, 339).
        THIS-OBJECT:ultraOptionSet1:Name = "ultraOptionSet1":U.
        THIS-OBJECT:ultraOptionSet1:Size = NEW System.Drawing.Size(250, 19).
        THIS-OBJECT:ultraOptionSet1:TabIndex = 11.
        THIS-OBJECT:ultraOptionSet1:ValueChanged:Subscribe(THIS-OBJECT:ultraOptionSet1_ValueChanged).
        /*  */
        /* ultraButton1 */
        /*  */
        DEFINE VARIABLE nestedvar5 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar5 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:ultraButton1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar5, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraButton1:DialogResult = System.Windows.Forms.DialogResult:OK.
        THIS-OBJECT:ultraButton1:Location = NEW System.Drawing.Point(425, 376).
        THIS-OBJECT:ultraButton1:Name = "ultraButton1":U.
        THIS-OBJECT:ultraButton1:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:ultraButton1:TabIndex = 12.
        THIS-OBJECT:ultraButton1:Text = "&OK":U.
        THIS-OBJECT:ultraButton1:Click:Subscribe(THIS-OBJECT:ultraButton1_Click).
        /*  */
        /* ultraButton2 */
        /*  */
        DEFINE VARIABLE nestedvar6 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar6 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:ultraButton2:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar6, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraButton2:DialogResult = System.Windows.Forms.DialogResult:Cancel.
        THIS-OBJECT:ultraButton2:Location = NEW System.Drawing.Point(506, 376).
        THIS-OBJECT:ultraButton2:Name = "ultraButton2":U.
        THIS-OBJECT:ultraButton2:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:ultraButton2:TabIndex = 13.
        THIS-OBJECT:ultraButton2:Text = "&Cancel":U.
        /*  */
        /* panel1 */
        /*  */
        DEFINE VARIABLE nestedvar7 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar7 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:panel1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar7, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:panel1:BackColor = System.Drawing.SystemColors:ControlLightLight.
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:ultraOptionSet1).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:btnRemove).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:lvBusinessEntityFields).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:btnAdd).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:lvIndexFields).
        THIS-OBJECT:panel1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:panel1:Name = "panel1":U.
        THIS-OBJECT:panel1:Size = NEW System.Drawing.Size(593, 370).
        THIS-OBJECT:panel1:TabIndex = 14.
        /*  */
        /* IndexFieldsForm */
        /*  */
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(593, 411).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraButton2).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:panel1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraButton1).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:Name = "IndexFieldsForm":U.
        THIS-OBJECT:Text = "Index Field Editor":U.
        THIS-OBJECT:UseStartupSizeAsMinSize = TRUE.
        THIS-OBJECT:WindowPositionRegistryKey = "IndexFieldsForm":U.
        CAST(THIS-OBJECT:lvIndexFields, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:lvBusinessEntityFields, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraOptionSet1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:panel1:ResumeLayout(FALSE).
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Evemt handler for the ItemSelectionChanged event of the lvIndexFields
        Notes:
        @param sender The System.Object with the data for this event
        @param e The ItemSelectionChangedEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID lvIndexFields_ItemSelectionChanged (sender AS System.Object,
                                                            e AS Infragistics.Win.UltraWinListView.ItemSelectionChangedEventArgs):

        IF lvIndexFields:SelectedItems:Count = 0 THEN DO:
            ultraOptionSet1:Value = ? .
            RETURN .
        END.

        IF UNBOX (lvIndexFields:SelectedItems[0]:Value) MATCHES ("*(ASCENDING)*":U) THEN DO:
            ultraOptionSet1:CheckedIndex = 0 .
            RETURN .
        END.

        IF UNBOX (lvIndexFields:SelectedItems[0]:Value) MATCHES ("*(DESCENDING)*":U) THEN DO:
            ultraOptionSet1:CheckedIndex = 1 .
            RETURN .
        END.

        ultraOptionSet1:Value = ? .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Populates the list views
        Notes:
        @param pcBusinessEntityName The name of the business entity
        @param pcTempTableName The name of the temp-table
        @param pcIndexName The name of the index
        @param pcFieldNames The comma delimited list of index field
        @param pcFieldSort The current sort order of the index fields
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID PopulateListViews (pcBusinessEntityName AS CHARACTER,
                                             pcTempTableName AS CHARACTER,
                                             pcIndexName AS CHARACTER,
                                             pcFieldNames AS CHARACTER,
                                             pcFieldSort AS CHARACTER):

        DEFINE VARIABLE i     AS INTEGER           NO-UNDO .

        FOR EACH eField WHERE eField.BusinessEntityName = pcBusinessEntityName
                          AND eField.TempTableName      = pcTempTableName
                          BY eField.FieldOrder ON ERROR UNDO, THROW:

            IF NOT ListHelper:EntryIsInList (eField.FieldName, pcFieldNames) THEN DO:
                lvBusinessEntityFields:Items:Add (eField.FieldName,
                                                  eField.FieldName) .
            END.
        END.

        DO i = 1 TO NUM-ENTRIES (pcFieldNames):
            lvIndexFields:Items:Add (ENTRY (i, pcFieldNames),
                                     SUBSTITUTE ("&1 (&2)":U,
                                                 ENTRY (i, pcFieldNames),
                                                 ENTRY (i, pcFieldSort))) .
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click evnet of the ultraButton1
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraButton1_Click (INPUT sender AS System.Object, INPUT e AS System.EventArgs):

        ASSIGN THIS-OBJECT:IndexFields = "":U
               THIS-OBJECT:IndexSort   = "":U .

        {Consultingwerk/foreach.i Infragistics.Win.UltraWinListView.UltraListViewItem oItem in lvIndexFields:Items}

            ASSIGN THIS-OBJECT:IndexFields = THIS-OBJECT:IndexFields + ",":U + oItem:Key
                   THIS-OBJECT:IndexSort   = THIS-OBJECT:IndexSort + ",":U +
                                             (IF UNBOX (oItem:Value) MATCHES "*(DESCENDING)*":U THEN "DESCENDING":U ELSE "ASCENDING":U) .
        END .

        ASSIGN THIS-OBJECT:IndexFields = TRIM (THIS-OBJECT:IndexFields, ",":U)
               THIS-OBJECT:IndexSort   = TRIM (THIS-OBJECT:IndexSort, ",":U) .

        GarbageCollectorHelper:DeleteObject (oUltraListViewOrderController) .
        GarbageCollectorHelper:DeleteObject (oUltraListViewLeftRightController) .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ValueChanged event of the ultraOptionSet1
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraOptionSet1_ValueChanged (sender AS System.Object,
                                                      e AS System.EventArgs):

        IF ultraOptionSet1:Value <> ? THEN DO:

            {Consultingwerk/foreach.i Infragistics.Win.UltraWinListView.UltraListViewItem oItem in lvIndexFields:SelectedItems}
                oItem:Value = SUBSTITUTE ("&1 (&2)":U, oItem:Key, UNBOX (ultraOptionSet1:CheckedItem:DataValue)) .
            END.
        END.

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the IndexFieldsForm class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC IndexFieldsForm ():

    END DESTRUCTOR .

END CLASS.
