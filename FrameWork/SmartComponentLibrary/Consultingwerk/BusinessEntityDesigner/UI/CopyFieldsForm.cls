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
    File        : CopyFieldsForm
    Purpose     : Allows to copy field names to the windows clipboard
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Jan 15 08:28:55 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW .

USING Consultingwerk.*           FROM PROPATH .
USING Consultingwerk.Assertion.* FROM PROPATH .
USING Consultingwerk.Forms.*     FROM PROPATH .
USING Consultingwerk.Util.*      FROM PROPATH .
USING Progress.Lang.*            FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.UI.CopyFieldsForm
    INHERITS BaseForm:

    DEFINE PRIVATE VARIABLE btnCopy AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE btnCancel AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraButton2 AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraButton1 AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE panel1 AS System.Windows.Forms.Panel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraButton3 AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraListView1 AS Infragistics.Win.UltraWinListView.UltraListView NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the handle of the eField buffer used to populate
                 the list view
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BufferHandle AS HANDLE NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the name of the selected temp-table
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY TableName AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the CopyFieldsForm class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC CopyFieldsForm ():

        SUPER().
        InitializeComponent().

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the btnCopy_Click
        Notes:   Copies selected fields to the clipboard
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID btnCopy_Click (sender AS System.Object,
                                       e AS System.EventArgs):

        DEFINE VARIABLE cCopyString AS CHARACTER NO-UNDO.

        {Consultingwerk/foreach.i Infragistics.Win.UltraWinListView.UltraListViewItem oItem in ultraListView1:Items}
            IF Progress.Util.EnumHelper:AreEqual (oItem:CheckState, System.Windows.Forms.CheckState:Checked) THEN
                cCopyString = cCopyString + SUBSTITUTE ("&1.&2~n":U,
                                                        THIS-OBJECT:TableName,
                                                        UNBOX (oItem:Value)).
        END.

        CLIPBOARD:VALUE = cCopyString .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

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
        DEFINE VARIABLE ultraListViewItem1 AS Infragistics.Win.UltraWinListView.UltraListViewItem NO-UNDO.
        ultraListViewItem1 = NEW Infragistics.Win.UltraWinListView.UltraListViewItem("test":U, ?, ?).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraListViewItem2 AS Infragistics.Win.UltraWinListView.UltraListViewItem NO-UNDO.
        ultraListViewItem2 = NEW Infragistics.Win.UltraWinListView.UltraListViewItem("test2":U, ?, ?).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraListViewItem3 AS Infragistics.Win.UltraWinListView.UltraListViewItem NO-UNDO.
        ultraListViewItem3 = NEW Infragistics.Win.UltraWinListView.UltraListViewItem("test4":U, ?, ?).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.BusinessEntityDesigner.UI.CopyFieldsForm":U).
        THIS-OBJECT:ultraListView1 = NEW Infragistics.Win.UltraWinListView.UltraListView().
        THIS-OBJECT:panel1 = NEW System.Windows.Forms.Panel().
        THIS-OBJECT:ultraButton3 = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:ultraButton2 = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:ultraButton1 = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:btnCopy = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:btnCancel = NEW Infragistics.Win.Misc.UltraButton().
        CAST(THIS-OBJECT:ultraListView1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:panel1:SuspendLayout().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraListView1 */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraListView1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        ultraListViewItem1:Key = "test":U.
        ultraListViewItem2:Key = "222":U.
        ultraListViewItem3:Key = "3333":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinListView.UltraListViewItem EXTENT 3 NO-UNDO.
        arrayvar0[1] = ultraListViewItem1.
        arrayvar0[2] = ultraListViewItem2.
        arrayvar0[3] = ultraListViewItem3.
        THIS-OBJECT:ultraListView1:Items:AddRange(arrayvar0).
        THIS-OBJECT:ultraListView1:ItemSettings:AllowEdit = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:ultraListView1:ItemSettings:DefaultImage = CAST(resources:GetObject("ultraListView1.ItemSettings.DefaultImage":U), System.Drawing.Image).
        THIS-OBJECT:ultraListView1:Location = NEW System.Drawing.Point(12, 41).
        THIS-OBJECT:ultraListView1:MainColumn:AllowSorting = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:ultraListView1:MainColumn:DataType = Progress.Util.TypeHelper:GetType("System.String":U).
        THIS-OBJECT:ultraListView1:MainColumn:ShowSortIndicators = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:ultraListView1:Name = "ultraListView1":U.
        THIS-OBJECT:ultraListView1:Size = NEW System.Drawing.Size(511, 294).
        THIS-OBJECT:ultraListView1:TabIndex = 0.
        THIS-OBJECT:ultraListView1:Text = "ultraListView1":U.
        THIS-OBJECT:ultraListView1:View = Infragistics.Win.UltraWinListView.UltraListViewStyle:List.
        THIS-OBJECT:ultraListView1:ViewSettingsList:CheckBoxStyle = Infragistics.Win.UltraWinListView.CheckBoxStyle:CheckBox.
        /*  */
        /* panel1 */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:panel1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:panel1:BackColor = System.Drawing.SystemColors:ControlLightLight.
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:ultraButton3).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:ultraButton2).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:ultraButton1).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:ultraListView1).
        THIS-OBJECT:panel1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:panel1:Name = "panel1":U.
        THIS-OBJECT:panel1:Size = NEW System.Drawing.Size(535, 348).
        THIS-OBJECT:panel1:TabIndex = 1.
        /*  */
        /* ultraButton3 */
        /*  */
        THIS-OBJECT:ultraButton3:Location = NEW System.Drawing.Point(174, 12).
        THIS-OBJECT:ultraButton3:Name = "ultraButton3":U.
        THIS-OBJECT:ultraButton3:Size = NEW System.Drawing.Size(117, 23).
        THIS-OBJECT:ultraButton3:TabIndex = 3.
        THIS-OBJECT:ultraButton3:Text = "Invert Selection":U.
        THIS-OBJECT:ultraButton3:Click:Subscribe(THIS-OBJECT:ultraButton3_Click).
        /*  */
        /* ultraButton2 */
        /*  */
        THIS-OBJECT:ultraButton2:Location = NEW System.Drawing.Point(93, 12).
        THIS-OBJECT:ultraButton2:Name = "ultraButton2":U.
        THIS-OBJECT:ultraButton2:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:ultraButton2:TabIndex = 2.
        THIS-OBJECT:ultraButton2:Text = "None":U.
        THIS-OBJECT:ultraButton2:Click:Subscribe(THIS-OBJECT:ultraButton2_Click).
        /*  */
        /* ultraButton1 */
        /*  */
        THIS-OBJECT:ultraButton1:Location = NEW System.Drawing.Point(12, 12).
        THIS-OBJECT:ultraButton1:Name = "ultraButton1":U.
        THIS-OBJECT:ultraButton1:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:ultraButton1:TabIndex = 1.
        THIS-OBJECT:ultraButton1:Text = "All":U.
        THIS-OBJECT:ultraButton1:Click:Subscribe(THIS-OBJECT:ultraButton1_Click).
        /*  */
        /* btnCopy */
        /*  */
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:btnCopy:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btnCopy:DialogResult = System.Windows.Forms.DialogResult:OK.
        THIS-OBJECT:btnCopy:Location = NEW System.Drawing.Point(367, 354).
        THIS-OBJECT:btnCopy:Name = "btnCopy":U.
        THIS-OBJECT:btnCopy:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:btnCopy:TabIndex = 2.
        THIS-OBJECT:btnCopy:Text = "&Copy":U.
        THIS-OBJECT:btnCopy:Click:Subscribe(THIS-OBJECT:btnCopy_Click).
        /*  */
        /* btnCancel */
        /*  */
        DEFINE VARIABLE nestedvar3 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar3 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:btnCancel:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar3, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btnCancel:DialogResult = System.Windows.Forms.DialogResult:Cancel.
        THIS-OBJECT:btnCancel:Location = NEW System.Drawing.Point(448, 354).
        THIS-OBJECT:btnCancel:Name = "btnCancel":U.
        THIS-OBJECT:btnCancel:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:btnCancel:TabIndex = 3.
        THIS-OBJECT:btnCancel:Text = "Ca&ncel":U.
        /*  */
        /* CopyFieldsForm */
        /*  */
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(535, 389).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btnCancel).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btnCopy).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:panel1).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:Name = "CopyFieldsForm":U.
        THIS-OBJECT:Text = "Copy Field Names":U.
        THIS-OBJECT:WindowPositionRegistryKey = "CopyFieldsForm":U.
        CAST(THIS-OBJECT:ultraListView1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:panel1:ResumeLayout(FALSE).
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the Load event
        Notes:
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnLoad (e AS System.EventArgs):

        SUPER:OnLoad (e) .

        THIS-OBJECT:PopulateFields () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Populates the List View with Field Names
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID PopulateFields ():

        DEFINE VARIABLE hQuery  AS HANDLE                                              NO-UNDO .
        DEFINE VARIABLE hBuffer AS HANDLE                                              NO-UNDO .
        DEFINE VARIABLE oItem   AS Infragistics.Win.UltraWinListView.UltraListViewItem NO-UNDO .

        HandleAssert:WidgetType (THIS-OBJECT:BufferHandle, WidgetTypeEnum:Buffer) .
        Assert:NotNullOrEmpty (THIS-OBJECT:TableName, "TableName":U) .

        ultraListView1:Items:Clear () .

        CREATE BUFFER hBuffer FOR TABLE (THIS-OBJECT:BufferHandle) .

        hQuery = QueryHelper:CreatePreparedQuery (hBuffer,
                                                  SUBSTITUTE ("for each eField where TempTableName = &1 by FieldOrder":U, QUOTER (THIS-OBJECT:TableName))) .

        DO WHILE NOT hQuery:QUERY-OFF-END:
            oItem = ultraListView1:Items:Add (hBuffer::FieldName, hBuffer::FieldName) .
            oItem:CheckState = System.Windows.Forms.CheckState:Checked .

            hQuery:GET-NEXT () .
        END.

        FINALLY:
            GarbageCollectorHelper:DeleteObject (hQuery) .
            GarbageCollectorHelper:DeleteObject (hBuffer) .
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the ultraButton1
        Notes:   Selects all Items
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraButton1_Click (sender AS System.Object,
                                            e AS System.EventArgs):

        {Consultingwerk/foreach.i Infragistics.Win.UltraWinListView.UltraListViewItem oItem in ultraListView1:Items}
            oItem:CheckState = System.Windows.Forms.CheckState:Checked .
        END.

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the ultraButton2
        Notes:   Deselects all Items
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraButton2_Click (sender AS System.Object,
                                            e AS System.EventArgs):

        {Consultingwerk/foreach.i Infragistics.Win.UltraWinListView.UltraListViewItem oItem in ultraListView1:Items}
            oItem:CheckState = System.Windows.Forms.CheckState:Unchecked .
        END.

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the ultraButton3
        Notes:   Inverts the current item selection
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraButton3_Click (sender AS System.Object,
                                            e AS System.EventArgs):

        {Consultingwerk/foreach.i Infragistics.Win.UltraWinListView.UltraListViewItem oItem in ultraListView1:Items}
            IF Progress.Util.EnumHelper:AreEqual (oItem:CheckState, System.Windows.Forms.CheckState:Unchecked) THEN
                oItem:CheckState = System.Windows.Forms.CheckState:Checked .
            ELSE
                oItem:CheckState = System.Windows.Forms.CheckState:Unchecked .
        END.

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the CopyFieldsForm class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC CopyFieldsForm ():

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.