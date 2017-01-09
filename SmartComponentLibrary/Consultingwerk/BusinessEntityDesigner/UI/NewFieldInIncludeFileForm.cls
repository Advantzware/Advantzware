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
    File        : NewFieldInIncludeFileForm
    Purpose     : Dialog used by TempTableImported when a new field
                  has been found in the include file
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Jul 10 13:03:33 CEST 2015
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.BusinessEntityDesigner.UI.* FROM PROPATH .
USING Consultingwerk.Framework.Collections.*     FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.*      FROM PROPATH .
USING Consultingwerk.Util.*                      FROM PROPATH .
USING Progress.Lang.*                            FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.UI.NewFieldInIncludeFileForm
    INHERITS SmartWindowForm:

    DEFINE PRIVATE VARIABLE btnOk AS System.Windows.Forms.Button NO-UNDO.
    DEFINE PRIVATE VARIABLE btnCancel AS System.Windows.Forms.Button NO-UNDO.
    DEFINE PRIVATE VARIABLE cbRemovedFields AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE rsRenamedField AS System.Windows.Forms.RadioButton NO-UNDO.
    DEFINE PRIVATE VARIABLE rsNewField AS System.Windows.Forms.RadioButton NO-UNDO.
    DEFINE PRIVATE VARIABLE tbFieldName AS System.Windows.Forms.TextBox NO-UNDO.
    DEFINE PRIVATE VARIABLE textBox1 AS System.Windows.Forms.TextBox NO-UNDO.
    DEFINE PRIVATE VARIABLE pictureBox1 AS System.Windows.Forms.PictureBox NO-UNDO.
    DEFINE PRIVATE VARIABLE panel1 AS System.Windows.Forms.Panel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the new field name
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY NewFieldName AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the list of remove fields
        Notes:   List is not initializted by this class
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY RemovedFields AS CharacterList NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns "" when the field should be considered a new field or the
                 name of the original field in case of a renamed field
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY RenamedFieldName AS CHARACTER NO-UNDO
    GET:
        IF rsRenamedField:Checked THEN
            RETURN cbRemovedFields:TEXT .
        ELSE RETURN "" .
    END GET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the NewFieldInIncludeFileForm class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC NewFieldInIncludeFileForm ():

        SUPER().
        InitializeComponent().

        &IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN
        THIS-OBJECT:ComponentsCollection:Add (THIS-OBJECT:components).
        &ENDIF

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
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.BusinessEntityDesigner.UI.NewFieldInIncludeFileForm":U).
        THIS-OBJECT:btnCancel = NEW System.Windows.Forms.Button().
        THIS-OBJECT:btnOk = NEW System.Windows.Forms.Button().
        THIS-OBJECT:panel1 = NEW System.Windows.Forms.Panel().
        THIS-OBJECT:cbRemovedFields = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:rsRenamedField = NEW System.Windows.Forms.RadioButton().
        THIS-OBJECT:rsNewField = NEW System.Windows.Forms.RadioButton().
        THIS-OBJECT:tbFieldName = NEW System.Windows.Forms.TextBox().
        THIS-OBJECT:pictureBox1 = NEW System.Windows.Forms.PictureBox().
        THIS-OBJECT:textBox1 = NEW System.Windows.Forms.TextBox().
        THIS-OBJECT:panel1:SuspendLayout().
        CAST(THIS-OBJECT:cbRemovedFields, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:pictureBox1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* btnCancel */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:btnCancel:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btnCancel:DialogResult = System.Windows.Forms.DialogResult:Cancel.
        THIS-OBJECT:btnCancel:Location = NEW System.Drawing.Point(409, 179).
        THIS-OBJECT:btnCancel:Name = "btnCancel":U.
        THIS-OBJECT:btnCancel:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:btnCancel:TabIndex = 5.
        THIS-OBJECT:btnCancel:Text = "&Cancel":U.
        THIS-OBJECT:btnCancel:UseCompatibleTextRendering = TRUE.
        THIS-OBJECT:btnCancel:UseVisualStyleBackColor = TRUE.
        /*  */
        /* btnOk */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:btnOk:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btnOk:DialogResult = System.Windows.Forms.DialogResult:OK.
        THIS-OBJECT:btnOk:Location = NEW System.Drawing.Point(328, 179).
        THIS-OBJECT:btnOk:Name = "btnOk":U.
        THIS-OBJECT:btnOk:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:btnOk:TabIndex = 4.
        THIS-OBJECT:btnOk:Text = "&Ok":U.
        THIS-OBJECT:btnOk:UseCompatibleTextRendering = TRUE.
        THIS-OBJECT:btnOk:UseVisualStyleBackColor = TRUE.
        /*  */
        /* panel1 */
        /*  */
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:panel1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:panel1:BackColor = System.Drawing.SystemColors:ControlLightLight.
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:cbRemovedFields).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:ultraLabel1).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:rsRenamedField).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:rsNewField).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:tbFieldName).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:pictureBox1).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:textBox1).
        THIS-OBJECT:panel1:Location = NEW System.Drawing.Point(0, 6).
        THIS-OBJECT:panel1:Margin = NEW System.Windows.Forms.Padding(0).
        THIS-OBJECT:panel1:Name = "panel1":U.
        THIS-OBJECT:panel1:Size = NEW System.Drawing.Size(496, 160).
        THIS-OBJECT:panel1:TabIndex = 3.
        /*  */
        /* cbRemovedFields */
        /*  */
        DEFINE VARIABLE nestedvar3 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar3 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:cbRemovedFields:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar3, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:cbRemovedFields:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:cbRemovedFields:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        THIS-OBJECT:cbRemovedFields:Enabled = FALSE.
        THIS-OBJECT:cbRemovedFields:Location = NEW System.Drawing.Point(153, 126).
        THIS-OBJECT:cbRemovedFields:Name = "cbRemovedFields":U.
        THIS-OBJECT:cbRemovedFields:Size = NEW System.Drawing.Size(331, 21).
        THIS-OBJECT:cbRemovedFields:TabIndex = 6.
        /*  */
        /* ultraLabel1 */
        /*  */
        THIS-OBJECT:ultraLabel1:Location = NEW System.Drawing.Point(48, 130).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1":U.
        THIS-OBJECT:ultraLabel1:Size = NEW System.Drawing.Size(109, 23).
        THIS-OBJECT:ultraLabel1:TabIndex = 8.
        THIS-OBJECT:ultraLabel1:Text = "Original field name:":U.
        /*  */
        /* rsRenamedField */
        /*  */
        THIS-OBJECT:rsRenamedField:AutoSize = TRUE.
        THIS-OBJECT:rsRenamedField:Location = NEW System.Drawing.Point(48, 100).
        THIS-OBJECT:rsRenamedField:Name = "rsRenamedField":U.
        THIS-OBJECT:rsRenamedField:Size = NEW System.Drawing.Size(249, 18).
        THIS-OBJECT:rsRenamedField:TabIndex = 7.
        THIS-OBJECT:rsRenamedField:Text = "the field has been renamed in the include file:":U.
        THIS-OBJECT:rsRenamedField:UseCompatibleTextRendering = TRUE.
        THIS-OBJECT:rsRenamedField:UseVisualStyleBackColor = TRUE.
        THIS-OBJECT:rsRenamedField:CheckedChanged:Subscribe(THIS-OBJECT:rsRenamedField_CheckedChanged).
        /*  */
        /* rsNewField */
        /*  */
        THIS-OBJECT:rsNewField:AutoSize = TRUE.
        THIS-OBJECT:rsNewField:Checked = TRUE.
        THIS-OBJECT:rsNewField:Location = NEW System.Drawing.Point(48, 76).
        THIS-OBJECT:rsNewField:Name = "rsNewField":U.
        THIS-OBJECT:rsNewField:Size = NEW System.Drawing.Size(108, 18).
        THIS-OBJECT:rsNewField:TabIndex = 6.
        THIS-OBJECT:rsNewField:TabStop = TRUE.
        THIS-OBJECT:rsNewField:Text = "this is a new field":U.
        THIS-OBJECT:rsNewField:UseCompatibleTextRendering = TRUE.
        THIS-OBJECT:rsNewField:UseVisualStyleBackColor = TRUE.
        /*  */
        /* tbFieldName */
        /*  */
        DEFINE VARIABLE nestedvar4 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar4 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbFieldName:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar4, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbFieldName:BackColor = System.Drawing.SystemColors:ControlLightLight.
        THIS-OBJECT:tbFieldName:BorderStyle = System.Windows.Forms.BorderStyle:None.
        THIS-OBJECT:tbFieldName:Enabled = FALSE.
        THIS-OBJECT:tbFieldName:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Bold, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:tbFieldName:Location = NEW System.Drawing.Point(50, 50).
        THIS-OBJECT:tbFieldName:Name = "tbFieldName":U.
        THIS-OBJECT:tbFieldName:Size = NEW System.Drawing.Size(434, 13).
        THIS-OBJECT:tbFieldName:TabIndex = 5.
        THIS-OBJECT:tbFieldName:Text = "Customer.CustNum":U.
        /*  */
        /* pictureBox1 */
        /*  */
        THIS-OBJECT:pictureBox1:Image = CAST(resources:GetObject("pictureBox1.Image":U), System.Drawing.Image).
        THIS-OBJECT:pictureBox1:Location = NEW System.Drawing.Point(12, 12).
        THIS-OBJECT:pictureBox1:Name = "pictureBox1":U.
        THIS-OBJECT:pictureBox1:Size = NEW System.Drawing.Size(32, 32).
        THIS-OBJECT:pictureBox1:SizeMode = System.Windows.Forms.PictureBoxSizeMode:AutoSize.
        THIS-OBJECT:pictureBox1:TabIndex = 4.
        THIS-OBJECT:pictureBox1:TabStop = FALSE.
        /*  */
        /* textBox1 */
        /*  */
        DEFINE VARIABLE nestedvar5 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar5 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:textBox1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar5, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:textBox1:BackColor = System.Drawing.SystemColors:ControlLightLight.
        THIS-OBJECT:textBox1:BorderStyle = System.Windows.Forms.BorderStyle:None.
        THIS-OBJECT:textBox1:Enabled = FALSE.
        THIS-OBJECT:textBox1:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Bold, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:textBox1:Location = NEW System.Drawing.Point(50, 12).
        THIS-OBJECT:textBox1:Multiline = TRUE.
        THIS-OBJECT:textBox1:Name = "textBox1":U.
        THIS-OBJECT:textBox1:Size = NEW System.Drawing.Size(434, 32).
        THIS-OBJECT:textBox1:TabIndex = 3.
        THIS-OBJECT:textBox1:Text = "The new following field has been found in the temp-table source code:":U.
        /*  */
        /* NewFieldInIncludeFileForm */
        /*  */
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(496, 208).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btnCancel).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btnOk).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:panel1).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:Name = "NewFieldInIncludeFileForm":U.
        THIS-OBJECT:Text = "New Field in Include File":U.
        THIS-OBJECT:WindowPositionRegistryKey = "NewFieldInIncludeFileForm":U.
        THIS-OBJECT:panel1:ResumeLayout(FALSE).
        THIS-OBJECT:panel1:PerformLayout().
        CAST(THIS-OBJECT:cbRemovedFields, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:pictureBox1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the Load event
        Notes:
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnLoad (e AS System.EventArgs):

        SUPER:OnLoad (e) .

        THIS-OBJECT:tbFieldName:Text = THIS-OBJECT:NewFieldName .

        THIS-OBJECT:cbRemovedFields:ValueList = ValueListHelper:FromList (THIS-OBJECT:RemovedFields) .

        IF THIS-OBJECT:cbRemovedFields:ValueList:ValueListItems:Count > 0 THEN
            THIS-OBJECT:cbRemovedFields:SelectedIndex = 0 .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the CheckedChanged evnent of the rsRenamedField
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID rsRenamedField_CheckedChanged (sender AS System.Object,
                                                       e AS System.EventArgs):

        ASSIGN cbRemovedFields:Enabled = rsRenamedField:Checked .

        IF rsRenamedField:Checked THEN
            cbRemovedFields:DropDown () .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the NewFieldInIncludeFileForm class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC NewFieldInIncludeFileForm ():

    END DESTRUCTOR .

END CLASS.
