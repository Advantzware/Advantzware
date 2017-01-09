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
    File        : InsertDataAccessValidationWizard
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jun 30 14:57:16 CEST 2013
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Forms.*                     FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.UI.* FROM PROPATH .
USING Consultingwerk.Framework.Collections.*     FROM PROPATH .
USING Consultingwerk.Util.*                      FROM PROPATH .
USING Progress.Lang.*                            FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.UI.InsertDataAccessValidationWizard INHERITS BaseForm:

    DEFINE PRIVATE VARIABLE btnOk AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE btnCancel AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE cbValidationType AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE cbBufferName AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE panel1 AS System.Windows.Forms.Panel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel2 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the list with the buffer names
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BufferList AS CharacterList NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the Method Name
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BufferName AS CHARACTER NO-UNDO
    GET():
.        RETURN cbBufferName:Text.
    END GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the Parameter Type
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ValidationType AS CHARACTER NO-UNDO
    GET():
        RETURN cbValidationType:Text.
    END GET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the InsertDataAccessValidationWizard class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC InsertDataAccessValidationWizard ():

        SUPER().
        InitializeComponent().

        &IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN
        THIS-OBJECT:ComponentsCollection:Add (THIS-OBJECT:components).
        &ENDIF

        THIS-OBJECT:StoreFormPosition = FALSE .

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the TextChanged event of the ComboBoxEditor controls
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this method
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID cbBufferName_TextChanged (sender AS System.Object,
                                                  e AS System.EventArgs):

        THIS-OBJECT:btnOK:Enabled = (THIS-OBJECT:cbBufferName:Text > "":U AND
                                     THIS-OBJECT:cbValidationType:Text > "":U) .
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
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.BusinessEntityDesigner.UI.InsertDataAccessValidationWizard":U).
        THIS-OBJECT:panel1 = NEW System.Windows.Forms.Panel().
        THIS-OBJECT:cbValidationType = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        THIS-OBJECT:cbBufferName = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        THIS-OBJECT:ultraLabel2 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:btnOk = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:btnCancel = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:panel1:SuspendLayout().
        CAST(THIS-OBJECT:cbValidationType, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:cbBufferName, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* panel1 */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:panel1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:panel1:BackColor = System.Drawing.SystemColors:ControlLightLight.
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:cbValidationType).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:cbBufferName).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:ultraLabel2).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:ultraLabel1).
        THIS-OBJECT:panel1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:panel1:Name = "panel1":U.
        THIS-OBJECT:panel1:Size = NEW System.Drawing.Size(478, 143).
        THIS-OBJECT:panel1:TabIndex = 0.
        /*  */
        /* cbValidationType */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:cbValidationType:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:cbValidationType:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:cbValidationType:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        THIS-OBJECT:cbValidationType:Location = NEW System.Drawing.Point(149, 61).
        THIS-OBJECT:cbValidationType:Name = "cbValidationType":U.
        THIS-OBJECT:cbValidationType:Size = NEW System.Drawing.Size(318, 21).
        THIS-OBJECT:cbValidationType:TabIndex = 4.
        THIS-OBJECT:cbValidationType:TextChanged:Subscribe(THIS-OBJECT:cbBufferName_TextChanged).
        /*  */
        /* cbBufferName */
        /*  */
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:cbBufferName:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:cbBufferName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:cbBufferName:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        THIS-OBJECT:cbBufferName:Location = NEW System.Drawing.Point(149, 34).
        THIS-OBJECT:cbBufferName:Name = "cbBufferName":U.
        THIS-OBJECT:cbBufferName:Size = NEW System.Drawing.Size(318, 21).
        THIS-OBJECT:cbBufferName:TabIndex = 3.
        THIS-OBJECT:cbBufferName:TextChanged:Subscribe(THIS-OBJECT:cbBufferName_TextChanged).
        /*  */
        /* ultraLabel2 */
        /*  */
        THIS-OBJECT:ultraLabel2:Location = NEW System.Drawing.Point(12, 64).
        THIS-OBJECT:ultraLabel2:Name = "ultraLabel2":U.
        THIS-OBJECT:ultraLabel2:Size = NEW System.Drawing.Size(131, 23).
        THIS-OBJECT:ultraLabel2:TabIndex = 2.
        THIS-OBJECT:ultraLabel2:Text = "Validation Type:":U.
        /*  */
        /* ultraLabel1 */
        /*  */
        THIS-OBJECT:ultraLabel1:Location = NEW System.Drawing.Point(12, 37).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1":U.
        THIS-OBJECT:ultraLabel1:Size = NEW System.Drawing.Size(131, 23).
        THIS-OBJECT:ultraLabel1:TabIndex = 0.
        THIS-OBJECT:ultraLabel1:Text = "Buffer Name:":U.
        /*  */
        /* btnOk */
        /*  */
        DEFINE VARIABLE nestedvar3 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar3 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:btnOk:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar3, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btnOk:DialogResult = System.Windows.Forms.DialogResult:OK.
        THIS-OBJECT:btnOk:Enabled = FALSE.
        THIS-OBJECT:btnOk:Location = NEW System.Drawing.Point(311, 157).
        THIS-OBJECT:btnOk:Name = "btnOk":U.
        THIS-OBJECT:btnOk:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:btnOk:TabIndex = 1.
        THIS-OBJECT:btnOk:Text = "&OK":U.
        /*  */
        /* btnCancel */
        /*  */
        DEFINE VARIABLE nestedvar4 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar4 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:btnCancel:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar4, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btnCancel:DialogResult = System.Windows.Forms.DialogResult:Cancel.
        THIS-OBJECT:btnCancel:Location = NEW System.Drawing.Point(392, 157).
        THIS-OBJECT:btnCancel:Name = "btnCancel":U.
        THIS-OBJECT:btnCancel:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:btnCancel:TabIndex = 2.
        THIS-OBJECT:btnCancel:Text = "&Cancel":U.
        /*  */
        /* InsertDataAccessValidationWizard */
        /*  */
        THIS-OBJECT:AcceptButton = THIS-OBJECT:btnOk.
        THIS-OBJECT:CancelButton = THIS-OBJECT:btnCancel.
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(479, 192).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btnCancel).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btnOk).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:panel1).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:Name = "InsertDataAccessValidationWizard":U.
        THIS-OBJECT:StartPosition = System.Windows.Forms.FormStartPosition:CenterParent.
        THIS-OBJECT:Text = "Data Access validation method wizard":U.
        THIS-OBJECT:WindowPositionRegistryKey = "InsertDataAccessValidationWizard":U.
        THIS-OBJECT:panel1:ResumeLayout(FALSE).
        THIS-OBJECT:panel1:PerformLayout().
        CAST(THIS-OBJECT:cbValidationType, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:cbBufferName, System.ComponentModel.ISupportInitialize):EndInit().
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
    METHOD OVERRIDE PROTECTED VOID OnLoad (INPUT e AS System.EventArgs):

        SUPER:OnLoad(INPUT e).

        THIS-OBJECT:cbBufferName:ValueList = ValueListHelper:FromList (THIS-OBJECT:BufferList) .

        THIS-OBJECT:cbValidationType:ValueList:ValueListItems:Add ("CreateBeginTrans":U) .
        THIS-OBJECT:cbValidationType:ValueList:ValueListItems:Add ("CreateEndTrans":U) .
        THIS-OBJECT:cbValidationType:ValueList:ValueListItems:Add ("ModifyBeginTrans":U) .
        THIS-OBJECT:cbValidationType:ValueList:ValueListItems:Add ("ModifyEndTrans":U) .
        THIS-OBJECT:cbValidationType:ValueList:ValueListItems:Add ("DeleteBeginTrans":U) .
        THIS-OBJECT:cbValidationType:ValueList:ValueListItems:Add ("DeleteEndTrans":U) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the InsertDataAccessValidationWizard class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC InsertDataAccessValidationWizard ():

    END DESTRUCTOR .

END CLASS.
