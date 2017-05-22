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
    File        : GetDatabaseNameForm
    Purpose     : Provides an UI to select the working database
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Nov 28 07:17:43 CET 2012
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Forms.*                    FROM PROPATH .
USING Consultingwerk.Windows.Framework.System.* FROM PROPATH .
USING Consultingwerk.Util.*                     FROM PROPATH .
USING Progress.Lang.*                           FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.System.GetDatabaseNameForm
    INHERITS BaseForm:

    DEFINE PRIVATE VARIABLE pictureBox1 AS System.Windows.Forms.PictureBox NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraComboEditor1 AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraButton2 AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraButton1 AS Infragistics.Win.Misc.UltraButton NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Returns the selected database name
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DatabaseName AS CHARACTER NO-UNDO
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the comma-delimited list of all available databases
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DatabaseNames AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the GetDatabaseNameForm class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC GetDatabaseNameForm ():


        SUPER().
        InitializeComponent().

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
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.System.GetDatabaseNameForm":U).
        THIS-OBJECT:pictureBox1 = NEW System.Windows.Forms.PictureBox().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraComboEditor1 = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        THIS-OBJECT:ultraButton1 = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:ultraButton2 = NEW Infragistics.Win.Misc.UltraButton().
        CAST(THIS-OBJECT:pictureBox1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraComboEditor1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* pictureBox1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:pictureBox1, "pictureBox1":U).
        THIS-OBJECT:pictureBox1:Name = "pictureBox1":U.
        THIS-OBJECT:pictureBox1:TabStop = FALSE.
        /*  */
        /* ultraLabel1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraLabel1, "ultraLabel1":U).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1":U.
        /*  */
        /* ultraComboEditor1 */
        /*  */
        THIS-OBJECT:ultraComboEditor1:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        resources:ApplyResources(THIS-OBJECT:ultraComboEditor1, "ultraComboEditor1":U).
        THIS-OBJECT:ultraComboEditor1:Name = "ultraComboEditor1":U.
        THIS-OBJECT:ultraComboEditor1:SelectionChanged:Subscribe(THIS-OBJECT:ultraComboEditor1_SelectionChanged).
        THIS-OBJECT:ultraComboEditor1:ValueChanged:Subscribe(THIS-OBJECT:ultraComboEditor1_ValueChanged).
        /*  */
        /* ultraButton1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraButton1, "ultraButton1":U).
        THIS-OBJECT:ultraButton1:DialogResult = System.Windows.Forms.DialogResult:OK.
        THIS-OBJECT:ultraButton1:Name = "ultraButton1":U.
        /*  */
        /* ultraButton2 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraButton2, "ultraButton2":U).
        THIS-OBJECT:ultraButton2:DialogResult = System.Windows.Forms.DialogResult:Cancel.
        THIS-OBJECT:ultraButton2:Name = "ultraButton2":U.
        /*  */
        /* GetDatabaseNameForm */
        /*  */
        THIS-OBJECT:AcceptButton = THIS-OBJECT:ultraButton1.
        THIS-OBJECT:CancelButton = THIS-OBJECT:ultraButton2.
        resources:ApplyResources(THIS-OBJECT, "$this":U).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraButton2).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraButton1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraComboEditor1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:pictureBox1).
        THIS-OBJECT:FormBorderStyle = System.Windows.Forms.FormBorderStyle:FixedDialog.
        THIS-OBJECT:MaximizeBox = FALSE.
        THIS-OBJECT:MinimizeBox = FALSE.
        THIS-OBJECT:Name = "GetDatabaseNameForm":U.
        THIS-OBJECT:ShowIcon = FALSE.
        THIS-OBJECT:ShowInTaskbar = FALSE.
        CAST(THIS-OBJECT:pictureBox1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraComboEditor1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
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

        ultraComboEditor1:ValueList = ValueListHelper:FromList (THIS-OBJECT:DatabaseNames) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the SelectionChanged event of the ultraComboEditor1
        Notes:
        @param sender The reference to the object that raised the error
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraComboEditor1_SelectionChanged (sender AS System.Object,
                                                            e AS System.EventArgs):

        ultraButton1:Enabled = VALID-OBJECT (ultraComboEditor1:SelectedItem) .

        IF VALID-OBJECT (ultraComboEditor1:SelectedItem) THEN
            THIS-OBJECT:DatabaseName = UNBOX (ultraComboEditor1:SelectedItem:DataValue) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ValueChanged event of the ultraComboEditor1
        Notes:
        @param sender The reference to the object that raised the error
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraComboEditor1_ValueChanged (sender AS System.Object,
                                                        e AS System.EventArgs ):

        ultraButton1:Enabled = VALID-OBJECT (ultraComboEditor1:SelectedItem) .

        IF VALID-OBJECT (ultraComboEditor1:SelectedItem) THEN
            THIS-OBJECT:DatabaseName = UNBOX (ultraComboEditor1:SelectedItem:DataValue) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the GetDatabaseNameForm class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC GetDatabaseNameForm ():

    END DESTRUCTOR .

END CLASS.
