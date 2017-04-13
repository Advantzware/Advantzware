/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : SelectObjectNameDialog
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu May 29 07:57:47 CEST 2014
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Forms.BaseForm.
USING Consultingwerk.BusinessEntityDesigner.Plugins.RollbaseImporter.* FROM PROPATH .
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Plugins.RollbaseImporter.SelectObjectNameDialog INHERITS BaseForm:

    DEFINE PRIVATE VARIABLE btnOk AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE btnCancel AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityLookup1 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Returns the selected integration name
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY IntegrationName AS CHARACTER NO-UNDO
    GET.
    PRIVATE SET .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SelectObjectNameDialog class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SelectObjectNameDialog ():

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
        THIS-OBJECT:components = NEW System.ComponentModel.Container().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.BusinessEntityDesigner.Plugins.RollbaseImporter.SelectObjectNameDialog":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton1 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton1 = NEW Infragistics.Win.UltraWinEditors.EditorButton("ShowLookupDialog":U).
        THIS-OBJECT:btnOk = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:btnCancel = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:smartBusinessEntityLookup1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        CAST(THIS-OBJECT:smartBusinessEntityLookup1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* btnOk */
        /*  */
        THIS-OBJECT:btnOk:AccessibleDescription = resources:GetString("btnOk.AccessibleDescription":U).
        THIS-OBJECT:btnOk:AccessibleName = resources:GetString("btnOk.AccessibleName":U).
        THIS-OBJECT:btnOk:Anchor = CAST(resources:GetObject("btnOk.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btnOk:AutoSize = System.Convert:ToBoolean(resources:GetObject("btnOk.AutoSize":U)).
        THIS-OBJECT:btnOk:BackgroundImage = CAST(resources:GetObject("btnOk.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:btnOk:BackgroundImageLayout = CAST(resources:GetObject("btnOk.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:btnOk:DialogResult = System.Windows.Forms.DialogResult:OK.
        THIS-OBJECT:btnOk:Dock = CAST(resources:GetObject("btnOk.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:btnOk:Enabled = System.Convert:ToBoolean(resources:GetObject("btnOk.Enabled":U)).
        THIS-OBJECT:btnOk:Font = CAST(resources:GetObject("btnOk.Font":U), System.Drawing.Font).
        THIS-OBJECT:btnOk:ImeMode = CAST(resources:GetObject("btnOk.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:btnOk:Location = CAST(resources:GetObject("btnOk.Location":U), System.Drawing.Point).
        THIS-OBJECT:btnOk:MaximumSize = CAST(resources:GetObject("btnOk.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:btnOk:Name = "btnOk":U.
        THIS-OBJECT:btnOk:RightToLeft = CAST(resources:GetObject("btnOk.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:btnOk:Size = CAST(resources:GetObject("btnOk.Size":U), System.Drawing.Size).
        THIS-OBJECT:btnOk:TabIndex = System.Convert:ToInt32(resources:GetObject("btnOk.TabIndex":U)).
        THIS-OBJECT:btnOk:Text = resources:GetString("btnOk.Text":U).
        /*  */
        /* btnCancel */
        /*  */
        THIS-OBJECT:btnCancel:AccessibleDescription = resources:GetString("btnCancel.AccessibleDescription":U).
        THIS-OBJECT:btnCancel:AccessibleName = resources:GetString("btnCancel.AccessibleName":U).
        THIS-OBJECT:btnCancel:Anchor = CAST(resources:GetObject("btnCancel.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btnCancel:AutoSize = System.Convert:ToBoolean(resources:GetObject("btnCancel.AutoSize":U)).
        THIS-OBJECT:btnCancel:BackgroundImage = CAST(resources:GetObject("btnCancel.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:btnCancel:BackgroundImageLayout = CAST(resources:GetObject("btnCancel.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:btnCancel:DialogResult = System.Windows.Forms.DialogResult:Cancel.
        THIS-OBJECT:btnCancel:Dock = CAST(resources:GetObject("btnCancel.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:btnCancel:Font = CAST(resources:GetObject("btnCancel.Font":U), System.Drawing.Font).
        THIS-OBJECT:btnCancel:ImeMode = CAST(resources:GetObject("btnCancel.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:btnCancel:Location = CAST(resources:GetObject("btnCancel.Location":U), System.Drawing.Point).
        THIS-OBJECT:btnCancel:MaximumSize = CAST(resources:GetObject("btnCancel.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:btnCancel:Name = "btnCancel":U.
        THIS-OBJECT:btnCancel:RightToLeft = CAST(resources:GetObject("btnCancel.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:btnCancel:Size = CAST(resources:GetObject("btnCancel.Size":U), System.Drawing.Size).
        THIS-OBJECT:btnCancel:TabIndex = System.Convert:ToInt32(resources:GetObject("btnCancel.TabIndex":U)).
        THIS-OBJECT:btnCancel:Text = resources:GetString("btnCancel.Text":U).
        /*  */
        /* smartBusinessEntityLookup1 */
        /*  */
        THIS-OBJECT:smartBusinessEntityLookup1:AccessibleDescription = resources:GetString("smartBusinessEntityLookup1.AccessibleDescription":U).
        THIS-OBJECT:smartBusinessEntityLookup1:AccessibleName = resources:GetString("smartBusinessEntityLookup1.AccessibleName":U).
        THIS-OBJECT:smartBusinessEntityLookup1:Anchor = CAST(resources:GetObject("smartBusinessEntityLookup1.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityLookup1:AppServerPartition = resources:GetString("smartBusinessEntityLookup1.AppServerPartition":U).
        THIS-OBJECT:smartBusinessEntityLookup1:AutoSize = System.Convert:ToBoolean(resources:GetObject("smartBusinessEntityLookup1.AutoSize":U)).
        THIS-OBJECT:smartBusinessEntityLookup1:BackgroundImage = CAST(resources:GetObject("smartBusinessEntityLookup1.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:smartBusinessEntityLookup1:BackgroundImageLayout = CAST(resources:GetObject("smartBusinessEntityLookup1.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        editorButton1:AccessibleDescription = resources:GetString("editorButton1.AccessibleDescription":U).
        editorButton1:AccessibleName = resources:GetString("editorButton1.AccessibleName":U).
        editorButton1:Key = "ShowLookupDialog":U.
        editorButton1:Text = resources:GetString("editorButton1.Text":U).
        THIS-OBJECT:smartBusinessEntityLookup1:ButtonsRight:Add(editorButton1).
        THIS-OBJECT:smartBusinessEntityLookup1:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:smartBusinessEntityLookup1:Dock = CAST(resources:GetObject("smartBusinessEntityLookup1.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:smartBusinessEntityLookup1:Font = CAST(resources:GetObject("smartBusinessEntityLookup1.Font":U), System.Drawing.Font).
        THIS-OBJECT:smartBusinessEntityLookup1:ImeMode = CAST(resources:GetObject("smartBusinessEntityLookup1.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:smartBusinessEntityLookup1:Location = CAST(resources:GetObject("smartBusinessEntityLookup1.Location":U), System.Drawing.Point).
        THIS-OBJECT:smartBusinessEntityLookup1:LookupAdditionalFields = resources:GetString("smartBusinessEntityLookup1.LookupAdditionalFields":U).
        THIS-OBJECT:smartBusinessEntityLookup1:LookupBrowserExcludeFields = resources:GetString("smartBusinessEntityLookup1.LookupBrowserExcludeFields":U).
        THIS-OBJECT:smartBusinessEntityLookup1:LookupBrowserFields = resources:GetString("smartBusinessEntityLookup1.LookupBrowserFields":U).
        THIS-OBJECT:smartBusinessEntityLookup1:LookupControls = resources:GetString("smartBusinessEntityLookup1.LookupControls":U).
        THIS-OBJECT:smartBusinessEntityLookup1:LookupDialogClassName = resources:GetString("smartBusinessEntityLookup1.LookupDialogClassName":U).
        THIS-OBJECT:smartBusinessEntityLookup1:LookupDialogFilterFields = resources:GetString("smartBusinessEntityLookup1.LookupDialogFilterFields":U).
        THIS-OBJECT:smartBusinessEntityLookup1:LookupDialogFilterOperators = resources:GetString("smartBusinessEntityLookup1.LookupDialogFilterOperators":U).
        THIS-OBJECT:smartBusinessEntityLookup1:LookupDialogQuerySort = resources:GetString("smartBusinessEntityLookup1.LookupDialogQuerySort":U).
        THIS-OBJECT:smartBusinessEntityLookup1:LookupDialogQueryString = resources:GetString("smartBusinessEntityLookup1.LookupDialogQueryString":U).
        THIS-OBJECT:smartBusinessEntityLookup1:LookupDialogTitle = resources:GetString("smartBusinessEntityLookup1.LookupDialogTitle":U).
        THIS-OBJECT:smartBusinessEntityLookup1:LookupEntityName = resources:GetString("smartBusinessEntityLookup1.LookupEntityName":U).
        THIS-OBJECT:smartBusinessEntityLookup1:LookupEntityTable = resources:GetString("smartBusinessEntityLookup1.LookupEntityTable":U).
        THIS-OBJECT:smartBusinessEntityLookup1:LookupEntityView = resources:GetString("smartBusinessEntityLookup1.LookupEntityView":U).
        THIS-OBJECT:smartBusinessEntityLookup1:LookupFields = resources:GetString("smartBusinessEntityLookup1.LookupFields":U).
        THIS-OBJECT:smartBusinessEntityLookup1:LookupKeyField = resources:GetString("smartBusinessEntityLookup1.LookupKeyField":U).
        THIS-OBJECT:smartBusinessEntityLookup1:LookupKeyValue = resources:GetString("smartBusinessEntityLookup1.LookupKeyValue":U).
        THIS-OBJECT:smartBusinessEntityLookup1:LookupKeyValueColumn = resources:GetString("smartBusinessEntityLookup1.LookupKeyValueColumn":U).
        THIS-OBJECT:smartBusinessEntityLookup1:LookupQuerySort = resources:GetString("smartBusinessEntityLookup1.LookupQuerySort":U).
        THIS-OBJECT:smartBusinessEntityLookup1:LookupQueryString = resources:GetString("smartBusinessEntityLookup1.LookupQueryString":U).
        THIS-OBJECT:smartBusinessEntityLookup1:MaximumSize = CAST(resources:GetObject("smartBusinessEntityLookup1.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:smartBusinessEntityLookup1:Name = "smartBusinessEntityLookup1":U.
        THIS-OBJECT:smartBusinessEntityLookup1:RightToLeft = CAST(resources:GetObject("smartBusinessEntityLookup1.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:smartBusinessEntityLookup1:Size = CAST(resources:GetObject("smartBusinessEntityLookup1.Size":U), System.Drawing.Size).
        THIS-OBJECT:smartBusinessEntityLookup1:TabIndex = System.Convert:ToInt32(resources:GetObject("smartBusinessEntityLookup1.TabIndex":U)).
        THIS-OBJECT:smartBusinessEntityLookup1:ValueChanged:Subscribe(THIS-OBJECT:smartBusinessEntityLookup1_ValueChanged).
        /*  */
        /* ultraLabel1 */
        /*  */
        THIS-OBJECT:ultraLabel1:AccessibleDescription = resources:GetString("ultraLabel1.AccessibleDescription":U).
        THIS-OBJECT:ultraLabel1:AccessibleName = resources:GetString("ultraLabel1.AccessibleName":U).
        THIS-OBJECT:ultraLabel1:Anchor = CAST(resources:GetObject("ultraLabel1.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraLabel1:AutoSize = System.Convert:ToBoolean(resources:GetObject("ultraLabel1.AutoSize":U)).
        THIS-OBJECT:ultraLabel1:BackgroundImage = CAST(resources:GetObject("ultraLabel1.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:ultraLabel1:BackgroundImageLayout = CAST(resources:GetObject("ultraLabel1.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:ultraLabel1:Dock = CAST(resources:GetObject("ultraLabel1.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:ultraLabel1:Font = CAST(resources:GetObject("ultraLabel1.Font":U), System.Drawing.Font).
        THIS-OBJECT:ultraLabel1:ImeMode = CAST(resources:GetObject("ultraLabel1.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:ultraLabel1:Location = CAST(resources:GetObject("ultraLabel1.Location":U), System.Drawing.Point).
        THIS-OBJECT:ultraLabel1:MaximumSize = CAST(resources:GetObject("ultraLabel1.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1":U.
        THIS-OBJECT:ultraLabel1:RightToLeft = CAST(resources:GetObject("ultraLabel1.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:ultraLabel1:Size = CAST(resources:GetObject("ultraLabel1.Size":U), System.Drawing.Size).
        THIS-OBJECT:ultraLabel1:TabIndex = System.Convert:ToInt32(resources:GetObject("ultraLabel1.TabIndex":U)).
        THIS-OBJECT:ultraLabel1:Text = resources:GetString("ultraLabel1.Text":U).
        /*  */
        /* SelectObjectNameDialog */
        /*  */
        THIS-OBJECT:AcceptButton = THIS-OBJECT:btnOk.
        THIS-OBJECT:AccessibleDescription = resources:GetString("$this.AccessibleDescription":U).
        THIS-OBJECT:AccessibleName = resources:GetString("$this.AccessibleName":U).
        THIS-OBJECT:AutoScroll = System.Convert:ToBoolean(resources:GetObject("$this.AutoScroll":U)).
        THIS-OBJECT:AutoSize = System.Convert:ToBoolean(resources:GetObject("$this.AutoSize":U)).
        THIS-OBJECT:AutoSizeMode = CAST(resources:GetObject("$this.AutoSizeMode":U), System.Windows.Forms.AutoSizeMode).
        THIS-OBJECT:BackgroundImage = CAST(resources:GetObject("$this.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:BackgroundImageLayout = CAST(resources:GetObject("$this.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:CancelButton = THIS-OBJECT:btnCancel.
        THIS-OBJECT:ClientSize = CAST(resources:GetObject("$this.ClientSize":U), System.Drawing.Size).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:smartBusinessEntityLookup1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btnCancel).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btnOk).
        THIS-OBJECT:Font = CAST(resources:GetObject("$this.Font":U), System.Drawing.Font).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:ImeMode = CAST(resources:GetObject("$this.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:Location = CAST(resources:GetObject("$this.Location":U), System.Drawing.Point).
        THIS-OBJECT:MaximizeBox = FALSE.
        THIS-OBJECT:MaximumSize = CAST(resources:GetObject("$this.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:MinimizeBox = FALSE.
        THIS-OBJECT:Name = "SelectObjectNameDialog":U.
        THIS-OBJECT:RightToLeft = CAST(resources:GetObject("$this.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:RightToLeftLayout = System.Convert:ToBoolean(resources:GetObject("$this.RightToLeftLayout":U)).
        THIS-OBJECT:StartPosition = CAST(resources:GetObject("$this.StartPosition":U), System.Windows.Forms.FormStartPosition).
        THIS-OBJECT:Text = resources:GetString("$this.Text":U).
        THIS-OBJECT:WindowPositionRegistryKey = resources:GetString("$this.WindowPositionRegistryKey":U).
        CAST(THIS-OBJECT:smartBusinessEntityLookup1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the FormClosing event
        Notes:
        @param e The FormClosingEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnFormClosing (e AS System.Windows.Forms.FormClosingEventArgs):

        ASSIGN THIS-OBJECT:IntegrationName = smartBusinessEntityLookup1:Text  .

        SUPER:OnFormClosing (e) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ValueChanged event of the smartBusinessEntityLookup1
        Notes:
        @param sender The reference to the object that raised this event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID smartBusinessEntityLookup1_ValueChanged (sender AS System.Object,
                                                                 e AS System.EventArgs):

        ASSIGN btnOk:Enabled = (smartBusinessEntityLookup1:Text > "":U) .

    END METHOD .

END CLASS.
