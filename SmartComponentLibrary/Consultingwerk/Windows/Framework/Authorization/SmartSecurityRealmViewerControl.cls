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
    File        : SmartSecurityRealmViewerControl
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 28.07.2013 13:46:34
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Base.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.Util.*                           FROM PROPATH .
USING Infragistics.Win.UltraWinEditors.*              FROM ASSEMBLY .
USING Progress.Lang.*                                 FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.Windows.Framework.Authorization.SmartSecurityRealmViewerControl
    INHERITS SmartViewerControl:

    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.

    DEFINE PRIVATE VARIABLE eSmartSecurityRealm_CompanyDependent AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSecurityRealm_DefaultRestricted AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSecurityRealm_SecurityRealmService_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSecurityRealm_SecurityRealmService AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSecurityRealm_SecurityRealmCode_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSecurityRealm_SecurityRealmCode AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSecurityRealm_SecurityRealmDescription_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSecurityRealm_SecurityRealmDescription AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.

    {Consultingwerk/Util/TempTables/ttClassNames.i}

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartSecurityRealmViewerControl class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartSecurityRealmViewerControl ():

        SUPER().
        InitializeComponent().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the EditorButtonClick event of the eSmartSecurityRealm_SecurityRealmService
        Notes:
        @param sender The reference to the object that raised the event
        @param e The EditorButtonEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID eSmartSecurityRealm_SecurityRealmService_EditorButtonClick (sender AS System.Object,
                                                                                    e AS Infragistics.Win.UltraWinEditors.EditorButtonEventArgs):

        DEFINE VARIABLE oForm         AS Consultingwerk.SmartComponents.Support.BusinessEntityPickerForm NO-UNDO .
        DEFINE VARIABLE oDialogResult AS System.Windows.Forms.DialogResult                               NO-UNDO .

        DEFINE VARIABLE oInfo         AS Infragistics.Win.Misc.UltraDesktopAlertWindowInfo               NO-UNDO .

        oInfo = Consultingwerk.SmartComponents.Support.DesktopAlertHelper:Show ("SmartFramework"{&TRAN},
                                                                                "Scanning Project Folder(s) for Security Realm Services."{&TRAN}) .

        Consultingwerk.Util.ClassHelper:GetClassNamesInClassPath ("Consultingwerk.SmartFramework.Authorization.ISecurityRealmService"{&TRAN},
                                                                  FALSE,
                                                                  OUTPUT TABLE ttClassNames BY-REFERENCE) .

        oInfo:Close () .

        oForm = NEW Consultingwerk.SmartComponents.Support.BusinessEntityPickerForm () .
        oForm:Text = "Select Security Realm Service"{&TRAN} .

        oForm:ReceiveBusinessEntityNames (TABLE ttClassNames) .

/*        WAIT-FOR oForm:ShowDialog () SET oDialogResult .*/

        /* Mike Fechner, Consultingwerk Ltd. 14.07.2010
           WAIT-FOR Workaround */
        ASSIGN oDialogResult = Consultingwerk.SmartComponents.FormHelper:ShowDialog(oForm) .

        IF Progress.Util.EnumHelper:AreEqual(oDialogResult,
                                             System.Windows.Forms.DialogResult:Ok) THEN

            ASSIGN THIS-OBJECT:eSmartSecurityRealm_SecurityRealmService:Text = oForm:GetSelectedBusinessEntityName () .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage(err) .
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
        THIS-OBJECT:components = NEW System.ComponentModel.Container().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.Authorization.SmartSecurityRealmViewerControl":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc1 = NEW Progress.Data.TableDesc("eSmartSecurityRealm":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
        appearance3 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton1 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton1 = NEW Infragistics.Win.UltraWinEditors.EditorButton().
        THIS-OBJECT:smartBusinessEntityBindingSource1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmCode_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmCode = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmDescription_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmDescription = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmService_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmService = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartSecurityRealm_DefaultRestricted = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:eSmartSecurityRealm_CompanyDependent = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartSecurityRealm_SecurityRealmCode, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartSecurityRealm_SecurityRealmDescription, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartSecurityRealm_SecurityRealmService, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartSecurityRealm_DefaultRestricted, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartSecurityRealm_CompanyDependent, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* smartBusinessEntityBindingSource1 */
        /*  */
        THIS-OBJECT:smartBusinessEntityBindingSource1:AddFields = FALSE.
        THIS-OBJECT:smartBusinessEntityBindingSource1:BindTo = resources:GetString("smartBusinessEntityBindingSource1.BindTo":U).
        THIS-OBJECT:smartBusinessEntityBindingSource1:ClearSchema = FALSE.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityJoin = resources:GetString("smartBusinessEntityBindingSource1.EntityJoin":U).
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityName = resources:GetString("smartBusinessEntityBindingSource1.EntityName":U).
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityTable = resources:GetString("smartBusinessEntityBindingSource1.EntityTable":U).
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityView = resources:GetString("smartBusinessEntityBindingSource1.EntityView":U).
        THIS-OBJECT:smartBusinessEntityBindingSource1:ImportSchema = FALSE.
        THIS-OBJECT:smartBusinessEntityBindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:NoLOBs = FALSE.
        tableDesc1:ChildTables = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Progress.Data.ColumnPropDesc EXTENT 6 NO-UNDO.
        arrayvar0[1] = NEW Progress.Data.ColumnPropDesc("SecurityRealmGuid":U, "SecurityRealmGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[2] = NEW Progress.Data.ColumnPropDesc("SecurityRealmCode":U, "Realm Code":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[3] = NEW Progress.Data.ColumnPropDesc("SecurityRealmDescription":U, "Realm Description":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[4] = NEW Progress.Data.ColumnPropDesc("SecurityRealmService":U, "Security Realm Service":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[5] = NEW Progress.Data.ColumnPropDesc("DefaultRestricted":U, "Default Restricted":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[6] = NEW Progress.Data.ColumnPropDesc("CompanyDependent":U, "Assignment Company Dependent":U, Progress.Data.DataType:LOGICAL).
        tableDesc1:Columns = arrayvar0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:TableSchema = tableDesc1.
        /*  */
        /* eSmartSecurityRealm_SecurityRealmCode_Label */
        /*  */
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmCode_Label:AccessibleDescription = resources:GetString("eSmartSecurityRealm_SecurityRealmCode_Label.AccessibleDescription":U).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmCode_Label:AccessibleName = resources:GetString("eSmartSecurityRealm_SecurityRealmCode_Label.AccessibleName":U).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmCode_Label:Anchor = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmCode_Label.Anchor":U), System.Windows.Forms.AnchorStyles).
        appearance1:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        appearance1:TextHAlignAsString = resources:GetString("appearance1.TextHAlignAsString":U).
        appearance1:TextVAlignAsString = resources:GetString("appearance1.TextVAlignAsString":U).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmCode_Label:Appearance = appearance1.
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmCode_Label:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartSecurityRealm_SecurityRealmCode_Label.AutoSize":U)).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmCode_Label:BackgroundImage = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmCode_Label.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmCode_Label:BackgroundImageLayout = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmCode_Label.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmCode_Label:Dock = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmCode_Label.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmCode_Label:Font = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmCode_Label.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmCode_Label:ImeMode = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmCode_Label.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmCode_Label:Location = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmCode_Label.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmCode_Label:MaximumSize = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmCode_Label.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmCode_Label:Name = "eSmartSecurityRealm_SecurityRealmCode_Label":U.
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmCode_Label:RightToLeft = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmCode_Label.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmCode_Label:Size = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmCode_Label.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmCode_Label:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartSecurityRealm_SecurityRealmCode_Label.TabIndex":U)).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmCode_Label:Text = resources:GetString("eSmartSecurityRealm_SecurityRealmCode_Label.Text":U).
        /*  */
        /* eSmartSecurityRealm_SecurityRealmCode */
        /*  */
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmCode:AccessibleDescription = resources:GetString("eSmartSecurityRealm_SecurityRealmCode.AccessibleDescription":U).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmCode:AccessibleName = resources:GetString("eSmartSecurityRealm_SecurityRealmCode.AccessibleName":U).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmCode:Anchor = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmCode.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmCode:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartSecurityRealm_SecurityRealmCode.AutoSize":U)).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmCode:BackgroundImage = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmCode.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmCode:BackgroundImageLayout = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmCode.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmCode:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "SecurityRealmCode":U, TRUE)).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmCode:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmCode:Dock = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmCode.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmCode:Font = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmCode.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmCode:ImeMode = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmCode.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmCode:Location = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmCode.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmCode:MaximumSize = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmCode.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmCode:Name = "eSmartSecurityRealm_SecurityRealmCode":U.
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmCode:RightToLeft = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmCode.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmCode:Size = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmCode.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmCode:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartSecurityRealm_SecurityRealmCode.TabIndex":U)).
        /*  */
        /* eSmartSecurityRealm_SecurityRealmDescription_Label */
        /*  */
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmDescription_Label:AccessibleDescription = resources:GetString("eSmartSecurityRealm_SecurityRealmDescription_Label.AccessibleDescription":U).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmDescription_Label:AccessibleName = resources:GetString("eSmartSecurityRealm_SecurityRealmDescription_Label.AccessibleName":U).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmDescription_Label:Anchor = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmDescription_Label.Anchor":U), System.Windows.Forms.AnchorStyles).
        appearance2:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        appearance2:TextHAlignAsString = resources:GetString("appearance2.TextHAlignAsString":U).
        appearance2:TextVAlignAsString = resources:GetString("appearance2.TextVAlignAsString":U).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmDescription_Label:Appearance = appearance2.
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmDescription_Label:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartSecurityRealm_SecurityRealmDescription_Label.AutoSize":U)).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmDescription_Label:BackgroundImage = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmDescription_Label.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmDescription_Label:BackgroundImageLayout = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmDescription_Label.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmDescription_Label:Dock = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmDescription_Label.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmDescription_Label:Font = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmDescription_Label.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmDescription_Label:ImeMode = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmDescription_Label.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmDescription_Label:Location = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmDescription_Label.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmDescription_Label:MaximumSize = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmDescription_Label.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmDescription_Label:Name = "eSmartSecurityRealm_SecurityRealmDescription_Label":U.
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmDescription_Label:RightToLeft = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmDescription_Label.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmDescription_Label:Size = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmDescription_Label.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmDescription_Label:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartSecurityRealm_SecurityRealmDescription_Label.TabIndex":U)).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmDescription_Label:Text = resources:GetString("eSmartSecurityRealm_SecurityRealmDescription_Label.Text":U).
        /*  */
        /* eSmartSecurityRealm_SecurityRealmDescription */
        /*  */
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmDescription:AccessibleDescription = resources:GetString("eSmartSecurityRealm_SecurityRealmDescription.AccessibleDescription":U).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmDescription:AccessibleName = resources:GetString("eSmartSecurityRealm_SecurityRealmDescription.AccessibleName":U).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmDescription:Anchor = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmDescription.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmDescription:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartSecurityRealm_SecurityRealmDescription.AutoSize":U)).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmDescription:BackgroundImage = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmDescription.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmDescription:BackgroundImageLayout = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmDescription.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmDescription:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "SecurityRealmDescription":U, TRUE)).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmDescription:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmDescription:Dock = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmDescription.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmDescription:Font = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmDescription.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmDescription:ImeMode = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmDescription.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmDescription:Location = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmDescription.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmDescription:MaximumSize = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmDescription.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmDescription:Name = "eSmartSecurityRealm_SecurityRealmDescription":U.
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmDescription:RightToLeft = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmDescription.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmDescription:Size = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmDescription.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmDescription:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartSecurityRealm_SecurityRealmDescription.TabIndex":U)).
        /*  */
        /* eSmartSecurityRealm_SecurityRealmService_Label */
        /*  */
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmService_Label:AccessibleDescription = resources:GetString("eSmartSecurityRealm_SecurityRealmService_Label.AccessibleDescription":U).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmService_Label:AccessibleName = resources:GetString("eSmartSecurityRealm_SecurityRealmService_Label.AccessibleName":U).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmService_Label:Anchor = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmService_Label.Anchor":U), System.Windows.Forms.AnchorStyles).
        appearance3:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        appearance3:TextHAlignAsString = resources:GetString("appearance3.TextHAlignAsString":U).
        appearance3:TextVAlignAsString = resources:GetString("appearance3.TextVAlignAsString":U).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmService_Label:Appearance = appearance3.
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmService_Label:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartSecurityRealm_SecurityRealmService_Label.AutoSize":U)).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmService_Label:BackgroundImage = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmService_Label.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmService_Label:BackgroundImageLayout = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmService_Label.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmService_Label:Dock = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmService_Label.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmService_Label:Font = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmService_Label.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmService_Label:ImeMode = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmService_Label.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmService_Label:Location = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmService_Label.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmService_Label:MaximumSize = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmService_Label.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmService_Label:Name = "eSmartSecurityRealm_SecurityRealmService_Label":U.
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmService_Label:RightToLeft = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmService_Label.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmService_Label:Size = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmService_Label.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmService_Label:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartSecurityRealm_SecurityRealmService_Label.TabIndex":U)).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmService_Label:Text = resources:GetString("eSmartSecurityRealm_SecurityRealmService_Label.Text":U).
        /*  */
        /* eSmartSecurityRealm_SecurityRealmService */
        /*  */
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmService:AccessibleDescription = resources:GetString("eSmartSecurityRealm_SecurityRealmService.AccessibleDescription":U).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmService:AccessibleName = resources:GetString("eSmartSecurityRealm_SecurityRealmService.AccessibleName":U).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmService:Anchor = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmService.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmService:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartSecurityRealm_SecurityRealmService.AutoSize":U)).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmService:BackgroundImage = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmService.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmService:BackgroundImageLayout = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmService.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        editorButton1:AccessibleDescription = resources:GetString("editorButton1.AccessibleDescription":U).
        editorButton1:AccessibleName = resources:GetString("editorButton1.AccessibleName":U).
        editorButton1:Text = resources:GetString("editorButton1.Text":U).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmService:ButtonsRight:Add(editorButton1).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmService:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "SecurityRealmService":U, TRUE)).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmService:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmService:Dock = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmService.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmService:Font = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmService.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmService:ImeMode = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmService.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmService:Location = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmService.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmService:MaximumSize = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmService.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmService:Name = "eSmartSecurityRealm_SecurityRealmService":U.
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmService:RightToLeft = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmService.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmService:Size = CAST(resources:GetObject("eSmartSecurityRealm_SecurityRealmService.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmService:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartSecurityRealm_SecurityRealmService.TabIndex":U)).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmService:EditorButtonClick:Subscribe(THIS-OBJECT:eSmartSecurityRealm_SecurityRealmService_EditorButtonClick).
        THIS-OBJECT:eSmartSecurityRealm_SecurityRealmService:KeyDown:Subscribe(THIS-OBJECT:LookupKeyDownHandler).
        /*  */
        /* eSmartSecurityRealm_DefaultRestricted */
        /*  */
        THIS-OBJECT:eSmartSecurityRealm_DefaultRestricted:AccessibleDescription = resources:GetString("eSmartSecurityRealm_DefaultRestricted.AccessibleDescription":U).
        THIS-OBJECT:eSmartSecurityRealm_DefaultRestricted:AccessibleName = resources:GetString("eSmartSecurityRealm_DefaultRestricted.AccessibleName":U).
        THIS-OBJECT:eSmartSecurityRealm_DefaultRestricted:Anchor = CAST(resources:GetObject("eSmartSecurityRealm_DefaultRestricted.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartSecurityRealm_DefaultRestricted:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartSecurityRealm_DefaultRestricted.AutoSize":U)).
        THIS-OBJECT:eSmartSecurityRealm_DefaultRestricted:BackgroundImage = CAST(resources:GetObject("eSmartSecurityRealm_DefaultRestricted.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartSecurityRealm_DefaultRestricted:BackgroundImageLayout = CAST(resources:GetObject("eSmartSecurityRealm_DefaultRestricted.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartSecurityRealm_DefaultRestricted:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "DefaultRestricted":U, TRUE)).
        THIS-OBJECT:eSmartSecurityRealm_DefaultRestricted:Dock = CAST(resources:GetObject("eSmartSecurityRealm_DefaultRestricted.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartSecurityRealm_DefaultRestricted:Font = CAST(resources:GetObject("eSmartSecurityRealm_DefaultRestricted.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartSecurityRealm_DefaultRestricted:ImeMode = CAST(resources:GetObject("eSmartSecurityRealm_DefaultRestricted.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartSecurityRealm_DefaultRestricted:Location = CAST(resources:GetObject("eSmartSecurityRealm_DefaultRestricted.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartSecurityRealm_DefaultRestricted:MaximumSize = CAST(resources:GetObject("eSmartSecurityRealm_DefaultRestricted.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartSecurityRealm_DefaultRestricted:Name = "eSmartSecurityRealm_DefaultRestricted":U.
        THIS-OBJECT:eSmartSecurityRealm_DefaultRestricted:RightToLeft = CAST(resources:GetObject("eSmartSecurityRealm_DefaultRestricted.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartSecurityRealm_DefaultRestricted:Size = CAST(resources:GetObject("eSmartSecurityRealm_DefaultRestricted.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartSecurityRealm_DefaultRestricted:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartSecurityRealm_DefaultRestricted.TabIndex":U)).
        THIS-OBJECT:eSmartSecurityRealm_DefaultRestricted:Text = resources:GetString("eSmartSecurityRealm_DefaultRestricted.Text":U).
        /*  */
        /* eSmartSecurityRealm_CompanyDependent */
        /*  */
        THIS-OBJECT:eSmartSecurityRealm_CompanyDependent:AccessibleDescription = resources:GetString("eSmartSecurityRealm_CompanyDependent.AccessibleDescription":U).
        THIS-OBJECT:eSmartSecurityRealm_CompanyDependent:AccessibleName = resources:GetString("eSmartSecurityRealm_CompanyDependent.AccessibleName":U).
        THIS-OBJECT:eSmartSecurityRealm_CompanyDependent:Anchor = CAST(resources:GetObject("eSmartSecurityRealm_CompanyDependent.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartSecurityRealm_CompanyDependent:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartSecurityRealm_CompanyDependent.AutoSize":U)).
        THIS-OBJECT:eSmartSecurityRealm_CompanyDependent:BackgroundImage = CAST(resources:GetObject("eSmartSecurityRealm_CompanyDependent.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartSecurityRealm_CompanyDependent:BackgroundImageLayout = CAST(resources:GetObject("eSmartSecurityRealm_CompanyDependent.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartSecurityRealm_CompanyDependent:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "CompanyDependent":U, TRUE)).
        THIS-OBJECT:eSmartSecurityRealm_CompanyDependent:Dock = CAST(resources:GetObject("eSmartSecurityRealm_CompanyDependent.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartSecurityRealm_CompanyDependent:Font = CAST(resources:GetObject("eSmartSecurityRealm_CompanyDependent.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartSecurityRealm_CompanyDependent:ImeMode = CAST(resources:GetObject("eSmartSecurityRealm_CompanyDependent.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartSecurityRealm_CompanyDependent:Location = CAST(resources:GetObject("eSmartSecurityRealm_CompanyDependent.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartSecurityRealm_CompanyDependent:MaximumSize = CAST(resources:GetObject("eSmartSecurityRealm_CompanyDependent.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartSecurityRealm_CompanyDependent:Name = "eSmartSecurityRealm_CompanyDependent":U.
        THIS-OBJECT:eSmartSecurityRealm_CompanyDependent:RightToLeft = CAST(resources:GetObject("eSmartSecurityRealm_CompanyDependent.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartSecurityRealm_CompanyDependent:Size = CAST(resources:GetObject("eSmartSecurityRealm_CompanyDependent.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartSecurityRealm_CompanyDependent:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartSecurityRealm_CompanyDependent.TabIndex":U)).
        THIS-OBJECT:eSmartSecurityRealm_CompanyDependent:Text = resources:GetString("eSmartSecurityRealm_CompanyDependent.Text":U).
        /*  */
        /* SmartSecurityRealmViewerControl */
        /*  */
        THIS-OBJECT:AccessibleDescription = resources:GetString("$this.AccessibleDescription":U).
        THIS-OBJECT:AccessibleName = resources:GetString("$this.AccessibleName":U).
        THIS-OBJECT:AutoScaleDimensions = CAST(resources:GetObject("$this.AutoScaleDimensions":U), System.Drawing.SizeF).
        THIS-OBJECT:AutoScroll = System.Convert:ToBoolean(resources:GetObject("$this.AutoScroll":U)).
        THIS-OBJECT:AutoSize = System.Convert:ToBoolean(resources:GetObject("$this.AutoSize":U)).
        THIS-OBJECT:AutoSizeMode = CAST(resources:GetObject("$this.AutoSizeMode":U), System.Windows.Forms.AutoSizeMode).
        THIS-OBJECT:BackgroundImage = CAST(resources:GetObject("$this.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:BackgroundImageLayout = CAST(resources:GetObject("$this.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartSecurityRealm_SecurityRealmCode_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartSecurityRealm_SecurityRealmCode).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartSecurityRealm_SecurityRealmDescription_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartSecurityRealm_SecurityRealmDescription).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartSecurityRealm_SecurityRealmService_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartSecurityRealm_SecurityRealmService).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartSecurityRealm_DefaultRestricted).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartSecurityRealm_CompanyDependent).
        THIS-OBJECT:Font = CAST(resources:GetObject("$this.Font":U), System.Drawing.Font).
        THIS-OBJECT:ImeMode = CAST(resources:GetObject("$this.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:Location = CAST(resources:GetObject("$this.Location":U), System.Drawing.Point).
        THIS-OBJECT:MaximumSize = CAST(resources:GetObject("$this.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:Name = "SmartSecurityRealmViewerControl":U.
        THIS-OBJECT:RightToLeft = CAST(resources:GetObject("$this.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:SecurityKey = resources:GetString("$this.SecurityKey":U).
        THIS-OBJECT:Size = CAST(resources:GetObject("$this.Size":U), System.Drawing.Size).
        THIS-OBJECT:SmartTableIOState = resources:GetString("$this.SmartTableIOState":U).
        THIS-OBJECT:ViewerLogicClass = resources:GetString("$this.ViewerLogicClass":U).
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartSecurityRealm_SecurityRealmCode, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartSecurityRealm_SecurityRealmDescription, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartSecurityRealm_SecurityRealmService, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartSecurityRealm_DefaultRestricted, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartSecurityRealm_CompanyDependent, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the KeyDown handler of the Text boxes with an
                 EditorButton (Lookup)
        Notes:   Maps F3 to EditorButtonClick
        @param sender The reference to the object that raised the event
        @param e The KeyEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID LookupKeyDownHandler (sender AS System.Object,
                                              e AS System.Windows.Forms.KeyEventArgs):

        DEFINE VARIABLE cControlName AS CHARACTER NO-UNDO.

        IF e:Handled = FALSE AND Progress.Util.EnumHelper:AreEqual (e:KeyCode, SmartLookupSettings:ShortcutKey)
            AND THIS-OBJECT:Enabled THEN DO:

&IF NOT PROVERSION BEGINS "10" &THEN
            ASSIGN cControlName = CAST (sender, System.Windows.Forms.Control):Name .

            DYNAMIC-INVOKE (THIS-OBJECT,
                            SUBSTITUTE ("&1_EditorButtonClick":U, cControlName),
                            sender,
                            NEW EditorButtonEventArgs (?, ?)) .
&ELSE
            /* Mike Fechner, Consultingwerk Ltd. 17.08.2015
               DYNAMIC-INVOKE seems to crash here on 10.2B */
            CASE sender:
                WHEN eSmartSecurityRealm_SecurityRealmService
                    THEN THIS-OBJECT:eSmartSecurityRealm_SecurityRealmService_EditorButtonClick (sender, NEW EditorButtonEventArgs (?, ?)) .
            END CASE .
&ENDIF
        END.

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SmartSecurityRealmViewerControl class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartSecurityRealmViewerControl ( ):

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
