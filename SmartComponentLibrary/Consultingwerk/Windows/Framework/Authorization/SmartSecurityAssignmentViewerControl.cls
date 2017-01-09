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
    File        : SmartSecurityAssignmentViewerControl
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner
    Created     : 28.07.2013 20:59:39
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.*                      FROM PROPATH .
USING Consultingwerk.Framework.Session.*              FROM PROPATH .
USING Consultingwerk.OERA.*                           FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Authorization.*   FROM PROPATH .
USING Consultingwerk.Util.*                           FROM PROPATH .
USING Progress.Lang.*                                 FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.Windows.Framework.Authorization.SmartSecurityAssignmentViewerControl
    INHERITS SmartViewerControl:

    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.

    DEFINE PRIVATE VARIABLE coSecurityRealm AS Consultingwerk.SmartComponents.Implementation.SmartComboEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSecurityAssignment_SecurityRealmGuid_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSecurityAssignment_Restricted_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSecurityAssignment_Restricted AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE loSecurityItem AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityLookup2 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityLookup1 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel3 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel2 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartSecurityAssignmentViewerControl class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartSecurityAssignmentViewerControl ():

        SUPER().
        InitializeComponent().


    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Assigns the properties of the Lookup Control based on the selected
                 security realm
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID AssignLookupProperties ():

        DEFINE VARIABLE oParameter AS SecurityRealmLookupParameter NO-UNDO .

        ASSIGN oParameter                   = NEW SecurityRealmLookupParameter ()
               oParameter:SecurityRealmGuid = UNBOX (coSecurityRealm:Value) .

        FrameworkSettings:ServiceAdapter:InvokeTask ("":U,
                                                     "Consultingwerk.SmartFramework.Authorization.SecurityRealmInfoService":U,
                                                     "GetSecurityRealmLookupParameter":U,
                                                     oParameter,
                                                     SessionManager:ContextDataset) .

        oParameter:LookupDescriptor:AssignToSmartBusinessEntityLookup (loSecurityItem) .

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
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.Authorization.SmartSecurityAssignmentViewerControl":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc1 = NEW Progress.Data.TableDesc("eSmartSecurityAssignment":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton1 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton1 = NEW Infragistics.Win.UltraWinEditors.EditorButton("ShowLookupDialog":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton2 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton2 = NEW Infragistics.Win.UltraWinEditors.EditorButton("ShowLookupDialog":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton3 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton3 = NEW Infragistics.Win.UltraWinEditors.EditorButton("ShowLookupDialog":U).
        THIS-OBJECT:eSmartSecurityAssignment_Restricted_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartSecurityAssignment_Restricted = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:eSmartSecurityAssignment_SecurityRealmGuid_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:coSecurityRealm = NEW Consultingwerk.SmartComponents.Implementation.SmartComboEditor().
        THIS-OBJECT:loSecurityItem = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityLookup1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup().
        THIS-OBJECT:smartBusinessEntityLookup2 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup().
        THIS-OBJECT:ultraLabel2 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel3 = NEW Infragistics.Win.Misc.UltraLabel().
        CAST(THIS-OBJECT:eSmartSecurityAssignment_Restricted, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:coSecurityRealm, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:loSecurityItem, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityLookup1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityLookup2, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* eSmartSecurityAssignment_Restricted_Label */
        /*  */
        THIS-OBJECT:eSmartSecurityAssignment_Restricted_Label:AccessibleDescription = resources:GetString("eSmartSecurityAssignment_Restricted_Label.AccessibleDescription":U).
        THIS-OBJECT:eSmartSecurityAssignment_Restricted_Label:AccessibleName = resources:GetString("eSmartSecurityAssignment_Restricted_Label.AccessibleName":U).
        THIS-OBJECT:eSmartSecurityAssignment_Restricted_Label:Anchor = CAST(resources:GetObject("eSmartSecurityAssignment_Restricted_Label.Anchor":U), System.Windows.Forms.AnchorStyles).
        appearance1:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        appearance1:TextHAlignAsString = resources:GetString("appearance1.TextHAlignAsString":U).
        appearance1:TextVAlignAsString = resources:GetString("appearance1.TextVAlignAsString":U).
        THIS-OBJECT:eSmartSecurityAssignment_Restricted_Label:Appearance = appearance1.
        THIS-OBJECT:eSmartSecurityAssignment_Restricted_Label:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartSecurityAssignment_Restricted_Label.AutoSize":U)).
        THIS-OBJECT:eSmartSecurityAssignment_Restricted_Label:BackgroundImage = CAST(resources:GetObject("eSmartSecurityAssignment_Restricted_Label.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartSecurityAssignment_Restricted_Label:BackgroundImageLayout = CAST(resources:GetObject("eSmartSecurityAssignment_Restricted_Label.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartSecurityAssignment_Restricted_Label:Dock = CAST(resources:GetObject("eSmartSecurityAssignment_Restricted_Label.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartSecurityAssignment_Restricted_Label:Font = CAST(resources:GetObject("eSmartSecurityAssignment_Restricted_Label.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartSecurityAssignment_Restricted_Label:ImeMode = CAST(resources:GetObject("eSmartSecurityAssignment_Restricted_Label.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartSecurityAssignment_Restricted_Label:Location = CAST(resources:GetObject("eSmartSecurityAssignment_Restricted_Label.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartSecurityAssignment_Restricted_Label:MaximumSize = CAST(resources:GetObject("eSmartSecurityAssignment_Restricted_Label.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartSecurityAssignment_Restricted_Label:Name = "eSmartSecurityAssignment_Restricted_Label":U.
        THIS-OBJECT:eSmartSecurityAssignment_Restricted_Label:RightToLeft = CAST(resources:GetObject("eSmartSecurityAssignment_Restricted_Label.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartSecurityAssignment_Restricted_Label:Size = CAST(resources:GetObject("eSmartSecurityAssignment_Restricted_Label.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartSecurityAssignment_Restricted_Label:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartSecurityAssignment_Restricted_Label.TabIndex":U)).
        THIS-OBJECT:eSmartSecurityAssignment_Restricted_Label:Text = resources:GetString("eSmartSecurityAssignment_Restricted_Label.Text":U).
        /*  */
        /* eSmartSecurityAssignment_Restricted */
        /*  */
        THIS-OBJECT:eSmartSecurityAssignment_Restricted:AccessibleDescription = resources:GetString("eSmartSecurityAssignment_Restricted.AccessibleDescription":U).
        THIS-OBJECT:eSmartSecurityAssignment_Restricted:AccessibleName = resources:GetString("eSmartSecurityAssignment_Restricted.AccessibleName":U).
        THIS-OBJECT:eSmartSecurityAssignment_Restricted:Anchor = CAST(resources:GetObject("eSmartSecurityAssignment_Restricted.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartSecurityAssignment_Restricted:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartSecurityAssignment_Restricted.AutoSize":U)).
        THIS-OBJECT:eSmartSecurityAssignment_Restricted:BackgroundImage = CAST(resources:GetObject("eSmartSecurityAssignment_Restricted.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartSecurityAssignment_Restricted:BackgroundImageLayout = CAST(resources:GetObject("eSmartSecurityAssignment_Restricted.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartSecurityAssignment_Restricted:DataBindings:Add(NEW System.Windows.Forms.Binding("Checked":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "Restricted":U, TRUE)).
        THIS-OBJECT:eSmartSecurityAssignment_Restricted:Dock = CAST(resources:GetObject("eSmartSecurityAssignment_Restricted.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartSecurityAssignment_Restricted:Font = CAST(resources:GetObject("eSmartSecurityAssignment_Restricted.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartSecurityAssignment_Restricted:ImeMode = CAST(resources:GetObject("eSmartSecurityAssignment_Restricted.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartSecurityAssignment_Restricted:Location = CAST(resources:GetObject("eSmartSecurityAssignment_Restricted.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartSecurityAssignment_Restricted:MaximumSize = CAST(resources:GetObject("eSmartSecurityAssignment_Restricted.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartSecurityAssignment_Restricted:Name = "eSmartSecurityAssignment_Restricted":U.
        THIS-OBJECT:eSmartSecurityAssignment_Restricted:RightToLeft = CAST(resources:GetObject("eSmartSecurityAssignment_Restricted.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartSecurityAssignment_Restricted:Size = CAST(resources:GetObject("eSmartSecurityAssignment_Restricted.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartSecurityAssignment_Restricted:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartSecurityAssignment_Restricted.TabIndex":U)).
        THIS-OBJECT:eSmartSecurityAssignment_Restricted:Text = resources:GetString("eSmartSecurityAssignment_Restricted.Text":U).
        THIS-OBJECT:eSmartSecurityAssignment_Restricted:UseMnemonics = TRUE.
        /*  */
        /* smartBusinessEntityBindingSource1 */
        /*  */
        THIS-OBJECT:smartBusinessEntityBindingSource1:BindTo = resources:GetString("smartBusinessEntityBindingSource1.BindTo":U).
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityJoin = resources:GetString("smartBusinessEntityBindingSource1.EntityJoin":U).
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityName = resources:GetString("smartBusinessEntityBindingSource1.EntityName":U).
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityTable = resources:GetString("smartBusinessEntityBindingSource1.EntityTable":U).
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityView = resources:GetString("smartBusinessEntityBindingSource1.EntityView":U).
        THIS-OBJECT:smartBusinessEntityBindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:NoLOBs = FALSE.
        tableDesc1:ChildTables = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Progress.Data.ColumnPropDesc EXTENT 14 NO-UNDO.
        arrayvar0[1] = NEW Progress.Data.ColumnPropDesc("SecurityAssignmentGuid":U, "SecurityAssignmentGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[2] = NEW Progress.Data.ColumnPropDesc("SecurityRealmGuid":U, "SecurityRealmGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[3] = NEW Progress.Data.ColumnPropDesc("SecurityItemGuid":U, "Security Item":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[4] = NEW Progress.Data.ColumnPropDesc("GroupGuid":U, "GroupGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[5] = NEW Progress.Data.ColumnPropDesc("UserGuid":U, "UserGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[6] = NEW Progress.Data.ColumnPropDesc("Restricted":U, "Restricted":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[7] = NEW Progress.Data.ColumnPropDesc("SecurityRealmCode":U, "Realm Code":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[8] = NEW Progress.Data.ColumnPropDesc("SecurityRealmDescription":U, "Realm Description":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[9] = NEW Progress.Data.ColumnPropDesc("SecurityItemDescription":U, "Security Item":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[10] = NEW Progress.Data.ColumnPropDesc("OwnerName":U, "User or Group":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[11] = NEW Progress.Data.ColumnPropDesc("OwnerType":U, "Type":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[12] = NEW Progress.Data.ColumnPropDesc("GroupName":U, "GroupName":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[13] = NEW Progress.Data.ColumnPropDesc("UserName":U, "UserName":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[14] = NEW Progress.Data.ColumnPropDesc("LoginCompanyGuid":U, "LoginCompanyGuid":U, Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:TableSchema = tableDesc1.
        /*  */
        /* eSmartSecurityAssignment_SecurityRealmGuid_Label */
        /*  */
        THIS-OBJECT:eSmartSecurityAssignment_SecurityRealmGuid_Label:AccessibleDescription = resources:GetString("eSmartSecurityAssignment_SecurityRealmGuid_Label.AccessibleDescription":U).
        THIS-OBJECT:eSmartSecurityAssignment_SecurityRealmGuid_Label:AccessibleName = resources:GetString("eSmartSecurityAssignment_SecurityRealmGuid_Label.AccessibleName":U).
        THIS-OBJECT:eSmartSecurityAssignment_SecurityRealmGuid_Label:Anchor = CAST(resources:GetObject("eSmartSecurityAssignment_SecurityRealmGuid_Label.Anchor":U), System.Windows.Forms.AnchorStyles).
        appearance2:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        appearance2:TextHAlignAsString = resources:GetString("appearance2.TextHAlignAsString":U).
        appearance2:TextVAlignAsString = resources:GetString("appearance2.TextVAlignAsString":U).
        THIS-OBJECT:eSmartSecurityAssignment_SecurityRealmGuid_Label:Appearance = appearance2.
        THIS-OBJECT:eSmartSecurityAssignment_SecurityRealmGuid_Label:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartSecurityAssignment_SecurityRealmGuid_Label.AutoSize":U)).
        THIS-OBJECT:eSmartSecurityAssignment_SecurityRealmGuid_Label:BackgroundImage = CAST(resources:GetObject("eSmartSecurityAssignment_SecurityRealmGuid_Label.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartSecurityAssignment_SecurityRealmGuid_Label:BackgroundImageLayout = CAST(resources:GetObject("eSmartSecurityAssignment_SecurityRealmGuid_Label.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartSecurityAssignment_SecurityRealmGuid_Label:Dock = CAST(resources:GetObject("eSmartSecurityAssignment_SecurityRealmGuid_Label.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartSecurityAssignment_SecurityRealmGuid_Label:Font = CAST(resources:GetObject("eSmartSecurityAssignment_SecurityRealmGuid_Label.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartSecurityAssignment_SecurityRealmGuid_Label:ImeMode = CAST(resources:GetObject("eSmartSecurityAssignment_SecurityRealmGuid_Label.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartSecurityAssignment_SecurityRealmGuid_Label:Location = CAST(resources:GetObject("eSmartSecurityAssignment_SecurityRealmGuid_Label.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartSecurityAssignment_SecurityRealmGuid_Label:MaximumSize = CAST(resources:GetObject("eSmartSecurityAssignment_SecurityRealmGuid_Label.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartSecurityAssignment_SecurityRealmGuid_Label:Name = "eSmartSecurityAssignment_SecurityRealmGuid_Label":U.
        THIS-OBJECT:eSmartSecurityAssignment_SecurityRealmGuid_Label:RightToLeft = CAST(resources:GetObject("eSmartSecurityAssignment_SecurityRealmGuid_Label.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartSecurityAssignment_SecurityRealmGuid_Label:Size = CAST(resources:GetObject("eSmartSecurityAssignment_SecurityRealmGuid_Label.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartSecurityAssignment_SecurityRealmGuid_Label:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartSecurityAssignment_SecurityRealmGuid_Label.TabIndex":U)).
        THIS-OBJECT:eSmartSecurityAssignment_SecurityRealmGuid_Label:Text = resources:GetString("eSmartSecurityAssignment_SecurityRealmGuid_Label.Text":U).
        /*  */
        /* coSecurityRealm */
        /*  */
        THIS-OBJECT:coSecurityRealm:AccessibleDescription = resources:GetString("coSecurityRealm.AccessibleDescription":U).
        THIS-OBJECT:coSecurityRealm:AccessibleName = resources:GetString("coSecurityRealm.AccessibleName":U).
        THIS-OBJECT:coSecurityRealm:Anchor = CAST(resources:GetObject("coSecurityRealm.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:coSecurityRealm:AppServerPartition = resources:GetString("coSecurityRealm.AppServerPartition":U).
        THIS-OBJECT:coSecurityRealm:AutoSize = System.Convert:ToBoolean(resources:GetObject("coSecurityRealm.AutoSize":U)).
        THIS-OBJECT:coSecurityRealm:BackgroundImage = CAST(resources:GetObject("coSecurityRealm.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:coSecurityRealm:BackgroundImageLayout = CAST(resources:GetObject("coSecurityRealm.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:coSecurityRealm:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "SecurityRealmGuid":U, TRUE)).
        THIS-OBJECT:coSecurityRealm:DisplayFields = resources:GetString("coSecurityRealm.DisplayFields":U).
        THIS-OBJECT:coSecurityRealm:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:coSecurityRealm:DisplaySubstitute = resources:GetString("coSecurityRealm.DisplaySubstitute":U).
        THIS-OBJECT:coSecurityRealm:Dock = CAST(resources:GetObject("coSecurityRealm.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:coSecurityRealm:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        THIS-OBJECT:coSecurityRealm:EntityName = resources:GetString("coSecurityRealm.EntityName":U).
        THIS-OBJECT:coSecurityRealm:EntityTable = resources:GetString("coSecurityRealm.EntityTable":U).
        THIS-OBJECT:coSecurityRealm:FlagText = resources:GetString("coSecurityRealm.FlagText":U).
        THIS-OBJECT:coSecurityRealm:FlagValue = resources:GetString("coSecurityRealm.FlagValue":U).
        THIS-OBJECT:coSecurityRealm:Font = CAST(resources:GetObject("coSecurityRealm.Font":U), System.Drawing.Font).
        THIS-OBJECT:coSecurityRealm:ImageField = resources:GetString("coSecurityRealm.ImageField":U).
        THIS-OBJECT:coSecurityRealm:ImageKeyField = resources:GetString("coSecurityRealm.ImageKeyField":U).
        THIS-OBJECT:coSecurityRealm:ImageKeyFieldSubstitute = resources:GetString("coSecurityRealm.ImageKeyFieldSubstitute":U).
        THIS-OBJECT:coSecurityRealm:ImeMode = CAST(resources:GetObject("coSecurityRealm.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:coSecurityRealm:KeyField = resources:GetString("coSecurityRealm.KeyField":U).
        THIS-OBJECT:coSecurityRealm:LimitToList = TRUE.
        THIS-OBJECT:coSecurityRealm:Location = CAST(resources:GetObject("coSecurityRealm.Location":U), System.Drawing.Point).
        THIS-OBJECT:coSecurityRealm:MaximumSize = CAST(resources:GetObject("coSecurityRealm.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:coSecurityRealm:Name = "coSecurityRealm":U.
        THIS-OBJECT:coSecurityRealm:QueryString = resources:GetString("coSecurityRealm.QueryString":U).
        THIS-OBJECT:coSecurityRealm:RightToLeft = CAST(resources:GetObject("coSecurityRealm.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:coSecurityRealm:Size = CAST(resources:GetObject("coSecurityRealm.Size":U), System.Drawing.Size).
        THIS-OBJECT:coSecurityRealm:TabIndex = System.Convert:ToInt32(resources:GetObject("coSecurityRealm.TabIndex":U)).
        /*  */
        /* loSecurityItem */
        /*  */
        THIS-OBJECT:loSecurityItem:AccessibleDescription = resources:GetString("loSecurityItem.AccessibleDescription":U).
        THIS-OBJECT:loSecurityItem:AccessibleName = resources:GetString("loSecurityItem.AccessibleName":U).
        THIS-OBJECT:loSecurityItem:Anchor = CAST(resources:GetObject("loSecurityItem.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:loSecurityItem:AppServerPartition = resources:GetString("loSecurityItem.AppServerPartition":U).
        THIS-OBJECT:loSecurityItem:AutoSize = System.Convert:ToBoolean(resources:GetObject("loSecurityItem.AutoSize":U)).
        THIS-OBJECT:loSecurityItem:BackgroundImage = CAST(resources:GetObject("loSecurityItem.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:loSecurityItem:BackgroundImageLayout = CAST(resources:GetObject("loSecurityItem.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        editorButton1:AccessibleDescription = resources:GetString("editorButton1.AccessibleDescription":U).
        editorButton1:AccessibleName = resources:GetString("editorButton1.AccessibleName":U).
        editorButton1:Key = "ShowLookupDialog":U.
        editorButton1:Text = resources:GetString("editorButton1.Text":U).
        THIS-OBJECT:loSecurityItem:ButtonsRight:Add(editorButton1).
        THIS-OBJECT:loSecurityItem:DataBindings:Add(NEW System.Windows.Forms.Binding("LookupKeyValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "SecurityItemGuid":U, TRUE)).
        THIS-OBJECT:loSecurityItem:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "SecurityItemDescription":U, TRUE)).
        THIS-OBJECT:loSecurityItem:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:loSecurityItem:Dock = CAST(resources:GetObject("loSecurityItem.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:loSecurityItem:Font = CAST(resources:GetObject("loSecurityItem.Font":U), System.Drawing.Font).
        THIS-OBJECT:loSecurityItem:ImeMode = CAST(resources:GetObject("loSecurityItem.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:loSecurityItem:Location = CAST(resources:GetObject("loSecurityItem.Location":U), System.Drawing.Point).
        THIS-OBJECT:loSecurityItem:LookupAdditionalFields = resources:GetString("loSecurityItem.LookupAdditionalFields":U).
        THIS-OBJECT:loSecurityItem:LookupBrowserExcludeFields = resources:GetString("loSecurityItem.LookupBrowserExcludeFields":U).
        THIS-OBJECT:loSecurityItem:LookupBrowserFields = resources:GetString("loSecurityItem.LookupBrowserFields":U).
        THIS-OBJECT:loSecurityItem:LookupControls = resources:GetString("loSecurityItem.LookupControls":U).
        THIS-OBJECT:loSecurityItem:LookupDialogClassName = resources:GetString("loSecurityItem.LookupDialogClassName":U).
        THIS-OBJECT:loSecurityItem:LookupDialogFilterFields = resources:GetString("loSecurityItem.LookupDialogFilterFields":U).
        THIS-OBJECT:loSecurityItem:LookupDialogFilterOperators = resources:GetString("loSecurityItem.LookupDialogFilterOperators":U).
        THIS-OBJECT:loSecurityItem:LookupDialogQuerySort = resources:GetString("loSecurityItem.LookupDialogQuerySort":U).
        THIS-OBJECT:loSecurityItem:LookupDialogQueryString = resources:GetString("loSecurityItem.LookupDialogQueryString":U).
        THIS-OBJECT:loSecurityItem:LookupDialogTitle = resources:GetString("loSecurityItem.LookupDialogTitle":U).
        THIS-OBJECT:loSecurityItem:LookupEntityName = resources:GetString("loSecurityItem.LookupEntityName":U).
        THIS-OBJECT:loSecurityItem:LookupEntityTable = resources:GetString("loSecurityItem.LookupEntityTable":U).
        THIS-OBJECT:loSecurityItem:LookupEntityView = resources:GetString("loSecurityItem.LookupEntityView":U).
        THIS-OBJECT:loSecurityItem:LookupFields = resources:GetString("loSecurityItem.LookupFields":U).
        THIS-OBJECT:loSecurityItem:LookupKeyField = resources:GetString("loSecurityItem.LookupKeyField":U).
        THIS-OBJECT:loSecurityItem:LookupKeyValue = resources:GetString("loSecurityItem.LookupKeyValue":U).
        THIS-OBJECT:loSecurityItem:LookupKeyValueColumn = resources:GetString("loSecurityItem.LookupKeyValueColumn":U).
        THIS-OBJECT:loSecurityItem:LookupQuerySort = resources:GetString("loSecurityItem.LookupQuerySort":U).
        THIS-OBJECT:loSecurityItem:LookupQueryString = resources:GetString("loSecurityItem.LookupQueryString":U).
        THIS-OBJECT:loSecurityItem:MaximumSize = CAST(resources:GetObject("loSecurityItem.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:loSecurityItem:Name = "loSecurityItem":U.
        THIS-OBJECT:loSecurityItem:RightToLeft = CAST(resources:GetObject("loSecurityItem.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:loSecurityItem:Size = CAST(resources:GetObject("loSecurityItem.Size":U), System.Drawing.Size).
        THIS-OBJECT:loSecurityItem:TabIndex = System.Convert:ToInt32(resources:GetObject("loSecurityItem.TabIndex":U)).
        THIS-OBJECT:loSecurityItem:BeforePerformLookup:Subscribe(THIS-OBJECT:loSecurityItem_BeforePerformLookup).
        THIS-OBJECT:loSecurityItem:ShowLookupDialog:Subscribe(THIS-OBJECT:loSecurityItem_ShowLookupDialog).
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
        /* smartBusinessEntityLookup1 */
        /*  */
        THIS-OBJECT:smartBusinessEntityLookup1:AccessibleDescription = resources:GetString("smartBusinessEntityLookup1.AccessibleDescription":U).
        THIS-OBJECT:smartBusinessEntityLookup1:AccessibleName = resources:GetString("smartBusinessEntityLookup1.AccessibleName":U).
        THIS-OBJECT:smartBusinessEntityLookup1:Anchor = CAST(resources:GetObject("smartBusinessEntityLookup1.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityLookup1:AppServerPartition = resources:GetString("smartBusinessEntityLookup1.AppServerPartition":U).
        THIS-OBJECT:smartBusinessEntityLookup1:AutoSize = System.Convert:ToBoolean(resources:GetObject("smartBusinessEntityLookup1.AutoSize":U)).
        THIS-OBJECT:smartBusinessEntityLookup1:BackgroundImage = CAST(resources:GetObject("smartBusinessEntityLookup1.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:smartBusinessEntityLookup1:BackgroundImageLayout = CAST(resources:GetObject("smartBusinessEntityLookup1.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        editorButton2:AccessibleDescription = resources:GetString("editorButton2.AccessibleDescription":U).
        editorButton2:AccessibleName = resources:GetString("editorButton2.AccessibleName":U).
        editorButton2:Key = "ShowLookupDialog":U.
        editorButton2:Text = resources:GetString("editorButton2.Text":U).
        THIS-OBJECT:smartBusinessEntityLookup1:ButtonsRight:Add(editorButton2).
        THIS-OBJECT:smartBusinessEntityLookup1:DataBindings:Add(NEW System.Windows.Forms.Binding("LookupKeyValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "UserGuid":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityLookup1:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "UserName":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityLookup1:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
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
        /*  */
        /* smartBusinessEntityLookup2 */
        /*  */
        THIS-OBJECT:smartBusinessEntityLookup2:AccessibleDescription = resources:GetString("smartBusinessEntityLookup2.AccessibleDescription":U).
        THIS-OBJECT:smartBusinessEntityLookup2:AccessibleName = resources:GetString("smartBusinessEntityLookup2.AccessibleName":U).
        THIS-OBJECT:smartBusinessEntityLookup2:Anchor = CAST(resources:GetObject("smartBusinessEntityLookup2.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityLookup2:AppServerPartition = resources:GetString("smartBusinessEntityLookup2.AppServerPartition":U).
        THIS-OBJECT:smartBusinessEntityLookup2:AutoSize = System.Convert:ToBoolean(resources:GetObject("smartBusinessEntityLookup2.AutoSize":U)).
        THIS-OBJECT:smartBusinessEntityLookup2:BackgroundImage = CAST(resources:GetObject("smartBusinessEntityLookup2.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:smartBusinessEntityLookup2:BackgroundImageLayout = CAST(resources:GetObject("smartBusinessEntityLookup2.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        editorButton3:AccessibleDescription = resources:GetString("editorButton3.AccessibleDescription":U).
        editorButton3:AccessibleName = resources:GetString("editorButton3.AccessibleName":U).
        editorButton3:Key = "ShowLookupDialog":U.
        editorButton3:Text = resources:GetString("editorButton3.Text":U).
        THIS-OBJECT:smartBusinessEntityLookup2:ButtonsRight:Add(editorButton3).
        THIS-OBJECT:smartBusinessEntityLookup2:DataBindings:Add(NEW System.Windows.Forms.Binding("LookupKeyValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "GroupGuid":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityLookup2:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "GroupName":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityLookup2:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:smartBusinessEntityLookup2:Dock = CAST(resources:GetObject("smartBusinessEntityLookup2.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:smartBusinessEntityLookup2:Font = CAST(resources:GetObject("smartBusinessEntityLookup2.Font":U), System.Drawing.Font).
        THIS-OBJECT:smartBusinessEntityLookup2:ImeMode = CAST(resources:GetObject("smartBusinessEntityLookup2.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:smartBusinessEntityLookup2:Location = CAST(resources:GetObject("smartBusinessEntityLookup2.Location":U), System.Drawing.Point).
        THIS-OBJECT:smartBusinessEntityLookup2:LookupAdditionalFields = resources:GetString("smartBusinessEntityLookup2.LookupAdditionalFields":U).
        THIS-OBJECT:smartBusinessEntityLookup2:LookupBrowserExcludeFields = resources:GetString("smartBusinessEntityLookup2.LookupBrowserExcludeFields":U).
        THIS-OBJECT:smartBusinessEntityLookup2:LookupBrowserFields = resources:GetString("smartBusinessEntityLookup2.LookupBrowserFields":U).
        THIS-OBJECT:smartBusinessEntityLookup2:LookupControls = resources:GetString("smartBusinessEntityLookup2.LookupControls":U).
        THIS-OBJECT:smartBusinessEntityLookup2:LookupDialogClassName = resources:GetString("smartBusinessEntityLookup2.LookupDialogClassName":U).
        THIS-OBJECT:smartBusinessEntityLookup2:LookupDialogFilterFields = resources:GetString("smartBusinessEntityLookup2.LookupDialogFilterFields":U).
        THIS-OBJECT:smartBusinessEntityLookup2:LookupDialogFilterOperators = resources:GetString("smartBusinessEntityLookup2.LookupDialogFilterOperators":U).
        THIS-OBJECT:smartBusinessEntityLookup2:LookupDialogQuerySort = resources:GetString("smartBusinessEntityLookup2.LookupDialogQuerySort":U).
        THIS-OBJECT:smartBusinessEntityLookup2:LookupDialogQueryString = resources:GetString("smartBusinessEntityLookup2.LookupDialogQueryString":U).
        THIS-OBJECT:smartBusinessEntityLookup2:LookupDialogTitle = resources:GetString("smartBusinessEntityLookup2.LookupDialogTitle":U).
        THIS-OBJECT:smartBusinessEntityLookup2:LookupEntityName = resources:GetString("smartBusinessEntityLookup2.LookupEntityName":U).
        THIS-OBJECT:smartBusinessEntityLookup2:LookupEntityTable = resources:GetString("smartBusinessEntityLookup2.LookupEntityTable":U).
        THIS-OBJECT:smartBusinessEntityLookup2:LookupEntityView = resources:GetString("smartBusinessEntityLookup2.LookupEntityView":U).
        THIS-OBJECT:smartBusinessEntityLookup2:LookupFields = resources:GetString("smartBusinessEntityLookup2.LookupFields":U).
        THIS-OBJECT:smartBusinessEntityLookup2:LookupKeyField = resources:GetString("smartBusinessEntityLookup2.LookupKeyField":U).
        THIS-OBJECT:smartBusinessEntityLookup2:LookupKeyValue = resources:GetString("smartBusinessEntityLookup2.LookupKeyValue":U).
        THIS-OBJECT:smartBusinessEntityLookup2:LookupKeyValueColumn = resources:GetString("smartBusinessEntityLookup2.LookupKeyValueColumn":U).
        THIS-OBJECT:smartBusinessEntityLookup2:LookupQuerySort = resources:GetString("smartBusinessEntityLookup2.LookupQuerySort":U).
        THIS-OBJECT:smartBusinessEntityLookup2:LookupQueryString = resources:GetString("smartBusinessEntityLookup2.LookupQueryString":U).
        THIS-OBJECT:smartBusinessEntityLookup2:MaximumSize = CAST(resources:GetObject("smartBusinessEntityLookup2.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:smartBusinessEntityLookup2:Name = "smartBusinessEntityLookup2":U.
        THIS-OBJECT:smartBusinessEntityLookup2:RightToLeft = CAST(resources:GetObject("smartBusinessEntityLookup2.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:smartBusinessEntityLookup2:Size = CAST(resources:GetObject("smartBusinessEntityLookup2.Size":U), System.Drawing.Size).
        THIS-OBJECT:smartBusinessEntityLookup2:TabIndex = System.Convert:ToInt32(resources:GetObject("smartBusinessEntityLookup2.TabIndex":U)).
        /*  */
        /* ultraLabel2 */
        /*  */
        THIS-OBJECT:ultraLabel2:AccessibleDescription = resources:GetString("ultraLabel2.AccessibleDescription":U).
        THIS-OBJECT:ultraLabel2:AccessibleName = resources:GetString("ultraLabel2.AccessibleName":U).
        THIS-OBJECT:ultraLabel2:Anchor = CAST(resources:GetObject("ultraLabel2.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraLabel2:AutoSize = System.Convert:ToBoolean(resources:GetObject("ultraLabel2.AutoSize":U)).
        THIS-OBJECT:ultraLabel2:BackgroundImage = CAST(resources:GetObject("ultraLabel2.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:ultraLabel2:BackgroundImageLayout = CAST(resources:GetObject("ultraLabel2.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:ultraLabel2:Dock = CAST(resources:GetObject("ultraLabel2.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:ultraLabel2:Font = CAST(resources:GetObject("ultraLabel2.Font":U), System.Drawing.Font).
        THIS-OBJECT:ultraLabel2:ImeMode = CAST(resources:GetObject("ultraLabel2.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:ultraLabel2:Location = CAST(resources:GetObject("ultraLabel2.Location":U), System.Drawing.Point).
        THIS-OBJECT:ultraLabel2:MaximumSize = CAST(resources:GetObject("ultraLabel2.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:ultraLabel2:Name = "ultraLabel2":U.
        THIS-OBJECT:ultraLabel2:RightToLeft = CAST(resources:GetObject("ultraLabel2.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:ultraLabel2:Size = CAST(resources:GetObject("ultraLabel2.Size":U), System.Drawing.Size).
        THIS-OBJECT:ultraLabel2:TabIndex = System.Convert:ToInt32(resources:GetObject("ultraLabel2.TabIndex":U)).
        THIS-OBJECT:ultraLabel2:Text = resources:GetString("ultraLabel2.Text":U).
        /*  */
        /* ultraLabel3 */
        /*  */
        THIS-OBJECT:ultraLabel3:AccessibleDescription = resources:GetString("ultraLabel3.AccessibleDescription":U).
        THIS-OBJECT:ultraLabel3:AccessibleName = resources:GetString("ultraLabel3.AccessibleName":U).
        THIS-OBJECT:ultraLabel3:Anchor = CAST(resources:GetObject("ultraLabel3.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraLabel3:AutoSize = System.Convert:ToBoolean(resources:GetObject("ultraLabel3.AutoSize":U)).
        THIS-OBJECT:ultraLabel3:BackgroundImage = CAST(resources:GetObject("ultraLabel3.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:ultraLabel3:BackgroundImageLayout = CAST(resources:GetObject("ultraLabel3.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:ultraLabel3:Dock = CAST(resources:GetObject("ultraLabel3.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:ultraLabel3:Font = CAST(resources:GetObject("ultraLabel3.Font":U), System.Drawing.Font).
        THIS-OBJECT:ultraLabel3:ImeMode = CAST(resources:GetObject("ultraLabel3.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:ultraLabel3:Location = CAST(resources:GetObject("ultraLabel3.Location":U), System.Drawing.Point).
        THIS-OBJECT:ultraLabel3:MaximumSize = CAST(resources:GetObject("ultraLabel3.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:ultraLabel3:Name = "ultraLabel3":U.
        THIS-OBJECT:ultraLabel3:RightToLeft = CAST(resources:GetObject("ultraLabel3.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:ultraLabel3:Size = CAST(resources:GetObject("ultraLabel3.Size":U), System.Drawing.Size).
        THIS-OBJECT:ultraLabel3:TabIndex = System.Convert:ToInt32(resources:GetObject("ultraLabel3.TabIndex":U)).
        THIS-OBJECT:ultraLabel3:Text = resources:GetString("ultraLabel3.Text":U).
        /*  */
        /* SmartSecurityAssignmentViewerControl */
        /*  */
        THIS-OBJECT:AccessibleDescription = resources:GetString("$this.AccessibleDescription":U).
        THIS-OBJECT:AccessibleName = resources:GetString("$this.AccessibleName":U).
        THIS-OBJECT:AutoScaleDimensions = CAST(resources:GetObject("$this.AutoScaleDimensions":U), System.Drawing.SizeF).
        THIS-OBJECT:AutoScroll = System.Convert:ToBoolean(resources:GetObject("$this.AutoScroll":U)).
        THIS-OBJECT:AutoSize = System.Convert:ToBoolean(resources:GetObject("$this.AutoSize":U)).
        THIS-OBJECT:AutoSizeMode = CAST(resources:GetObject("$this.AutoSizeMode":U), System.Windows.Forms.AutoSizeMode).
        THIS-OBJECT:BackgroundImage = CAST(resources:GetObject("$this.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:BackgroundImageLayout = CAST(resources:GetObject("$this.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel3).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel2).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:smartBusinessEntityLookup2).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:smartBusinessEntityLookup1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:loSecurityItem).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:coSecurityRealm).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartSecurityAssignment_Restricted_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartSecurityAssignment_Restricted).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartSecurityAssignment_SecurityRealmGuid_Label).
        THIS-OBJECT:Font = CAST(resources:GetObject("$this.Font":U), System.Drawing.Font).
        THIS-OBJECT:ImeMode = CAST(resources:GetObject("$this.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:Location = CAST(resources:GetObject("$this.Location":U), System.Drawing.Point).
        THIS-OBJECT:MaximumSize = CAST(resources:GetObject("$this.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:Name = "SmartSecurityAssignmentViewerControl":U.
        THIS-OBJECT:RightToLeft = CAST(resources:GetObject("$this.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:SecurityKey = resources:GetString("$this.SecurityKey":U).
        THIS-OBJECT:Size = CAST(resources:GetObject("$this.Size":U), System.Drawing.Size).
        THIS-OBJECT:SmartTableIOState = resources:GetString("$this.SmartTableIOState":U).
        THIS-OBJECT:ViewerLogicClass = resources:GetString("$this.ViewerLogicClass":U).
        CAST(THIS-OBJECT:eSmartSecurityAssignment_Restricted, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:coSecurityRealm, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:loSecurityItem, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityLookup1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityLookup2, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the BeforePerformLookup event of the loSecurityItem
        Notes:
        @param sender The reference to the object that raised the event
        @param e The CancelEventHandler with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID loSecurityItem_BeforePerformLookup (sender AS System.Object,
                                                            e AS System.ComponentModel.CancelEventArgs):

        IF NOT VALID-OBJECT (coSecurityRealm:SelectedItem) THEN DO:
            MessageFormHelper:ShowMessage ("Please select a security realm first."{&TRAN},
                                           THIS-OBJECT:FindForm ():Text,
                                           Consultingwerk.Windows.Util.Forms.MessageFormImages:ImageError) .
            e:Cancel = TRUE .
            RETURN .
        END .

        THIS-OBJECT:AssignLookupProperties () .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ShowLookupDialog event of the loSecurityItem
        Notes:
        @param sender The reference to the object that raised the event
        @param e The CancelEventHandler with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID loSecurityItem_ShowLookupDialog (sender AS System.Object,
                                                         e AS System.ComponentModel.CancelEventArgs):

        IF NOT VALID-OBJECT (coSecurityRealm:SelectedItem) THEN DO:
            MessageFormHelper:ShowMessage ("Please select a security realm first."{&TRAN},
                                           THIS-OBJECT:FindForm ():Text,
                                           Consultingwerk.Windows.Util.Forms.MessageFormImages:ImageError) .
            e:Cancel = TRUE .
            RETURN .
        END .

        THIS-OBJECT:AssignLookupProperties () .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the Load event
        Notes:
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnLoad (e AS System.EventArgs):

        SUPER:OnLoad (e) .

        IF SessionManager:LoginCompanyKey > "":U THEN DO:

            ASSIGN smartBusinessEntityLookup1:LookupQueryString = SUBSTITUTE
                ("for each eSmartUser where eSmartUser.UserName begins ~"&&1~" and eSmartUser.LoginCompanyGuid = &1 ~
                                         or eSmartUser.UserName begins ~"&&1~" and eSmartUser.LoginCompanyGuid = &2":U,
                  QUOTER (SessionManager:LoginCompanyKey),
                  QUOTER ("":U)) .

            ASSIGN smartBusinessEntityLookup1:LookupDialogQueryString = SUBSTITUTE
                ("for each eSmartUser where eSmartUser.LoginCompanyGuid = &1 ~
                                         or eSmartUser.LoginCompanyGuid = &2":U,
                  QUOTER (SessionManager:LoginCompanyKey),
                  QUOTER ("":U)) .
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SmartSecurityAssignmentViewerControl class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartSecurityAssignmentViewerControl ( ):

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
