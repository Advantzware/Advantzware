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
    File        : SmartFieldSecurityItemViewerControl
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 18.08.2014 11:00:02
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Base.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.Util.*                           FROM PROPATH .
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.Authorization.SmartFieldSecurityItemViewerControl
    INHERITS SmartViewerControl:

    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.

    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartFieldSecurityItem_FieldName_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartFieldSecurityItem_FieldName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartFieldSecurityItem_SecurityKey_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartFieldSecurityItem_SecurityKey AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartFieldSecurityItem_ObjectName_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartFieldSecurityItem_ObjectName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartFieldSecurityItem_Description_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartFieldSecurityItem_Description AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartFieldSecurityItemViewerControl class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartFieldSecurityItemViewerControl ():

        SUPER().
        InitializeComponent().

        &IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN
        THIS-OBJECT:ComponentsCollection:Add (THIS-OBJECT:components).
        &ENDIF

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
        DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc1 = NEW Progress.Data.TableDesc("eSmartFieldSecurityItem":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.Authorization.SmartFieldSecurityItemViewerControl":U).
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
        DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
        appearance4 = NEW Infragistics.Win.Appearance().
        THIS-OBJECT:smartBusinessEntityBindingSource1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:eSmartFieldSecurityItem_FieldName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartFieldSecurityItem_FieldName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartFieldSecurityItem_SecurityKey_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartFieldSecurityItem_SecurityKey = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartFieldSecurityItem_ObjectName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartFieldSecurityItem_ObjectName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartFieldSecurityItem_Description_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartFieldSecurityItem_Description = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartFieldSecurityItem_FieldName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartFieldSecurityItem_SecurityKey, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartFieldSecurityItem_ObjectName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartFieldSecurityItem_Description, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* smartBusinessEntityBindingSource1 */
        /*  */
        THIS-OBJECT:smartBusinessEntityBindingSource1:AddFields = FALSE.
        THIS-OBJECT:smartBusinessEntityBindingSource1:BindTo = "QUERY":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:ClearSchema = FALSE.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityName = "Consultingwerk.SmartFramework.Authorization.FieldSecurityItemBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityTable = "eSmartFieldSecurityItem":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:ImportSchema = FALSE.
        THIS-OBJECT:smartBusinessEntityBindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:NoLOBs = FALSE.
        tableDesc1:ChildTables = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Progress.Data.ColumnPropDesc EXTENT 5 NO-UNDO.
        arrayvar0[1] = NEW Progress.Data.ColumnPropDesc("FieldSecurityItemGuid":U, "FieldSecurityItemGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[2] = NEW Progress.Data.ColumnPropDesc("FieldName":U, "Field Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[3] = NEW Progress.Data.ColumnPropDesc("SecurityKey":U, "Security Key":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[4] = NEW Progress.Data.ColumnPropDesc("ObjectName":U, "Object Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[5] = NEW Progress.Data.ColumnPropDesc("Description":U, "Description":U, Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:TableSchema = tableDesc1.
        /*  */
        /* eSmartFieldSecurityItem_FieldName_Label */
        /*  */
        THIS-OBJECT:eSmartFieldSecurityItem_FieldName_Label:AccessibleDescription = resources:GetString("eSmartFieldSecurityItem_FieldName_Label.AccessibleDescription":U).
        THIS-OBJECT:eSmartFieldSecurityItem_FieldName_Label:AccessibleName = resources:GetString("eSmartFieldSecurityItem_FieldName_Label.AccessibleName":U).
        THIS-OBJECT:eSmartFieldSecurityItem_FieldName_Label:Anchor = CAST(resources:GetObject("eSmartFieldSecurityItem_FieldName_Label.Anchor":U), System.Windows.Forms.AnchorStyles).
        appearance1:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        appearance1:TextHAlignAsString = resources:GetString("appearance1.TextHAlignAsString":U).
        appearance1:TextVAlignAsString = resources:GetString("appearance1.TextVAlignAsString":U).
        THIS-OBJECT:eSmartFieldSecurityItem_FieldName_Label:Appearance = appearance1.
        THIS-OBJECT:eSmartFieldSecurityItem_FieldName_Label:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartFieldSecurityItem_FieldName_Label.AutoSize":U)).
        THIS-OBJECT:eSmartFieldSecurityItem_FieldName_Label:BackgroundImage = CAST(resources:GetObject("eSmartFieldSecurityItem_FieldName_Label.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartFieldSecurityItem_FieldName_Label:BackgroundImageLayout = CAST(resources:GetObject("eSmartFieldSecurityItem_FieldName_Label.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartFieldSecurityItem_FieldName_Label:Dock = CAST(resources:GetObject("eSmartFieldSecurityItem_FieldName_Label.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartFieldSecurityItem_FieldName_Label:Font = CAST(resources:GetObject("eSmartFieldSecurityItem_FieldName_Label.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartFieldSecurityItem_FieldName_Label:ImeMode = CAST(resources:GetObject("eSmartFieldSecurityItem_FieldName_Label.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartFieldSecurityItem_FieldName_Label:Location = CAST(resources:GetObject("eSmartFieldSecurityItem_FieldName_Label.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartFieldSecurityItem_FieldName_Label:MaximumSize = CAST(resources:GetObject("eSmartFieldSecurityItem_FieldName_Label.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartFieldSecurityItem_FieldName_Label:Name = "eSmartFieldSecurityItem_FieldName_Label":U.
        THIS-OBJECT:eSmartFieldSecurityItem_FieldName_Label:RightToLeft = CAST(resources:GetObject("eSmartFieldSecurityItem_FieldName_Label.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartFieldSecurityItem_FieldName_Label:Size = CAST(resources:GetObject("eSmartFieldSecurityItem_FieldName_Label.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartFieldSecurityItem_FieldName_Label:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartFieldSecurityItem_FieldName_Label.TabIndex":U)).
        THIS-OBJECT:eSmartFieldSecurityItem_FieldName_Label:Text = resources:GetString("eSmartFieldSecurityItem_FieldName_Label.Text":U).
        /*  */
        /* eSmartFieldSecurityItem_FieldName */
        /*  */
        THIS-OBJECT:eSmartFieldSecurityItem_FieldName:AccessibleDescription = resources:GetString("eSmartFieldSecurityItem_FieldName.AccessibleDescription":U).
        THIS-OBJECT:eSmartFieldSecurityItem_FieldName:AccessibleName = resources:GetString("eSmartFieldSecurityItem_FieldName.AccessibleName":U).
        THIS-OBJECT:eSmartFieldSecurityItem_FieldName:Anchor = CAST(resources:GetObject("eSmartFieldSecurityItem_FieldName.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartFieldSecurityItem_FieldName:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartFieldSecurityItem_FieldName.AutoSize":U)).
        THIS-OBJECT:eSmartFieldSecurityItem_FieldName:BackgroundImage = CAST(resources:GetObject("eSmartFieldSecurityItem_FieldName.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartFieldSecurityItem_FieldName:BackgroundImageLayout = CAST(resources:GetObject("eSmartFieldSecurityItem_FieldName.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartFieldSecurityItem_FieldName:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "FieldName":U, TRUE)).
        THIS-OBJECT:eSmartFieldSecurityItem_FieldName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartFieldSecurityItem_FieldName:Dock = CAST(resources:GetObject("eSmartFieldSecurityItem_FieldName.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartFieldSecurityItem_FieldName:Font = CAST(resources:GetObject("eSmartFieldSecurityItem_FieldName.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartFieldSecurityItem_FieldName:ImeMode = CAST(resources:GetObject("eSmartFieldSecurityItem_FieldName.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartFieldSecurityItem_FieldName:Location = CAST(resources:GetObject("eSmartFieldSecurityItem_FieldName.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartFieldSecurityItem_FieldName:MaximumSize = CAST(resources:GetObject("eSmartFieldSecurityItem_FieldName.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartFieldSecurityItem_FieldName:Name = "eSmartFieldSecurityItem_FieldName":U.
        THIS-OBJECT:eSmartFieldSecurityItem_FieldName:RightToLeft = CAST(resources:GetObject("eSmartFieldSecurityItem_FieldName.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartFieldSecurityItem_FieldName:Size = CAST(resources:GetObject("eSmartFieldSecurityItem_FieldName.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartFieldSecurityItem_FieldName:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartFieldSecurityItem_FieldName.TabIndex":U)).
        /*  */
        /* eSmartFieldSecurityItem_SecurityKey_Label */
        /*  */
        THIS-OBJECT:eSmartFieldSecurityItem_SecurityKey_Label:AccessibleDescription = resources:GetString("eSmartFieldSecurityItem_SecurityKey_Label.AccessibleDescription":U).
        THIS-OBJECT:eSmartFieldSecurityItem_SecurityKey_Label:AccessibleName = resources:GetString("eSmartFieldSecurityItem_SecurityKey_Label.AccessibleName":U).
        THIS-OBJECT:eSmartFieldSecurityItem_SecurityKey_Label:Anchor = CAST(resources:GetObject("eSmartFieldSecurityItem_SecurityKey_Label.Anchor":U), System.Windows.Forms.AnchorStyles).
        appearance2:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        appearance2:TextHAlignAsString = resources:GetString("appearance2.TextHAlignAsString":U).
        appearance2:TextVAlignAsString = resources:GetString("appearance2.TextVAlignAsString":U).
        THIS-OBJECT:eSmartFieldSecurityItem_SecurityKey_Label:Appearance = appearance2.
        THIS-OBJECT:eSmartFieldSecurityItem_SecurityKey_Label:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartFieldSecurityItem_SecurityKey_Label.AutoSize":U)).
        THIS-OBJECT:eSmartFieldSecurityItem_SecurityKey_Label:BackgroundImage = CAST(resources:GetObject("eSmartFieldSecurityItem_SecurityKey_Label.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartFieldSecurityItem_SecurityKey_Label:BackgroundImageLayout = CAST(resources:GetObject("eSmartFieldSecurityItem_SecurityKey_Label.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartFieldSecurityItem_SecurityKey_Label:Dock = CAST(resources:GetObject("eSmartFieldSecurityItem_SecurityKey_Label.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartFieldSecurityItem_SecurityKey_Label:Font = CAST(resources:GetObject("eSmartFieldSecurityItem_SecurityKey_Label.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartFieldSecurityItem_SecurityKey_Label:ImeMode = CAST(resources:GetObject("eSmartFieldSecurityItem_SecurityKey_Label.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartFieldSecurityItem_SecurityKey_Label:Location = CAST(resources:GetObject("eSmartFieldSecurityItem_SecurityKey_Label.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartFieldSecurityItem_SecurityKey_Label:MaximumSize = CAST(resources:GetObject("eSmartFieldSecurityItem_SecurityKey_Label.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartFieldSecurityItem_SecurityKey_Label:Name = "eSmartFieldSecurityItem_SecurityKey_Label":U.
        THIS-OBJECT:eSmartFieldSecurityItem_SecurityKey_Label:RightToLeft = CAST(resources:GetObject("eSmartFieldSecurityItem_SecurityKey_Label.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartFieldSecurityItem_SecurityKey_Label:Size = CAST(resources:GetObject("eSmartFieldSecurityItem_SecurityKey_Label.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartFieldSecurityItem_SecurityKey_Label:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartFieldSecurityItem_SecurityKey_Label.TabIndex":U)).
        THIS-OBJECT:eSmartFieldSecurityItem_SecurityKey_Label:Text = resources:GetString("eSmartFieldSecurityItem_SecurityKey_Label.Text":U).
        /*  */
        /* eSmartFieldSecurityItem_SecurityKey */
        /*  */
        THIS-OBJECT:eSmartFieldSecurityItem_SecurityKey:AccessibleDescription = resources:GetString("eSmartFieldSecurityItem_SecurityKey.AccessibleDescription":U).
        THIS-OBJECT:eSmartFieldSecurityItem_SecurityKey:AccessibleName = resources:GetString("eSmartFieldSecurityItem_SecurityKey.AccessibleName":U).
        THIS-OBJECT:eSmartFieldSecurityItem_SecurityKey:Anchor = CAST(resources:GetObject("eSmartFieldSecurityItem_SecurityKey.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartFieldSecurityItem_SecurityKey:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartFieldSecurityItem_SecurityKey.AutoSize":U)).
        THIS-OBJECT:eSmartFieldSecurityItem_SecurityKey:BackgroundImage = CAST(resources:GetObject("eSmartFieldSecurityItem_SecurityKey.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartFieldSecurityItem_SecurityKey:BackgroundImageLayout = CAST(resources:GetObject("eSmartFieldSecurityItem_SecurityKey.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartFieldSecurityItem_SecurityKey:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "SecurityKey":U, TRUE)).
        THIS-OBJECT:eSmartFieldSecurityItem_SecurityKey:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartFieldSecurityItem_SecurityKey:Dock = CAST(resources:GetObject("eSmartFieldSecurityItem_SecurityKey.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartFieldSecurityItem_SecurityKey:Font = CAST(resources:GetObject("eSmartFieldSecurityItem_SecurityKey.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartFieldSecurityItem_SecurityKey:ImeMode = CAST(resources:GetObject("eSmartFieldSecurityItem_SecurityKey.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartFieldSecurityItem_SecurityKey:Location = CAST(resources:GetObject("eSmartFieldSecurityItem_SecurityKey.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartFieldSecurityItem_SecurityKey:MaximumSize = CAST(resources:GetObject("eSmartFieldSecurityItem_SecurityKey.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartFieldSecurityItem_SecurityKey:Name = "eSmartFieldSecurityItem_SecurityKey":U.
        THIS-OBJECT:eSmartFieldSecurityItem_SecurityKey:RightToLeft = CAST(resources:GetObject("eSmartFieldSecurityItem_SecurityKey.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartFieldSecurityItem_SecurityKey:Size = CAST(resources:GetObject("eSmartFieldSecurityItem_SecurityKey.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartFieldSecurityItem_SecurityKey:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartFieldSecurityItem_SecurityKey.TabIndex":U)).
        /*  */
        /* eSmartFieldSecurityItem_ObjectName_Label */
        /*  */
        THIS-OBJECT:eSmartFieldSecurityItem_ObjectName_Label:AccessibleDescription = resources:GetString("eSmartFieldSecurityItem_ObjectName_Label.AccessibleDescription":U).
        THIS-OBJECT:eSmartFieldSecurityItem_ObjectName_Label:AccessibleName = resources:GetString("eSmartFieldSecurityItem_ObjectName_Label.AccessibleName":U).
        THIS-OBJECT:eSmartFieldSecurityItem_ObjectName_Label:Anchor = CAST(resources:GetObject("eSmartFieldSecurityItem_ObjectName_Label.Anchor":U), System.Windows.Forms.AnchorStyles).
        appearance3:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        appearance3:TextHAlignAsString = resources:GetString("appearance3.TextHAlignAsString":U).
        appearance3:TextVAlignAsString = resources:GetString("appearance3.TextVAlignAsString":U).
        THIS-OBJECT:eSmartFieldSecurityItem_ObjectName_Label:Appearance = appearance3.
        THIS-OBJECT:eSmartFieldSecurityItem_ObjectName_Label:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartFieldSecurityItem_ObjectName_Label.AutoSize":U)).
        THIS-OBJECT:eSmartFieldSecurityItem_ObjectName_Label:BackgroundImage = CAST(resources:GetObject("eSmartFieldSecurityItem_ObjectName_Label.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartFieldSecurityItem_ObjectName_Label:BackgroundImageLayout = CAST(resources:GetObject("eSmartFieldSecurityItem_ObjectName_Label.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartFieldSecurityItem_ObjectName_Label:Dock = CAST(resources:GetObject("eSmartFieldSecurityItem_ObjectName_Label.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartFieldSecurityItem_ObjectName_Label:Font = CAST(resources:GetObject("eSmartFieldSecurityItem_ObjectName_Label.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartFieldSecurityItem_ObjectName_Label:ImeMode = CAST(resources:GetObject("eSmartFieldSecurityItem_ObjectName_Label.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartFieldSecurityItem_ObjectName_Label:Location = CAST(resources:GetObject("eSmartFieldSecurityItem_ObjectName_Label.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartFieldSecurityItem_ObjectName_Label:MaximumSize = CAST(resources:GetObject("eSmartFieldSecurityItem_ObjectName_Label.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartFieldSecurityItem_ObjectName_Label:Name = "eSmartFieldSecurityItem_ObjectName_Label":U.
        THIS-OBJECT:eSmartFieldSecurityItem_ObjectName_Label:RightToLeft = CAST(resources:GetObject("eSmartFieldSecurityItem_ObjectName_Label.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartFieldSecurityItem_ObjectName_Label:Size = CAST(resources:GetObject("eSmartFieldSecurityItem_ObjectName_Label.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartFieldSecurityItem_ObjectName_Label:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartFieldSecurityItem_ObjectName_Label.TabIndex":U)).
        THIS-OBJECT:eSmartFieldSecurityItem_ObjectName_Label:Text = resources:GetString("eSmartFieldSecurityItem_ObjectName_Label.Text":U).
        /*  */
        /* eSmartFieldSecurityItem_ObjectName */
        /*  */
        THIS-OBJECT:eSmartFieldSecurityItem_ObjectName:AccessibleDescription = resources:GetString("eSmartFieldSecurityItem_ObjectName.AccessibleDescription":U).
        THIS-OBJECT:eSmartFieldSecurityItem_ObjectName:AccessibleName = resources:GetString("eSmartFieldSecurityItem_ObjectName.AccessibleName":U).
        THIS-OBJECT:eSmartFieldSecurityItem_ObjectName:Anchor = CAST(resources:GetObject("eSmartFieldSecurityItem_ObjectName.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartFieldSecurityItem_ObjectName:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartFieldSecurityItem_ObjectName.AutoSize":U)).
        THIS-OBJECT:eSmartFieldSecurityItem_ObjectName:BackgroundImage = CAST(resources:GetObject("eSmartFieldSecurityItem_ObjectName.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartFieldSecurityItem_ObjectName:BackgroundImageLayout = CAST(resources:GetObject("eSmartFieldSecurityItem_ObjectName.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartFieldSecurityItem_ObjectName:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ObjectName":U, TRUE)).
        THIS-OBJECT:eSmartFieldSecurityItem_ObjectName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartFieldSecurityItem_ObjectName:Dock = CAST(resources:GetObject("eSmartFieldSecurityItem_ObjectName.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartFieldSecurityItem_ObjectName:Font = CAST(resources:GetObject("eSmartFieldSecurityItem_ObjectName.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartFieldSecurityItem_ObjectName:ImeMode = CAST(resources:GetObject("eSmartFieldSecurityItem_ObjectName.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartFieldSecurityItem_ObjectName:Location = CAST(resources:GetObject("eSmartFieldSecurityItem_ObjectName.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartFieldSecurityItem_ObjectName:MaximumSize = CAST(resources:GetObject("eSmartFieldSecurityItem_ObjectName.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartFieldSecurityItem_ObjectName:Name = "eSmartFieldSecurityItem_ObjectName":U.
        THIS-OBJECT:eSmartFieldSecurityItem_ObjectName:RightToLeft = CAST(resources:GetObject("eSmartFieldSecurityItem_ObjectName.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartFieldSecurityItem_ObjectName:Size = CAST(resources:GetObject("eSmartFieldSecurityItem_ObjectName.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartFieldSecurityItem_ObjectName:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartFieldSecurityItem_ObjectName.TabIndex":U)).
        /*  */
        /* eSmartFieldSecurityItem_Description_Label */
        /*  */
        THIS-OBJECT:eSmartFieldSecurityItem_Description_Label:AccessibleDescription = resources:GetString("eSmartFieldSecurityItem_Description_Label.AccessibleDescription":U).
        THIS-OBJECT:eSmartFieldSecurityItem_Description_Label:AccessibleName = resources:GetString("eSmartFieldSecurityItem_Description_Label.AccessibleName":U).
        THIS-OBJECT:eSmartFieldSecurityItem_Description_Label:Anchor = CAST(resources:GetObject("eSmartFieldSecurityItem_Description_Label.Anchor":U), System.Windows.Forms.AnchorStyles).
        appearance4:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        appearance4:TextHAlignAsString = resources:GetString("appearance4.TextHAlignAsString":U).
        appearance4:TextVAlignAsString = resources:GetString("appearance4.TextVAlignAsString":U).
        THIS-OBJECT:eSmartFieldSecurityItem_Description_Label:Appearance = appearance4.
        THIS-OBJECT:eSmartFieldSecurityItem_Description_Label:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartFieldSecurityItem_Description_Label.AutoSize":U)).
        THIS-OBJECT:eSmartFieldSecurityItem_Description_Label:BackgroundImage = CAST(resources:GetObject("eSmartFieldSecurityItem_Description_Label.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartFieldSecurityItem_Description_Label:BackgroundImageLayout = CAST(resources:GetObject("eSmartFieldSecurityItem_Description_Label.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartFieldSecurityItem_Description_Label:Dock = CAST(resources:GetObject("eSmartFieldSecurityItem_Description_Label.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartFieldSecurityItem_Description_Label:Font = CAST(resources:GetObject("eSmartFieldSecurityItem_Description_Label.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartFieldSecurityItem_Description_Label:ImeMode = CAST(resources:GetObject("eSmartFieldSecurityItem_Description_Label.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartFieldSecurityItem_Description_Label:Location = CAST(resources:GetObject("eSmartFieldSecurityItem_Description_Label.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartFieldSecurityItem_Description_Label:MaximumSize = CAST(resources:GetObject("eSmartFieldSecurityItem_Description_Label.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartFieldSecurityItem_Description_Label:Name = "eSmartFieldSecurityItem_Description_Label":U.
        THIS-OBJECT:eSmartFieldSecurityItem_Description_Label:RightToLeft = CAST(resources:GetObject("eSmartFieldSecurityItem_Description_Label.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartFieldSecurityItem_Description_Label:Size = CAST(resources:GetObject("eSmartFieldSecurityItem_Description_Label.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartFieldSecurityItem_Description_Label:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartFieldSecurityItem_Description_Label.TabIndex":U)).
        THIS-OBJECT:eSmartFieldSecurityItem_Description_Label:Text = resources:GetString("eSmartFieldSecurityItem_Description_Label.Text":U).
        /*  */
        /* eSmartFieldSecurityItem_Description */
        /*  */
        THIS-OBJECT:eSmartFieldSecurityItem_Description:AccessibleDescription = resources:GetString("eSmartFieldSecurityItem_Description.AccessibleDescription":U).
        THIS-OBJECT:eSmartFieldSecurityItem_Description:AccessibleName = resources:GetString("eSmartFieldSecurityItem_Description.AccessibleName":U).
        THIS-OBJECT:eSmartFieldSecurityItem_Description:Anchor = CAST(resources:GetObject("eSmartFieldSecurityItem_Description.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartFieldSecurityItem_Description:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartFieldSecurityItem_Description.AutoSize":U)).
        THIS-OBJECT:eSmartFieldSecurityItem_Description:BackgroundImage = CAST(resources:GetObject("eSmartFieldSecurityItem_Description.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartFieldSecurityItem_Description:BackgroundImageLayout = CAST(resources:GetObject("eSmartFieldSecurityItem_Description.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartFieldSecurityItem_Description:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "Description":U, TRUE)).
        THIS-OBJECT:eSmartFieldSecurityItem_Description:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartFieldSecurityItem_Description:Dock = CAST(resources:GetObject("eSmartFieldSecurityItem_Description.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartFieldSecurityItem_Description:Font = CAST(resources:GetObject("eSmartFieldSecurityItem_Description.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartFieldSecurityItem_Description:ImeMode = CAST(resources:GetObject("eSmartFieldSecurityItem_Description.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartFieldSecurityItem_Description:Location = CAST(resources:GetObject("eSmartFieldSecurityItem_Description.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartFieldSecurityItem_Description:MaximumSize = CAST(resources:GetObject("eSmartFieldSecurityItem_Description.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartFieldSecurityItem_Description:Multiline = System.Convert:ToBoolean(resources:GetObject("eSmartFieldSecurityItem_Description.Multiline":U)).
        THIS-OBJECT:eSmartFieldSecurityItem_Description:Name = "eSmartFieldSecurityItem_Description":U.
        THIS-OBJECT:eSmartFieldSecurityItem_Description:RightToLeft = CAST(resources:GetObject("eSmartFieldSecurityItem_Description.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartFieldSecurityItem_Description:Size = CAST(resources:GetObject("eSmartFieldSecurityItem_Description.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartFieldSecurityItem_Description:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartFieldSecurityItem_Description.TabIndex":U)).
        /*  */
        /* SmartFieldSecurityItemViewerControl */
        /*  */
        THIS-OBJECT:AccessibleDescription = resources:GetString("$this.AccessibleDescription":U).
        THIS-OBJECT:AccessibleName = resources:GetString("$this.AccessibleName":U).
        THIS-OBJECT:AutoScaleDimensions = CAST(resources:GetObject("$this.AutoScaleDimensions":U), System.Drawing.SizeF).
        THIS-OBJECT:AutoScroll = System.Convert:ToBoolean(resources:GetObject("$this.AutoScroll":U)).
        THIS-OBJECT:AutoSize = System.Convert:ToBoolean(resources:GetObject("$this.AutoSize":U)).
        THIS-OBJECT:AutoSizeMode = CAST(resources:GetObject("$this.AutoSizeMode":U), System.Windows.Forms.AutoSizeMode).
        THIS-OBJECT:BackgroundImage = CAST(resources:GetObject("$this.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:BackgroundImageLayout = CAST(resources:GetObject("$this.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartFieldSecurityItem_FieldName_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartFieldSecurityItem_FieldName).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartFieldSecurityItem_SecurityKey_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartFieldSecurityItem_SecurityKey).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartFieldSecurityItem_ObjectName_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartFieldSecurityItem_ObjectName).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartFieldSecurityItem_Description_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartFieldSecurityItem_Description).
        THIS-OBJECT:Font = CAST(resources:GetObject("$this.Font":U), System.Drawing.Font).
        THIS-OBJECT:ImeMode = CAST(resources:GetObject("$this.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:Location = CAST(resources:GetObject("$this.Location":U), System.Drawing.Point).
        THIS-OBJECT:MaximumSize = CAST(resources:GetObject("$this.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:MinimumSize = CAST(resources:GetObject("$this.MinimumSize":U), System.Drawing.Size).
        THIS-OBJECT:Name = "SmartFieldSecurityItemViewerControl":U.
        THIS-OBJECT:RightToLeft = CAST(resources:GetObject("$this.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:SecurityKey = resources:GetString("$this.SecurityKey":U).
        THIS-OBJECT:Size = CAST(resources:GetObject("$this.Size":U), System.Drawing.Size).
        THIS-OBJECT:SmartTableIOState = resources:GetString("$this.SmartTableIOState":U).
        THIS-OBJECT:ViewerLogicClass = resources:GetString("$this.ViewerLogicClass":U).
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartFieldSecurityItem_FieldName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartFieldSecurityItem_SecurityKey, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartFieldSecurityItem_ObjectName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartFieldSecurityItem_Description, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SmartFieldSecurityItemViewerControl class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartFieldSecurityItemViewerControl ( ):

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
