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
    File        : SmartLinkTypeViewerControl
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : 21.05.2014 10:29:09
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Base.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.Util.*                           FROM PROPATH .
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.Repository.Class.SmartLinkTypeViewerControl 
    INHERITS SmartViewerControl: 
    
    DEFINE PRIVATE VARIABLE components                                 AS System.ComponentModel.IContainer                                               NO-UNDO.

    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1          AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartLinkType_LinkName_Label              AS Infragistics.Win.Misc.UltraLabel                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartLinkType_LinkName                    AS Infragistics.Win.UltraWinEditors.UltraTextEditor                               NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartLinkType_SourcePropertyName_Label    AS Infragistics.Win.Misc.UltraLabel                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartLinkType_SourcePropertyName          AS Infragistics.Win.UltraWinEditors.UltraTextEditor                               NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartLinkType_SourceHandshakeMethod_Label AS Infragistics.Win.Misc.UltraLabel                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartLinkType_SourceHandshakeMethod       AS Infragistics.Win.UltraWinEditors.UltraTextEditor                               NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartLinkType_TargetPropertyName_Label    AS Infragistics.Win.Misc.UltraLabel                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartLinkType_TargetPropertyName          AS Infragistics.Win.UltraWinEditors.UltraTextEditor                               NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartLinkType_TargetHandshakeMethod_Label AS Infragistics.Win.Misc.UltraLabel                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartLinkType_TargetHandshakeMethod       AS Infragistics.Win.UltraWinEditors.UltraTextEditor                               NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartLinkTypeViewerControl class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartLinkTypeViewerControl ():
                
        SUPER().
        InitializeComponent().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Initializes the Visual Design
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID InitializeComponent ():
        
        /* NOTE: The following method is automatically generated.
        
        We strongly suggest that the contents of this method only be modified using the
        Visual Designer to avoid any incompatible modifications.
        
        Modifying the contents of this method using a code editor will invalidate any support for this file. */
        THIS-OBJECT:components = NEW System.ComponentModel.Container().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.Repository.Class.SmartLinkTypeViewerControl":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc1 = NEW Progress.Data.TableDesc("eSmartLinkType":U).
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
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance5 AS Infragistics.Win.Appearance NO-UNDO.
        appearance5 = NEW Infragistics.Win.Appearance().
        THIS-OBJECT:smartBusinessEntityBindingSource1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:eSmartLinkType_LinkName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartLinkType_LinkName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartLinkType_SourcePropertyName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartLinkType_SourcePropertyName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartLinkType_SourceHandshakeMethod_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartLinkType_SourceHandshakeMethod = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartLinkType_TargetPropertyName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartLinkType_TargetPropertyName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartLinkType_TargetHandshakeMethod_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartLinkType_TargetHandshakeMethod = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartLinkType_LinkName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartLinkType_SourcePropertyName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartLinkType_SourceHandshakeMethod, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartLinkType_TargetPropertyName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartLinkType_TargetHandshakeMethod, System.ComponentModel.ISupportInitialize):BeginInit().
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
        arrayvar0[1] = NEW Progress.Data.ColumnPropDesc("LinkTypeGuid":U, "LinkTypeGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[2] = NEW Progress.Data.ColumnPropDesc("LinkName":U, "Link Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[3] = NEW Progress.Data.ColumnPropDesc("SourcePropertyName":U, "Source Property Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[4] = NEW Progress.Data.ColumnPropDesc("SourceHandshakeMethod":U, "Source Handshake Method":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[5] = NEW Progress.Data.ColumnPropDesc("TargetPropertyName":U, "Target Property Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[6] = NEW Progress.Data.ColumnPropDesc("TargetHandshakeMethod":U, "Target Handshake Method":U, Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:TableSchema = tableDesc1.
        /*  */
        /* eSmartLinkType_LinkName_Label */
        /*  */
        THIS-OBJECT:eSmartLinkType_LinkName_Label:AccessibleDescription = resources:GetString("eSmartLinkType_LinkName_Label.AccessibleDescription":U).
        THIS-OBJECT:eSmartLinkType_LinkName_Label:AccessibleName = resources:GetString("eSmartLinkType_LinkName_Label.AccessibleName":U).
        THIS-OBJECT:eSmartLinkType_LinkName_Label:Anchor = CAST(resources:GetObject("eSmartLinkType_LinkName_Label.Anchor":U), System.Windows.Forms.AnchorStyles).
        appearance1:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        appearance1:TextHAlignAsString = resources:GetString("appearance1.TextHAlignAsString":U).
        appearance1:TextVAlignAsString = resources:GetString("appearance1.TextVAlignAsString":U).
        THIS-OBJECT:eSmartLinkType_LinkName_Label:Appearance = appearance1.
        THIS-OBJECT:eSmartLinkType_LinkName_Label:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartLinkType_LinkName_Label.AutoSize":U)).
        THIS-OBJECT:eSmartLinkType_LinkName_Label:BackgroundImage = CAST(resources:GetObject("eSmartLinkType_LinkName_Label.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartLinkType_LinkName_Label:BackgroundImageLayout = CAST(resources:GetObject("eSmartLinkType_LinkName_Label.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartLinkType_LinkName_Label:Dock = CAST(resources:GetObject("eSmartLinkType_LinkName_Label.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartLinkType_LinkName_Label:Font = CAST(resources:GetObject("eSmartLinkType_LinkName_Label.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartLinkType_LinkName_Label:ImeMode = CAST(resources:GetObject("eSmartLinkType_LinkName_Label.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartLinkType_LinkName_Label:Location = CAST(resources:GetObject("eSmartLinkType_LinkName_Label.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartLinkType_LinkName_Label:MaximumSize = CAST(resources:GetObject("eSmartLinkType_LinkName_Label.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartLinkType_LinkName_Label:Name = "eSmartLinkType_LinkName_Label":U.
        THIS-OBJECT:eSmartLinkType_LinkName_Label:RightToLeft = CAST(resources:GetObject("eSmartLinkType_LinkName_Label.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartLinkType_LinkName_Label:Size = CAST(resources:GetObject("eSmartLinkType_LinkName_Label.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartLinkType_LinkName_Label:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartLinkType_LinkName_Label.TabIndex":U)).
        THIS-OBJECT:eSmartLinkType_LinkName_Label:Text = resources:GetString("eSmartLinkType_LinkName_Label.Text":U).
        /*  */
        /* eSmartLinkType_LinkName */
        /*  */
        THIS-OBJECT:eSmartLinkType_LinkName:AccessibleDescription = resources:GetString("eSmartLinkType_LinkName.AccessibleDescription":U).
        THIS-OBJECT:eSmartLinkType_LinkName:AccessibleName = resources:GetString("eSmartLinkType_LinkName.AccessibleName":U).
        THIS-OBJECT:eSmartLinkType_LinkName:Anchor = CAST(resources:GetObject("eSmartLinkType_LinkName.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartLinkType_LinkName:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartLinkType_LinkName.AutoSize":U)).
        THIS-OBJECT:eSmartLinkType_LinkName:BackgroundImage = CAST(resources:GetObject("eSmartLinkType_LinkName.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartLinkType_LinkName:BackgroundImageLayout = CAST(resources:GetObject("eSmartLinkType_LinkName.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartLinkType_LinkName:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "LinkName":U, TRUE)).
        THIS-OBJECT:eSmartLinkType_LinkName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartLinkType_LinkName:Dock = CAST(resources:GetObject("eSmartLinkType_LinkName.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartLinkType_LinkName:Font = CAST(resources:GetObject("eSmartLinkType_LinkName.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartLinkType_LinkName:ImeMode = CAST(resources:GetObject("eSmartLinkType_LinkName.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartLinkType_LinkName:Location = CAST(resources:GetObject("eSmartLinkType_LinkName.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartLinkType_LinkName:MaximumSize = CAST(resources:GetObject("eSmartLinkType_LinkName.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartLinkType_LinkName:Name = "eSmartLinkType_LinkName":U.
        THIS-OBJECT:eSmartLinkType_LinkName:RightToLeft = CAST(resources:GetObject("eSmartLinkType_LinkName.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartLinkType_LinkName:Size = CAST(resources:GetObject("eSmartLinkType_LinkName.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartLinkType_LinkName:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartLinkType_LinkName.TabIndex":U)).
        /*  */
        /* eSmartLinkType_SourcePropertyName_Label */
        /*  */
        THIS-OBJECT:eSmartLinkType_SourcePropertyName_Label:AccessibleDescription = resources:GetString("eSmartLinkType_SourcePropertyName_Label.AccessibleDescription":U).
        THIS-OBJECT:eSmartLinkType_SourcePropertyName_Label:AccessibleName = resources:GetString("eSmartLinkType_SourcePropertyName_Label.AccessibleName":U).
        THIS-OBJECT:eSmartLinkType_SourcePropertyName_Label:Anchor = CAST(resources:GetObject("eSmartLinkType_SourcePropertyName_Label.Anchor":U), System.Windows.Forms.AnchorStyles).
        appearance2:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        appearance2:TextHAlignAsString = resources:GetString("appearance2.TextHAlignAsString":U).
        appearance2:TextVAlignAsString = resources:GetString("appearance2.TextVAlignAsString":U).
        THIS-OBJECT:eSmartLinkType_SourcePropertyName_Label:Appearance = appearance2.
        THIS-OBJECT:eSmartLinkType_SourcePropertyName_Label:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartLinkType_SourcePropertyName_Label.AutoSize":U)).
        THIS-OBJECT:eSmartLinkType_SourcePropertyName_Label:BackgroundImage = CAST(resources:GetObject("eSmartLinkType_SourcePropertyName_Label.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartLinkType_SourcePropertyName_Label:BackgroundImageLayout = CAST(resources:GetObject("eSmartLinkType_SourcePropertyName_Label.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartLinkType_SourcePropertyName_Label:Dock = CAST(resources:GetObject("eSmartLinkType_SourcePropertyName_Label.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartLinkType_SourcePropertyName_Label:Font = CAST(resources:GetObject("eSmartLinkType_SourcePropertyName_Label.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartLinkType_SourcePropertyName_Label:ImeMode = CAST(resources:GetObject("eSmartLinkType_SourcePropertyName_Label.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartLinkType_SourcePropertyName_Label:Location = CAST(resources:GetObject("eSmartLinkType_SourcePropertyName_Label.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartLinkType_SourcePropertyName_Label:MaximumSize = CAST(resources:GetObject("eSmartLinkType_SourcePropertyName_Label.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartLinkType_SourcePropertyName_Label:Name = "eSmartLinkType_SourcePropertyName_Label":U.
        THIS-OBJECT:eSmartLinkType_SourcePropertyName_Label:RightToLeft = CAST(resources:GetObject("eSmartLinkType_SourcePropertyName_Label.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartLinkType_SourcePropertyName_Label:Size = CAST(resources:GetObject("eSmartLinkType_SourcePropertyName_Label.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartLinkType_SourcePropertyName_Label:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartLinkType_SourcePropertyName_Label.TabIndex":U)).
        THIS-OBJECT:eSmartLinkType_SourcePropertyName_Label:Text = resources:GetString("eSmartLinkType_SourcePropertyName_Label.Text":U).
        /*  */
        /* eSmartLinkType_SourcePropertyName */
        /*  */
        THIS-OBJECT:eSmartLinkType_SourcePropertyName:AccessibleDescription = resources:GetString("eSmartLinkType_SourcePropertyName.AccessibleDescription":U).
        THIS-OBJECT:eSmartLinkType_SourcePropertyName:AccessibleName = resources:GetString("eSmartLinkType_SourcePropertyName.AccessibleName":U).
        THIS-OBJECT:eSmartLinkType_SourcePropertyName:Anchor = CAST(resources:GetObject("eSmartLinkType_SourcePropertyName.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartLinkType_SourcePropertyName:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartLinkType_SourcePropertyName.AutoSize":U)).
        THIS-OBJECT:eSmartLinkType_SourcePropertyName:BackgroundImage = CAST(resources:GetObject("eSmartLinkType_SourcePropertyName.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartLinkType_SourcePropertyName:BackgroundImageLayout = CAST(resources:GetObject("eSmartLinkType_SourcePropertyName.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartLinkType_SourcePropertyName:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "SourcePropertyName":U, TRUE)).
        THIS-OBJECT:eSmartLinkType_SourcePropertyName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartLinkType_SourcePropertyName:Dock = CAST(resources:GetObject("eSmartLinkType_SourcePropertyName.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartLinkType_SourcePropertyName:Font = CAST(resources:GetObject("eSmartLinkType_SourcePropertyName.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartLinkType_SourcePropertyName:ImeMode = CAST(resources:GetObject("eSmartLinkType_SourcePropertyName.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartLinkType_SourcePropertyName:Location = CAST(resources:GetObject("eSmartLinkType_SourcePropertyName.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartLinkType_SourcePropertyName:MaximumSize = CAST(resources:GetObject("eSmartLinkType_SourcePropertyName.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartLinkType_SourcePropertyName:Name = "eSmartLinkType_SourcePropertyName":U.
        THIS-OBJECT:eSmartLinkType_SourcePropertyName:RightToLeft = CAST(resources:GetObject("eSmartLinkType_SourcePropertyName.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartLinkType_SourcePropertyName:Size = CAST(resources:GetObject("eSmartLinkType_SourcePropertyName.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartLinkType_SourcePropertyName:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartLinkType_SourcePropertyName.TabIndex":U)).
        /*  */
        /* eSmartLinkType_SourceHandshakeMethod_Label */
        /*  */
        THIS-OBJECT:eSmartLinkType_SourceHandshakeMethod_Label:AccessibleDescription = resources:GetString("eSmartLinkType_SourceHandshakeMethod_Label.AccessibleDescription":U).
        THIS-OBJECT:eSmartLinkType_SourceHandshakeMethod_Label:AccessibleName = resources:GetString("eSmartLinkType_SourceHandshakeMethod_Label.AccessibleName":U).
        THIS-OBJECT:eSmartLinkType_SourceHandshakeMethod_Label:Anchor = CAST(resources:GetObject("eSmartLinkType_SourceHandshakeMethod_Label.Anchor":U), System.Windows.Forms.AnchorStyles).
        appearance3:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        appearance3:TextHAlignAsString = resources:GetString("appearance3.TextHAlignAsString":U).
        appearance3:TextVAlignAsString = resources:GetString("appearance3.TextVAlignAsString":U).
        THIS-OBJECT:eSmartLinkType_SourceHandshakeMethod_Label:Appearance = appearance3.
        THIS-OBJECT:eSmartLinkType_SourceHandshakeMethod_Label:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartLinkType_SourceHandshakeMethod_Label.AutoSize":U)).
        THIS-OBJECT:eSmartLinkType_SourceHandshakeMethod_Label:BackgroundImage = CAST(resources:GetObject("eSmartLinkType_SourceHandshakeMethod_Label.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartLinkType_SourceHandshakeMethod_Label:BackgroundImageLayout = CAST(resources:GetObject("eSmartLinkType_SourceHandshakeMethod_Label.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartLinkType_SourceHandshakeMethod_Label:Dock = CAST(resources:GetObject("eSmartLinkType_SourceHandshakeMethod_Label.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartLinkType_SourceHandshakeMethod_Label:Font = CAST(resources:GetObject("eSmartLinkType_SourceHandshakeMethod_Label.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartLinkType_SourceHandshakeMethod_Label:ImeMode = CAST(resources:GetObject("eSmartLinkType_SourceHandshakeMethod_Label.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartLinkType_SourceHandshakeMethod_Label:Location = CAST(resources:GetObject("eSmartLinkType_SourceHandshakeMethod_Label.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartLinkType_SourceHandshakeMethod_Label:MaximumSize = CAST(resources:GetObject("eSmartLinkType_SourceHandshakeMethod_Label.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartLinkType_SourceHandshakeMethod_Label:Name = "eSmartLinkType_SourceHandshakeMethod_Label":U.
        THIS-OBJECT:eSmartLinkType_SourceHandshakeMethod_Label:RightToLeft = CAST(resources:GetObject("eSmartLinkType_SourceHandshakeMethod_Label.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartLinkType_SourceHandshakeMethod_Label:Size = CAST(resources:GetObject("eSmartLinkType_SourceHandshakeMethod_Label.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartLinkType_SourceHandshakeMethod_Label:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartLinkType_SourceHandshakeMethod_Label.TabIndex":U)).
        THIS-OBJECT:eSmartLinkType_SourceHandshakeMethod_Label:Text = resources:GetString("eSmartLinkType_SourceHandshakeMethod_Label.Text":U).
        /*  */
        /* eSmartLinkType_SourceHandshakeMethod */
        /*  */
        THIS-OBJECT:eSmartLinkType_SourceHandshakeMethod:AccessibleDescription = resources:GetString("eSmartLinkType_SourceHandshakeMethod.AccessibleDescription":U).
        THIS-OBJECT:eSmartLinkType_SourceHandshakeMethod:AccessibleName = resources:GetString("eSmartLinkType_SourceHandshakeMethod.AccessibleName":U).
        THIS-OBJECT:eSmartLinkType_SourceHandshakeMethod:Anchor = CAST(resources:GetObject("eSmartLinkType_SourceHandshakeMethod.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartLinkType_SourceHandshakeMethod:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartLinkType_SourceHandshakeMethod.AutoSize":U)).
        THIS-OBJECT:eSmartLinkType_SourceHandshakeMethod:BackgroundImage = CAST(resources:GetObject("eSmartLinkType_SourceHandshakeMethod.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartLinkType_SourceHandshakeMethod:BackgroundImageLayout = CAST(resources:GetObject("eSmartLinkType_SourceHandshakeMethod.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartLinkType_SourceHandshakeMethod:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "SourceHandshakeMethod":U, TRUE)).
        THIS-OBJECT:eSmartLinkType_SourceHandshakeMethod:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartLinkType_SourceHandshakeMethod:Dock = CAST(resources:GetObject("eSmartLinkType_SourceHandshakeMethod.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartLinkType_SourceHandshakeMethod:Font = CAST(resources:GetObject("eSmartLinkType_SourceHandshakeMethod.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartLinkType_SourceHandshakeMethod:ImeMode = CAST(resources:GetObject("eSmartLinkType_SourceHandshakeMethod.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartLinkType_SourceHandshakeMethod:Location = CAST(resources:GetObject("eSmartLinkType_SourceHandshakeMethod.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartLinkType_SourceHandshakeMethod:MaximumSize = CAST(resources:GetObject("eSmartLinkType_SourceHandshakeMethod.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartLinkType_SourceHandshakeMethod:Name = "eSmartLinkType_SourceHandshakeMethod":U.
        THIS-OBJECT:eSmartLinkType_SourceHandshakeMethod:RightToLeft = CAST(resources:GetObject("eSmartLinkType_SourceHandshakeMethod.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartLinkType_SourceHandshakeMethod:Size = CAST(resources:GetObject("eSmartLinkType_SourceHandshakeMethod.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartLinkType_SourceHandshakeMethod:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartLinkType_SourceHandshakeMethod.TabIndex":U)).
        /*  */
        /* eSmartLinkType_TargetPropertyName_Label */
        /*  */
        THIS-OBJECT:eSmartLinkType_TargetPropertyName_Label:AccessibleDescription = resources:GetString("eSmartLinkType_TargetPropertyName_Label.AccessibleDescription":U).
        THIS-OBJECT:eSmartLinkType_TargetPropertyName_Label:AccessibleName = resources:GetString("eSmartLinkType_TargetPropertyName_Label.AccessibleName":U).
        THIS-OBJECT:eSmartLinkType_TargetPropertyName_Label:Anchor = CAST(resources:GetObject("eSmartLinkType_TargetPropertyName_Label.Anchor":U), System.Windows.Forms.AnchorStyles).
        appearance4:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        appearance4:TextHAlignAsString = resources:GetString("appearance4.TextHAlignAsString":U).
        appearance4:TextVAlignAsString = resources:GetString("appearance4.TextVAlignAsString":U).
        THIS-OBJECT:eSmartLinkType_TargetPropertyName_Label:Appearance = appearance4.
        THIS-OBJECT:eSmartLinkType_TargetPropertyName_Label:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartLinkType_TargetPropertyName_Label.AutoSize":U)).
        THIS-OBJECT:eSmartLinkType_TargetPropertyName_Label:BackgroundImage = CAST(resources:GetObject("eSmartLinkType_TargetPropertyName_Label.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartLinkType_TargetPropertyName_Label:BackgroundImageLayout = CAST(resources:GetObject("eSmartLinkType_TargetPropertyName_Label.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartLinkType_TargetPropertyName_Label:Dock = CAST(resources:GetObject("eSmartLinkType_TargetPropertyName_Label.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartLinkType_TargetPropertyName_Label:Font = CAST(resources:GetObject("eSmartLinkType_TargetPropertyName_Label.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartLinkType_TargetPropertyName_Label:ImeMode = CAST(resources:GetObject("eSmartLinkType_TargetPropertyName_Label.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartLinkType_TargetPropertyName_Label:Location = CAST(resources:GetObject("eSmartLinkType_TargetPropertyName_Label.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartLinkType_TargetPropertyName_Label:MaximumSize = CAST(resources:GetObject("eSmartLinkType_TargetPropertyName_Label.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartLinkType_TargetPropertyName_Label:Name = "eSmartLinkType_TargetPropertyName_Label":U.
        THIS-OBJECT:eSmartLinkType_TargetPropertyName_Label:RightToLeft = CAST(resources:GetObject("eSmartLinkType_TargetPropertyName_Label.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartLinkType_TargetPropertyName_Label:Size = CAST(resources:GetObject("eSmartLinkType_TargetPropertyName_Label.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartLinkType_TargetPropertyName_Label:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartLinkType_TargetPropertyName_Label.TabIndex":U)).
        THIS-OBJECT:eSmartLinkType_TargetPropertyName_Label:Text = resources:GetString("eSmartLinkType_TargetPropertyName_Label.Text":U).
        /*  */
        /* eSmartLinkType_TargetPropertyName */
        /*  */
        THIS-OBJECT:eSmartLinkType_TargetPropertyName:AccessibleDescription = resources:GetString("eSmartLinkType_TargetPropertyName.AccessibleDescription":U).
        THIS-OBJECT:eSmartLinkType_TargetPropertyName:AccessibleName = resources:GetString("eSmartLinkType_TargetPropertyName.AccessibleName":U).
        THIS-OBJECT:eSmartLinkType_TargetPropertyName:Anchor = CAST(resources:GetObject("eSmartLinkType_TargetPropertyName.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartLinkType_TargetPropertyName:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartLinkType_TargetPropertyName.AutoSize":U)).
        THIS-OBJECT:eSmartLinkType_TargetPropertyName:BackgroundImage = CAST(resources:GetObject("eSmartLinkType_TargetPropertyName.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartLinkType_TargetPropertyName:BackgroundImageLayout = CAST(resources:GetObject("eSmartLinkType_TargetPropertyName.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartLinkType_TargetPropertyName:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "TargetPropertyName":U, TRUE)).
        THIS-OBJECT:eSmartLinkType_TargetPropertyName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartLinkType_TargetPropertyName:Dock = CAST(resources:GetObject("eSmartLinkType_TargetPropertyName.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartLinkType_TargetPropertyName:Font = CAST(resources:GetObject("eSmartLinkType_TargetPropertyName.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartLinkType_TargetPropertyName:ImeMode = CAST(resources:GetObject("eSmartLinkType_TargetPropertyName.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartLinkType_TargetPropertyName:Location = CAST(resources:GetObject("eSmartLinkType_TargetPropertyName.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartLinkType_TargetPropertyName:MaximumSize = CAST(resources:GetObject("eSmartLinkType_TargetPropertyName.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartLinkType_TargetPropertyName:Name = "eSmartLinkType_TargetPropertyName":U.
        THIS-OBJECT:eSmartLinkType_TargetPropertyName:RightToLeft = CAST(resources:GetObject("eSmartLinkType_TargetPropertyName.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartLinkType_TargetPropertyName:Size = CAST(resources:GetObject("eSmartLinkType_TargetPropertyName.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartLinkType_TargetPropertyName:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartLinkType_TargetPropertyName.TabIndex":U)).
        /*  */
        /* eSmartLinkType_TargetHandshakeMethod_Label */
        /*  */
        THIS-OBJECT:eSmartLinkType_TargetHandshakeMethod_Label:AccessibleDescription = resources:GetString("eSmartLinkType_TargetHandshakeMethod_Label.AccessibleDescription":U).
        THIS-OBJECT:eSmartLinkType_TargetHandshakeMethod_Label:AccessibleName = resources:GetString("eSmartLinkType_TargetHandshakeMethod_Label.AccessibleName":U).
        THIS-OBJECT:eSmartLinkType_TargetHandshakeMethod_Label:Anchor = CAST(resources:GetObject("eSmartLinkType_TargetHandshakeMethod_Label.Anchor":U), System.Windows.Forms.AnchorStyles).
        appearance5:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        appearance5:TextHAlignAsString = resources:GetString("appearance5.TextHAlignAsString":U).
        appearance5:TextVAlignAsString = resources:GetString("appearance5.TextVAlignAsString":U).
        THIS-OBJECT:eSmartLinkType_TargetHandshakeMethod_Label:Appearance = appearance5.
        THIS-OBJECT:eSmartLinkType_TargetHandshakeMethod_Label:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartLinkType_TargetHandshakeMethod_Label.AutoSize":U)).
        THIS-OBJECT:eSmartLinkType_TargetHandshakeMethod_Label:BackgroundImage = CAST(resources:GetObject("eSmartLinkType_TargetHandshakeMethod_Label.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartLinkType_TargetHandshakeMethod_Label:BackgroundImageLayout = CAST(resources:GetObject("eSmartLinkType_TargetHandshakeMethod_Label.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartLinkType_TargetHandshakeMethod_Label:Dock = CAST(resources:GetObject("eSmartLinkType_TargetHandshakeMethod_Label.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartLinkType_TargetHandshakeMethod_Label:Font = CAST(resources:GetObject("eSmartLinkType_TargetHandshakeMethod_Label.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartLinkType_TargetHandshakeMethod_Label:ImeMode = CAST(resources:GetObject("eSmartLinkType_TargetHandshakeMethod_Label.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartLinkType_TargetHandshakeMethod_Label:Location = CAST(resources:GetObject("eSmartLinkType_TargetHandshakeMethod_Label.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartLinkType_TargetHandshakeMethod_Label:MaximumSize = CAST(resources:GetObject("eSmartLinkType_TargetHandshakeMethod_Label.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartLinkType_TargetHandshakeMethod_Label:Name = "eSmartLinkType_TargetHandshakeMethod_Label":U.
        THIS-OBJECT:eSmartLinkType_TargetHandshakeMethod_Label:RightToLeft = CAST(resources:GetObject("eSmartLinkType_TargetHandshakeMethod_Label.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartLinkType_TargetHandshakeMethod_Label:Size = CAST(resources:GetObject("eSmartLinkType_TargetHandshakeMethod_Label.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartLinkType_TargetHandshakeMethod_Label:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartLinkType_TargetHandshakeMethod_Label.TabIndex":U)).
        THIS-OBJECT:eSmartLinkType_TargetHandshakeMethod_Label:Text = resources:GetString("eSmartLinkType_TargetHandshakeMethod_Label.Text":U).
        /*  */
        /* eSmartLinkType_TargetHandshakeMethod */
        /*  */
        THIS-OBJECT:eSmartLinkType_TargetHandshakeMethod:AccessibleDescription = resources:GetString("eSmartLinkType_TargetHandshakeMethod.AccessibleDescription":U).
        THIS-OBJECT:eSmartLinkType_TargetHandshakeMethod:AccessibleName = resources:GetString("eSmartLinkType_TargetHandshakeMethod.AccessibleName":U).
        THIS-OBJECT:eSmartLinkType_TargetHandshakeMethod:Anchor = CAST(resources:GetObject("eSmartLinkType_TargetHandshakeMethod.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartLinkType_TargetHandshakeMethod:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartLinkType_TargetHandshakeMethod.AutoSize":U)).
        THIS-OBJECT:eSmartLinkType_TargetHandshakeMethod:BackgroundImage = CAST(resources:GetObject("eSmartLinkType_TargetHandshakeMethod.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartLinkType_TargetHandshakeMethod:BackgroundImageLayout = CAST(resources:GetObject("eSmartLinkType_TargetHandshakeMethod.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartLinkType_TargetHandshakeMethod:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "TargetHandshakeMethod":U, TRUE)).
        THIS-OBJECT:eSmartLinkType_TargetHandshakeMethod:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartLinkType_TargetHandshakeMethod:Dock = CAST(resources:GetObject("eSmartLinkType_TargetHandshakeMethod.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartLinkType_TargetHandshakeMethod:Font = CAST(resources:GetObject("eSmartLinkType_TargetHandshakeMethod.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartLinkType_TargetHandshakeMethod:ImeMode = CAST(resources:GetObject("eSmartLinkType_TargetHandshakeMethod.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartLinkType_TargetHandshakeMethod:Location = CAST(resources:GetObject("eSmartLinkType_TargetHandshakeMethod.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartLinkType_TargetHandshakeMethod:MaximumSize = CAST(resources:GetObject("eSmartLinkType_TargetHandshakeMethod.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartLinkType_TargetHandshakeMethod:Name = "eSmartLinkType_TargetHandshakeMethod":U.
        THIS-OBJECT:eSmartLinkType_TargetHandshakeMethod:RightToLeft = CAST(resources:GetObject("eSmartLinkType_TargetHandshakeMethod.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartLinkType_TargetHandshakeMethod:Size = CAST(resources:GetObject("eSmartLinkType_TargetHandshakeMethod.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartLinkType_TargetHandshakeMethod:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartLinkType_TargetHandshakeMethod.TabIndex":U)).
        /*  */
        /* SmartLinkTypeViewerControl */
        /*  */
        THIS-OBJECT:AccessibleDescription = resources:GetString("$this.AccessibleDescription":U).
        THIS-OBJECT:AccessibleName = resources:GetString("$this.AccessibleName":U).
        THIS-OBJECT:AutoScaleDimensions = CAST(resources:GetObject("$this.AutoScaleDimensions":U), System.Drawing.SizeF).
        THIS-OBJECT:AutoScroll = System.Convert:ToBoolean(resources:GetObject("$this.AutoScroll":U)).
        THIS-OBJECT:AutoSize = System.Convert:ToBoolean(resources:GetObject("$this.AutoSize":U)).
        THIS-OBJECT:AutoSizeMode = CAST(resources:GetObject("$this.AutoSizeMode":U), System.Windows.Forms.AutoSizeMode).
        THIS-OBJECT:BackgroundImage = CAST(resources:GetObject("$this.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:BackgroundImageLayout = CAST(resources:GetObject("$this.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartLinkType_LinkName_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartLinkType_LinkName).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartLinkType_SourcePropertyName_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartLinkType_SourcePropertyName).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartLinkType_SourceHandshakeMethod_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartLinkType_SourceHandshakeMethod).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartLinkType_TargetPropertyName_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartLinkType_TargetPropertyName).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartLinkType_TargetHandshakeMethod_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartLinkType_TargetHandshakeMethod).
        THIS-OBJECT:Font = CAST(resources:GetObject("$this.Font":U), System.Drawing.Font).
        THIS-OBJECT:ImeMode = CAST(resources:GetObject("$this.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:Location = CAST(resources:GetObject("$this.Location":U), System.Drawing.Point).
        THIS-OBJECT:MaximumSize = CAST(resources:GetObject("$this.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:Name = "SmartLinkTypeViewerControl":U.
        THIS-OBJECT:RightToLeft = CAST(resources:GetObject("$this.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:SecurityKey = resources:GetString("$this.SecurityKey":U).
        THIS-OBJECT:Size = CAST(resources:GetObject("$this.Size":U), System.Drawing.Size).
        THIS-OBJECT:SmartTableIOState = resources:GetString("$this.SmartTableIOState":U).
        THIS-OBJECT:ViewerLogicClass = resources:GetString("$this.ViewerLogicClass":U).
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartLinkType_LinkName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartLinkType_SourcePropertyName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartLinkType_SourceHandshakeMethod, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartLinkType_TargetPropertyName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartLinkType_TargetHandshakeMethod, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SmartLinkTypeViewerControl class
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartLinkTypeViewerControl ( ):

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
