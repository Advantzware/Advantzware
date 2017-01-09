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
    File        : SmartCustomizationTypeViewerControl
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : 21.05.2014 11:04:23
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Base.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.Util.*                           FROM PROPATH .
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.Repository.Customization.SmartCustomizationTypeViewerControl 
    INHERITS SmartViewerControl: 
    
    DEFINE PRIVATE VARIABLE components                                                 AS System.ComponentModel.IContainer                                               NO-UNDO.

    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1                          AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartCustomizationType_CustomizationTypeCode_Label        AS Infragistics.Win.Misc.UltraLabel                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartCustomizationType_CustomizationTypeCode              AS Infragistics.Win.UltraWinEditors.UltraTextEditor                               NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartCustomizationType_CustomizationTypeDescription_Label AS Infragistics.Win.Misc.UltraLabel                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartCustomizationType_CustomizationTypeDescription       AS Infragistics.Win.UltraWinEditors.UltraTextEditor                               NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartCustomizationType_ServiceTypeName_Label              AS Infragistics.Win.Misc.UltraLabel                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartCustomizationType_ServiceTypeName                    AS Infragistics.Win.UltraWinEditors.UltraTextEditor                               NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartCustomizationType_ServiceMethodName_Label            AS Infragistics.Win.Misc.UltraLabel                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartCustomizationType_ServiceMethodName                  AS Infragistics.Win.UltraWinEditors.UltraTextEditor                               NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartCustomizationTypeViewerControl class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartCustomizationTypeViewerControl ():
                
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
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.Repository.Customization.SmartCustomizationTypeViewerControl":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc1 = NEW Progress.Data.TableDesc("eSmartCustomizationType":U).
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
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeCode_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeCode = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeDescription_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeDescription = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartCustomizationType_ServiceTypeName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartCustomizationType_ServiceTypeName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartCustomizationType_ServiceMethodName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartCustomizationType_ServiceMethodName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartCustomizationType_CustomizationTypeCode, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartCustomizationType_CustomizationTypeDescription, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartCustomizationType_ServiceTypeName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartCustomizationType_ServiceMethodName, System.ComponentModel.ISupportInitialize):BeginInit().
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
        DEFINE VARIABLE arrayvar0 AS Progress.Data.ColumnPropDesc EXTENT 5 NO-UNDO.
        arrayvar0[1] = NEW Progress.Data.ColumnPropDesc("CustomizationTypeGuid":U, "CustomizationTypeGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[2] = NEW Progress.Data.ColumnPropDesc("CustomizationTypeCode":U, "Customization Type":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[3] = NEW Progress.Data.ColumnPropDesc("CustomizationTypeDescription":U, "Description":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[4] = NEW Progress.Data.ColumnPropDesc("ServiceTypeName":U, "Service Type Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[5] = NEW Progress.Data.ColumnPropDesc("ServiceMethodName":U, "Method Name":U, Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:TableSchema = tableDesc1.
        /*  */
        /* eSmartCustomizationType_CustomizationTypeCode_Label */
        /*  */
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeCode_Label:AccessibleDescription = resources:GetString("eSmartCustomizationType_CustomizationTypeCode_Label.AccessibleDescription":U).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeCode_Label:AccessibleName = resources:GetString("eSmartCustomizationType_CustomizationTypeCode_Label.AccessibleName":U).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeCode_Label:Anchor = CAST(resources:GetObject("eSmartCustomizationType_CustomizationTypeCode_Label.Anchor":U), System.Windows.Forms.AnchorStyles).
        appearance1:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        appearance1:TextHAlignAsString = resources:GetString("appearance1.TextHAlignAsString":U).
        appearance1:TextVAlignAsString = resources:GetString("appearance1.TextVAlignAsString":U).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeCode_Label:Appearance = appearance1.
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeCode_Label:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartCustomizationType_CustomizationTypeCode_Label.AutoSize":U)).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeCode_Label:BackgroundImage = CAST(resources:GetObject("eSmartCustomizationType_CustomizationTypeCode_Label.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeCode_Label:BackgroundImageLayout = CAST(resources:GetObject("eSmartCustomizationType_CustomizationTypeCode_Label.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeCode_Label:Dock = CAST(resources:GetObject("eSmartCustomizationType_CustomizationTypeCode_Label.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeCode_Label:Font = CAST(resources:GetObject("eSmartCustomizationType_CustomizationTypeCode_Label.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeCode_Label:ImeMode = CAST(resources:GetObject("eSmartCustomizationType_CustomizationTypeCode_Label.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeCode_Label:Location = CAST(resources:GetObject("eSmartCustomizationType_CustomizationTypeCode_Label.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeCode_Label:MaximumSize = CAST(resources:GetObject("eSmartCustomizationType_CustomizationTypeCode_Label.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeCode_Label:Name = "eSmartCustomizationType_CustomizationTypeCode_Label":U.
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeCode_Label:RightToLeft = CAST(resources:GetObject("eSmartCustomizationType_CustomizationTypeCode_Label.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeCode_Label:Size = CAST(resources:GetObject("eSmartCustomizationType_CustomizationTypeCode_Label.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeCode_Label:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartCustomizationType_CustomizationTypeCode_Label.TabIndex":U)).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeCode_Label:Text = resources:GetString("eSmartCustomizationType_CustomizationTypeCode_Label.Text":U).
        /*  */
        /* eSmartCustomizationType_CustomizationTypeCode */
        /*  */
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeCode:AccessibleDescription = resources:GetString("eSmartCustomizationType_CustomizationTypeCode.AccessibleDescription":U).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeCode:AccessibleName = resources:GetString("eSmartCustomizationType_CustomizationTypeCode.AccessibleName":U).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeCode:Anchor = CAST(resources:GetObject("eSmartCustomizationType_CustomizationTypeCode.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeCode:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartCustomizationType_CustomizationTypeCode.AutoSize":U)).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeCode:BackgroundImage = CAST(resources:GetObject("eSmartCustomizationType_CustomizationTypeCode.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeCode:BackgroundImageLayout = CAST(resources:GetObject("eSmartCustomizationType_CustomizationTypeCode.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeCode:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "CustomizationTypeCode":U, TRUE)).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeCode:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeCode:Dock = CAST(resources:GetObject("eSmartCustomizationType_CustomizationTypeCode.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeCode:Font = CAST(resources:GetObject("eSmartCustomizationType_CustomizationTypeCode.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeCode:ImeMode = CAST(resources:GetObject("eSmartCustomizationType_CustomizationTypeCode.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeCode:Location = CAST(resources:GetObject("eSmartCustomizationType_CustomizationTypeCode.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeCode:MaximumSize = CAST(resources:GetObject("eSmartCustomizationType_CustomizationTypeCode.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeCode:Name = "eSmartCustomizationType_CustomizationTypeCode":U.
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeCode:RightToLeft = CAST(resources:GetObject("eSmartCustomizationType_CustomizationTypeCode.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeCode:Size = CAST(resources:GetObject("eSmartCustomizationType_CustomizationTypeCode.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeCode:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartCustomizationType_CustomizationTypeCode.TabIndex":U)).
        /*  */
        /* eSmartCustomizationType_CustomizationTypeDescription_Label */
        /*  */
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeDescription_Label:AccessibleDescription = resources:GetString("eSmartCustomizationType_CustomizationTypeDescription_Label.AccessibleDescription":U).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeDescription_Label:AccessibleName = resources:GetString("eSmartCustomizationType_CustomizationTypeDescription_Label.AccessibleName":U).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeDescription_Label:Anchor = CAST(resources:GetObject("eSmartCustomizationType_CustomizationTypeDescription_Label.Anchor":U), System.Windows.Forms.AnchorStyles).
        appearance2:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        appearance2:TextHAlignAsString = resources:GetString("appearance2.TextHAlignAsString":U).
        appearance2:TextVAlignAsString = resources:GetString("appearance2.TextVAlignAsString":U).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeDescription_Label:Appearance = appearance2.
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeDescription_Label:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartCustomizationType_CustomizationTypeDescription_Label.AutoSize":U)).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeDescription_Label:BackgroundImage = CAST(resources:GetObject("eSmartCustomizationType_CustomizationTypeDescription_Label.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeDescription_Label:BackgroundImageLayout = CAST(resources:GetObject("eSmartCustomizationType_CustomizationTypeDescription_Label.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeDescription_Label:Dock = CAST(resources:GetObject("eSmartCustomizationType_CustomizationTypeDescription_Label.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeDescription_Label:Font = CAST(resources:GetObject("eSmartCustomizationType_CustomizationTypeDescription_Label.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeDescription_Label:ImeMode = CAST(resources:GetObject("eSmartCustomizationType_CustomizationTypeDescription_Label.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeDescription_Label:Location = CAST(resources:GetObject("eSmartCustomizationType_CustomizationTypeDescription_Label.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeDescription_Label:MaximumSize = CAST(resources:GetObject("eSmartCustomizationType_CustomizationTypeDescription_Label.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeDescription_Label:Name = "eSmartCustomizationType_CustomizationTypeDescription_Label":U.
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeDescription_Label:RightToLeft = CAST(resources:GetObject("eSmartCustomizationType_CustomizationTypeDescription_Label.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeDescription_Label:Size = CAST(resources:GetObject("eSmartCustomizationType_CustomizationTypeDescription_Label.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeDescription_Label:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartCustomizationType_CustomizationTypeDescription_Label.TabIndex":U)).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeDescription_Label:Text = resources:GetString("eSmartCustomizationType_CustomizationTypeDescription_Label.Text":U).
        /*  */
        /* eSmartCustomizationType_CustomizationTypeDescription */
        /*  */
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeDescription:AccessibleDescription = resources:GetString("eSmartCustomizationType_CustomizationTypeDescription.AccessibleDescription":U).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeDescription:AccessibleName = resources:GetString("eSmartCustomizationType_CustomizationTypeDescription.AccessibleName":U).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeDescription:Anchor = CAST(resources:GetObject("eSmartCustomizationType_CustomizationTypeDescription.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeDescription:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartCustomizationType_CustomizationTypeDescription.AutoSize":U)).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeDescription:BackgroundImage = CAST(resources:GetObject("eSmartCustomizationType_CustomizationTypeDescription.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeDescription:BackgroundImageLayout = CAST(resources:GetObject("eSmartCustomizationType_CustomizationTypeDescription.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeDescription:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "CustomizationTypeDescription":U, TRUE)).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeDescription:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeDescription:Dock = CAST(resources:GetObject("eSmartCustomizationType_CustomizationTypeDescription.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeDescription:Font = CAST(resources:GetObject("eSmartCustomizationType_CustomizationTypeDescription.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeDescription:ImeMode = CAST(resources:GetObject("eSmartCustomizationType_CustomizationTypeDescription.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeDescription:Location = CAST(resources:GetObject("eSmartCustomizationType_CustomizationTypeDescription.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeDescription:MaximumSize = CAST(resources:GetObject("eSmartCustomizationType_CustomizationTypeDescription.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeDescription:Multiline = System.Convert:ToBoolean(resources:GetObject("eSmartCustomizationType_CustomizationTypeDescription.Multiline":U)).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeDescription:Name = "eSmartCustomizationType_CustomizationTypeDescription":U.
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeDescription:RightToLeft = CAST(resources:GetObject("eSmartCustomizationType_CustomizationTypeDescription.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeDescription:Size = CAST(resources:GetObject("eSmartCustomizationType_CustomizationTypeDescription.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeDescription:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartCustomizationType_CustomizationTypeDescription.TabIndex":U)).
        /*  */
        /* eSmartCustomizationType_ServiceTypeName_Label */
        /*  */
        THIS-OBJECT:eSmartCustomizationType_ServiceTypeName_Label:AccessibleDescription = resources:GetString("eSmartCustomizationType_ServiceTypeName_Label.AccessibleDescription":U).
        THIS-OBJECT:eSmartCustomizationType_ServiceTypeName_Label:AccessibleName = resources:GetString("eSmartCustomizationType_ServiceTypeName_Label.AccessibleName":U).
        THIS-OBJECT:eSmartCustomizationType_ServiceTypeName_Label:Anchor = CAST(resources:GetObject("eSmartCustomizationType_ServiceTypeName_Label.Anchor":U), System.Windows.Forms.AnchorStyles).
        appearance3:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        appearance3:TextHAlignAsString = resources:GetString("appearance3.TextHAlignAsString":U).
        appearance3:TextVAlignAsString = resources:GetString("appearance3.TextVAlignAsString":U).
        THIS-OBJECT:eSmartCustomizationType_ServiceTypeName_Label:Appearance = appearance3.
        THIS-OBJECT:eSmartCustomizationType_ServiceTypeName_Label:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartCustomizationType_ServiceTypeName_Label.AutoSize":U)).
        THIS-OBJECT:eSmartCustomizationType_ServiceTypeName_Label:BackgroundImage = CAST(resources:GetObject("eSmartCustomizationType_ServiceTypeName_Label.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartCustomizationType_ServiceTypeName_Label:BackgroundImageLayout = CAST(resources:GetObject("eSmartCustomizationType_ServiceTypeName_Label.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartCustomizationType_ServiceTypeName_Label:Dock = CAST(resources:GetObject("eSmartCustomizationType_ServiceTypeName_Label.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartCustomizationType_ServiceTypeName_Label:Font = CAST(resources:GetObject("eSmartCustomizationType_ServiceTypeName_Label.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartCustomizationType_ServiceTypeName_Label:ImeMode = CAST(resources:GetObject("eSmartCustomizationType_ServiceTypeName_Label.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartCustomizationType_ServiceTypeName_Label:Location = CAST(resources:GetObject("eSmartCustomizationType_ServiceTypeName_Label.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartCustomizationType_ServiceTypeName_Label:MaximumSize = CAST(resources:GetObject("eSmartCustomizationType_ServiceTypeName_Label.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartCustomizationType_ServiceTypeName_Label:Name = "eSmartCustomizationType_ServiceTypeName_Label":U.
        THIS-OBJECT:eSmartCustomizationType_ServiceTypeName_Label:RightToLeft = CAST(resources:GetObject("eSmartCustomizationType_ServiceTypeName_Label.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartCustomizationType_ServiceTypeName_Label:Size = CAST(resources:GetObject("eSmartCustomizationType_ServiceTypeName_Label.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartCustomizationType_ServiceTypeName_Label:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartCustomizationType_ServiceTypeName_Label.TabIndex":U)).
        THIS-OBJECT:eSmartCustomizationType_ServiceTypeName_Label:Text = resources:GetString("eSmartCustomizationType_ServiceTypeName_Label.Text":U).
        /*  */
        /* eSmartCustomizationType_ServiceTypeName */
        /*  */
        THIS-OBJECT:eSmartCustomizationType_ServiceTypeName:AccessibleDescription = resources:GetString("eSmartCustomizationType_ServiceTypeName.AccessibleDescription":U).
        THIS-OBJECT:eSmartCustomizationType_ServiceTypeName:AccessibleName = resources:GetString("eSmartCustomizationType_ServiceTypeName.AccessibleName":U).
        THIS-OBJECT:eSmartCustomizationType_ServiceTypeName:Anchor = CAST(resources:GetObject("eSmartCustomizationType_ServiceTypeName.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartCustomizationType_ServiceTypeName:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartCustomizationType_ServiceTypeName.AutoSize":U)).
        THIS-OBJECT:eSmartCustomizationType_ServiceTypeName:BackgroundImage = CAST(resources:GetObject("eSmartCustomizationType_ServiceTypeName.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartCustomizationType_ServiceTypeName:BackgroundImageLayout = CAST(resources:GetObject("eSmartCustomizationType_ServiceTypeName.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartCustomizationType_ServiceTypeName:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ServiceTypeName":U, TRUE)).
        THIS-OBJECT:eSmartCustomizationType_ServiceTypeName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartCustomizationType_ServiceTypeName:Dock = CAST(resources:GetObject("eSmartCustomizationType_ServiceTypeName.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartCustomizationType_ServiceTypeName:Font = CAST(resources:GetObject("eSmartCustomizationType_ServiceTypeName.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartCustomizationType_ServiceTypeName:ImeMode = CAST(resources:GetObject("eSmartCustomizationType_ServiceTypeName.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartCustomizationType_ServiceTypeName:Location = CAST(resources:GetObject("eSmartCustomizationType_ServiceTypeName.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartCustomizationType_ServiceTypeName:MaximumSize = CAST(resources:GetObject("eSmartCustomizationType_ServiceTypeName.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartCustomizationType_ServiceTypeName:Name = "eSmartCustomizationType_ServiceTypeName":U.
        THIS-OBJECT:eSmartCustomizationType_ServiceTypeName:RightToLeft = CAST(resources:GetObject("eSmartCustomizationType_ServiceTypeName.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartCustomizationType_ServiceTypeName:Size = CAST(resources:GetObject("eSmartCustomizationType_ServiceTypeName.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartCustomizationType_ServiceTypeName:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartCustomizationType_ServiceTypeName.TabIndex":U)).
        /*  */
        /* eSmartCustomizationType_ServiceMethodName_Label */
        /*  */
        THIS-OBJECT:eSmartCustomizationType_ServiceMethodName_Label:AccessibleDescription = resources:GetString("eSmartCustomizationType_ServiceMethodName_Label.AccessibleDescription":U).
        THIS-OBJECT:eSmartCustomizationType_ServiceMethodName_Label:AccessibleName = resources:GetString("eSmartCustomizationType_ServiceMethodName_Label.AccessibleName":U).
        THIS-OBJECT:eSmartCustomizationType_ServiceMethodName_Label:Anchor = CAST(resources:GetObject("eSmartCustomizationType_ServiceMethodName_Label.Anchor":U), System.Windows.Forms.AnchorStyles).
        appearance4:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        appearance4:TextHAlignAsString = resources:GetString("appearance4.TextHAlignAsString":U).
        appearance4:TextVAlignAsString = resources:GetString("appearance4.TextVAlignAsString":U).
        THIS-OBJECT:eSmartCustomizationType_ServiceMethodName_Label:Appearance = appearance4.
        THIS-OBJECT:eSmartCustomizationType_ServiceMethodName_Label:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartCustomizationType_ServiceMethodName_Label.AutoSize":U)).
        THIS-OBJECT:eSmartCustomizationType_ServiceMethodName_Label:BackgroundImage = CAST(resources:GetObject("eSmartCustomizationType_ServiceMethodName_Label.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartCustomizationType_ServiceMethodName_Label:BackgroundImageLayout = CAST(resources:GetObject("eSmartCustomizationType_ServiceMethodName_Label.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartCustomizationType_ServiceMethodName_Label:Dock = CAST(resources:GetObject("eSmartCustomizationType_ServiceMethodName_Label.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartCustomizationType_ServiceMethodName_Label:Font = CAST(resources:GetObject("eSmartCustomizationType_ServiceMethodName_Label.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartCustomizationType_ServiceMethodName_Label:ImeMode = CAST(resources:GetObject("eSmartCustomizationType_ServiceMethodName_Label.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartCustomizationType_ServiceMethodName_Label:Location = CAST(resources:GetObject("eSmartCustomizationType_ServiceMethodName_Label.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartCustomizationType_ServiceMethodName_Label:MaximumSize = CAST(resources:GetObject("eSmartCustomizationType_ServiceMethodName_Label.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartCustomizationType_ServiceMethodName_Label:Name = "eSmartCustomizationType_ServiceMethodName_Label":U.
        THIS-OBJECT:eSmartCustomizationType_ServiceMethodName_Label:RightToLeft = CAST(resources:GetObject("eSmartCustomizationType_ServiceMethodName_Label.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartCustomizationType_ServiceMethodName_Label:Size = CAST(resources:GetObject("eSmartCustomizationType_ServiceMethodName_Label.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartCustomizationType_ServiceMethodName_Label:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartCustomizationType_ServiceMethodName_Label.TabIndex":U)).
        THIS-OBJECT:eSmartCustomizationType_ServiceMethodName_Label:Text = resources:GetString("eSmartCustomizationType_ServiceMethodName_Label.Text":U).
        /*  */
        /* eSmartCustomizationType_ServiceMethodName */
        /*  */
        THIS-OBJECT:eSmartCustomizationType_ServiceMethodName:AccessibleDescription = resources:GetString("eSmartCustomizationType_ServiceMethodName.AccessibleDescription":U).
        THIS-OBJECT:eSmartCustomizationType_ServiceMethodName:AccessibleName = resources:GetString("eSmartCustomizationType_ServiceMethodName.AccessibleName":U).
        THIS-OBJECT:eSmartCustomizationType_ServiceMethodName:Anchor = CAST(resources:GetObject("eSmartCustomizationType_ServiceMethodName.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartCustomizationType_ServiceMethodName:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartCustomizationType_ServiceMethodName.AutoSize":U)).
        THIS-OBJECT:eSmartCustomizationType_ServiceMethodName:BackgroundImage = CAST(resources:GetObject("eSmartCustomizationType_ServiceMethodName.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartCustomizationType_ServiceMethodName:BackgroundImageLayout = CAST(resources:GetObject("eSmartCustomizationType_ServiceMethodName.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartCustomizationType_ServiceMethodName:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ServiceMethodName":U, TRUE)).
        THIS-OBJECT:eSmartCustomizationType_ServiceMethodName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartCustomizationType_ServiceMethodName:Dock = CAST(resources:GetObject("eSmartCustomizationType_ServiceMethodName.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartCustomizationType_ServiceMethodName:Font = CAST(resources:GetObject("eSmartCustomizationType_ServiceMethodName.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartCustomizationType_ServiceMethodName:ImeMode = CAST(resources:GetObject("eSmartCustomizationType_ServiceMethodName.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartCustomizationType_ServiceMethodName:Location = CAST(resources:GetObject("eSmartCustomizationType_ServiceMethodName.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartCustomizationType_ServiceMethodName:MaximumSize = CAST(resources:GetObject("eSmartCustomizationType_ServiceMethodName.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartCustomizationType_ServiceMethodName:Name = "eSmartCustomizationType_ServiceMethodName":U.
        THIS-OBJECT:eSmartCustomizationType_ServiceMethodName:RightToLeft = CAST(resources:GetObject("eSmartCustomizationType_ServiceMethodName.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartCustomizationType_ServiceMethodName:Size = CAST(resources:GetObject("eSmartCustomizationType_ServiceMethodName.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartCustomizationType_ServiceMethodName:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartCustomizationType_ServiceMethodName.TabIndex":U)).
        /*  */
        /* SmartCustomizationTypeViewerControl */
        /*  */
        THIS-OBJECT:AccessibleDescription = resources:GetString("$this.AccessibleDescription":U).
        THIS-OBJECT:AccessibleName = resources:GetString("$this.AccessibleName":U).
        THIS-OBJECT:AutoScaleDimensions = CAST(resources:GetObject("$this.AutoScaleDimensions":U), System.Drawing.SizeF).
        THIS-OBJECT:AutoScroll = System.Convert:ToBoolean(resources:GetObject("$this.AutoScroll":U)).
        THIS-OBJECT:AutoSize = System.Convert:ToBoolean(resources:GetObject("$this.AutoSize":U)).
        THIS-OBJECT:AutoSizeMode = CAST(resources:GetObject("$this.AutoSizeMode":U), System.Windows.Forms.AutoSizeMode).
        THIS-OBJECT:BackgroundImage = CAST(resources:GetObject("$this.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:BackgroundImageLayout = CAST(resources:GetObject("$this.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartCustomizationType_CustomizationTypeCode_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartCustomizationType_CustomizationTypeCode).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartCustomizationType_CustomizationTypeDescription_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartCustomizationType_CustomizationTypeDescription).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartCustomizationType_ServiceTypeName_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartCustomizationType_ServiceTypeName).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartCustomizationType_ServiceMethodName_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartCustomizationType_ServiceMethodName).
        THIS-OBJECT:Font = CAST(resources:GetObject("$this.Font":U), System.Drawing.Font).
        THIS-OBJECT:ImeMode = CAST(resources:GetObject("$this.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:Location = CAST(resources:GetObject("$this.Location":U), System.Drawing.Point).
        THIS-OBJECT:MaximumSize = CAST(resources:GetObject("$this.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:Name = "SmartCustomizationTypeViewerControl":U.
        THIS-OBJECT:RightToLeft = CAST(resources:GetObject("$this.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:SecurityKey = resources:GetString("$this.SecurityKey":U).
        THIS-OBJECT:Size = CAST(resources:GetObject("$this.Size":U), System.Drawing.Size).
        THIS-OBJECT:SmartTableIOState = resources:GetString("$this.SmartTableIOState":U).
        THIS-OBJECT:ViewerLogicClass = resources:GetString("$this.ViewerLogicClass":U).
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartCustomizationType_CustomizationTypeCode, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartCustomizationType_CustomizationTypeDescription, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartCustomizationType_ServiceTypeName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartCustomizationType_ServiceMethodName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SmartCustomizationTypeViewerControl class
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartCustomizationTypeViewerControl ( ):

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
