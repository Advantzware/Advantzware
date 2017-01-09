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
    File        : SmartProductViewerControl
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 06.04.2014 11:19:02
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Base.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.Util.*                           FROM PROPATH .
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.Menu.SmartProductViewerControl 
    INHERITS SmartViewerControl: 
    
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.

    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartProduct_ProductCode_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartProduct_ProductCode AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartProduct_ProductName_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartProduct_ProductName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartProduct_ProductDescription_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartProduct_ProductDescription AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartProduct_ProductInstalled AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartProductViewerControl class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartProductViewerControl ():
                
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
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.Menu.SmartProductViewerControl":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc1 = NEW Progress.Data.TableDesc("eSmartProduct":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
        appearance3 = NEW Infragistics.Win.Appearance().
        THIS-OBJECT:smartBusinessEntityBindingSource1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:eSmartProduct_ProductCode_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartProduct_ProductCode = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartProduct_ProductName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartProduct_ProductName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartProduct_ProductDescription_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartProduct_ProductDescription = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartProduct_ProductInstalled = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartProduct_ProductCode, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartProduct_ProductName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartProduct_ProductDescription, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartProduct_ProductInstalled, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
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
        DEFINE VARIABLE arrayvar0 AS Progress.Data.ColumnPropDesc EXTENT 5 NO-UNDO.
        arrayvar0[1] = NEW Progress.Data.ColumnPropDesc("ProductGuid":U, "ProductGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[2] = NEW Progress.Data.ColumnPropDesc("ProductCode":U, "Code":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[3] = NEW Progress.Data.ColumnPropDesc("ProductName":U, "Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[4] = NEW Progress.Data.ColumnPropDesc("ProductDescription":U, "ProductDescription":U, Progress.Data.DataType:CLOB).
        arrayvar0[5] = NEW Progress.Data.ColumnPropDesc("ProductInstalled":U, "Installed":U, Progress.Data.DataType:LOGICAL).
        tableDesc1:Columns = arrayvar0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:TableSchema = tableDesc1.
        /*  */
        /* eSmartProduct_ProductCode_Label */
        /*  */
        THIS-OBJECT:eSmartProduct_ProductCode_Label:AccessibleDescription = resources:GetString("eSmartProduct_ProductCode_Label.AccessibleDescription":U).
        THIS-OBJECT:eSmartProduct_ProductCode_Label:AccessibleName = resources:GetString("eSmartProduct_ProductCode_Label.AccessibleName":U).
        THIS-OBJECT:eSmartProduct_ProductCode_Label:Anchor = CAST(resources:GetObject("eSmartProduct_ProductCode_Label.Anchor":U), System.Windows.Forms.AnchorStyles).
        appearance1:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        appearance1:TextHAlignAsString = resources:GetString("appearance1.TextHAlignAsString":U).
        appearance1:TextVAlignAsString = resources:GetString("appearance1.TextVAlignAsString":U).
        THIS-OBJECT:eSmartProduct_ProductCode_Label:Appearance = appearance1.
        THIS-OBJECT:eSmartProduct_ProductCode_Label:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartProduct_ProductCode_Label.AutoSize":U)).
        THIS-OBJECT:eSmartProduct_ProductCode_Label:BackgroundImage = CAST(resources:GetObject("eSmartProduct_ProductCode_Label.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartProduct_ProductCode_Label:BackgroundImageLayout = CAST(resources:GetObject("eSmartProduct_ProductCode_Label.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartProduct_ProductCode_Label:Dock = CAST(resources:GetObject("eSmartProduct_ProductCode_Label.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartProduct_ProductCode_Label:Font = CAST(resources:GetObject("eSmartProduct_ProductCode_Label.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartProduct_ProductCode_Label:ImeMode = CAST(resources:GetObject("eSmartProduct_ProductCode_Label.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartProduct_ProductCode_Label:Location = CAST(resources:GetObject("eSmartProduct_ProductCode_Label.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartProduct_ProductCode_Label:MaximumSize = CAST(resources:GetObject("eSmartProduct_ProductCode_Label.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartProduct_ProductCode_Label:Name = "eSmartProduct_ProductCode_Label":U.
        THIS-OBJECT:eSmartProduct_ProductCode_Label:RightToLeft = CAST(resources:GetObject("eSmartProduct_ProductCode_Label.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartProduct_ProductCode_Label:Size = CAST(resources:GetObject("eSmartProduct_ProductCode_Label.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartProduct_ProductCode_Label:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartProduct_ProductCode_Label.TabIndex":U)).
        THIS-OBJECT:eSmartProduct_ProductCode_Label:Text = resources:GetString("eSmartProduct_ProductCode_Label.Text":U).
        /*  */
        /* eSmartProduct_ProductCode */
        /*  */
        THIS-OBJECT:eSmartProduct_ProductCode:AccessibleDescription = resources:GetString("eSmartProduct_ProductCode.AccessibleDescription":U).
        THIS-OBJECT:eSmartProduct_ProductCode:AccessibleName = resources:GetString("eSmartProduct_ProductCode.AccessibleName":U).
        THIS-OBJECT:eSmartProduct_ProductCode:Anchor = CAST(resources:GetObject("eSmartProduct_ProductCode.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartProduct_ProductCode:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartProduct_ProductCode.AutoSize":U)).
        THIS-OBJECT:eSmartProduct_ProductCode:BackgroundImage = CAST(resources:GetObject("eSmartProduct_ProductCode.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartProduct_ProductCode:BackgroundImageLayout = CAST(resources:GetObject("eSmartProduct_ProductCode.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartProduct_ProductCode:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ProductCode":U, TRUE)).
        THIS-OBJECT:eSmartProduct_ProductCode:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartProduct_ProductCode:Dock = CAST(resources:GetObject("eSmartProduct_ProductCode.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartProduct_ProductCode:Font = CAST(resources:GetObject("eSmartProduct_ProductCode.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartProduct_ProductCode:ImeMode = CAST(resources:GetObject("eSmartProduct_ProductCode.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartProduct_ProductCode:Location = CAST(resources:GetObject("eSmartProduct_ProductCode.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartProduct_ProductCode:MaximumSize = CAST(resources:GetObject("eSmartProduct_ProductCode.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartProduct_ProductCode:Name = "eSmartProduct_ProductCode":U.
        THIS-OBJECT:eSmartProduct_ProductCode:RightToLeft = CAST(resources:GetObject("eSmartProduct_ProductCode.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartProduct_ProductCode:Size = CAST(resources:GetObject("eSmartProduct_ProductCode.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartProduct_ProductCode:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartProduct_ProductCode.TabIndex":U)).
        /*  */
        /* eSmartProduct_ProductName_Label */
        /*  */
        THIS-OBJECT:eSmartProduct_ProductName_Label:AccessibleDescription = resources:GetString("eSmartProduct_ProductName_Label.AccessibleDescription":U).
        THIS-OBJECT:eSmartProduct_ProductName_Label:AccessibleName = resources:GetString("eSmartProduct_ProductName_Label.AccessibleName":U).
        THIS-OBJECT:eSmartProduct_ProductName_Label:Anchor = CAST(resources:GetObject("eSmartProduct_ProductName_Label.Anchor":U), System.Windows.Forms.AnchorStyles).
        appearance2:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        appearance2:TextHAlignAsString = resources:GetString("appearance2.TextHAlignAsString":U).
        appearance2:TextVAlignAsString = resources:GetString("appearance2.TextVAlignAsString":U).
        THIS-OBJECT:eSmartProduct_ProductName_Label:Appearance = appearance2.
        THIS-OBJECT:eSmartProduct_ProductName_Label:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartProduct_ProductName_Label.AutoSize":U)).
        THIS-OBJECT:eSmartProduct_ProductName_Label:BackgroundImage = CAST(resources:GetObject("eSmartProduct_ProductName_Label.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartProduct_ProductName_Label:BackgroundImageLayout = CAST(resources:GetObject("eSmartProduct_ProductName_Label.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartProduct_ProductName_Label:Dock = CAST(resources:GetObject("eSmartProduct_ProductName_Label.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartProduct_ProductName_Label:Font = CAST(resources:GetObject("eSmartProduct_ProductName_Label.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartProduct_ProductName_Label:ImeMode = CAST(resources:GetObject("eSmartProduct_ProductName_Label.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartProduct_ProductName_Label:Location = CAST(resources:GetObject("eSmartProduct_ProductName_Label.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartProduct_ProductName_Label:MaximumSize = CAST(resources:GetObject("eSmartProduct_ProductName_Label.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartProduct_ProductName_Label:Name = "eSmartProduct_ProductName_Label":U.
        THIS-OBJECT:eSmartProduct_ProductName_Label:RightToLeft = CAST(resources:GetObject("eSmartProduct_ProductName_Label.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartProduct_ProductName_Label:Size = CAST(resources:GetObject("eSmartProduct_ProductName_Label.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartProduct_ProductName_Label:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartProduct_ProductName_Label.TabIndex":U)).
        THIS-OBJECT:eSmartProduct_ProductName_Label:Text = resources:GetString("eSmartProduct_ProductName_Label.Text":U).
        /*  */
        /* eSmartProduct_ProductName */
        /*  */
        THIS-OBJECT:eSmartProduct_ProductName:AccessibleDescription = resources:GetString("eSmartProduct_ProductName.AccessibleDescription":U).
        THIS-OBJECT:eSmartProduct_ProductName:AccessibleName = resources:GetString("eSmartProduct_ProductName.AccessibleName":U).
        THIS-OBJECT:eSmartProduct_ProductName:Anchor = CAST(resources:GetObject("eSmartProduct_ProductName.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartProduct_ProductName:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartProduct_ProductName.AutoSize":U)).
        THIS-OBJECT:eSmartProduct_ProductName:BackgroundImage = CAST(resources:GetObject("eSmartProduct_ProductName.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartProduct_ProductName:BackgroundImageLayout = CAST(resources:GetObject("eSmartProduct_ProductName.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartProduct_ProductName:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ProductName":U, TRUE)).
        THIS-OBJECT:eSmartProduct_ProductName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartProduct_ProductName:Dock = CAST(resources:GetObject("eSmartProduct_ProductName.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartProduct_ProductName:Font = CAST(resources:GetObject("eSmartProduct_ProductName.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartProduct_ProductName:ImeMode = CAST(resources:GetObject("eSmartProduct_ProductName.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartProduct_ProductName:Location = CAST(resources:GetObject("eSmartProduct_ProductName.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartProduct_ProductName:MaximumSize = CAST(resources:GetObject("eSmartProduct_ProductName.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartProduct_ProductName:Name = "eSmartProduct_ProductName":U.
        THIS-OBJECT:eSmartProduct_ProductName:RightToLeft = CAST(resources:GetObject("eSmartProduct_ProductName.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartProduct_ProductName:Size = CAST(resources:GetObject("eSmartProduct_ProductName.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartProduct_ProductName:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartProduct_ProductName.TabIndex":U)).
        /*  */
        /* eSmartProduct_ProductDescription_Label */
        /*  */
        THIS-OBJECT:eSmartProduct_ProductDescription_Label:AccessibleDescription = resources:GetString("eSmartProduct_ProductDescription_Label.AccessibleDescription":U).
        THIS-OBJECT:eSmartProduct_ProductDescription_Label:AccessibleName = resources:GetString("eSmartProduct_ProductDescription_Label.AccessibleName":U).
        THIS-OBJECT:eSmartProduct_ProductDescription_Label:Anchor = CAST(resources:GetObject("eSmartProduct_ProductDescription_Label.Anchor":U), System.Windows.Forms.AnchorStyles).
        appearance3:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        appearance3:TextHAlignAsString = resources:GetString("appearance3.TextHAlignAsString":U).
        appearance3:TextVAlignAsString = resources:GetString("appearance3.TextVAlignAsString":U).
        THIS-OBJECT:eSmartProduct_ProductDescription_Label:Appearance = appearance3.
        THIS-OBJECT:eSmartProduct_ProductDescription_Label:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartProduct_ProductDescription_Label.AutoSize":U)).
        THIS-OBJECT:eSmartProduct_ProductDescription_Label:BackgroundImage = CAST(resources:GetObject("eSmartProduct_ProductDescription_Label.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartProduct_ProductDescription_Label:BackgroundImageLayout = CAST(resources:GetObject("eSmartProduct_ProductDescription_Label.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartProduct_ProductDescription_Label:Dock = CAST(resources:GetObject("eSmartProduct_ProductDescription_Label.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartProduct_ProductDescription_Label:Font = CAST(resources:GetObject("eSmartProduct_ProductDescription_Label.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartProduct_ProductDescription_Label:ImeMode = CAST(resources:GetObject("eSmartProduct_ProductDescription_Label.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartProduct_ProductDescription_Label:Location = CAST(resources:GetObject("eSmartProduct_ProductDescription_Label.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartProduct_ProductDescription_Label:MaximumSize = CAST(resources:GetObject("eSmartProduct_ProductDescription_Label.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartProduct_ProductDescription_Label:Name = "eSmartProduct_ProductDescription_Label":U.
        THIS-OBJECT:eSmartProduct_ProductDescription_Label:RightToLeft = CAST(resources:GetObject("eSmartProduct_ProductDescription_Label.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartProduct_ProductDescription_Label:Size = CAST(resources:GetObject("eSmartProduct_ProductDescription_Label.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartProduct_ProductDescription_Label:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartProduct_ProductDescription_Label.TabIndex":U)).
        THIS-OBJECT:eSmartProduct_ProductDescription_Label:Text = resources:GetString("eSmartProduct_ProductDescription_Label.Text":U).
        /*  */
        /* eSmartProduct_ProductDescription */
        /*  */
        THIS-OBJECT:eSmartProduct_ProductDescription:AccessibleDescription = resources:GetString("eSmartProduct_ProductDescription.AccessibleDescription":U).
        THIS-OBJECT:eSmartProduct_ProductDescription:AccessibleName = resources:GetString("eSmartProduct_ProductDescription.AccessibleName":U).
        THIS-OBJECT:eSmartProduct_ProductDescription:Anchor = CAST(resources:GetObject("eSmartProduct_ProductDescription.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartProduct_ProductDescription:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartProduct_ProductDescription.AutoSize":U)).
        THIS-OBJECT:eSmartProduct_ProductDescription:BackgroundImage = CAST(resources:GetObject("eSmartProduct_ProductDescription.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartProduct_ProductDescription:BackgroundImageLayout = CAST(resources:GetObject("eSmartProduct_ProductDescription.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartProduct_ProductDescription:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ProductDescription":U, TRUE)).
        THIS-OBJECT:eSmartProduct_ProductDescription:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartProduct_ProductDescription:Dock = CAST(resources:GetObject("eSmartProduct_ProductDescription.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartProduct_ProductDescription:Font = CAST(resources:GetObject("eSmartProduct_ProductDescription.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartProduct_ProductDescription:ImeMode = CAST(resources:GetObject("eSmartProduct_ProductDescription.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartProduct_ProductDescription:Location = CAST(resources:GetObject("eSmartProduct_ProductDescription.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartProduct_ProductDescription:MaximumSize = CAST(resources:GetObject("eSmartProduct_ProductDescription.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartProduct_ProductDescription:Multiline = System.Convert:ToBoolean(resources:GetObject("eSmartProduct_ProductDescription.Multiline":U)).
        THIS-OBJECT:eSmartProduct_ProductDescription:Name = "eSmartProduct_ProductDescription":U.
        THIS-OBJECT:eSmartProduct_ProductDescription:RightToLeft = CAST(resources:GetObject("eSmartProduct_ProductDescription.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartProduct_ProductDescription:Size = CAST(resources:GetObject("eSmartProduct_ProductDescription.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartProduct_ProductDescription:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartProduct_ProductDescription.TabIndex":U)).
        /*  */
        /* eSmartProduct_ProductInstalled */
        /*  */
        THIS-OBJECT:eSmartProduct_ProductInstalled:AccessibleDescription = resources:GetString("eSmartProduct_ProductInstalled.AccessibleDescription":U).
        THIS-OBJECT:eSmartProduct_ProductInstalled:AccessibleName = resources:GetString("eSmartProduct_ProductInstalled.AccessibleName":U).
        THIS-OBJECT:eSmartProduct_ProductInstalled:Anchor = CAST(resources:GetObject("eSmartProduct_ProductInstalled.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartProduct_ProductInstalled:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartProduct_ProductInstalled.AutoSize":U)).
        THIS-OBJECT:eSmartProduct_ProductInstalled:BackgroundImage = CAST(resources:GetObject("eSmartProduct_ProductInstalled.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartProduct_ProductInstalled:BackgroundImageLayout = CAST(resources:GetObject("eSmartProduct_ProductInstalled.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartProduct_ProductInstalled:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ProductInstalled":U, TRUE)).
        THIS-OBJECT:eSmartProduct_ProductInstalled:Dock = CAST(resources:GetObject("eSmartProduct_ProductInstalled.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartProduct_ProductInstalled:Font = CAST(resources:GetObject("eSmartProduct_ProductInstalled.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartProduct_ProductInstalled:ImeMode = CAST(resources:GetObject("eSmartProduct_ProductInstalled.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartProduct_ProductInstalled:Location = CAST(resources:GetObject("eSmartProduct_ProductInstalled.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartProduct_ProductInstalled:MaximumSize = CAST(resources:GetObject("eSmartProduct_ProductInstalled.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartProduct_ProductInstalled:Name = "eSmartProduct_ProductInstalled":U.
        THIS-OBJECT:eSmartProduct_ProductInstalled:RightToLeft = CAST(resources:GetObject("eSmartProduct_ProductInstalled.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartProduct_ProductInstalled:Size = CAST(resources:GetObject("eSmartProduct_ProductInstalled.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartProduct_ProductInstalled:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartProduct_ProductInstalled.TabIndex":U)).
        THIS-OBJECT:eSmartProduct_ProductInstalled:Text = resources:GetString("eSmartProduct_ProductInstalled.Text":U).
        THIS-OBJECT:eSmartProduct_ProductInstalled:UseMnemonics = TRUE.
        /*  */
        /* SmartProductViewerControl */
        /*  */
        THIS-OBJECT:AccessibleDescription = resources:GetString("$this.AccessibleDescription":U).
        THIS-OBJECT:AccessibleName = resources:GetString("$this.AccessibleName":U).
        THIS-OBJECT:AutoScaleDimensions = CAST(resources:GetObject("$this.AutoScaleDimensions":U), System.Drawing.SizeF).
        THIS-OBJECT:AutoScroll = System.Convert:ToBoolean(resources:GetObject("$this.AutoScroll":U)).
        THIS-OBJECT:AutoSize = System.Convert:ToBoolean(resources:GetObject("$this.AutoSize":U)).
        THIS-OBJECT:AutoSizeMode = CAST(resources:GetObject("$this.AutoSizeMode":U), System.Windows.Forms.AutoSizeMode).
        THIS-OBJECT:BackgroundImage = CAST(resources:GetObject("$this.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:BackgroundImageLayout = CAST(resources:GetObject("$this.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartProduct_ProductCode_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartProduct_ProductCode).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartProduct_ProductName_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartProduct_ProductName).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartProduct_ProductDescription_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartProduct_ProductDescription).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartProduct_ProductInstalled).
        THIS-OBJECT:Font = CAST(resources:GetObject("$this.Font":U), System.Drawing.Font).
        THIS-OBJECT:ImeMode = CAST(resources:GetObject("$this.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:Location = CAST(resources:GetObject("$this.Location":U), System.Drawing.Point).
        THIS-OBJECT:MaximumSize = CAST(resources:GetObject("$this.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:Name = "SmartProductViewerControl":U.
        THIS-OBJECT:RightToLeft = CAST(resources:GetObject("$this.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:SecurityKey = resources:GetString("$this.SecurityKey":U).
        THIS-OBJECT:Size = CAST(resources:GetObject("$this.Size":U), System.Drawing.Size).
        THIS-OBJECT:SmartTableIOState = resources:GetString("$this.SmartTableIOState":U).
        THIS-OBJECT:ViewerLogicClass = resources:GetString("$this.ViewerLogicClass":U).
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartProduct_ProductCode, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartProduct_ProductName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartProduct_ProductDescription, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartProduct_ProductInstalled, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SmartProductViewerControl class
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartProductViewerControl ( ):

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
