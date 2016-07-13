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
    File        : SmartToolbarSecurityItemViewerControl
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 20.04.2014 11:46:13
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Base.*            FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*            FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.*  FROM PROPATH .
USING Consultingwerk.Windows.Framework.Authorization.* FROM PROPATH . 
USING Consultingwerk.Util.*                            FROM PROPATH .
USING Infragistics.Win.UltraWinEditors.*               FROM ASSEMBLY .
USING Progress.Lang.*                                  FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.Authorization.SmartToolbarSecurityItemViewerControl 
    INHERITS SmartViewerControl: 
    
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.

    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartToolbarSecurityItem_ObjectName_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartToolbarSecurityItem_ObjectName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartToolbarSecurityItem_ToolbarItemKey_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartToolbarSecurityItem_Description_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartToolbarSecurityItem_Description AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraComboEditor1 AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartToolbarSecurityItemViewerControl class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartToolbarSecurityItemViewerControl ():
                
        SUPER().
        InitializeComponent().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the EditorButtonClick event of the eSmartToolbarSecurityItem_ObjectName
        Notes:   
        @param sender The reference to the object that raised this event
        @param e The EditorButtonEventArgs with the data for this event  
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID eSmartToolbarSecurityItem_ObjectName_EditorButtonClick (sender AS System.Object,   
	                                                                            e AS Infragistics.Win.UltraWinEditors.EditorButtonEventArgs):
		
		THIS-OBJECT:LookupToolItemContainer () .

	END METHOD .

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
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.Authorization.SmartToolbarSecurityItemViewerControl":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc1 = NEW Progress.Data.TableDesc("eSmartToolbarSecurityItem":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton1 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton1 = NEW Infragistics.Win.UltraWinEditors.EditorButton().
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
        THIS-OBJECT:eSmartToolbarSecurityItem_ObjectName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartToolbarSecurityItem_ObjectName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartToolbarSecurityItem_ToolbarItemKey_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartToolbarSecurityItem_Description_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartToolbarSecurityItem_Description = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraComboEditor1 = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartToolbarSecurityItem_ObjectName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartToolbarSecurityItem_Description, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraComboEditor1, System.ComponentModel.ISupportInitialize):BeginInit().
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
        DEFINE VARIABLE arrayvar0 AS Progress.Data.ColumnPropDesc EXTENT 4 NO-UNDO.
        arrayvar0[1] = NEW Progress.Data.ColumnPropDesc("ToolbarSecurityItemGuid":U, "ToolbarSecurityItemGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[2] = NEW Progress.Data.ColumnPropDesc("ObjectName":U, "Container / Object Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[3] = NEW Progress.Data.ColumnPropDesc("ToolbarItemKey":U, "Toolbar Item":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[4] = NEW Progress.Data.ColumnPropDesc("Description":U, "Description":U, Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:TableSchema = tableDesc1.
        /*  */
        /* eSmartToolbarSecurityItem_ObjectName_Label */
        /*  */
        THIS-OBJECT:eSmartToolbarSecurityItem_ObjectName_Label:AccessibleDescription = resources:GetString("eSmartToolbarSecurityItem_ObjectName_Label.AccessibleDescription":U).
        THIS-OBJECT:eSmartToolbarSecurityItem_ObjectName_Label:AccessibleName = resources:GetString("eSmartToolbarSecurityItem_ObjectName_Label.AccessibleName":U).
        THIS-OBJECT:eSmartToolbarSecurityItem_ObjectName_Label:Anchor = CAST(resources:GetObject("eSmartToolbarSecurityItem_ObjectName_Label.Anchor":U), System.Windows.Forms.AnchorStyles).
        appearance1:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        appearance1:TextHAlignAsString = resources:GetString("appearance1.TextHAlignAsString":U).
        appearance1:TextVAlignAsString = resources:GetString("appearance1.TextVAlignAsString":U).
        THIS-OBJECT:eSmartToolbarSecurityItem_ObjectName_Label:Appearance = appearance1.
        THIS-OBJECT:eSmartToolbarSecurityItem_ObjectName_Label:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartToolbarSecurityItem_ObjectName_Label.AutoSize":U)).
        THIS-OBJECT:eSmartToolbarSecurityItem_ObjectName_Label:BackgroundImage = CAST(resources:GetObject("eSmartToolbarSecurityItem_ObjectName_Label.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartToolbarSecurityItem_ObjectName_Label:BackgroundImageLayout = CAST(resources:GetObject("eSmartToolbarSecurityItem_ObjectName_Label.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartToolbarSecurityItem_ObjectName_Label:Dock = CAST(resources:GetObject("eSmartToolbarSecurityItem_ObjectName_Label.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartToolbarSecurityItem_ObjectName_Label:Font = CAST(resources:GetObject("eSmartToolbarSecurityItem_ObjectName_Label.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartToolbarSecurityItem_ObjectName_Label:ImeMode = CAST(resources:GetObject("eSmartToolbarSecurityItem_ObjectName_Label.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartToolbarSecurityItem_ObjectName_Label:Location = CAST(resources:GetObject("eSmartToolbarSecurityItem_ObjectName_Label.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartToolbarSecurityItem_ObjectName_Label:MaximumSize = CAST(resources:GetObject("eSmartToolbarSecurityItem_ObjectName_Label.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartToolbarSecurityItem_ObjectName_Label:Name = "eSmartToolbarSecurityItem_ObjectName_Label":U.
        THIS-OBJECT:eSmartToolbarSecurityItem_ObjectName_Label:RightToLeft = CAST(resources:GetObject("eSmartToolbarSecurityItem_ObjectName_Label.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartToolbarSecurityItem_ObjectName_Label:Size = CAST(resources:GetObject("eSmartToolbarSecurityItem_ObjectName_Label.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartToolbarSecurityItem_ObjectName_Label:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartToolbarSecurityItem_ObjectName_Label.TabIndex":U)).
        THIS-OBJECT:eSmartToolbarSecurityItem_ObjectName_Label:Text = resources:GetString("eSmartToolbarSecurityItem_ObjectName_Label.Text":U).
        /*  */
        /* eSmartToolbarSecurityItem_ObjectName */
        /*  */
        THIS-OBJECT:eSmartToolbarSecurityItem_ObjectName:AccessibleDescription = resources:GetString("eSmartToolbarSecurityItem_ObjectName.AccessibleDescription":U).
        THIS-OBJECT:eSmartToolbarSecurityItem_ObjectName:AccessibleName = resources:GetString("eSmartToolbarSecurityItem_ObjectName.AccessibleName":U).
        THIS-OBJECT:eSmartToolbarSecurityItem_ObjectName:Anchor = CAST(resources:GetObject("eSmartToolbarSecurityItem_ObjectName.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartToolbarSecurityItem_ObjectName:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartToolbarSecurityItem_ObjectName.AutoSize":U)).
        THIS-OBJECT:eSmartToolbarSecurityItem_ObjectName:BackgroundImage = CAST(resources:GetObject("eSmartToolbarSecurityItem_ObjectName.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartToolbarSecurityItem_ObjectName:BackgroundImageLayout = CAST(resources:GetObject("eSmartToolbarSecurityItem_ObjectName.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        editorButton1:AccessibleDescription = resources:GetString("editorButton1.AccessibleDescription":U).
        editorButton1:AccessibleName = resources:GetString("editorButton1.AccessibleName":U).
        appearance2:Image = CAST(resources:GetObject("appearance2.Image":U), System.Object).
        appearance2:TextHAlignAsString = resources:GetString("appearance2.TextHAlignAsString":U).
        appearance2:TextVAlignAsString = resources:GetString("appearance2.TextVAlignAsString":U).
        editorButton1:Appearance = appearance2.
        editorButton1:Text = resources:GetString("editorButton1.Text":U).
        THIS-OBJECT:eSmartToolbarSecurityItem_ObjectName:ButtonsRight:Add(editorButton1).
        THIS-OBJECT:eSmartToolbarSecurityItem_ObjectName:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ObjectName":U, TRUE)).
        THIS-OBJECT:eSmartToolbarSecurityItem_ObjectName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartToolbarSecurityItem_ObjectName:Dock = CAST(resources:GetObject("eSmartToolbarSecurityItem_ObjectName.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartToolbarSecurityItem_ObjectName:Font = CAST(resources:GetObject("eSmartToolbarSecurityItem_ObjectName.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartToolbarSecurityItem_ObjectName:ImeMode = CAST(resources:GetObject("eSmartToolbarSecurityItem_ObjectName.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartToolbarSecurityItem_ObjectName:Location = CAST(resources:GetObject("eSmartToolbarSecurityItem_ObjectName.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartToolbarSecurityItem_ObjectName:MaximumSize = CAST(resources:GetObject("eSmartToolbarSecurityItem_ObjectName.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartToolbarSecurityItem_ObjectName:Name = "eSmartToolbarSecurityItem_ObjectName":U.
        THIS-OBJECT:eSmartToolbarSecurityItem_ObjectName:RightToLeft = CAST(resources:GetObject("eSmartToolbarSecurityItem_ObjectName.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartToolbarSecurityItem_ObjectName:Size = CAST(resources:GetObject("eSmartToolbarSecurityItem_ObjectName.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartToolbarSecurityItem_ObjectName:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartToolbarSecurityItem_ObjectName.TabIndex":U)).
        THIS-OBJECT:eSmartToolbarSecurityItem_ObjectName:EditorButtonClick:Subscribe(THIS-OBJECT:eSmartToolbarSecurityItem_ObjectName_EditorButtonClick).
        THIS-OBJECT:eSmartToolbarSecurityItem_ObjectName:KeyDown:Subscribe(THIS-OBJECT:LookupKeyDownHandler).
        /*  */
        /* eSmartToolbarSecurityItem_ToolbarItemKey_Label */
        /*  */
        THIS-OBJECT:eSmartToolbarSecurityItem_ToolbarItemKey_Label:AccessibleDescription = resources:GetString("eSmartToolbarSecurityItem_ToolbarItemKey_Label.AccessibleDescription":U).
        THIS-OBJECT:eSmartToolbarSecurityItem_ToolbarItemKey_Label:AccessibleName = resources:GetString("eSmartToolbarSecurityItem_ToolbarItemKey_Label.AccessibleName":U).
        THIS-OBJECT:eSmartToolbarSecurityItem_ToolbarItemKey_Label:Anchor = CAST(resources:GetObject("eSmartToolbarSecurityItem_ToolbarItemKey_Label.Anchor":U), System.Windows.Forms.AnchorStyles).
        appearance3:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        appearance3:TextHAlignAsString = resources:GetString("appearance3.TextHAlignAsString":U).
        appearance3:TextVAlignAsString = resources:GetString("appearance3.TextVAlignAsString":U).
        THIS-OBJECT:eSmartToolbarSecurityItem_ToolbarItemKey_Label:Appearance = appearance3.
        THIS-OBJECT:eSmartToolbarSecurityItem_ToolbarItemKey_Label:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartToolbarSecurityItem_ToolbarItemKey_Label.AutoSize":U)).
        THIS-OBJECT:eSmartToolbarSecurityItem_ToolbarItemKey_Label:BackgroundImage = CAST(resources:GetObject("eSmartToolbarSecurityItem_ToolbarItemKey_Label.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartToolbarSecurityItem_ToolbarItemKey_Label:BackgroundImageLayout = CAST(resources:GetObject("eSmartToolbarSecurityItem_ToolbarItemKey_Label.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartToolbarSecurityItem_ToolbarItemKey_Label:Dock = CAST(resources:GetObject("eSmartToolbarSecurityItem_ToolbarItemKey_Label.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartToolbarSecurityItem_ToolbarItemKey_Label:Font = CAST(resources:GetObject("eSmartToolbarSecurityItem_ToolbarItemKey_Label.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartToolbarSecurityItem_ToolbarItemKey_Label:ImeMode = CAST(resources:GetObject("eSmartToolbarSecurityItem_ToolbarItemKey_Label.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartToolbarSecurityItem_ToolbarItemKey_Label:Location = CAST(resources:GetObject("eSmartToolbarSecurityItem_ToolbarItemKey_Label.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartToolbarSecurityItem_ToolbarItemKey_Label:MaximumSize = CAST(resources:GetObject("eSmartToolbarSecurityItem_ToolbarItemKey_Label.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartToolbarSecurityItem_ToolbarItemKey_Label:Name = "eSmartToolbarSecurityItem_ToolbarItemKey_Label":U.
        THIS-OBJECT:eSmartToolbarSecurityItem_ToolbarItemKey_Label:RightToLeft = CAST(resources:GetObject("eSmartToolbarSecurityItem_ToolbarItemKey_Label.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartToolbarSecurityItem_ToolbarItemKey_Label:Size = CAST(resources:GetObject("eSmartToolbarSecurityItem_ToolbarItemKey_Label.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartToolbarSecurityItem_ToolbarItemKey_Label:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartToolbarSecurityItem_ToolbarItemKey_Label.TabIndex":U)).
        THIS-OBJECT:eSmartToolbarSecurityItem_ToolbarItemKey_Label:Text = resources:GetString("eSmartToolbarSecurityItem_ToolbarItemKey_Label.Text":U).
        /*  */
        /* eSmartToolbarSecurityItem_Description_Label */
        /*  */
        THIS-OBJECT:eSmartToolbarSecurityItem_Description_Label:AccessibleDescription = resources:GetString("eSmartToolbarSecurityItem_Description_Label.AccessibleDescription":U).
        THIS-OBJECT:eSmartToolbarSecurityItem_Description_Label:AccessibleName = resources:GetString("eSmartToolbarSecurityItem_Description_Label.AccessibleName":U).
        THIS-OBJECT:eSmartToolbarSecurityItem_Description_Label:Anchor = CAST(resources:GetObject("eSmartToolbarSecurityItem_Description_Label.Anchor":U), System.Windows.Forms.AnchorStyles).
        appearance4:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        appearance4:TextHAlignAsString = resources:GetString("appearance4.TextHAlignAsString":U).
        appearance4:TextVAlignAsString = resources:GetString("appearance4.TextVAlignAsString":U).
        THIS-OBJECT:eSmartToolbarSecurityItem_Description_Label:Appearance = appearance4.
        THIS-OBJECT:eSmartToolbarSecurityItem_Description_Label:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartToolbarSecurityItem_Description_Label.AutoSize":U)).
        THIS-OBJECT:eSmartToolbarSecurityItem_Description_Label:BackgroundImage = CAST(resources:GetObject("eSmartToolbarSecurityItem_Description_Label.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartToolbarSecurityItem_Description_Label:BackgroundImageLayout = CAST(resources:GetObject("eSmartToolbarSecurityItem_Description_Label.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartToolbarSecurityItem_Description_Label:Dock = CAST(resources:GetObject("eSmartToolbarSecurityItem_Description_Label.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartToolbarSecurityItem_Description_Label:Font = CAST(resources:GetObject("eSmartToolbarSecurityItem_Description_Label.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartToolbarSecurityItem_Description_Label:ImeMode = CAST(resources:GetObject("eSmartToolbarSecurityItem_Description_Label.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartToolbarSecurityItem_Description_Label:Location = CAST(resources:GetObject("eSmartToolbarSecurityItem_Description_Label.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartToolbarSecurityItem_Description_Label:MaximumSize = CAST(resources:GetObject("eSmartToolbarSecurityItem_Description_Label.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartToolbarSecurityItem_Description_Label:Name = "eSmartToolbarSecurityItem_Description_Label":U.
        THIS-OBJECT:eSmartToolbarSecurityItem_Description_Label:RightToLeft = CAST(resources:GetObject("eSmartToolbarSecurityItem_Description_Label.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartToolbarSecurityItem_Description_Label:Size = CAST(resources:GetObject("eSmartToolbarSecurityItem_Description_Label.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartToolbarSecurityItem_Description_Label:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartToolbarSecurityItem_Description_Label.TabIndex":U)).
        THIS-OBJECT:eSmartToolbarSecurityItem_Description_Label:Text = resources:GetString("eSmartToolbarSecurityItem_Description_Label.Text":U).
        /*  */
        /* eSmartToolbarSecurityItem_Description */
        /*  */
        THIS-OBJECT:eSmartToolbarSecurityItem_Description:AccessibleDescription = resources:GetString("eSmartToolbarSecurityItem_Description.AccessibleDescription":U).
        THIS-OBJECT:eSmartToolbarSecurityItem_Description:AccessibleName = resources:GetString("eSmartToolbarSecurityItem_Description.AccessibleName":U).
        THIS-OBJECT:eSmartToolbarSecurityItem_Description:Anchor = CAST(resources:GetObject("eSmartToolbarSecurityItem_Description.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartToolbarSecurityItem_Description:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartToolbarSecurityItem_Description.AutoSize":U)).
        THIS-OBJECT:eSmartToolbarSecurityItem_Description:BackgroundImage = CAST(resources:GetObject("eSmartToolbarSecurityItem_Description.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartToolbarSecurityItem_Description:BackgroundImageLayout = CAST(resources:GetObject("eSmartToolbarSecurityItem_Description.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartToolbarSecurityItem_Description:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "Description":U, TRUE)).
        THIS-OBJECT:eSmartToolbarSecurityItem_Description:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartToolbarSecurityItem_Description:Dock = CAST(resources:GetObject("eSmartToolbarSecurityItem_Description.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartToolbarSecurityItem_Description:Font = CAST(resources:GetObject("eSmartToolbarSecurityItem_Description.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartToolbarSecurityItem_Description:ImeMode = CAST(resources:GetObject("eSmartToolbarSecurityItem_Description.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartToolbarSecurityItem_Description:Location = CAST(resources:GetObject("eSmartToolbarSecurityItem_Description.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartToolbarSecurityItem_Description:MaximumSize = CAST(resources:GetObject("eSmartToolbarSecurityItem_Description.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartToolbarSecurityItem_Description:Multiline = System.Convert:ToBoolean(resources:GetObject("eSmartToolbarSecurityItem_Description.Multiline":U)).
        THIS-OBJECT:eSmartToolbarSecurityItem_Description:Name = "eSmartToolbarSecurityItem_Description":U.
        THIS-OBJECT:eSmartToolbarSecurityItem_Description:RightToLeft = CAST(resources:GetObject("eSmartToolbarSecurityItem_Description.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartToolbarSecurityItem_Description:Size = CAST(resources:GetObject("eSmartToolbarSecurityItem_Description.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartToolbarSecurityItem_Description:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartToolbarSecurityItem_Description.TabIndex":U)).
        /*  */
        /* ultraComboEditor1 */
        /*  */
        THIS-OBJECT:ultraComboEditor1:AccessibleDescription = resources:GetString("ultraComboEditor1.AccessibleDescription":U).
        THIS-OBJECT:ultraComboEditor1:AccessibleName = resources:GetString("ultraComboEditor1.AccessibleName":U).
        THIS-OBJECT:ultraComboEditor1:Anchor = CAST(resources:GetObject("ultraComboEditor1.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraComboEditor1:AutoSize = System.Convert:ToBoolean(resources:GetObject("ultraComboEditor1.AutoSize":U)).
        THIS-OBJECT:ultraComboEditor1:BackgroundImage = CAST(resources:GetObject("ultraComboEditor1.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:ultraComboEditor1:BackgroundImageLayout = CAST(resources:GetObject("ultraComboEditor1.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:ultraComboEditor1:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ToolbarItemKey":U, TRUE)).
        THIS-OBJECT:ultraComboEditor1:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:ultraComboEditor1:Dock = CAST(resources:GetObject("ultraComboEditor1.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:ultraComboEditor1:Font = CAST(resources:GetObject("ultraComboEditor1.Font":U), System.Drawing.Font).
        THIS-OBJECT:ultraComboEditor1:ImeMode = CAST(resources:GetObject("ultraComboEditor1.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:ultraComboEditor1:Location = CAST(resources:GetObject("ultraComboEditor1.Location":U), System.Drawing.Point).
        THIS-OBJECT:ultraComboEditor1:MaximumSize = CAST(resources:GetObject("ultraComboEditor1.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:ultraComboEditor1:Name = "ultraComboEditor1":U.
        THIS-OBJECT:ultraComboEditor1:RightToLeft = CAST(resources:GetObject("ultraComboEditor1.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:ultraComboEditor1:Size = CAST(resources:GetObject("ultraComboEditor1.Size":U), System.Drawing.Size).
        THIS-OBJECT:ultraComboEditor1:TabIndex = System.Convert:ToInt32(resources:GetObject("ultraComboEditor1.TabIndex":U)).
        THIS-OBJECT:ultraComboEditor1:ValueChanged:Subscribe(THIS-OBJECT:ultraComboEditor1_ValueChanged).
        /*  */
        /* SmartToolbarSecurityItemViewerControl */
        /*  */
        THIS-OBJECT:AccessibleDescription = resources:GetString("$this.AccessibleDescription":U).
        THIS-OBJECT:AccessibleName = resources:GetString("$this.AccessibleName":U).
        THIS-OBJECT:AutoScaleDimensions = CAST(resources:GetObject("$this.AutoScaleDimensions":U), System.Drawing.SizeF).
        THIS-OBJECT:AutoScroll = System.Convert:ToBoolean(resources:GetObject("$this.AutoScroll":U)).
        THIS-OBJECT:AutoSize = System.Convert:ToBoolean(resources:GetObject("$this.AutoSize":U)).
        THIS-OBJECT:AutoSizeMode = CAST(resources:GetObject("$this.AutoSizeMode":U), System.Windows.Forms.AutoSizeMode).
        THIS-OBJECT:BackgroundImage = CAST(resources:GetObject("$this.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:BackgroundImageLayout = CAST(resources:GetObject("$this.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraComboEditor1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartToolbarSecurityItem_ObjectName_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartToolbarSecurityItem_ObjectName).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartToolbarSecurityItem_ToolbarItemKey_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartToolbarSecurityItem_Description_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartToolbarSecurityItem_Description).
        THIS-OBJECT:Font = CAST(resources:GetObject("$this.Font":U), System.Drawing.Font).
        THIS-OBJECT:ImeMode = CAST(resources:GetObject("$this.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:Location = CAST(resources:GetObject("$this.Location":U), System.Drawing.Point).
        THIS-OBJECT:MaximumSize = CAST(resources:GetObject("$this.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:Name = "SmartToolbarSecurityItemViewerControl":U.
        THIS-OBJECT:RightToLeft = CAST(resources:GetObject("$this.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:SecurityKey = resources:GetString("$this.SecurityKey":U).
        THIS-OBJECT:Size = CAST(resources:GetObject("$this.Size":U), System.Drawing.Size).
        THIS-OBJECT:SmartTableIOState = resources:GetString("$this.SmartTableIOState":U).
        THIS-OBJECT:ViewerLogicClass = resources:GetString("$this.ViewerLogicClass":U).
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartToolbarSecurityItem_ObjectName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartToolbarSecurityItem_Description, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraComboEditor1, System.ComponentModel.ISupportInitialize):EndInit().
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
                WHEN eSmartToolbarSecurityItem_ObjectName 
                    THEN THIS-OBJECT:eSmartToolbarSecurityItem_ObjectName_EditorButtonClick (sender, NEW EditorButtonEventArgs (?, ?)) .
            END CASE . 
&ENDIF 
        END.

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .                
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Performs a lookup for a Tool Item Container 
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID LookupToolItemContainer ():
		
        DEFINE VARIABLE oForm         AS ToolbarContainerLookupDialog      NO-UNDO . 
        DEFINE VARIABLE oDialogResult AS System.Windows.Forms.DialogResult NO-UNDO . 
		DEFINE VARIABLE i             AS INTEGER                           NO-UNDO .
		
		oForm = NEW ToolbarContainerLookupDialog () . 
		
		WAIT-FOR oForm:ShowDialog () SET oDialogResult . 
		
		{Consultingwerk/check-dialogresult-ok.i oDialogResult} .
		
		IF oForm:SelectedObject > "":U THEN DO:
		    
		    ASSIGN THIS-OBJECT:eSmartToolbarSecurityItem_ObjectName:Text = oForm:SelectedObject . 
		         
		    IF THIS-OBJECT:SmartTableIOState = Consultingwerk.SmartComponents.Enum.TableIOStateEnum:FieldsEnabled THEN 
		        THIS-OBJECT:SmartTableIOState = Consultingwerk.SmartComponents.Enum.TableIOStateEnum:ModifyingData .
		        
		    ultraComboEditor1:ValueList:ValueListItems:Clear () .
		    
		    CASE oForm:ToolbarContainerType:
		        WHEN ToolbarContainerTypeEnum:Form THEN DO:
		            
		            ultraComboEditor1:ValueList:ValueListItems:Clear () .
		    
		            IF VALID-OBJECT (oForm:SelectedToolbarController) THEN DO:
		                {Consultingwerk/foreach.i Infragistics.Win.UltraWinToolbars.ToolBase oTool in oForm:SelectedToolbarController:Tools}
                            IF NOT ListHelper:EntryIsInList (oTool:Key, Consultingwerk.SmartComponents.Base.Const:TABLEIO_TOOLS) AND 
                               NOT ListHelper:EntryIsInList (oTool:Key, Consultingwerk.SmartComponents.Base.Const:NAVIGATION_TOOLS) AND 
                               NOT ListHelper:EntryIsInList (oTool:Key, Consultingwerk.SmartComponents.Base.Const:TRANSACTION_TOOLS) THEN 
		                        ultraComboEditor1:ValueList:ValueListItems:Add (oTool:Key, oTool:SharedProps:Caption) .
		                END.
		            END.
		        END.
                WHEN ToolbarContainerTypeEnum:TableIOTarget THEN DO:
                    
                    ultraComboEditor1:ValueList:ValueListItems:Clear () .
                    
                    DO i = 1 TO NUM-ENTRIES (Consultingwerk.SmartComponents.Base.Const:TABLEIO_TOOLS):
                        ultraComboEditor1:ValueList:ValueListItems:Add (ENTRY (i, Consultingwerk.SmartComponents.Base.Const:TABLEIO_TOOLS)) .
                    END.
                END.		         
		    END CASE .
		    
		    ultraComboEditor1:Focus () .
		    ultraComboEditor1:DropDown() .
		END.

        GarbageCollectorHelper:DeleteObject (oForm) .

	END METHOD .

	/*------------------------------------------------------------------------------
	    Purpose: Event handler for the ValueChanged event of the ultraComboEditor1
	    Notes:
	    @param sender The reference to the object that raised the event
	    @param e The System.EventArgs with the data for this event
	------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID ultraComboEditor1_ValueChanged (sender AS System.Object,
	                                                    e AS System.EventArgs):
		
        DEFINE VARIABLE cStackTrace AS CHARACTER NO-UNDO.

        ASSIGN cStackTrace = System.Environment:StackTrace . 
                                      
        IF cStackTrace MATCHES "*Progress.Data.DataSource.OnListChanged*":U  OR 
           cStackTrace MATCHES "*System.Windows.Forms.Binding.PushData*":U 
            THEN RETURN .

		IF ultraComboEditor1:Focused THEN 
		    eSmartToolbarSecurityItem_Description:Text = ultraComboEditor1:Text .  

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SmartToolbarSecurityItemViewerControl class
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartToolbarSecurityItemViewerControl ( ):

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
