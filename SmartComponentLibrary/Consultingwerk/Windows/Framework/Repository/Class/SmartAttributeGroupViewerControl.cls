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
    File        : SmartAttributeGroupViewerControl
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : 20.05.2014 17:42:02
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Base.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.Util.*                           FROM PROPATH .
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.Repository.Class.SmartAttributeGroupViewerControl 
    INHERITS SmartViewerControl: 
    
    DEFINE PRIVATE VARIABLE components                                           AS System.ComponentModel.IContainer                                               NO-UNDO.

    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1                    AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartAttributeGroup_AttributeGroupName_Label        AS Infragistics.Win.Misc.UltraLabel                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartAttributeGroup_AttributeGroupName              AS Infragistics.Win.UltraWinEditors.UltraTextEditor                               NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartAttributeGroup_AttributeGroupDescription_Label AS Infragistics.Win.Misc.UltraLabel                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartAttributeGroup_AttributeGroupDescription       AS Infragistics.Win.UltraWinEditors.UltraTextEditor                               NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartAttributeGroupViewerControl class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartAttributeGroupViewerControl ():
                
        SUPER().
        InitializeComponent().
        
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
        tableDesc1 = NEW Progress.Data.TableDesc("eSmartAttributeGroup":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.Repository.Class.SmartAttributeGroupViewerControl":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        THIS-OBJECT:smartBusinessEntityBindingSource1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupDescription_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupDescription = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartAttributeGroup_AttributeGroupName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartAttributeGroup_AttributeGroupDescription, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* smartBusinessEntityBindingSource1 */
        /*  */
        THIS-OBJECT:smartBusinessEntityBindingSource1:BindTo = "QUERY":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityName = "Consultingwerk.SmartFramework.Repository.Class.AttributeGroupBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityTable = "eSmartAttributeGroup":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:NoLOBs = FALSE.
        tableDesc1:ChildTables = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Progress.Data.ColumnPropDesc EXTENT 3 NO-UNDO.
        arrayvar0[1] = NEW Progress.Data.ColumnPropDesc("AttributeGroupGuid":U, "AttributeGroupGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[2] = NEW Progress.Data.ColumnPropDesc("AttributeGroupName":U, "Attribute Group":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[3] = NEW Progress.Data.ColumnPropDesc("AttributeGroupDescription":U, "Description":U, Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:TableSchema = tableDesc1.
        /*  */
        /* eSmartAttributeGroup_AttributeGroupName_Label */
        /*  */
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupName_Label:AccessibleDescription = resources:GetString("eSmartAttributeGroup_AttributeGroupName_Label.AccessibleDescription":U).
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupName_Label:AccessibleName = resources:GetString("eSmartAttributeGroup_AttributeGroupName_Label.AccessibleName":U).
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupName_Label:Anchor = CAST(resources:GetObject("eSmartAttributeGroup_AttributeGroupName_Label.Anchor":U), System.Windows.Forms.AnchorStyles).
        appearance1:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        appearance1:TextHAlignAsString = resources:GetString("appearance1.TextHAlignAsString":U).
        appearance1:TextVAlignAsString = resources:GetString("appearance1.TextVAlignAsString":U).
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupName_Label:Appearance = appearance1.
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupName_Label:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartAttributeGroup_AttributeGroupName_Label.AutoSize":U)).
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupName_Label:BackgroundImage = CAST(resources:GetObject("eSmartAttributeGroup_AttributeGroupName_Label.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupName_Label:BackgroundImageLayout = CAST(resources:GetObject("eSmartAttributeGroup_AttributeGroupName_Label.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupName_Label:Dock = CAST(resources:GetObject("eSmartAttributeGroup_AttributeGroupName_Label.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupName_Label:Font = CAST(resources:GetObject("eSmartAttributeGroup_AttributeGroupName_Label.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupName_Label:ImeMode = CAST(resources:GetObject("eSmartAttributeGroup_AttributeGroupName_Label.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupName_Label:Location = CAST(resources:GetObject("eSmartAttributeGroup_AttributeGroupName_Label.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupName_Label:MaximumSize = CAST(resources:GetObject("eSmartAttributeGroup_AttributeGroupName_Label.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupName_Label:Name = "eSmartAttributeGroup_AttributeGroupName_Label":U.
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupName_Label:RightToLeft = CAST(resources:GetObject("eSmartAttributeGroup_AttributeGroupName_Label.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupName_Label:Size = CAST(resources:GetObject("eSmartAttributeGroup_AttributeGroupName_Label.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupName_Label:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartAttributeGroup_AttributeGroupName_Label.TabIndex":U)).
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupName_Label:Text = resources:GetString("eSmartAttributeGroup_AttributeGroupName_Label.Text":U).
        /*  */
        /* eSmartAttributeGroup_AttributeGroupName */
        /*  */
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupName:AccessibleDescription = resources:GetString("eSmartAttributeGroup_AttributeGroupName.AccessibleDescription":U).
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupName:AccessibleName = resources:GetString("eSmartAttributeGroup_AttributeGroupName.AccessibleName":U).
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupName:Anchor = CAST(resources:GetObject("eSmartAttributeGroup_AttributeGroupName.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupName:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartAttributeGroup_AttributeGroupName.AutoSize":U)).
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupName:BackgroundImage = CAST(resources:GetObject("eSmartAttributeGroup_AttributeGroupName.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupName:BackgroundImageLayout = CAST(resources:GetObject("eSmartAttributeGroup_AttributeGroupName.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupName:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "AttributeGroupName":U, TRUE)).
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupName:Dock = CAST(resources:GetObject("eSmartAttributeGroup_AttributeGroupName.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupName:Font = CAST(resources:GetObject("eSmartAttributeGroup_AttributeGroupName.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupName:ImeMode = CAST(resources:GetObject("eSmartAttributeGroup_AttributeGroupName.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupName:Location = CAST(resources:GetObject("eSmartAttributeGroup_AttributeGroupName.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupName:MaximumSize = CAST(resources:GetObject("eSmartAttributeGroup_AttributeGroupName.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupName:Name = "eSmartAttributeGroup_AttributeGroupName":U.
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupName:RightToLeft = CAST(resources:GetObject("eSmartAttributeGroup_AttributeGroupName.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupName:Size = CAST(resources:GetObject("eSmartAttributeGroup_AttributeGroupName.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupName:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartAttributeGroup_AttributeGroupName.TabIndex":U)).
        /*  */
        /* eSmartAttributeGroup_AttributeGroupDescription_Label */
        /*  */
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupDescription_Label:AccessibleDescription = resources:GetString("eSmartAttributeGroup_AttributeGroupDescription_Label.AccessibleDescription":U).
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupDescription_Label:AccessibleName = resources:GetString("eSmartAttributeGroup_AttributeGroupDescription_Label.AccessibleName":U).
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupDescription_Label:Anchor = CAST(resources:GetObject("eSmartAttributeGroup_AttributeGroupDescription_Label.Anchor":U), System.Windows.Forms.AnchorStyles).
        appearance2:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        appearance2:TextHAlignAsString = resources:GetString("appearance2.TextHAlignAsString":U).
        appearance2:TextVAlignAsString = resources:GetString("appearance2.TextVAlignAsString":U).
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupDescription_Label:Appearance = appearance2.
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupDescription_Label:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartAttributeGroup_AttributeGroupDescription_Label.AutoSize":U)).
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupDescription_Label:BackgroundImage = CAST(resources:GetObject("eSmartAttributeGroup_AttributeGroupDescription_Label.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupDescription_Label:BackgroundImageLayout = CAST(resources:GetObject("eSmartAttributeGroup_AttributeGroupDescription_Label.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupDescription_Label:Dock = CAST(resources:GetObject("eSmartAttributeGroup_AttributeGroupDescription_Label.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupDescription_Label:Font = CAST(resources:GetObject("eSmartAttributeGroup_AttributeGroupDescription_Label.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupDescription_Label:ImeMode = CAST(resources:GetObject("eSmartAttributeGroup_AttributeGroupDescription_Label.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupDescription_Label:Location = CAST(resources:GetObject("eSmartAttributeGroup_AttributeGroupDescription_Label.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupDescription_Label:MaximumSize = CAST(resources:GetObject("eSmartAttributeGroup_AttributeGroupDescription_Label.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupDescription_Label:Name = "eSmartAttributeGroup_AttributeGroupDescription_Label":U.
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupDescription_Label:RightToLeft = CAST(resources:GetObject("eSmartAttributeGroup_AttributeGroupDescription_Label.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupDescription_Label:Size = CAST(resources:GetObject("eSmartAttributeGroup_AttributeGroupDescription_Label.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupDescription_Label:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartAttributeGroup_AttributeGroupDescription_Label.TabIndex":U)).
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupDescription_Label:Text = resources:GetString("eSmartAttributeGroup_AttributeGroupDescription_Label.Text":U).
        /*  */
        /* eSmartAttributeGroup_AttributeGroupDescription */
        /*  */
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupDescription:AccessibleDescription = resources:GetString("eSmartAttributeGroup_AttributeGroupDescription.AccessibleDescription":U).
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupDescription:AccessibleName = resources:GetString("eSmartAttributeGroup_AttributeGroupDescription.AccessibleName":U).
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupDescription:Anchor = CAST(resources:GetObject("eSmartAttributeGroup_AttributeGroupDescription.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupDescription:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartAttributeGroup_AttributeGroupDescription.AutoSize":U)).
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupDescription:BackgroundImage = CAST(resources:GetObject("eSmartAttributeGroup_AttributeGroupDescription.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupDescription:BackgroundImageLayout = CAST(resources:GetObject("eSmartAttributeGroup_AttributeGroupDescription.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupDescription:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "AttributeGroupDescription":U, TRUE)).
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupDescription:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupDescription:Dock = CAST(resources:GetObject("eSmartAttributeGroup_AttributeGroupDescription.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupDescription:Font = CAST(resources:GetObject("eSmartAttributeGroup_AttributeGroupDescription.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupDescription:ImeMode = CAST(resources:GetObject("eSmartAttributeGroup_AttributeGroupDescription.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupDescription:Location = CAST(resources:GetObject("eSmartAttributeGroup_AttributeGroupDescription.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupDescription:MaximumSize = CAST(resources:GetObject("eSmartAttributeGroup_AttributeGroupDescription.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupDescription:Multiline = System.Convert:ToBoolean(resources:GetObject("eSmartAttributeGroup_AttributeGroupDescription.Multiline":U)).
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupDescription:Name = "eSmartAttributeGroup_AttributeGroupDescription":U.
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupDescription:RightToLeft = CAST(resources:GetObject("eSmartAttributeGroup_AttributeGroupDescription.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupDescription:Size = CAST(resources:GetObject("eSmartAttributeGroup_AttributeGroupDescription.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartAttributeGroup_AttributeGroupDescription:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartAttributeGroup_AttributeGroupDescription.TabIndex":U)).
        /*  */
        /* SmartAttributeGroupViewerControl */
        /*  */
        THIS-OBJECT:AccessibleDescription = resources:GetString("$this.AccessibleDescription":U).
        THIS-OBJECT:AccessibleName = resources:GetString("$this.AccessibleName":U).
        THIS-OBJECT:AutoScaleDimensions = CAST(resources:GetObject("$this.AutoScaleDimensions":U), System.Drawing.SizeF).
        THIS-OBJECT:AutoScroll = System.Convert:ToBoolean(resources:GetObject("$this.AutoScroll":U)).
        THIS-OBJECT:AutoSize = System.Convert:ToBoolean(resources:GetObject("$this.AutoSize":U)).
        THIS-OBJECT:AutoSizeMode = CAST(resources:GetObject("$this.AutoSizeMode":U), System.Windows.Forms.AutoSizeMode).
        THIS-OBJECT:BackgroundImage = CAST(resources:GetObject("$this.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:BackgroundImageLayout = CAST(resources:GetObject("$this.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartAttributeGroup_AttributeGroupName_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartAttributeGroup_AttributeGroupName).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartAttributeGroup_AttributeGroupDescription_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartAttributeGroup_AttributeGroupDescription).
        THIS-OBJECT:Font = CAST(resources:GetObject("$this.Font":U), System.Drawing.Font).
        THIS-OBJECT:ImeMode = CAST(resources:GetObject("$this.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:Location = CAST(resources:GetObject("$this.Location":U), System.Drawing.Point).
        THIS-OBJECT:MaximumSize = CAST(resources:GetObject("$this.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:Name = "SmartAttributeGroupViewerControl":U.
        THIS-OBJECT:RightToLeft = CAST(resources:GetObject("$this.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:SecurityKey = resources:GetString("$this.SecurityKey":U).
        THIS-OBJECT:Size = CAST(resources:GetObject("$this.Size":U), System.Drawing.Size).
        THIS-OBJECT:SmartTableIOState = resources:GetString("$this.SmartTableIOState":U).
        THIS-OBJECT:ViewerLogicClass = resources:GetString("$this.ViewerLogicClass":U).
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartAttributeGroup_AttributeGroupName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartAttributeGroup_AttributeGroupDescription, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SmartAttributeGroupViewerControl class
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartAttributeGroupViewerControl ( ):

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
