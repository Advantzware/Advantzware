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
    File        : SmartSecurityTokenViewerControl
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 17.04.2014 20:52:26
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Base.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.Util.*                           FROM PROPATH .
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.Authorization.SmartSecurityTokenViewerControl 
    INHERITS SmartViewerControl: 
    
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.

    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSecurityToken_SecurityTokenCode_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSecurityToken_SecurityTokenCode AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSecurityToken_SecurityTokenDescription_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSecurityToken_SecurityTokenDescription AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartSecurityTokenViewerControl class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartSecurityTokenViewerControl ():
                
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
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.Authorization.SmartSecurityTokenViewerControl":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc1 = NEW Progress.Data.TableDesc("eSmartSecurityToken":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        THIS-OBJECT:smartBusinessEntityBindingSource1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenCode_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenCode = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenDescription_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenDescription = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartSecurityToken_SecurityTokenCode, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartSecurityToken_SecurityTokenDescription, System.ComponentModel.ISupportInitialize):BeginInit().
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
        DEFINE VARIABLE arrayvar0 AS Progress.Data.ColumnPropDesc EXTENT 3 NO-UNDO.
        arrayvar0[1] = NEW Progress.Data.ColumnPropDesc("SecurityTokenGuid":U, "SecurityTokenGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[2] = NEW Progress.Data.ColumnPropDesc("SecurityTokenCode":U, "Token Code":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[3] = NEW Progress.Data.ColumnPropDesc("SecurityTokenDescription":U, "Token Description":U, Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:TableSchema = tableDesc1.
        /*  */
        /* eSmartSecurityToken_SecurityTokenCode_Label */
        /*  */
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenCode_Label:AccessibleDescription = resources:GetString("eSmartSecurityToken_SecurityTokenCode_Label.AccessibleDescription":U).
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenCode_Label:AccessibleName = resources:GetString("eSmartSecurityToken_SecurityTokenCode_Label.AccessibleName":U).
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenCode_Label:Anchor = CAST(resources:GetObject("eSmartSecurityToken_SecurityTokenCode_Label.Anchor":U), System.Windows.Forms.AnchorStyles).
        appearance1:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        appearance1:TextHAlignAsString = resources:GetString("appearance1.TextHAlignAsString":U).
        appearance1:TextVAlignAsString = resources:GetString("appearance1.TextVAlignAsString":U).
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenCode_Label:Appearance = appearance1.
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenCode_Label:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartSecurityToken_SecurityTokenCode_Label.AutoSize":U)).
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenCode_Label:BackgroundImage = CAST(resources:GetObject("eSmartSecurityToken_SecurityTokenCode_Label.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenCode_Label:BackgroundImageLayout = CAST(resources:GetObject("eSmartSecurityToken_SecurityTokenCode_Label.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenCode_Label:Dock = CAST(resources:GetObject("eSmartSecurityToken_SecurityTokenCode_Label.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenCode_Label:Font = CAST(resources:GetObject("eSmartSecurityToken_SecurityTokenCode_Label.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenCode_Label:ImeMode = CAST(resources:GetObject("eSmartSecurityToken_SecurityTokenCode_Label.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenCode_Label:Location = CAST(resources:GetObject("eSmartSecurityToken_SecurityTokenCode_Label.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenCode_Label:MaximumSize = CAST(resources:GetObject("eSmartSecurityToken_SecurityTokenCode_Label.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenCode_Label:Name = "eSmartSecurityToken_SecurityTokenCode_Label":U.
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenCode_Label:RightToLeft = CAST(resources:GetObject("eSmartSecurityToken_SecurityTokenCode_Label.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenCode_Label:Size = CAST(resources:GetObject("eSmartSecurityToken_SecurityTokenCode_Label.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenCode_Label:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartSecurityToken_SecurityTokenCode_Label.TabIndex":U)).
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenCode_Label:Text = resources:GetString("eSmartSecurityToken_SecurityTokenCode_Label.Text":U).
        /*  */
        /* eSmartSecurityToken_SecurityTokenCode */
        /*  */
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenCode:AccessibleDescription = resources:GetString("eSmartSecurityToken_SecurityTokenCode.AccessibleDescription":U).
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenCode:AccessibleName = resources:GetString("eSmartSecurityToken_SecurityTokenCode.AccessibleName":U).
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenCode:Anchor = CAST(resources:GetObject("eSmartSecurityToken_SecurityTokenCode.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenCode:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartSecurityToken_SecurityTokenCode.AutoSize":U)).
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenCode:BackgroundImage = CAST(resources:GetObject("eSmartSecurityToken_SecurityTokenCode.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenCode:BackgroundImageLayout = CAST(resources:GetObject("eSmartSecurityToken_SecurityTokenCode.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenCode:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "SecurityTokenCode":U, TRUE)).
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenCode:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenCode:Dock = CAST(resources:GetObject("eSmartSecurityToken_SecurityTokenCode.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenCode:Font = CAST(resources:GetObject("eSmartSecurityToken_SecurityTokenCode.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenCode:ImeMode = CAST(resources:GetObject("eSmartSecurityToken_SecurityTokenCode.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenCode:Location = CAST(resources:GetObject("eSmartSecurityToken_SecurityTokenCode.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenCode:MaximumSize = CAST(resources:GetObject("eSmartSecurityToken_SecurityTokenCode.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenCode:Name = "eSmartSecurityToken_SecurityTokenCode":U.
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenCode:RightToLeft = CAST(resources:GetObject("eSmartSecurityToken_SecurityTokenCode.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenCode:Size = CAST(resources:GetObject("eSmartSecurityToken_SecurityTokenCode.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenCode:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartSecurityToken_SecurityTokenCode.TabIndex":U)).
        /*  */
        /* eSmartSecurityToken_SecurityTokenDescription_Label */
        /*  */
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenDescription_Label:AccessibleDescription = resources:GetString("eSmartSecurityToken_SecurityTokenDescription_Label.AccessibleDescription":U).
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenDescription_Label:AccessibleName = resources:GetString("eSmartSecurityToken_SecurityTokenDescription_Label.AccessibleName":U).
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenDescription_Label:Anchor = CAST(resources:GetObject("eSmartSecurityToken_SecurityTokenDescription_Label.Anchor":U), System.Windows.Forms.AnchorStyles).
        appearance2:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        appearance2:TextHAlignAsString = resources:GetString("appearance2.TextHAlignAsString":U).
        appearance2:TextVAlignAsString = resources:GetString("appearance2.TextVAlignAsString":U).
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenDescription_Label:Appearance = appearance2.
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenDescription_Label:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartSecurityToken_SecurityTokenDescription_Label.AutoSize":U)).
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenDescription_Label:BackgroundImage = CAST(resources:GetObject("eSmartSecurityToken_SecurityTokenDescription_Label.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenDescription_Label:BackgroundImageLayout = CAST(resources:GetObject("eSmartSecurityToken_SecurityTokenDescription_Label.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenDescription_Label:Dock = CAST(resources:GetObject("eSmartSecurityToken_SecurityTokenDescription_Label.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenDescription_Label:Font = CAST(resources:GetObject("eSmartSecurityToken_SecurityTokenDescription_Label.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenDescription_Label:ImeMode = CAST(resources:GetObject("eSmartSecurityToken_SecurityTokenDescription_Label.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenDescription_Label:Location = CAST(resources:GetObject("eSmartSecurityToken_SecurityTokenDescription_Label.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenDescription_Label:MaximumSize = CAST(resources:GetObject("eSmartSecurityToken_SecurityTokenDescription_Label.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenDescription_Label:Name = "eSmartSecurityToken_SecurityTokenDescription_Label":U.
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenDescription_Label:RightToLeft = CAST(resources:GetObject("eSmartSecurityToken_SecurityTokenDescription_Label.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenDescription_Label:Size = CAST(resources:GetObject("eSmartSecurityToken_SecurityTokenDescription_Label.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenDescription_Label:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartSecurityToken_SecurityTokenDescription_Label.TabIndex":U)).
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenDescription_Label:Text = resources:GetString("eSmartSecurityToken_SecurityTokenDescription_Label.Text":U).
        /*  */
        /* eSmartSecurityToken_SecurityTokenDescription */
        /*  */
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenDescription:AccessibleDescription = resources:GetString("eSmartSecurityToken_SecurityTokenDescription.AccessibleDescription":U).
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenDescription:AccessibleName = resources:GetString("eSmartSecurityToken_SecurityTokenDescription.AccessibleName":U).
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenDescription:Anchor = CAST(resources:GetObject("eSmartSecurityToken_SecurityTokenDescription.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenDescription:AutoSize = System.Convert:ToBoolean(resources:GetObject("eSmartSecurityToken_SecurityTokenDescription.AutoSize":U)).
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenDescription:BackgroundImage = CAST(resources:GetObject("eSmartSecurityToken_SecurityTokenDescription.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenDescription:BackgroundImageLayout = CAST(resources:GetObject("eSmartSecurityToken_SecurityTokenDescription.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenDescription:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "SecurityTokenDescription":U, TRUE)).
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenDescription:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenDescription:Dock = CAST(resources:GetObject("eSmartSecurityToken_SecurityTokenDescription.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenDescription:Font = CAST(resources:GetObject("eSmartSecurityToken_SecurityTokenDescription.Font":U), System.Drawing.Font).
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenDescription:ImeMode = CAST(resources:GetObject("eSmartSecurityToken_SecurityTokenDescription.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenDescription:Location = CAST(resources:GetObject("eSmartSecurityToken_SecurityTokenDescription.Location":U), System.Drawing.Point).
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenDescription:MaximumSize = CAST(resources:GetObject("eSmartSecurityToken_SecurityTokenDescription.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenDescription:Multiline = System.Convert:ToBoolean(resources:GetObject("eSmartSecurityToken_SecurityTokenDescription.Multiline":U)).
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenDescription:Name = "eSmartSecurityToken_SecurityTokenDescription":U.
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenDescription:RightToLeft = CAST(resources:GetObject("eSmartSecurityToken_SecurityTokenDescription.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenDescription:Size = CAST(resources:GetObject("eSmartSecurityToken_SecurityTokenDescription.Size":U), System.Drawing.Size).
        THIS-OBJECT:eSmartSecurityToken_SecurityTokenDescription:TabIndex = System.Convert:ToInt32(resources:GetObject("eSmartSecurityToken_SecurityTokenDescription.TabIndex":U)).
        /*  */
        /* SmartSecurityTokenViewerControl */
        /*  */
        THIS-OBJECT:AccessibleDescription = resources:GetString("$this.AccessibleDescription":U).
        THIS-OBJECT:AccessibleName = resources:GetString("$this.AccessibleName":U).
        THIS-OBJECT:AutoScaleDimensions = CAST(resources:GetObject("$this.AutoScaleDimensions":U), System.Drawing.SizeF).
        THIS-OBJECT:AutoScroll = System.Convert:ToBoolean(resources:GetObject("$this.AutoScroll":U)).
        THIS-OBJECT:AutoSize = System.Convert:ToBoolean(resources:GetObject("$this.AutoSize":U)).
        THIS-OBJECT:AutoSizeMode = CAST(resources:GetObject("$this.AutoSizeMode":U), System.Windows.Forms.AutoSizeMode).
        THIS-OBJECT:BackgroundImage = CAST(resources:GetObject("$this.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:BackgroundImageLayout = CAST(resources:GetObject("$this.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartSecurityToken_SecurityTokenCode_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartSecurityToken_SecurityTokenCode).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartSecurityToken_SecurityTokenDescription_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartSecurityToken_SecurityTokenDescription).
        THIS-OBJECT:Font = CAST(resources:GetObject("$this.Font":U), System.Drawing.Font).
        THIS-OBJECT:ImeMode = CAST(resources:GetObject("$this.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:Location = CAST(resources:GetObject("$this.Location":U), System.Drawing.Point).
        THIS-OBJECT:MaximumSize = CAST(resources:GetObject("$this.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:Name = "SmartSecurityTokenViewerControl":U.
        THIS-OBJECT:RightToLeft = CAST(resources:GetObject("$this.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:SecurityKey = resources:GetString("$this.SecurityKey":U).
        THIS-OBJECT:Size = CAST(resources:GetObject("$this.Size":U), System.Drawing.Size).
        THIS-OBJECT:SmartTableIOState = resources:GetString("$this.SmartTableIOState":U).
        THIS-OBJECT:ViewerLogicClass = resources:GetString("$this.ViewerLogicClass":U).
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartSecurityToken_SecurityTokenCode, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartSecurityToken_SecurityTokenDescription, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SmartSecurityTokenViewerControl class
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartSecurityTokenViewerControl ( ):

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
