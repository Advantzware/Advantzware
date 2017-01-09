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
    File        : BaseDialogForm
    Purpose     : Simple Dialog Form template following the Consultingwerk
                  Dialog Standards with dialog contents in a white panel
                  and the buttons in grey. When validation for OK demanded,
                  the ButtonOk should be disabled when the validation is not
                  passed
    Syntax      :
    Description : Template for forms to be started using the ShowDialog()
                  method
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Dec 08 13:37:29 CET 2014
    Notes       : http://confluence.consultingwerkcloud.com/wiki/display/SCL/Using+Visual+Base+From+classes
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Forms.* FROM PROPATH .
USING Progress.Lang.*        FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.Windows.BaseForms.BaseDialogForm
    INHERITS BaseForm:

    DEFINE PROTECTED VARIABLE ButtonOk AS System.Windows.Forms.Button NO-UNDO.
    DEFINE PROTECTED VARIABLE ButtonCancel AS System.Windows.Forms.Button NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PROTECTED VARIABLE ContentPanel AS System.Windows.Forms.Panel NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the BaseDialogForm class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC BaseDialogForm ():

        SUPER().
        InitializeComponent().

        &IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN
        THIS-OBJECT:ComponentsCollection:Add (THIS-OBJECT:components).
        &ENDIF

        /* Mike Fechner, Consultingwerk Ltd. 08.12.2014
           Dialogs should not store their position by default */
        THIS-OBJECT:StoreFormPosition = FALSE .

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
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.BaseForms.BaseDialogForm":U).
        THIS-OBJECT:ContentPanel = NEW System.Windows.Forms.Panel().
        THIS-OBJECT:ButtonOk = NEW System.Windows.Forms.Button().
        THIS-OBJECT:ButtonCancel = NEW System.Windows.Forms.Button().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ContentPanel */
        /*  */
        THIS-OBJECT:ContentPanel:AccessibleDescription = resources:GetString("ContentPanel.AccessibleDescription":U).
        THIS-OBJECT:ContentPanel:AccessibleName = resources:GetString("ContentPanel.AccessibleName":U).
        THIS-OBJECT:ContentPanel:Anchor = CAST(resources:GetObject("ContentPanel.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ContentPanel:AutoScroll = System.Convert:ToBoolean(resources:GetObject("ContentPanel.AutoScroll":U)).
        THIS-OBJECT:ContentPanel:AutoSize = System.Convert:ToBoolean(resources:GetObject("ContentPanel.AutoSize":U)).
        THIS-OBJECT:ContentPanel:AutoSizeMode = CAST(resources:GetObject("ContentPanel.AutoSizeMode":U), System.Windows.Forms.AutoSizeMode).
        THIS-OBJECT:ContentPanel:BackColor = System.Drawing.Color:White.
        THIS-OBJECT:ContentPanel:BackgroundImage = CAST(resources:GetObject("ContentPanel.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:ContentPanel:BackgroundImageLayout = CAST(resources:GetObject("ContentPanel.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:ContentPanel:Dock = CAST(resources:GetObject("ContentPanel.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:ContentPanel:Font = CAST(resources:GetObject("ContentPanel.Font":U), System.Drawing.Font).
        THIS-OBJECT:ContentPanel:ImeMode = CAST(resources:GetObject("ContentPanel.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:ContentPanel:Location = CAST(resources:GetObject("ContentPanel.Location":U), System.Drawing.Point).
        THIS-OBJECT:ContentPanel:MaximumSize = CAST(resources:GetObject("ContentPanel.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:ContentPanel:Name = "ContentPanel":U.
        THIS-OBJECT:ContentPanel:RightToLeft = CAST(resources:GetObject("ContentPanel.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:ContentPanel:Size = CAST(resources:GetObject("ContentPanel.Size":U), System.Drawing.Size).
        THIS-OBJECT:ContentPanel:TabIndex = System.Convert:ToInt32(resources:GetObject("ContentPanel.TabIndex":U)).
        /*  */
        /* ButtonOk */
        /*  */
        THIS-OBJECT:ButtonOk:AccessibleDescription = resources:GetString("ButtonOk.AccessibleDescription":U).
        THIS-OBJECT:ButtonOk:AccessibleName = resources:GetString("ButtonOk.AccessibleName":U).
        THIS-OBJECT:ButtonOk:Anchor = CAST(resources:GetObject("ButtonOk.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ButtonOk:AutoSize = System.Convert:ToBoolean(resources:GetObject("ButtonOk.AutoSize":U)).
        THIS-OBJECT:ButtonOk:AutoSizeMode = CAST(resources:GetObject("ButtonOk.AutoSizeMode":U), System.Windows.Forms.AutoSizeMode).
        THIS-OBJECT:ButtonOk:BackgroundImage = CAST(resources:GetObject("ButtonOk.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:ButtonOk:BackgroundImageLayout = CAST(resources:GetObject("ButtonOk.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:ButtonOk:DialogResult = System.Windows.Forms.DialogResult:OK.
        THIS-OBJECT:ButtonOk:Dock = CAST(resources:GetObject("ButtonOk.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:ButtonOk:FlatStyle = CAST(resources:GetObject("ButtonOk.FlatStyle":U), System.Windows.Forms.FlatStyle).
        THIS-OBJECT:ButtonOk:Font = CAST(resources:GetObject("ButtonOk.Font":U), System.Drawing.Font).
        THIS-OBJECT:ButtonOk:ImageAlign = CAST(resources:GetObject("ButtonOk.ImageAlign":U), System.Drawing.ContentAlignment).
        THIS-OBJECT:ButtonOk:ImageIndex = System.Convert:ToInt32(resources:GetObject("ButtonOk.ImageIndex":U)).
        THIS-OBJECT:ButtonOk:ImageKey = resources:GetString("ButtonOk.ImageKey":U).
        THIS-OBJECT:ButtonOk:ImeMode = CAST(resources:GetObject("ButtonOk.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:ButtonOk:Location = CAST(resources:GetObject("ButtonOk.Location":U), System.Drawing.Point).
        THIS-OBJECT:ButtonOk:MaximumSize = CAST(resources:GetObject("ButtonOk.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:ButtonOk:Name = "ButtonOk":U.
        THIS-OBJECT:ButtonOk:RightToLeft = CAST(resources:GetObject("ButtonOk.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:ButtonOk:Size = CAST(resources:GetObject("ButtonOk.Size":U), System.Drawing.Size).
        THIS-OBJECT:ButtonOk:TabIndex = System.Convert:ToInt32(resources:GetObject("ButtonOk.TabIndex":U)).
        THIS-OBJECT:ButtonOk:Text = resources:GetString("ButtonOk.Text":U).
        THIS-OBJECT:ButtonOk:TextAlign = CAST(resources:GetObject("ButtonOk.TextAlign":U), System.Drawing.ContentAlignment).
        THIS-OBJECT:ButtonOk:TextImageRelation = CAST(resources:GetObject("ButtonOk.TextImageRelation":U), System.Windows.Forms.TextImageRelation).
        THIS-OBJECT:ButtonOk:UseCompatibleTextRendering = TRUE.
        THIS-OBJECT:ButtonOk:UseVisualStyleBackColor = TRUE.
        /*  */
        /* ButtonCancel */
        /*  */
        THIS-OBJECT:ButtonCancel:AccessibleDescription = resources:GetString("ButtonCancel.AccessibleDescription":U).
        THIS-OBJECT:ButtonCancel:AccessibleName = resources:GetString("ButtonCancel.AccessibleName":U).
        THIS-OBJECT:ButtonCancel:Anchor = CAST(resources:GetObject("ButtonCancel.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ButtonCancel:AutoSize = System.Convert:ToBoolean(resources:GetObject("ButtonCancel.AutoSize":U)).
        THIS-OBJECT:ButtonCancel:AutoSizeMode = CAST(resources:GetObject("ButtonCancel.AutoSizeMode":U), System.Windows.Forms.AutoSizeMode).
        THIS-OBJECT:ButtonCancel:BackgroundImage = CAST(resources:GetObject("ButtonCancel.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:ButtonCancel:BackgroundImageLayout = CAST(resources:GetObject("ButtonCancel.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:ButtonCancel:DialogResult = System.Windows.Forms.DialogResult:Cancel.
        THIS-OBJECT:ButtonCancel:Dock = CAST(resources:GetObject("ButtonCancel.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:ButtonCancel:FlatStyle = CAST(resources:GetObject("ButtonCancel.FlatStyle":U), System.Windows.Forms.FlatStyle).
        THIS-OBJECT:ButtonCancel:Font = CAST(resources:GetObject("ButtonCancel.Font":U), System.Drawing.Font).
        THIS-OBJECT:ButtonCancel:ImageAlign = CAST(resources:GetObject("ButtonCancel.ImageAlign":U), System.Drawing.ContentAlignment).
        THIS-OBJECT:ButtonCancel:ImageIndex = System.Convert:ToInt32(resources:GetObject("ButtonCancel.ImageIndex":U)).
        THIS-OBJECT:ButtonCancel:ImageKey = resources:GetString("ButtonCancel.ImageKey":U).
        THIS-OBJECT:ButtonCancel:ImeMode = CAST(resources:GetObject("ButtonCancel.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:ButtonCancel:Location = CAST(resources:GetObject("ButtonCancel.Location":U), System.Drawing.Point).
        THIS-OBJECT:ButtonCancel:MaximumSize = CAST(resources:GetObject("ButtonCancel.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:ButtonCancel:Name = "ButtonCancel":U.
        THIS-OBJECT:ButtonCancel:RightToLeft = CAST(resources:GetObject("ButtonCancel.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:ButtonCancel:Size = CAST(resources:GetObject("ButtonCancel.Size":U), System.Drawing.Size).
        THIS-OBJECT:ButtonCancel:TabIndex = System.Convert:ToInt32(resources:GetObject("ButtonCancel.TabIndex":U)).
        THIS-OBJECT:ButtonCancel:Text = resources:GetString("ButtonCancel.Text":U).
        THIS-OBJECT:ButtonCancel:TextAlign = CAST(resources:GetObject("ButtonCancel.TextAlign":U), System.Drawing.ContentAlignment).
        THIS-OBJECT:ButtonCancel:TextImageRelation = CAST(resources:GetObject("ButtonCancel.TextImageRelation":U), System.Windows.Forms.TextImageRelation).
        THIS-OBJECT:ButtonCancel:UseCompatibleTextRendering = TRUE.
        THIS-OBJECT:ButtonCancel:UseVisualStyleBackColor = TRUE.
        /*  */
        /* BaseDialogForm */
        /*  */
        THIS-OBJECT:AcceptButton = THIS-OBJECT:ButtonOk.
        THIS-OBJECT:AccessibleDescription = resources:GetString("$this.AccessibleDescription":U).
        THIS-OBJECT:AccessibleName = resources:GetString("$this.AccessibleName":U).
        THIS-OBJECT:AutoScroll = System.Convert:ToBoolean(resources:GetObject("$this.AutoScroll":U)).
        THIS-OBJECT:AutoSize = System.Convert:ToBoolean(resources:GetObject("$this.AutoSize":U)).
        THIS-OBJECT:AutoSizeMode = CAST(resources:GetObject("$this.AutoSizeMode":U), System.Windows.Forms.AutoSizeMode).
        THIS-OBJECT:BackgroundImage = CAST(resources:GetObject("$this.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:BackgroundImageLayout = CAST(resources:GetObject("$this.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:CancelButton = THIS-OBJECT:ButtonCancel.
        THIS-OBJECT:ClientSize = CAST(resources:GetObject("$this.ClientSize":U), System.Drawing.Size).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ButtonCancel).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ButtonOk).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ContentPanel).
        THIS-OBJECT:Font = CAST(resources:GetObject("$this.Font":U), System.Drawing.Font).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:ImeMode = CAST(resources:GetObject("$this.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:Location = CAST(resources:GetObject("$this.Location":U), System.Drawing.Point).
        THIS-OBJECT:MaximumSize = CAST(resources:GetObject("$this.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:MinimizeBox = FALSE.
        THIS-OBJECT:Name = "BaseDialogForm":U.
        THIS-OBJECT:RightToLeft = CAST(resources:GetObject("$this.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:RightToLeftLayout = System.Convert:ToBoolean(resources:GetObject("$this.RightToLeftLayout":U)).
        THIS-OBJECT:StartPosition = CAST(resources:GetObject("$this.StartPosition":U), System.Windows.Forms.FormStartPosition).
        THIS-OBJECT:Text = resources:GetString("$this.Text":U).
        THIS-OBJECT:WindowPositionRegistryKey = resources:GetString("$this.WindowPositionRegistryKey":U).
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the BaseDialogForm class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC BaseDialogForm ():

    END DESTRUCTOR.

END CLASS.
