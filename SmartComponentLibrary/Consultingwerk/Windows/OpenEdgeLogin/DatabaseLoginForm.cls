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
    File        : DatabaseLoginForm
    Purpose     : .NET Version of the OpenEdge Database Login Form
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Mar 07 10:43:13 CET 2013
    Notes       : Used by _login.p, _prostar.p in the same package
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Util.*                  FROM PROPATH . 
USING Consultingwerk.Windows.OpenEdgeLogin.* FROM PROPATH .  
USING Progress.Lang.*                        FROM PROPATH .
USING Progress.Windows.Form                  FROM ASSEMBLY . 

CLASS Consultingwerk.Windows.OpenEdgeLogin.DatabaseLoginForm 
    INHERITS Form: 

    DEFINE PROTECTED VARIABLE buOk AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PROTECTED VARIABLE buCancel AS Infragistics.Win.Misc.UltraButton NO-UNDO.
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE lblDatabase AS System.Windows.Forms.Label NO-UNDO.
    DEFINE PRIVATE VARIABLE label5 AS System.Windows.Forms.Label NO-UNDO.
    DEFINE PRIVATE VARIABLE label4 AS System.Windows.Forms.Label NO-UNDO.
    DEFINE PRIVATE VARIABLE label3 AS System.Windows.Forms.Label NO-UNDO.
    DEFINE PRIVATE VARIABLE label2 AS System.Windows.Forms.Label NO-UNDO.
    DEFINE PRIVATE VARIABLE label1 AS System.Windows.Forms.Label NO-UNDO.
    DEFINE PRIVATE VARIABLE tgOptOut AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE txtUserId AS System.Windows.Forms.TextBox NO-UNDO.
    DEFINE PRIVATE VARIABLE txtPassword AS System.Windows.Forms.TextBox NO-UNDO.
    DEFINE PRIVATE VARIABLE txtDomain AS System.Windows.Forms.TextBox NO-UNDO.
    DEFINE PROTECTED VARIABLE ultraGroupBox1 AS Infragistics.Win.Misc.UltraGroupBox NO-UNDO.
	
    DEFINE VARIABLE oClearButtonProvider   AS Consultingwerk.Windows.Controls.ClearButtonProvider NO-UNDO EXTENT 3 .        
	
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current database name 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY DatabaseName AS CHARACTER NO-UNDO 
	GET:
		RETURN lblDatabase:Text . 
	END GET.
	SET (arg AS CHARACTER):
		ASSIGN lblDatabase:Text = arg . 		
	END SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current domain name 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DomainName AS CHARACTER NO-UNDO 
    GET:
        RETURN txtDomain:Text . 
    END GET.
    SET (arg AS CHARACTER):
        ASSIGN txtDomain:Text = arg .         
    END SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current domain name 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY OptOut AS LOGICAL NO-UNDO 
	GET:
		RETURN tgOptOut:Checked . 
	END GET.
	SET (arg AS LOGICAL):
		ASSIGN tgOptOut:Checked = arg .  		
	END SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current password value 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Password AS CHARACTER NO-UNDO 
    GET:
        RETURN txtPassword:Text . 
    END GET.
    SET (arg AS CHARACTER):
        ASSIGN txtPassword:Text = arg .         
    END SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current user id 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY UserId AS CHARACTER NO-UNDO 
    GET:
        RETURN txtUserId:Text . 
    END GET.
    SET (arg AS CHARACTER):
        ASSIGN txtUserId:Text = arg .         
    END SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the DatabaseLoginForm class 
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC DatabaseLoginForm ():
		
        SUPER().

        InitializeComponent().

        /* Mike Fechner, Consultingwerk Ltd. 17.03.2013
           Disable the Domain field on 10.2B */
        IF PROVERSION BEGINS "10":U THEN DO:
            THIS-OBJECT:txtDomain:Enabled = FALSE .
            THIS-OBJECT:txtDomain:Visible = FALSE .
            THIS-OBJECT:label5:Visible = FALSE . 
        END.

        ASSIGN oClearButtonProvider[1] = NEW Consultingwerk.Windows.Controls.ClearButtonProvider (txtUserId)
               oClearButtonProvider[2] = NEW Consultingwerk.Windows.Controls.ClearButtonProvider (txtPassword) 
               oClearButtonProvider[3] = NEW Consultingwerk.Windows.Controls.ClearButtonProvider (txtDomain) .

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

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
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.OpenEdgeLogin.DatabaseLoginForm":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        THIS-OBJECT:ultraGroupBox1 = NEW Infragistics.Win.Misc.UltraGroupBox().
        THIS-OBJECT:tgOptOut = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:buCancel = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:buOk = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:label1 = NEW System.Windows.Forms.Label().
        THIS-OBJECT:label2 = NEW System.Windows.Forms.Label().
        THIS-OBJECT:label3 = NEW System.Windows.Forms.Label().
        THIS-OBJECT:label4 = NEW System.Windows.Forms.Label().
        THIS-OBJECT:label5 = NEW System.Windows.Forms.Label().
        THIS-OBJECT:txtUserId = NEW System.Windows.Forms.TextBox().
        THIS-OBJECT:txtPassword = NEW System.Windows.Forms.TextBox().
        THIS-OBJECT:txtDomain = NEW System.Windows.Forms.TextBox().
        THIS-OBJECT:lblDatabase = NEW System.Windows.Forms.Label().
        CAST(THIS-OBJECT:ultraGroupBox1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraGroupBox1:SuspendLayout().
        CAST(THIS-OBJECT:tgOptOut, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraGroupBox1 */
        /*  */
        THIS-OBJECT:ultraGroupBox1:AccessibleDescription = resources:GetString("ultraGroupBox1.AccessibleDescription":U).
        THIS-OBJECT:ultraGroupBox1:AccessibleName = resources:GetString("ultraGroupBox1.AccessibleName":U).
        THIS-OBJECT:ultraGroupBox1:Anchor = CAST(resources:GetObject("ultraGroupBox1.Anchor":U), System.Windows.Forms.AnchorStyles).
        appearance1:BackColor = System.Drawing.SystemColors:ButtonFace.
        appearance1:TextHAlignAsString = resources:GetString("appearance1.TextHAlignAsString":U).
        appearance1:TextVAlignAsString = resources:GetString("appearance1.TextVAlignAsString":U).
        THIS-OBJECT:ultraGroupBox1:Appearance = appearance1.
        THIS-OBJECT:ultraGroupBox1:BackgroundImage = CAST(resources:GetObject("ultraGroupBox1.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:ultraGroupBox1:BackgroundImageLayout = CAST(resources:GetObject("ultraGroupBox1.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:ultraGroupBox1:BorderStyle = Infragistics.Win.Misc.GroupBoxBorderStyle:None.
        THIS-OBJECT:ultraGroupBox1:Controls:Add(THIS-OBJECT:tgOptOut).
        THIS-OBJECT:ultraGroupBox1:Controls:Add(THIS-OBJECT:buCancel).
        THIS-OBJECT:ultraGroupBox1:Controls:Add(THIS-OBJECT:buOk).
        THIS-OBJECT:ultraGroupBox1:Dock = CAST(resources:GetObject("ultraGroupBox1.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:ultraGroupBox1:Font = CAST(resources:GetObject("ultraGroupBox1.Font":U), System.Drawing.Font).
        THIS-OBJECT:ultraGroupBox1:ImeMode = CAST(resources:GetObject("ultraGroupBox1.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:ultraGroupBox1:Location = CAST(resources:GetObject("ultraGroupBox1.Location":U), System.Drawing.Point).
        THIS-OBJECT:ultraGroupBox1:MaximumSize = CAST(resources:GetObject("ultraGroupBox1.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:ultraGroupBox1:Name = "ultraGroupBox1":U.
        THIS-OBJECT:ultraGroupBox1:RightToLeft = CAST(resources:GetObject("ultraGroupBox1.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:ultraGroupBox1:Size = CAST(resources:GetObject("ultraGroupBox1.Size":U), System.Drawing.Size).
        THIS-OBJECT:ultraGroupBox1:TabIndex = System.Convert:ToInt32(resources:GetObject("ultraGroupBox1.TabIndex":U)).
        /*  */
        /* tgOptOut */
        /*  */
        THIS-OBJECT:tgOptOut:AccessibleDescription = resources:GetString("tgOptOut.AccessibleDescription":U).
        THIS-OBJECT:tgOptOut:AccessibleName = resources:GetString("tgOptOut.AccessibleName":U).
        THIS-OBJECT:tgOptOut:Anchor = CAST(resources:GetObject("tgOptOut.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tgOptOut:AutoSize = System.Convert:ToBoolean(resources:GetObject("tgOptOut.AutoSize":U)).
        THIS-OBJECT:tgOptOut:BackgroundImage = CAST(resources:GetObject("tgOptOut.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:tgOptOut:BackgroundImageLayout = CAST(resources:GetObject("tgOptOut.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:tgOptOut:Dock = CAST(resources:GetObject("tgOptOut.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:tgOptOut:Font = CAST(resources:GetObject("tgOptOut.Font":U), System.Drawing.Font).
        THIS-OBJECT:tgOptOut:ImeMode = CAST(resources:GetObject("tgOptOut.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:tgOptOut:Location = CAST(resources:GetObject("tgOptOut.Location":U), System.Drawing.Point).
        THIS-OBJECT:tgOptOut:MaximumSize = CAST(resources:GetObject("tgOptOut.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:tgOptOut:Name = "tgOptOut":U.
        THIS-OBJECT:tgOptOut:RightToLeft = CAST(resources:GetObject("tgOptOut.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:tgOptOut:Size = CAST(resources:GetObject("tgOptOut.Size":U), System.Drawing.Size).
        THIS-OBJECT:tgOptOut:TabIndex = System.Convert:ToInt32(resources:GetObject("tgOptOut.TabIndex":U)).
        THIS-OBJECT:tgOptOut:Text = resources:GetString("tgOptOut.Text":U).
        /*  */
        /* buCancel */
        /*  */
        THIS-OBJECT:buCancel:AccessibleDescription = resources:GetString("buCancel.AccessibleDescription":U).
        THIS-OBJECT:buCancel:AccessibleName = resources:GetString("buCancel.AccessibleName":U).
        THIS-OBJECT:buCancel:Anchor = CAST(resources:GetObject("buCancel.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:buCancel:AutoSize = System.Convert:ToBoolean(resources:GetObject("buCancel.AutoSize":U)).
        THIS-OBJECT:buCancel:BackgroundImage = CAST(resources:GetObject("buCancel.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:buCancel:BackgroundImageLayout = CAST(resources:GetObject("buCancel.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:buCancel:DialogResult = System.Windows.Forms.DialogResult:Cancel.
        THIS-OBJECT:buCancel:Dock = CAST(resources:GetObject("buCancel.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:buCancel:Font = CAST(resources:GetObject("buCancel.Font":U), System.Drawing.Font).
        THIS-OBJECT:buCancel:ImeMode = CAST(resources:GetObject("buCancel.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:buCancel:Location = CAST(resources:GetObject("buCancel.Location":U), System.Drawing.Point).
        THIS-OBJECT:buCancel:MaximumSize = CAST(resources:GetObject("buCancel.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:buCancel:Name = "buCancel":U.
        THIS-OBJECT:buCancel:RightToLeft = CAST(resources:GetObject("buCancel.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:buCancel:Size = CAST(resources:GetObject("buCancel.Size":U), System.Drawing.Size).
        THIS-OBJECT:buCancel:TabIndex = System.Convert:ToInt32(resources:GetObject("buCancel.TabIndex":U)).
        THIS-OBJECT:buCancel:Text = resources:GetString("buCancel.Text":U).
        /*  */
        /* buOk */
        /*  */
        THIS-OBJECT:buOk:AccessibleDescription = resources:GetString("buOk.AccessibleDescription":U).
        THIS-OBJECT:buOk:AccessibleName = resources:GetString("buOk.AccessibleName":U).
        THIS-OBJECT:buOk:Anchor = CAST(resources:GetObject("buOk.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:buOk:AutoSize = System.Convert:ToBoolean(resources:GetObject("buOk.AutoSize":U)).
        THIS-OBJECT:buOk:BackgroundImage = CAST(resources:GetObject("buOk.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:buOk:BackgroundImageLayout = CAST(resources:GetObject("buOk.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:buOk:DialogResult = System.Windows.Forms.DialogResult:OK.
        THIS-OBJECT:buOk:Dock = CAST(resources:GetObject("buOk.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:buOk:Font = CAST(resources:GetObject("buOk.Font":U), System.Drawing.Font).
        THIS-OBJECT:buOk:ImeMode = CAST(resources:GetObject("buOk.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:buOk:Location = CAST(resources:GetObject("buOk.Location":U), System.Drawing.Point).
        THIS-OBJECT:buOk:MaximumSize = CAST(resources:GetObject("buOk.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:buOk:Name = "buOk":U.
        THIS-OBJECT:buOk:RightToLeft = CAST(resources:GetObject("buOk.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:buOk:Size = CAST(resources:GetObject("buOk.Size":U), System.Drawing.Size).
        THIS-OBJECT:buOk:TabIndex = System.Convert:ToInt32(resources:GetObject("buOk.TabIndex":U)).
        THIS-OBJECT:buOk:Text = resources:GetString("buOk.Text":U).
        /*  */
        /* label1 */
        /*  */
        THIS-OBJECT:label1:AccessibleDescription = resources:GetString("label1.AccessibleDescription":U).
        THIS-OBJECT:label1:AccessibleName = resources:GetString("label1.AccessibleName":U).
        THIS-OBJECT:label1:Anchor = CAST(resources:GetObject("label1.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:label1:AutoSize = System.Convert:ToBoolean(resources:GetObject("label1.AutoSize":U)).
        THIS-OBJECT:label1:BackgroundImageLayout = CAST(resources:GetObject("label1.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:label1:Dock = CAST(resources:GetObject("label1.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:label1:Font = CAST(resources:GetObject("label1.Font":U), System.Drawing.Font).
        THIS-OBJECT:label1:ImageAlign = CAST(resources:GetObject("label1.ImageAlign":U), System.Drawing.ContentAlignment).
        THIS-OBJECT:label1:ImageIndex = System.Convert:ToInt32(resources:GetObject("label1.ImageIndex":U)).
        THIS-OBJECT:label1:ImageKey = resources:GetString("label1.ImageKey":U).
        THIS-OBJECT:label1:ImeMode = CAST(resources:GetObject("label1.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:label1:Location = CAST(resources:GetObject("label1.Location":U), System.Drawing.Point).
        THIS-OBJECT:label1:MaximumSize = CAST(resources:GetObject("label1.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:label1:Name = "label1":U.
        THIS-OBJECT:label1:RightToLeft = CAST(resources:GetObject("label1.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:label1:Size = CAST(resources:GetObject("label1.Size":U), System.Drawing.Size).
        THIS-OBJECT:label1:TabIndex = System.Convert:ToInt32(resources:GetObject("label1.TabIndex":U)).
        THIS-OBJECT:label1:Text = resources:GetString("label1.Text":U).
        THIS-OBJECT:label1:TextAlign = CAST(resources:GetObject("label1.TextAlign":U), System.Drawing.ContentAlignment).
        THIS-OBJECT:label1:UseCompatibleTextRendering = TRUE.
        /*  */
        /* label2 */
        /*  */
        THIS-OBJECT:label2:AccessibleDescription = resources:GetString("label2.AccessibleDescription":U).
        THIS-OBJECT:label2:AccessibleName = resources:GetString("label2.AccessibleName":U).
        THIS-OBJECT:label2:Anchor = CAST(resources:GetObject("label2.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:label2:AutoSize = System.Convert:ToBoolean(resources:GetObject("label2.AutoSize":U)).
        THIS-OBJECT:label2:BackgroundImageLayout = CAST(resources:GetObject("label2.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:label2:Dock = CAST(resources:GetObject("label2.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:label2:Font = CAST(resources:GetObject("label2.Font":U), System.Drawing.Font).
        THIS-OBJECT:label2:ImageAlign = CAST(resources:GetObject("label2.ImageAlign":U), System.Drawing.ContentAlignment).
        THIS-OBJECT:label2:ImageIndex = System.Convert:ToInt32(resources:GetObject("label2.ImageIndex":U)).
        THIS-OBJECT:label2:ImageKey = resources:GetString("label2.ImageKey":U).
        THIS-OBJECT:label2:ImeMode = CAST(resources:GetObject("label2.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:label2:Location = CAST(resources:GetObject("label2.Location":U), System.Drawing.Point).
        THIS-OBJECT:label2:MaximumSize = CAST(resources:GetObject("label2.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:label2:Name = "label2":U.
        THIS-OBJECT:label2:RightToLeft = CAST(resources:GetObject("label2.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:label2:Size = CAST(resources:GetObject("label2.Size":U), System.Drawing.Size).
        THIS-OBJECT:label2:TabIndex = System.Convert:ToInt32(resources:GetObject("label2.TabIndex":U)).
        THIS-OBJECT:label2:Text = resources:GetString("label2.Text":U).
        THIS-OBJECT:label2:TextAlign = CAST(resources:GetObject("label2.TextAlign":U), System.Drawing.ContentAlignment).
        THIS-OBJECT:label2:UseCompatibleTextRendering = TRUE.
        /*  */
        /* label3 */
        /*  */
        THIS-OBJECT:label3:AccessibleDescription = resources:GetString("label3.AccessibleDescription":U).
        THIS-OBJECT:label3:AccessibleName = resources:GetString("label3.AccessibleName":U).
        THIS-OBJECT:label3:Anchor = CAST(resources:GetObject("label3.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:label3:AutoSize = System.Convert:ToBoolean(resources:GetObject("label3.AutoSize":U)).
        THIS-OBJECT:label3:BackgroundImageLayout = CAST(resources:GetObject("label3.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:label3:Dock = CAST(resources:GetObject("label3.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:label3:Font = CAST(resources:GetObject("label3.Font":U), System.Drawing.Font).
        THIS-OBJECT:label3:ImageAlign = CAST(resources:GetObject("label3.ImageAlign":U), System.Drawing.ContentAlignment).
        THIS-OBJECT:label3:ImageIndex = System.Convert:ToInt32(resources:GetObject("label3.ImageIndex":U)).
        THIS-OBJECT:label3:ImageKey = resources:GetString("label3.ImageKey":U).
        THIS-OBJECT:label3:ImeMode = CAST(resources:GetObject("label3.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:label3:Location = CAST(resources:GetObject("label3.Location":U), System.Drawing.Point).
        THIS-OBJECT:label3:MaximumSize = CAST(resources:GetObject("label3.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:label3:Name = "label3":U.
        THIS-OBJECT:label3:RightToLeft = CAST(resources:GetObject("label3.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:label3:Size = CAST(resources:GetObject("label3.Size":U), System.Drawing.Size).
        THIS-OBJECT:label3:TabIndex = System.Convert:ToInt32(resources:GetObject("label3.TabIndex":U)).
        THIS-OBJECT:label3:Text = resources:GetString("label3.Text":U).
        THIS-OBJECT:label3:TextAlign = CAST(resources:GetObject("label3.TextAlign":U), System.Drawing.ContentAlignment).
        THIS-OBJECT:label3:UseCompatibleTextRendering = TRUE.
        /*  */
        /* label4 */
        /*  */
        THIS-OBJECT:label4:AccessibleDescription = resources:GetString("label4.AccessibleDescription":U).
        THIS-OBJECT:label4:AccessibleName = resources:GetString("label4.AccessibleName":U).
        THIS-OBJECT:label4:Anchor = CAST(resources:GetObject("label4.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:label4:AutoSize = System.Convert:ToBoolean(resources:GetObject("label4.AutoSize":U)).
        THIS-OBJECT:label4:BackgroundImageLayout = CAST(resources:GetObject("label4.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:label4:Dock = CAST(resources:GetObject("label4.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:label4:Font = CAST(resources:GetObject("label4.Font":U), System.Drawing.Font).
        THIS-OBJECT:label4:ImageAlign = CAST(resources:GetObject("label4.ImageAlign":U), System.Drawing.ContentAlignment).
        THIS-OBJECT:label4:ImageIndex = System.Convert:ToInt32(resources:GetObject("label4.ImageIndex":U)).
        THIS-OBJECT:label4:ImageKey = resources:GetString("label4.ImageKey":U).
        THIS-OBJECT:label4:ImeMode = CAST(resources:GetObject("label4.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:label4:Location = CAST(resources:GetObject("label4.Location":U), System.Drawing.Point).
        THIS-OBJECT:label4:MaximumSize = CAST(resources:GetObject("label4.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:label4:Name = "label4":U.
        THIS-OBJECT:label4:RightToLeft = CAST(resources:GetObject("label4.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:label4:Size = CAST(resources:GetObject("label4.Size":U), System.Drawing.Size).
        THIS-OBJECT:label4:TabIndex = System.Convert:ToInt32(resources:GetObject("label4.TabIndex":U)).
        THIS-OBJECT:label4:Text = resources:GetString("label4.Text":U).
        THIS-OBJECT:label4:TextAlign = CAST(resources:GetObject("label4.TextAlign":U), System.Drawing.ContentAlignment).
        THIS-OBJECT:label4:UseCompatibleTextRendering = TRUE.
        /*  */
        /* label5 */
        /*  */
        THIS-OBJECT:label5:AccessibleDescription = resources:GetString("label5.AccessibleDescription":U).
        THIS-OBJECT:label5:AccessibleName = resources:GetString("label5.AccessibleName":U).
        THIS-OBJECT:label5:Anchor = CAST(resources:GetObject("label5.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:label5:AutoSize = System.Convert:ToBoolean(resources:GetObject("label5.AutoSize":U)).
        THIS-OBJECT:label5:BackgroundImageLayout = CAST(resources:GetObject("label5.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:label5:Dock = CAST(resources:GetObject("label5.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:label5:Font = CAST(resources:GetObject("label5.Font":U), System.Drawing.Font).
        THIS-OBJECT:label5:ImageAlign = CAST(resources:GetObject("label5.ImageAlign":U), System.Drawing.ContentAlignment).
        THIS-OBJECT:label5:ImageIndex = System.Convert:ToInt32(resources:GetObject("label5.ImageIndex":U)).
        THIS-OBJECT:label5:ImageKey = resources:GetString("label5.ImageKey":U).
        THIS-OBJECT:label5:ImeMode = CAST(resources:GetObject("label5.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:label5:Location = CAST(resources:GetObject("label5.Location":U), System.Drawing.Point).
        THIS-OBJECT:label5:MaximumSize = CAST(resources:GetObject("label5.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:label5:Name = "label5":U.
        THIS-OBJECT:label5:RightToLeft = CAST(resources:GetObject("label5.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:label5:Size = CAST(resources:GetObject("label5.Size":U), System.Drawing.Size).
        THIS-OBJECT:label5:TabIndex = System.Convert:ToInt32(resources:GetObject("label5.TabIndex":U)).
        THIS-OBJECT:label5:Text = resources:GetString("label5.Text":U).
        THIS-OBJECT:label5:TextAlign = CAST(resources:GetObject("label5.TextAlign":U), System.Drawing.ContentAlignment).
        THIS-OBJECT:label5:UseCompatibleTextRendering = TRUE.
        /*  */
        /* txtUserId */
        /*  */
        THIS-OBJECT:txtUserId:AccessibleDescription = resources:GetString("txtUserId.AccessibleDescription":U).
        THIS-OBJECT:txtUserId:AccessibleName = resources:GetString("txtUserId.AccessibleName":U).
        THIS-OBJECT:txtUserId:Anchor = CAST(resources:GetObject("txtUserId.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:txtUserId:BackgroundImage = CAST(resources:GetObject("txtUserId.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:txtUserId:BackgroundImageLayout = CAST(resources:GetObject("txtUserId.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:txtUserId:Dock = CAST(resources:GetObject("txtUserId.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:txtUserId:Font = CAST(resources:GetObject("txtUserId.Font":U), System.Drawing.Font).
        THIS-OBJECT:txtUserId:ImeMode = CAST(resources:GetObject("txtUserId.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:txtUserId:Location = CAST(resources:GetObject("txtUserId.Location":U), System.Drawing.Point).
        THIS-OBJECT:txtUserId:MaximumSize = CAST(resources:GetObject("txtUserId.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:txtUserId:MaxLength = System.Convert:ToInt32(resources:GetObject("txtUserId.MaxLength":U)).
        THIS-OBJECT:txtUserId:Multiline = System.Convert:ToBoolean(resources:GetObject("txtUserId.Multiline":U)).
        THIS-OBJECT:txtUserId:Name = "txtUserId":U.
        THIS-OBJECT:txtUserId:PasswordChar = System.Convert:ToChar(resources:GetObject("txtUserId.PasswordChar":U)).
        THIS-OBJECT:txtUserId:RightToLeft = CAST(resources:GetObject("txtUserId.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:txtUserId:ScrollBars = CAST(resources:GetObject("txtUserId.ScrollBars":U), System.Windows.Forms.ScrollBars).
        THIS-OBJECT:txtUserId:Size = CAST(resources:GetObject("txtUserId.Size":U), System.Drawing.Size).
        THIS-OBJECT:txtUserId:TabIndex = System.Convert:ToInt32(resources:GetObject("txtUserId.TabIndex":U)).
        THIS-OBJECT:txtUserId:TextAlign = CAST(resources:GetObject("txtUserId.TextAlign":U), System.Windows.Forms.HorizontalAlignment).
        THIS-OBJECT:txtUserId:WordWrap = System.Convert:ToBoolean(resources:GetObject("txtUserId.WordWrap":U)).
        /*  */
        /* txtPassword */
        /*  */
        THIS-OBJECT:txtPassword:AccessibleDescription = resources:GetString("txtPassword.AccessibleDescription":U).
        THIS-OBJECT:txtPassword:AccessibleName = resources:GetString("txtPassword.AccessibleName":U).
        THIS-OBJECT:txtPassword:Anchor = CAST(resources:GetObject("txtPassword.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:txtPassword:BackgroundImage = CAST(resources:GetObject("txtPassword.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:txtPassword:BackgroundImageLayout = CAST(resources:GetObject("txtPassword.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:txtPassword:Dock = CAST(resources:GetObject("txtPassword.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:txtPassword:Font = CAST(resources:GetObject("txtPassword.Font":U), System.Drawing.Font).
        THIS-OBJECT:txtPassword:ImeMode = CAST(resources:GetObject("txtPassword.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:txtPassword:Location = CAST(resources:GetObject("txtPassword.Location":U), System.Drawing.Point).
        THIS-OBJECT:txtPassword:MaximumSize = CAST(resources:GetObject("txtPassword.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:txtPassword:MaxLength = System.Convert:ToInt32(resources:GetObject("txtPassword.MaxLength":U)).
        THIS-OBJECT:txtPassword:Multiline = System.Convert:ToBoolean(resources:GetObject("txtPassword.Multiline":U)).
        THIS-OBJECT:txtPassword:Name = "txtPassword":U.
        THIS-OBJECT:txtPassword:PasswordChar = System.Convert:ToChar(resources:GetObject("txtPassword.PasswordChar":U)).
        THIS-OBJECT:txtPassword:RightToLeft = CAST(resources:GetObject("txtPassword.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:txtPassword:ScrollBars = CAST(resources:GetObject("txtPassword.ScrollBars":U), System.Windows.Forms.ScrollBars).
        THIS-OBJECT:txtPassword:Size = CAST(resources:GetObject("txtPassword.Size":U), System.Drawing.Size).
        THIS-OBJECT:txtPassword:TabIndex = System.Convert:ToInt32(resources:GetObject("txtPassword.TabIndex":U)).
        THIS-OBJECT:txtPassword:TextAlign = CAST(resources:GetObject("txtPassword.TextAlign":U), System.Windows.Forms.HorizontalAlignment).
        THIS-OBJECT:txtPassword:WordWrap = System.Convert:ToBoolean(resources:GetObject("txtPassword.WordWrap":U)).
        /*  */
        /* txtDomain */
        /*  */
        THIS-OBJECT:txtDomain:AccessibleDescription = resources:GetString("txtDomain.AccessibleDescription":U).
        THIS-OBJECT:txtDomain:AccessibleName = resources:GetString("txtDomain.AccessibleName":U).
        THIS-OBJECT:txtDomain:Anchor = CAST(resources:GetObject("txtDomain.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:txtDomain:BackgroundImage = CAST(resources:GetObject("txtDomain.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:txtDomain:BackgroundImageLayout = CAST(resources:GetObject("txtDomain.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:txtDomain:Dock = CAST(resources:GetObject("txtDomain.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:txtDomain:Font = CAST(resources:GetObject("txtDomain.Font":U), System.Drawing.Font).
        THIS-OBJECT:txtDomain:ImeMode = CAST(resources:GetObject("txtDomain.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:txtDomain:Location = CAST(resources:GetObject("txtDomain.Location":U), System.Drawing.Point).
        THIS-OBJECT:txtDomain:MaximumSize = CAST(resources:GetObject("txtDomain.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:txtDomain:MaxLength = System.Convert:ToInt32(resources:GetObject("txtDomain.MaxLength":U)).
        THIS-OBJECT:txtDomain:Multiline = System.Convert:ToBoolean(resources:GetObject("txtDomain.Multiline":U)).
        THIS-OBJECT:txtDomain:Name = "txtDomain":U.
        THIS-OBJECT:txtDomain:PasswordChar = System.Convert:ToChar(resources:GetObject("txtDomain.PasswordChar":U)).
        THIS-OBJECT:txtDomain:RightToLeft = CAST(resources:GetObject("txtDomain.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:txtDomain:ScrollBars = CAST(resources:GetObject("txtDomain.ScrollBars":U), System.Windows.Forms.ScrollBars).
        THIS-OBJECT:txtDomain:Size = CAST(resources:GetObject("txtDomain.Size":U), System.Drawing.Size).
        THIS-OBJECT:txtDomain:TabIndex = System.Convert:ToInt32(resources:GetObject("txtDomain.TabIndex":U)).
        THIS-OBJECT:txtDomain:TextAlign = CAST(resources:GetObject("txtDomain.TextAlign":U), System.Windows.Forms.HorizontalAlignment).
        THIS-OBJECT:txtDomain:WordWrap = System.Convert:ToBoolean(resources:GetObject("txtDomain.WordWrap":U)).
        /*  */
        /* lblDatabase */
        /*  */
        THIS-OBJECT:lblDatabase:AccessibleDescription = resources:GetString("lblDatabase.AccessibleDescription":U).
        THIS-OBJECT:lblDatabase:AccessibleName = resources:GetString("lblDatabase.AccessibleName":U).
        THIS-OBJECT:lblDatabase:Anchor = CAST(resources:GetObject("lblDatabase.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:lblDatabase:AutoSize = System.Convert:ToBoolean(resources:GetObject("lblDatabase.AutoSize":U)).
        THIS-OBJECT:lblDatabase:BackgroundImageLayout = CAST(resources:GetObject("lblDatabase.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:lblDatabase:Dock = CAST(resources:GetObject("lblDatabase.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:lblDatabase:Font = CAST(resources:GetObject("lblDatabase.Font":U), System.Drawing.Font).
        THIS-OBJECT:lblDatabase:ImageAlign = CAST(resources:GetObject("lblDatabase.ImageAlign":U), System.Drawing.ContentAlignment).
        THIS-OBJECT:lblDatabase:ImageIndex = System.Convert:ToInt32(resources:GetObject("lblDatabase.ImageIndex":U)).
        THIS-OBJECT:lblDatabase:ImageKey = resources:GetString("lblDatabase.ImageKey":U).
        THIS-OBJECT:lblDatabase:ImeMode = CAST(resources:GetObject("lblDatabase.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:lblDatabase:Location = CAST(resources:GetObject("lblDatabase.Location":U), System.Drawing.Point).
        THIS-OBJECT:lblDatabase:MaximumSize = CAST(resources:GetObject("lblDatabase.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:lblDatabase:Name = "lblDatabase":U.
        THIS-OBJECT:lblDatabase:RightToLeft = CAST(resources:GetObject("lblDatabase.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:lblDatabase:Size = CAST(resources:GetObject("lblDatabase.Size":U), System.Drawing.Size).
        THIS-OBJECT:lblDatabase:TabIndex = System.Convert:ToInt32(resources:GetObject("lblDatabase.TabIndex":U)).
        THIS-OBJECT:lblDatabase:Text = resources:GetString("lblDatabase.Text":U).
        THIS-OBJECT:lblDatabase:TextAlign = CAST(resources:GetObject("lblDatabase.TextAlign":U), System.Drawing.ContentAlignment).
        THIS-OBJECT:lblDatabase:UseCompatibleTextRendering = TRUE.
        /*  */
        /* DatabaseLoginForm */
        /*  */
        THIS-OBJECT:AcceptButton = THIS-OBJECT:buOk.
        THIS-OBJECT:AccessibleDescription = resources:GetString("$this.AccessibleDescription":U).
        THIS-OBJECT:AccessibleName = resources:GetString("$this.AccessibleName":U).
        THIS-OBJECT:AutoScaleMode = System.Windows.Forms.AutoScaleMode:Inherit.
        THIS-OBJECT:AutoScroll = System.Convert:ToBoolean(resources:GetObject("$this.AutoScroll":U)).
        THIS-OBJECT:AutoSize = System.Convert:ToBoolean(resources:GetObject("$this.AutoSize":U)).
        THIS-OBJECT:AutoSizeMode = CAST(resources:GetObject("$this.AutoSizeMode":U), System.Windows.Forms.AutoSizeMode).
        THIS-OBJECT:BackColor = System.Drawing.SystemColors:ControlLightLight.
        THIS-OBJECT:BackgroundImage = CAST(resources:GetObject("$this.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:BackgroundImageLayout = CAST(resources:GetObject("$this.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:CancelButton = THIS-OBJECT:buCancel.
        THIS-OBJECT:ClientSize = CAST(resources:GetObject("$this.ClientSize":U), System.Drawing.Size).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:lblDatabase).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:txtDomain).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:txtPassword).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:txtUserId).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:label5).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:label4).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:label3).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:label2).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:label1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraGroupBox1).
        THIS-OBJECT:Font = CAST(resources:GetObject("$this.Font":U), System.Drawing.Font).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:ImeMode = CAST(resources:GetObject("$this.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:Location = CAST(resources:GetObject("$this.Location":U), System.Drawing.Point).
        THIS-OBJECT:MaximizeBox = FALSE.
        THIS-OBJECT:MaximumSize = CAST(resources:GetObject("$this.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:MinimizeBox = FALSE.
        THIS-OBJECT:Name = "DatabaseLoginForm":U.
        THIS-OBJECT:RightToLeft = CAST(resources:GetObject("$this.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:RightToLeftLayout = System.Convert:ToBoolean(resources:GetObject("$this.RightToLeftLayout":U)).
        THIS-OBJECT:StartPosition = CAST(resources:GetObject("$this.StartPosition":U), System.Windows.Forms.FormStartPosition).
        THIS-OBJECT:Text = resources:GetString("$this.Text":U).
        CAST(THIS-OBJECT:ultraGroupBox1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraGroupBox1:ResumeLayout(FALSE).
        THIS-OBJECT:ultraGroupBox1:PerformLayout().
        CAST(THIS-OBJECT:tgOptOut, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the Load event 
        Notes:   
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED OVERRIDE VOID OnLoad (e AS System.EventArgs):
		
		SUPER:OnLoad (e) .

        THIS-OBJECT:MinimumSize = THIS-OBJECT:Size .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the DatabaseLoginForm class 
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC DatabaseLoginForm ():

        GarbageCollectorHelper:DeleteObject (oClearButtonProvider[1]) .
        GarbageCollectorHelper:DeleteObject (oClearButtonProvider[2]) .
        GarbageCollectorHelper:DeleteObject (oClearButtonProvider[3]) .

    END DESTRUCTOR .

END CLASS.
