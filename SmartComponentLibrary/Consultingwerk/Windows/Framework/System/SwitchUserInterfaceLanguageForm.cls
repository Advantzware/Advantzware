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
    File        : SwitchUserInterfaceLanguageForm
    Purpose     : Form that allows the user to switch the user interface language
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Jan 15 22:35:16 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Framework.Session.*        FROM PROPATH . 
USING Consultingwerk.Forms.*                    FROM PROPATH .
USING Consultingwerk.SmartFramework.*           FROM PROPATH . 
USING Consultingwerk.Util.*                     FROM PROPATH .  
USING Consultingwerk.Windows.Framework.*        FROM PROPATH . 
USING Consultingwerk.Windows.Framework.System.* FROM PROPATH .  
USING Progress.Lang.*                           FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.System.SwitchUserInterfaceLanguageForm INHERITS BaseForm: 
	
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraComboEditor1 AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraButtonOK AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraButtonCancel AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE pictureBox1 AS System.Windows.Forms.PictureBox NO-UNDO.

    {Consultingwerk/SmartFramework/ttLanguage.i &NO-BEFORE &REFERENCE-ONLY=REFERENCE-ONLY}  

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SwitchUserInterfaceLanguageForm class 
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC SwitchUserInterfaceLanguageForm ():
		
        SUPER().
        InitializeComponent().
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
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.System.SwitchUserInterfaceLanguageForm":U).
        THIS-OBJECT:pictureBox1 = NEW System.Windows.Forms.PictureBox().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraButtonOK = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:ultraButtonCancel = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:ultraComboEditor1 = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        CAST(THIS-OBJECT:pictureBox1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraComboEditor1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* pictureBox1 */
        /*  */
        THIS-OBJECT:pictureBox1:AccessibleDescription = resources:GetString("pictureBox1.AccessibleDescription":U).
        THIS-OBJECT:pictureBox1:AccessibleName = resources:GetString("pictureBox1.AccessibleName":U).
        THIS-OBJECT:pictureBox1:Anchor = CAST(resources:GetObject("pictureBox1.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:pictureBox1:BackgroundImage = CAST(resources:GetObject("pictureBox1.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:pictureBox1:BackgroundImageLayout = CAST(resources:GetObject("pictureBox1.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:pictureBox1:Dock = CAST(resources:GetObject("pictureBox1.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:pictureBox1:Font = CAST(resources:GetObject("pictureBox1.Font":U), System.Drawing.Font).
        THIS-OBJECT:pictureBox1:Image = CAST(resources:GetObject("pictureBox1.Image":U), System.Drawing.Image).
        THIS-OBJECT:pictureBox1:ImageLocation = resources:GetString("pictureBox1.ImageLocation":U).
        THIS-OBJECT:pictureBox1:ImeMode = CAST(resources:GetObject("pictureBox1.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:pictureBox1:Location = CAST(resources:GetObject("pictureBox1.Location":U), System.Drawing.Point).
        THIS-OBJECT:pictureBox1:MaximumSize = CAST(resources:GetObject("pictureBox1.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:pictureBox1:Name = "pictureBox1":U.
        THIS-OBJECT:pictureBox1:RightToLeft = CAST(resources:GetObject("pictureBox1.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:pictureBox1:Size = CAST(resources:GetObject("pictureBox1.Size":U), System.Drawing.Size).
        THIS-OBJECT:pictureBox1:SizeMode = CAST(resources:GetObject("pictureBox1.SizeMode":U), System.Windows.Forms.PictureBoxSizeMode).
        THIS-OBJECT:pictureBox1:TabIndex = System.Convert:ToInt32(resources:GetObject("pictureBox1.TabIndex":U)).
        THIS-OBJECT:pictureBox1:TabStop = FALSE.
        THIS-OBJECT:pictureBox1:WaitOnLoad = System.Convert:ToBoolean(resources:GetObject("pictureBox1.WaitOnLoad":U)).
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
        /* ultraButtonOK */
        /*  */
        THIS-OBJECT:ultraButtonOK:AccessibleDescription = resources:GetString("ultraButtonOK.AccessibleDescription":U).
        THIS-OBJECT:ultraButtonOK:AccessibleName = resources:GetString("ultraButtonOK.AccessibleName":U).
        THIS-OBJECT:ultraButtonOK:Anchor = CAST(resources:GetObject("ultraButtonOK.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraButtonOK:AutoSize = System.Convert:ToBoolean(resources:GetObject("ultraButtonOK.AutoSize":U)).
        THIS-OBJECT:ultraButtonOK:BackgroundImage = CAST(resources:GetObject("ultraButtonOK.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:ultraButtonOK:BackgroundImageLayout = CAST(resources:GetObject("ultraButtonOK.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:ultraButtonOK:DialogResult = System.Windows.Forms.DialogResult:OK.
        THIS-OBJECT:ultraButtonOK:Dock = CAST(resources:GetObject("ultraButtonOK.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:ultraButtonOK:Enabled = System.Convert:ToBoolean(resources:GetObject("ultraButtonOK.Enabled":U)).
        THIS-OBJECT:ultraButtonOK:Font = CAST(resources:GetObject("ultraButtonOK.Font":U), System.Drawing.Font).
        THIS-OBJECT:ultraButtonOK:ImeMode = CAST(resources:GetObject("ultraButtonOK.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:ultraButtonOK:Location = CAST(resources:GetObject("ultraButtonOK.Location":U), System.Drawing.Point).
        THIS-OBJECT:ultraButtonOK:MaximumSize = CAST(resources:GetObject("ultraButtonOK.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:ultraButtonOK:Name = "ultraButtonOK":U.
        THIS-OBJECT:ultraButtonOK:RightToLeft = CAST(resources:GetObject("ultraButtonOK.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:ultraButtonOK:Size = CAST(resources:GetObject("ultraButtonOK.Size":U), System.Drawing.Size).
        THIS-OBJECT:ultraButtonOK:TabIndex = System.Convert:ToInt32(resources:GetObject("ultraButtonOK.TabIndex":U)).
        THIS-OBJECT:ultraButtonOK:Text = resources:GetString("ultraButtonOK.Text":U).
        THIS-OBJECT:ultraButtonOK:Click:Subscribe(THIS-OBJECT:ultraButtonOK_Click).
        /*  */
        /* ultraButtonCancel */
        /*  */
        THIS-OBJECT:ultraButtonCancel:AccessibleDescription = resources:GetString("ultraButtonCancel.AccessibleDescription":U).
        THIS-OBJECT:ultraButtonCancel:AccessibleName = resources:GetString("ultraButtonCancel.AccessibleName":U).
        THIS-OBJECT:ultraButtonCancel:Anchor = CAST(resources:GetObject("ultraButtonCancel.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraButtonCancel:AutoSize = System.Convert:ToBoolean(resources:GetObject("ultraButtonCancel.AutoSize":U)).
        THIS-OBJECT:ultraButtonCancel:BackgroundImage = CAST(resources:GetObject("ultraButtonCancel.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:ultraButtonCancel:BackgroundImageLayout = CAST(resources:GetObject("ultraButtonCancel.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:ultraButtonCancel:DialogResult = System.Windows.Forms.DialogResult:Cancel.
        THIS-OBJECT:ultraButtonCancel:Dock = CAST(resources:GetObject("ultraButtonCancel.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:ultraButtonCancel:Font = CAST(resources:GetObject("ultraButtonCancel.Font":U), System.Drawing.Font).
        THIS-OBJECT:ultraButtonCancel:ImeMode = CAST(resources:GetObject("ultraButtonCancel.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:ultraButtonCancel:Location = CAST(resources:GetObject("ultraButtonCancel.Location":U), System.Drawing.Point).
        THIS-OBJECT:ultraButtonCancel:MaximumSize = CAST(resources:GetObject("ultraButtonCancel.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:ultraButtonCancel:Name = "ultraButtonCancel":U.
        THIS-OBJECT:ultraButtonCancel:RightToLeft = CAST(resources:GetObject("ultraButtonCancel.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:ultraButtonCancel:Size = CAST(resources:GetObject("ultraButtonCancel.Size":U), System.Drawing.Size).
        THIS-OBJECT:ultraButtonCancel:TabIndex = System.Convert:ToInt32(resources:GetObject("ultraButtonCancel.TabIndex":U)).
        THIS-OBJECT:ultraButtonCancel:Text = resources:GetString("ultraButtonCancel.Text":U).
        /*  */
        /* ultraComboEditor1 */
        /*  */
        THIS-OBJECT:ultraComboEditor1:AccessibleDescription = resources:GetString("ultraComboEditor1.AccessibleDescription":U).
        THIS-OBJECT:ultraComboEditor1:AccessibleName = resources:GetString("ultraComboEditor1.AccessibleName":U).
        THIS-OBJECT:ultraComboEditor1:Anchor = CAST(resources:GetObject("ultraComboEditor1.Anchor":U), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraComboEditor1:AutoSize = System.Convert:ToBoolean(resources:GetObject("ultraComboEditor1.AutoSize":U)).
        THIS-OBJECT:ultraComboEditor1:BackgroundImage = CAST(resources:GetObject("ultraComboEditor1.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:ultraComboEditor1:BackgroundImageLayout = CAST(resources:GetObject("ultraComboEditor1.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:ultraComboEditor1:Dock = CAST(resources:GetObject("ultraComboEditor1.Dock":U), System.Windows.Forms.DockStyle).
        THIS-OBJECT:ultraComboEditor1:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        THIS-OBJECT:ultraComboEditor1:Font = CAST(resources:GetObject("ultraComboEditor1.Font":U), System.Drawing.Font).
        THIS-OBJECT:ultraComboEditor1:ImeMode = CAST(resources:GetObject("ultraComboEditor1.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:ultraComboEditor1:LimitToList = TRUE.
        THIS-OBJECT:ultraComboEditor1:Location = CAST(resources:GetObject("ultraComboEditor1.Location":U), System.Drawing.Point).
        THIS-OBJECT:ultraComboEditor1:MaximumSize = CAST(resources:GetObject("ultraComboEditor1.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:ultraComboEditor1:Name = "ultraComboEditor1":U.
        THIS-OBJECT:ultraComboEditor1:RightToLeft = CAST(resources:GetObject("ultraComboEditor1.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:ultraComboEditor1:Size = CAST(resources:GetObject("ultraComboEditor1.Size":U), System.Drawing.Size).
        THIS-OBJECT:ultraComboEditor1:TabIndex = System.Convert:ToInt32(resources:GetObject("ultraComboEditor1.TabIndex":U)).
        THIS-OBJECT:ultraComboEditor1:ValueChanged:Subscribe(THIS-OBJECT:ultraComboEditor1_ValueChanged).
        /*  */
        /* SwitchUserInterfaceLanguageForm */
        /*  */
        THIS-OBJECT:AcceptButton = THIS-OBJECT:ultraButtonOK.
        THIS-OBJECT:AccessibleDescription = resources:GetString("$this.AccessibleDescription":U).
        THIS-OBJECT:AccessibleName = resources:GetString("$this.AccessibleName":U).
        THIS-OBJECT:AutoScroll = System.Convert:ToBoolean(resources:GetObject("$this.AutoScroll":U)).
        THIS-OBJECT:AutoSize = System.Convert:ToBoolean(resources:GetObject("$this.AutoSize":U)).
        THIS-OBJECT:AutoSizeMode = CAST(resources:GetObject("$this.AutoSizeMode":U), System.Windows.Forms.AutoSizeMode).
        THIS-OBJECT:BackgroundImage = CAST(resources:GetObject("$this.BackgroundImage":U), System.Drawing.Image).
        THIS-OBJECT:BackgroundImageLayout = CAST(resources:GetObject("$this.BackgroundImageLayout":U), System.Windows.Forms.ImageLayout).
        THIS-OBJECT:CancelButton = THIS-OBJECT:ultraButtonCancel.
        THIS-OBJECT:ClientSize = CAST(resources:GetObject("$this.ClientSize":U), System.Drawing.Size).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:pictureBox1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraButtonOK).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraButtonCancel).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraComboEditor1).
        THIS-OBJECT:Font = CAST(resources:GetObject("$this.Font":U), System.Drawing.Font).
        THIS-OBJECT:FormBorderStyle = System.Windows.Forms.FormBorderStyle:FixedDialog.
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:ImeMode = CAST(resources:GetObject("$this.ImeMode":U), System.Windows.Forms.ImeMode).
        THIS-OBJECT:Location = CAST(resources:GetObject("$this.Location":U), System.Drawing.Point).
        THIS-OBJECT:MaximizeBox = FALSE.
        THIS-OBJECT:MaximumSize = CAST(resources:GetObject("$this.MaximumSize":U), System.Drawing.Size).
        THIS-OBJECT:MinimizeBox = FALSE.
        THIS-OBJECT:Name = "SwitchUserInterfaceLanguageForm":U.
        THIS-OBJECT:RightToLeft = CAST(resources:GetObject("$this.RightToLeft":U), System.Windows.Forms.RightToLeft).
        THIS-OBJECT:RightToLeftLayout = System.Convert:ToBoolean(resources:GetObject("$this.RightToLeftLayout":U)).
        THIS-OBJECT:ShowInTaskbar = FALSE.
        THIS-OBJECT:StartPosition = CAST(resources:GetObject("$this.StartPosition":U), System.Windows.Forms.FormStartPosition).
        THIS-OBJECT:StoreFormPosition = FALSE.
        THIS-OBJECT:Text = resources:GetString("$this.Text":U).
        THIS-OBJECT:WindowPositionRegistryKey = resources:GetString("$this.WindowPositionRegistryKey":U).
        CAST(THIS-OBJECT:pictureBox1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraComboEditor1, System.ComponentModel.ISupportInitialize):EndInit().
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
		
        DEFINE VARIABLE oValueListItem   AS Infragistics.Win.ValueListItem NO-UNDO . 
		
		SUPER:OnLoad (e) .
		
        &IF DEFINED (SmartFramework) NE 0 &THEN
        
        DEFINE VARIABLE oImageProvider    AS IImageProvider    NO-UNDO .
        DEFINE VARIABLE oLanguageProvider AS ILanguageProvider NO-UNDO . 
        
        ASSIGN oLanguageProvider = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.ILanguageProvider} 
               oImageProvider = {Consultingwerk/get-service.i Consultingwerk.Windows.Framework.IImageProvider}.
        
        ultraComboEditor1:ValueList:ValueListItems:Clear () .
            
        oLanguageProvider:GetAvailableLanguages (OUTPUT TABLE ttLanguage) .
        
        FOR EACH ttLanguage BY ttLanguage.LanguageName:
            oValueListItem = ultraComboEditor1:ValueList:ValueListItems:Add (BOX (ttLanguage.LanguageKey),
                                                                             ttLanguage.LanguageName) .
            
            oValueListItem:Tag = BOX (ttLanguage.LanguageIsoCode) .
            
            IF ttLanguage.LanguageImage > "":U AND VALID-OBJECT (oImageProvider) THEN 
                oValueListItem:Appearance:Image = oImageProvider:ImageForKey (ttLanguage.LanguageImage) .
        END.
        
        IF VALID-OBJECT (oImageProvider) THEN 
            ultraComboEditor1:ValueList:DisplayStyle = Infragistics.Win.ValueListDisplayStyle:DisplayTextAndPicture .
            
        ultraComboEditor1:Value = SessionManager:LanguageKey .            
            
        &ENDIF		

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the OK Button
        Notes:   
        @param sender The reference to hte object that raised the event
        @param e The System.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID ultraButtonOK_Click (sender AS System.Object, 
	                                         e AS System.EventArgs):
		
		DEFINE VARIABLE oTranslationProvider AS Consultingwerk.SmartFramework.ITranslationProvider NO-UNDO . 
		
        SessionManager:LanguageKey = UNBOX (THIS-OBJECT:ultraComboEditor1:Value) .
        
        oTranslationProvider = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.ITranslationProvider} .
        
        IF VALID-OBJECT (oTranslationProvider) THEN 
            oTranslationProvider:CacheAllTranslations () .
        
        SessionManager:LanguageDisplayName = THIS-OBJECT:ultraComboEditor1:SelectedItem:DisplayText .
        SessionManager:LanguageIsoCode = UNBOX (THIS-OBJECT:ultraComboEditor1:SelectedItem:Tag) .

        CATCH err AS Progress.Lang.Error :
        	ErrorHelper:ShowErrorMessage (err) .	
        END CATCH.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ValueChanged event of the language combo box
        Notes:   
        @param sender The reference to hte object that raised the event
        @param e The System.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID ultraComboEditor1_ValueChanged (sender AS System.Object, 
	                                                    e AS System.EventArgs):
		
		ultraButtonOK:Enabled = (UNBOX (ultraComboEditor1:Value) > "":U) .  

        CATCH err AS Progress.Lang.Error :
            ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SwitchUserInterfaceLanguageForm class 
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SwitchUserInterfaceLanguageForm ():

    END DESTRUCTOR .

END CLASS.
