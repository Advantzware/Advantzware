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
    File        : CharacterInputPromptForm
    Purpose     : Input prompt for a character value
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Feb 25 23:12:26 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                    FROM PROPATH . 
USING Consultingwerk.Framework.*          FROM PROPATH . 
USING Consultingwerk.Framework.Enum.*     FROM PROPATH .
USING Consultingwerk.Util.*               FROM PROPATH .   
USING Consultingwerk.Windows.Util.Forms.* FROM PROPATH .
USING Progress.Lang.*                     FROM PROPATH .
USING System.Windows.Forms.*              FROM ASSEMBLY . 

CLASS Consultingwerk.Windows.Util.Forms.CharacterInputPromptForm 
    INHERITS BaseInputPromptForm: 
    
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE saveFileDialog1 AS System.Windows.Forms.SaveFileDialog NO-UNDO.
    DEFINE PRIVATE VARIABLE openFileDialog1 AS System.Windows.Forms.OpenFileDialog NO-UNDO.
    DEFINE PRIVATE VARIABLE folderBrowserDialog1 AS System.Windows.Forms.FolderBrowserDialog NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraComboEditor1 AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTextEditor1 AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Character Value of the CharacterInputPromptForm
        Notes:                                                          
    ------------------------------------------------------------------------------*/
 	DEFINE PUBLIC PROPERTY CharacterValue AS CHARACTER NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the reference to the InputValidator for this instance
        Notes:                                                          
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY InputValidator AS ICharacterInputValidator NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the PromptType as a CharacterPromptTypeEnum value
        Notes:                                                          
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY PromptType AS CharacterPromptTypeEnum NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the ValueList as a ListNameValuePair
        Notes:   When this property is set, the Dialog will use an UltraComboEditor                                                          
                 for the data entry
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY ValueList AS ListNameValuePair NO-UNDO  
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the CharacterInputPromptForm class                                                               
        Notes:                                                          
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC CharacterInputPromptForm ():
        
        SUPER().

        InitializeComponent().
        
        THIS-OBJECT:buOk:Click:Subscribe (OkButtonClickHandler) .
                
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
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE editorButton1 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton1 = NEW Infragistics.Win.UltraWinEditors.EditorButton().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Util.Forms.CharacterInputPromptForm":U).
        THIS-OBJECT:ultraTextEditor1 = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraComboEditor1 = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        THIS-OBJECT:folderBrowserDialog1 = NEW System.Windows.Forms.FolderBrowserDialog().
        THIS-OBJECT:openFileDialog1 = NEW System.Windows.Forms.OpenFileDialog().
        THIS-OBJECT:saveFileDialog1 = NEW System.Windows.Forms.SaveFileDialog().
        CAST(THIS-OBJECT:ultraGroupBox1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraGroupBox1:SuspendLayout().
        CAST(THIS-OBJECT:ultraTextEditor1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraComboEditor1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraGroupBox1 */
        /*  */
        THIS-OBJECT:ultraGroupBox1:Location = NEW System.Drawing.Point(0, 154).
        /*  */
        /* ultraTextEditor1 */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraTextEditor1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        appearance1:Image = CAST(resources:GetObject("appearance1.Image":U), System.Object).
        editorButton1:Appearance = appearance1.
        editorButton1:Visible = FALSE.
        THIS-OBJECT:ultraTextEditor1:ButtonsRight:Add(editorButton1).
        THIS-OBJECT:ultraTextEditor1:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:ultraTextEditor1:Location = NEW System.Drawing.Point(12, 118).
        THIS-OBJECT:ultraTextEditor1:Name = "ultraTextEditor1":U.
        THIS-OBJECT:ultraTextEditor1:Size = NEW System.Drawing.Size(392, 24).
        THIS-OBJECT:ultraTextEditor1:TabIndex = 6.
        THIS-OBJECT:ultraTextEditor1:EditorButtonClick:Subscribe(THIS-OBJECT:ultraTextEditor1_EditorButtonClick).
        /*  */
        /* ultraComboEditor1 */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraComboEditor1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraComboEditor1:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:ultraComboEditor1:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        THIS-OBJECT:ultraComboEditor1:Location = NEW System.Drawing.Point(12, 118).
        THIS-OBJECT:ultraComboEditor1:Name = "ultraComboEditor1":U.
        THIS-OBJECT:ultraComboEditor1:Size = NEW System.Drawing.Size(392, 24).
        THIS-OBJECT:ultraComboEditor1:TabIndex = 7.
        /*  */
        /* openFileDialog1 */
        /*  */
        THIS-OBJECT:openFileDialog1:FileName = "":U.
        THIS-OBJECT:openFileDialog1:RestoreDirectory = TRUE.
        /*  */
        /* saveFileDialog1 */
        /*  */
        THIS-OBJECT:saveFileDialog1:RestoreDirectory = TRUE.
        /*  */
        /* CharacterInputPromptForm */
        /*  */
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(412, 198).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraTextEditor1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraComboEditor1).
        THIS-OBJECT:MinimumSize = NEW System.Drawing.Size(428, 232).
        THIS-OBJECT:Name = "CharacterInputPromptForm":U.
        THIS-OBJECT:Text = "Question":U.
        THIS-OBJECT:Controls:SetChildIndex(THIS-OBJECT:ultraComboEditor1, 0).
        THIS-OBJECT:Controls:SetChildIndex(THIS-OBJECT:ultraTextEditor1, 0).
        THIS-OBJECT:Controls:SetChildIndex(THIS-OBJECT:ultraGroupBox1, 0).
        CAST(THIS-OBJECT:ultraGroupBox1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraGroupBox1:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:ultraTextEditor1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraComboEditor1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

	/*------------------------------------------------------------------------------
		Purpose: Event handler for the Click event of the OK Button 																	  
		Notes:   Assigns the new CharacterValue 													
		@param sender The object that raised the event
		@param e The System.EventArgs with the data for this event				  
	------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID OkButtonClickHandler (sender AS System.Object,
	                                            e AS System.EventArgs):
		
		IF NOT VALID-OBJECT (THIS-OBJECT:ValueList) THEN 
		    THIS-OBJECT:CharacterValue = UNBOX (THIS-OBJECT:ultraTextEditor1:Value) . 
        ELSE 
            THIS-OBJECT:CharacterValue = UNBOX (THIS-OBJECT:ultraComboEditor1:Value) . 
        
	END METHOD.

	/*------------------------------------------------------------------------------
		Purpose: Raises the Load event 																	  
		Notes:  						
		@param e The System.EventArgs with the data for this event 											  
	------------------------------------------------------------------------------*/
	METHOD OVERRIDE PROTECTED VOID OnLoad (e AS System.EventArgs):
		
		IF NOT VALID-OBJECT (THIS-OBJECT:ValueList) THEN DO:
		    IF NOT VALID-OBJECT (THIS-OBJECT:PromptType) THEN 
		        THIS-OBJECT:PromptType = CharacterPromptTypeEnum:TextBox .
		    
		    CASE THIS-OBJECT:PromptType:
                WHEN CharacterPromptTypeEnum:TextEditor THEN DO:
                    
                    ultraTextEditor1:Multiline = TRUE . 
                    ultraTextEditor1:WordWrap = TRUE . 
                    ultraTextEditor1:Scrollbars = ScrollBars:Both .
                    ultraTextEditor1:Anchor = CAST (EnumHelperExt:Or (AnchorStyles:Top,
                                                                      AnchorStyles:Left,
                                                                      AnchorStyles:Right,
                                                                      AnchorStyles:Bottom),
                                                    AnchorStyles) .
                END .
                WHEN CharacterPromptTypeEnum:DirectoryName THEN DO:
                    ultraTextEditor1:ButtonsRight[0]:Visible = TRUE .
                END.
                WHEN CharacterPromptTypeEnum:FileNameAskOverwrite THEN DO:
                    ultraTextEditor1:ButtonsRight[0]:Visible = TRUE .
                END.
                WHEN CharacterPromptTypeEnum:FileNameMustExist THEN DO:
                    ultraTextEditor1:ButtonsRight[0]:Visible = TRUE .
                END.
		        
		    END CASE . 
		    
		    THIS-OBJECT:ultraTextEditor1:Text = THIS-OBJECT:CharacterValue . 
		    THIS-OBJECT:ultraComboEditor1:Visible = FALSE . 
		END.
		ELSE DO:
		    THIS-OBJECT:ultraComboEditor1:ValueList = ValueListHelper:FromList (THIS-OBJECT:ValueList) .
            THIS-OBJECT:ultraComboEditor1:Value = BOX (THIS-OBJECT:CharacterValue) . 
            THIS-OBJECT:ultraTextEditor1:Visible = FALSE . 
		END.
		
		SUPER:OnLoad (e) .

        IF VALID-OBJECT (THIS-OBJECT:InputValidator) THEN DO:
            IF NOT VALID-OBJECT (THIS-OBJECT:ValueList) THEN 
                THIS-OBJECT:ultraTextEditor1:ValueChanged:Subscribe (ValueChangedHandler) .
            ELSE 
                THIS-OBJECT:ultraComboEditor1:ValueChanged:Subscribe (ValueChangedHandler) .

            THIS-OBJECT:ValueChangedHandler (THIS-OBJECT:ultraTextEditor1,
                                             System.EventArgs:Empty) .
        END.

        THIS-OBJECT:MinimumSize = THIS-OBJECT:Size .

        IF THIS-OBJECT:PromptType = CharacterPromptTypeEnum:TextEditor THEN 
            THIS-OBJECT:Height = THIS-OBJECT:Height + 50 . 

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the Shown event                                                                    
        Notes:                          
        @param e The System.EventArgs with the data for this event                                            
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID OnShown (e AS System.EventArgs):
        
        SUPER:OnShown (e) .
        
        THIS-OBJECT:ultraTextEditor1:Focus () .
        THIS-OBJECT:ultraTextEditor1:SelectAll () .
        
    END METHOD .
    
	/*------------------------------------------------------------------------------
		Purpose: Event handler for the EditorButtonClick event of the ultraTextEditor1  																	  
		Notes:  																	  
		@param sender The reference to the System.Object that raised the event
		@param e The EdutorButtonEventArgs with the data for this event
	------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID ultraTextEditor1_EditorButtonClick (sender AS System.Object, 
	                                                        e AS Infragistics.Win.UltraWinEditors.EditorButtonEventArgs):
		
		DEFINE VARIABLE oDialogResult AS DialogResult NO-UNDO . 
		
		CASE THIS-OBJECT:PromptType:
		    WHEN CharacterPromptTypeEnum:DirectoryName THEN DO:
	
	            ASSIGN folderBrowserDialog1:Description  = THIS-OBJECT:Title
	                   folderBrowserDialog1:SelectedPath = UNBOX (ultraTextEditor1:Value) .
		        
                WAIT-FOR folderBrowserDialog1:ShowDialog () SET oDialogResult . 
                
                {Consultingwerk/check-dialogresult-ok.i oDialogResult} .

                ultraTextEditor1:Value = BOX (folderBrowserDialog1:SelectedPath) . 
		    END . 
            WHEN CharacterPromptTypeEnum:FileNameAskOverwrite THEN DO:
    
                ASSIGN saveFileDialog1:Title = THIS-OBJECT:Title
                       saveFileDialog1:FileName = UNBOX (ultraTextEditor1:Value) .
                
                WAIT-FOR saveFileDialog1:ShowDialog () SET oDialogResult . 
                
                {Consultingwerk/check-dialogresult-ok.i oDialogResult} .

                ultraTextEditor1:Value = BOX (saveFileDialog1:FileName) . 
            END . 
            WHEN CharacterPromptTypeEnum:FileNameMustExist THEN DO:
    
                ASSIGN openFileDialog1:Title = THIS-OBJECT:Title
                       openFileDialog1:FileName = UNBOX (ultraTextEditor1:Value) .
                
                WAIT-FOR openFileDialog1:ShowDialog () SET oDialogResult . 
                
                {Consultingwerk/check-dialogresult-ok.i oDialogResult} .

                ultraTextEditor1:Value = BOX (openFileDialog1:FileName) . 
            END . 
		END CASE . 

	END METHOD.

	/*------------------------------------------------------------------------------
		Purpose: Event handler for the ValueChanged event of the text editor  																	  
		Notes:   Uses the InputValidator instance to validate the value														
		@param sender The reference to the System.Object that raised this event
		@param e The System.EventArgs with the data for this event
	------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID ValueChangedHandler (sender AS System.Object,
	                                           e AS System.EventArgs):
		
		IF VALID-OBJECT (THIS-OBJECT:InputValidator) THEN DO:
            IF NOT VALID-OBJECT (THIS-OBJECT:ValueList) THEN 	
    		    THIS-OBJECT:buOk:Enabled = THIS-OBJECT:InputValidator:ValidateCharacterValue 
    		                                      (UNBOX (THIS-OBJECT:ultraTextEditor1:Value)) .
		    ELSE 
                THIS-OBJECT:buOk:Enabled = THIS-OBJECT:InputValidator:ValidateCharacterValue 
                                                  (UNBOX (THIS-OBJECT:ultraComboEditor1:Value)) .
		END.
		
	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the CharacterInputPromptForm class                                                               
        Notes:                                                          
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC CharacterInputPromptForm ():

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
