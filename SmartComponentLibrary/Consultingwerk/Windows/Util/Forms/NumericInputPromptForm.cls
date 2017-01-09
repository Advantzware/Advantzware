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
    File        : NumericInputPromptForm
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

CLASS Consultingwerk.Windows.Util.Forms.NumericInputPromptForm 
    INHERITS BaseInputPromptForm: 
    
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraNumericEditor1 AS Infragistics.Win.UltraWinEditors.UltraNumericEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTrackBar1 AS Infragistics.Win.UltraWinEditors.UltraTrackBar NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the reference to the InputValidator for this instance
        Notes:                                                          
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DecimalInputValidator AS IDecimalInputValidator NO-UNDO 
    GET.
    SET. 
    
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Decimal Value of the NumericInputPromptForm
        Notes:                                                          
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DecimalValue AS DECIMAL NO-UNDO 
    GET.
    SET. 
    
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the reference to the InputValidator for this instance
        Notes:                                                          
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY IntegerInputValidator AS IIntegerInputValidator NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Integer Value of the NumericInputPromptForm
        Notes:                                                          
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY IntegerValue AS INTEGER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Maximum Value of the NumericInputPromptForm
        Notes:                                                          
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY MaximumValue AS DECIMAL NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Minimum Value of the NumericInputPromptForm
        Notes:                                                          
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY MinimumValue AS DECIMAL NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the NumericType as a NumericTypeEnum value
        Notes:                                                          
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY NumericType AS NumericTypeEnum NO-UNDO 
    GET.
    SET. 
    
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the PromptType as a NumericPromptTypeEnum value
        Notes:                                                          
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY PromptType AS NumericPromptTypeEnum NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the NumericInputPromptForm class                                                               
        Notes:                                                          
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC NumericInputPromptForm ():
        
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
        THIS-OBJECT:ultraNumericEditor1 = NEW Infragistics.Win.UltraWinEditors.UltraNumericEditor().
        THIS-OBJECT:ultraTrackBar1 = NEW Infragistics.Win.UltraWinEditors.UltraTrackBar().
        CAST(THIS-OBJECT:ultraGroupBox1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraGroupBox1:SuspendLayout().
        CAST(THIS-OBJECT:ultraNumericEditor1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraTrackBar1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraGroupBox1 */
        /*  */
        THIS-OBJECT:ultraGroupBox1:Location = NEW System.Drawing.Point(0, 154).
        /*  */
        /* ultraNumericEditor1 */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraNumericEditor1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraNumericEditor1:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:ultraNumericEditor1:Location = NEW System.Drawing.Point(12, 118).
        THIS-OBJECT:ultraNumericEditor1:Name = "ultraNumericEditor1":U.
        THIS-OBJECT:ultraNumericEditor1:Size = NEW System.Drawing.Size(392, 21).
        THIS-OBJECT:ultraNumericEditor1:SpinButtonDisplayStyle = Infragistics.Win.ButtonDisplayStyle:Always.
        THIS-OBJECT:ultraNumericEditor1:SpinWrap = TRUE.
        THIS-OBJECT:ultraNumericEditor1:TabIndex = 8.
        /*  */
        /* ultraTrackBar1 */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraTrackBar1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraTrackBar1:Location = NEW System.Drawing.Point(12, 118).
        THIS-OBJECT:ultraTrackBar1:Name = "ultraTrackBar1":U.
        THIS-OBJECT:ultraTrackBar1:Size = NEW System.Drawing.Size(392, 29).
        THIS-OBJECT:ultraTrackBar1:TabIndex = 9.
        /*  */
        /* NumericInputPromptForm */
        /*  */
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(412, 198).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraTrackBar1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraNumericEditor1).
        THIS-OBJECT:MinimumSize = NEW System.Drawing.Size(428, 232).
        THIS-OBJECT:Name = "NumericInputPromptForm":U.
        THIS-OBJECT:Text = "Question":U.
        THIS-OBJECT:Controls:SetChildIndex(THIS-OBJECT:ultraGroupBox1, 0).
        THIS-OBJECT:Controls:SetChildIndex(THIS-OBJECT:ultraNumericEditor1, 0).
        THIS-OBJECT:Controls:SetChildIndex(THIS-OBJECT:ultraTrackBar1, 0).
        CAST(THIS-OBJECT:ultraGroupBox1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraGroupBox1:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:ultraNumericEditor1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraTrackBar1, System.ComponentModel.ISupportInitialize):EndInit().
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
		
        IF THIS-OBJECT:PromptType = NumericPromptTypeEnum:TextBox THEN DO:		
    		IF THIS-OBJECT:NumericType = NumericTypeEnum:Integer THEN 
    		    THIS-OBJECT:IntegerValue = UNBOX (THIS-OBJECT:ultraNumericEditor1:Value) .
            ELSE
                THIS-OBJECT:DecimalValue = UNBOX (THIS-OBJECT:ultraNumericEditor1:Value) .
        END.
        ELSE DO:
            IF THIS-OBJECT:NumericType = NumericTypeEnum:Integer THEN 
                THIS-OBJECT:IntegerValue = THIS-OBJECT:ultraTrackBar1:Value .
            ELSE
                THIS-OBJECT:DecimalValue = THIS-OBJECT:ultraTrackBar1:Value .
        END.
        
	END METHOD.

	/*------------------------------------------------------------------------------
		Purpose: Raises the Load event 																	  
		Notes:  						
		@param e The System.EventArgs with the data for this event 											  
	------------------------------------------------------------------------------*/
	METHOD OVERRIDE PROTECTED VOID OnLoad (e AS System.EventArgs):
	
        IF THIS-OBJECT:NumericType = NumericTypeEnum:Integer THEN DO:
    
            IF THIS-OBJECT:PromptType = NumericPromptTypeEnum:TextBox THEN DO:
                ultraNumericEditor1:NumericType = Infragistics.Win.UltraWinEditors.NumericType:Integer . 
                ultraNumericEditor1:Value = BOX (THIS-OBJECT:IntegerValue) .
                ultraNumericEditor1:MaxValue = BOX (INTEGER (THIS-OBJECT:MaximumValue)) .
                ultraNumericEditor1:MinValue = BOX (INTEGER (THIS-OBJECT:MinimumValue)) .
                ultraTrackBar1:Visible = FALSE . 
            END. 
            ELSE DO:
                ultraTrackBar1:Value = THIS-OBJECT:IntegerValue .
                ultraTrackBar1:MaxValue = INTEGER (THIS-OBJECT:MaximumValue) .
                ultraTrackBar1:MinValue = INTEGER (THIS-OBJECT:MinimumValue) .
                ultraNumericEditor1:Visible = FALSE . 
            END.            
        END.
        ELSE DO:
            IF THIS-OBJECT:PromptType = NumericPromptTypeEnum:TextBox THEN DO:
                ultraNumericEditor1:NumericType = Infragistics.Win.UltraWinEditors.NumericType:Double . 
                ultraNumericEditor1:Value = BOX (THIS-OBJECT:DecimalValue) .
                ultraNumericEditor1:MaxValue = BOX (THIS-OBJECT:MaximumValue) .
                ultraNumericEditor1:MinValue = BOX (THIS-OBJECT:MinimumValue) .
                ultraTrackBar1:Visible = FALSE . 
            END. 
            ELSE DO:
                ultraTrackBar1:Value = INTEGER (THIS-OBJECT:DecimalValue) .
                ultraTrackBar1:MaxValue = INTEGER (THIS-OBJECT:MaximumValue) .
                ultraTrackBar1:MinValue = INTEGER (THIS-OBJECT:MinimumValue) .
                ultraNumericEditor1:Visible = FALSE . 
            END.            
        END .
                                                                                          
		SUPER:OnLoad (e) .

        IF VALID-OBJECT (THIS-OBJECT:IntegerInputValidator) OR
           VALID-OBJECT (THIS-OBJECT:DecimalInputValidator) THEN DO:

            THIS-OBJECT:ultraNumericEditor1:ValueChanged:Subscribe (ValueChangedHandler) .

            THIS-OBJECT:ValueChangedHandler (THIS-OBJECT:ultraNumericEditor1,
                                             System.EventArgs:Empty) .

        END .
        
        THIS-OBJECT:MinimumSize = THIS-OBJECT:Size .

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the Shown event                                                                    
        Notes:                          
        @param e The System.EventArgs with the data for this event                                            
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID OnShown (e AS System.EventArgs):
        
        SUPER:OnShown (e) .
        
        THIS-OBJECT:ultraNumericEditor1:Focus () .
        THIS-OBJECT:ultraNumericEditor1:SelectAll () .
        
    END METHOD .

	/*------------------------------------------------------------------------------
		Purpose: Event handler for the ValueChanged event of the text editor  																	  
		Notes:   Uses the InputValidator instance to validate the value														
		@param sender The reference to the System.Object that raised this event
		@param e The System.EventArgs with the data for this event
	------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID ValueChangedHandler (sender AS System.Object,
	                                           e AS System.EventArgs):
		
		IF THIS-OBJECT:NumericType = NumericTypeEnum:Integer AND 
		   VALID-OBJECT (THIS-OBJECT:IntegerInputValidator) THEN DO: 
		    
		    IF THIS-OBJECT:PromptType = NumericPromptTypeEnum:TextBox THEN
		        THIS-OBJECT:buOk:Enabled = THIS-OBJECT:IntegerInputValidator:ValidateIntegerValue
		                                      (UNBOX (THIS-OBJECT:ultraNumericEditor1:Value)) .
            ELSE 		                                     
                THIS-OBJECT:buOk:Enabled = THIS-OBJECT:IntegerInputValidator:ValidateIntegerValue
                                              (THIS-OBJECT:ultraTrackBar1:Value) .
	    END . 
	    ELSE DO:
            IF THIS-OBJECT:NumericType = NumericTypeEnum:Decimal AND 
               VALID-OBJECT (THIS-OBJECT:DecimalInputValidator) THEN 
    
            IF THIS-OBJECT:PromptType = NumericPromptTypeEnum:TextBox THEN
                THIS-OBJECT:buOk:Enabled = THIS-OBJECT:DecimalInputValidator:ValidateDecimalValue
                                                  (UNBOX (THIS-OBJECT:ultraNumericEditor1:Value)) .
            ELSE                                             
                THIS-OBJECT:buOk:Enabled = THIS-OBJECT:DecimalInputValidator:ValidateDecimalValue
                                              (THIS-OBJECT:ultraTrackBar1:Value) .
        END.
        		
	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the NumericInputPromptForm class                                                               
        Notes:                                                          
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC NumericInputPromptForm ():

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
