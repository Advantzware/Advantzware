/**********************************************************************
 * Copyright (C) 2006-2012 by Consultingwerk Ltd. ("CW") -            *
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
    File        : PropertySheetControl
    Purpose     : Dynamic property sheet for ABL objects implementing the 
                  Consultingwerk.Framework.TypeDescriptor.IHasTypeDescriptor 
                  interface 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Apr 29 18:57:59 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.TypeDescriptor.* FROM PROPATH .
USING Consultingwerk.Windows.Controls.*         FROM PROPATH .
USING Progress.Windows.*                        FROM ASSEMBLY .
USING Progress.Lang.*                           FROM PROPATH .
USING Consultingwerk.Util.* FROM PROPATH.
USING Infragistics.Win.Misc.* FROM ASSEMBLY.
USING Infragistics.Win.UltraWinEditors.* FROM ASSEMBLY.
USING System.Windows.Forms.* FROM ASSEMBLY.
USING Progress.Util.* FROM ASSEMBLY.
USING Infragistics.Win.* FROM ASSEMBLY.

CLASS Consultingwerk.Windows.Controls.PropertySheetControl 
    INHERITS UserControl: 
	
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
		
    DEFINE VARIABLE oTypeDescriptor AS ITypeDescriptor NO-UNDO . 

    DEFINE VARIABLE lDisplaying AS LOGICAL NO-UNDO INIT FALSE .
		
    /*------------------------------------------------------------------------------
        Purpose: Raised when a property value has changed
        Notes:   
        @param sender The object that raised the PropertyChanged event
        @param e The PropertyChangedEventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT PropertyChanged SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                        e AS PropertyChangedEventArgs).
    
	/*------------------------------------------------------------------------------
	    Purpose: Event handler for the Disposed event of the user control
	    Notes:
	    @param sender The reference to the object that raised the event
	    @param e The System.EventArgs with the data for this event
	------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID DisposedHandler (sender AS System.Object,
	                                       e AS System.EventArgs):
		
		THIS-OBJECT:Clear () .

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the PropertyChanged
        Notes:   
        @param e The PropertyChangedEventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnPropertyChanged (e AS PropertyChangedEventArgs):
    		
        Consultingwerk.Assertion.EventArgsAssert:IsValid (e, "PropertyChanged":U) .
     
        THIS-OBJECT:PropertyChanged:Publish (THIS-OBJECT, e) .
    
    END METHOD .		
		
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the reference to the object shown in the property sheet  
        Notes:
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY ObjectReference AS IHasTypeDescriptor NO-UNDO 
	GET.
	SET (arg AS IHasTypeDescriptor):
	    THIS-OBJECT:ObjectReference = arg . 
	    
	    oTypeDescriptor = arg:GetTypeDescriptor() .
	    
	    THIS-OBJECT:RenderProperties (oTypeDescriptor) .
	    
	    THIS-OBJECT:RefreshPropertySheet () .
	END. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the PropertySheetControl class  
        Notes:
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC PropertySheetControl ():
		
        SUPER().

        InitializeComponent().

        &IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN
        THIS-OBJECT:ComponentsCollection:Add (THIS-OBJECT:components).
        &ENDIF

        THIS-OBJECT:Disposed:Subscribe (DisposedHandler) .

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

	END CONSTRUCTOR.

	/*------------------------------------------------------------------------------
	    Purpose: Assigns the object properties
	    Notes:
	------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID Assign ():
		
        DEFINE VARIABLE oControl AS System.Windows.Forms.Control NO-UNDO .
        
        {Consultingwerk/linqABL.i IPropertyDescriptor oProperty in oTypeDescriptor:PropertyDescriptors
                                  where Visible = true and Editable = true}

            oControl = THIS-OBJECT:Controls[oProperty:PropertyName] .
    
            CASE oProperty:EditorType:
                WHEN EditorTypeEnum:Checkbox THEN DO:
                    DYNAMIC-PROPERTY (THIS-OBJECT:ObjectReference, oProperty:PropertyName) = CAST (oControl, UltraCheckEditor):Checked .  
                END.
                WHEN EditorTypeEnum:Date THEN DO:
                    DYNAMIC-PROPERTY (THIS-OBJECT:ObjectReference, oProperty:PropertyName) = UNBOX (CAST (oControl, UltraDateTimeEditor):Value) .  
                END.
                WHEN EditorTypeEnum:DateTime THEN DO:
                    DYNAMIC-PROPERTY (THIS-OBJECT:ObjectReference, oProperty:PropertyName) = UNBOX (CAST (oControl, UltraDateTimeEditor):Value) .  
                END.
                WHEN EditorTypeEnum:List THEN DO:
                    DYNAMIC-PROPERTY (THIS-OBJECT:ObjectReference, oProperty:PropertyName) = UNBOX (CAST (oControl, UltraComboEditor):Value) .  
                END.
                WHEN EditorTypeEnum:NumericInteger THEN DO:
                    DYNAMIC-PROPERTY (THIS-OBJECT:ObjectReference, oProperty:PropertyName) = UNBOX (CAST (oControl, UltraNumericEditor):Value) .  
                END.
                WHEN EditorTypeEnum:NumericDecimal THEN DO:
                    DYNAMIC-PROPERTY (THIS-OBJECT:ObjectReference, oProperty:PropertyName) = UNBOX (CAST (oControl, UltraNumericEditor):Value) .  
                END.
                OTHERWISE 
                    DYNAMIC-PROPERTY (THIS-OBJECT:ObjectReference, oProperty:PropertyName) = UNBOX (CAST (oControl, UltraTextEditor):Value) .  
    
            END CASE .     
        END.

	END METHOD.

	/*------------------------------------------------------------------------------
	    Purpose: Clears the Control
	    Notes:
	------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID Clear ():
		
        DO ON ERROR UNDO, THROW:
            THIS-OBJECT:Controls:Remove (THIS-OBJECT:ultraLabel1) .
            THIS-OBJECT:Controls:Clear () . 
            THIS-OBJECT:Controls:Add (THIS-OBJECT:ultraLabel1) .
        END .
        
	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Creates a editor control
        Notes:
        @param poProperty The Property to create a label for
        @param piTop The Top Coordinate for the Control
        @return The reference to the generated label control 
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED System.Windows.Forms.Control CreateControl (poProperty AS IPropertyDescriptor,
                                                                 piTop AS INTEGER):
		
        DEFINE VARIABLE oEditor        AS System.Windows.Forms.Control NO-UNDO .
                                       
        DEFINE VARIABLE oCheckEditor   AS UltraCheckEditor             NO-UNDO . 
        DEFINE VARIABLE oDateEditor    AS UltraDateTimeEditor          NO-UNDO . 
        DEFINE VARIABLE oComboEditor   AS UltraComboEditor             NO-UNDO . 
        DEFINE VARIABLE oNumericEditor AS UltraNumericEditor           NO-UNDO .

        CASE poProperty:EditorType:
            WHEN EditorTypeEnum:Checkbox THEN DO: 
                oCheckEditor = NEW UltraCheckEditor (). 
                oEditor = oCheckEditor .
                
                oCheckEditor:CheckedChanged:Subscribe (TextChangedHandler) .
            END.
            WHEN EditorTypeEnum:Date THEN DO:
                oDateEditor = NEW UltraDateTimeEditor () . 
                oEditor = oDateEditor . 
                
                IF SESSION:DATE-FORMAT = "mdy":U THEN  
                    oDateEditor:MaskInput = "~{LOC~}mm/dd/yyyy":U .
                ELSE 
                    oDateEditor:MaskInput = "~{LOC~}dd/mm/yyyy":U .

                oDateEditor:ValueChanged:Subscribe (TextChangedHandler) .
            END.
            WHEN EditorTypeEnum:DateTime THEN DO:
                oDateEditor = NEW UltraDateTimeEditor () . 
                oEditor = oDateEditor . 
                IF SESSION:DATE-FORMAT = "mdy":U THEN  
                    oDateEditor:MaskInput = "~{LOC~}mm/dd/yyyy hh:mm:ss":U .
                ELSE 
                    oDateEditor:MaskInput = "~{LOC~}dd/mm/yyyy hh:mm:ss":U .
                    
                oDateEditor:ValueChanged:Subscribe (TextChangedHandler) .
            END.
            WHEN EditorTypeEnum:List THEN DO:
                oComboEditor = NEW UltraComboEditor () . 
                oEditor = oComboEditor.
                oComboEditor:ValueList = ValueListHelper:FromListItemPairs (poProperty:ListItemPairs).
                oComboEditor:LimitToList = TRUE . 
                oComboEditor:DropDownStyle = DropDownStyle:DropDownList . 
                
                oComboEditor:ValueChanged:Subscribe (TextChangedHandler) .
            END.
            WHEN EditorTypeEnum:NumericInteger THEN DO:
                oNumericEditor = NEW UltraNumericEditor () .
                oEditor = oNumericEditor .
                oNumericEditor:NumericType = NumericType:Integer .
                
                oNumericEditor:ValueChanged:Subscribe (TextChangedHandler) . 
            END.
            WHEN EditorTypeEnum:NumericDecimal THEN DO:
                oNumericEditor = NEW UltraNumericEditor () .
                oEditor = oNumericEditor .
                oNumericEditor:NumericType = NumericType:Decimal . 
                oNumericEditor:Value = BOX (0) . 
                oNumericEditor:MaskInput = "~{LOC~}-nnnnnnnnnn.nn":U .
                
                oNumericEditor:ValueChanged:Subscribe (TextChangedHandler) . 
            END.
            OTHERWISE DO:
                oEditor = NEW UltraTextEditor () .
                oEditor:TextChanged:Subscribe (TextChangedHandler) .
            END. 
        END CASE . 

        oEditor:Width = THIS-OBJECT:ClientSize:Width - 135 .
        oEditor:Anchor = CAST (EnumHelperExt:Or (AnchorStyles:Top, AnchorStyles:Left, AnchorStyles:Right), AnchorStyles).
        oEditor:Location = NEW System.Drawing.Point (130, piTop) .
        oEditor:TabIndex = THIS-OBJECT:Controls:Count + 1.
        oEditor:Name = poProperty:PropertyName .

        oEditor:Enabled = poProperty:Editable .  

        THIS-OBJECT:Controls:Add (oEditor) .

        RETURN oEditor . 

	END METHOD.

	/*------------------------------------------------------------------------------
	    Purpose: Creates a label control
	    Notes:
	    @param poProperty The Property to create a label for
	    @param piTop The Top Coordinate for the Control
	    @return The reference to the generated label control 
	------------------------------------------------------------------------------*/
	METHOD PROTECTED UltraLabel CreateLabel (poProperty AS IPropertyDescriptor,
	                                         piTop AS INTEGER):
		
        DEFINE VARIABLE oLabel AS UltraLabel NO-UNDO . 
            
        oLabel = NEW UltraLabel () .
        oLabel:AutoSize = TRUE .
        oLabel:Location = NEW System.Drawing.Point (3, piTop + 4) .
        oLabel:TabIndex = THIS-OBJECT:Controls:Count + 1.
        
        IF poProperty:PropertyLabel > "":U THEN 
            oLabel:Text = poProperty:PropertyLabel .
        ELSE 
            oLabel:Text = poProperty:PropertyName .

        THIS-OBJECT:Controls:Add (oLabel) .

        RETURN oLabel . 

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Initializes the visual control  
        Notes:
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID InitializeComponent ():
		
        /* NOTE: The following method is automatically generated.
        
        We strongly suggest that the contents of this method only be modified using the
        Visual Designer to avoid any incompatible modifications.
        
        Modifying the contents of this method using a code editor will invalidate any support for this file. */
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
        appearance3 = NEW Infragistics.Win.Appearance().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraLabel1 */
        /*  */
        appearance3:FontData:BoldAsString = "True":U.
        THIS-OBJECT:ultraLabel1:Appearance = appearance3.
        THIS-OBJECT:ultraLabel1:Dock = System.Windows.Forms.DockStyle:Top.
        THIS-OBJECT:ultraLabel1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1":U.
        THIS-OBJECT:ultraLabel1:Padding = NEW System.Drawing.Size(3, 3).
        THIS-OBJECT:ultraLabel1:Size = NEW System.Drawing.Size(300, 23).
        THIS-OBJECT:ultraLabel1:TabIndex = 0.
        THIS-OBJECT:ultraLabel1:Text = "Selected Object":U.
        /*  */
        /* PropertySheetControl */
        /*  */
        THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
        THIS-OBJECT:AutoScroll = TRUE.
        THIS-OBJECT:AutoSizeMode = System.Windows.Forms.AutoSizeMode:GrowAndShrink.
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel1).
        THIS-OBJECT:MinimumSize = NEW System.Drawing.Size(200, 50).
        THIS-OBJECT:Name = "PropertySheetControl":U.
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
	END METHOD.

	/*------------------------------------------------------------------------------
	    Purpose: Refreshes the field values shown in the property sheet with the 
	             current values from the object instance
	    Notes:
	------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID RefreshPropertySheet ():
		
		DEFINE VARIABLE oControl AS System.Windows.Forms.Control NO-UNDO .
		
		ASSIGN lDisplaying = TRUE . 
		
        {Consultingwerk/linqABL.i IPropertyDescriptor oProperty in oTypeDescriptor:PropertyDescriptors
                                  where Visible = true}

            oControl = THIS-OBJECT:Controls[oProperty:PropertyName] .
    
            CASE oProperty:EditorType:
                WHEN EditorTypeEnum:Checkbox THEN DO:
                    CAST (oControl, UltraCheckEditor):Checked = DYNAMIC-PROPERTY (THIS-OBJECT:ObjectReference, oProperty:PropertyName) .  
                END.
                WHEN EditorTypeEnum:Date THEN DO:
                    CAST (oControl, UltraDateTimeEditor):Value = DYNAMIC-PROPERTY (THIS-OBJECT:ObjectReference, oProperty:PropertyName) .  
                END.
                WHEN EditorTypeEnum:DateTime THEN DO:
                    CAST (oControl, UltraDateTimeEditor):Value = DYNAMIC-PROPERTY (THIS-OBJECT:ObjectReference, oProperty:PropertyName) .  
                END.
                WHEN EditorTypeEnum:List THEN DO:
                    CAST (oControl, UltraComboEditor):Value = DYNAMIC-PROPERTY (THIS-OBJECT:ObjectReference, oProperty:PropertyName) .  
                END.
                WHEN EditorTypeEnum:NumericInteger THEN DO:
                    CAST (oControl, UltraNumericEditor):Value = DYNAMIC-PROPERTY (THIS-OBJECT:ObjectReference, oProperty:PropertyName) .  
                END.
                WHEN EditorTypeEnum:NumericDecimal THEN DO:
                    CAST (oControl, UltraNumericEditor):Value = DYNAMIC-PROPERTY (THIS-OBJECT:ObjectReference, oProperty:PropertyName) .  
                END.
                OTHERWISE 
                    CAST (oControl, UltraTextEditor):Value = DYNAMIC-PROPERTY (THIS-OBJECT:ObjectReference, oProperty:PropertyName) .  
    
            END CASE .     
        END.

        FINALLY:
            ASSIGN lDisplaying = FALSE . 		
        END FINALLY.

	END METHOD.

	/*------------------------------------------------------------------------------
	    Purpose: Renders the properties on the user control 
	    Notes:
	    @param poTypeDescriptor The reference to the type descriptor to render properties from
	------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID RenderProperties (poTypeDescriptor AS ITypeDescriptor):
		
        DEFINE VARIABLE iTop           AS INTEGER NO-UNDO INIT 25.		
        DEFINE VARIABLE iMaxWidthLabel AS INTEGER NO-UNDO INIT 0.
		
		DO ON ERROR UNDO, THROW:
		    THIS-OBJECT:Controls:Remove (THIS-OBJECT:ultraLabel1) .
		    THIS-OBJECT:Controls:Clear () . 
            THIS-OBJECT:Controls:Add (THIS-OBJECT:ultraLabel1) .
		END .
		
		IF poTypeDescriptor:Description > "":U THEN 
		    THIS-OBJECT:ultraLabel1:Text = poTypeDescriptor:Description .
		ELSE  
	        THIS-OBJECT:ultraLabel1:Text = StringHelper:WellFormFromCamelCase (ENTRY (NUM-ENTRIES (poTypeDescriptor:TypeName, ".":U), 
	                                                                           poTypeDescriptor:TypeName, 
	                                                                           ".":U)) .

        {Consultingwerk/linqABL.i IPropertyDescriptor oProperty in poTypeDescriptor:PropertyDescriptors
                                  where DefiningTypeName = poTypeDescriptor:TypeName
                                    and Visible = true}

            ASSIGN iMaxWidthLabel = MAXIMUM (THIS-OBJECT:CreateLabel (oProperty, iTop):Width,
                                             iMaxWidthLabel) .
            THIS-OBJECT:CreateControl (oProperty, iTop) .

            ASSIGN iTop = iTop + 26 . 
        END.

        {Consultingwerk/linqABL.i IPropertyDescriptor oProperty2 in poTypeDescriptor:PropertyDescriptors
                                  where DefiningTypeName <> poTypeDescriptor:TypeName
                                    and Visible = true}

            ASSIGN iMaxWidthLabel = MAXIMUM (THIS-OBJECT:CreateLabel (oProperty2, iTop):Width,
                                             iMaxWidthLabel) .

            THIS-OBJECT:CreateControl (oProperty2, iTop) .

            ASSIGN iTop = iTop + 26 . 
        END.

	END METHOD.

	/*------------------------------------------------------------------------------
	    Purpose: Event handler for the TextChanged event of the dynamically created
	             Controls
	    Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
	------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID TextChangedHandler (sender AS System.Object,
	                                          e AS System.EventArgs):
		
        IF lDisplaying THEN 
            RETURN . 		


        THIS-OBJECT:OnPropertyChanged (NEW PropertyChangedEventArgs (CAST (sender, System.Windows.Forms.Control):Name)) .

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the PropertySheetControl class  
        Notes:
    ------------------------------------------------------------------------------*/
	DESTRUCTOR PUBLIC PropertySheetControl ():

	END DESTRUCTOR.

END CLASS.
