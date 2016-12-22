/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : WidgetFacade
    Purpose     : Abstract base class for all Widget representations
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Sep 05 09:16:42 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Assertion.*                                       FROM PROPATH .
USING Consultingwerk.Windows.LegacyGuiMigration.Widgets.*              FROM PROPATH .  
USING Progress.Lang.*                                                  FROM PROPATH .
USING System.Windows.Forms.*                                           FROM ASSEMBLY .
USING Consultingwerk.Windows.LegacyGuiMigration.Widgets.Exceptions.* FROM PROPATH.

&scoped-define InfragisticsNS Consultingwerk.Windows.LegacyGuiMigration.Widgets.Infragistics
&scoped-define MicrosoftNS Consultingwerk.Windows.LegacyGuiMigration.Widgets.Microsoft

{Consultingwerk/products.i}

CLASS Consultingwerk.Windows.LegacyGuiMigration.Widgets.WidgetFacade
    IMPLEMENTS IWidgetFacade 
    USE-WIDGET-POOL 
    ABSTRACT: 
   
   DEFINE VARIABLE cFormat AS CHARACTER NO-UNDO .
   DEFINE VARIABLE lHidden AS LOGICAL   NO-UNDO .
   
    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the Control this instance represents
        Notes:
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY Control AS Control NO-UNDO 
	GET.
	PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: A character value that represents the data type of the field 
                 associated with the widget 
        Notes:   For example, the DATA-TYPE attribute of a slider widget always 
                 returns the value "INTEGER" because slider widgets can only 
                 represent integer values
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC ABSTRACT PROPERTY DATA-TYPE AS CHARACTER NO-UNDO  
    GET.
    
    /*------------------------------------------------------------------------------
        Purpose: The text format of a widget or browse-cell 
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY FORMAT AS CHARACTER NO-UNDO 
    GET:
        RETURN THIS-OBJECT:GetFORMAT () . 
    END GET . 
    SET (arg AS CHARACTER):
        THIS-OBJECT:SetFORMAT (arg) . 
    END SET .  
    
    /*------------------------------------------------------------------------------
        Purpose: Indicates whether to "hide" a widget
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY HIDDEN AS LOGICAL NO-UNDO 
    GET:
        RETURN THIS-OBJECT:GetHIDDEN () . 
    END GET . 
    SET (arg AS LOGICAL):
        THIS-OBJECT:SetHIDDEN (arg) . 
    END SET .  

    /*------------------------------------------------------------------------------
        Purpose: Indicates whether the value of the SCREEN-VALUE attribute for the 
                 widget has changed
        Notes:   For all widgets, the MODIFIED attribute is set to TRUE when the 
                 SCREEN-VALUE attribute for the widget is changed, whether or not 
                 the field is enabled. For all widgets except the editor widget, 
                 the MODIFIED attribute is set to TRUE if the SCREEN-VALUE attribute 
                 for the widget is changed using an ABL statement, such as assignment 
                 or DISPLAY. DISPLAY sets MODIFIED to TRUE only when the field is 
                 enabled. You can then reset the attribute to FALSE for each widget 
                 that can receive input focus or otherwise change value after it is 
                 initially displayed.
                 For editors, the successful execution of either the SAVE-FILE( ) or 
                 the READ-FILE( ) methods sets the MODIFIED attribute to FALSE.
                 For browses, if any browse cell changes, the AVM sets MODIFIED to 
                 TRUE. The application can reset MODIFIED to FALSE as necessary. If 
                 the query associated with a browse is reopened, the AVM resets MODIFIED 
                 to FALSE.
                 You can use the SENSITIVE attribute to reset the MODIFIED attribute. 
                 For example, when the screen-value of a fill-in field is changed, 
                 MODIFIED is set to TRUE. If you then set SENSITIVE to FALSE and then 
                 to TRUE, you enable the fill-in for further input and the MODIFIED 
                 attribute is reset to FALSE.
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY MODIFIED AS LOGICAL NO-UNDO INIT FALSE  
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: A string identifier for the specified object or widget
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY NAME AS CHARACTER NO-UNDO 
    GET:
        RETURN THIS-OBJECT:Control:Name .
    END GET . 
    SET (arg AS CHARACTER):
        ASSIGN THIS-OBJECT:Control:Name = arg . 
    END SET .  

    /*------------------------------------------------------------------------------
        Purpose: An arbitrary string associated with the handle of an object or widget
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY PRIVATE-DATA AS CHARACTER NO-UNDO 
    GET:
        RETURN STRING (UNBOX (THIS-OBJECT:Control:Tag)) .
    END GET . 
    SET (arg AS CHARACTER):
        ASSIGN THIS-OBJECT:Control:Tag = BOX (arg) . 
    END SET .  

    /*------------------------------------------------------------------------------
        Purpose: The data value in the screen buffer associated with the widget
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SCREEN-VALUE AS CHARACTER NO-UNDO 
    GET:
        RETURN THIS-OBJECT:GetSCREEN-VALUE () . 
    END GET . 
    SET (arg AS CHARACTER):
        THIS-OBJECT:SetSCREEN-VALUE (arg) . 
    END SET .  

    /*------------------------------------------------------------------------------
        Purpose: Indicates whether a widget can receive input focus or events
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SENSITIVE AS LOGICAL NO-UNDO 
    GET:
        RETURN THIS-OBJECT:GetSENSITIVE () . 
    END GET . 
    SET (arg AS LOGICAL):
        THIS-OBJECT:SetSENSITIVE (arg) . 
    END SET .  

    /*------------------------------------------------------------------------------
        Purpose: The type of a handle
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC ABSTRACT PROPERTY TYPE AS CHARACTER NO-UNDO  
    GET.

    /*------------------------------------------------------------------------------
        Purpose: Indicates whether a widget is currently visible on the display
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY VISIBLE AS LOGICAL NO-UNDO 
    GET:
        RETURN THIS-OBJECT:GetVISIBLE () . 
    END GET . 
    SET (arg AS LOGICAL):
        THIS-OBJECT:SetVISIBLE (arg) . 
    END SET .   

	/*------------------------------------------------------------------------------
	    Purpose: Constructor for the WidgetFacade class
	    Notes:
        @param poControl The reference to the UltraTextEditor this instance should represent	        
	------------------------------------------------------------------------------*/
	CONSTRUCTOR PROTECTED WidgetFacade (poControl AS System.Windows.Forms.Control):
		SUPER ().
		
        ObjectAssert:IsValid (poControl, "poControl":U) .
		
		THIS-OBJECT:Control = poControl .
		
		/* Subscribe to the Enabled Changed event, to reset MODIFIED */
		THIS-OBJECT:Control:EnabledChanged:Subscribe (EnabledChangedHandler) .
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the EnabledChanged event of the .NET Control 
        Notes:   Resets the MODIFIFED Attribute when Enabled = TRUE
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID EnabledChangedHandler (sender AS System.Object,
	                                           e AS System.EventArgs):
		
		/* When settings Enabled = TRUE, we reset the modified attribute */
		IF THIS-OBJECT:Control:Enabled = TRUE THEN 
		    THIS-OBJECT:MODIFIED = FALSE . 

	END METHOD .

	/*------------------------------------------------------------------------------
	    Purpose: Returns the IWidget reference for a given .NET Control
	    Notes:   Factory method. 
	             Throws UnsupportedControlTypeException when Controls can not be mapped
	             to an WidgetFacade instance 
	    @param poControl The .NET Control to return the Widget reference for
	    @return The reference to the IWidget representing the .NET Control
	------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC IWidgetFacade FromControl (poControl AS System.Windows.Forms.Control):
		
		DEFINE VARIABLE oWidget AS IWidgetFacade NO-UNDO . 
		
		ObjectAssert:IsValid (poControl, "poControl":U) .

        /* Control library specific properties */		
		IF NOT VALID-OBJECT (oWidget) AND poControl:GetType():FullName BEGINS "Infragistics.":U THEN 
		    oWidget = {&InfragisticsNS}.InfragisticsWidgetFactory:FromControl (poControl) .

        IF NOT VALID-OBJECT (oWidget) AND poControl:GetType():FullName BEGINS "System.Windows.Forms.":U THEN 
            oWidget = {&MicrosoftNS}.MicrosoftWidgetFactory:FromControl (poControl) .

        /* Error handling */
        IF NOT VALID-OBJECT (oWidget) THEN 
            UNDO, THROW NEW UnsupportedControlTypeException (SUBSTITUTE ("No IWidget factory for &1."{&TRAN},
                                                                         poControl:GetType():FullName),
                                                             0,
                                                             poControl) .
        RETURN oWidget . 
        
	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: GET method for the FORMAT property
        Notes:
        @return Character value indicating the FORMAT Value
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER GetFORMAT ():
        
        RETURN cFormat .
    
    END METHOD . 
    
	/*------------------------------------------------------------------------------
        Purpose: GET method for the HIDDEN property
        Notes:
        @return Logical value indicating the HIDDEN Value
	------------------------------------------------------------------------------*/
	METHOD PROTECTED LOGICAL GetHIDDEN ():
		
        RETURN lHidden .
        
	END METHOD.

	/*------------------------------------------------------------------------------
        Purpose: GET method for the SCREEN-VALUE property
        Notes:
        @return Character value indicating the SCREEN-VALUE Value
	------------------------------------------------------------------------------*/
	METHOD PROTECTED CHARACTER GetSCREEN-VALUE ():
	    
	    RETURN THIS-OBJECT:Control:Text .
	
	END METHOD . 

	/*------------------------------------------------------------------------------
	    Purpose: GET method for the SENSITIVE property
	    Notes:
	    @return Logical value indicating the SENSITIVE Value
	------------------------------------------------------------------------------*/
	METHOD PROTECTED LOGICAL GetSENSITIVE ():
		
		RETURN THIS-OBJECT:Control:Enabled .

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: GET method for the VISIBLE property
        Notes:
        @return Logical value indicating the VISIBLE Value
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED LOGICAL GetVISIBLE ():
    
        RETURN THIS-OBJECT:Control:Visible .    

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: SET method for the FORMAT property
        Notes:
        @param pcValue The new value for the the FORMAT property
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID SetFORMAT (pcValue AS CHARACTER):
        
        ASSIGN cFormat = pcValue .

    END METHOD.

	/*------------------------------------------------------------------------------
        Purpose: SET method for the HIDDEN property
        Notes:
        @param plValue The new value for the the HIDDEN property
	------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID SetHIDDEN (plValue AS LOGICAL):
		
        IF plValue = TRUE THEN 
            ASSIGN THIS-OBJECT:VISIBLE = FALSE . 
            
        ASSIGN lHidden = plValue .             
        
	END METHOD.

	/*------------------------------------------------------------------------------
        Purpose: SET method for the SCREEN-VALUE property
        Notes:
        @param pcValue The new value for the the SCREEN-VALUE property
	------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID SetSCREEN-VALUE (pcValue AS CHARACTER):
		
		ASSIGN THIS-OBJECT:Control:Text = pcValue .

	END METHOD.

	/*------------------------------------------------------------------------------
        Purpose: SET method for the SENSITIVE property
        Notes:
        @param plValue The new value for the the SENSITIVE property
	------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID SetSENSITIVE (plValue AS LOGICAL):
		
		ASSIGN THIS-OBJECT:Control:Enabled = plValue . 

	END METHOD.

	/*------------------------------------------------------------------------------
        Purpose: SET method for the VISIBLE property
        Notes:
        @param plValue The new value for the the VISIBLE property
	------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID SetVISIBLE (plValue AS LOGICAL):
		
        IF plValue = TRUE THEN 
            ASSIGN THIS-OBJECT:HIDDEN = FALSE .          
                       
        ASSIGN THIS-OBJECT:Control:Visible = plValue .  
            
	END METHOD.

END CLASS.
