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
    File        : UltraOptionSetWidget
    Purpose     : Represents a radio-set
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Sep 05 09:22:12 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Windows.LegacyGuiMigration.Widgets.*              FROM PROPATH .
USING Consultingwerk.Windows.LegacyGuiMigration.Widgets.Exceptions.*   FROM PROPATH .
USING Consultingwerk.Windows.LegacyGuiMigration.Widgets.Infragistics.* FROM PROPATH .  
USING Infragistics.Win.*                                               FROM ASSEMBLY .
USING Infragistics.Win.UltraWinEditors.*                               FROM ASSEMBLY .
USING Progress.Lang.*                                                  FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.Windows.LegacyGuiMigration.Widgets.Infragistics.UltraOptionSetWidget 
    INHERITS InfragisticsControlWidget
    IMPLEMENTS ICharacterInputValueWidget: 

    DEFINE VARIABLE oControl      AS UltraOptionSet NO-UNDO . 
    DEFINE VARIABLE cDelimiter    AS CHARACTER      NO-UNDO .
    DEFINE VARIABLE cRadioButtons AS CHARACTER      NO-UNDO .
    
    /*------------------------------------------------------------------------------
        Purpose: A character value that represents the data type of the field 
                 associated with the widget 
        Notes:   For example, the DATA-TYPE attribute of a slider widget always 
                 returns the value "INTEGER" because slider widgets can only 
                 represent integer values
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC OVERRIDE PROPERTY DATA-TYPE AS CHARACTER NO-UNDO INIT "CHARACTER":U  
    GET.
    PROTECTED SET.
    
    /*------------------------------------------------------------------------------
        Purpose: The character that separates values input to or output from a combo 
                 box or selection list 
        Notes:   
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY DELIMITER AS CHARACTER NO-UNDO INIT ",":U
    GET:
        RETURN cDelimiter .
    END GET.
    SET (arg AS CHARACTER):
        ASSIGN cDelimiter = arg .
    END SET.
                      
    /*------------------------------------------------------------------------------
        Purpose: Used for data-representation widgets, such as field-level widgets 
                 that represent variables or database fields
        Notes:   The value for the INPUT-VALUE attribute is the unformatted 
                 SCREEN-VALUE of a widget                          
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY INPUT-VALUE AS CHARACTER NO-UNDO 
    GET:
        RETURN UNBOX (oControl:Value) . 
    END GET .
    SET (arg AS CHARACTER):
        ASSIGN oControl:Value = BOX (arg) . 
    END SET . 
                      
    /*------------------------------------------------------------------------------
        Purpose: The label and value associated with each radio button in a radio set 
        Notes:   You can set this attribute to a comma-separated list containing the 
                 label/value pairs associated with each button. Each label and each 
                 value should be followed by a comma, as in 
                 "label1,value1,label2,value2,...labeln,valuen".
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY RADIO-BUTTONS AS CHARACTER NO-UNDO 
    GET():
        RETURN cRadioButtons .
    END.
    SET(arg AS CHARACTER):
        DEFINE VARIABLE i      AS INTEGER       NO-UNDO.
        DEFINE VARIABLE cLabel AS CHARACTER     NO-UNDO.
        DEFINE VARIABLE cValue AS CHARACTER     NO-UNDO.
        DEFINE VARIABLE itm    AS ValueListItem NO-UNDO . 
        
        cRadioButtons = arg .
        
        oControl:Items:Clear() .
        
        DO i = 1 TO NUM-ENTRIES (arg, THIS-OBJECT:Delimiter) BY 2:
            ASSIGN cLabel = ENTRY(i, arg, THIS-OBJECT:Delimiter)
                   cValue = ENTRY(i + 1, arg, THIS-OBJECT:Delimiter) .
                    
            itm = NEW ValueListItem () .
            itm:DataValue = BOX(cValue).
            itm:DisplayText = cLabel.
            itm:Tag = cValue .
            
            oControl:Items:Add(itm).                                                              
        END.
    END.
        
	/*------------------------------------------------------------------------------
        Purpose: The type of a handle
	    Notes:   RADIO-SET
	------------------------------------------------------------------------------*/
	DEFINE PUBLIC OVERRIDE PROPERTY TYPE AS CHARACTER NO-UNDO INIT "RADIO-SET":U 
	GET .
	PROTECTED SET . 

	/*------------------------------------------------------------------------------
	   Purpose: Constructor for the UltraOptionSetWidget class
	   Notes:
	   @param poEditor The reference to the UltraOptionSet this instance should represent
	------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC UltraOptionSetWidget (poEditor AS UltraOptionSet):
		SUPER (poEditor).
		
		ASSIGN oControl = poEditor . 
		
		{Consultingwerk/foreach.i ValueListItem oItem in oControl:Items}
		    IF cRadioButtons > "":U THEN 
		        ASSIGN cRadioButtons = cRadioButtons + THIS-OBJECT:DELIMITER.
		
		    ASSIGN cRadioButtons = cRadioButtons + 
		                           SUBSTITUTE ("&1&2&3":U,
		                                       oItem:DisplayText,
		                                       THIS-OBJECT:DELIMITER,
		                                       STRING (UNBOX (oItem:DataValue))) .  
		END.
	
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: GET method for the SCREEN-VALUE property
        Notes:
        @return Character value indicating the SCREEN-VALUE Value
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE CHARACTER GetSCREEN-VALUE ():
        
        IF oControl:CheckedIndex >= 0 THEN 
            RETURN ENTRY((oControl:CheckedIndex + 1) * 2, cRadioButtons, THIS-OBJECT:Delimiter) .
        
        RETURN ?.
    
    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: SET method for the SCREEN-VALUE property
        Notes:
        @param pcValue The new value for the the SCREEN-VALUE property
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID SetSCREEN-VALUE (pcValue AS CHARACTER):
        
        DEFINE VARIABLE i AS INTEGER    NO-UNDO.
        
        DO i = 0 TO (NUM-ENTRIES(cRadioButtons, THIS-OBJECT:Delimiter) / 2) - 1 :
            IF ENTRY((i + 1) * 2, cRadioButtons, THIS-OBJECT:Delimiter) = pcValue THEN DO:
                oControl:CheckedIndex = i .

                RETURN . 
            END.
        END.        
        
        UNDO, THROW NEW UnsupportedWidgetAttributeValueException (SUBSTITUTE ("Invalid value &1 for SCREEN-VALUE of RADIO-SET."{&TRAN},
                                                                              pcValue),
                                                                  0,
                                                                  oControl, 
                                                                  "SCREEN-VALUE":U) .       
    END METHOD.

END CLASS.
