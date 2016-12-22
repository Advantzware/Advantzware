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
    File        : UltraComboEditorWidget
    Purpose     : Represents a toggle-box
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
USING Infragistics.Win.UltraWinEditors.*                               FROM ASSEMBLY .
USING Progress.Lang.*                                                  FROM PROPATH .
USING Infragistics.Win.* FROM ASSEMBLY.
USING Progress.Util.* FROM ASSEMBLY.

{Consultingwerk/products.i}

CLASS Consultingwerk.Windows.LegacyGuiMigration.Widgets.Infragistics.UltraComboEditorWidget 
    INHERITS InfragisticsControlWidget
    IMPLEMENTS ICharacterInputValueWidget: 

    DEFINE VARIABLE oControl   AS UltraComboEditor NO-UNDO .    
    DEFINE VARIABLE cDelimiter AS CHARACTER        NO-UNDO . 

    /*------------------------------------------------------------------------------
        Purpose: Specifies that the combo-box widget automatically complete keyboard 
                 input based on a potential match to items in the drop-down list  
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY AUTO-COMPLETION AS LOGICAL NO-UNDO
    GET:
        RETURN oControl:AutoComplete .
    END.
    SET (arg AS LOGICAL):
        oControl:AutoComplete = arg . 
    END. 

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
        Purpose: A list of the label-value pairs associated with a combo box or selection 
                 list. The list is delimiter-separated 
        Notes:   
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY LIST-ITEM-PAIRS AS CHARACTER NO-UNDO 
    GET:
        /* Return Label, Value, Label, Value */
        DEFINE VARIABLE i      AS INTEGER    NO-UNDO.
        DEFINE VARIABLE cRet   AS CHARACTER  NO-UNDO.
        DEFINE VARIABLE cLabel AS CHARACTER  NO-UNDO.
        DEFINE VARIABLE cValue AS CHARACTER  NO-UNDO.   
                
        DO i = 1 TO THIS-OBJECT:NUM-ITEMS:
            ASSIGN cLabel = oControl:Items[i - 1]:DisplayText
                   cValue = oControl:Items[i - 1]:DataValue . 
                
            IF cLabel = ? THEN cLabel = "?":U.
            IF cValue = ? THEN cValue = "?":U.
            
            cRet = cRet + (IF cRet > "":U THEN THIS-OBJECT:DELIMITER ELSE "":U) 
                        + cLabel + THIS-OBJECT:DELIMITER + cValue .               
        END.
        
        RETURN cRet.
    END GET.
    SET(arg AS CHARACTER):              
        DEFINE VARIABLE i AS INTEGER    NO-UNDO.
        
        oControl:Items:Clear() . 
        
        DO i = 1 TO NUM-ENTRIES(arg, THIS-OBJECT:DELIMITER) BY 2:
            ADD-LAST(ENTRY(i, arg, THIS-OBJECT:DELIMITER),
                     ENTRY(i + 1, arg, THIS-OBJECT:DELIMITER)) .     
        END.        
        
    END SET.
    
    /*------------------------------------------------------------------------------
        Purpose: A list of the items associated with a combo box or selection list. The list is delimiter-separated. 
        Notes:   
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY LIST-ITEMS AS CHARACTER NO-UNDO 
    GET():
        /* Return Label, Value, Label, Value */
        DEFINE VARIABLE i      AS INTEGER    NO-UNDO.
        DEFINE VARIABLE cRet   AS CHARACTER  NO-UNDO.
        DEFINE VARIABLE cLabel AS CHARACTER  NO-UNDO.
        DEFINE VARIABLE cValue AS CHARACTER  NO-UNDO.   
                
        DO i = 1 TO THIS-OBJECT:NUM-ITEMS:
            ASSIGN cValue = oControl:Items[i - 1]:DataValue . 
                
            IF cValue = ? THEN cValue = "?":U.
            
            cRet = cRet + (IF cRet > "":U THEN THIS-OBJECT:DELIMITER ELSE "":U) + cValue .               
        END.
        
        RETURN cRet.
    END GET.
    SET(arg AS CHARACTER):
        DEFINE VARIABLE i AS INTEGER    NO-UNDO.
        
        oControl:Items:Clear() . 
        
        DO i = 1 TO NUM-ENTRIES(arg, THIS-OBJECT:DELIMITER):
            ADD-LAST(ENTRY(i, arg, THIS-OBJECT:DELIMITER),
                     ENTRY(i, arg, THIS-OBJECT:DELIMITER)) .     
        END.        
        
    END.
    
    /*------------------------------------------------------------------------------
        Purpose: The number of entries in a combo box or selection list
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY NUM-ITEMS AS INTEGER NO-UNDO 
    GET():
        RETURN oControl:Items:Count .       
    END.

    /*------------------------------------------------------------------------------
        Purpose: Indicates whether to sort new additions to the item list of a widget
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SORT AS LOGICAL NO-UNDO 
    GET:
        EnumHelper:AreEqual (oControl:SortStyle,
                             ValueListSortStyle:Ascending) . 
    END.
    SET(arg AS LOGICAL):
        IF arg THEN 
            oControl:SortStyle = ValueListSortStyle:Ascending .
        ELSE 
            oControl:SortStyle = ValueListSortStyle:None .
    END.   
    
    /*------------------------------------------------------------------------------
        Purpose: The subtype of a widget
        Notes:   For combo boxes, the value of this attribute is either "SIMPLE", 
                 "DROP-DOWN", or "DROP-DOWN-LIST"
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SUBTYPE AS CHARACTER NO-UNDO 
    GET():
        IF EnumHelper:AreEqual (oControl:DropDownStyle, DropDownStyle:DropDownList) 
            THEN RETURN "DROP-DOWN-LIST":U .
        ELSE IF EnumHelper:AreEqual (oControl:DropDownStyle, DropDownStyle:DropDown) 
            THEN RETURN "DROP-DOWN":U .
        ELSE RETURN ? . 
    END.
    SET (arg AS CHARACTER):
        CASE arg:
            WHEN "DROP-DOWN-LIST":U THEN 
                oControl:DropDownStyle = DropDownStyle:DropDownList .
            WHEN "DROP-DOWN":U THEN 
                 oControl:DropDownStyle = DropDownStyle:DropDown.
            OTHERWISE 
                UNDO, THROW NEW UnsupportedWidgetAttributeValueException
                                    ("The VALUE for SUBTYPE of a COMBO-BOX is not supported."{&TRAN},
                                     0,
                                     oControl,
                                     "SUBTYPE":U) .
         END CASE.
    END.
        
	/*------------------------------------------------------------------------------
        Purpose: The type of a handle
	    Notes:   COMBO-BOX
	------------------------------------------------------------------------------*/
	DEFINE PUBLIC OVERRIDE PROPERTY TYPE AS CHARACTER NO-UNDO INIT "COMBO-BOX":U 
	GET .
	PROTECTED SET . 

	/*------------------------------------------------------------------------------
	   Purpose: Constructor for the UltraComboEditorWidget class
	   Notes:
	   @param poEditor The reference to the UltraComboEditor this instance should represent
	------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC UltraComboEditorWidget (poEditor AS UltraComboEditor):
		SUPER (poEditor).
		
		ASSIGN oControl = poEditor . 
	
	END CONSTRUCTOR.

	/*------------------------------------------------------------------------------
       Purpose: Adds one or more items to the bottom of a combo box, radio set, or 
                selection list
       Notes:
       @param pcValue The value to add to the combo-box
       @return Logical value indicating the success of the method
	------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL ADD-LAST (pcValue AS CHARACTE ):
        RETURN ADD-LAST (pcValue, pcValue) .
    END.
    
    /*------------------------------------------------------------------------------
       Purpose: Adds one or more items to the bottom of a combo box, radio set, or 
                selection list
       Notes:
       @param pcLabel A character-string expression that represents the label of a label-value pair
       @param pcValue The value the AVM assigns to the field or variable if the user selects the corresponding label
       @return Logical value indicating the success of the method
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL ADD-LAST(pcLabel AS CHARACTER, pcValue AS CHARACTER):
        
        DEFINE VARIABLE oNewItem AS Infragistics.Win.ValueListItem NO-UNDO . 
        DEFINE VARIABLE cValue   AS CHARACTER                      NO-UNDO .
        
        /* Ensure propert data type for combo-box item */
        DO ON ERROR UNDO, THROW :
            CASE DATA-TYPE:
                WHEN "CHARACTER":U THEN ASSIGN cValue = pcValue . 
                WHEN "DECIMAL":U   THEN ASSIGN cValue = STRING(DECIMAL(pcValue)) .
                WHEN "INTEGER":U   THEN ASSIGN cValue = STRING(INTEGER(pcValue)) .
                WHEN "LOGICAL":U   THEN ASSIGN cValue = STRING(LOGICAL(pcValue)) .
                WHEN "DATE":U      THEN ASSIGN cValue = STRING(DATE(pcValue)) .
                OTHERWISE DO:
                    UNDO, THROW NEW UnsupportedWidgetAttributeValueException
                                        (SUBSTITUTE ("The DATA-TYPE &1 of the COMBO-BOX Widget is not supported for ADD-LAST."{&TRAN},
                                                     THIS-OBJECT:DATA-TYPE),
                                         0,
                                         oControl,
                                         "FORMAT":U) .
                END.                
            END.
            
            
            oNewItem = NEW Infragistics.Win.ValueListItem () .
            
            oNewItem:DataValue = BOX(cValue) . 
            oNewItem:DisplayText = pcLabel .
            
            oControl:Items:Add(oNewItem) .       
            
            CATCH e AS Progress.Lang.SysError :
                /* ** Invalid character in numeric input <character>. (76) */
                /* ** Invalid date input. (85) */
                /* ** Input value: <input> should be <yes/no>. (87) */
                               
                IF CAN-DO("76,85,87":U, STRING(e:GetMessageNum(1))) THEN
                    UNDO, THROW NEW UnsupportedWidgetAttributeValueException
                                        ("Invalid data-type for ADD-LAST":U,
                                         0,
                                         oControl,
                                         "FORMAT":U) .
                ELSE 
                    UNDO, THROW e.                                        
                    
            END CATCH.
        END.
        
        RETURN TRUE. 
        
    END METHOD .     

	/*------------------------------------------------------------------------------
	    Purpose: Returns the character-string value of the specified list entry.
	    Notes:   Trailing _ as otherwise the ENTRY function would be overloaded within 
	             the class itself
	    @param piListIndex An integer expression that specifies an entry within the combo-box list or selection list
	    @return The ENTRY at the specified list index
	------------------------------------------------------------------------------*/
	METHOD PUBLIC CHARACTER ENTRY_ (piListIndex AS INTEGER):

        RETURN STRING(oControl:Items[piListIndex - 1]:DataValue) .

        CATCH ex AS Progress.Lang.Error:
            RETURN "":U .
        END.

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: GET method for the SCREEN-VALUE property
        Notes:
        @return Character value indicating the SCREEN-VALUE Value
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE CHARACTER GetSCREEN-VALUE ():
        
        IF VALID-OBJECT (oControl:SelectedItem) THEN 
            RETURN STRING(oControl:SelectedItem:DataValue) . 
    
        RETURN ? . 
    
    END METHOD . 

	/*------------------------------------------------------------------------------
        Purpose: Returns the index of the specified item in a combo-box list or selection list.
        Notes:   Trailing _ as otherwise the LOOKUP function would be overloaded within 
                 the class itself
                 If list-string has the Unknown value (?), LOOKUP returns the Unknown value (?).
                 If list-string is not in the list, LOOKUP returns 0. 
        @param pcListString A character-string expression that specifies a single value in the combo box or selection list
        @return The list index of the specified string or 0
	------------------------------------------------------------------------------*/
    METHOD PUBLIC INTEGER LOOKUP_ (pcListString AS CHARACTER):

        DEFINE VARIABLE i AS INTEGER    NO-UNDO.
        
        IF pcListString = ? THEN 
            RETURN ? .
        
        DO i = 1 TO NUM-ITEMS:
            IF STRING(oControl:Items[i - 1]:DataValue) = pcListString
                THEN RETURN i .
        END.
        
        RETURN 0 .
        
        CATCH ex AS Progress.Lang.Error:
            RETURN 0 .
        END.

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: SET method for the SCREEN-VALUE property
        Notes:
        @param pcValue The new value for the the SCREEN-VALUE property
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID SetSCREEN-VALUE (pcValue AS CHARACTER):
        
        DEFINE VARIABLE lFound    AS LOGICAL       NO-UNDO INIT FALSE .

        {Consultingwerk/foreach.i ValueListItem oItem in oControl:Items}
        
            IF STRING(oItem:DataValue) = pcValue THEN DO:
                ASSIGN oControl:SelectedItem = oItem .
                RETURN .  
            END.  
        END. 
        
        UNDO, THROW NEW UnsupportedWidgetAttributeValueException (SUBSTITUTE ("Invalid value &1 for SCREEN-VALUE of COMBO-BOX."{&TRAN},
                                                                              pcValue),
                                                                  0,
                                                                  oControl, 
                                                                  "SCREEN-VALUE":U) .       
    END METHOD.

END CLASS.
