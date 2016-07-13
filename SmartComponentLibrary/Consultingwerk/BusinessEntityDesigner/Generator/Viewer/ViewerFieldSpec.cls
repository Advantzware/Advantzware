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
    File        : ViewerFieldSpec
    Purpose     : Specifies properties of the SmartViewerField to be created
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd. 
    Created     : Tue Dec 03 06:06:30 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                                         FROM PROPATH . 
USING Consultingwerk.BusinessEntityDesigner.Generator.Viewer.* FROM PROPATH .  
USING Progress.Lang.*                                          FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Generator.Viewer.ViewerFieldSpec 
&IF "{&FrameworkSerializationType}" EQ "XML" OR PROVERSION BEGINS "10.2":U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF  
    :

    {Consultingwerk/JsonSerializableProperty.i AlternativeControlType CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i ColumnName CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i ControlName CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i DataType CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i FieldLabel CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i ListItems CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i ListItemPairs CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i NoLabel LOGICAL "INIT FALSE"} .
    {Consultingwerk/JsonSerializableProperty.i PrivateData CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i RadioButtons CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i ViewAs CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i WidgetType CHARACTER "INIT ""FILL-IN"":U"} .
    {Consultingwerk/JsonSerializableProperty.i WidgetValue CHARACTER} .
    
    {Consultingwerk/JsonSerializableProperty.i Left INTEGER} .
    {Consultingwerk/JsonSerializableProperty.i Top INTEGER} .
    {Consultingwerk/JsonSerializableProperty.i Width INTEGER} .
    {Consultingwerk/JsonSerializableProperty.i Height INTEGER} .

    /*------------------------------------------------------------------------------
        Purpose: Code block for the Click (CHOOSE) event
        Notes:   
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i ClickTriggerBlock LONGCHAR}

    /*------------------------------------------------------------------------------
        Purpose: Code block for the Validating (LEAVE) event
        Notes:   
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i ValidatingTriggerBlock LONGCHAR}
    
    /*------------------------------------------------------------------------------
        Purpose: Code block for the ValueChanged (VALUE-CHANGED) event
        Notes:   
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i ValueChangedTriggerBlock LONGCHAR}

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ViewerFieldSpec class
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC ViewerFieldSpec ():
		SUPER ().

        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ViewerFieldSpec class
        Notes:   
        @param pcColumnName The Colum Name Value 
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ViewerFieldSpec (pcColumnName AS CHARACTER):
        THIS-OBJECT ().
        
        THIS-OBJECT:ColumnName = pcColumnName .
        
    END CONSTRUCTOR.

END CLASS.
