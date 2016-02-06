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
    File        : CanFindParameter
    Purpose     : Parameter object to the BusinessEntity:CanFind method 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Nov 11 07:01:55 CET 2015
    Notes       : Find String starting with "WHERE Buffer.Field = value" 
                  or "Buffer.Field = value"
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*      FROM PROPATH .
USING Consultingwerk.OERA.* FROM PROPATH .  
USING Progress.Lang.*       FROM PROPATH .

CLASS Consultingwerk.OERA.CanFindParameter 
&IF '{&FrameworkSerializationType}' EQ 'XML' OR PROVERSION BEGINS '10.2':U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF
    {&SERIALIZABLE}:

    {Consultingwerk/JsonSerializableProperty.i FindString CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i Result LOGICAL "INIT FALSE"} .
    {Consultingwerk/JsonSerializableProperty.i TableName CHARACTER} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the CanFindParameter class
        Notes:   
        @param pcFindString The value for the FindString
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC CanFindParameter (pcFindString AS CHARACTER):
		THIS-OBJECT ().

        ASSIGN THIS-OBJECT:FindString = pcFindString . 
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the CanFindParameter class
        Notes:   Starting with "WHERE Buffer.Field = value" or "Buffer.Field = value"
        @param pcFindString The value for the FindString
        @param pcTableName The value for the TableName
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC CanFindParameter (pcFindString AS CHARACTER,
                                         pcTableName AS CHARACTER):
        THIS-OBJECT ().
        
        ASSIGN THIS-OBJECT:FindString = pcFindString 
               THIS-OBJECT:TableName  = pcTableName . 
               
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the CanFindParameter class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC CanFindParameter ():
        SUPER ().
        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .

    END CONSTRUCTOR. 

END CLASS.
