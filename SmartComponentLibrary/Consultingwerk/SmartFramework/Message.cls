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
    File        : Message
    Purpose     : Structure describing a message 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jan 09 21:51:56 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.* FROM PROPATH .  
USING Consultingwerk.SmartFramework.* FROM PROPATH .  
USING Progress.Lang.*                 FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Message 
&IF "{&FrameworkSerializationType}" EQ "XML" OR PROVERSION BEGINS "10.2":U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF    
	IMPLEMENTS IMessage:

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Message Detail
        Notes:   
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i Detail CHARACTER} .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Message Text
        Notes:   
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i Text CHARACTER} .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Type of the Message as an MessageTypeEnum member
        Notes:   Information, Warning, Error, Question   
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i Type Consultingwerk.SmartFramework.MessageTypeEnum} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the Message class
        Notes:   
        @param pcText The message Text
        @param pcDetail The message detail
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC Message (pcText AS CHARACTER, 
                                pcDetail AS CHARACTER):
        THIS-OBJECT () .
        
        ASSIGN THIS-OBJECT:Text   = pcText 
               THIS-OBJECT:Detail = pcDetail .
        
    END CONSTRUCTOR .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the Message class
        Notes:   
        @param pcText The message Text
        @param pcDetail The message detail
        @param poType The message type
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC Message (pcText AS CHARACTER, 
                                pcDetail AS CHARACTER, 
                                poType AS MessageTypeEnum):
        THIS-OBJECT () .
        
        ASSIGN THIS-OBJECT:Text   = pcText 
               THIS-OBJECT:Detail = pcDetail
               THIS-OBJECT:Type   = poType .
        
    END CONSTRUCTOR .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the Message class
        Notes:   
        @param pcText The message Text
        @param poType The message type
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC Message (pcText AS CHARACTER, 
                                poType AS MessageTypeEnum):
        THIS-OBJECT () .
        
        ASSIGN THIS-OBJECT:Text = pcText 
               THIS-OBJECT:Type = poType .
        
    END CONSTRUCTOR .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the Message class
        Notes:   
        @param pcText The message Text
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC Message (pcText AS CHARACTER):
        THIS-OBJECT () .
        
        ASSIGN THIS-OBJECT:Text = pcText .
        
    END CONSTRUCTOR .
        
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the Message class
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC Message ():
		SUPER ().

        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .

	END CONSTRUCTOR.

END CLASS.
