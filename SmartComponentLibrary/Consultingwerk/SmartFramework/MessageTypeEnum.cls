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
    File        : MessageTypeEnum
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jan 09 22:48:22 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

&IF DEFINED (NativeEnums) NE 0 &THEN
&GLOBAL-DEFINE CLASS ENUM
&ELSE
&GLOBAL-DEFINE CLASS CLASS
&ENDIF

USING Consultingwerk.*                       FROM PROPATH .
USING Consultingwerk.Framework.Collections.* FROM PROPATH .
USING Consultingwerk.SmartFramework.*        FROM PROPATH .  
USING Progress.Lang.*                        FROM PROPATH .

{&CLASS} Consultingwerk.SmartFramework.MessageTypeEnum 
    &IF "{&CLASS}" EQ "CLASS" &THEN
    INHERITS Enum
    &ENDIF    
        : 

&IF "{&CLASS}" EQ "ENUM" &THEN
    DEFINE ENUM
&ENDIF    

    {Consultingwerk/EnumMember.i Information 0 MessageTypeEnum}
    {Consultingwerk/EnumMember.i Warning 1 MessageTypeEnum}
    {Consultingwerk/EnumMember.i Error 2 MessageTypeEnum}
    {Consultingwerk/EnumMember.i Question 3 MessageTypeEnum}

&IF "{&CLASS}" EQ "ENUM" &THEN
    .
&ENDIF    

&IF "{&CLASS}" EQ "CLASS" &THEN

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the MessageTypeEnum members
        Notes:
        @param piValue The internal (numeric) representation of the Enumeration member
        @param pcLabel The text label of the Enumaration member
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE MessageTypeEnum (piValue AS INTEGER, pcLabel AS CHARACTER):
        SUPER ().
 
        ASSIGN THIS-OBJECT:Value = piValue
               THIS-OBJECT:Label = pcLabel .
 
    END CONSTRUCTOR.
 
    /*------------------------------------------------------------------------------
        Purpose: Returns the MessageTypeEnum value for the provided Character value
        Notes:   
        @param pcValue The charcater value to return the MessageTypeEnum for
        @return 
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC MessageTypeEnum FromString (pcValue AS CHARACTER):
		
		IF pcValue BEGINS "I":U THEN 
		    RETURN MessageTypeEnum:Information .
        IF pcValue BEGINS "W":U THEN 
            RETURN MessageTypeEnum:Warning .
        IF pcValue BEGINS "E":U THEN 
            RETURN MessageTypeEnum:Error .
        IF pcValue BEGINS "Q":U THEN 
            RETURN MessageTypeEnum:Question .
		     

        UNDO, THROW NEW Consultingwerk.Exceptions.InvalidValueException (pcValue, 
                                                                         "pcValue":U) . 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns a CharacterList with the names of the Enum members
        Notes:   
        @return The CharacterList with the names of the Enum members
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CharacterList GetNames ():
 
        DEFINE VARIABLE oReturn AS CharacterList NO-UNDO . 
 
        RETURN NEW CharacterList ('{&EnumMembers}':U).
 
    END METHOD .
	
&ENDIF 

END {&CLASS} .
