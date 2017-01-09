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
    File        : OperatorEnum
    Purpose     : Enumeration of Operators
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Oct 10 11:58:06 CEST 2012
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
USING Consultingwerk.Exceptions.*            FROM PROPATH . 
USING Consultingwerk.Framework.Collections.* FROM PROPATH .
USING Progress.Lang.*                        FROM PROPATH .

{&CLASS} Consultingwerk.OperatorEnum 
    &IF "{&CLASS}" EQ "CLASS" &THEN
    INHERITS Enum
    &ENDIF    
        : 

&IF "{&CLASS}" EQ "ENUM" &THEN
    DEFINE ENUM
&ENDIF    

    {Consultingwerk/EnumMember.i EQ 0 OperatorEnum}
    {Consultingwerk/EnumMember.i Begins 1 OperatorEnum}
    {Consultingwerk/EnumMember.i Contains 2 OperatorEnum}
    {Consultingwerk/EnumMember.i Matches 3 OperatorEnum}
    {Consultingwerk/EnumMember.i GE 4 OperatorEnum}
    {Consultingwerk/EnumMember.i GT 5 OperatorEnum}
    {Consultingwerk/EnumMember.i LE 6 OperatorEnum}
    {Consultingwerk/EnumMember.i LT 7 OperatorEnum}
    {Consultingwerk/EnumMember.i NE 8 OperatorEnum}

    {Consultingwerk/EnumMember.i And 10 OperatorEnum}
    {Consultingwerk/EnumMember.i Or 11 OperatorEnum}

&IF "{&CLASS}" EQ "ENUM" &THEN
    .
&ENDIF    

&IF "{&CLASS}" EQ "CLASS" &THEN

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the OperatorEnum members
        Notes:
        @param piValue The internal (numeric) representation of the Enumeration member
        @param pcLabel The text label of the Enumaration member
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE OperatorEnum (piValue AS INTEGER, pcLabel AS CHARACTER):
        SUPER ().
 
        ASSIGN THIS-OBJECT:Value = piValue
               THIS-OBJECT:Label = pcLabel .
 
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Returns the Enum member with the given name
        Notes:   Supports the Enum member names as well as =, <>, >=, >, <= and < 
                 Method is duplicated on as Consultingwerk.QueryParser:OperatorFromString
                 as this method here is not supported anymore from OpenEdge 11.6 on
                 when using the native Enums   
        @param pcName The Name of the Enum member
        @return The Enum member with the given name
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC OperatorEnum FromString (pcName AS CHARACTER):
		
		RETURN QueryParser:OperatorFromString (pcName) .
		
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the Character expression of the Enum
        Notes:   
        @return The Character representation of the Enum
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC CHARACTER ToString ():
        
        RETURN UPPER (SUPER:ToString()) .

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
