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
    File        : NumericTypeEnum
    Purpose     : Contains possible values for the type of a numeric entry
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Feb 17 20:58:00 CET 2012
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
USING Consultingwerk.Framework.Enum.*        FROM PROPATH .
USING Progress.Lang.*                        FROM PROPATH .

{&CLASS} Consultingwerk.Framework.Enum.NumericTypeEnum
    &IF "{&CLASS}" EQ "CLASS" &THEN
    INHERITS Enum
    &ENDIF    
        : 

&IF "{&CLASS}" EQ "ENUM" &THEN
    DEFINE ENUM
&ENDIF    

    {Consultingwerk/EnumMember.i Integer 0 NumericTypeEnum}     
    {Consultingwerk/EnumMember.i Decimal 1 NumericTypeEnum}

&IF "{&CLASS}" EQ "ENUM" &THEN
    .
&ENDIF    

&IF "{&CLASS}" EQ "CLASS" &THEN

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the NumericTypeEnum                                                                     
        Notes:           
        @param piValue The internal (numeric) representation of the Enumeration member
        @param pcLabel The text label of the Enumaration member
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE NumericTypeEnum (piValue AS INTEGER, pcLabel AS CHARACTER):
        SUPER ().
        
        ASSIGN THIS-OBJECT:Value = piValue
               THIS-OBJECT:Label = pcLabel . 
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Returns a CharacterList with the names of the Enum members 
        Notes:   
        @return The CharacterList with the names of the Enum members
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CharacterList GetNames ():
        
        DEFINE VARIABLE oReturn AS CharacterList NO-UNDO . 
        
        RETURN NEW CharacterList ('{&EnumMembers}':U).

    END METHOD .
    
    {Consultingwerk/EnumFromString.i Consultingwerk.Framework.Enum.NumericTypeEnum}    
    
&ENDIF    
    
END {&CLASS} .
