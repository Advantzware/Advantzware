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
    File        : MonthEnum
    Purpose     : Enumeration of the Months of a year
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Jun 06 13:59:57 CEST 2015
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
USING Consultingwerk.*                       FROM PROPATH .  
USING Progress.Lang.*                        FROM PROPATH .

{&CLASS} Consultingwerk.MonthEnum 
    &IF "{&CLASS}" EQ "CLASS" &THEN
    INHERITS Enum
    &ENDIF    
        : 

&IF "{&CLASS}" EQ "ENUM" &THEN
    DEFINE ENUM
&ENDIF    

    {Consultingwerk/EnumMember.i January 1 MonthEnum}
    {Consultingwerk/EnumMember.i February 2 MonthEnum}
    {Consultingwerk/EnumMember.i March 3 MonthEnum}
    {Consultingwerk/EnumMember.i April 4 MonthEnum}
    {Consultingwerk/EnumMember.i May 5 MonthEnum}
    {Consultingwerk/EnumMember.i June 6 MonthEnum}
    {Consultingwerk/EnumMember.i July 7 MonthEnum}
    {Consultingwerk/EnumMember.i August 8 MonthEnum}
    {Consultingwerk/EnumMember.i September 9 MonthEnum}
    {Consultingwerk/EnumMember.i October 10 MonthEnum}
    {Consultingwerk/EnumMember.i November 11 MonthEnum}
    {Consultingwerk/EnumMember.i December 12 MonthEnum}

&IF "{&CLASS}" EQ "ENUM" &THEN
    .
&ENDIF    

&IF "{&CLASS}" EQ "CLASS" &THEN

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the MonthEnum members
        Notes:
        @param piValue The internal (numeric) representation of the Enumeration member
        @param pcLabel The text label of the Enumaration member
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE MonthEnum (piValue AS INTEGER, pcLabel AS CHARACTER):
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
	
    {Consultingwerk/EnumFromString.i Consultingwerk.MonthEnum} 

&ENDIF 

END {&CLASS} .
