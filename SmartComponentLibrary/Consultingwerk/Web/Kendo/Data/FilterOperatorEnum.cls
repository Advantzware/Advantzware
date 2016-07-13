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
    File        : FilterOperatorEnum
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jul 19 07:43:54 CEST 2015
    Notes       : The supported operators are: "eq" (equal to), "neq" (not equal to), 
                  "lt" (less than), "lte" (less than or equal to), 
                  "gt" (greater than), "gte" (greater than or equal to), 
                  "startswith", "endswith", "contains". The last three are 
                  supported only for string fields.
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
USING Consultingwerk.Web.Kendo.Data.*        FROM PROPATH .  
USING Progress.Lang.*                        FROM PROPATH .

{&CLASS} Consultingwerk.Web.Kendo.Data.FilterOperatorEnum 
    &IF "{&CLASS}" EQ "CLASS" &THEN
    INHERITS Enum
    &ENDIF    
        : 

&IF "{&CLASS}" EQ "ENUM" &THEN
    DEFINE ENUM
&ENDIF    

    {Consultingwerk/EnumMember.i eq 0 FilterOperatorEnum}
    {Consultingwerk/EnumMember.i neq 1 FilterOperatorEnum}
    {Consultingwerk/EnumMember.i lt 2 FilterOperatorEnum}
    {Consultingwerk/EnumMember.i lte 3 FilterOperatorEnum}
    {Consultingwerk/EnumMember.i gt 4 FilterOperatorEnum}
    {Consultingwerk/EnumMember.i gte 5 FilterOperatorEnum}
    {Consultingwerk/EnumMember.i startswith 6 FilterOperatorEnum}
    {Consultingwerk/EnumMember.i endswith 7 FilterOperatorEnum}
    {Consultingwerk/EnumMember.i contains 8 FilterOperatorEnum}

&IF "{&CLASS}" EQ "ENUM" &THEN
    .
&ENDIF    

&IF "{&CLASS}" EQ "CLASS" &THEN

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the FilterOperatorEnum members
        Notes:
        @param piValue The internal (numeric) representation of the Enumeration member
        @param pcLabel The text label of the Enumaration member
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE FilterOperatorEnum (piValue AS INTEGER, pcLabel AS CHARACTER):
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
	
    {Consultingwerk/EnumFromString.i Consultingwerk.Web.Kendo.Data.FilterOperatorEnum} 

&ENDIF

END {&CLASS} .
