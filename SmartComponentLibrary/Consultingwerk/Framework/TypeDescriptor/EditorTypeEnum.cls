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
    File        : EditorTypeEnum
    Purpose     : Enumeration of Editor types
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Apr 29 07:18:33 CEST 2015
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

&IF DEFINED (NativeEnums) NE 0 &THEN
&GLOBAL-DEFINE CLASS ENUM
&ELSE
&GLOBAL-DEFINE CLASS CLASS
&ENDIF

USING Consultingwerk.*                          FROM PROPATH .
USING Consultingwerk.Framework.Collections.*    FROM PROPATH .
USING Consultingwerk.Framework.TypeDescriptor.* FROM PROPATH .
USING Progress.Lang.*                           FROM PROPATH .

{&CLASS} Consultingwerk.Framework.TypeDescriptor.EditorTypeEnum
    &IF "{&CLASS}" EQ "CLASS" &THEN
    INHERITS Enum
    &ENDIF
        :

&IF "{&CLASS}" EQ "ENUM" &THEN
    DEFINE ENUM
&ENDIF

    {Consultingwerk/EnumMember.i Text 0 EditorTypeEnum}
    {Consultingwerk/EnumMember.i Checkbox 1 EditorTypeEnum}
    {Consultingwerk/EnumMember.i NumericInteger 2 EditorTypeEnum}
    {Consultingwerk/EnumMember.i NumericDecimal 3 EditorTypeEnum}
    {Consultingwerk/EnumMember.i Date 4 EditorTypeEnum}
    {Consultingwerk/EnumMember.i DateTime 4 EditorTypeEnum}
    {Consultingwerk/EnumMember.i List 4 EditorTypeEnum}

&IF "{&CLASS}" EQ "ENUM" &THEN
    .
&ENDIF

&IF "{&CLASS}" EQ "CLASS" &THEN

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the EditorTypeEnum members
        Notes:
        @param piValue The internal (numeric) representation of the Enumeration member
        @param pcLabel The text label of the Enumaration member
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE EditorTypeEnum (piValue AS INTEGER, pcLabel AS CHARACTER):
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

        RETURN NEW CharacterList ('{&EnumMembers}':U).

    END METHOD .

    {Consultingwerk/EnumFromString.i Consultingwerk.Framework.TypeDescriptor.EditorTypeEnum}

&ENDIF

END {&CLASS} .
