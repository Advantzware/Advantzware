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
    File        : StructureTypeEnum
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Thu Jan 10 00:01:29 CET 2013
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
USING Consultingwerk.Windows.Framework.*     FROM PROPATH .
USING Progress.Lang.*                        FROM PROPATH .

{&CLASS} Consultingwerk.Windows.Framework.StructureTypeEnum
    &IF "{&CLASS}" EQ "CLASS" &THEN
    INHERITS Enum
    &ENDIF
        :

&IF "{&CLASS}" EQ "ENUM" &THEN
    DEFINE ENUM
&ENDIF

    {Consultingwerk/EnumMember.i Menu 0 StructureTypeEnum}
    {Consultingwerk/EnumMember.i Item 1 StructureTypeEnum}

&IF "{&CLASS}" EQ "ENUM" &THEN
    .
&ENDIF

&IF "{&CLASS}" EQ "CLASS" &THEN

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the StructureTypeEnum members
        Notes:
        @param piValue The internal (numeric) representation of the Enumeration member
        @param pcLabel The text label of the Enumaration member
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE StructureTypeEnum (piValue AS INTEGER, pcLabel AS CHARACTER):
        SUPER ().

        ASSIGN THIS-OBJECT:Value = piValue
               THIS-OBJECT:Label = pcLabel .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Returns the StructureTypeEnum member with the given Name
        Notes:
        @param pcValue The CHARACTER Value to return the Enum member for
        @return The Enum member with the given Name
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC StructureTypeEnum FromString (pcValue AS CHARACTER):

        &IF NOT PROVERSION BEGINS "10.2":U &THEN
        RETURN DYNAMIC-PROPERTY ("Consultingwerk.Windows.Framework.StructureTypeEnum":U,
                                 pcValue) .
        &ELSE
        CASE pcValue:
            WHEN "Menu":U THEN RETURN StructureTypeEnum:Menu .
            WHEN "Item":U THEN RETURN StructureTypeEnum:Item .
            OTHERWISE
                UNDO, THROW NEW Consultingwerk.Exceptions.InvalidParameterValueException
                    ("pcValue":U,
                     pcValue,
                     "Consultingwerk.Windows.Framework.StructureTypeEnum":U) .
        END CASE .
        &ENDIF

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns a CharacterList with the names of the Enum members
        Notes:
        @return The CharacterList with the names of the Enum members
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CharacterList GetNames ():

        RETURN NEW CharacterList ('{&EnumMembers}':U).

    END METHOD .

&ENDIF

END {&CLASS} .
