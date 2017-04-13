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
    File        : FieldRestrictionEnum
    Purpose     : Enum with possible values for the field level security
                  on the user interface
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Nov 09 00:10:19 CET 2012
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                         FROM PROPATH .
USING Consultingwerk.Framework.Authorization.* FROM PROPATH .
USING Consultingwerk.Framework.Collections.*   FROM PROPATH .
USING Progress.Lang.*                          FROM PROPATH .

&IF DEFINED (NativeEnums) NE 0 &THEN
ENUM Consultingwerk.Framework.Authorization.FieldRestrictionEnum:

    DEFINE ENUM None Disabled Hidden AddOnly UpdateOnly .

END ENUM .
&ELSE
CLASS Consultingwerk.Framework.Authorization.FieldRestrictionEnum
    INHERITS Consultingwerk.Enum:

    /*------------------------------------------------------------------------------
        Purpose: The field has no restrictions
        Notes:
    ------------------------------------------------------------------------------*/
    {Consultingwerk/EnumMember.i None 0 FieldRestrictionEnum}

    /*------------------------------------------------------------------------------
        Purpose: The user may view the field but not edit it's value
        Notes:
    ------------------------------------------------------------------------------*/
    {Consultingwerk/EnumMember.i Disabled 1 FieldRestrictionEnum}

    /*------------------------------------------------------------------------------
        Purpose: The user has no privilegues to view the field
        Notes:
    ------------------------------------------------------------------------------*/
    {Consultingwerk/EnumMember.i Hidden 2 FieldRestrictionEnum}

    /*------------------------------------------------------------------------------
        Purpose: The user may view the field but only edit for new records
        Notes:
    ------------------------------------------------------------------------------*/
    {Consultingwerk/EnumMember.i AddOnly 3 FieldRestrictionEnum}

    /*------------------------------------------------------------------------------
        Purpose: The user may view the field but only edit for existing records
        Notes:
    ------------------------------------------------------------------------------*/
    {Consultingwerk/EnumMember.i UpdateOnly 4 FieldRestrictionEnum}

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the FieldRestrictionEnum members
        Notes:
        @param piValue The internal (numeric) representation of the Enumeration member
        @param pcLabel The text label of the Enumaration member
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE FieldRestrictionEnum (piValue AS INTEGER, pcLabel AS CHARACTER):
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

    {Consultingwerk/EnumFromString.i Consultingwerk.Framework.Authorization.FieldRestrictionEnum}

END CLASS .
&ENDIF
