/**********************************************************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
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
    File        : ValidationMessageSeverity
    Purpose     : Enum of values for the Severity property of the ValidationMessage class
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Nov 27 09:40:16 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                         FROM PROPATH .
USING Consultingwerk.Framework.Collections.*   FROM PROPATH .
USING Consultingwerk.OERA.ValidationMessages.* FROM PROPATH .
USING Progress.Lang.*                          FROM PROPATH .

{Consultingwerk/products.i}

&IF DEFINED (NativeEnums) NE 0 &THEN
ENUM Consultingwerk.OERA.ValidationMessages.ValidationMessageSeverity:

    DEFINE ENUM
            /**
             * Purpose: Information Message
             */
            Info

            /**
             * Purpose: Warning Message
             */
            Warning

            /**
             * Purpose: Error Message (Default in ValidationMessage class)
             */
            Error .

END ENUM .

&ELSE
CLASS Consultingwerk.OERA.ValidationMessages.ValidationMessageSeverity
    INHERITS Enum:

    {Consultingwerk/EnumMember.i Info 1 ValidationMessageSeverity}
    {Consultingwerk/EnumMember.i Warning 2 ValidationMessageSeverity}
    {Consultingwerk/EnumMember.i Error 3 ValidationMessageSeverity}

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ValidationMessageSeverity members
        Notes:
        @param piValue The internal (numeric) representation of the Enumeration member
        @param pcLabel The text label of the Enumaration member
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE ValidationMessageSeverity (piValue AS INTEGER, pcLabel AS CHARACTER):
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

    {Consultingwerk/EnumFromString.i Consultingwerk.OERA.ValidationMessages.ValidationMessageSeverity}

END CLASS.
&ENDIF