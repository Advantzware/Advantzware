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
    File        : NumericPromptTypeEnum
    Purpose     : Contains possible values for the types of the
                  NumericInputPrompt type used by the InputPromptHelper
                  class
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Feb 17 20:58:00 CET 2012
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                       FROM PROPATH .
USING Consultingwerk.Framework.Collections.* FROM PROPATH .
USING Consultingwerk.Framework.Enum.*        FROM PROPATH .
USING Progress.Lang.*                        FROM PROPATH .

&IF DEFINED (NativeEnums) NE 0 &THEN
ENUM Consultingwerk.Framework.Enum.NumericPromptTypeEnum:

    DEFINE ENUM TextBox Slider .

END ENUM .

&ELSE
CLASS Consultingwerk.Framework.Enum.NumericPromptTypeEnum
    INHERITS Consultingwerk.Enum:

    {Consultingwerk/EnumMember.i TextBox 0 NumericPromptTypeEnum}
    {Consultingwerk/EnumMember.i Slider 1 NumericPromptTypeEnum}

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the NumericPromptTypeEnum
        Notes:
        @param piValue The internal (numeric) representation of the Enumeration member
        @param pcLabel The text label of the Enumaration member
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE NumericPromptTypeEnum (piValue AS INTEGER, pcLabel AS CHARACTER):
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

    {Consultingwerk/EnumFromString.i Consultingwerk.Framework.Enum.NumericPromptTypeEnum}

END CLASS .
&ENDIF
