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
    File        : FormStartTypeEnum
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Oct 12 02:13:28 CEST 2012
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

{&CLASS} Consultingwerk.Windows.Framework.FormStartTypeEnum
    &IF "{&CLASS}" EQ "CLASS" &THEN
    INHERITS Enum
    &ENDIF
        :

&IF "{&CLASS}" EQ "ENUM" &THEN
    DEFINE ENUM
&ENDIF

    {Consultingwerk/EnumMember.i Default 0 FormStartTypeEnum}
    {Consultingwerk/EnumMember.i MdiChild 1 FormStartTypeEnum}
    {Consultingwerk/EnumMember.i SeparateForm 2 FormStartTypeEnum}
    {Consultingwerk/EnumMember.i Dialog 3 FormStartTypeEnum}

&IF "{&CLASS}" EQ "ENUM" &THEN
    .
&ENDIF

&IF "{&CLASS}" EQ "CLASS" &THEN

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the FormStartTypeEnum members
        Notes:
        @param piValue The internal (numeric) representation of the Enumeration member
        @param pcLabel The text label of the Enumaration member
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE FormStartTypeEnum (piValue AS INTEGER, pcLabel AS CHARACTER):
        SUPER ().

        ASSIGN THIS-OBJECT:Value = piValue
               THIS-OBJECT:Label = pcLabel .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Returns the FormStartTypeEnum member with the given Name
        Notes:
        @param pcValue The CHARACTER Value to return the Enum member for
        @return The Enum member with the given Name
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC FormStartTypeEnum FromString (pcValue AS CHARACTER):

        &IF NOT PROVERSION BEGINS "10.2":U &THEN
        RETURN DYNAMIC-PROPERTY ("Consultingwerk.Windows.Framework.FormStartTypeEnum":U,
                                 pcValue) .
        &ELSE
        CASE pcValue:
            WHEN "Default":U THEN RETURN FormStartTypeEnum:Default .
            WHEN "MdiChild":U THEN RETURN FormStartTypeEnum:MdiChild .
            WHEN "SeparateForm":U THEN RETURN FormStartTypeEnum:SeparateForm .
            WHEN "Dialog":U THEN RETURN FormStartTypeEnum:Dialog .
            OTHERWISE
                UNDO, THROW NEW Consultingwerk.Exceptions.InvalidParameterValueException
                    ("pcValue":U,
                     pcValue,
                     "Consultingwerk.Windows.Framework.FormStartTypeEnum":U) .
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
