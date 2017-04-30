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
    File        : ExtendedNameFormatEnum
    Purpose     : Specifies a format for a directory service object name.
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jun 18 21:51:52 CEST 2012
    Notes       : http://msdn.microsoft.com/en-us/library/windows/desktop/ms724268(v=vs.85).aspx
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

&IF DEFINED (NativeEnums) NE 0 &THEN
&GLOBAL-DEFINE CLASS ENUM
&ELSE
&GLOBAL-DEFINE CLASS CLASS
&ENDIF

USING Consultingwerk.*                       FROM PROPATH.
USING Consultingwerk.Framework.Collections.* FROM PROPATH.
USING Consultingwerk.Windows.API.*           FROM PROPATH.
USING Progress.Lang.*                        FROM PROPATH.

{&CLASS} Consultingwerk.Windows.API.ExtendedNameFormatEnum
    &IF "{&CLASS}" EQ "CLASS" &THEN
    INHERITS Enum
    &ENDIF
        :

&IF "{&CLASS}" EQ "ENUM" &THEN
    DEFINE ENUM
&ENDIF

    {Consultingwerk/EnumMember.i NameUnknown 0 ExtendedNameFormatEnum}
    {Consultingwerk/EnumMember.i NameFullyQualifiedDN 1 ExtendedNameFormatEnum}
    {Consultingwerk/EnumMember.i NameSamCompatible 2 ExtendedNameFormatEnum}
    {Consultingwerk/EnumMember.i NameDisplay 3 ExtendedNameFormatEnum}
    {Consultingwerk/EnumMember.i NameUniqueId 6 ExtendedNameFormatEnum}
    {Consultingwerk/EnumMember.i NameCanonical 7 ExtendedNameFormatEnum}
    {Consultingwerk/EnumMember.i NameUserPrincipal 8 ExtendedNameFormatEnum}
    {Consultingwerk/EnumMember.i NameCanonicalEx 9 ExtendedNameFormatEnum}
    {Consultingwerk/EnumMember.i NameServicePrincipal 10 ExtendedNameFormatEnum}
    {Consultingwerk/EnumMember.i NameDnsDomain 12  ExtendedNameFormatEnum}

&IF "{&CLASS}" EQ "ENUM" &THEN
    .
&ENDIF

&IF "{&CLASS}" EQ "CLASS" &THEN

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ExtendedNameFormatEnum members
        Notes:
        @param piValue The internal (numeric) representation of the Enumeration member
        @param pcLabel The text label of the Enumaration member
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE ExtendedNameFormatEnum (piValue AS INTEGER, pcLabel AS CHARACTER):
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

    {Consultingwerk/EnumFromString.i Consultingwerk.Windows.API.ExtendedNameFormatEnum}

&ENDIF

END {&CLASS} .
