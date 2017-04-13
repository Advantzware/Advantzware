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
    File        : LogEntryTypesEnum
    Purpose     : Enum of ABL Log Entry Types
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Mar 14 09:52:12 CET 2013
    Notes       : Entries starting with "4" start with Four
                  . in the entry name is replaced with _
                  * in the entry name is replaced with #
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
USING Consultingwerk.Util.Enum.*             FROM PROPATH .
USING Progress.Lang.*                        FROM PROPATH .

{&CLASS} Consultingwerk.Util.Enum.LogEntryTypesEnum
    &IF "{&CLASS}" EQ "CLASS" &THEN
    INHERITS Enum
    &ENDIF
        :

&IF "{&CLASS}" EQ "ENUM" &THEN
    DEFINE ENUM
&ENDIF

    {Consultingwerk/EnumMember.i FourGLMessages 0 LogEntryTypesEnum}
    {Consultingwerk/EnumMember.i FourGLTrace 1 LogEntryTypesEnum}
    {Consultingwerk/EnumMember.i FourGLTrans 2 LogEntryTypesEnum}
    {Consultingwerk/EnumMember.i Actional 3 LogEntryTypesEnum}
    {Consultingwerk/EnumMember.i ASDefault 4 LogEntryTypesEnum}
    {Consultingwerk/EnumMember.i ASPlumbing 5 LogEntryTypesEnum}
    {Consultingwerk/EnumMember.i ASK 6 LogEntryTypesEnum}
    {Consultingwerk/EnumMember.i DB_# 7 LogEntryTypesEnum}
    {Consultingwerk/EnumMember.i DB_Connects 8 LogEntryTypesEnum}
    {Consultingwerk/EnumMember.i DB_Encryption 9 LogEntryTypesEnum}
    {Consultingwerk/EnumMember.i DS_# 10 LogEntryTypesEnum}
    {Consultingwerk/EnumMember.i DS_Cursor 11 LogEntryTypesEnum}
    {Consultingwerk/EnumMember.i DS_QryInfo 12 LogEntryTypesEnum}
    {Consultingwerk/EnumMember.i DS_Performance 13 LogEntryTypesEnum}
    {Consultingwerk/EnumMember.i DynObjects_# 14 LogEntryTypesEnum}
    {Consultingwerk/EnumMember.i DynObjects_Class 15 LogEntryTypesEnum}
    {Consultingwerk/EnumMember.i DynObjects_DB 16 LogEntryTypesEnum}
    {Consultingwerk/EnumMember.i DynObjects_Other 17 LogEntryTypesEnum}
    {Consultingwerk/EnumMember.i DynObjects_UI 18 LogEntryTypesEnum}
    {Consultingwerk/EnumMember.i DynObjects_XML 19 LogEntryTypesEnum}
    {Consultingwerk/EnumMember.i FileID 20 LogEntryTypesEnum}
    {Consultingwerk/EnumMember.i IgnoredOps 21 LogEntryTypesEnum}
    {Consultingwerk/EnumMember.i MSASAdmin 22 LogEntryTypesEnum}
    {Consultingwerk/EnumMember.i MSASSessions 23 LogEntryTypesEnum}
    {Consultingwerk/EnumMember.i MSASSignals 24 LogEntryTypesEnum}
    {Consultingwerk/EnumMember.i MSASSockets 25 LogEntryTypesEnum}
    {Consultingwerk/EnumMember.i ProEvents_# 26 LogEntryTypesEnum}
    {Consultingwerk/EnumMember.i ProEvents_UI_# 27 LogEntryTypesEnum}
    {Consultingwerk/EnumMember.i ProEvents_UI_Char 28 LogEntryTypesEnum}
    {Consultingwerk/EnumMember.i ProEvents_UI_Command 29 LogEntryTypesEnum}
    {Consultingwerk/EnumMember.i ProEvents_Other 30 LogEntryTypesEnum}
    {Consultingwerk/EnumMember.i QryInfo 31 LogEntryTypesEnum}
    {Consultingwerk/EnumMember.i SAX 32 LogEntryTypesEnum}
    {Consultingwerk/EnumMember.i Temp-Tables 33 LogEntryTypesEnum}
    {Consultingwerk/EnumMember.i TTStats 34 LogEntryTypesEnum}
    {Consultingwerk/EnumMember.i WSO 35 LogEntryTypesEnum}

&IF "{&CLASS}" EQ "ENUM" &THEN
    .
&ENDIF

&IF "{&CLASS}" EQ "CLASS" &THEN

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the LogEntryTypesEnum members
        Notes:
        @param piValue The internal (numeric) representation of the Enumeration member
        @param pcLabel The text label of the Enumaration member
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE LogEntryTypesEnum (piValue AS INTEGER, pcLabel AS CHARACTER):
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

    {Consultingwerk/EnumFromString.i Consultingwerk.Util.Enum.LogEntryTypesEnum}

&ENDIF

END {&CLASS} .
