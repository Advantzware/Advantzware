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
    File        : ColumnDisplayTypeEnum
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Sebastian Düngel / Consultingwerk Ltd.
    Created     : Tue Mar 26 02:13:08 CET 2013
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
USING Consultingwerk.Web.Widgets.Grid.*      FROM PROPATH .
USING Progress.Lang.*                        FROM PROPATH .

{&CLASS} Consultingwerk.Web.Widgets.Grid.ColumnDisplayTypeEnum
    &IF "{&CLASS}" EQ "CLASS" &THEN
    INHERITS Enum
    &ENDIF
        :

&IF "{&CLASS}" EQ "ENUM" &THEN
    DEFINE ENUM
&ENDIF

    {Consultingwerk/EnumMember.i Text 0 ColumnDisplayTypeEnum}
    {Consultingwerk/EnumMember.i Image 1 ColumnDisplayTypeEnum}
    {Consultingwerk/EnumMember.i ImageAndText 2 ColumnDisplayTypeEnum}

&IF "{&CLASS}" EQ "ENUM" &THEN
    .
&ENDIF

&IF "{&CLASS}" EQ "CLASS" &THEN

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ColumnDisplayTypeEnum members
        Notes:
        @param piValue The internal (numeric) representation of the Enumeration member
        @param pcLabel The text label of the Enumaration member
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE ColumnDisplayTypeEnum (piValue AS INTEGER, pcLabel AS CHARACTER):
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

    {Consultingwerk/EnumFromString.i Consultingwerk.Web.Widgets.Grid.ColumnDisplayTypeEnum}

&ENDIF

END {&CLASS} .
