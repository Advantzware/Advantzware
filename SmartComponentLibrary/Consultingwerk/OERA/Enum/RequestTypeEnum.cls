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
    File        : RequestTypeEnum
    Purpose     : Enum with possible values for the RequestType property
                  of the BusinessEntity class
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Aug 25 09:40:48 CEST 2010
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
USING Consultingwerk.OERA.Enum.*             FROM PROPATH .
USING Progress.Lang.*                        FROM PROPATH .

{&CLASS} Consultingwerk.OERA.Enum.RequestTypeEnum
    &IF "{&CLASS}" EQ "CLASS" &THEN
    INHERITS Enum
    &ENDIF
        :

&IF "{&CLASS}" EQ "ENUM" &THEN
    DEFINE ENUM
&ENDIF

    /*------------------------------------------------------------------------------
        Purpose: The Business Entity is performing a FetchData request
        Notes:
    ------------------------------------------------------------------------------*/
    {Consultingwerk/EnumMember.i FetchData 1 RequestTypeEnum}

    /*------------------------------------------------------------------------------
        Purpose: The Business Entity is performing a SaveChanges request
        Notes:
    ------------------------------------------------------------------------------*/
    {Consultingwerk/EnumMember.i SaveChanges 2 RequestTypeEnum}

&IF "{&CLASS}" EQ "ENUM" &THEN
    .
&ENDIF

&IF "{&CLASS}" EQ "CLASS" &THEN

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the RequestTypeEnum class
        Notes:
        @param piValue The integer enum value
        @param pcLabel The charcter enum label
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE RequestTypeEnum (piValue AS INTEGER, pcLabel AS CHARACTER):
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

    {Consultingwerk/EnumFromString.i Consultingwerk.OERA.Enum.RequestTypeEnum}

&ENDIF

END {&CLASS} .
