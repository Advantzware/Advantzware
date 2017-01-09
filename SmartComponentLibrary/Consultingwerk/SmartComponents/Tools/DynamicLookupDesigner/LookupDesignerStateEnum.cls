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
    File        : LookupDesignerStateEnum
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk
    Created     : Wed Oct 26 11:30:00 CEST 2011
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

&IF DEFINED (NativeEnums) NE 0 &THEN
&GLOBAL-DEFINE CLASS ENUM
&ELSE
&GLOBAL-DEFINE CLASS CLASS
&ENDIF

USING Consultingwerk.*                                             FROM PROPATH .
USING Consultingwerk.Framework.Collections.*                       FROM PROPATH .
USING Consultingwerk.SmartComponents.Tools.DynamicLookupDesigner.* FROM PROPATH .
USING Progress.Lang.*                                              FROM PROPATH .

{&CLASS} Consultingwerk.SmartComponents.Tools.DynamicLookupDesigner.LookupDesignerStateEnum
    &IF "{&CLASS}" EQ "CLASS" &THEN
    INHERITS Enum
    &ENDIF
        :

&IF "{&CLASS}" EQ "ENUM" &THEN
    DEFINE ENUM
&ENDIF

    /* Static members */
    {Consultingwerk/EnumMember.i Default 1 LookupDesignerStateEnum}
    {Consultingwerk/EnumMember.i New 2 LookupDesignerStateEnum}
    {Consultingwerk/EnumMember.i Opening 3 LookupDesignerStateEnum}
    {Consultingwerk/EnumMember.i Editing 4 LookupDesignerStateEnum}

&IF "{&CLASS}" EQ "ENUM" &THEN
    .
&ENDIF

&IF "{&CLASS}" EQ "CLASS" &THEN

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the LookupDesignerStateEnum class
        Notes:
        @param piValue The integer enum value
        @param pcLabel The charcter enum label
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE LookupDesignerStateEnum (piValue AS INTEGER,
                                                 pcLabel AS CHARACTER):
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

    {Consultingwerk/EnumFromString.i Consultingwerk.SmartComponents.Tools.DynamicLookupDesigner.LookupDesignerStateEnum}

&ENDIF

END {&CLASS} .
