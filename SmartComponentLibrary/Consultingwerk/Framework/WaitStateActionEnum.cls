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
    File        : WaitStateActionEnum
    Purpose     : Enum with possible values for the wait-state, used by the
                  WaitStateManager events
    Syntax      :
    Description :
    Author(s)   : Mike Fechner
    Created     : Sun Jun 17 10:44:06 CEST 2012
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
USING Consultingwerk.Framework.*             FROM PROPATH .
USING Progress.Lang.*                        FROM PROPATH .

{&CLASS} Consultingwerk.Framework.WaitStateActionEnum
    &IF "{&CLASS}" EQ "CLASS" &THEN
    INHERITS Enum
    &ENDIF
        :

&IF "{&CLASS}" EQ "ENUM" &THEN
    DEFINE ENUM
&ENDIF

    /*------------------------------------------------------------------------------
        Purpose: The wait-state is active
        Notes:
    ------------------------------------------------------------------------------*/
    {Consultingwerk/EnumMember.i SetWaitState 0 WaitStateActionEnum}

    /*------------------------------------------------------------------------------
        Purpose: No active wait-state
        Notes:
    ------------------------------------------------------------------------------*/
    {Consultingwerk/EnumMember.i ClearWaitState 1 WaitStateActionEnum}


&IF "{&CLASS}" EQ "ENUM" &THEN
    .
&ENDIF

&IF "{&CLASS}" EQ "CLASS" &THEN


    /*------------------------------------------------------------------------------
        Purpose: Constructor for the WaitStateActionEnum members
        Notes:
        @param piValue The internal (numeric) representation of the Enumeration member
        @param pcLabel The text label of the Enumaration member
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE WaitStateActionEnum (piValue AS INTEGER, pcLabel AS CHARACTER):
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

    {Consultingwerk/EnumFromString.i Consultingwerk.Framework.WaitStateActionEnum}

&ENDIF

END {&CLASS} .
