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
    File        : RetrieveDataStatusEnum
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Dec 29 00:19:56 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

&IF DEFINED (NativeEnums) NE 0 &THEN
&GLOBAL-DEFINE CLASS ENUM
&ELSE
&GLOBAL-DEFINE CLASS CLASS
&ENDIF

USING Consultingwerk.*                                FROM PROPATH .
USING Consultingwerk.Framework.Collections.*          FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .  
USING Progress.Lang.*                                 FROM PROPATH .

{&CLASS} Consultingwerk.SmartComponents.Implementation.RetrieveDataStatusEnum 
    &IF "{&CLASS}" EQ "CLASS" &THEN
    INHERITS Enum
    &ENDIF    
        : 

&IF "{&CLASS}" EQ "ENUM" &THEN
    DEFINE ENUM
&ENDIF    

    {Consultingwerk/EnumMember.i NotBatching 0 RetrieveDataStatusEnum}
    {Consultingwerk/EnumMember.i ForwardBatching 1 RetrieveDataStatusEnum}
    {Consultingwerk/EnumMember.i BackwardsBatching 2 RetrieveDataStatusEnum}
    {Consultingwerk/EnumMember.i ForwardAndBackwardsBatching 3 RetrieveDataStatusEnum}

&IF "{&CLASS}" EQ "ENUM" &THEN
    .
&ENDIF    

&IF "{&CLASS}" EQ "CLASS" &THEN

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the RetrieveDataStatusEnum members
        Notes:
        @param piValue The internal (numeric) representation of the Enumeration member
        @param pcLabel The text label of the Enumaration member
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE RetrieveDataStatusEnum (piValue AS INTEGER, pcLabel AS CHARACTER):
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
 
        DEFINE VARIABLE oReturn AS CharacterList NO-UNDO . 
 
        RETURN NEW CharacterList ('{&EnumMembers}':U).
 
    END METHOD .

    {Consultingwerk/EnumFromString.i Consultingwerk.SmartComponents.Implementation.RetrieveDataStatusEnum}    

&ENDIF

END {&CLASS} .
