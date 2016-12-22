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
    File        : BindingSourceFieldsEnum
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jun 11 21:59:59 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

&IF DEFINED (NativeEnums) NE 0 &THEN
&GLOBAL-DEFINE CLASS ENUM
&ELSE
&GLOBAL-DEFINE CLASS CLASS
&ENDIF

USING Consultingwerk.*                               FROM PROPATH .
USING Consultingwerk.Framework.Collections.*         FROM PROPATH .
USING Consultingwerk.WindowIntegrationKit.Controls.* FROM PROPATH .  
USING Progress.Lang.*                                FROM PROPATH .

{&CLASS} Consultingwerk.WindowIntegrationKit.Controls.BindingSourceFieldsEnum 
    &IF "{&CLASS}" EQ "CLASS" &THEN
    INHERITS Enum
    &ENDIF    
        : 

&IF "{&CLASS}" EQ "ENUM" &THEN
    DEFINE ENUM
&ENDIF    

    {Consultingwerk/EnumMember.i AllFields 0 BindingSourceFieldsEnum}
    {Consultingwerk/EnumMember.i BrowserFields 1 BindingSourceFieldsEnum}
    {Consultingwerk/EnumMember.i UseList 2 BindingSourceFieldsEnum}
    {Consultingwerk/EnumMember.i ListAndFields 3 BindingSourceFieldsEnum}

&IF "{&CLASS}" EQ "ENUM" &THEN
    .
&ENDIF    

&IF "{&CLASS}" EQ "CLASS" &THEN

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the BindingSourceFieldsEnum members
        Notes:
        @param piValue The internal (numeric) representation of the Enumeration member
        @param pcLabel The text label of the Enumaration member
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE BindingSourceFieldsEnum (piValue AS INTEGER, pcLabel AS CHARACTER):
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
    
    {Consultingwerk/EnumFromString.i Consultingwerk.WindowIntegrationKit.Controls.BindingSourceFieldsEnum}    
    
&ENDIF
    
END {&CLASS} .
