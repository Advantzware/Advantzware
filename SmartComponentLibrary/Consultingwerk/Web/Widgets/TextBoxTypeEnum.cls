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
    File        : TextBoxTypeEnum
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun May 27 00:32:02 CEST 2012
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
USING Consultingwerk.Web.Widgets.*           FROM PROPATH .  
USING Progress.Lang.*                        FROM PROPATH .

{&CLASS} Consultingwerk.Web.Widgets.TextBoxTypeEnum 
    &IF "{&CLASS}" EQ "CLASS" &THEN
    INHERITS Enum
    &ENDIF    
        : 

&IF "{&CLASS}" EQ "ENUM" &THEN
    DEFINE ENUM
&ENDIF    
 
    {Consultingwerk/EnumMember.i Text 0 TextBoxTypeEnum}
    {Consultingwerk/EnumMember.i Password 1 TextBoxTypeEnum}
    {Consultingwerk/EnumMember.i Tel 2 TextBoxTypeEnum}     
    {Consultingwerk/EnumMember.i Number 3 TextBoxTypeEnum}
    {Consultingwerk/EnumMember.i Email 4 TextBoxTypeEnum}
    {Consultingwerk/EnumMember.i Date 5 TextBoxTypeEnum}
    {Consultingwerk/EnumMember.i Datetime 6 TextBoxTypeEnum}
    {Consultingwerk/EnumMember.i Search 7 TextBoxTypeEnum}
    
&IF "{&CLASS}" EQ "ENUM" &THEN
    .
&ENDIF    

&IF "{&CLASS}" EQ "CLASS" &THEN
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the TextBoxTypeEnum members
        Notes:
        @param piValue The internal (numeric) representation of the Enumeration member
        @param pcLabel The text label of the Enumaration member
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE TextBoxTypeEnum (piValue AS INTEGER, pcLabel AS CHARACTER):
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
    
    {Consultingwerk/EnumFromString.i Consultingwerk.Web.Widgets.TextBoxTypeEnum}    
    
&ENDIF
    
END {&CLASS} .
