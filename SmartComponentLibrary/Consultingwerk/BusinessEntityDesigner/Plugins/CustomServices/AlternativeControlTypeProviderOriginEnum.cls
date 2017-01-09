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
    File        : AlternativeControlTypeProviderOriginEnum
    Purpose     : Values for the Origin parameter of the ISmartViewerBuilderAlternativeControlTypeProvider:AddCustomControlTypes method 
    Syntax      : 
    Description : Describes from where ISmartViewerBuilderAlternativeControlTypeProvider:AddCustomControlTypes 
                  is called
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Apr 16 22:44:22 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

&IF DEFINED (NativeEnums) NE 0 &THEN
&GLOBAL-DEFINE CLASS ENUM
&ELSE
&GLOBAL-DEFINE CLASS CLASS
&ENDIF

USING Consultingwerk.*                                               FROM PROPATH .
USING Consultingwerk.Framework.Collections.*                         FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Plugins.CustomServices.* FROM PROPATH .  
USING Progress.Lang.*                                                FROM PROPATH .

{&CLASS} Consultingwerk.BusinessEntityDesigner.Plugins.CustomServices.AlternativeControlTypeProviderOriginEnum 
    &IF "{&CLASS}" EQ "CLASS" &THEN
    INHERITS Enum
    &ENDIF    
        : 

&IF "{&CLASS}" EQ "ENUM" &THEN
    DEFINE ENUM
&ENDIF    

    {Consultingwerk/EnumMember.i BusinessEntityDesigner 0 AlternativeControlTypeProviderOriginEnum}
    {Consultingwerk/EnumMember.i AblFrameMigrationForm 1 AlternativeControlTypeProviderOriginEnum}

&IF "{&CLASS}" EQ "ENUM" &THEN
    .
&ENDIF    

&IF "{&CLASS}" EQ "CLASS" &THEN

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the AlternativeControlTypeProviderOriginEnum members
        Notes:
        @param piValue The internal (numeric) representation of the Enumeration member
        @param pcLabel The text label of the Enumaration member
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE AlternativeControlTypeProviderOriginEnum (piValue AS INTEGER, pcLabel AS CHARACTER):
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
	
    {Consultingwerk/EnumFromString.i Consultingwerk.BusinessEntityDesigner.Plugins.CustomServices.AlternativeControlTypeProviderOriginEnum} 

&ENDIF

END {&CLASS} .
