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
    File        : FieldRemovedHandlingEnum
    Purpose     : Enum with possible values how to deal with temp-table fields
                  that are still part of the model, but the source database 
                  field is not longer existing
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Sep 10 12:14:18 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

&IF DEFINED (NativeEnums) NE 0 &THEN
&GLOBAL-DEFINE CLASS ENUM
&ELSE
&GLOBAL-DEFINE CLASS CLASS
&ENDIF

USING Consultingwerk.Enum                              FROM PROPATH .
USING Consultingwerk.Framework.Collections.*           FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Services.* FROM PROPATH .  
USING Progress.Lang.*                                  FROM PROPATH .

{&CLASS} Consultingwerk.BusinessEntityDesigner.Services.FieldRemovedHandlingEnum 
    &IF "{&CLASS}" EQ "CLASS" &THEN
    INHERITS Enum
    &ENDIF    
        : 

&IF "{&CLASS}" EQ "ENUM" &THEN
    DEFINE ENUM
&ENDIF    

    /*------------------------------------------------------------------------------
        Purpose: Clears the source column information of the temp-table field in the
                 model                                       
        Notes:          
    ------------------------------------------------------------------------------*/
    {Consultingwerk/EnumMember.i RemoveSourceColumn 0 FieldRemovedHandlingEnum}
    
    /*------------------------------------------------------------------------------
        Purpose: Removes the temp-table field from the model                                           
        Notes:          
    ------------------------------------------------------------------------------*/
    {Consultingwerk/EnumMember.i RemoveField 1 FieldRemovedHandlingEnum}

    /*------------------------------------------------------------------------------
        Purpose: Do nothing, the temp-table field will not be deleted and the source 
                 column information will not be cleared                                                                      
        Notes:          
    ------------------------------------------------------------------------------*/
    {Consultingwerk/EnumMember.i DoNothing 2 FieldRemovedHandlingEnum}

&IF "{&CLASS}" EQ "ENUM" &THEN
    .
&ENDIF    

&IF "{&CLASS}" EQ "CLASS" &THEN

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the FieldRemovedHandlingEnum members
        Notes:
        @param piValue The internal (numeric) representation of the Enumeration member
        @param pcLabel The text label of the Enumaration member
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE FieldRemovedHandlingEnum (piValue AS INTEGER, pcLabel AS CHARACTER):
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
    
    {Consultingwerk/EnumFromString.i Consultingwerk.BusinessEntityDesigner.Services.FieldRemovedHandlingEnum}

&ENDIF 

END {&CLASS} .
