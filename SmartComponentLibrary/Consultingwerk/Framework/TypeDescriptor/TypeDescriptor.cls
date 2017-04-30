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
    File        : TypeDescriptor
    Purpose     : Provides additional information about an ABL class
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Apr 29 07:05:34 CEST 2015
    Notes       : SCL-785
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                          FROM PROPATH . 
USING Consultingwerk.Framework.TypeDescriptor.* FROM PROPATH .  
USING Progress.Lang.*                           FROM PROPATH .

&GLOBAL-DEFINE FrameworkSerializationType XML

CLASS Consultingwerk.Framework.TypeDescriptor.TypeDescriptor 
    INHERITS XmlSerializable
    IMPLEMENTS ITypeDescriptor: 

    /*------------------------------------------------------------------------------
        Purpose: Returns a description for the type
        Notes:
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i Description CHARACTER} .

    /*------------------------------------------------------------------------------
        Purpose: Returns the list of property descriptors for the type
        Notes:
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i PropertyDescriptors ListIPropertyDescriptor} .

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the type that defines the property
        Notes:   
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i TypeName CHARACTER} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the TypeDescriptor class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC TypeDescriptor ():
        SUPER ().
        
        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .
        
        ASSIGN THIS-OBJECT:PropertyDescriptors = NEW ListIPropertyDescriptor () .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the TypeDescriptor class
        Notes:
        @param pcTypeName The name of the type 
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC TypeDescriptor (pcTypeName AS CHARACTER):
        THIS-OBJECT ().
        
        ASSIGN THIS-OBJECT:TypeName    = pcTypeName .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the TypeDescriptor class
        Notes:
        @param pcTypeName The name of the type 
        @param pcDescription The description of the type
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC TypeDescriptor (pcTypeName AS CHARACTER,
                                       pcDescription AS CHARACTER):
        THIS-OBJECT ().
        
        ASSIGN THIS-OBJECT:TypeName    = pcTypeName 
               THIS-OBJECT:Description = pcDescription .
        
    END CONSTRUCTOR.

END CLASS.
