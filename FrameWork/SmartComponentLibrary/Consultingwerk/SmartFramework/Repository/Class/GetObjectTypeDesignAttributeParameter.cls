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
    File        : GetObjectTypeDesignAttributeParameter
    Purpose     : Parameter object for the GetObjectTypeDesignAttribute 
                  method of the AttributeValueBusinessEntity
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Oct 20 21:11:56 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                                 FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.Class.* FROM PROPATH .  
USING Progress.Lang.*                                  FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Repository.Class.GetObjectTypeDesignAttributeParameter 
&IF '{&FrameworkSerializationType}' EQ 'XML' OR PROVERSION BEGINS '10.2':U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF
    {&SERIALIZABLE}:

    {Consultingwerk/JsonSerializableProperty.i ObjectTypeName CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i AttributeLabel CHARACTER} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the GetObjectTypeDesignAttributesParameter class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC GetObjectTypeDesignAttributeParameter ():
        SUPER ().
        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .

    END CONSTRUCTOR. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the GetObjectTypeDesignAttributesParameter class
        Notes:
        @param pcObjectTypeName The name of the Object Type to retrieve the design attribute record for
        @param pcAttributeLabel The Label of the Attribute to retrieve
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC GetObjectTypeDesignAttributeParameter (pcObjectTypeName AS CHARACTER,
                                                              pcAttributeLabel AS CHARACTER):
        THIS-OBJECT ().
        
        ASSIGN THIS-OBJECT:ObjectTypeName = pcObjectTypeName 
               THIS-OBJECT:AttributeLabel = pcAttributeLabel .

    END CONSTRUCTOR. 

END CLASS.
