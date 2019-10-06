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
    File        : GetObjectMasterDesignAttributeParameter
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

CLASS Consultingwerk.SmartFramework.Repository.Class.GetObjectMasterDesignAttributeParameter
&IF '{&FrameworkSerializationType}' EQ 'XML' OR PROVERSION BEGINS '10.2':U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF
    {&SERIALIZABLE}:

    {Consultingwerk/JsonSerializableProperty.i ObjectMasterGuid CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i AttributeLabel CHARACTER} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the GetObjectMasterDesignAttributeParameter class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC GetObjectMasterDesignAttributeParameter ():
        SUPER ().
        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the GetObjectMasterDesignAttributeParameter class
        Notes:
        @param pcObjectMasterGuid The GUID of the Object Master to retrieve the design attribute record for
        @param pcAttributeLabel The Label of the Attribute to retrieve
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC GetObjectMasterDesignAttributeParameter (pcObjectMasterGuid AS CHARACTER,
                                                                pcAttributeLabel AS CHARACTER):
        THIS-OBJECT ().

        ASSIGN THIS-OBJECT:ObjectMasterGuid = pcObjectMasterGuid
               THIS-OBJECT:AttributeLabel   = pcAttributeLabel .

    END CONSTRUCTOR.

END CLASS.
