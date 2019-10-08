/**********************************************************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
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
    File        : StoreObjectTypeDesignAttributeParameter
    Purpose     : Parameter object for the StoreObjectTypeDesignAttribute
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

CLASS Consultingwerk.SmartFramework.Repository.Class.StoreObjectTypeDesignAttributeParameter
&IF '{&FrameworkSerializationType}' EQ 'XML' OR PROVERSION BEGINS '10.2':U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF
    {&SERIALIZABLE}:

    {Consultingwerk/JsonSerializableProperty.i ObjectTypeGuid CHARACTER} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the StoreObjectTypeDesignAttributeParameter class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC StoreObjectTypeDesignAttributeParameter ():
        SUPER ().
        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the StoreObjectTypeDesignAttributeParameter class
        Notes:
        @param pcObjectTypeGuid The Guid of the Object Type to store attribute values for
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC StoreObjectTypeDesignAttributeParameter (pcObjectTypeGuid AS CHARACTER):
        THIS-OBJECT ().

        ASSIGN THIS-OBJECT:ObjectTypeGuid = pcObjectTypeGuid .

    END CONSTRUCTOR.

END CLASS.
