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
    File        : GetBusinessEntityDescriptorParameter
    Purpose     : Parameter for the GetBusinessEntityDescriptor method
                  of the BusinessEntity class
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Jul 10 17:55:09 CEST 2015
    Notes       : http://confluence.consultingwerkcloud.com/wiki/display/SCL/Business+Entity+Descriptor
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                               FROM PROPATH . 
USING Consultingwerk.OERA.BusinessEntityDescriptor.* FROM PROPATH .  
USING Progress.Lang.*                                FROM PROPATH .

CLASS Consultingwerk.OERA.BusinessEntityDescriptor.GetBusinessEntityDescriptorParameter
&IF '{&FrameworkSerializationType}' EQ 'XML' OR PROVERSION BEGINS '10.2':U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF
    {&SERIALIZABLE}:

    {Consultingwerk/JsonSerializableProperty.i Descriptor Consultingwerk.OERA.BusinessEntityDescriptor.IBusinessEntityDescriptor}        
    {Consultingwerk/JsonSerializableProperty.i ReturnDataset LOGICAL "INIT FALSE"}        

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the GetBusinessEntityDescriptorParameter class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC GetBusinessEntityDescriptorParameter ():
        SUPER ().
        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .

    END CONSTRUCTOR .  

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the GetBusinessEntityDescriptorParameter class
        Notes:
        @param plReturnDataset Logical value indicating if the request should return the Dataset of the Business Entity as well
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC GetBusinessEntityDescriptorParameter (plReturnDataset AS LOGICAL):
        THIS-OBJECT ().

        ASSIGN THIS-OBJECT:ReturnDataset = plReturnDataset . 

    END CONSTRUCTOR .  

END CLASS.
