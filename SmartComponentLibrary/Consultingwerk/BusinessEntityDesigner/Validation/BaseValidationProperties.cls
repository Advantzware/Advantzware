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
    File        : BaseValidationProperties
    Purpose     : Base Properties for Field Validation
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon May 18 21:19:19 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                                   FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Validation.* FROM PROPATH .  
USING Consultingwerk.Framework.TypeDescriptor.*          FROM PROPATH .
USING Progress.Lang.*                                    FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Validation.BaseValidationProperties 
    INHERITS JsonSerializable
    IMPLEMENTS IHasTypeDescriptor
    {&SERIALIZABLE}
    ABSTRACT:

    {Consultingwerk/JsonSerializableProperty.i ValidationMessage CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i SmartMessageGroup CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i SmartMessageNumber INTEGER} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the BaseValidationProperties class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC BaseValidationProperties ():
        SUPER ().
        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .

    END CONSTRUCTOR. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the Type Descriptor for the object instance
        Notes:
        @return The ITypeDescriptor for the object instance
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC ITypeDescriptor GetTypeDescriptor ():
		
		DEFINE VARIABLE oTypeDescriptor AS TypeDescriptor NO-UNDO . 
		
		oTypeDescriptor = NEW TypeDescriptor () . 

        oTypeDescriptor:PropertyDescriptors:Add ("Consultingwerk.BusinessEntityDesigner.Validation.BaseValidationProperties":U,
                                                 "ValidationMessage":U,
                                                 "Validation Message"{&TRAN}) . 

        oTypeDescriptor:PropertyDescriptors:Add ("Consultingwerk.BusinessEntityDesigner.Validation.BaseValidationProperties":U,
                                                 "SmartMessageGroup":U,
                                                 "Message Group"{&TRAN}) . 

        oTypeDescriptor:PropertyDescriptors:Add ("Consultingwerk.BusinessEntityDesigner.Validation.BaseValidationProperties":U,
                                                 "SmartMessageNumber":U,
                                                 "Message Number"{&TRAN},
                                                 EditorTypeEnum:NumericInteger) . 

        RETURN oTypeDescriptor .

	END METHOD.

END CLASS.
