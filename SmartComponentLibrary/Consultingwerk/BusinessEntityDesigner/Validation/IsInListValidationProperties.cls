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
    File        : IsInListValidationProperties
    Purpose     : Properties for Character Field Validation based on a
                  valid entry in a list
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

CLASS Consultingwerk.BusinessEntityDesigner.Validation.IsInListValidationProperties 
    INHERITS BaseValidationProperties 
    IMPLEMENTS IHasTypeDescriptor
    {&SERIALIZABLE}:

    {Consultingwerk/JsonSerializableProperty.i List CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i Delimiter CHARACTER "INIT ',':U"} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the IsInListValidationProperties class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC IsInListValidationProperties ():
        SUPER ().
        
        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .

    END CONSTRUCTOR. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the Type Descriptor for the object instance
        Notes:
        @return The ITypeDescriptor for the object instance
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC OVERRIDE ITypeDescriptor GetTypeDescriptor ():
		
        DEFINE VARIABLE oTypeDescriptor AS ITypeDescriptor NO-UNDO . 
        
        oTypeDescriptor = SUPER:GetTypeDescriptor () . 
        oTypeDescriptor:TypeName = "Consultingwerk.BusinessEntityDesigner.Validation.IsInListValidationProperties":U . 

        oTypeDescriptor:PropertyDescriptors:Add ("Consultingwerk.BusinessEntityDesigner.Validation.IsInListValidationProperties":U,
                                                 "List":U,
                                                 "List"{&TRAN}) . 

        oTypeDescriptor:PropertyDescriptors:Add ("Consultingwerk.BusinessEntityDesigner.Validation.IsInListValidationProperties":U,
                                                 "Delimiter":U,
                                                 "List Delimiter"{&TRAN}) . 
                                                 
        RETURN oTypeDescriptor . 

	END METHOD.

END CLASS.
