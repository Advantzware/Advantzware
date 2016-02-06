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
    File        : MandatoryValidationProperties
    Purpose     : Properties for Mandatory Field Validation
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

CLASS Consultingwerk.BusinessEntityDesigner.Validation.MandatoryValidationProperties 
    INHERITS BaseValidationProperties 
    IMPLEMENTS IHasTypeDescriptor
    {&SERIALIZABLE}:

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the MandatoryValidationProperties class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC MandatoryValidationProperties ():
        SUPER ().
/*        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .*/

    END CONSTRUCTOR. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the Type Descriptor for the object instance
        Notes:
        @return The ITypeDescriptor for the object instance
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC OVERRIDE ITypeDescriptor GetTypeDescriptor ():
		
        DEFINE VARIABLE oTypeDescriptor AS ITypeDescriptor NO-UNDO . 
        
        oTypeDescriptor = SUPER:GetTypeDescriptor () . 
        oTypeDescriptor:TypeName = "Consultingwerk.BusinessEntityDesigner.Validation.MandatoryValidationProperties":U . 
		
		RETURN oTypeDescriptor .

	END METHOD.

END CLASS.
