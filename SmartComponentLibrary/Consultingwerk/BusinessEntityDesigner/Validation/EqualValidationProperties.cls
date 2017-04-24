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
    File        : EqualValidationProperties
    Purpose     : Properties for field is equal to a value validation
    Syntax      :
    Description :
    Author(s)   : Mark Bartscherer / Consultingwerk Ltd.
    Created     : Thu Dec 29 15:35:17 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.BusinessEntityDesigner.Validation.* FROM PROPATH .
USING Progress.Lang.*                                    FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Validation.EqualValidationProperties
    INHERITS BaseComparisonValidationProperties:

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the EqualValidationProperties class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC EqualValidationProperties ():

        SUPER ("EQ":U, "Consultingwerk.BusinessEntityDesigner.Validation.EqualValidationProperties":U ).

        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .

    END CONSTRUCTOR.

END CLASS.
