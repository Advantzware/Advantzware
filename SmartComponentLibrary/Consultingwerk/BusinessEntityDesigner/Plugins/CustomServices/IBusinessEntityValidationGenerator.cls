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
    File        : IBusinessEntityValidationGenerator
    Purpose     : Service Interface for custom generators for Business
                  Entity Validation code
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue May 26 20:45:35 CEST 2015
    Notes       : SCL-424
  ----------------------------------------------------------------------*/

USING Consultingwerk.BusinessEntityDesigner.Plugins.CustomServices.* FROM PROPATH .
USING Progress.Lang.*                                                FROM PROPATH .

INTERFACE Consultingwerk.BusinessEntityDesigner.Plugins.CustomServices.IBusinessEntityValidationGenerator:

    {Consultingwerk/BusinessEntityDesigner/Services/dsBusinessEntity.i &REFERENCE-ONLY=REFERENCE-ONLY}

    /*------------------------------------------------------------------------------
        Purpose: Generates the Business Entity Validation Code
        Notes:
        @param dsBusinessEntity The Business Entity Dataset passed by-reference
        @return The generated validation code, including the @GeneratedValidationStart. and @GeneratedValidationEnd. Annotations
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LONGCHAR GenerateValidationCode (DATASET dsBusinessEntity).

END INTERFACE.
