/**********************************************************************
 * Copyright (C) 2006-2012 by Consultingwerk Ltd. ("CW") -            *
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
    File        : IFieldValidationTypeProvider
    Purpose     : Interface for service instances that provide available
                  Validation types to the Business Entity Designer 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon May 18 21:06:08 CEST 2015
    Notes       : SCL-424
  ----------------------------------------------------------------------*/

USING Consultingwerk.*                                   FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Validation.* FROM PROPATH . 
USING Progress.Lang.*                                    FROM PROPATH .

INTERFACE Consultingwerk.BusinessEntityDesigner.Validation.IFieldValidationTypeProvider:  
  
    /*------------------------------------------------------------------------------
        Purpose: Returns the list of available validation types
        Notes:
        @return The list of Validation Types 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC ListNameValuePair GetFieldValidationTypes ().

END INTERFACE.
