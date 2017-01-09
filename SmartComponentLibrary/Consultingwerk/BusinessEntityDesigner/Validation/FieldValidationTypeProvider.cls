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
    File        : FieldValidationTypeProvider
    Purpose     : Default IFieldValidationTypeProvider implementation.
                  Provides available validation types to the Business 
                  Entity Designer
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon May 18 21:13:43 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                                   FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Validation.* FROM PROPATH .  
USING Progress.Lang.*                                    FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Validation.FieldValidationTypeProvider 
    IMPLEMENTS IFieldValidationTypeProvider: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the list of available validation types
        Notes:
        @return The list of Validation Types 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC ListNameValuePair GetFieldValidationTypes ():

        DEFINE VARIABLE oList AS ListNameValuePair NO-UNDO . 
        
        oList = NEW ListNameValuePair () . 
        
        oList:Add ("Consultingwerk.BusinessEntityDesigner.Validation.MandatoryValidationProperties":U, "Mandatory":U) . 
        oList:Add ("Consultingwerk.BusinessEntityDesigner.Validation.IsInListValidationProperties":U,  "Is In List":U) . 
         		
        RETURN oList .         		

	END METHOD.

END CLASS.
