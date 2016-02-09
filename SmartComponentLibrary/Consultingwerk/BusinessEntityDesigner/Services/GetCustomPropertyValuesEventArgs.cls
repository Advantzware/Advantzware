/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : GetCustomPropertyValuesEventArgs
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd. 
    Created     : Tue May 20 16:09:30 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                                 FROM PROPATH .
USING Consultingwerk.Framework.Collections.*           FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Services.* FROM PROPATH .  
USING Progress.Lang.*                                  FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Services.GetCustomPropertyValuesEventArgs 
    INHERITS EventArgs: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the current custom property 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY PropertyName AS CHARACTER NO-UNDO 
	GET.
	PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the list of possible values 
        Notes:   Event Subscribes are supposed to add property values to the list
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY PropertyValues AS CharacterList NO-UNDO 
	GET.
	PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the current property scope
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY Scope AS CustomPropertiesScopeEnum NO-UNDO 
	GET.
	PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the GetCustomPropertyValuesEventArgs class
        Notes:   
        @param poCustomPropertiesScope The property scope to edit the custom properties for
        @param pcPropertyName The name of the custom property 
        @param poList The list to add values to
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC GetCustomPropertyValuesEventArgs (poCustomPropertiesScope AS CustomPropertiesScopeEnum,
	                                                     pcPropertyName AS CHARACTER,
	                                                     poList AS CharacterList):
		SUPER ().
		
		ASSIGN THIS-OBJECT:Scope          = poCustomPropertiesScope
		       THIS-OBJECT:PropertyName   = pcPropertyName 
		       THIS-OBJECT:PropertyValues = poList . 
		
	END CONSTRUCTOR.

END CLASS.
