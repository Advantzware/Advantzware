/**********************************************************************
 * Copyright (C) 2006-2013 by Consultingwerk Ltd. ("CW") -            *
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
    File        : PropertyDescriptorCollection
    Purpose     : Generic Dictionary of IPropertyDescriptor objects
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Feb 22 15:13:02 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.TypeDescriptor.* FROM PROPATH .  
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.SmartComponents.TypeDescriptor.PropertyDescriptorCollection
    INHERITS Consultingwerk.Framework.Base.GenericDictionary: 

    {Consultingwerk/Framework/Base/GenericDictionary.i Consultingwerk.SmartComponents.TypeDescriptor.IPropertyDescriptor}

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the name of the class that these properties belong to  
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY TypeName AS CHARACTER NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Adds an IPropertyDescriptor to the Collection
        Notes:   
        @param pcPropertyName The name of the property to add
        @return The new IPropertyDescriptor instance 
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC IPropertyDescriptor Add (pcPropertyName AS CHARACTER):
		
		RETURN THIS-OBJECT:Add (pcPropertyName, NEW PropertyDescriptor (pcPropertyName)) .

	END METHOD .

END CLASS.
