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
    File        : IPropertyDescriptor
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Feb 22 15:01:33 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartComponents.TypeDescriptor.* FROM PROPATH .  
USING Progress.Lang.*                                 FROM PROPATH.

INTERFACE Consultingwerk.SmartComponents.TypeDescriptor.IPropertyDescriptor:  
  
    /*------------------------------------------------------------------------------
        Purpose: Returns the List of Attributes for the Property
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Attributes AS "System.Collections.Generic.List<System.Attribute>":U NO-UNDO 
    GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the Name of the Property 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY PropertyName AS CHARACTER NO-UNDO 
	GET.

END INTERFACE.