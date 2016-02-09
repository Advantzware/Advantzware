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
    File        : IHasTypeDescriptor
    Purpose     : Interface for classes that provide a type descriptor
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Apr 29 06:53:04 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Framework.TypeDescriptor.* FROM PROPATH . 
USING Progress.Lang.*                           FROM PROPATH .

INTERFACE Consultingwerk.Framework.TypeDescriptor.IHasTypeDescriptor:  

	/*------------------------------------------------------------------------------
	    Purpose: Returns the Type Descriptor for the object instance
	    Notes:
	    @return The ITypeDescriptor for the object instance
	------------------------------------------------------------------------------*/
	METHOD PUBLIC ITypeDescriptor GetTypeDescriptor ().

END INTERFACE.
