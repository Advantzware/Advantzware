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
    File        : IClassAnnotationProvider
    Purpose     : Describes method required to return the ClassAnnotations
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jul 01 21:54:23 CEST 2015
    Notes       : Uses cache to optimize runtime access to class information 
                  http://confluence.consultingwerkcloud.com/wiki/display/SCL/The+Annotation+based+Type+Descriptor
  ----------------------------------------------------------------------*/

USING Consultingwerk.Framework.TypeDescriptor.* FROM PROPATH .  
USING Progress.Lang.*                           FROM PROPATH .

INTERFACE Consultingwerk.Framework.TypeDescriptor.IClassAnnotationProvider:  
  
    /*------------------------------------------------------------------------------
        Purpose: Clears the cache  
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID ClearCache ().

    /*------------------------------------------------------------------------------
        Purpose: Returns the ClassAnnotationContainer with the annotations of the 
                 given class
        Notes:   
        @param poClass The class to return the Annotations for
        @return The ClassAnnotationContainer with the annotations of the given class
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC ClassAnnotationContainer GetClassAnnotations (poClass AS Progress.Lang.Class).

END INTERFACE.
