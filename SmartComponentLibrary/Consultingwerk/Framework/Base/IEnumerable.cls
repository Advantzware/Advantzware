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
    File        : IEnumerable
    Purpose     : Interface definition for classes supporting the Enumerator 
                  pattern, typically implemented by Lists or Collections
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Jan 20 20:43:32 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Framework.Base.* FROM PROPATH .
USING Progress.Lang.*                 FROM PROPATH .

INTERFACE Consultingwerk.Framework.Base.IEnumerable:  
  
    /*------------------------------------------------------------------------------
        Purpose: Returns a new IEnumerator instance for this object instance                                                                      
        Notes:                                                                        
        @return The IEnumerator instance for this object
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC IEnumerator GetEnumerator ().

END INTERFACE.
