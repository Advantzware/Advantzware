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
    File        : IClassCacheProvider
    Purpose     : Provides caching functionality for the list of physical 
                  classes implementing a given base type 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd. 
    Created     : Mon Sep 22 21:08:45 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartComponents.Support.* FROM PROPATH .  
USING Progress.Lang.*                          FROM PROPATH .

INTERFACE Consultingwerk.SmartComponents.Support.IClassCacheProvider:  
  
    {Consultingwerk/Util/TempTables/ttClassNames.i}
  
    /*------------------------------------------------------------------------------
        Purpose: Returns the list of Class Names for the given base class  
        Notes:   
        @param pcClassName The base type (class or interface) that the returned classes need to match
        @param ttClassNames OUTPUT Temp-Table with the list of matching classnames 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetClassNames (pcClassName AS CHARACTER, 
                                      OUTPUT TABLE ttClassNames).

    /*------------------------------------------------------------------------------
        Purpose: Refreshes the Cache for the given base type
        Notes:   
        @param pcClassName The base type (class or interface) that the returned classes need to match
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Refresh (pcClassName AS CHARACTER).

END INTERFACE.
