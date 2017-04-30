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
    File        : IClassCache
    Purpose     : Provides cache functionality for classes derived from 
                  a given base type, typically during design time 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Sep 22 20:55:27 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartComponents.Support.* FROM PROPATH .  
USING Progress.Lang.*                          FROM PROPATH .

INTERFACE Consultingwerk.SmartComponents.Support.IClassCache:  
  
    {Consultingwerk/Util/TempTables/ttClassNames.i}
  
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the ClassName this class is caching 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ClassName AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the class names temp-table
        Notes:   
        @param ttClassNames The temp-table with the class names 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetClassNames (OUTPUT TABLE ttClassNames) .

    /*------------------------------------------------------------------------------
        Purpose: Refreshes the Class Names Cache 
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Refresh ().

END INTERFACE.
