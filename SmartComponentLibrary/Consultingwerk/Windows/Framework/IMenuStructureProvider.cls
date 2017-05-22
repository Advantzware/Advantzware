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
    File        : IMenuStructureProvide
    Purpose     : Interface for Service Provider Classes that can return
                  a menu structure
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Dec 07 17:34:44 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartFramework.*      FROM PROPATH .  
USING Consultingwerk.SmartFramework.Menu.* FROM PROPATH .  
USING Consultingwerk.Windows.Framework.*   FROM PROPATH .  
USING Progress.Lang.*                      FROM PROPATH .

INTERFACE Consultingwerk.Windows.Framework.IMenuStructureProvider:  
  
    {Consultingwerk/Windows/Framework/ttMenuStructure.i}
  
    /*------------------------------------------------------------------------------
        Purpose: Returns the Menu Items referenced by the givens keys
        Notes:   Used by the Menu Function LRU
        @param pcKeys The CHARACTER Array with the menu item keys 
        @param ttMenuStructure OUTOUT Temp-Table with the Menu Structure data 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetMenuItems (pcKeys AS CHARACTER EXTENT,
                                     OUTPUT TABLE ttMenuStructure).

    /*------------------------------------------------------------------------------
        Purpose: Returns the Menu Structure for a given root key
        Notes:   
        @param oGetMenuStructureParameter The IGetMenuStructureParameter with the data for the request 
        @param ttMenuStructure OUTOUT Temp-Table with the Menu Structure data 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetMenuStructure (oGetMenuStructureParameter AS IGetMenuStructureParameter,
                                         OUTPUT TABLE ttMenuStructure).

END INTERFACE.
