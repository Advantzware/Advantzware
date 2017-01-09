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
    File        : IGetMenuStructureParameter
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Dec 07 17:48:30 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartFramework.*      FROM PROPATH .  
USING Consultingwerk.SmartFramework.Menu.* FROM PROPATH .  
USING Progress.Lang.*                      FROM PROPATH .

INTERFACE Consultingwerk.SmartFramework.IGetMenuStructureParameter:  

    /*------------------------------------------------------------------------------
        Purpose: Returns if the Root Menu Entry should also te retrieved 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY IncludeRootMenuEntry AS LOGICAL NO-UNDO 
    GET.
    
    /*------------------------------------------------------------------------------
        Purpose: Returns the Number of Levels deep the Menu is requested 
        Notes:   Use 0 to request all menu data
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY NumberOfLevels AS INTEGER NO-UNDO 
    GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the Root Node Key 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY RootMenuKey AS CHARACTER NO-UNDO 
    GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns if the authorization checks should be skipps 
        Notes:   Allows to retrieve full menu structure to clients that perform 
                 authentication only after the first menu item has been selected 
                 etc.
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SkipAuthorizationCheck AS LOGICAL NO-UNDO 
    GET.

END INTERFACE.