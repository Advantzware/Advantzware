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
    File        : IMenuItemFilter
    Purpose     : Allows filtering of MENU-ITEMS during 
                  UltraToolbarsHelper:BuildToolsCollection
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Oct 15 11:23:53 CEST 2014
    Notes       : Jira Ticket WINKIT-13
  ----------------------------------------------------------------------*/

USING Consultingwerk.Framework.*            FROM PROPATH .
USING Consultingwerk.WindowIntegrationKit.* FROM PROPATH .
USING Progress.Lang.*                       FROM PROPATH .

INTERFACE Consultingwerk.WindowIntegrationKit.IMenuItemFilter:  
  
	/*------------------------------------------------------------------------------
	   Purpose:  Returns if the Widget should be rendered 																	  
	   Notes:    Return FALSE to filter out the current menu item 
       @param poForm The IFormWithUltraToolbarsManager instance
       @param phWidget The handle of the represented menu item widget (menu item, sub-menu, ...)
       @return Logical value indicating if the Widget should be filtered out or not (FALSE will skip the current widget)		     																	  
	------------------------------------------------------------------------------*/
	METHOD PUBLIC LOGICAL FilterMenuItem (poForm   AS IFormWithUltraToolbarsManager,
	                                      phWidget AS HANDLE).

END INTERFACE.