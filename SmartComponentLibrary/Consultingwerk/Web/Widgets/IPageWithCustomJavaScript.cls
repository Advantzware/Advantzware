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
    File        : IPageWithCustomJavaScript
    Purpose     : Interface for SmartWeb Page objects that require
                  the output of specialized JavaScript library references 
                  in the html page header
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Nov 11 19:02:15 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Web.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH.

INTERFACE Consultingwerk.Web.Widgets.IPageWithCustomJavaScript:  
  
    /*------------------------------------------------------------------------------
        Purpose: Returns the comma delimited list of JavaScript libraries that 
                 need to be outputted with the Page 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY CustomJavaScriptLibraries AS CHARACTER NO-UNDO 
    GET.

END INTERFACE.