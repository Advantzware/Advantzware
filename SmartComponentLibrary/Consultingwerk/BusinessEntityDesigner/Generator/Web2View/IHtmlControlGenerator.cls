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
    File        : IHtmlControlGenerator
    Purpose     : Interface for Control HTML Code Generators Services
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Nov 01 12:58:29 GMT+01:00 2015
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.BusinessEntityDesigner.Generator.Web2View.* FROM PROPATH .  
USING Progress.Lang.*                                            FROM PROPATH.

INTERFACE Consultingwerk.BusinessEntityDesigner.Generator.Web2View.IHtmlControlGenerator:  
  
    /*------------------------------------------------------------------------------
        Purpose: Returns the HTML code for the Control
        Notes:   
        @param pcTag The HTML tag for the Control
        @param pcAttributes The attributes for the Control
        @param pcBinding The Control binding attribute
        @param pcInnerHtml The inner html for the control
        @return The HTML code for the Control 
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC CHARACTER GenerateHtmlCode (pcTag AS CHARACTER,
	                                          pcAttributes AS CHARACTER,
	                                          pcBinding AS CHARACTER,
	                                          pcInnerHtml AS CHARACTER).

    /*------------------------------------------------------------------------------
        Purpose: Returns the HTML code for the Control
        Notes:   
        @param pcTag The HTML tag for the Control
        @param pcAttributes The attributes for the Control
        @param pcBinding The Control binding attribute
        @return The HTML code for the Control 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GenerateHtmlCode (pcTag AS CHARACTER,
                                              pcAttributes AS CHARACTER,
                                              pcBinding AS CHARACTER).

    /*------------------------------------------------------------------------------
        Purpose: Returns the HTML code for the Control
        Notes:   
        @param pcTag The HTML tag for the Control
        @param pcAttributes The attributes for the Control
        @return The HTML code for the Control 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GenerateHtmlCode (pcTag AS CHARACTER,
                                              pcAttributes AS CHARACTER).

END INTERFACE.
