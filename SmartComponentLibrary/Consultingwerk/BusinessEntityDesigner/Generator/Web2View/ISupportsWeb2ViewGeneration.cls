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
    File        : ISupportsWeb2ViewGeneration
    Purpose     : Interface for Control Generators that support rendering 
                  of Web2 Views 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Nov 01 12:53:40 GMT+01:00 2015
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.BusinessEntityDesigner.Generator.Web2View.* FROM PROPATH .  
USING Progress.Lang.*                                            FROM PROPATH .

INTERFACE Consultingwerk.BusinessEntityDesigner.Generator.Web2View.ISupportsWeb2ViewGeneration:  

    /*------------------------------------------------------------------------------
        Purpose: Returns the HTML for the current Control 
        Notes:   
        @return The HTML source for the current Control 
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC CHARACTER GenerateHtml ().

END INTERFACE.
