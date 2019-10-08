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
    File        : IPageFooterHandler
    Purpose     : Service Interface for classes that provide a footer to 
                  SmartComponents.Web pages
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Jun 02 23:08:38 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Web.* FROM PROPATH .  
USING Progress.Lang.*      FROM PROPATH .

INTERFACE Consultingwerk.Web.IPageFooterHandler:  
  
    /*------------------------------------------------------------------------------
        Purpose: Writes the Footer to the Webstream
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID OutputFooter ().

END INTERFACE.