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
    File        : IDynamicBrowserRenderer
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jul 15 15:35:55 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .  
USING Progress.Lang.*                                 FROM PROPATH.

INTERFACE Consultingwerk.SmartComponents.Implementation.IDynamicBrowserRenderer:  
  
    {Consultingwerk/Framework/RepositoryServices/dsDynamicBrowser.i &REFERENCE-ONLY=REFERENCE-ONLY}
  
    /*------------------------------------------------------------------------------
        Purpose: Renders a Dynamic Browser based on the repository definition 
        Notes:   
        @param pcBrowserKey The key of the dynamic browser to render
        @param poBrowser The reference to the SmartDataBrowser to apply the settings to
        @param dsDynamicBrowser DATASET The dataset with the browser layout information
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RenderDynamicBrowser (pcBrowserKey AS CHARACTER,
                                             poBrowser AS SmartDataBrowser,
                                             DATASET dsDynamicBrowser).

END INTERFACE.
