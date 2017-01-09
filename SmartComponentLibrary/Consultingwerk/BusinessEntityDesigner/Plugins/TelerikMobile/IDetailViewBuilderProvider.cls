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
    File        : IDetailViewBuilderProvider
    Purpose     : Service Interface for classes which can provide the 
                  Detail View Builder class names and the list of available 
                  Templates to the DetailViewWizard
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon May 04 09:18:46 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.*                                              FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Plugins.TelerikMobile.* FROM PROPATH .  
USING Progress.Lang.*                                               FROM PROPATH .

INTERFACE Consultingwerk.BusinessEntityDesigner.Plugins.TelerikMobile.IDetailViewBuilderProvider:  

    /*------------------------------------------------------------------------------
        Purpose: Returns the list of available Detail View Builders 
        Notes:   
        @return The ListNameValuePair with the available Grid Builder classes
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC ListNameValuePair GetAvailableDetailViewBuilders ().

    /*------------------------------------------------------------------------------
        Purpose: Returns the List of the available Page Template Names 
        Notes:   
        @return The ListNameValuePair with the available Page Template Names
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC ListNameValuePair GetAvailableTemplates ().

END INTERFACE.
