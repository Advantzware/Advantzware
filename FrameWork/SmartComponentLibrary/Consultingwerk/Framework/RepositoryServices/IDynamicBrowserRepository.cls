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
    File        : IDynamicBrowserRepository
    Purpose     : Interface definition for classes that can load and store 
                  the design of a dynamic viewer
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jul 15 15:09:23 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Framework.RepositoryServices.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH.

INTERFACE Consultingwerk.Framework.RepositoryServices.IDynamicBrowserRepository:  
  
    {Consultingwerk/Framework/RepositoryServices/dsDynamicBrowser.i}
  
    /*------------------------------------------------------------------------------
        Purpose: Loads the Layout of the dynamic browser                                                                      
        Notes:                                                                          
        @param pcSettingsKey The key to retrieve the settings for
        @param dsDynamicBrowser OUTPUT DATASET The dataset with the layout of the browser                                                                              
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID LoadSettings (pcSettingsKey AS CHARACTER,
                                     OUTPUT DATASET FOR dsDynamicBrowser) .
    
    /*------------------------------------------------------------------------------
        Purpose: Saves the Layout of the dynamic browser                                                                       
        Notes:
        @param pcSettingsKey The key to store the settings for 
        @param dsDynamicBrowser INPUT DATASET The dataset with the layout of the browser                                                                             
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SaveSettings (pcSettingsKey AS CHARACTER,
                                     DATASET FOR dsDynamicBrowser):
  
END INTERFACE.
