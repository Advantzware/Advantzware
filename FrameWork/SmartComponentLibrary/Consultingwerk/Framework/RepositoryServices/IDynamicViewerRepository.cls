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
    File        : IDynamicViewerRepository
    Purpose     : Interface definition for classes that can load and store 
                  the design of a dynamic viewer
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Apr 01 16:09:06 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

INTERFACE Consultingwerk.Framework.RepositoryServices.IDynamicViewerRepository:  
    
    {Consultingwerk/Framework/RepositoryServices/ttViewerDesign.i}
    
    /*------------------------------------------------------------------------------
        Purpose: Loads the Layout of the dynamic viewer.                                                                      
        Notes:                                                                          
        @param pcSettingsKey The key to retrieve the settings for
        @param ttViewerDesign OUTPUT TABLE The table that contains the viewer design                                                                              
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID LoadSettings (pcSettingsKey AS CHARACTER,
                                     OUTPUT TABLE ttViewerDesign) .
    
    /*------------------------------------------------------------------------------
        Purpose: Saves the Layout of the dynamic viewer.                                                                        
        Notes:
        @param pcSettingsKey The key to store the settings for 
        @param ttViewerDesign INPUT TABLE The table that contains the viewer design to be stored                                                                             
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SaveSettings (pcSettingsKey AS CHARACTER,
                                     TABLE ttViewerDesign):
    
END INTERFACE.