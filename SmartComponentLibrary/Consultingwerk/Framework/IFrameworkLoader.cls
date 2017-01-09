/**********************************************************************
 * Copyright (C) 2006-2012 by Consultingwerk Ltd. ("CW") -            *
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
    File        : IFrameworkLoader
    Purpose     : Provides framework initialization functionality
    Syntax      : 
    Description : Implement this interface to provide application specific
                  framework initialization. Register your own implementation
                  in the .applicationsettings file as the "frameworkLoader"
                  entry
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat May 04 23:45:17 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Framework.* FROM PROPATH . 
USING Progress.Lang.*            FROM PROPATH .

INTERFACE Consultingwerk.Framework.IFrameworkLoader:  
  
	/*------------------------------------------------------------------------------
		Purpose: Performs the framework initialization 																	  
		Notes:  									
		@param poConfigurationProvider The IConfigurationProvider with the configuration for the framework								  
	------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID Initialize (poConfigurationProvider AS IConfigurationProvider).

END INTERFACE.
