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
    File        : IToolImageProvider
    Purpose     : Interface used for customer ImageProviders
    Syntax      : 
    Description : A ToolImageProvider is a service that can be used to 
                  assign custom images (and images of different resolutions
                  to a ButtonTool if required, e.g. Ribbon buttons). It 
                  can be used to assign a custom empty image to tools 
                  where the ABL Widget (Button/Menu item) did not have 
                  an image or to assign standard Images to buttons like
                  Ok/Cancel/Close ...  
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Aug 24 11:49:54 CEST 2011
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

INTERFACE Consultingwerk.WindowIntegrationKit.IToolImageProvider:  

    /*------------------------------------------------------------------------------
        Purpose: Verifies if the Image should be reloaded during UltraToolbarsHelper:RefreshTools                                                                   
        Notes:                                                                        
        @param phWidget The handle of the menu-item or button widget 
        @param poButtonTool The reference to the Infragistics ButtonTool that relaces the widget
        @return True when an Image needs to be reloaded, oterwise False
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL CheckReloadImage (phWidget AS HANDLE, 
                                            poButtonTool AS Infragistics.Win.UltraWinToolbars.ToolBase).

    /*------------------------------------------------------------------------------
        Purpose: Customized routine to load an image to a Tool                                                                      
        Notes:   Can be customized using an IToolImageProvider Implementation     
                 The LOGICAL RETURN value indicates if an image has been loaded and
                 influences the Tool PreferredSizeOnRibbon                                                                    
        @param phWidget The handle of the menu-item or button widget 
        @param poButtonTool The reference to the Infragistics ButtonTool that relaces the widget
        @return True when an Image was loaded, otherwise False
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL LoadImage (phWidget AS HANDLE, 
                                     poButtonTool AS Infragistics.Win.UltraWinToolbars.ToolBase) .  
  
END INTERFACE.