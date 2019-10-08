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
    File        : IBusinessEntityDesignerPlugin
    Purpose     : Interface describing methods required for Plugins 
                  into the Business Entity Designer application
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Mar 25 23:25:21 CET 2011
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.BusinessEntityDesigner.Services.* .
USING Consultingwerk.BusinessEntityDesigner.UI.* .
USING Infragistics.Win.UltraWinToolbars.* .
USING Progress.Lang.* .

INTERFACE Consultingwerk.BusinessEntityDesigner.IBusinessEntityDesignerPlugin:  
  
    { Consultingwerk/BusinessEntityDesigner/Services/dsBusinessEntity.i &REFERENCE-ONLY="REFERENCE-ONLY" }
  
    /*------------------------------------------------------------------------------
        Purpose: Method called when the Business Entity Designer is starting and 
                 has loaded the plugin.                                                                      
        Notes:   Passes references to some key components of the Designer to 
                 the plugin, use this to initialize the plugin, subscribe to events                                                                     
        @param dsBusinessEntity Business Entity with the Design dataset, called with the BIND keyword
        @param poController The BusinessEntityDatasetController
        @param poForm The main Form of the BusinessEntityDesigner
        @param poToolbar The reference to the UltraToolbarsManager used by the BusinessEntityDesigner                                                                     
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Startup (DATASET dsBusinessEntity BIND,
                                poController AS BusinessEntityDatasetController,
                                poForm AS BusinessEntityDesignerForm,
                                poToolbar AS UltraToolbarsManager) .
  
    /*------------------------------------------------------------------------------
        Purpose: Method called when the Business Entity Designer will get closed                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Shutdown () .
  
END INTERFACE.