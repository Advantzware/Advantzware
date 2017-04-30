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
    File        : IReplacedWidgetCustomizer
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri May 25 13:38:20 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.WindowIntegrationKit.* FROM PROPATH .  
USING Progress.Lang.*                       FROM PROPATH.

INTERFACE Consultingwerk.WindowIntegrationKit.IReplacedWidgetCustomizer:  
  
    /*------------------------------------------------------------------------------
        Purpose: Provides customization capabilities
        Notes:   Used by Consultingwerk.Util.WidgetHelper:ReplaceWidget   
        @param poControl The reference to the generated .NET Control
        @param phWidget The handle of the original ABL Widget
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID AssignControlProperties (poControl AS System.Windows.Forms.Control,
                                                phWidget AS HANDLE).

    /*------------------------------------------------------------------------------
        Purpose: Provides customization capabilities
        Notes:   Used by Consultingwerk.Util.WidgetHelper:ReplaceWidget   
                 Allows loading a custom image. When this method is supposed to return
                 the custom image reference 
        @param poControl The reference to the generated .NET Control
        @param phWidget The handle of the original ABL Widget
        @return The reference to the custom image for the Control
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC System.Drawing.Image LoadCustomImage (poControl AS System.Windows.Forms.Control,
                                                        phWidget AS HANDLE).

    /*------------------------------------------------------------------------------
        Purpose: Provides customization capabilities
        Notes:   Used by Consultingwerk.Util.WidgetHelper:ReplaceWidget   
        @param poControl The reference to the generated .NET Control
        @param phWidget The handle of the original ABL Widget
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RefreshControlProperties (poControl AS System.Windows.Forms.Control,
                                                 phWidget AS HANDLE).


END INTERFACE.
