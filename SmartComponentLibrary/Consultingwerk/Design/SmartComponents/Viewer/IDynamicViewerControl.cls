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
    File        : IDynamicViewerControl
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Nov 15 06:47:33 CET 2010
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

INTERFACE Consultingwerk.Design.SmartComponents.Viewer.IDynamicViewerControl:  
  
    /*------------------------------------------------------------------------------
        Purpose: Closes the Designer                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID CloseDesigner ().

    /*------------------------------------------------------------------------------
        Purpose: Saves the current design using the SaveSettings method and closes
                 the Visual Designer                                                                       
        Notes:          
        @param poTemplateControl The reference to the Visual Designer root control
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SaveDesigner (poTemplateControl AS System.Windows.Forms.Control).

END INTERFACE.