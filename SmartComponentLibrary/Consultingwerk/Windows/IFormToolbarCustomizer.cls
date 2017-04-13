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
    File        : IFormToolbarCustomizer
    Purpose     : Interface for services that allow the customization of
                  the UltraToolbarsManager instance that is owned by an
                  Consultingwerk.Forms.BaseForm instance
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Aug 16 21:16:09 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Windows.*            FROM PROPATH .  
USING Infragistics.Win.UltraWinToolbars.* FROM ASSEMBLY  . 
USING Progress.Lang.*                     FROM PROPATH .

INTERFACE Consultingwerk.Windows.IFormToolbarCustomizer:  

    /*------------------------------------------------------------------------------
        Purpose: Customizes a UltraToolbarsManager instance during OnLoad of a BaseForm
                 instance 
        Notes:   
        @param poToolbarsManager The UltraToolbarsManager instance to customizer
        @param poOwningForm The owning Form 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID CustomizeToolbar (poToolbarsManager AS UltraToolbarsManager,
                                         poOwningForm AS System.Windows.Forms.Form).

END INTERFACE.
