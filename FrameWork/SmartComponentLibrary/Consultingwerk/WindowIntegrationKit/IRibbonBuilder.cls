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
    File        : IRibbonBuilder
    Purpose     : Service class to turn a Toolbar definition into a Ribbon
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Feb 28 15:46:20 CET 2012
    Notes       : Used in the WinKit after the UltraToolbarsHelper routines 
                  have built Infragistics Toolbars and menus to turn 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

INTERFACE Consultingwerk.WindowIntegrationKit.IRibbonBuilder:  
  
    /*------------------------------------------------------------------------------
        Purpose: Builds a Ribbon for a toolbar manager instance          
        Notes:   Used in the WinKit after the UltraToolbarsHelper routines have built 
                 Infragistics Toolbars and menus to turn                                                  
        @param poToolbarsManager The Infragistics.Win.UltraWinToolbars.UltraToolbarsManager instance to build a Ribbon for
        @param phCallbackProcedure The PROCEDURE to use for callbacks                      
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID BuildRibbonForToolbar (poToolbarsManager AS Infragistics.Win.UltraWinToolbars.UltraToolbarsManager,
                                              phCallbackProcedure AS HANDLE).

END INTERFACE.