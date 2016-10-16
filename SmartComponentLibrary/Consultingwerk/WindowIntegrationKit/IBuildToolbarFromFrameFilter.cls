/**********************************************************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
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
    File        : IBuildToolbarFromFrameFilter
    Purpose     : Allows to filter the UltraToolbarsHelper:BuildRibbonGroupFromFrame
                  method on a per Widget base
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Mar 14 17:54:20 CET 2016
    Notes       : 
  ----------------------------------------------------------------------*/
USING Consultingwerk.WindowIntegrationKit.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH.

INTERFACE Consultingwerk.WindowIntegrationKit.IBuildToolbarFromFrameFilter:  

    /*------------------------------------------------------------------------------
       Purpose: Allows filtering out widgets/tools from the UltraToolbarsHelper:BuildToolbarFromFrame
                API functionality
       Notes:
       @param phWidget The handle of the represented ABL widget (menu item, button, ...)
       @return Logical value indicating if the Widget should be filtered out or not (FALSE will skip the current widget)
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL FilterTool (phWidget AS HANDLE).
      
END INTERFACE.
