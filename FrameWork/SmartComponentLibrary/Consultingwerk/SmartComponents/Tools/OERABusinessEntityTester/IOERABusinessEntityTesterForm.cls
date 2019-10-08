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
    File        : IOERABusinessEntityTesterForm
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Mon Aug 03 09:20:36 CEST 2009
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.* FROM ASSEMBLY .

INTERFACE Consultingwerk.SmartComponents.Tools.OERABusinessEntityTester.IOERABusinessEntityTesterForm:  
  
    /*------------------------------------------------------------------------------
        Purpose: Set the tools (ToolButton and MenuItem) enabled.
        Notes:   
        @param pcToolName The name of the tool to enable
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID EnableTool (pcToolName AS CHARACTER).

    /*------------------------------------------------------------------------------
        Purpose: Set the tools (ToolButton and MenuItem) disabled.
        Notes:   
        @param pcToolName The name of the tool to disable
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DisableTool (pcToolName AS CHARACTER).
    
END INTERFACE.
