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
    File        : ToolClickEventArgs
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun May 27 23:10:53 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.EventArgs     FROM PROPATH .
USING Consultingwerk.Web.Widgets.* FROM PROPATH .  
USING Progress.Lang.*              FROM PROPATH .

CLASS Consultingwerk.Web.Widgets.ToolClickEventArgs 
    INHERITS EventArgs: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the Tool that was clicked 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Tool AS Consultingwerk.Web.Widgets.ToolBase NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ToolClickEventArgs class                                                                      
        Notes:                                               
        @param poTool The ToolBase instance that has been clicked                         
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ToolClickEventArgs (poTool AS ToolBase):
        SUPER ().
        
        THIS-OBJECT:Tool = poTool . 
        
    END CONSTRUCTOR.

END CLASS.
