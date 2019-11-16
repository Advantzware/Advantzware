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
    File        : UnnamedWidgetHandler
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Nov 25 19:43:36 CET 2011
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.WindowIntegrationKit.* FROM PROPATH .
USING Infragistics.Win.UltraWinToolbars.*   FROM ASSEMBLY .
USING Progress.Lang.*                       FROM PROPATH .

CLASS Consultingwerk.WindowIntegrationKit.UnnamedWidgetHandler 
    IMPLEMENTS IUnnamedWidgetHandler: 

    /*------------------------------------------------------------------------------
        Purpose: Handles unnamed widgets found during the rendering of menu's and
                 toolbars to UltraToolbarsHandler                                                                     
        Notes:   This is the default implementation, that will just return an error
        @param phWidget The unnamd widget that needs to be handled
        @param phProcedure The owning procedure handle; may be used for call backs
        @param poToolbarsManager The owning UltraToolbarsManager instance (where the ButtonTool should have been created)
        @return The tool name (key) to use for the unnamed widget
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER HandleUnnamedWidget (phWidget AS HANDLE, 
                                                 phProcedure AS HANDLE, 
                                                 poToolbarsManager AS UltraToolbarsManager ):
        
        UNDO, THROW NEW AppError (SUBSTITUTE ("Unable to render widget as Infragistics ButtonTool. Name may not be empty.&1"{&TRAN} +
                                              "Widget-Type: &5&1Dynamic Widget: &2&1Creator: &3&1Label: &4"{&TRAN},
                                              System.Environment:NewLine,
                                              phWidget:DYNAMIC,
                                              (IF VALID-HANDLE (phWidget:INSTANTIATING-PROCEDURE) THEN phWidget:INSTANTIATING-PROCEDURE:FILE-NAME ELSE "?"{&TRAN}),
                                              phWidget:LABEL,
                                              phWidget:TYPE), 0) .

    END METHOD.

END CLASS.