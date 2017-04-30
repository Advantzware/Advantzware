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
    File        : Label
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Apr 10 23:35:59 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Consultingwerk.Web.Widgets.Control.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.Web.Widgets.Label INHERITS Control: 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current screen value of the Label Control                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ScreenValue AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the Label class                                                                     
        Notes:                                                                        
        @param pcName The name for the Control
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC Label (pcName AS CHARACTER):
        SUPER (INPUT pcName).
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the Label class                                                                     
        Notes:                                          
        @param pcName The name for the Control
        @param pcScreenValue The screen-value for the Control                              
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC Label (pcName AS CHARACTER,
                              pcScreenValue AS CHARACTER):
        THIS-OBJECT (INPUT pcName).
        
        THIS-OBJECT:ScreenValue = pcScreenValue . 
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Performs output for this Control to the web stream                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID PerformOutput ():
        
        ASSIGN THIS-OBJECT:ScreenValue = RIGHT-TRIM (THIS-OBJECT:ScreenValue, ":":U) . 
        
        THIS-OBJECT:OUTPUT (SUBSTITUTE ('<label style="top: &2px; left: &3px">&1:</label>':U,
                                        THIS-OBJECT:HtmlEncode (THIS-OBJECT:ScreenValue),
                                        THIS-OBJECT:Top,
                                        THIS-OBJECT:Left), TRUE) .
        

    END METHOD.

END CLASS.