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
    File        : WaitStateEventArgs
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jun 17 10:43:36 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.EventArgs.

USING Consultingwerk.Framework.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.Framework.WaitStateChangedEventArgs INHERITS EventArgs: 

    /*------------------------------------------------------------------------------
        Purpose: Gets the action associated with the WaitState event
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Action AS WaitStateActionEnum NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets the message associated with the WaitState Change  
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Message AS CHARACTER NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the WaitStateChangedEventArgs class
        Notes:
        @param poAction The reference to the WaitStateActionEnum
        @param pcMessage The wait state message
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC WaitStateChangedEventArgs (poAction AS WaitStateActionEnum,
                                                  pcMessage AS CHARACTER):
        SUPER ().
        
        ASSIGN THIS-OBJECT:Action  = poAction
               THIS-OBJECT:Message = pcMessage .
        
    END CONSTRUCTOR.

END CLASS.
