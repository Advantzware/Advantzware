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
    File        : LogManagerMessageHandler
    Purpose     : Default ILogMessageHandler implementation, writes to the 
                  client log using the Consultingwerk.Util.LogManager
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Feb 13 17:12:25 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.ILogMessageHandler FROM PROPATH .
USING Consultingwerk.Util.*                       FROM PROPATH . 
USING Progress.Lang.*                             FROM PROPATH .

CLASS Consultingwerk.Framework.LogManagerMessageHandler 
    IMPLEMENTS ILogMessageHandler: 

    /*------------------------------------------------------------------------------
        Purpose: Handles a log message (writes it to a log)                                                                       
        Notes:                                                                        
        @param pcMessage The message to write to a log or console
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID LogMessage (pcMessage AS CHARACTER):
        
        LogManager:WriteMessage (pcMessage) .

    END METHOD.

END CLASS.