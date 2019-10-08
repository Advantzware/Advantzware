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
    File        : ILogMessageHandler
    Purpose     : Generic interface to classes handling log messages
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Feb 13 16:29:29 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

INTERFACE Consultingwerk.Framework.ILogMessageHandler:  
  
    /*------------------------------------------------------------------------------
        Purpose: Handles a log message (writes it to a log)                                                                       
        Notes:                                                                        
        @param pcMessage The message to write to a log or console
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID LogMessage (pcMessage AS CHARACTER) .

END INTERFACE .
