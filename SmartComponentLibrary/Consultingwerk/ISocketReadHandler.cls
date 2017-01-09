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
    File        : ISocketReadHandler
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jan 02 19:54:05 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH.

INTERFACE Consultingwerk.ISocketReadHandler:  
  
    /*------------------------------------------------------------------------------
        Purpose: Data is available on a socket or the remote end of a connection has 
                 closed its socket; applies only to socket objects.
        Notes:   Invoked from the Socket READ-RESPONSE-PROCEDURE
                 The AVM invokes this procedure whenever it detects that data is 
                 available on the socket or that the remote end of the socket has 
                 closed its end of the socket. In this procedure, the SELF handle 
                 identifies the affected socket object.
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID ReadResponseHandler ().

END INTERFACE.