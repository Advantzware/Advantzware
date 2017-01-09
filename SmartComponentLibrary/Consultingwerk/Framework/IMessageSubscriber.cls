/**********************************************************************
 * Copyright (C) 2006-2012 by Consultingwerk Ltd. ("CW") -            *
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
    File        : IMessageSubscriber
    Purpose     : Interface for classes subscribing to messages from the 
                  IMessagePublisher service
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Dec 04 17:41:12 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

INTERFACE Consultingwerk.Framework.IMessageSubscriber:  

	/*------------------------------------------------------------------------------
	    Purpose: Callback method invoked when messages are distributed/published
	             to subscribers
	    Notes:
	    @param poMessage The reference to the message object
	------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID MessageCallback (poMessage AS Progress.Lang.Object).

END INTERFACE.
