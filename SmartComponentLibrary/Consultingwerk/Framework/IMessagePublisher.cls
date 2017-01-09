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
    File        : IMessagePublisher
    Purpose     : Service interface for classes that provide the message
                  publishing functionality 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Dec 04 17:45:22 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Framework.* FROM PROPATH.

INTERFACE Consultingwerk.Framework.IMessagePublisher:  

	/*------------------------------------------------------------------------------
	    Purpose: Publishes a Message to all subscriber that are subscribed to messages
	             of that type
	    Notes:   Subscription is for the actual message type and child classes
	    @param poMessage The Message to publish
	------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID Publish (poMessage AS Progress.Lang.Object).

    /*------------------------------------------------------------------------------
        Purpose: Subscribes an Message handler call back to the given Message type 
        Notes:   Subscription is for the actual message type and child classes
        @param poCallback The reference to the callback instance
        @param poType The Message type to subscribe to
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Subscribe (poCallback AS IMessageSubscriber,
                                  poType AS Progress.Lang.Class).

    /*------------------------------------------------------------------------------
        Purpose: Unsubscribes an Message handler call back to the given Message type 
        Notes:
        @param poCallback The reference to the callback instance
        @param poType The Message type to unsubscribe from
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Unsubscribe (poCallback AS IMessageSubscriber,
                                    poType AS Progress.Lang.Class) .
                                    
END INTERFACE.
