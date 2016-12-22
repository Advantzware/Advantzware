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
    File        : MessagePublisher
    Purpose     : Default IMessagePublisher implementation
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Dec 04 17:44:51 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                      FROM PROPATH .
USING Consultingwerk.Assertion.ObjectAssert FROM PROPATH .
USING Consultingwerk.Framework.*            FROM PROPATH .
USING Consultingwerk.Util.*                 FROM PROPATH .
USING Progress.Lang.*                       FROM PROPATH .

CLASS Consultingwerk.Framework.MessagePublisher
    IMPLEMENTS IMessagePublisher: 

    DEFINE TEMP-TABLE ttSubscriber NO-UNDO 
        FIELD MessageType AS Progress.Lang.Object
        FIELD Subscriber AS Progress.Lang.Object 
        INDEX MessageType MessageType Subscriber.

    /*------------------------------------------------------------------------------
        Purpose: Publishes a Message to all subscriber that are subscribed to messages
                 of that type
        Notes:   Subscription is for the actual message type and child classes
        @param poMessage The Message to publish
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Publish (poMessage AS Progress.Lang.Object):

        DEFINE VARIABLE oType       AS Progress.Lang.Class NO-UNDO . 
        DEFINE VARIABLE oSubscriber AS IMessageSubscriber  NO-UNDO . 
        
        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poMessage """poMessage"":U"} .
        
        messageloop: FOR EACH ttSubscriber ON ERROR UNDO, THROW:

            oType = CAST (ttSubscriber.MessageType, Progress.Lang.Class) .
            
            IF poMessage:GetClass():IsA (oType) THEN DO ON ERROR UNDO, THROW:
                
                oSubscriber = CAST (ttSubscriber.Subscriber, IMessageSubscriber) . 
                oSubscriber:MessageCallback (poMessage) .
                
                IF VALID-OBJECT (poMessage) AND TYPE-OF (poMessage, ICancelableMessage) AND CAST (poMessage, ICancelableMessage):Cancel THEN 
                    LEAVE messageloop . 
            END.
        END.
    END.

    /*------------------------------------------------------------------------------
        Purpose: Subscribes an Message handler call back to the given Message type 
        Notes:   Subscription is for the actual message type and child classes
        @param poCallback The reference to the callback instance
        @param poType The Message type to subscribe to
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Subscribe (poCallback AS IMessageSubscriber,
                                  poType AS Progress.Lang.Class):

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poCallback """poCallback"":U"} .
        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poType """poType"":U"} .

        IF CAN-FIND (ttSubscriber WHERE ttSubscriber.MessageType = poType 
                                    AND ttSubscriber.Subscriber  = poCallback) THEN
            RETURN . 
            
        CREATE ttSubscriber .
        ASSIGN ttSubscriber.MessageType = poType 
               ttSubscriber.Subscriber  = poCallback
               .                                                                              
    END.                                       

    /*------------------------------------------------------------------------------
        Purpose: Unsubscribes an Message handler call back to the given Message type 
        Notes:
        @param poCallback The reference to the callback instance
        @param poType The Message type to unsubscribe from
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Unsubscribe (poCallback AS IMessageSubscriber,
                                    poType AS Progress.Lang.Class):

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poCallback """poCallback"":U"} .
        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poType """poType"":U"} .

        FIND ttSubscriber WHERE ttSubscriber.MessageType = poType 
                            AND ttSubscriber.Subscriber  = poCallback NO-ERROR . 
                           
        IF AVAILABLE ttSubscriber THEN 
            DELETE ttSubscriber .                          
    END.                                       

END CLASS.
