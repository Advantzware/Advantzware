/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : MessageInteractionService
    Purpose     : Default service that provide message interaction between 
                  business logic and consumer
    Syntax      : 
    Description : The MessageInteractionService is used from the business
                  logic (through the ServiceContainer) to register messages 
                  (questions) that should be answered from the consumer 
                  (user) and provides access to already given answers 
                  using the same method call
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Sep 01 09:08:25 CEST 2015
    Notes       : SCL-996 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Assertion.*                    FROM PROPATH .
USING Consultingwerk.Framework.MessageInteraction.* FROM PROPATH .  
USING Progress.Lang.*                               FROM PROPATH .

CLASS Consultingwerk.Framework.MessageInteraction.MessageInteractionService
    IMPLEMENTS IMessageInteractionService: 

    /*------------------------------------------------------------------------------
        Purpose: Adds a question to be answered to the list of questions or returns
                 and already given reply 
        Notes:   The pcMessageId parameter is mandatory, the pcMessageText and poMessageButtons
                 are mandatory when the message needs to be created
                 Returns MessageReplyEnum:Unanswered when the question as not yet answered, 
                 indicating that the calling code should stop and return to the caller
                 where the messages should be shown to the user
        @param poQuestions The List of Questions to add the question to (mandatory)        
        @param pcMessageId The message id, representing the location of the message in the source code (e.g. a guid, mandatory)
        @param pcMessageContext The message context, e.g. to distingish an iteration of a loop of code (message id stays same in this case)
        @param pcMessageText The text for the message
        @param pcMessageTitle The title for the message
        @param pcMessageDetail The message detail to show to the users
        @param poMessageButtons The combination of message buttons
        @param poDefaultReply The default message button
        @return The message reply provided by the user or Unanswered when the user has not yet answered the question 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC MessageReplyEnum AskQuestion (poQuestions AS ListQuestion,
                                                pcMessageId AS CHARACTER,
                                                pcMessageContext AS CHARACTER,
                                                pcMessageText AS CHARACTER,
                                                pcMessageTitle AS CHARACTER, 
                                                pcMessageDetail AS CHARACTER,
                                                poMessageButtons AS MessageButtons,
                                                poDefaultReply AS MessageReplyEnum):
        
        Assert:NotNullOrEmpty (pcMessageId, "pcMessageId":U) .
        ObjectAssert:IsValid (poQuestions, "poQuestions":U) .
                     
        /* 1. Check if we already have a reply for this question */
        {Consultingwerk/foreachABL.i Question oQuestion in poQuestions}
            IF oQuestion:MessageId      = pcMessageId AND 
               oQuestion:MessageContext = pcMessageContext THEN  
                
                RETURN oQuestion:MessageReply .
        END.

        Assert:NotNullOrEmpty (pcMessageText, "pcMessageText":U) .
        ObjectAssert:IsValid (poMessageButtons, "poMessageButtons":U) .

        oQuestion = poQuestions:Add (NEW Question ()) .  
        
        ASSIGN oQuestion:MessageTitle   = pcMessageTitle
               oQuestion:MessageText    = pcMessageText
               oQuestion:MessageDetail  = pcMessageDetail
               oQuestion:MessageButtons = poMessageButtons
               oQuestion:MessageReply   = MessageReplyEnum:Unanswered
               oQuestion:DefaultReply   = poDefaultReply 
               oQuestion:MessageID      = pcMessageID
               oQuestion:MessageContext = pcMessageContext .
    
        RETURN MessageReplyEnum:Unanswered . 

     END METHOD .

END CLASS.
