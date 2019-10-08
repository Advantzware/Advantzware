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
    File        : IMessageInteractionService
    Purpose     : Service Interface for services that provide message interaction
                  between business logic and consumer
    Syntax      :
    Description : The IMessageInteractionService is used from the business
                  logic to register messages (questions) that should be
                  answered from the consumer (user) and provides access
                  to already given answers using the same method call
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Sep 01 09:04:00 CEST 2015
    Notes       : SCL-996
  ----------------------------------------------------------------------*/

USING Consultingwerk.Framework.MessageInteraction.* FROM PROPATH .

INTERFACE Consultingwerk.Framework.MessageInteraction.IMessageInteractionService:

    /**
     * Purpose: Adds a question to be answered to the list of questions or returns
     *          an already given reply
     * Notes:   The pcMessageId parameter is mandatory, the pcMessageText and poMessageButtons
     *          are mandatory when the message needs to be created
     *          Returns MessageReplyEnum:Unanswered when the question as not yet answered,
     *          indicating that the calling code should stop and return to the caller
     *          where the messages should be shown to the user
     * @param poQuestions The List of Questions to add the question to (mandatory)
     * @param pcMessageId The message id, representing the location of the message in the source code (e.g. a guid, mandatory)
     * @param pcMessageContext The message context, e.g. to distingish an iteration of a loop of code (message id stays same in this case)
     * @param pcMessageText The text for the message
     * @param pcMessageTitle The title for the message
     * @param pcMessageDetail The message detail to show to the users
     * @param poMessageButtons The combination of message buttons
     * @param poDefaultReply The default message button
     * @return The message reply provided by the user or Unanswered when the user has not yet answered the question
     */
    METHOD PUBLIC MessageReplyEnum AskQuestion (poQuestions AS ListQuestion,
                                                pcMessageId AS CHARACTER,
                                                pcMessageContext AS CHARACTER,
                                                pcMessageText AS CHARACTER,
                                                pcMessageTitle AS CHARACTER,
                                                pcMessageDetail AS CHARACTER,
                                                poMessageButtons AS MessageButtons,
                                                poDefaultReply AS MessageReplyEnum) .

END INTERFACE.
