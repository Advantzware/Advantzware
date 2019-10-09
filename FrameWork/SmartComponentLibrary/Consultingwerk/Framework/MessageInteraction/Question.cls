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
    File        : Question
    Purpose     : Represents a single message raisd during execution of 
                  business logic on the backend 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Aug 31 23:53:51 CET 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                              FROM PROPATH .
USING Consultingwerk.Framework.MessageInteraction.* FROM PROPATH .  
USING Progress.Lang.*                               FROM PROPATH .

CLASS Consultingwerk.Framework.MessageInteraction.Question 
&IF '{&FrameworkSerializationType}' EQ 'XML' OR PROVERSION BEGINS '10.2':U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF
    :

    {Consultingwerk/JsonSerializableProperty.i MessageTitle CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i MessageText CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i MessageDetail CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i MessageButtons Consultingwerk.Framework.MessageInteraction.MessageButtons} .
    {Consultingwerk/JsonSerializableProperty.i MessageReply Consultingwerk.Framework.MessageInteraction.MessageReplyEnum} .
    {Consultingwerk/JsonSerializableProperty.i DefaultReply Consultingwerk.Framework.MessageInteraction.MessageReplyEnum} .
    {Consultingwerk/JsonSerializableProperty.i MessageID CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i MessageContext CHARACTER} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the Question class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC Question ():
        SUPER ().
        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .

        THIS-OBJECT:MessageReply = MessageReplyEnum:Unanswered .
        
    END CONSTRUCTOR. 

END CLASS.
