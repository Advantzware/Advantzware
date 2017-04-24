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
    File        : MessageReplyEnum
    Purpose     : Enumeration of Message replies
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Aug 31 23:59:13 CET 2015
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                              FROM PROPATH .
USING Consultingwerk.Framework.Collections.*        FROM PROPATH .
USING Consultingwerk.Framework.MessageInteraction.* FROM PROPATH .
USING Progress.Lang.*                               FROM PROPATH .

&IF DEFINED (NativeEnums) NE 0 &THEN
ENUM Consultingwerk.Framework.MessageInteraction.MessageReplyEnum:

    DEFINE ENUM Unanswered ReplyYes ReplyNo ReplyOk ReplyCancel .

END ENUM .

&ELSE
CLASS Consultingwerk.Framework.MessageInteraction.MessageReplyEnum
    INHERITS Consultingwerk.Enum:

    {Consultingwerk/EnumMember.i Unanswered 0 MessageReplyEnum}
    {Consultingwerk/EnumMember.i ReplyYes 1 MessageReplyEnum}
    {Consultingwerk/EnumMember.i ReplyNo 2 MessageReplyEnum}
    {Consultingwerk/EnumMember.i ReplyOk 3 MessageReplyEnum}
    {Consultingwerk/EnumMember.i ReplyCancel 4 MessageReplyEnum}

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the MessageReplyEnum members
        Notes:
        @param piValue The internal (numeric) representation of the Enumeration member
        @param pcLabel The text label of the Enumaration member
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE MessageReplyEnum (piValue AS INTEGER, pcLabel AS CHARACTER):
        SUPER ().

        ASSIGN THIS-OBJECT:Value = piValue
               THIS-OBJECT:Label = pcLabel .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Returns a CharacterList with the names of the Enum members
        Notes:
        @return The CharacterList with the names of the Enum members
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CharacterList GetNames ():

        RETURN NEW CharacterList ('{&EnumMembers}':U).

    END METHOD .

    {Consultingwerk/EnumFromString.i Consultingwerk.Framework.MessageInteraction.MessageReplyEnum}

END CLASS .
&ENDIF
