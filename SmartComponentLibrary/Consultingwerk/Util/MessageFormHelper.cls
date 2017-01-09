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
    File        : MessageFormHelper
    Purpose     : Various support routines for displaying messages
    Syntax      : Static methods only, Protected constructor to disallow
                  instance creation
    Description : See http://confluence.consultingwerkcloud.com/wiki/display/SCL/Using+custom+buttons+on+the+MessageForm
                  for instructions on how to add custom buttons to Message Form
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Jul 04 13:29:48 CEST 2009
    Notes       : This file is mostly intended for GUI for .NET only
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{ Consultingwerk/products.i }

USING Consultingwerk.Framework.*                    FROM PROPATH .
USING Consultingwerk.Framework.Enum.*               FROM PROPATH .
USING Consultingwerk.Framework.MessageInteraction.* FROM PROPATH .
USING Consultingwerk.SmartFramework.*               FROM PROPATH .
USING Consultingwerk.Util.*                         FROM PROPATH .
USING Consultingwerk.Util.Forms.*                   FROM PROPATH .
&IF DEFINED (DotNetAccessible) NE 0 &THEN
USING Consultingwerk.Windows.Util.Forms.*           FROM PROPATH .
USING Progress.Lang.*                               FROM ASSEMBLY .
USING System.Windows.Forms.*                        FROM ASSEMBLY .
&ENDIF

CLASS Consultingwerk.Util.MessageFormHelper:

    /**
     * Purpose: Protected default constructor.
     * Notes:   There's no need to create instances of the helper classes
     */
    CONSTRUCTOR PROTECTED MessageFormHelper ():
        SUPER ().

    END CONSTRUCTOR.

&IF DEFINED (DotNetAccessible) NE 0 &THEN
    /**
     * Purpose: The Parent for all MessageForms
     * Notes:
     */
    DEFINE PUBLIC STATIC PROPERTY MessageFormParent AS Progress.Windows.Form NO-UNDO
    GET.
    SET.
&ENDIF

    /**
     * Purpose: Gets and sets if the MessageForm should be set TopMost
     * Notes:   Default to FALSE
     */
    DEFINE PUBLIC STATIC PROPERTY MessageFormTopMost AS LOGICAL NO-UNDO INITIAL FALSE
    GET.
    SET.

    /**
     * Purpose: The type name of the Form to be used as the MessageForm
     * Notes:   The type needs to implement the
     *          Consultingwerk.Util.Forms.IMessageForm interface
     */
    DEFINE PUBLIC STATIC PROPERTY MessageFormType AS CHARACTER NO-UNDO
         INITIAL "Consultingwerk.Windows.Util.Forms.MessageForm":U
    GET.
    SET.

    /**
     * Purpose: Ask a question
     * Notes:   Uses the message dialog API in a simplified way
     * @param pcMessage The question text to display
     * @return The DialogResultEnum matching the response of the user
     */
    METHOD PUBLIC STATIC DialogResultEnum AskQuestion (pcMessage       AS CHARACTER):

        RETURN AskQuestion (pcMessage, "":U, "":U, LOGICAL (?)) .

    END METHOD .

&IF DEFINED (DotNetAccessible) NE 0 &THEN
    /**
     * Purpose: Ask a question
     * Notes:   Uses the message dialog API in a simplified way
     * @param pcMessage The question text to display
     * @param poParentForm The Form to parent the Message Dialog to
     * @return The DialogResultEnum matching the response of the user
     */
    METHOD PUBLIC STATIC DialogResultEnum AskQuestion (pcMessage       AS CHARACTER,
                                                       poParentForm    AS Progress.Windows.Form):

        DEFINE VARIABLE oOldParentForm AS Progress.Windows.Form NO-UNDO .

        ASSIGN oOldParentForm                      = MessageFormHelper:MessageFormParent .
               MessageFormHelper:MessageFormParent = poParentForm .

        RETURN AskQuestion (pcMessage, "":U, "":U, LOGICAL (?)) .

        FINALLY:
            MessageFormHelper:MessageFormParent = oOldParentForm .
        END FINALLY.

    END METHOD .
&ENDIF

    /**
     * Purpose: Ask a question
     * Notes:   Uses the message dialog API in a simplified way
     * @param pcMessage The question text to display
     * @param pcMessageTitle The Title of the message/question dialog
     * @return The DialogResultEnum matching the response of the user
     */
    METHOD PUBLIC STATIC DialogResultEnum AskQuestion (pcMessage       AS CHARACTER,
                                                       pcMessageTitle  AS CHARACTER):

        RETURN AskQuestion (pcMessage, pcMessageTitle, "":U, LOGICAL (?)) .

    END METHOD .

&IF DEFINED (DotNetAccessible) NE 0 &THEN
    /**
     * Purpose: Ask a question
     * Notes:   Uses the message dialog API in a simplified way
     * @param pcMessage The question text to display
     * @param pcMessageTitle The Title of the message/question dialog
     * @param poParentForm The Form to parent the Message Dialog to
     * @return The DialogResultEnum matching the response of the user
     */
    METHOD PUBLIC STATIC DialogResultEnum AskQuestion (pcMessage       AS CHARACTER,
                                                       pcMessageTitle  AS CHARACTER,
                                                       poParentForm    AS Progress.Windows.Form):

        DEFINE VARIABLE oOldParentForm AS Progress.Windows.Form NO-UNDO .

        ASSIGN oOldParentForm                      = MessageFormHelper:MessageFormParent .
               MessageFormHelper:MessageFormParent = poParentForm .

        RETURN AskQuestion (pcMessage, pcMessageTitle, "":U, LOGICAL (?)) .

        FINALLY:
            MessageFormHelper:MessageFormParent = oOldParentForm .
        END FINALLY.

    END METHOD .
&ENDIF

    /**
     * Purpose: Ask a question
     * Notes:   Uses the message dialog API in a simplified way
     * @param pcMessage The question text to display
     * @param pcMessageTitle The Title of the message/question dialog
     * @param pcMessageDetail The Details of the message/question that can be expanded by the user if needed
     * @return The DialogResultEnum matching the response of the user
     */
    METHOD PUBLIC STATIC DialogResultEnum AskQuestion (pcMessage       AS CHARACTER,
                                                       pcMessageTitle  AS CHARACTER,
                                                       pcMessageDetail AS CHARACTER):

        RETURN AskQuestion (pcMessage, pcMessageTitle, pcMessageDetail, LOGICAL (?)) .

    END METHOD .

&IF DEFINED (DotNetAccessible) NE 0 &THEN
    /**
     * Purpose: Ask a question
     * Notes:   Uses the message dialog API in a simplified way
     * @param pcMessage The question text to display
     * @param pcMessageTitle The Title of the message/question dialog
     * @param pcMessageDetail The Details of the message/question that can be expanded by the user if needed
     * @param poParentForm The Form to parent the Message Dialog to
     * @return The DialogResultEnum matching the response of the user
     */
    METHOD PUBLIC STATIC DialogResultEnum AskQuestion (pcMessage       AS CHARACTER,
                                                       pcMessageTitle  AS CHARACTER,
                                                       pcMessageDetail AS CHARACTER,
                                                       poParentForm    AS Progress.Windows.Form):

        DEFINE VARIABLE oOldParentForm AS Progress.Windows.Form NO-UNDO .

        ASSIGN oOldParentForm                      = MessageFormHelper:MessageFormParent .
               MessageFormHelper:MessageFormParent = poParentForm .

        RETURN AskQuestion (pcMessage, pcMessageTitle, pcMessageDetail, LOGICAL (?)) .

        FINALLY:
            MessageFormHelper:MessageFormParent = oOldParentForm .
        END FINALLY.

    END METHOD .
&ENDIF

    /**
     * Purpose: Ask a question to the user
     * Notes:   Uses the message dialog API in a simplified way
     * @param pcMessage The question text to display
     * @param pcMessageTitle The Title of the message/question dialog
     * @param pcMessageDetail The Details of the message/question that can be expanded by the user if needed
     * @param plDefault The logical default/initial answer for the question resulting in the default button
     * @return The DialogResultEnum matching the response of the user
     */
    METHOD PUBLIC STATIC DialogResultEnum AskQuestion (pcMessage       AS CHARACTER,
                                                       pcMessageTitle  AS CHARACTER,
                                                       pcMessageDetail AS CHARACTER,
                                                       plDefault       AS LOGICAL):

        DEFINE VARIABLE oImage AS Progress.Lang.Object NO-UNDO .
        DEFINE VARIABLE oDefault AS DialogResultEnum   NO-UNDO .

        CASE plDefault:
            WHEN TRUE THEN
                ASSIGN oDefault = DialogResultEnum:DialogResultYes .
            WHEN FALSE THEN
                ASSIGN oDefault = DialogResultEnum:DialogResultNo .
        END CASE .

&IF DEFINED (DotNetAccessible) NE 0 &THEN
        oImage = MessageFormImages:ImageQuestion .
&ENDIF
        RETURN ShowMessage (pcMessage,
                            pcMessageTitle,
                            pcMessageDetail,
                            oImage,
                            MessageFormDefaults:LabelYesNo,
                            MessageFormDefaults:GetDialogResultYesNo(),
                            oDefault,
                            0) .

    END METHOD .

&IF DEFINED (DotNetAccessible) NE 0 &THEN
    /**
     * Purpose: Ask a question to the user
     * Notes:   Uses the message dialog API in a simplified way
     * @param pcMessage The question text to display
     * @param pcMessageTitle The Title of the message/question dialog
     * @param pcMessageDetail The Details of the message/question that can be expanded by the user if needed
     * @param plDefault The logical default/initial answer for the question resulting in the default button
     * @param poParentForm The Form to parent the Message Dialog to
     * @return The DialogResultEnum matching the response of the user
     */
    METHOD PUBLIC STATIC DialogResultEnum AskQuestion (pcMessage       AS CHARACTER,
                                                       pcMessageTitle  AS CHARACTER,
                                                       pcMessageDetail AS CHARACTER,
                                                       plDefault       AS LOGICAL,
                                                       poParentForm    AS Progress.Windows.Form):

        DEFINE VARIABLE oOldParentForm AS Progress.Windows.Form NO-UNDO .

        ASSIGN oOldParentForm                      = MessageFormHelper:MessageFormParent .
               MessageFormHelper:MessageFormParent = poParentForm .

        RETURN AskQuestion (pcMessage, pcMessageTitle, pcMessageDetail, plDefault) .

        FINALLY:
            MessageFormHelper:MessageFormParent = oOldParentForm .
        END FINALLY.

    END METHOD .
&ENDIF

    /**
     * Purpose: Processes a list of questions
     * Notes:   Displays each unanswered question from the list to the user. Returns
     *          true when there were unanswered questions that are answered during the
     *          call and should be send to the server for further processing. Returns
     *          FALSE when no unanswered questions have been present
     * @param poQuestions The ListQuestion with the questions to process
     * @return Logical value indicating if questions have been answered or not (not unanswered questions were present)
     */
    METHOD PUBLIC STATIC LOGICAL ProcessQuestions (poQuestions AS ListQuestion):

        DEFINE VARIABLE lReturn       AS LOGICAL          NO-UNDO INITIAL FALSE .
        DEFINE VARIABLE oDialogResult AS DialogResultEnum NO-UNDO .
        DEFINE VARIABLE oDefault      AS DialogResultEnum NO-UNDO .

        {Consultingwerk/linqABL.i Question oQuestion in poQuestions
                                  where MessageReply = MessageReplyEnum:Unanswered}

            ASSIGN oDialogResult = ? .

            CASE oQuestion:MessageButtons:

                WHEN MessageButtons:Ok THEN DO:
                    MessageFormHelper:ShowMessage (oQuestion:MessageText,
                                                   oQuestion:MessageTitle,
                                                   oQuestion:MessageDetail,
&IF DEFINED (DotNetAccessible) NE 0 &THEN
                                                   MessageFormImages:ImageInfo,
&ELSE
                                                   ?,
&ENDIF
                                                   0) .

                    ASSIGN oQuestion:MessageReply = MessageReplyEnum:ReplyOk .
                END.
                WHEN MessageButtons:YesNo THEN DO:
                    oDialogResult = MessageFormHelper:AskQuestion (oQuestion:MessageText,
                                                                   oQuestion:MessageTitle,
                                                                   oQuestion:MessageDetail,
                                                                   (oQuestion:DefaultReply = MessageReplyEnum:ReplyYes)) .

                    IF oDialogResult = DialogResultEnum:DialogResultYes THEN
                        ASSIGN oQuestion:MessageReply = MessageReplyEnum:ReplyYes .
                    ELSE IF oDialogResult = DialogResultEnum:DialogResultNo THEN
                        ASSIGN oQuestion:MessageReply = MessageReplyEnum:ReplyNo .
                END.
                WHEN MessageButtons:YesNoCancel THEN DO:
                    IF oQuestion:DefaultReply = MessageReplyEnum:ReplyYes THEN
                        oDefault = DialogResultEnum:DialogResultYes .
                    ELSE IF oQuestion:DefaultReply = MessageReplyEnum:ReplyNo THEN
                        oDefault = DialogResultEnum:DialogResultNo .
                    ELSE IF oQuestion:DefaultReply = MessageReplyEnum:ReplyCancel THEN
                        oDefault = DialogResultEnum:DialogResultCancel .

                    oDialogResult = MessageFormHelper:ShowMessage (oQuestion:MessageText,
                                                                   oQuestion:MessageTitle,
                                                                   oQuestion:MessageDetail,
&IF DEFINED (DotNetAccessible) NE 0 &THEN
                                                                   MessageFormImages:ImageQuestion,
&ELSE
                                                                   ?,
&ENDIF
                                                                   MessageFormDefaults:LabelYesNoCancel,
                                                                   MessageFormDefaults:GetDialogResultYesNoCancel(),
                                                                   oDefault,
                                                                   0) .

                    IF oDialogResult = DialogResultEnum:DialogResultYes THEN
                        ASSIGN oQuestion:MessageReply = MessageReplyEnum:ReplyYes .
                    ELSE IF oDialogResult = DialogResultEnum:DialogResultNo THEN
                        ASSIGN oQuestion:MessageReply = MessageReplyEnum:ReplyNo .
                    ELSE IF oDialogResult = DialogResultEnum:DialogResultCancel THEN
                        ASSIGN oQuestion:MessageReply = MessageReplyEnum:ReplyCancel .
                END.
                WHEN MessageButtons:OkCancel THEN DO:
                    IF oQuestion:DefaultReply = MessageReplyEnum:ReplyOk THEN
                        oDefault = DialogResultEnum:DialogResultOk .
                    ELSE IF oQuestion:DefaultReply = MessageReplyEnum:ReplyCancel THEN
                        oDefault = DialogResultEnum:DialogResultCancel .

                    oDialogResult = MessageFormHelper:ShowMessage (oQuestion:MessageText,
                                                                   oQuestion:MessageTitle,
                                                                   oQuestion:MessageDetail,
&IF DEFINED (DotNetAccessible) NE 0 &THEN
                                                                   MessageFormImages:ImageQuestion,
&ELSE
                                                                   ?,
&ENDIF
                                                                   MessageFormDefaults:LabelOkCancel,
                                                                   MessageFormDefaults:GetDialogResultOkCancel(),
                                                                   oDefault,
                                                                   0) .

                    IF oDialogResult = DialogResultEnum:DialogResultOk THEN
                        ASSIGN oQuestion:MessageReply = MessageReplyEnum:ReplyOk .
                    ELSE IF oDialogResult = DialogResultEnum:DialogResultCancel THEN
                        ASSIGN oQuestion:MessageReply = MessageReplyEnum:ReplyCancel .
                END.

            END CASE .

            ASSIGN lReturn = TRUE .
        END.

        RETURN lReturn .

    END METHOD .

    /**
     * Purpose: Display Message based on a Message structure
     * Notes:   The parameter piMessageTimer is the (optional) message timeout in seconds.
     *          When specified the message form will close itself after the specified
     *          number of seconds (or when the user hits one of the buttons) and
     *          return the default DialogResultEnum.
     * @param poMessage The Message structure describing the message
     * @param piMessageTimer The Timer value for the Message Box in seconds
     * @return The DialogResultEnum indicating the button the user has pressed (useful for questions)
     */
    METHOD PUBLIC STATIC DialogResultEnum ShowMessage (poMessage AS IMessage,
                                                       piMessageTimer AS INTEGER):

        DEFINE VARIABLE cTitle                      AS CHARACTER            NO-UNDO.
        DEFINE VARIABLE oMessageImage               AS Progress.Lang.Object NO-UNDO .
        DEFINE VARIABLE cMessageButtons             AS CHARACTER            NO-UNDO EXTENT .
        DEFINE VARIABLE oMessageButtonDialogResults AS DialogResultEnum     NO-UNDO EXTENT .
        DEFINE VARIABLE oDefaultDialogResult        AS DialogResultEnum     NO-UNDO .

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poMessage """Message""{&TRAN}"} .

        CASE poMessage:Type:
            WHEN MessageTypeEnum:Error THEN
                ASSIGN cTitle = {Consultingwerk/translate.i &string="'Error':U"}
&IF DEFINED (DotNetAccessible) NE 0 &THEN
                       oMessageImage = MessageFormImages:ImageError
&ENDIF
                    .
            WHEN MessageTypeEnum:Information THEN
                ASSIGN cTitle = {Consultingwerk/translate.i &string="'Information':U"}
&IF DEFINED (DotNetAccessible) NE 0 &THEN
                       oMessageImage = MessageFormImages:ImageInfo
&ENDIF
                    .
            WHEN MessageTypeEnum:Question THEN
                ASSIGN cTitle = {Consultingwerk/translate.i &string="'Question':U"}
&IF DEFINED (DotNetAccessible) NE 0 &THEN
                       oMessageImage   = MessageFormImages:ImageQuestion
&ENDIF
                       cMessageButtons = MessageFormDefaults:LabelYesNo
                       oMessageButtonDialogResults = MessageFormDefaults:GetDialogResultYesNo()
                       oDefaultDialogResult        = DialogResultEnum:DialogResultNo
                    .
            WHEN MessageTypeEnum:Warning THEN
                ASSIGN cTitle = {Consultingwerk/translate.i &string="'Warning':U"}
&IF DEFINED (DotNetAccessible) NE 0 &THEN
                       oMessageImage = MessageFormImages:ImageWarning
&ENDIF
                    .
        END CASE.

        RETURN MessageFormHelper:ShowMessage (poMessage:Text,
                                              cTitle,
                                              poMessage:Detail,
                                              oMessageImage,
                                              cMessageButtons,
                                              oMessageButtonDialogResults,
                                              oDefaultDialogResult,
                                              piMessageTimer) .
    END METHOD .

    /**
     * Purpose: Display Message based on a Message structure
     * Notes:   The parameter piMessageTimer is the (optional) message timeout in seconds.
     *          When specified the message form will close itself after the specified
     *          number of seconds (or when the user hits one of the buttons) and
     *          return the default DialogResultEnum.
     * @param poMessage The Message structure describing the message
     * @param pcMessageButton1 The label of the first message button
     * @param poMessageButtonDialogResult1 The DialogResultEnum value for the first message button
     * @param piMessageTimer The Timer value for the Message Box in seconds
     * @return The DialogResultEnum indicating the button the user has pressed (useful for questions)
     */
    METHOD PUBLIC STATIC DialogResultEnum ShowMessage (poMessage                    AS IMessage,
                                                       pcMessageButton1             AS CHARACTER,
                                                       poMessageButtonDialogResult1 AS DialogResultEnum,
                                                       piMessageTimer AS INTEGER):

        DEFINE VARIABLE cButtons       AS CHARACTER        EXTENT 1 NO-UNDO .
        DEFINE VARIABLE oDialogResults AS DialogResultEnum EXTENT 1 NO-UNDO .

        ASSIGN cButtons[1]       = pcMessageButton1
               oDialogResults[1] = poMessageButtonDialogResult1 .

        RETURN ShowMessage (poMessage,
                            cButtons,
                            oDialogResults,
                            poMessageButtonDialogResult1,
                            piMessageTimer) .

    END METHOD .

    /**
     * Purpose: Display Message based on a Message structure
     * Notes:   The parameter piMessageTimer is the (optional) message timeout in seconds.
     *          When specified the message form will close itself after the specified
     *          number of seconds (or when the user hits one of the buttons) and
     *          return the default DialogResultEnum.
     * @param poMessage The Message structure describing the message
     * @param pcMessageButton1 The label of the first message button
     * @param poMessageButtonDialogResult1 The DialogResultEnum value for the first message button
     * @param pcMessageButton2 The label of the second message button
     * @param poMessageButtonDialogResult2 The DialogResultEnum value for the second message button
     * @param poDefaultDialogResult The default DialogResultEnum value
     * @param piMessageTimer The Timer value for the Message Box in seconds
     * @return The DialogResultEnum indicating the button the user has pressed (useful for questions)
     */
    METHOD PUBLIC STATIC DialogResultEnum ShowMessage (poMessage                    AS IMessage,
                                                       pcMessageButton1             AS CHARACTER,
                                                       poMessageButtonDialogResult1 AS DialogResultEnum,
                                                       pcMessageButton2             AS CHARACTER,
                                                       poMessageButtonDialogResult2 AS DialogResultEnum,
                                                       poDefaultDialogResult        AS DialogResultEnum,
                                                       piMessageTimer AS INTEGER):

        DEFINE VARIABLE cButtons       AS CHARACTER        EXTENT 2 NO-UNDO .
        DEFINE VARIABLE oDialogResults AS DialogResultEnum EXTENT 2 NO-UNDO .

        ASSIGN cButtons[1]       = pcMessageButton1
               oDialogResults[1] = poMessageButtonDialogResult1
               cButtons[2]       = pcMessageButton2
               oDialogResults[2] = poMessageButtonDialogResult2 .

        RETURN ShowMessage (poMessage,
                            cButtons,
                            oDialogResults,
                            poDefaultDialogResult,
                            piMessageTimer) .

    END METHOD .

    /**
     * Purpose: Display Message based on a Message structure
     * Notes:   The parameter piMessageTimer is the (optional) message timeout in seconds.
     *          When specified the message form will close itself after the specified
     *          number of seconds (or when the user hits one of the buttons) and
     *          return the default DialogResultEnum.
     * @param poMessage The Message structure describing the message
     * @param pcMessageButton1 The label of the first message button
     * @param poMessageButtonDialogResult1 The DialogResultEnum value for the first message button
     * @param pcMessageButton2 The label of the second message button
     * @param poMessageButtonDialogResult2 The DialogResultEnum value for the second message button
     * @param pcMessageButton3 The label of the third message button
     * @param poMessageButtonDialogResult3 The DialogResultEnum value for the second message button
     * @param poDefaultDialogResult The default DialogResultEnum value
     * @param piMessageTimer The Timer value for the Message Box in seconds
     * @return The DialogResultEnum indicating the button the user has pressed (useful for questions)
     */
    METHOD PUBLIC STATIC DialogResultEnum ShowMessage (poMessage                    AS IMessage,
                                                       pcMessageButton1             AS CHARACTER,
                                                       poMessageButtonDialogResult1 AS DialogResultEnum,
                                                       pcMessageButton2             AS CHARACTER,
                                                       poMessageButtonDialogResult2 AS DialogResultEnum,
                                                       pcMessageButton3             AS CHARACTER,
                                                       poMessageButtonDialogResult3 AS DialogResultEnum,
                                                       poDefaultDialogResult        AS DialogResultEnum,
                                                       piMessageTimer AS INTEGER):

        DEFINE VARIABLE cButtons       AS CHARACTER        EXTENT 3 NO-UNDO .
        DEFINE VARIABLE oDialogResults AS DialogResultEnum EXTENT 3 NO-UNDO .

        ASSIGN cButtons[1]       = pcMessageButton1
               oDialogResults[1] = poMessageButtonDialogResult1
               cButtons[2]       = pcMessageButton2
               oDialogResults[2] = poMessageButtonDialogResult2
               cButtons[3]       = pcMessageButton3
               oDialogResults[3] = poMessageButtonDialogResult3 .

        RETURN ShowMessage (poMessage,
                            cButtons,
                            oDialogResults,
                            poDefaultDialogResult,
                            piMessageTimer) .

    END METHOD .

    /**
     * Purpose: Display Message based on a Message structure
     * Notes:   The parameter piMessageTimer is the (optional) message timeout in seconds.
     *          When specified the message form will close itself after the specified
     *          number of seconds (or when the user hits one of the buttons) and
     *          return the default DialogResultEnum.
     * @param poMessage The Message structure describing the message
     * @param pcMessageButtons The array of labels of the message buttons
     * @param poMessageButtonDialogResults The array of DialogResultEnum values for the message buttons
     * @param poDefaultDialogResult The default DialogResultEnum value
     * @param piMessageTimer The Timer value for the Message Box in seconds
     * @return The DialogResultEnum indicating the button the user has pressed (useful for questions)
     */
    METHOD PUBLIC STATIC DialogResultEnum ShowMessage (poMessage                    AS IMessage,
                                                       pcMessageButtons             AS CHARACTER EXTENT,
                                                       poMessageButtonDialogResults AS DialogResultEnum EXTENT,
                                                       poDefaultDialogResult        AS DialogResultEnum,
                                                       piMessageTimer AS INTEGER):

        DEFINE VARIABLE cTitle                      AS CHARACTER            NO-UNDO.
        DEFINE VARIABLE oMessageImage               AS Progress.Lang.Object NO-UNDO .
        DEFINE VARIABLE oDefaultDialogResult        AS DialogResultEnum     NO-UNDO .

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poMessage """Message""{&TRAN}"} .

        CASE poMessage:Type:
            WHEN MessageTypeEnum:Error THEN
                ASSIGN cTitle = {Consultingwerk/translate.i &string="'Error':U"}
&IF DEFINED (DotNetAccessible) NE 0 &THEN
                       oMessageImage = MessageFormImages:ImageError
&ENDIF
                    .
            WHEN MessageTypeEnum:Information THEN
                ASSIGN cTitle = {Consultingwerk/translate.i &string="'Information':U"}
&IF DEFINED (DotNetAccessible) NE 0 &THEN
                       oMessageImage = MessageFormImages:ImageInfo
&ENDIF
                    .
            WHEN MessageTypeEnum:Question THEN
                ASSIGN cTitle = {Consultingwerk/translate.i &string="'Question':U"}
&IF DEFINED (DotNetAccessible) NE 0 &THEN
                       oMessageImage   = MessageFormImages:ImageQuestion
&ENDIF
                       oDefaultDialogResult        = DialogResultEnum:DialogResultNo
                    .
            WHEN MessageTypeEnum:Warning THEN
                ASSIGN cTitle = {Consultingwerk/translate.i &string="'Warning':U"}
&IF DEFINED (DotNetAccessible) NE 0 &THEN
                       oMessageImage = MessageFormImages:ImageWarning
&ENDIF
                    .
        END CASE.

        RETURN MessageFormHelper:ShowMessage (poMessage:Text,
                                              cTitle,
                                              poMessage:Detail,
                                              oMessageImage,
                                              pcMessageButtons,
                                              poMessageButtonDialogResults,
                                              poDefaultDialogResult,
                                              piMessageTimer) .

    END METHOD .

    /**
     * Purpose: Display Message
     * Notes:   The parameter piMessageTimer is the (optional) message timeout in seconds.
     *          When specified the message form will close itself after the specified
     *          number of seconds (or when the user hits one of the buttons) and
     *          return the default DialogResultEnum.
     * @param pcMessage The message text
     * @param piMessageTimer The Timer value for the Message Box in seconds
     * @return The reference to the DialogResultEnum value representing the button pressed
     */
    METHOD PUBLIC STATIC DialogResultEnum ShowMessage (pcMessage AS CHARACTER,
                                                       piMessageTimer AS INTEGER):

        DEFINE VARIABLE c AS CHARACTER        EXTENT NO-UNDO .
        DEFINE VARIABLE o AS DialogResultEnum EXTENT NO-UNDO .

        RETURN ShowMessage (pcMessage, "":U, "":U, ?, c, o, ?, piMessageTimer) .

    END METHOD .

    /**
     * Purpose: Display Message
     * Notes:   The parameter piMessageTimer is the (optional) message timeout in seconds.
     *          When specified the message form will close itself after the specified
     *          number of seconds (or when the user hits one of the buttons) and
     *          return the default DialogResultEnum.
     * @param pcMessage The message text
     * @param poMessageImage The reference to the Image for the message
     * @param piMessageTimer The Timer value for the Message Box in seconds
     * @return The reference to the DialogResultEnum value representing the button pressed
     */
    METHOD PUBLIC STATIC DialogResultEnum ShowMessage (pcMessage       AS CHARACTER,
                                                       poMessageImage  AS Progress.Lang.Object,
                                                       piMessageTimer AS INTEGER):

        DEFINE VARIABLE c AS CHARACTER        EXTENT NO-UNDO .
        DEFINE VARIABLE o AS DialogResultEnum EXTENT NO-UNDO .

        RETURN ShowMessage (pcMessage, "":U, "":U, poMessageImage, c, o, ?, piMessageTimer) .

    END METHOD .

    /**
     * Purpose: Display Message
     * Notes:   The parameter piMessageTimer is the (optional) message timeout in seconds.
     *          When specified the message form will close itself after the specified
     *          number of seconds (or when the user hits one of the buttons) and
     *          return the default DialogResultEnum.
     * @param pcMessage The message text
     * @param pcMessageTitle The title of the message
     * @param piMessageTimer The Timer value for the Message Box in seconds
     * @return The reference to the DialogResultEnum value representing the button pressed
     */
    METHOD PUBLIC STATIC DialogResultEnum ShowMessage (pcMessage      AS CHARACTER,
                                                       pcMessageTitle AS CHARACTER,
                                                       piMessageTimer AS INTEGER):

        DEFINE VARIABLE c AS CHARACTER        EXTENT NO-UNDO .
        DEFINE VARIABLE o AS DialogResultEnum EXTENT NO-UNDO .

        RETURN ShowMessage (pcMessage, pcMessageTitle, "":U, ?, c, o, ?, piMessageTimer) .

    END METHOD .

    /**
     * Purpose: Display Message
     * Notes:   The parameter piMessageTimer is the (optional) message timeout in seconds.
     *          When specified the message form will close itself after the specified
     *          number of seconds (or when the user hits one of the buttons) and
     *          return the default DialogResultEnum.
     * @param pcMessage The message text
     * @param pcMessageTitle The title of the message
     * @param poMessageImage The reference to the Image for the message
     * @param piMessageTimer The Timer value for the Message Box in seconds
     * @return The reference to the DialogResultEnum value representing the button pressed
     */
    METHOD PUBLIC STATIC DialogResultEnum ShowMessage (pcMessage       AS CHARACTER,
                                                       pcMessageTitle  AS CHARACTER,
                                                       poMessageImage  AS Progress.Lang.Object,
                                                       piMessageTimer AS INTEGER):

        DEFINE VARIABLE c AS CHARACTER        EXTENT NO-UNDO .
        DEFINE VARIABLE o AS DialogResultEnum EXTENT NO-UNDO .

        RETURN ShowMessage (pcMessage, pcMessageTitle, "":U, poMessageImage, c, o, ?, piMessageTimer) .

    END METHOD .

    /**
     * Purpose: Display Message
     * Notes:   The parameter piMessageTimer is the (optional) message timeout in seconds.
     *          When specified the message form will close itself after the specified
     *          number of seconds (or when the user hits one of the buttons) and
     *          return the default DialogResultEnum.
     * @param pcMessage The message text
     * @param pcMessageTitle The title of the message
     * @param pcMessageDetail The detail text for the message
     * @param piMessageTimer The Timer value for the Message Box in seconds
     * @return The reference to the DialogResultEnum value representing the button pressed
     */
    METHOD PUBLIC STATIC DialogResultEnum ShowMessage (pcMessage       AS CHARACTER,
                                                       pcMessageTitle  AS CHARACTER,
                                                       pcMessageDetail AS CHARACTER,
                                                       piMessageTimer AS INTEGER):

        DEFINE VARIABLE c AS CHARACTER        EXTENT NO-UNDO .
        DEFINE VARIABLE o AS DialogResultEnum EXTENT NO-UNDO .

        RETURN ShowMessage (pcMessage, pcMessageTitle, pcMessageDetail, ?, c, o, ?, piMessageTimer) .

    END METHOD .

    /**
     * Purpose: Display Message
     * Notes:   The parameter piMessageTimer is the (optional) message timeout in seconds.
     *          When specified the message form will close itself after the specified
     *          number of seconds (or when the user hits one of the buttons) and
     *          return the default DialogResultEnum.
     * @param pcMessage The message text
     * @param pcMessageTitle The title of the message
     * @param pcMessageDetail The detail text for the message
     * @param poMessageImage The reference to the Image for the message
     * @param piMessageTimer The Timer value for the Message Box in seconds
     * @return The reference to the DialogResultEnum value representing the button pressed
     */
    METHOD PUBLIC STATIC DialogResultEnum ShowMessage (pcMessage       AS CHARACTER,
                                                       pcMessageTitle  AS CHARACTER,
                                                       pcMessageDetail AS CHARACTER,
                                                       poMessageImage  AS Progress.Lang.Object,
                                                       piMessageTimer AS INTEGER):

        DEFINE VARIABLE c AS CHARACTER        EXTENT NO-UNDO .
        DEFINE VARIABLE o AS DialogResultEnum EXTENT NO-UNDO .

        RETURN ShowMessage (pcMessage, pcMessageTitle, pcMessageDetail, poMessageImage, c, o, ?, piMessageTimer) .

    END METHOD .

    /**
     * Purpose: Display Message
     * Notes:   The parameter piMessageTimer is the (optional) message timeout in seconds.
     *          When specified the message form will close itself after the specified
     *          number of seconds (or when the user hits one of the buttons) and
     *          return the default DialogResultEnum.
     * @param pcMessage The message text
     * @param pcMessageTitle The title of the message
     * @param pcMessageDetail The detail text for the message
     * @param poMessageImage The reference to the Image for the message
     * @param pcMessageButton1 The label of the first message button
     * @param poMessageButtonDialogResult1 The DialogResultEnum value for the first message button
     * @param piMessageTimer The Timer value for the Message Box in seconds
     * @return The reference to the DialogResultEnum value representing the button pressed
     */
    METHOD PUBLIC STATIC DialogResultEnum ShowMessage (pcMessage                    AS CHARACTER,
                                                       pcMessageTitle               AS CHARACTER,
                                                       pcMessageDetail              AS CHARACTER,
                                                       poMessageImage               AS Progress.Lang.Object,
                                                       pcMessageButton1             AS CHARACTER,
                                                       poMessageButtonDialogResult1 AS DialogResultEnum,
                                                       piMessageTimer AS INTEGER):

        DEFINE VARIABLE cButtons AS CHARACTER        NO-UNDO EXTENT 1.
        DEFINE VARIABLE oResults AS DialogResultEnum NO-UNDO EXTENT 1.

        ASSIGN cButtons[1] = pcMessageButton1
               oResults[1] = poMessageButtonDialogResult1 .

        RETURN ShowMessage (pcMessage, pcMessageTitle, pcMessageDetail,
                            poMessageImage, cButtons, oResults, ?, piMessageTimer) .

    END METHOD .

    /**
     * Purpose: Display Message
     * Notes:   The parameter piMessageTimer is the (optional) message timeout in seconds.
     *          When specified the message form will close itself after the specified
     *          number of seconds (or when the user hits one of the buttons) and
     *          return the default DialogResultEnum.
     * @param pcMessage The message text
     * @param pcMessageTitle The title of the message
     * @param pcMessageDetail The detail text for the message
     * @param poMessageImage The reference to the Image for the message
     * @param pcMessageButton1 The label of the first message button
     * @param poMessageButtonDialogResult1 The DialogResultEnum value for the first message button
     * @param pcMessageButton2 The label of the second message button
     * @param poMessageButtonDialogResult2 The DialogResultEnum value for the second message button
     * @param piMessageTimer The Timer value for the Message Box in seconds
     * @return The reference to the DialogResultEnum value representing the button pressed
     */
    METHOD PUBLIC STATIC DialogResultEnum ShowMessage (pcMessage                    AS CHARACTER,
                                                       pcMessageTitle               AS CHARACTER,
                                                       pcMessageDetail              AS CHARACTER,
                                                       poMessageImage               AS Progress.Lang.Object,
                                                       pcMessageButton1             AS CHARACTER,
                                                       poMessageButtonDialogResult1 AS DialogResultEnum,
                                                       pcMessageButton2             AS CHARACTER,
                                                       poMessageButtonDialogResult2 AS DialogResultEnum,
                                                       piMessageTimer AS INTEGER):

        DEFINE VARIABLE cButtons AS CHARACTER        NO-UNDO EXTENT 2.
        DEFINE VARIABLE oResults AS DialogResultEnum NO-UNDO EXTENT 2.

        ASSIGN cButtons[1] = pcMessageButton1
               oResults[1] = poMessageButtonDialogResult1
               cButtons[2] = pcMessageButton2
               oResults[2] = poMessageButtonDialogResult2 .

        RETURN ShowMessage (pcMessage, pcMessageTitle, pcMessageDetail,
                            poMessageImage, cButtons, oResults, ?, piMessageTimer) .

    END METHOD .

    /**
     * Purpose: Display Message
     * Notes:   The parameter piMessageTimer is the (optional) message timeout in seconds.
     *          When specified the message form will close itself after the specified
     *          number of seconds (or when the user hits one of the buttons) and
     *          return the default DialogResultEnum.
     * @param pcMessage The message text
     * @param pcMessageTitle The title of the message
     * @param pcMessageDetail The detail text for the message
     * @param poMessageImage The reference to the Image for the message
     * @param pcMessageButton1 The label of the first message button
     * @param poMessageButtonDialogResult1 The DialogResultEnum value for the first message button
     * @param pcMessageButton2 The label of the second message button
     * @param poMessageButtonDialogResult2 The DialogResultEnum value for the second message button
     * @param pcMessageButton3 The label of the third message button
     * @param poMessageButtonDialogResult3 The DialogResultEnum value for the third message button
     * @param piMessageTimer The Timer value for the Message Box in seconds
     * @return The reference to the DialogResultEnum value representing the button pressed
     */
    METHOD PUBLIC STATIC DialogResultEnum ShowMessage (pcMessage                    AS CHARACTER,
                                                       pcMessageTitle               AS CHARACTER,
                                                       pcMessageDetail              AS CHARACTER,
                                                       poMessageImage               AS Progress.Lang.Object,
                                                       pcMessageButton1             AS CHARACTER,
                                                       poMessageButtonDialogResult1 AS DialogResultEnum,
                                                       pcMessageButton2             AS CHARACTER,
                                                       poMessageButtonDialogResult2 AS DialogResultEnum,
                                                       pcMessageButton3             AS CHARACTER,
                                                       poMessageButtonDialogResult3 AS DialogResultEnum,
                                                       piMessageTimer AS INTEGER):

        DEFINE VARIABLE cButtons AS CHARACTER        NO-UNDO EXTENT 3.
        DEFINE VARIABLE oResults AS DialogResultEnum NO-UNDO EXTENT 3.

        ASSIGN cButtons[1] = pcMessageButton1
               oResults[1] = poMessageButtonDialogResult1
               cButtons[2] = pcMessageButton2
               oResults[2] = poMessageButtonDialogResult2
               cButtons[3] = pcMessageButton3
               oResults[3] = poMessageButtonDialogResult3 .

        RETURN ShowMessage (pcMessage, pcMessageTitle, pcMessageDetail,
                            poMessageImage, cButtons, oResults, ?, piMessageTimer) .

    END METHOD .

    /**
     * Purpose: Display Message
     * Notes:   The parameter piMessageTimer is the (optional) message timeout in seconds.
     *          When specified the message form will close itself after the specified
     *          number of seconds (or when the user hits one of the buttons) and
     *          return the default DialogResultEnum.
     * @param pcMessage The message text
     * @param pcMessageTitle The title of the message
     * @param pcMessageDetail The detail text for the message
     * @param poMessageImage The reference to the Image for the message
     * @param pcMessageButtons The array of labels of the message buttons
     * @param poMessageButtonDialogResults The array of DialogResultEnum values for the message buttons
     * @param piMessageTimer The Timer value for the Message Box in seconds
     * @return The reference to the DialogResultEnum value representing the button pressed
     */
    METHOD PUBLIC STATIC DialogResultEnum ShowMessage (pcMessage                    AS CHARACTER,
                                                       pcMessageTitle               AS CHARACTER,
                                                       pcMessageDetail              AS CHARACTER,
                                                       poMessageImage               AS Progress.Lang.Object,
                                                       pcMessageButtons             AS CHARACTER EXTENT,
                                                       poMessageButtonDialogResults AS DialogResultEnum EXTENT,
                                                       piMessageTimer AS INTEGER):


        RETURN ShowMessage (pcMessage, pcMessageTitle, pcMessageDetail,
                            poMessageImage, pcMessageButtons, poMessageButtonDialogResults, ?, piMessageTimer) .

    END METHOD .

    /**
     * Purpose: Display a Message to the User
     * Notes:   The parameter piMessageTimer is the (optional) message timeout in seconds.
     *          When specified the message form will close itself after the specified
     *          number of seconds (or when the user hits one of the buttons) and
     *          return the default DialogResultEnum.
     * @param pcMessage The message text
     * @param pcMessageTitle The title of the message
     * @param pcMessageDetail The detail text for the message
     * @param poMessageImage The reference to the Image for the message
     * @param pcMessageButtons The array of labels of the message buttons
     * @param poMessageButtonDialogResults The array of DialogResultEnum values for the message buttons
     * @param poDefaultDialogResult The default DialogResultEnum value
     * @param piMessageTimer The Timer value for the Message Box in seconds
     * @return The reference to the DialogResultEnum value representing the button pressed
     */
    METHOD PUBLIC STATIC DialogResultEnum ShowMessage (pcMessage                    AS CHARACTER,
                                                       pcMessageTitle               AS CHARACTER,
                                                       pcMessageDetail              AS CHARACTER,
                                                       poMessageImage               AS Progress.Lang.Object,
                                                       pcMessageButtons             AS CHARACTER EXTENT,
                                                       poMessageButtonDialogResults AS DialogResultEnum EXTENT,
                                                       poDefaultDialogResult        AS DialogResultEnum,
                                                       piMessageTimer AS INTEGER):

        DEFINE VARIABLE oMessageForm AS Consultingwerk.Util.Forms.IMessageForm NO-UNDO .
&IF DEFINED (DotNetAccessible) NE 0 &THEN
        DEFINE VARIABLE oForm        AS System.Windows.Forms.Form              NO-UNDO .
&ENDIF
        DEFINE VARIABLE oResult      AS DialogResultEnum                       NO-UNDO .

&IF DEFINED (DotNetAccessible) NE 0 &THEN
        IF Consultingwerk.Util.StartupParameterHelper:IOEverywhere <> 1 THEN
&ENDIF
            RETURN ShowMessageBox (pcMessage,
                                   pcMessageTitle,
                                   pcMessageDetail,
                                   poMessageButtonDialogResults) .

&IF DEFINED (DotNetAccessible) NE 0 &THEN
        IF pcMessageTitle > "":U THEN .
        ELSE pcMessageTitle = " ":U .

        DO ON ERROR UNDO, THROW:
            /* Mike Fechner, Consultingwerk Ltd. 16.07.2009
               Try display of GUI for .NET Message Dialog (will fail when run with
               function / non void method in call stack) */
            IF MessageFormHelper:MessageFormType > "":U THEN
                oMessageForm = DYNAMIC-NEW (MessageFormHelper:MessageFormType) () .
            ELSE
                oMessageForm = NEW Consultingwerk.Windows.Util.Forms.MessageForm () .

            ASSIGN oMessageForm:MessageText         = pcMessage
                   oMessageForm:MessageDetail       = pcMessageDetail
                   oMessageForm:DefaultDialogResult = poDefaultDialogResult
                   oMessageForm:MessageTimer        = piMessageTimer .

            IF VALID-OBJECT (poMessageImage) THEN
                oMessageForm:MessageImage = poMessageImage .

            IF EXTENT (pcMessageButtons) >= 1 THEN
                oMessageForm:MessageButtonLabels = pcMessageButtons .

            IF EXTENT (poMessageButtonDialogResults) >= 1 THEN
                oMessageForm:MessageButtonDialogResults = poMessageButtonDialogResults .

            oForm = CAST(CAST(oMessageForm, Progress.Lang.Object), System.Windows.Forms.Form) .

            oForm:Text = pcMessageTitle .

            IF MessageFormHelper:MessageFormTopMost = TRUE THEN
                oForm:TopMost = TRUE .

            ShowMessageForm (oForm, OUTPUT oResult) .

            RETURN oResult .

            CATCH err AS Progress.Lang.Error :
                Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
            END CATCH.
        END.

        FINALLY:
            IF VALID-OBJECT (oMessageForm) THEN
                DELETE OBJECT oMessageForm.
        END FINALLY.
&ENDIF
    END METHOD.

    /**
     * Purpose: Display Message based on a Message structure
     * Notes:
     * @param poMessage The Message structure describing the message
     * @return The DialogResultEnum indicating the button the user has pressed (useful for questions)
     */
    METHOD PUBLIC STATIC DialogResultEnum ShowMessage (poMessage AS IMessage):

        RETURN MessageFormHelper:ShowMessage (poMessage, 0) .

    END METHOD .

    /**
     * Purpose: Display Message based on a Message structure
     * Notes:
     * @param poMessage The Message structure describing the message
     * @param pcMessageButton1 The label of the first message button
     * @param poMessageButtonDialogResult1 The DialogResultEnum value for the first message button
     * @return The DialogResultEnum indicating the button the user has pressed (useful for questions)
     */
    METHOD PUBLIC STATIC DialogResultEnum ShowMessage (poMessage                    AS IMessage,
                                                       pcMessageButton1             AS CHARACTER,
                                                       poMessageButtonDialogResult1 AS DialogResultEnum):

        DEFINE VARIABLE cButtons       AS CHARACTER        EXTENT 1 NO-UNDO .
        DEFINE VARIABLE oDialogResults AS DialogResultEnum EXTENT 1 NO-UNDO .

        ASSIGN cButtons[1]       = pcMessageButton1
               oDialogResults[1] = poMessageButtonDialogResult1 .

        RETURN ShowMessage (poMessage,
                            cButtons,
                            oDialogResults,
                            poMessageButtonDialogResult1,
                            0) .

    END METHOD .

    /**
     * Purpose: Display Message based on a Message structure
     * Notes:
     * @param poMessage The Message structure describing the message
     * @param pcMessageButton1 The label of the first message button
     * @param poMessageButtonDialogResult1 The DialogResultEnum value for the first message button
     * @param pcMessageButton2 The label of the second message button
     * @param poMessageButtonDialogResult2 The DialogResultEnum value for the second message button
     * @param poDefaultDialogResult The default DialogResultEnum value
     * @return The DialogResultEnum indicating the button the user has pressed (useful for questions)
     */
    METHOD PUBLIC STATIC DialogResultEnum ShowMessage (poMessage                    AS IMessage,
                                                       pcMessageButton1             AS CHARACTER,
                                                       poMessageButtonDialogResult1 AS DialogResultEnum,
                                                       pcMessageButton2             AS CHARACTER,
                                                       poMessageButtonDialogResult2 AS DialogResultEnum,
                                                       poDefaultDialogResult        AS DialogResultEnum):

        DEFINE VARIABLE cButtons       AS CHARACTER        EXTENT 2 NO-UNDO .
        DEFINE VARIABLE oDialogResults AS DialogResultEnum EXTENT 2 NO-UNDO .

        ASSIGN cButtons[1]       = pcMessageButton1
               oDialogResults[1] = poMessageButtonDialogResult1
               cButtons[2]       = pcMessageButton2
               oDialogResults[2] = poMessageButtonDialogResult2 .

        RETURN ShowMessage (poMessage,
                            cButtons,
                            oDialogResults,
                            poDefaultDialogResult,
                            0) .

    END METHOD .

    /**
     * Purpose: Display Message based on a Message structure
     * Notes:
     * @param poMessage The Message structure describing the message
     * @param pcMessageButton1 The label of the first message button
     * @param poMessageButtonDialogResult1 The DialogResultEnum value for the first message button
     * @param pcMessageButton2 The label of the second message button
     * @param poMessageButtonDialogResult2 The DialogResultEnum value for the second message button
     * @param pcMessageButton3 The label of the third message button
     * @param poMessageButtonDialogResult3 The DialogResultEnum value for the second message button
     * @param poDefaultDialogResult The default DialogResultEnum value
     * @return The DialogResultEnum indicating the button the user has pressed (useful for questions)
     */
    METHOD PUBLIC STATIC DialogResultEnum ShowMessage (poMessage                    AS IMessage,
                                                       pcMessageButton1             AS CHARACTER,
                                                       poMessageButtonDialogResult1 AS DialogResultEnum,
                                                       pcMessageButton2             AS CHARACTER,
                                                       poMessageButtonDialogResult2 AS DialogResultEnum,
                                                       pcMessageButton3             AS CHARACTER,
                                                       poMessageButtonDialogResult3 AS DialogResultEnum,
                                                       poDefaultDialogResult        AS DialogResultEnum):

        DEFINE VARIABLE cButtons       AS CHARACTER        EXTENT 3 NO-UNDO .
        DEFINE VARIABLE oDialogResults AS DialogResultEnum EXTENT 3 NO-UNDO .

        ASSIGN cButtons[1]       = pcMessageButton1
               oDialogResults[1] = poMessageButtonDialogResult1
               cButtons[2]       = pcMessageButton2
               oDialogResults[2] = poMessageButtonDialogResult2
               cButtons[3]       = pcMessageButton3
               oDialogResults[3] = poMessageButtonDialogResult3 .

        RETURN ShowMessage (poMessage,
                            cButtons,
                            oDialogResults,
                            poDefaultDialogResult,
                            0) .

    END METHOD .

    /**
     * Purpose: Display Message based on a Message structure
     * Notes:
     * @param poMessage The Message structure describing the message
     * @param pcMessageButtons The array of labels of the message buttons
     * @param poMessageButtonDialogResults The array of DialogResultEnum values for the message buttons
     * @param poDefaultDialogResult The default DialogResultEnum value
     * @return The DialogResultEnum indicating the button the user has pressed (useful for questions)
     */
    METHOD PUBLIC STATIC DialogResultEnum ShowMessage (poMessage                    AS IMessage,
                                                       pcMessageButtons             AS CHARACTER EXTENT,
                                                       poMessageButtonDialogResults AS DialogResultEnum EXTENT,
                                                       poDefaultDialogResult        AS DialogResultEnum):

        RETURN MessageFormHelper:ShowMessage (poMessage,
                                              pcMessageButtons,
                                              poMessageButtonDialogResults,
                                              poDefaultDialogResult,
                                              0) .

    END METHOD .

    /**
     * Purpose: Display Message
     * Notes:
     * @param pcMessage The message text
     * @return The reference to the DialogResultEnum value representing the button pressed
     */
    METHOD PUBLIC STATIC DialogResultEnum ShowMessage (pcMessage AS CHARACTER):

        DEFINE VARIABLE c AS CHARACTER        EXTENT NO-UNDO .
        DEFINE VARIABLE o AS DialogResultEnum EXTENT NO-UNDO .

        RETURN ShowMessage (pcMessage, "":U, "":U, ?, c, o, ?, 0) .

    END METHOD .

    /**
     * Purpose: Display Message
     * Notes:
     * @param pcMessage The message text
     * @param poMessageImage The reference to the Image for the message
     * @return The reference to the DialogResultEnum value representing the button pressed
     */
    METHOD PUBLIC STATIC DialogResultEnum ShowMessage (pcMessage       AS CHARACTER,
                                                       poMessageImage  AS Progress.Lang.Object):

        DEFINE VARIABLE c AS CHARACTER        EXTENT NO-UNDO .
        DEFINE VARIABLE o AS DialogResultEnum EXTENT NO-UNDO .

        RETURN ShowMessage (pcMessage, "":U, "":U, poMessageImage, c, o, ?, 0) .

    END METHOD .

    /**
     * Purpose: Display Message
     * Notes:
     * @param pcMessage The message text
     * @param pcMessageTitle The title of the message
     * @return The reference to the DialogResultEnum value representing the button pressed
     */
    METHOD PUBLIC STATIC DialogResultEnum ShowMessage (pcMessage      AS CHARACTER,
                                                       pcMessageTitle AS CHARACTER):

        DEFINE VARIABLE c AS CHARACTER        EXTENT NO-UNDO .
        DEFINE VARIABLE o AS DialogResultEnum EXTENT NO-UNDO .

        RETURN ShowMessage (pcMessage, pcMessageTitle, "":U, ?, c, o, ?, 0) .

    END METHOD .

    /**
     * Purpose: Display Message
     * Notes:
     * @param pcMessage The message text
     * @param pcMessageTitle The title of the message
     * @param poMessageImage The reference to the Image for the message
     * @return The reference to the DialogResultEnum value representing the button pressed
     */
    METHOD PUBLIC STATIC DialogResultEnum ShowMessage (pcMessage       AS CHARACTER,
                                                       pcMessageTitle  AS CHARACTER,
                                                       poMessageImage  AS Progress.Lang.Object):

        DEFINE VARIABLE c AS CHARACTER        EXTENT NO-UNDO .
        DEFINE VARIABLE o AS DialogResultEnum EXTENT NO-UNDO .

        RETURN ShowMessage (pcMessage, pcMessageTitle, "":U, poMessageImage, c, o, ?, 0) .

    END METHOD .

    /**
     * Purpose: Display Message
     * Notes:
     * @param pcMessage The message text
     * @param pcMessageTitle The title of the message
     * @param pcMessageDetail The detail text for the message
     * @return The reference to the DialogResultEnum value representing the button pressed
     */
    METHOD PUBLIC STATIC DialogResultEnum ShowMessage (pcMessage       AS CHARACTER,
                                                       pcMessageTitle  AS CHARACTER,
                                                       pcMessageDetail AS CHARACTER):

        DEFINE VARIABLE c AS CHARACTER        EXTENT NO-UNDO .
        DEFINE VARIABLE o AS DialogResultEnum EXTENT NO-UNDO .

        RETURN ShowMessage (pcMessage, pcMessageTitle, pcMessageDetail, ?, c, o, ?, 0) .

    END METHOD .

    /**
     * Purpose: Display Message
     * Notes:
     * @param pcMessage The message text
     * @param pcMessageTitle The title of the message
     * @param pcMessageDetail The detail text for the message
     * @param poMessageImage The reference to the Image for the message
     * @return The reference to the DialogResultEnum value representing the button pressed
     */
    METHOD PUBLIC STATIC DialogResultEnum ShowMessage (pcMessage       AS CHARACTER,
                                                       pcMessageTitle  AS CHARACTER,
                                                       pcMessageDetail AS CHARACTER,
                                                       poMessageImage  AS Progress.Lang.Object):

        DEFINE VARIABLE c AS CHARACTER        EXTENT NO-UNDO .
        DEFINE VARIABLE o AS DialogResultEnum EXTENT NO-UNDO .

        RETURN ShowMessage (pcMessage, pcMessageTitle, pcMessageDetail, poMessageImage, c, o, ?, 0) .

    END METHOD .

    /**
     * Purpose: Display Message
     * Notes:
     * @param pcMessage The message text
     * @param pcMessageTitle The title of the message
     * @param pcMessageDetail The detail text for the message
     * @param poMessageImage The reference to the Image for the message
     * @param pcMessageButton1 The label of the first message button
     * @param poMessageButtonDialogResult1 The DialogResultEnum value for the first message button
     * @return The reference to the DialogResultEnum value representing the button pressed
     */
    METHOD PUBLIC STATIC DialogResultEnum ShowMessage (pcMessage                    AS CHARACTER,
                                                       pcMessageTitle               AS CHARACTER,
                                                       pcMessageDetail              AS CHARACTER,
                                                       poMessageImage               AS Progress.Lang.Object,
                                                       pcMessageButton1             AS CHARACTER,
                                                       poMessageButtonDialogResult1 AS DialogResultEnum):

        DEFINE VARIABLE cButtons AS CHARACTER        NO-UNDO EXTENT 1.
        DEFINE VARIABLE oResults AS DialogResultEnum NO-UNDO EXTENT 1.

        ASSIGN cButtons[1] = pcMessageButton1
               oResults[1] = poMessageButtonDialogResult1 .

        RETURN ShowMessage (pcMessage, pcMessageTitle, pcMessageDetail,
                            poMessageImage, cButtons, oResults, ?, 0) .

    END METHOD .

    /**
     * Purpose: Display Message
     * Notes:
     * @param pcMessage The message text
     * @param pcMessageTitle The title of the message
     * @param pcMessageDetail The detail text for the message
     * @param poMessageImage The reference to the Image for the message
     * @param pcMessageButton1 The label of the first message button
     * @param poMessageButtonDialogResult1 The DialogResultEnum value for the first message button
     * @param pcMessageButton2 The label of the second message button
     * @param poMessageButtonDialogResult2 The DialogResultEnum value for the second message button
     * @return The reference to the DialogResultEnum value representing the button pressed
     */
    METHOD PUBLIC STATIC DialogResultEnum ShowMessage (pcMessage                    AS CHARACTER,
                                                       pcMessageTitle               AS CHARACTER,
                                                       pcMessageDetail              AS CHARACTER,
                                                       poMessageImage               AS Progress.Lang.Object,
                                                       pcMessageButton1             AS CHARACTER,
                                                       poMessageButtonDialogResult1 AS DialogResultEnum,
                                                       pcMessageButton2             AS CHARACTER,
                                                       poMessageButtonDialogResult2 AS DialogResultEnum):

        DEFINE VARIABLE cButtons AS CHARACTER        NO-UNDO EXTENT 2.
        DEFINE VARIABLE oResults AS DialogResultEnum NO-UNDO EXTENT 2.

        ASSIGN cButtons[1] = pcMessageButton1
               oResults[1] = poMessageButtonDialogResult1
               cButtons[2] = pcMessageButton2
               oResults[2] = poMessageButtonDialogResult2 .

        RETURN ShowMessage (pcMessage, pcMessageTitle, pcMessageDetail,
                            poMessageImage, cButtons, oResults, ?, 0) .

    END METHOD .

    /**
     * Purpose: Display Message
     * Notes:
     * @param pcMessage The message text
     * @param pcMessageTitle The title of the message
     * @param pcMessageDetail The detail text for the message
     * @param poMessageImage The reference to the Image for the message
     * @param pcMessageButton1 The label of the first message button
     * @param poMessageButtonDialogResult1 The DialogResultEnum value for the first message button
     * @param pcMessageButton2 The label of the second message button
     * @param poMessageButtonDialogResult2 The DialogResultEnum value for the second message button
     * @param pcMessageButton3 The label of the third message button
     * @param poMessageButtonDialogResult3 The DialogResultEnum value for the third message button
     * @return The reference to the DialogResultEnum value representing the button pressed
     */
    METHOD PUBLIC STATIC DialogResultEnum ShowMessage (pcMessage                    AS CHARACTER,
                                                       pcMessageTitle               AS CHARACTER,
                                                       pcMessageDetail              AS CHARACTER,
                                                       poMessageImage               AS Progress.Lang.Object,
                                                       pcMessageButton1             AS CHARACTER,
                                                       poMessageButtonDialogResult1 AS DialogResultEnum,
                                                       pcMessageButton2             AS CHARACTER,
                                                       poMessageButtonDialogResult2 AS DialogResultEnum,
                                                       pcMessageButton3             AS CHARACTER,
                                                       poMessageButtonDialogResult3 AS DialogResultEnum):

        DEFINE VARIABLE cButtons AS CHARACTER        NO-UNDO EXTENT 3.
        DEFINE VARIABLE oResults AS DialogResultEnum NO-UNDO EXTENT 3.

        ASSIGN cButtons[1] = pcMessageButton1
               oResults[1] = poMessageButtonDialogResult1
               cButtons[2] = pcMessageButton2
               oResults[2] = poMessageButtonDialogResult2
               cButtons[3] = pcMessageButton3
               oResults[3] = poMessageButtonDialogResult3 .

        RETURN ShowMessage (pcMessage, pcMessageTitle, pcMessageDetail,
                            poMessageImage, cButtons, oResults, ?, 0) .

    END METHOD .

    /**
     * Purpose: Display Message
     * Notes:
     * @param pcMessage The message text
     * @param pcMessageTitle The title of the message
     * @param pcMessageDetail The detail text for the message
     * @param poMessageImage The reference to the Image for the message
     * @param pcMessageButtons The array of labels of the message buttons
     * @param poMessageButtonDialogResults The array of DialogResultEnum values for the message buttons
     * @return The reference to the DialogResultEnum value representing the button pressed
     */
    METHOD PUBLIC STATIC DialogResultEnum ShowMessage (pcMessage                    AS CHARACTER,
                                                       pcMessageTitle               AS CHARACTER,
                                                       pcMessageDetail              AS CHARACTER,
                                                       poMessageImage               AS Progress.Lang.Object,
                                                       pcMessageButtons             AS CHARACTER EXTENT,
                                                       poMessageButtonDialogResults AS DialogResultEnum EXTENT):

        RETURN ShowMessage (pcMessage, pcMessageTitle, pcMessageDetail,
                            poMessageImage, pcMessageButtons, poMessageButtonDialogResults, ?, 0) .

    END METHOD .

    /**
     * Purpose: Display a Message to the User
     * Notes:
     * @param pcMessage The message text
     * @param pcMessageTitle The title of the message
     * @param pcMessageDetail The detail text for the message
     * @param poMessageImage The reference to the Image for the message
     * @param pcMessageButtons The array of labels of the message buttons
     * @param poMessageButtonDialogResults The array of DialogResultEnum values for the message buttons
     * @param poDefaultDialogResult The default DialogResultEnum value
     * @return The reference to the DialogResultEnum value representing the button pressed
     */
    METHOD PUBLIC STATIC DialogResultEnum ShowMessage (pcMessage                    AS CHARACTER,
                                                       pcMessageTitle               AS CHARACTER,
                                                       pcMessageDetail              AS CHARACTER,
                                                       poMessageImage               AS Progress.Lang.Object,
                                                       pcMessageButtons             AS CHARACTER EXTENT,
                                                       poMessageButtonDialogResults AS DialogResultEnum EXTENT,
                                                       poDefaultDialogResult        AS DialogResultEnum):

        RETURN MessageFormHelper:ShowMessage (pcMessage,
                                              pcMessageTitle,
                                              pcMessageDetail,
                                              poMessageImage,
                                              pcMessageButtons,
                                              poMessageButtonDialogResults,
                                              poDefaultDialogResult,
                                              0) .

    END METHOD.

    /**
     * Purpose: Shows an message using the ABL message statement
     * Notes:
     * @param pcMessage The text for the message box
     * @param pcMessageTitle The title for the message box
     * @param pcMessageDetail The detail text for the message
     * @param poMessageButtonDialogResults The array of DialogResultEnum values for the message buttons
     * @return The DialogResultEnum value indicating the button pressed
     */
    METHOD PUBLIC STATIC DialogResultEnum ShowMessageBox (pcMessage                    AS CHARACTER,
                                                          pcMessageTitle               AS CHARACTER,
                                                          pcMessageDetail              AS CHARACTER,
                                                          poMessageButtonDialogResults AS DialogResultEnum EXTENT):

        DEFINE VARIABLE lResult AS LOGICAL NO-UNDO INITIAL ? .

        IF pcMessageDetail > "":U THEN DO:
            IF EXTENT (poMessageButtonDialogResults) = 2 AND
               poMessageButtonDialogResults[1]:Equals (DialogResultEnum:DialogResultYes) AND
               poMessageButtonDialogResults[2]:Equals (DialogResultEnum:DialogResultNo)
                THEN DO:
                    MESSAGE pcMessage SKIP(1) pcMessageDetail
                        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE pcMessageTitle UPDATE lResult .

                   IF lResult THEN
                       RETURN DialogResultEnum:DialogResultYes.
                   ELSE
                       RETURN DialogResultEnum:DialogResultNo.
                END.
            ELSE IF EXTENT (poMessageButtonDialogResults) = 3 AND
                    poMessageButtonDialogResults[1]:Equals (DialogResultEnum:DialogResultYes) AND
                    poMessageButtonDialogResults[2]:Equals (DialogResultEnum:DialogResultNo) AND
                    poMessageButtonDialogResults[3]:Equals (DialogResultEnum:DialogResultCancel)
                THEN DO:
                    MESSAGE pcMessage SKIP(1) pcMessageDetail
                        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL TITLE pcMessageTitle UPDATE lResult .

                   CASE lResult:
                       WHEN TRUE THEN
                            RETURN DialogResultEnum:DialogResultYes.
                       WHEN FALSE THEN
                            RETURN DialogResultEnum:DialogResultNo.
                       OTHERWISE
                           RETURN DialogResultEnum:DialogResultCancel.
                   END CASE .
                END.
            ELSE IF EXTENT (poMessageButtonDialogResults) = 2 AND
                    poMessageButtonDialogResults[1]:Equals (DialogResultEnum:DialogResultOk) AND
                    poMessageButtonDialogResults[2]:Equals (DialogResultEnum:DialogResultCancel)
                THEN DO:
                    MESSAGE pcMessage SKIP(1) pcMessageDetail
                        VIEW-AS ALERT-BOX QUESTION BUTTONS OK-CANCEL TITLE pcMessageTitle UPDATE lResult .

                   IF lResult THEN
                       RETURN DialogResultEnum:DialogResultOk.
                   ELSE
                       RETURN DialogResultEnum:DialogResultCancel.
                END.
            ELSE IF EXTENT (poMessageButtonDialogResults) = 2 AND
                    poMessageButtonDialogResults[1]:Equals (DialogResultEnum:DialogResultRetry) AND
                    poMessageButtonDialogResults[2]:Equals (DialogResultEnum:DialogResultCancel)
                THEN DO:
                    MESSAGE pcMessage SKIP(1) pcMessageDetail
                        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL TITLE pcMessageTitle UPDATE lResult .

                   CASE lResult:
                       WHEN TRUE THEN
                           RETURN DialogResultEnum:DialogResultRetry.
                       WHEN FALSE THEN
                           RETURN DialogResultEnum:DialogResultCancel.
                   END CASE .
                END.
            ELSE
                MESSAGE pcMessage SKIP(1) pcMessageDetail
                    VIEW-AS ALERT-BOX INFORMATION TITLE pcMessageTitle .
        END.
        ELSE DO:
            IF EXTENT (poMessageButtonDialogResults) = 2 AND
               poMessageButtonDialogResults[1]:Equals (DialogResultEnum:DialogResultYes) AND
               poMessageButtonDialogResults[2]:Equals (DialogResultEnum:DialogResultNo)
                THEN DO:
                    MESSAGE pcMessage
                        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE pcMessageTitle UPDATE lResult .

                   IF lResult THEN
                       RETURN DialogResultEnum:DialogResultYes.
                   ELSE
                       RETURN DialogResultEnum:DialogResultNo.
                END.
            ELSE IF EXTENT (poMessageButtonDialogResults) = 3 AND
                    poMessageButtonDialogResults[1]:Equals (DialogResultEnum:DialogResultYes) AND
                    poMessageButtonDialogResults[2]:Equals (DialogResultEnum:DialogResultNo) AND
                    poMessageButtonDialogResults[3]:Equals (DialogResultEnum:DialogResultCancel)
                THEN DO:
                    MESSAGE pcMessage
                        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL TITLE pcMessageTitle UPDATE lResult .

                   CASE lResult:
                       WHEN TRUE THEN
                            RETURN DialogResultEnum:DialogResultYes.
                       WHEN FALSE THEN
                            RETURN DialogResultEnum:DialogResultNo.
                       OTHERWISE
                           RETURN DialogResultEnum:DialogResultCancel.
                   END CASE .
                END.
            ELSE IF EXTENT (poMessageButtonDialogResults) = 2 AND
                    poMessageButtonDialogResults[1]:Equals (DialogResultEnum:DialogResultOk) AND
                    poMessageButtonDialogResults[2]:Equals (DialogResultEnum:DialogResultCancel)
                THEN DO:
                    MESSAGE pcMessage
                        VIEW-AS ALERT-BOX QUESTION BUTTONS OK-CANCEL TITLE pcMessageTitle UPDATE lResult .

                   IF lResult THEN
                       RETURN DialogResultEnum:DialogResultOk.
                   ELSE
                       RETURN DialogResultEnum:DialogResultCancel.
                END.
            ELSE IF EXTENT (poMessageButtonDialogResults) = 2 AND
                    poMessageButtonDialogResults[1]:Equals (DialogResultEnum:DialogResultRetry) AND
                    poMessageButtonDialogResults[2]:Equals (DialogResultEnum:DialogResultCancel)
                THEN DO:
                    MESSAGE pcMessage
                        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL TITLE pcMessageTitle UPDATE lResult .

                   CASE lResult:
                       WHEN TRUE THEN
                            RETURN DialogResultEnum:DialogResultRetry.
                       WHEN FALSE THEN
                            RETURN DialogResultEnum:DialogResultCancel.
                   END CASE .
                END.
            ELSE
                MESSAGE pcMessage
                    VIEW-AS ALERT-BOX INFORMATION TITLE pcMessageTitle .
        END.

        RETURN DialogResultEnum:DialogResultOK .

    END METHOD.

&IF DEFINED (DotNetAccessible) NE 0 &THEN
    /**
     * Purpose: Shows a MessageForm
     * Notes:   Private method. Works around potential compilation isuses on OpenEdge
     *          10.2B which methods returning a value and input-blocking statements
     * @param poForm The MessageForm to be shown
     * @param poResult Output parameter with the DialogResultEnum value
     */
    METHOD PRIVATE STATIC VOID ShowMessageForm (poForm AS System.Windows.Forms.Form,
                                                OUTPUT poResult AS DialogResultEnum):

        DEFINE VARIABLE oDialogResult      AS DialogResult                                   NO-UNDO .
        DEFINE VARIABLE oShowDialogSupport AS Consultingwerk.Util.Support.IShowDialogSupport NO-UNDO .

        oShowDialogSupport = {Consultingwerk/get-service.i Consultingwerk.Util.Support.IShowDialogSupport} .

        /* Mike Fechner, Consultingwerk Ltd. 11.06.2012
           Changed the way the Dialog parent/owner is set because at one client
           using the ShowDialog with the Owner parameter caused this error
           (likely Progress Bug):
           You can only use a .NET-specific input-blocking statement (WAIT-FOR x:y())
           once any .NET forms are shown (other than to wait for an ABL dialog box). (13967) */
        IF VALID-OBJECT (MessageFormHelper:MessageFormParent) THEN
            poForm:Owner = MessageFormHelper:MessageFormParent .

/*        IF VALID-OBJECT (MessageFormHelper:MessageFormParent) THEN                              */
/*            WAIT-FOR poForm:ShowDialog (MessageFormHelper:MessageFormParent) SET oDialogResult .*/
/*        ELSE                                                                                    */

        IF VALID-OBJECT (oShowDialogSupport) THEN DO:
            oShowDialogSupport:ShowDialog (poForm) .
        END.
        ELSE
            WAIT-FOR poForm:ShowDialog () SET oDialogResult .

        /* workaround possible OpenEdge issue */
        IF NOT VALID-OBJECT (oDialogResult) THEN
            oDialogResult = poForm:DialogResult .

        poResult = DialogResultEnum:FromSystemWindowsForms (oDialogResult) .

    END METHOD.
&ENDIF

END CLASS.
