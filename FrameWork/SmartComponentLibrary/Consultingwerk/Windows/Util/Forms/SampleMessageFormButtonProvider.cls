/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : SampleMessageFormButtonProvider
    Purpose     : Sample service that provides custom Buttons for the
                  MessageForm
    Syntax      : Allows adding buttons to report errors etc.
    Description : See http://confluence.consultingwerkcloud.com/wiki/display/SCL/Using+custom+buttons+on+the+MessageForm
                  for further instructions
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Jan 10 14:27:31 CET 2014
    Notes       : The MessageForm assigns Location and Anchor. All other
                  properties are to be assigned by the service implementation
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Framework.*          FROM PROPATH .
USING Consultingwerk.Framework.Enum.*     FROM PROPATH .
USING Consultingwerk.SmartFramework.*     FROM PROPATH .
USING Consultingwerk.Util.*               FROM PROPATH .
USING Consultingwerk.Windows.Util.Forms.* FROM PROPATH .
USING Progress.Lang.*                     FROM PROPATH .

CLASS Consultingwerk.Windows.Util.Forms.SampleMessageFormButtonProvider
    IMPLEMENTS IMessageFormButtonProvider:

    /*------------------------------------------------------------------------------
        Purpose: Returns the List of Buttons to be added to the dialog
        Notes:   Buttons should have a meaningful name
        @param poMessage The Message that is currently being processed
        @return The List the System.Windows.Forms.Button objects to be added to the MessageForm
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC "System.Collections.Generic.List<System.Windows.Forms.Button>" GetButtonsForMessage (poMessage AS Message):

        DEFINE VARIABLE oList   AS "System.Collections.Generic.List<System.Windows.Forms.Button>" NO-UNDO .
        DEFINE VARIABLE oButton AS System.Windows.Forms.Button                                    NO-UNDO .

        oList = NEW "System.Collections.Generic.List<System.Windows.Forms.Button>" () .

        IF FrameworkSettings:DebugMode THEN DO:
            oButton = NEW System.Windows.Forms.Button () .
            oButton:Name = "Kill":U .
            oButton:Text = "Kill session"{&TRAN} .

            oList:Add (oButton) .
        END.

        oButton = NEW System.Windows.Forms.Button () .
        oButton:Name = "Copy":U .
        oButton:Text = "Copy"{&TRAN} .

        oList:Add (oButton) .

        oButton = NEW System.Windows.Forms.Button () .
        oButton:Name = "Stacktrace":U .
        oButton:Text = "Stacktrace":U .

        oList:Add (oButton) .

        RETURN oList .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Handles the click event of a custom button
        Notes:
        @param poButton The reference to the Button that was clicked
        @param poMessage The reference to the Message that is currently shown
        @return Logical value indicating if the  Message Form should be closed (True = Close)
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL HandleButtonClick (poButton AS System.Windows.Forms.Button,
                                             poMessage AS Message):

        CASE poButton:Name:
            WHEN "Kill":U THEN DO:
                IF MessageFormHelper:AskQuestion ("Are you sure?"{&TRAN}) = DialogResultEnum:DialogResultYes THEN
                    System.Diagnostics.Process:GetCurrentProcess():Kill () .
            END.

            WHEN "Copy":U THEN
                CLIPBOARD:VALUE = SUBSTITUTE ("&1&3&2":U, poMessage:Text, poMessage:Detail, Consultingwerk.Environment:NewLine) .

            WHEN "Stacktrace":U THEN
                MESSAGE Consultingwerk.Util.ErrorHelper:CurrentStacktrace VIEW-AS ALERT-BOX INFORMATION .

        END CASE.

    END METHOD .

END CLASS.
