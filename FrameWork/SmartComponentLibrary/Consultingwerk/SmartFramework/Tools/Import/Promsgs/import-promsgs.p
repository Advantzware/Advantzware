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
    File        : import-promsgs.p
    Purpose     : Import messages from promsgs into SmartDB

    Syntax      :

    Description :

    Author(s)   :
    Created     : Sun Aug 17 00:26:07 CEST 2014
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/SmartFramework/System/dsMessage.i}
{Consultingwerk/SmartFramework/Tools/Import/Promsgs/ttPromsgs.i}
{Consultingwerk/Util/TempTables/ttFileNames.i}

DEFINE VARIABLE iMsgNum         AS INTEGER   NO-UNDO.
DEFINE VARIABLE cMsgText        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMsgDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFlag1          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFlag2          AS CHARACTER NO-UNDO.

DEFINE VARIABLE oRequest AS Consultingwerk.OERA.FetchDataRequest NO-UNDO .

/* ***************************  Main Block  *************************** */

Consultingwerk.Util.FileHelper:GetFileList (Consultingwerk.Util.SessionHelper:DLC +
                                            "/prohelp/msgdata":U,
                                            "*.":U,
                                            OUTPUT TABLE ttFileNames) .
FOR EACH ttFileNames ON ERROR UNDO, THROW:
    INPUT FROM VALUE (ttFileNames.FileName) .

    REPEAT ON ERROR UNDO, THROW:
        IMPORT iMsgNum cMsgText cMsgDescription cFlag1 cFlag2 .

        IF iMsgNum > 0 THEN DO:
            CREATE ttPromsgs .
            ASSIGN ttPromsgs.MsgNum         = iMsgNum
                   ttPromsgs.MsgText        = cMsgText
                   ttPromsgs.MsgDescription = cMsgDescription .
            RELEASE ttPromsgs .
        END.

        CATCH syserr AS Progress.Lang.SysError :
            IF syserr:GetMessageNum (1) = 76 THEN
               NEXT .
        END CATCH.
    END .

    FINALLY:
        INPUT CLOSE .
    END FINALLY.
END.

ASSIGN oRequest = NEW Consultingwerk.OERA.FetchDataRequest ("eSmartMessage":U,
                                                            "for each eSmartMessage where eSmartMessage.MessageGroup = ~"OpenEdge~"":U) .

Consultingwerk.OERA.ServiceInterface:FetchData ("Consultingwerk.SmartFramework.System.MessageBusinessEntity":U,
                                                oRequest,
                                                OUTPUT DATASET dsMessage) .

Consultingwerk.Util.DatasetHelper:SetTrackingChanges (DATASET dsMessage:HANDLE, TRUE).

FOR EACH ttPromsgs:
    FIND eSmartMessage WHERE eSmartMessage.MessageNumber = ttPromsgs.MsgNum
                         AND eSmartMessage.MessageGroup  = "OpenEdge":U
                         AND eSmartMessage.LanguageGuid  = "":U NO-ERROR .

    IF NOT AVAILABLE eSmartMessage THEN DO:
        CREATE eSmartMessage.
        ASSIGN eSmartMessage.MessageGuid   = ?
               eSmartMessage.MessageNumber = ttPromsgs.MsgNum
               eSmartMessage.MessageGroup  = "OpenEdge":U
               eSmartMessage.LanguageGuid  = "":U .
    END.

    ASSIGN eSmartMessage.MessageText   = ttPromsgs.MsgText
           eSmartMessage.MessageDetail = ttPromsgs.MsgDescription .
END.

Consultingwerk.OERA.ServiceInterface:SaveChanges ("Consultingwerk.SmartFramework.System.MessageBusinessEntity":U,
                                                  INPUT-OUTPUT DATASET dsMessage) .

IF NOT SESSION:BATCH-MODE THEN
    MESSAGE "Done.":U
        VIEW-AS ALERT-BOX.

CATCH err AS Progress.Lang.Error:
    Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
END CATCH.