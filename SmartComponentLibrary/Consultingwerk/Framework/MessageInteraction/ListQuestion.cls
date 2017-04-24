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
    File        : ListQuestion
    Purpose     : List of Question instances
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Sep 01 00:11:03 CET 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.Base.*               FROM PROPATH .
USING Consultingwerk.Framework.MessageInteraction.* FROM PROPATH .  
USING Progress.Lang.*                               FROM PROPATH .

CLASS Consultingwerk.Framework.MessageInteraction.ListQuestion 
    INHERITS GenericList: 

    {Consultingwerk/Framework/Base/GenericList.i Consultingwerk.Framework.MessageInteraction.Question}

    /*------------------------------------------------------------------------------
        Purpose: Checks if the List of Questions contains an unanswered question
        Notes:   
        @return Logical value indicating if the list contains an unanswered question 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL HasUnansweredQuestion ():
        
        /* 1. Checken, ob wir bereits eine Antwort auf die Frage haben */
        {Consultingwerk/foreachABL.i Question oQuestion in THIS-OBJECT}
            IF oQuestion:MessageReply = MessageReplyEnum:Unanswered THEN 
                RETURN TRUE .  
        END.

        RETURN FALSE .

    END METHOD .

END CLASS.
