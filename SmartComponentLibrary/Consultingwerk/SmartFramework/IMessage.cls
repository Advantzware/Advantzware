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
    File        : IMessage
    Purpose     : Interface for structures describing a Message 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Jun 13 23:38:05 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartFramework.* FROM PROPATH .  
USING Progress.Lang.*                 FROM PROPATH .

INTERFACE Consultingwerk.SmartFramework.IMessage:  
  
    /*------------------------------------------------------------------------------
        Purpose: Returns the Message Detail
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Detail AS CHARACTER NO-UNDO  
    GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the Message Text
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Text AS CHARACTER NO-UNDO  
    GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the Type of the Message as an MessageTypeEnum member
        Notes:   Information, Warning, Error, Question   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Type AS Consultingwerk.SmartFramework.MessageTypeEnum NO-UNDO  
    GET.
  
END INTERFACE.
