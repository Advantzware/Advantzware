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
    File        : ICancelableMessage
    Purpose     : Optional interface for messages published by th IMessagePublisher 
    Syntax      : 
    Description : When messages implement this intreface, subscribers can actually 
                  cancel the message delivery, the message will in those cases not
                  be delivered to the next subscribers 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Dec 09 10:53:00 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Framework.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH.

INTERFACE Consultingwerk.Framework.ICancelableMessage:  
  
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if the message has been cancelled 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Cancel AS LOGICAL NO-UNDO 
    GET.
    SET. 

END INTERFACE.