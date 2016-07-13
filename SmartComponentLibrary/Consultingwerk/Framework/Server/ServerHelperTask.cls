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
    File        : ServerHelperTask
    Purpose     : General purpose helper Business Task
    Syntax      : 
    Description : Provides interaction between client and AppServer
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Jul 23 23:44:51 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                  FROM PROPATH . 
USING Consultingwerk.Framework.Server.* FROM PROPATH .  
USING Consultingwerk.OERA.*             FROM PROPATH .
USING Consultingwerk.Util.*             FROM PROPATH . 
USING Progress.Lang.*                   FROM PROPATH .

CLASS Consultingwerk.Framework.Server.ServerHelperTask 
    IMPLEMENTS IBusinessService: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ServerHelperTask class
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC ServerHelperTask ():
		SUPER ().
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Routine that can be used by clients to PING the AppServer 
        Notes:   
        @param poKeepAlive The CharacterHolder who's value will be set to the Ping response
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID Ping (poKeepAlive AS CharacterHolder):
		
		IF VALID-OBJECT (poKeepAlive) THEN 
		    poKeepAlive:Value = SUBSTITUTE ("Hello, &1"{&TRAN}, NOW) .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Writes a message to the AppServer logfile 
        Notes:   
        @param poMessage The CharacterHolder instance with the message to be written to the logfile
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID WriteToLogFile (poMessage AS CharacterHolder):
		
		IF VALID-OBJECT (poMessage) THEN 
            LogManager:WriteFormattedMessage ("[Client-Message] &1":U, poMessage:Value) .		    

	END METHOD .

END CLASS.
