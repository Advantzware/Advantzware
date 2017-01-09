/**********************************************************************
 * Copyright (C) 2006-2012 by Consultingwerk Ltd. ("CW") -            *
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
    File        : LogfileOutputStream
    Purpose     : Output Stream with the client-logfile as the output 
                  destination
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Mar 07 22:11:30 CET 2013
    Notes       : The Write () method does not support writing to a ling
                  without a trailing new line
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.* FROM PROPATH .
USING Progress.Lang.*  FROM PROPATH .

CLASS Consultingwerk.LogfileOutputStream IMPLEMENTS IOutputStream: 

    /*------------------------------------------------------------------------------
        Purpose: Writes text to the output destination                                                                        
        Notes:                                          
        @param pcText The text for the output                             
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Write (pcText AS CHARACTER):

        Consultingwerk.Util.LogManager:WriteMessage (pcText) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Writes text to the output destination and terminates it with a 
                 line break                                                                        
        Notes:                                          
        @param pcText The text for the output                             
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID WriteLine (pcText AS CHARACTER):

        Consultingwerk.Util.LogManager:WriteMessage (pcText) .

    END METHOD.

END CLASS.
