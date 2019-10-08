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
    File        : DefaultOutputStream
    Purpose     : Output stream using the DEFAULT OUTPUT (stdio for 
                  batch clients) as the output destination
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Mar 07 22:11:30 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.* FROM PROPATH .
USING Progress.Lang.*  FROM PROPATH .

CLASS Consultingwerk.DefaultOutputStream IMPLEMENTS IOutputStream: 

    /*------------------------------------------------------------------------------
        Purpose: Writes text to the output destination                                                                        
        Notes:                                          
        @param pcText The text for the output                             
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Write (pcText AS CHARACTER):

        PUT UNFORMATTED pcText .    

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Writes text to the output destination and terminates it with a 
                 line break                                                                        
        Notes:                                          
        @param pcText The text for the output                             
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID WriteLine (pcText AS CHARACTER):

        PUT UNFORMATTED pcText SKIP .    

    END METHOD.

END CLASS.
