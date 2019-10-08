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
    File        : OutputStream
    Purpose     : Output stream using a stream-handle as the output destination
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Mar 07 22:11:30 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.* FROM PROPATH .
USING Progress.Lang.*  FROM PROPATH .

CLASS Consultingwerk.OutputStream IMPLEMENTS IOutputStream: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the handle to the stream used for the output                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY StreamHandle AS HANDLE NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the OutputStream class                                                                        
        Notes:          
        @param phStreamHandle The handle of the stream to use as the output destination                                                              
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC OutputStream (phStreamHandle AS HANDLE):
        SUPER ().

        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phStreamHandle Consultingwerk.WidgetTypeEnum:Stream} .
        
        ASSIGN THIS-OBJECT:StreamHandle = phStreamHandle . 
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Writes text to the output destination                                                                        
        Notes:                                          
        @param pcText The text for the output                             
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Write (pcText AS CHARACTER):

        PUT STREAM-HANDLE THIS-OBJECT:StreamHandle pcText .    

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Writes text to the output destination and terminates it with a 
                 line break                                                                        
        Notes:                                          
        @param pcText The text for the output                             
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID WriteLine (pcText AS CHARACTER):

        PUT STREAM-HANDLE THIS-OBJECT:StreamHandle pcText SKIP .    

    END METHOD.

END CLASS.
