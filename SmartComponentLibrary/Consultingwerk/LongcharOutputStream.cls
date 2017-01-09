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
    File        : LongcharOutputStream
    Purpose     : Output Stream with a LONGCHAR variables as the output 
                  destination
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Mar 07 22:11:30 CET 2013
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.* FROM PROPATH .
USING Progress.Lang.*  FROM PROPATH .

CLASS Consultingwerk.LongcharOutputStream IMPLEMENTS IOutputStream: 

    DEFINE VARIABLE lcStream AS LONGCHAR NO-UNDO . 

	/*------------------------------------------------------------------------------
		Purpose: Returns the current longchar data  																	  
		Notes:
	    @return The current longchar data  																	  
	------------------------------------------------------------------------------*/
	METHOD PUBLIC LONGCHAR GetLongchar ():
		
		RETURN lcStream.

	END METHOD.

	/*------------------------------------------------------------------------------
		Purpose: Resets the stream by deleting the stored LONGCHAR data  																	  
		Notes:  																	  
	------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID Reset ():
		
		lcStream = "":U . 

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Resets the stream by deleting the stored LONGCHAR data and sets the
                 stream code page                                                                      
        Notes:                   
        @param pcCodepage The codepage to use for the stream data                                                     
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Reset (pcCodepage AS CHARACTER):
        
        lcStream = "":U . 

        FIX-CODEPAGE (lcStream) = pcCodepage . 

    END METHOD.
    /*------------------------------------------------------------------------------
        Purpose: Writes text to the output destination                                                                        
        Notes:                                          
        @param pcText The text for the output                             
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Write (pcText AS CHARACTER):

        IF pcText = ? THEN 
            pcText = "?":U . 

        lcStream = lcStream + pcText .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Writes text to the output destination and terminates it with a 
                 line break                                                                        
        Notes:                                          
        @param pcText The text for the output                             
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID WriteLine (pcText AS CHARACTER):

        IF pcText = ? THEN 
            pcText = "?":U . 

        lcStream = lcStream + pcText + Consultingwerk.Environment:NewLine .

	END METHOD.

END CLASS.
