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
    File        : HandleCommitErrorEventArgs
    Purpose     : Event Argument for the HandleCommitError event and HandleCommitSysError 
                  of the Data Access class
    Syntax      : 
    Description : Allows custom handling of commit erorrs
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Jan 11 00:39:36 CET 2014
    Notes       : Typically errors should be stored in the Buffers ERROR-STRING
                  attribute
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*      FROM PROPATH .
USING Consultingwerk.OERA.* FROM PROPATH .  
USING Progress.Lang.*       FROM PROPATH .

CLASS Consultingwerk.OERA.HandleCommitErrorEventArgs INHERITS EventArgs: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the handle of the Before Buffer associated with this event 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BufferHandle AS HANDLE NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the error instance associated with this event
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Error AS Progress.Lang.Error NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if the error is handled
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Handled AS LOGICAL INITIAL FALSE NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the HandleCommitErrorEventArgs class
        Notes:   
        @param poError The reference to the error associated with this event
        @param phBufferHandle The handle of the before buffer associated with this event
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC HandleCommitErrorEventArgs (poError AS Progress.Lang.Error,
                                                   phBufferHandle AS HANDLE):
        SUPER ().
        
        ASSIGN THIS-OBJECT:Error        = poError
               THIS-OBJECT:BufferHandle = phBufferHandle .
        
    END CONSTRUCTOR.

END CLASS.
