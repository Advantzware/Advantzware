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
    File        : AsyncTaskCompletedEventArgs
    Purpose     : Event argument for the AsyncTaskCompleted event of the 
                  ServiceAdapter
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Oct 11 14:28:45 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*      FROM PROPATH .
USING Consultingwerk.OERA.* FROM PROPATH .  
USING Progress.Lang.*       FROM PROPATH .

CLASS Consultingwerk.OERA.AsyncTaskCompletedEventArgs 
    INHERITS EventArgs: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference of the context object associated with the 
                 async task that was completed 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Context AS Progress.Lang.Object NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the parameter object instance returned by 
                 the task 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Parameter AS ISerializable NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the handle of the async-request that was completed 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY RequestHandle AS HANDLE NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the AsyncTaskCompletedEventArgs class
        Notes:   
        @param poContext The reference to the context object instance
        @param phRequestHandle The handle of the async request
        @param poParameter The reference to the parameter object instance returned by the task
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC AsyncTaskCompletedEventArgs (poContext AS Progress.Lang.Object,
                                                    phRequestHandle AS HANDLE,
                                                    poParameter AS ISerializable):
        SUPER ().
        
        ASSIGN THIS-OBJECT:Context       = poContext
               THIS-OBJECT:RequestHandle = phRequestHandle
               THIS-OBJECT:Parameter     = poParameter . 
        
    END CONSTRUCTOR.

END CLASS.
