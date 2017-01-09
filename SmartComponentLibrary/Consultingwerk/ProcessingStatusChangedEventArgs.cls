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
    File        : ProcessingStatusChangedEventArgs
    Purpose     : Cancelable Event Argument to be used for status events 
                  raised by batch like tasks
    Syntax      : 
    Description : May be used in the context of the GenericProcessingStatusWithCancelForm class  
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Jul 22 21:33:21 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.* FROM PROPATH .  
USING Progress.Lang.*  FROM PROPATH .

CLASS Consultingwerk.ProcessingStatusChangedEventArgs 
    INHERITS CancelableEventArgs: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the status message of this event
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Message AS CHARACTER NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ProcessingStatusChangedEventArgs class
        Notes:   
        @param pcMessage The message associated with this event
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ProcessingStatusChangedEventArgs (pcMessage AS CHARACTER):
        SUPER ().
        
        ASSIGN THIS-OBJECT:Message = pcMessage .
        
    END CONSTRUCTOR.

END CLASS.
