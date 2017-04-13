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
    File        : StatusChangedEventArgs
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Nov 19 08:07:04 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.EventArgs.


USING Consultingwerk.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.StatusChangedEventArgs INHERITS EventArgs: 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the status associated with the StatusChanged event 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Status AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the StatusChangedEventArgs class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC StatusChangedEventArgs ():
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the StatusChangedEventArgs class
        Notes:   
        @param pcStatus The value for the Status property
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC StatusChangedEventArgs (pcStatus AS CHARACTER):
        SUPER ().
        
        THIS-OBJECT:Status = pcStatus . 
        
    END CONSTRUCTOR.

END CLASS.
