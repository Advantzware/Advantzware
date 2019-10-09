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
    File        : LastEvent
    Purpose     : This class can be used in situations where legacy code
                  relies on LAST-EVENT:LABEL 
    Syntax      : Static class
    Description : When the WinKit does APPLY events to ABL widgets, the 
                  LAST-EVENT:LABEL is not always being assigned to the 
                  Widget that has been APPLY'd. In those cases the legacy 
                  code, should be changed to use Consultingwerk.WindowIntegrationKit:LastEvent:LABEL 
                  instead of LAST-EVENT:LABEL
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Sep 06 15:07:56 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.


USING Consultingwerk.WindowIntegrationKit.* FROM PROPATH .  
USING Progress.Lang.*                       FROM PROPATH .

CLASS Consultingwerk.WindowIntegrationKit.LastEvent: 

    DEFINE STATIC VARIABLE cEvent AS CHARACTER NO-UNDO INIT ?.

    /*------------------------------------------------------------------------------
        Purpose: Returns the LABEL of the LAST-EVENT
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY LABEL AS CHARACTER NO-UNDO 
    GET:
        IF cEvent > "":U THEN 
            RETURN cEvent .
        ELSE 
            RETURN LAST-EVENT:LABEL .
                    
    END GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns to LAST-EVENT:LABEL, resets the tweak
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC  VOID ClearLastEvent ():
        
        ASSIGN cEvent = ? .  

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Sets the Event Label that should be returned instead of LAST-EVENT:LABEL
        Notes:   
        @param pcEvent The Event Label to return as LAST-EVENT:LABEL
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID SetLastEvent (pcEvent AS CHARACTER):
        
        ASSIGN cEvent = pcEvent .

    END METHOD .

END CLASS.
