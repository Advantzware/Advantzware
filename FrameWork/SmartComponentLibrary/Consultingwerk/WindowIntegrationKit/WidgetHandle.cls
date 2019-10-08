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
    File        : WidgetHandle
    Purpose     : Helper Class that can return a Widget-Handle from a 
                  .NET Control's Tag property 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu May 29 13:50:39 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.WindowIntegrationKit.* FROM PROPATH .  
USING Progress.Lang.*                       FROM PROPATH .

CLASS Consultingwerk.WindowIntegrationKit.WidgetHandle: 

    /*------------------------------------------------------------------------------
        Purpose: Private Constructor for the WidgetHandle class
        Notes:   Disallow instance creation   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE WidgetHandle ():
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Returns the Widget-Handle value that is stored in a Control's Tag 
                 property 
        Notes:   
        @param poControl The Control that contains the Widget-Handle in the Tag property
        @return The Widget-Handle or ? 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC HANDLE FromControl (poControl AS System.Windows.Forms.Control):
        
        DEFINE VARIABLE hWidget AS HANDLE NO-UNDO.
        
        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poControl """Control"":U"} .
        
        DO ON ERROR UNDO, THROW:
        
            ASSIGN hWidget = WIDGET-HANDLE (UNBOX (poControl:Tag)) .
        
            RETURN hWidget . 
        
            CATCH err AS Progress.Lang.Error:
                RETURN ? .    
            END CATCH.
        END.

        RETURN ? . 

    END METHOD .

END CLASS.
