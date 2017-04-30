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
    File        : BrowseSupport
    Purpose     : API to methods of the ABL Browse widget that can be
                  used against the RenderedBrowseControl and the ABL
                  Browse Widget
    Syntax      : Static methods
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Jul 11 18:31:42 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.WindowIntegrationKit.*          FROM PROPATH .  
USING Consultingwerk.WindowIntegrationKit.Controls.* FROM PROPATH .  
USING Progress.Lang.*                                FROM PROPATH .

CLASS Consultingwerk.WindowIntegrationKit.BrowseMethods: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to an IBrowseApi instance to be used for the 
                 current method
        Notes:   
        @param phBrowseWidget The handle of the ABL Browse widget
        @return The reference to the IBrowseApi instance  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC IBrowseApi ForHandle (phBrowseWidget AS HANDLE):

        /* Mike Fechner, Consultingwerk Ltd. 05.08.2013
           RenderedBrowseControl:FromBrowseHandle (phBrowseWidget) Crashes on 10.2A */
        &IF NOT PROVERSION GE "4" AND PROVERSION GE "10.2B" &THEN
        DEFINE VARIABLE oRenderedBrowseControl AS RenderedBrowseControl NO-UNDO . 

        IF WinKitSettings:WinKitActive = TRUE THEN DO:

            {Consultingwerk/Assertion/HandleAssert/WidgetType.i phBrowseWidget Consultingwerk.WidgetTypeEnum:Browse} .
    
            oRenderedBrowseControl = WinKitControls:FromBrowseHandle (phBrowseWidget) .
    
            IF VALID-OBJECT (oRenderedBrowseControl) THEN 
                RETURN NEW RenderedBrowseControlApi (oRenderedBrowseControl) . 
            ELSE 
                RETURN NEW AblBrowseWidgetApi (phBrowseWidget) .
        END.
        ELSE
        &ENDIF
            RETURN NEW AblBrowseWidgetApi (phBrowseWidget) .

    END METHOD .

END CLASS.
