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
    File        : DesktopAlertHelper
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Dec 01 19:16:51 CET 2010
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.SmartComponents.Support.DesktopAlertHelper: 

    DEFINE PRIVATE STATIC VARIABLE oDesktopAlert AS Infragistics.Win.Misc.UltraDesktopAlert NO-UNDO . 

    /*------------------------------------------------------------------------------
        Purpose: Static constructor for the DesktopAlertHelper class                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR STATIC DesktopAlertHelper ():
        
        oDesktopAlert = NEW Infragistics.Win.Misc.UltraDesktopAlert () .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Private constructor for the DesktopAlertHelper class                                                                       
        Notes:   Disallow instance creation                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE DesktopAlertHelper ():
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Shows an UltraDesktopAlert                                                                        
        Notes:                             
        @param pcMessageTitle The title for the desktop alert
        @param pcMessageText The text for the desktop alert
        @return The reference to the UltraDesktopAlertWindowInfo for the new alert                                           
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC Infragistics.Win.Misc.UltraDesktopAlertWindowInfo Show (pcMessageTitle AS CHARACTER,
                                                                                 pcMessageText AS CHARACTER):
        
        DEFINE VARIABLE oImage AS System.Drawing.Image NO-UNDO . 
        
        oDesktopAlert:AnimationSpeed = Infragistics.Win.Misc.AnimationSpeed:Slow.
        oDesktopAlert:AnimationStyleAutoClose = Infragistics.Win.Misc.AnimationStyle:FadeAndScroll.
        oDesktopAlert:AnimationStyleShow = Infragistics.Win.Misc.AnimationStyle:None.
        oDesktopAlert:Opacity = 0.75.
        oDesktopAlert:Style = Infragistics.Win.Misc.DesktopAlertStyle:Office2007.
        
        oDesktopAlert:AnimationStyleShow = Infragistics.Win.Misc.AnimationStyle:NONE .
        oDesktopAlert:AnimationStyleAutoClose = Infragistics.Win.Misc.AnimationStyle:FadeAndScroll .

        FILE-INFO:FILE-NAME = "Consultingwerk\SmartComponents\Support\Images\hourglass.png":U . 
        IF FILE-INFO:FULL-PATHNAME > "":U THEN 
            ASSIGN oImage = Consultingwerk.Util.ImageHelper:Load (FILE-INFO:FULL-PATHNAME) .

        RETURN oDesktopAlert:Show (pcMessageTitle, 
                                   pcMessageText,
                                   oImage) .

    END METHOD.

END CLASS.