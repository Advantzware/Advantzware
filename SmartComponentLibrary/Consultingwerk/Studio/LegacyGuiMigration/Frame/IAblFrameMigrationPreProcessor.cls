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
    File        : IAblFrameMigrationPreProcessor
    Purpose     : Service interface for classes that allow to pre-process
                  the FrameParser output before presenting it to the user
                  This service allows to preassign the IsSelected and AltrenativeControlType
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Dec 10 12:09:28 CET 2014
    Notes       : Service Interface with no default implementation
                  {Consultingwerk/Studio/LegacyGuiMigration/Frame/ttFrameWidget.i}
                  DEFINE TEMP-TABLE ttSelectedFrameWidget NO-UNDO 
                      LIKE ttFrameWidget
                      FIELD IsSelected             AS LOGICAL   INIT TRUE 
                      FIELD AlternativeControlType AS CHARACTER INIT "":U.    
  ----------------------------------------------------------------------*/

USING Consultingwerk.Studio.LegacyGuiMigration.Frame.* FROM PROPATH .  
USING Progress.Lang.*                                  FROM PROPATH .

INTERFACE Consultingwerk.Studio.LegacyGuiMigration.Frame.IAblFrameMigrationPreProcessor:  

    {Consultingwerk/Studio/LegacyGuiMigration/Frame/ttFrameWidget.i}

    DEFINE TEMP-TABLE ttSelectedFrameWidget NO-UNDO 
        LIKE ttFrameWidget
        FIELD IsSelected             AS LOGICAL   INIT TRUE 
        FIELD AlternativeControlType AS CHARACTER INIT "":U.
        
    /*------------------------------------------------------------------------------
        Purpose: Allows post-processing of the frame parser result 
        Notes:   Temp-Table ttSelectedFrameWidget passed BY-REFERENCE from the FrameParser
        @param pcFileName The name of the file to parse
        @param pcFrameName The name of the frame to return the widgets for
        @param ttSelectedFrameWidget INPUT-OUTPUT TABLE The temp-table with the frame widget information
        @param poViewerSettings INPUT-OUTPUT The reference to the ViewerSettings instance with the parameters related to the BindingSource settings and viewer name  
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID PreProcessFrameWidgets (pcFileName AS CHARACTER,
                                               pcFrameName AS CHARACTER,
                                               INPUT-OUTPUT TABLE ttSelectedFrameWidget,
                                               INPUT-OUTPUT poViewerSettings AS ViewerSettings).

END INTERFACE.
