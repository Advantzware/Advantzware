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
    File        : IFrameParserPostProcessor
    Purpose     : Service interface for classes that allow to post-process
                  the FrameParser output before presenting it to the user
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Dec 10 12:09:28 CET 2014
    Notes       : Service Interface with no default implementation
  ----------------------------------------------------------------------*/

USING Consultingwerk.Studio.LegacyGuiMigration.Frame.* FROM PROPATH .  
USING Progress.Lang.*                                  FROM PROPATH .

INTERFACE Consultingwerk.Studio.LegacyGuiMigration.Frame.IFrameParserPostProcessor:  

    {Consultingwerk/Studio/LegacyGuiMigration/Frame/ttFrameWidget.i}

    /*------------------------------------------------------------------------------
        Purpose: Allows post-processing of the frame parser result 
        Notes:   Temp-Table ttFrameWidget passed BY-REFERENCE from the FrameParser
        @param pcFileName The name of the file to parse
        @param pcFrameName The name of the frame to return the widgets for
        @param ttFrameWidget INPUT-OUTPUT TABLE The temp-table with the frame widget information (from Consultingwerk/Studio/LegacyGuiMigration/Frame/ttFrameWidget.i) 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID PostProcessFrameWidgets (pcFileName AS CHARACTER,
                                                pcFrameName AS CHARACTER,
                                                INPUT-OUTPUT TABLE ttFrameWidget).

END INTERFACE.
