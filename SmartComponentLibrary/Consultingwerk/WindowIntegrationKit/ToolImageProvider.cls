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
    File        : ToolImageProvider
    Purpose     : Sample implementation for the IToolImageProvider Interface
                  This is to be replaced in a project with a customized
                  version (if required)
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Aug 24 12:08:24 CEST 2011
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Util.* .
USING Consultingwerk.WindowIntegrationKit.IToolImageProvider.
USING Infragistics.Win.UltraWinToolbars.* .
USING Progress.Lang.*.

CLASS Consultingwerk.WindowIntegrationKit.ToolImageProvider IMPLEMENTS IToolImageProvider:

    DEFINE VARIABLE oImages AS "System.Collections.Generic.Dictionary<System.String, System.Drawing.Image>":U NO-UNDO .

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the ToolImageProvider class
        Notes:   Initializes Dictionary used for caching images
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ToolImageProvider ():
        SUPER ().

        oImages = NEW "System.Collections.Generic.Dictionary<System.String, System.Drawing.Image>":U () .

        oImages:Add ("window-16":U, Consultingwerk.Util.ImageHelper:Load ("Consultingwerk/WindowIntegrationKit/Images/window-16.png":U)) .
        oImages:Add ("window-32":U, Consultingwerk.Util.ImageHelper:Load ("Consultingwerk/WindowIntegrationKit/Images/window-32.png":U)) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Verifies if the Image should be reloaded during UltraToolbarsHelper:RefreshTools
        Notes:
        @param phWidget The handle of the menu-item or button widget
        @param poButtonTool The reference to the Infragistics ButtonTool that relaces the widget
        @return True when an Image needs to be reloaded, oterwise False
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL CheckReloadImage (phWidget AS HANDLE,
                                            poButtonTool AS ToolBase):

        IF CAN-QUERY (phWidget, "IMAGE":U) AND
           VALID-OBJECT (poButtonTool:SharedProps:AppearancesSmall:Appearance) AND
           VALID-OBJECT (poButtonTool:SharedProps:AppearancesSmall:Appearance:Image) AND

               /* Mike Fechner, Consultingwerk Ltd. 09.09.2011
                  Store the ABL widget's IMAGE (filename) in the Tag of the .NET Image */
               UNBOX (CAST(poButtonTool:SharedProps:AppearancesSmall:Appearance:Image,
                           System.Drawing.Image):Tag) = phWidget:IMAGE THEN
            RETURN FALSE .

        ELSE
            RETURN CAN-QUERY (phWidget, "IMAGE":U) AND phWidget:IMAGE > "":U .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Customized routine to load an image to a Tool
        Notes:   Can be customized using an IToolImageProvider Implementation
                 The LOGICAL RETURN value indicates if an image has been loaded and
                 influences the Tool PreferredSizeOnRibbon
        @param phWidget The handle of the menu-item or button widget
        @param poButtonTool The reference to the Infragistics ButtonTool that relaces the widget
        @return True when an Image was loaded, otherwise False
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL LoadImage (phWidget AS HANDLE,
                                     poButtonTool AS ToolBase):

        DEFINE VARIABLE lReturn AS LOGICAL NO-UNDO INIT FALSE .

        IF CAN-QUERY (phWidget, "IMAGE":U) THEN
            ASSIGN lReturn = UltraToolbarsHelper:LoadImage (phWidget, poButtonTool, FALSE) .

        IF lReturn = FALSE THEN DO:

            poButtonTool:SharedProps:AppearancesSmall:Appearance:Image = oImages["window-16":U] .
            poButtonTool:SharedProps:AppearancesLarge:Appearance:Image = oImages["window-32":U] .

            poButtonTool:SharedProps:DisplayStyle = ToolDisplayStyle:ImageOnlyOnToolbars .

            lReturn = TRUE .
        END.

        RETURN lReturn .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor of the ToolImageProvider class
        Notes:   Dispose all cached Images
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC ToolImageProvider ():

        {Consultingwerk/foreach.i System.Drawing.Image oImage in oImages:Values}
            oImage:Dispose () .
        END.

        oImages:Clear() .

    END DESTRUCTOR.

END CLASS.