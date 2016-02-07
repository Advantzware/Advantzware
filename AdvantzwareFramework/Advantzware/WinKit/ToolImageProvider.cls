/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike
    Created     : Sat Feb 06 21:09:49 CET 2016
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Advantzware.WinKit.*                  FROM PROPATH .  
USING Consultingwerk.WindowIntegrationKit.* FROM PROPATH .
USING Progress.Lang.*                       FROM PROPATH .
USING Consultingwerk.Util.* FROM PROPATH.

CLASS Advantzware.WinKit.ToolImageProvider IMPLEMENTS IToolImageProvider: 


    DEFINE VARIABLE oImages16 AS "System.Collections.Generic.Dictionary<System.String, System.Drawing.Image>":U NO-UNDO .
    DEFINE VARIABLE oImages32 AS "System.Collections.Generic.Dictionary<System.String, System.Drawing.Image>":U NO-UNDO .

    DEFINE VARIABLE oCaptions AS "System.Collections.Generic.Dictionary<System.String, System.String>":U NO-UNDO .



    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ToolImageProvider class
        Notes:   
        @param parameter1 Description
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC ToolImageProvider ():
		SUPER ().

        ASSIGN oImages16 = NEW "System.Collections.Generic.Dictionary<System.String, System.Drawing.Image>":U ()
               oImages32 = NEW "System.Collections.Generic.Dictionary<System.String, System.Drawing.Image>":U ()
               oCaptions = NEW "System.Collections.Generic.Dictionary<System.String, System.String>":U        ()
               .

       oImages16:Add ("images/homekeyg", ImageHelper:Load ("SampleImages/112_RefreshArrow_Blue_16x16_72.png")) .
       oImages32:Add ("images/homekeyg", ImageHelper:Load ("SampleImages/112_RefreshArrow_Blue_32x32_72.png")) .
       oCaptions:Add ("images/homekeyg", "Refresh") .    
		
		
       oImages16:Add ("images/add", ImageHelper:Load ("SampleImages/112_Plus_Blue_16x16_72.png")) .
       oImages32:Add ("images/add", ImageHelper:Load ("SampleImages/112_Plus_Blue_32x32_72.png")) .
       oCaptions:Add ("images/add", "Refresh") .                                                           
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Verifies if the Image should be reloaded during UltraToolbarsHelper:RefreshTools                                                                   
        Notes:                                                                        
        @param phWidget The handle of the menu-item or button widget 
        @param poButtonTool The reference to the Infragistics ButtonTool that relaces the widget
        @return True when an Image needs to be reloaded, oterwise False
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC LOGICAL CheckReloadImage (phWidget AS HANDLE, 
	                                        poButtonTool AS Infragistics.Win.UltraWinToolbars.ToolBase):
		
        IF CAN-QUERY (phWidget, "IMAGE":U) AND 
           VALID-OBJECT (poButtonTool:SharedProps:AppearancesSmall:Appearance) AND 
           VALID-OBJECT (poButtonTool:SharedProps:AppearancesSmall:Appearance:Image) AND 
               
               /* Mike Fechner, Consultingwerk Ltd. 09.09.2011
                  Store the ABL widget's IMAGE (filename) in the Tag of the .NET Image */
               UNBOX (CAST(poButtonTool:SharedProps:AppearancesSmall:Appearance:Image,
                           System.Drawing.Image):Tag) = phWidget:IMAGE THEN RETURN FALSE . 
        
        ELSE 
            RETURN CAN-QUERY (phWidget, "IMAGE":U) AND phWidget:IMAGE > "":U .
            
	END METHOD .

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
	                                 poButtonTool AS Infragistics.Win.UltraWinToolbars.ToolBase):
		
        DEFINE VARIABLE lReturn AS LOGICAL              NO-UNDO INIT FALSE .
        DEFINE VARIABLE oImage  AS System.Drawing.Image NO-UNDO . 

        DEFINE VARIABLE cImage AS CHARACTER NO-UNDO.


        IF CAN-QUERY (phWidget, "IMAGE":U) THEN DO:
            ASSIGN lReturn = UltraToolbarsHelper:LoadImage (phWidget, poButtonTool, FALSE) .

            IF lReturn THEN RETURN TRUE  .

            /* Attempt to add .bmp to the image file name */
            IF phWidget:IMAGE > "" OR phWidget:IMAGE-UP > "" THEN DO:

                IF phWidget:IMAGE > "" THEN 
                    cImage = phWidget:IMAGE .
                ELSE
                    cImage = phWidget:IMAGE-UP .    

                ASSIGN cImage = REPLACE (cImage, "~\", "/") .

                IF oImages16:ContainsKey(cImage) THEN DO:
                    ASSIGN 
                        poButtonTool:SharedProps:AppearancesLarge:Appearance:Image = oImages32[cImage]
                        poButtonTool:SharedProps:AppearancesSmall:Appearance:Image = oImages16[cImage]
                        poButtonTool:SharedProps:Caption                           = oCaptions[cImage] .                    
                    
                    RETURN TRUE . 
                END.

                oImage = ImageHelper:Load (cImage) .

                IF VALID-OBJECT (oImage) THEN DO:
                    ASSIGN
                        poButtonTool:SharedProps:AppearancesLarge:Appearance:Image = oImage
                        poButtonTool:SharedProps:AppearancesSmall:Appearance:Image = oImage .

                    RETURN TRUE .
                END.

            END.

        END.
        
        
        RETURN lReturn . 
        
        CATCH err AS Progress.Lang.Error :
        	RETURN FALSE . 	
        END CATCH.
	END METHOD .

END CLASS.
