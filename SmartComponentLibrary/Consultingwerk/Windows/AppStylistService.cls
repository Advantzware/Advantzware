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
    File        : AppStylistService
    Purpose     : Service that allows switching and editing Infragistics
                  Style Libraries 
    Syntax      : 
    Description : Default implementation of the IAppStylistService Interface
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Sep 22 00:33:59 CEST 2013
    Notes       : Uses "InfragisticsStyleSheetFolder" setting from the 
                  IConfigurationProvider (typically .applicationsettings) 
                  to determine the folder name used to load ISL files from
                  Uses "InfragisticsDefaultStyleSheet" setting from the
                  IConfigurationProvider (typically .applicationsettings)
                  to determine the default style sheet to load at startup
                  when the user has no StyleSheet selected in his user profile 
                  yet 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.* FROM PROPATH .
USING Consultingwerk.Util.*      FROM PROPATH .
USING Consultingwerk.Windows.*   FROM PROPATH .  
USING Progress.Lang.*            FROM PROPATH .

CLASS Consultingwerk.Windows.AppStylistService 
    IMPLEMENTS IAppStylistService: 

    /*------------------------------------------------------------------------------
        Purpose: Raised when the Style Library has changed
        Notes:   
        @param sender The object that raised the StyleLibraryChanged event
        @param e The Consultingwerk.EventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT StyleLibraryChanged SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                            e AS Consultingwerk.EventArgs).
    
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the name of the folder from where the 
                 Style Library files will be loaded 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY FolderName AS CHARACTER NO-UNDO INIT "Consultingwerk/Windows/Styles":U 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the current style library 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY StyleLibrary AS CHARACTER NO-UNDO 
	GET.
	PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the current style library file 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY StyleLibraryFileName AS CHARACTER NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the AppStylistService class
        Notes:   Restores previously selected StyleSheet from the user profile
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC AppStylistService ():
	    
        DEFINE VARIABLE oSettings     AS ISettingsService       NO-UNDO . 
        DEFINE VARIABLE oConfig       AS IConfigurationProvider NO-UNDO . 
        DEFINE VARIABLE cStyleLibrary AS CHARACTER              NO-UNDO .  
        DEFINE VARIABLE cFolderName   AS CHARACTER              NO-UNDO .      
	    
		SUPER ().

        oConfig = {Consultingwerk/get-service.i Consultingwerk.Framework.IConfigurationProvider} . 
        
        IF VALID-OBJECT (oConfig) THEN 
            ASSIGN cFolderName = oConfig:GetValue ("InfragisticsStyleSheetFolder":U) . 

        IF cFolderName > "":U THEN 
            ASSIGN THIS-OBJECT:FolderName = cFolderName .

        ASSIGN oSettings     = {Consultingwerk/get-service.i Consultingwerk.Framework.ISettingsService} 
               cStyleLibrary = oSettings:GetSetting ("AppStylistService":U,
                                                     "StyleLibrary":U) .

        IF cStyleLibrary > "":U THEN . 
        ELSE 
            cStyleLibrary = oConfig:GetValue ("InfragisticsDefaultStyleSheet":U) .

        IF cStyleLibrary > "":U THEN DO:     
            /* Mike Fechner, Consultingwerk Ltd. 09.10.2012
               Default to Office 2010 Styles, when Office 2007 was stored in the user's profile */
            IF NOT PROVERSION BEGINS "10":U THEN 
                ASSIGN cStyleLibrary = REPLACE (cStyleLibrary,
                                                "Office2007":U, 
                                                "Office2010":U) .

            THIS-OBJECT:LoadStyleLibrary (cStyleLibrary) . 
        END.
        ELSE DO:
            IF PROVERSION BEGINS "10":U THEN
                THIS-OBJECT:LoadStyleLibrary ("Office2007Blue":U) .
            ELSE 
                THIS-OBJECT:LoadStyleLibrary ("Office2010Blue":U) .    
        END.                
                
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Invokes the designer for the loaded style library 
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID EditStyleLibrary ():
        
        DEFINE VARIABLE oDesigner AS Infragistics.Win.AppStyling.Runtime.AppStylistRuntime NO-UNDO .
        
        oDesigner = NEW Infragistics.Win.AppStyling.Runtime.AppStylistRuntime ()  .
        
        oDesigner:ShowRuntimeApplicationStylingEditor 
            (Consultingwerk.Framework.FrameworkSettings:MdiContainer,
             {Consultingwerk/translate.i &string="'AppStylist Designer':U"}) .
            
    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Loads a Style Library
        Notes:   
        @param pcStyleLibrary The name of the style library file, without the .isl extension
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID LoadStyleLibrary (pcStyleLibrary AS CHARACTER):

        DEFINE VARIABLE cFileName AS CHARACTER                                 NO-UNDO .
        DEFINE VARIABLE oSettings AS Consultingwerk.Framework.ISettingsService NO-UNDO . 
		
		IF THIS-OBJECT:FolderName > "":U THEN 
            ASSIGN cFileName = SUBSTITUTE("&1/&2.isl":U, 
                                          THIS-OBJECT:FolderName,
                                          pcStyleLibrary) .
        ELSE 
            ASSIGN cFileName = SUBSTITUTE("&1.isl":U, 
                                          pcStyleLibrary) .

        Consultingwerk.Util.StyleLibraryHelper:LoadFromFile 
            (cFileName) .


        ASSIGN THIS-OBJECT:StyleLibrary         = pcStyleLibrary
               THIS-OBJECT:StyleLibraryFileName = cFileName . 

        THIS-OBJECT:OnStyleLibraryChanged (Consultingwerk.EventArgs:Empty) .

        ASSIGN oSettings = {Consultingwerk/get-service.i Consultingwerk.Framework.ISettingsService} .

        oSettings:StoreSetting ("AppStylistService":U,
                                "StyleLibrary":U,
                                pcStyleLibrary) .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the StyleLibraryChanged
        Notes:   
        @param e The Consultingwerk.EventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnStyleLibraryChanged (e AS Consultingwerk.EventArgs):
            
        IF NOT VALID-OBJECT (e) THEN 
            e = Consultingwerk.EventArgs:Empty . 
    
        THIS-OBJECT:StyleLibraryChanged:Publish (THIS-OBJECT, e) .
    
    END METHOD .

END CLASS.
