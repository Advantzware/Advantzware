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
    File        : AblBrowseMigrationSettings
    Purpose     : Settings class for the ABL FRAME Migration Utility 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Nov 27 20:40:24 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Exceptions.*                       FROM PROPATH .
USING Consultingwerk.Studio.LegacyGuiMigration.Browse.* FROM PROPATH .
USING Consultingwerk.Util.*                             FROM PROPATH . 
USING Consultingwerk.Windows.Util.Forms.*               FROM PROPATH .
USING Progress.Lang.*                                   FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.Studio.LegacyGuiMigration.Browse.AblBrowseMigrationSettings: 

    DEFINE STATIC TEMP-TABLE ttAblBrowseMigrationSettings NO-UNDO 
        FIELD CustomServices AS CHARACTER .
        
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets a custom services file to be used by the Business Entity 
                 Designer
        Notes:   Custom services will be registered in the default ServiceContainer 
                 and allow customization as an alternative to plugins 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY CustomServices AS CHARACTER NO-UNDO 
    GET:
        RETURN ttAblBrowseMigrationSettings.CustomServices .
    END GET .
    SET (arg AS CHARACTER):
        ttAblBrowseMigrationSettings.CustomServices = arg .
    END SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the Settings File that settings were loaded from 
        Notes:   May return "<no settings file>" in a fresh environment
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY SettingsFile AS CHARACTER NO-UNDO INIT "<no settings file>":U  
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns if a settings file was found when starting the Business 
                 Entity Designerthe 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY SettingsFileLoaded AS LOGICAL NO-UNDO INIT FALSE   
    GET.
    PRIVATE SET. 

	/*------------------------------------------------------------------------------
	    Purpose: Private constructor for the AblBrowseMigrationSettings class
	    Notes:   Disallow instance creation
	------------------------------------------------------------------------------*/
	CONSTRUCTOR PRIVATE AblBrowseMigrationSettings ():
		SUPER ().
		
	END CONSTRUCTOR.

	/*------------------------------------------------------------------------------
	    Purpose: Static constructor for the AblBrowseMigrationSettings class
	    Notes:
	------------------------------------------------------------------------------*/
	CONSTRUCTOR STATIC  AblBrowseMigrationSettings ():

        FILE-INFO:FILE-NAME = ".AblBrowseMigrationSettings.xml":U .

        IF FILE-INFO:FULL-PATHNAME <> ? THEN DO: 
            TEMP-TABLE ttAblBrowseMigrationSettings:READ-XML ("FILE":U, 
                                                              FILE-INFO:FULL-PATHNAME,
                                                              "EMPTY":U, ?, ?) .
            
            ASSIGN AblBrowseMigrationSettings:SettingsFile       = FILE-INFO:FULL-PATHNAME 
                   AblBrowseMigrationSettings:SettingsFileLoaded = TRUE .
        END.                                                                 
                                                                 
        FIND FIRST ttAblBrowseMigrationSettings NO-ERROR .
        
        IF NOT AVAILABLE ttAblBrowseMigrationSettings THEN DO:
            CREATE ttAblBrowseMigrationSettings .             
        
            /* Assign defaults */
            
            FIND FIRST ttAblBrowseMigrationSettings .
        END.
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Saves settings to the file .BusinessEntityDesignerSettings.xml                                                                         
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID SaveSettings ():
        
        DEFINE VARIABLE oException AS ExceptionWithTitle NO-UNDO . 
        DEFINE VARIABLE cFileName  AS CHARACTER          NO-UNDO .
        
        FILE-INFO:FILE-NAME = ".AblBrowseMigrationSettings.xml":U .
        
        IF FILE-INFO:FULL-PATHNAME > "":U THEN 
            cFileName = FILE-INFO:FULL-PATHNAME .
        ELSE 
            cFileName = ".AblBrowseMigrationSettings.xml":U . 
        
        IF FILE-INFO:FULL-PATHNAME > "":U AND NOT FILE-INFO:FILE-TYPE MATCHES "*W*":U THEN  
            Consultingwerk.Util.MessageFormHelper:ShowMessage ("Unable to store application preferences."{&TRAN},
                                                               "ABL GUI FRAME Widget Migration"{&TRAN},
                                                               "The file .AblBrowseMigrationSettings.xml is write protected."{&TRAN},
                                                               MessageFormImages:ImageWarning) . 
        ELSE DO ON ERROR UNDO, THROW: 
        
            TEMP-TABLE ttAblBrowseMigrationSettings:WRITE-XML ("FILE":U, 
                                                              cFileName,
                                                              TRUE) .
                                                                  
            CATCH err AS Progress.Lang.Error:
                    
                oException = NEW ExceptionWithTitle (err,
                                                     "Unable to store application preferences.~nThe file .BusinessEntityDesignerSettings.xml may be write protected or hidden."{&TRAN},
                                                     0,
                                                     "Business Entity Designer"{&TRAN}) .
        
                ErrorHelper:ShowErrorMessage (oException) .                                 
                    
            END CATCH.                                                                  
        END.                                                                  
                                                                  
        FIND FIRST ttAblBrowseMigrationSettings . 

    END METHOD.

END CLASS.
