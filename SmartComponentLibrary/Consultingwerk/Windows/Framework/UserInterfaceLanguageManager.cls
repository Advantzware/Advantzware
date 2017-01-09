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
    File        : UserInterfaceLanguageManager
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Jan 15 22:11:16 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Framework.Session.*                 FROM PROPATH . 
USING Consultingwerk.SmartComponents.Resources.*         FROM PROPATH .
USING Consultingwerk.Util.*                              FROM PROPATH . 
USING Consultingwerk.Util.Forms.*                        FROM PROPATH .   
USING Consultingwerk.Windows.Framework.*                 FROM PROPATH .
USING Consultingwerk.Windows.Localization.*              FROM PROPATH . 
USING Consultingwerk.Windows.Localization.Infragistics.* FROM PROPATH . 
USING Progress.Lang.*                                    FROM PROPATH .
USING System.Globalization.*                             FROM ASSEMBLY . 
USING System.Threading.*                                 FROM ASSEMBLY . 

CLASS Consultingwerk.Windows.Framework.UserInterfaceLanguageManager: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the UserInterfaceLanguageManager class
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC UserInterfaceLanguageManager ():
		SUPER ().
		
		SessionManager:LanguageIsoCodeChanged:Subscribe (LanguageIsoCodeChangedHandler) . 
				
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Initializes a Culture for the UserInterface
        Notes:   http://msdn.microsoft.com/en-us/library/system.globalization.cultureinfo.cultureinfo(v=vs.80).aspx  
        @param pcCulture The name of the culture to initialize
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID InitializeUiCulture (pcCulture AS CHARACTER):
		
		DEFINE VARIABLE oLocalizer AS ILocalizer NO-UNDO . 
		
        Thread:CurrentThread:CurrentUICulture = NEW CultureInfo (SessionManager:LanguageIsoCode) .

        MessageFormResources:InitializeFromTranslationProvider () .

        &IF DEFINED (SmartComponentLibrary) NE 0 &THEN
        SmartComponentLibraryCustomizer:InitializeFromTranslationProvider() .
        &ENDIF
        
        &IF DEFINED (SmartFramework) NE 0 &THEN
        oLocalizer = NEW SmartFrameworkLocalizer () .
        oLocalizer:Localize () .
        &ENDIF
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the LanguageIsoCodeChanged event of the SessionManager  
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID LanguageIsoCodeChangedHandler ():

        THIS-OBJECT:InitializeUiCulture (SessionManager:LanguageIsoCode) .

        CATCH err AS Progress.Lang.Error :
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.  

    END METHOD . 
    
END CLASS.
