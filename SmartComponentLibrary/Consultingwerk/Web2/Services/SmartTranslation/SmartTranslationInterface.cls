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
    File        : SmartTranslationInterface
    Purpose     : Service Interface for return all keyed Translations 
                  to the SmartComponents.Web2 browser
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jul 19 10:26:30 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

@openapi.openedge.export FILE(type="REST", executionMode="singleton", useReturnValue="false", writeDataSetBeforeImage="false").

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Assertion.*                      FROM PROPATH .
USING Consultingwerk.Framework.*                      FROM PROPATH .
USING Consultingwerk.OERA.*                           FROM PROPATH .
USING Consultingwerk.SmartFramework.System.*          FROM PROPATH .
USING Consultingwerk.Web2.Services.SmartTranslation.* FROM PROPATH .  
USING Consultingwerk.Util.*                           FROM PROPATH .
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.Web2.Services.SmartTranslation.SmartTranslationInterface: 

    { Consultingwerk/SmartFramework/System/dsTranslation.i }

    DEFINE VARIABLE oConfig AS IConfigurationProvider NO-UNDO . 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartTranslationInterface class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartTranslationInterface ():
        
        DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.

        SUPER ().

        FileAssert:Exists (".restapplicationsettings":U) .
        
        ASSIGN cFileName = FileHelper:FindFile (".restapplicationsettings":U) .
        
        ASSIGN oConfig = {Consultingwerk/get-service.i Consultingwerk.Framework.IConfigurationProvider
                                                       "NEW ConfigurationProvider (cFileName)"} .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Returns all translations in the given language, and the system 
                 default language (empty langauge key) to the browser
        Notes:   
        @param pcLanguageKey The Langauge Key to return
        @param dsTranslation OUTPUT Dataset with translations
    ------------------------------------------------------------------------------*/
    @openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
    METHOD PUBLIC VOID GetTranslations (pcLanguageKey AS CHARACTER,
                                        OUTPUT DATASET dsTranslation):
        
        DEFINE BUFFER beSmartTranslation FOR eSmartTranslation . 
        
        DEFINE VARIABLE oRequest AS FetchDataRequest NO-UNDO . 
        
        oRequest = NEW FetchDataRequest ("eSmartTranslation":U,
                                         SUBSTITUTE ("for each eSmartTranslation WHERE eSmartTranslation.TranslationKey > '' AND eSmartTranslation.LanguageGuid = &1 ~
                                                                                    OR eSmartTranslation.TranslationKey > '' AND eSmartTranslation.LanguageGuid = ''":U,
                                                     QUOTER (pcLanguageKey))) . 
        
        ServiceInterface:FetchData ("Consultingwerk.SmartFramework.System.TranslationBusinessEntity":U,
                                    oRequest,
                                    OUTPUT DATASET dsTranslation)  .        
        
        FOR EACH eSmartTranslation WHERE eSmartTranslation.LanguageGuid = "":U 
                                     AND CAN-FIND (beSmartTranslation WHERE beSmartTranslation.TranslationScope = eSmartTranslation.TranslationScope
                                                                        AND beSmartTranslation.TranslationKey   = eSmartTranslation.TranslationKey
                                                                        AND beSmartTranslation.LanguageGuid     = pcLanguageKey):  

            DELETE eSmartTranslation .
        
        END.
        
    END METHOD .

END CLASS.
