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
    File        : FrameworkBootstrap
    Purpose     : Provides interarction with a framework bootstrap
    Syntax      : 
    Description : Default IFrameworkBootstrap implementation 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat May 04 23:21:10 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*           FROM PROPATH . 
USING Consultingwerk.Framework.* FROM PROPATH .
USING Consultingwerk.Util.*      FROM PROPATH .  
USING Progress.Lang.*            FROM PROPATH .


CLASS Consultingwerk.Framework.FrameworkBootstrap
    IMPLEMENTS IFrameworkBootstrap: 

    /*------------------------------------------------------------------------------
        Purpose: Returns logical value indicating if the framework bootstrap has
                 already been executed                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY IsInitialized AS LOGICAL NO-UNDO INITIAL FALSE
    GET.
    PRIVATE SET .

    /*------------------------------------------------------------------------------
        Purpose: Executes the framework bootstrap                                                                     
        Notes:   The IsInitialized method will return True after the bootstrap has 
                 been executed                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Initialize ():    

        DEFINE VARIABLE oConfigurationProvider AS IConfigurationProvider NO-UNDO . 
        DEFINE VARIABLE oFrameworkLoader       AS IFrameworkLoader       NO-UNDO . 
        DEFINE VARIABLE cFrameworkLoader       AS CHARACTER              NO-UNDO .

        &IF PROVERSION NE "10.2B" &THEN
        IF FileHelper:Exists ('.applicationsettings':U) THEN
            /* Jsonbased config provider only from 11.x */
            oConfigurationProvider = {Consultingwerk/get-service.i
                                        Consultingwerk.Framework.IConfigurationProvider
                                        "NEW Consultingwerk.Framework.ConfigurationProvider ('.applicationsettings':U)"} .
        &ELSE
        IF FileHelper:Exists ('.applicationsettings.xml':U) THEN 
            /* XML based config provider for 10.2B */
            oConfigurationProvider = {Consultingwerk/get-service.i 
                                        Consultingwerk.Framework.IConfigurationProvider 
                                        "NEW Consultingwerk.Framework.XmlConfigurationProvider ('.applicationsettings.xml':U)"} .  
        &ENDIF

        IF VALID-OBJECT (oConfigurationProvider) THEN DO:
            
            cFrameworkLoader = oConfigurationProvider:GetValue ("frameworkLoader":U) .
            
            IF CharacterType:IsNotNullOrEmpty (cFrameworkLoader) THEN DO:
            
                oFrameworkLoader = DYNAMIC-NEW (cFrameworkLoader) () .

                oFrameworkLoader:Initialize (oConfigurationProvider) .
            END.
        END.

        THIS-OBJECT:IsInitialized = TRUE . 

    END METHOD .

END CLASS.
