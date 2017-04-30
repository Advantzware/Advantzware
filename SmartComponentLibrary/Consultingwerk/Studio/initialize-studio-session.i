/**********************************************************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
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
    File        : initialize-studio-session.i
    Purpose     : Initializes a Consultingwerk Studio Session

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd. 
    Created     : Wed Nov 02 19:28:33 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE VARIABLE oConfigurationProvider AS IConfigurationProvider NO-UNDO .
DEFINE VARIABLE oContextDatasetFactory AS IContextDatasetFactory NO-UNDO .

/* ***************************  Main Block  *************************** */

&IF PROVERSION NE "10.2B" &THEN
/* JSON based config provider only from 11.x */
oConfigurationProvider = {Consultingwerk/get-service.i
                            Consultingwerk.Framework.IConfigurationProvider
                            "NEW Consultingwerk.Framework.ConfigurationProvider ('.applicationsettings':U)"} .
&ELSE
oConfigurationProvider = {Consultingwerk/get-service.i
                            Consultingwerk.Framework.IConfigurationProvider
                            "NEW Consultingwerk.Framework.XmlConfigurationProvider ('.applicationsettings.xml':U)"} .
&ENDIF

IF oConfigurationProvider:GetValue("StudioServicesFile":U, "":U) > "":U THEN DO:
    ServiceLoader:LoadFromFile (oConfigurationProvider:GetValue("StudioServicesFile":U),
                                TRUE) .

    /* Mike Fechner, Consultingwerk Ltd. 19.10.2011
       Initialize Session Context Dataset */
    oContextDatasetFactory = {Consultingwerk/get-service.i Consultingwerk.OERA.Context.IContextDatasetFactory} .

    IF VALID-OBJECT (oContextDatasetFactory) THEN
        Consultingwerk.Framework.Session.SessionManager:ContextDataset = oContextDatasetFactory:CreateContextDataset() .
END .
