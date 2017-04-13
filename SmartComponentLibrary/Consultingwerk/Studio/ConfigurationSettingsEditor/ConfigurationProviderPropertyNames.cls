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
    File        : ConfigurationProviderPropertyNames
    Purpose     : Service wich provides a ValueList with all
                  Configuration Property Names
    Syntax      :
    Description :
    Author(s)   : Mark Bartscherer / Consultingwerk Ltd.
    Created     : Thu Dec 01 09:47:23 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Studio.ConfigurationSettingsEditor.* FROM PROPATH .
USING Progress.Lang.*                                     FROM PROPATH .

CLASS Consultingwerk.Studio.ConfigurationSettingsEditor.ConfigurationProviderPropertyNames
    IMPLEMENTS IConfigurationProviderPropertyNames:

    DEFINE VARIABLE oPropertyNameValueList AS Infragistics.Win.ValueList NO-UNDO.

    /**
     * Purpose: Constructor for the ConfigurationProviderPropertyNames class
     * Notes:
     */
    CONSTRUCTOR PUBLIC ConfigurationProviderPropertyNames():

        ASSIGN oPropertyNameValueList = NEW Infragistics.Win.ValueList ().

        AddPropertyName ("RestServiceName":U).
        AddPropertyName ("RestServiceAddress":U).
        AddPropertyName ("AnnotationCacheMaxAge":U).
        AddPropertyName ("frameworkLoader":U).
        AddPropertyName ("systemDescription":U).
        AddPropertyName ("BusinessEntityDescriptorCacheMaxAge":U).
        AddPropertyName ("WebLocalizationScope":U).
        AddPropertyName ("ServiceLifeCycleDefinition":U).
        AddPropertyName ("SmtpHostName":U).
        AddPropertyName ("SmtpPassword":U).
        AddPropertyName ("SmtpPortNumber":U).
        AddPropertyName ("SmtpSenderName":U).
        AddPropertyName ("SmtpUserName":U).
        AddPropertyName ("defaultPartition":U).
        AddPropertyName ("authenticationDb":U).
        AddPropertyName ("performMenuStructureSecurityCheck":U).
        AddPropertyName ("DontFetchTranslationsWhenAllTranslationsCached":U).
        AddPropertyName ("StudioServicesFile":U).
        AddPropertyName ("WebLocalizationScope":U).
        AddPropertyName ("RepositoryFieldTypes":U).

    END CONSTRUCTOR.

    /**
     * Purpose: Adds a PropertyName in a ValueList
     * Notes: PropertyName is added as DataValue and DisplayText
     * @param pcPropertyName The name of the Property
     */
    METHOD PRIVATE VOID AddPropertyName (pcPropertyName AS CHARACTER):

        oPropertyNameValueList:ValueListItems:Add (pcPropertyName, pcPropertyName).

    END METHOD.

    /**
     * Purpose: Returns a Infragistics.Win.ValueList containing the PropertyNames
     * Notes:
     * @return The Infragistics.Win.ValueList containing the PropertyNames
     */
    METHOD PUBLIC Infragistics.Win.ValueList GetPropertyNames():

        RETURN oPropertyNameValueList.

    END METHOD.

END CLASS.
