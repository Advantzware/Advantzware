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
    File        : IConfigurationProviderPropertyNames
    Purpose     : Interface for ConfigurationProviderPropertyNames implementations
    Syntax      :
    Description :
    Author(s)   : Mark Bartscherer / Consultingwerk Ltd.
    Created     : Thu Dec 01 08:33:20 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

INTERFACE Consultingwerk.Studio.ConfigurationSettingsEditor.IConfigurationProviderPropertyNames:

    /**
     * Purpose: Returns a Infragistics.Win.ValueList containing the PropertyNames
     * Notes:
     * @return The Infragistics.Win.ValueList containing the PropertyNames
     */
    METHOD PUBLIC Infragistics.Win.ValueList GetPropertyNames().

END INTERFACE.