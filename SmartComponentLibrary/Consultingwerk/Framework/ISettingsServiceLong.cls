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
    File        : ISettingsServiceLong
    Purpose     : Interface for Services allowing to store user settings
    Syntax      :
    Description : LONGCHAR Alternative to ISettingsService
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Aug 03 21:22:38 CEST 2011
    Notes       : Allows abstraction from writing to the Registry or
                  actually using a different implementation on the client
                  or on the AppServer
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

INTERFACE Consultingwerk.Framework.ISettingsServiceLong:

    /**
     * Purpose: Returns a user setting
     * Notes:
     * @param pcBaseKey The settings base key
     * @param pcKey The settings key
     * @return The setting value
     */
    METHOD PUBLIC LONGCHAR GetSettingLong (pcBaseKey AS CHARACTER,
                                           pcKey AS CHARACTER) .

    /**
     * Purpose: Stores a user settings
     * Notes:
     * @param pcBaseKey The settings base key
     * @param pcKey The settings key
     * @param pcValue The value to assign to the setting
     */
    METHOD PUBLIC VOID StoreSettingLong (pcBaseKey AS CHARACTER,
                                         pcKey AS CHARACTER,
                                         pcValue AS LONGCHAR) .

END INTERFACE.
