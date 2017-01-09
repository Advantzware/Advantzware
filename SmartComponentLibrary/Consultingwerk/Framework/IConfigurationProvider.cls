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
    File        : IConfigurationProvider
    Purpose     : Describes methods of a class tham can return configuration
                  data (name/value pairs)
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jan 20 16:19:33 CET 2013
    Notes       :
  ----------------------------------------------------------------------*/

USING Consultingwerk.Framework.* FROM PROPATH .
USING Progress.Lang.*            FROM PROPATH.

INTERFACE Consultingwerk.Framework.IConfigurationProvider:

    /**
     * Purpose: Returns a configuration setting
     * Notes:
     * @param pcName The configuration property name
     * @return The value for the configuration property or ? when not set
     */
    METHOD PUBLIC CHARACTER GetValue (pcName AS CHARACTER).

    /**
     * Purpose: Returns a configuration setting or the default value when the setting
     *          is not available
     * Notes:
     * @param pcName The configuration property name
     * @param pcDefault The default value to return when the setting is not availalbe
     * @return The value for the configuration property or ? when not set
     */
    METHOD PUBLIC CHARACTER GetValue (pcName AS CHARACTER,
                                      pcDefault AS CHARACTER).

END INTERFACE.