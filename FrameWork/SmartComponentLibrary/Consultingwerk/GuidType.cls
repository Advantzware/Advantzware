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
    File        : GuidType
    Purpose     : Provides helper functions for GUID types
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue May 20 14:07:46 CEST 2014
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.* FROM PROPATH .
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.GuidType:

    /*------------------------------------------------------------------------------
        Purpose: Private constructor for the GuidType class
        Notes:   Disallow instance creation
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE GuidType ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Returns a new 36 character GUID value
        Notes:
        @return The 36 character GUID value
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER NewGuid ():

        RETURN GUID .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the 22 character BASE64 representation of a new UUID value
        Notes:   The BASE64 encoded UUID representation is shorter compared to the
                 original GUID value. However, it can only be used for unique keys,
                 when the comparison is may case-sensitive. BASE64 may return the
                 same value (only distinguished by the case) for two different UUID
                 values!
        @return The 22 character BASE64 representation of a new UUID value
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER NewUuid ():

        DEFINE VARIABLE rawUUID AS RAW       NO-UNDO .
        DEFINE VARIABLE cBase64 AS CHARACTER NO-UNDO .

        ASSIGN
            rawUUID = GENERATE-UUID
            cBase64 = BASE64-ENCODE(rawUUID).

        RETURN SUBSTRING (cBase64, 1, 22, "CHARACTER":U) .

    END METHOD .

END CLASS.
