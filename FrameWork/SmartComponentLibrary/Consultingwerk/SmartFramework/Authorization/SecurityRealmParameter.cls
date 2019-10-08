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
    File        : SecurityRealmParameter
    Purpose     : Parameter object for remote routines that require a
                  Security Realm input parameter
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwekr Ltd.
    Created     : Sat Jul 16 11:56:49 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                              FROM PROPATH .
USING Consultingwerk.SmartFramework.Authorization.* FROM PROPATH .
USING Progress.Lang.*                               FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Authorization.SecurityRealmParameter
    INHERITS CharacterHolder:

    /**
     * Purpose: Constructor for the SecurityRealmParameter
     * Notes:
     * @param pcRealmCode The code of the security realm to verify
     */
    CONSTRUCTOR PUBLIC SecurityRealmParameter (pcRealmCode AS CHARACTER):
        SUPER (pcRealmCode).

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the SecurityRealmParameter
     * Notes:
     */
    CONSTRUCTOR PUBLIC SecurityRealmParameter ():
        SUPER ().

    END CONSTRUCTOR.

END CLASS.
