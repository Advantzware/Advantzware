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
    File        : MessageSubSystem
    Purpose     : Holder class for LogManager message sub system
    Syntax      : Represents a custom log entry type. Simplifies signatures
                  of the LogManager:WriteMessage... methods as this type
                  allows specific signatures for passing the custom logging
                  types
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon May 05 20:13:39 CEST 2014
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                FROM PROPATH .
USING Consultingwerk.Framework.Enum.* FROM PROPATH .
USING Consultingwerk.Util.*           FROM PROPATH .
USING Progress.Lang.*                 FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.Util.MessageSubSystem
    INHERITS CharacterHolder:

    /**
     * Purpose: Gets and sets the Logging Level associated with this
     *          Custom Log entry type
     * Notes:
     */
    DEFINE PUBLIC PROPERTY LoggingLevel AS LoggingLevelEnum NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Constructor for the MessageSubSystem class
     * Notes:
     * @param pcMessageSubSys The value for the Message Sub System
     * @param poLoggingLevel The LoggingLevelEnum value (None, Errors, Basic, Verbose, Extended)
     */
    CONSTRUCTOR PUBLIC MessageSubSystem (pcMessageSubSys AS CHARACTER,
                                         poLoggingLevel AS LoggingLevelEnum):
        SUPER (pcMessageSubSys).

        ASSIGN THIS-OBJECT:LoggingLevel = poLoggingLevel .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the MessageSubSystem class
     * Notes:
     * @param pcMessageSubSys The value for the Message Sub System
     */
    CONSTRUCTOR PUBLIC MessageSubSystem (pcMessageSubSys AS CHARACTER):
        SUPER (pcMessageSubSys).

    END CONSTRUCTOR.

    /**
     * Purpose: Returns a Character representation of the value
     * Notes:
     * @return The Character representation of the value
     */
    METHOD OVERRIDE PUBLIC CHARACTER ToString ():

        IF VALID-OBJECT (THIS-OBJECT:LoggingLevel) THEN
            RETURN SUBSTITUTE ("&1:&2":U,
                               THIS-OBJECT:Value,
                               THIS-OBJECT:LoggingLevel:GetValue()) .
        ELSE
            RETURN THIS-OBJECT:Value .

    END METHOD .

END CLASS.
