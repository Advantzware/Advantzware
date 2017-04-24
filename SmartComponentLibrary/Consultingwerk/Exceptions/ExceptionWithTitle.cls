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
    File        : ExceptionWithTitle
    Purpose     : Common base error class, implementing inner exception
                  handling / access, returning the Error instance
                  that caused the current exception.
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Feb 20 20:19:00 CET 2008
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW .

{Consultingwerk/products.i}

USING Consultingwerk.Exceptions.* FROM PROPATH .
USING Progress.Lang.*             FROM PROPATH .

CLASS Consultingwerk.Exceptions.ExceptionWithTitle
    INHERITS Exception
    IMPLEMENTS ISupportsInnerException, IErrorTitle
    {&SERIALIZABLE}:

    /**
     * Purpose: Returns the Title associated with the Exception
     * Notes:
     */
    DEFINE PUBLIC PROPERTY Title AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Constructor of the ExceptionWithTitle class
     * Notes:
     * @param pcTitle The title for the Exception
     */
    CONSTRUCTOR PUBLIC ExceptionWithTitle (pcTitle AS CHARACTER):

        SUPER ().

        ASSIGN THIS-OBJECT:Title = pcTitle .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor of the ExceptionWithTitle class
     * Notes:
     * @param pcErrorString The error message associated with this ExceptionWithTitle object
     * @param piMessageNumber The error message number associated with this ExceptionWithTitle object
     * @param pcTitle The title for the Exception
     */
    CONSTRUCTOR PUBLIC ExceptionWithTitle (pcErrorString AS CHARACTER,
                                           piMessageNumber AS INTEGER,
                                           pcTitle AS CHARACTER):

        SUPER (pcErrorString, piMessageNumber).

        ASSIGN THIS-OBJECT:Title = pcTitle .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor of the ExceptionWithTitle class
     * Notes:
     * @param pcErrorString The error message associated with this ExceptionWithTitle object
     * @param pcTitle The title for the Exception
     */
    CONSTRUCTOR PUBLIC ExceptionWithTitle (pcErrorString AS CHARACTER,
                                           pcTitle AS CHARACTER):

        SUPER (pcErrorString).

        ASSIGN THIS-OBJECT:Title = pcTitle .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor of the ExceptionWithTitle class
     * Notes:
     * @param poInnerException The reference to the original error
     * @param pcTitle The title for the Exception
     */
    CONSTRUCTOR PUBLIC ExceptionWithTitle (poInnerException AS Error,
                                           pcTitle AS CHARACTER):

        SUPER (poInnerException).

        ASSIGN THIS-OBJECT:Title          = pcTitle .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor of the ExceptionWithTitle class
     * Notes:
     * @param poInnerException The reference to the original error
     * @param pcErrorString The error message associated with this ExceptionWithTitle object
     * @param piMessageNumber The error message number associated with this Exception object
     * @param pcTitle The title for the Exception
     */
    CONSTRUCTOR PUBLIC ExceptionWithTitle (poInnerException AS Error,
                                           pcErrorString AS CHARACTER,
                                           piMessageNumber AS INTEGER,
                                           pcTitle AS CHARACTER):

        SUPER (poInnerException, pcErrorString, piMessageNumber).

        THIS-OBJECT:Title = pcTitle .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor of the ExceptionWithTitle class
     * Notes:
     * @param poInnerException The reference to the original error
     * @param pcErrorString The error message associated with this Exception object
     * @param pcTitle The title for the Exception
     */
    CONSTRUCTOR PUBLIC ExceptionWithTitle (poInnerException AS Error,
                                           pcErrorString AS CHARACTER,
                                           pcTitle AS CHARACTER):

        SUPER (poInnerException, pcErrorString).

        ASSIGN THIS-OBJECT:Title          = pcTitle .

    END CONSTRUCTOR.

END CLASS.
