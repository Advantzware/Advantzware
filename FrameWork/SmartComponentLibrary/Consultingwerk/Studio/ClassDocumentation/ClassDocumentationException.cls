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
    File        : ClassDocumentationException
    Purpose     : Exception thrown by the class documentation routines
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon May 09 19:49:14 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Exceptions.*                FROM PROPATH .
USING Consultingwerk.Studio.ClassDocumentation.* FROM PROPATH .
USING Progress.Lang.*                            FROM PROPATH .

CLASS Consultingwerk.Studio.ClassDocumentation.ClassDocumentationException
    INHERITS Exception:

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the name of the file that caused the Exception
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY FileName AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the ClassDocumentationException class
        Notes:
        @param poInnerException The reference to the original error
        @param pcErrorString The error message associated with this Exception object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ClassDocumentationException (poInnerException AS Progress.Lang.Error,
                                                    pcErrorString AS CHARACTER):
        SUPER (poInnerException, pcErrorString).

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the ClassDocumentationException class
        Notes:
        @param poInnerException The reference to the original error
        @param pcErrorString The error message associated with this Exception object
        @param piMessageNumber The error message number associated with this Exception object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ClassDocumentationException (poInnerException AS Progress.Lang.Error,
                                                    pcErrorString AS CHARACTER,
                                                    piMessageNumber AS INTEGER):
        SUPER (poInnerException, pcErrorString, piMessageNumber).

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the ClassDocumentationException class
        Notes:
        @param poInnerException The reference to the original error
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ClassDocumentationException (poInnerException AS Progress.Lang.Error):
        SUPER (poInnerException).

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the ClassDocumentationException class
        Notes:
        @param pcErrorString The error message associated with this Exception object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ClassDocumentationException (pcErrorString AS CHARACTER):
        SUPER (pcErrorString).

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the ClassDocumentationException class
        Notes:
        @param pcErrorString The error message associated with this Exception object
        @param piMessageNumber The error message number associated with this Exception object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ClassDocumentationException (pcErrorString AS CHARACTER,
                                                    piMessageNumber AS INTEGER):
        SUPER (pcErrorString, piMessageNumber).

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the ClassDocumentationException class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ClassDocumentationException ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the ClassDocumentationException class
        Notes:
        @param poInnerException The reference to the original error
        @param pcErrorString The error message associated with this Exception object
        @param pcFileName The value for the FileName property
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ClassDocumentationException (poInnerException AS Progress.Lang.Error,
                                                    pcErrorString AS CHARACTER,
                                                    pcFileName AS CHARACTER):
        SUPER (poInnerException, pcErrorString).

        THIS-OBJECT:FileName = pcFileName .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the ClassDocumentationException class
        Notes:
        @param poInnerException The reference to the original error
        @param pcErrorString The error message associated with this Exception object
        @param piMessageNumber The error message number associated with this Exception object
        @param pcFileName The value for the FileName property
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ClassDocumentationException (poInnerException AS Progress.Lang.Error,
                                                    pcErrorString AS CHARACTER,
                                                    piMessageNumber AS INTEGER,
                                                    pcFileName AS CHARACTER):
        SUPER (poInnerException, pcErrorString, piMessageNumber).

        THIS-OBJECT:FileName = pcFileName .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the ClassDocumentationException class
        Notes:
        @param pcErrorString The error message associated with this Exception object
        @param pcFileName The value for the FileName property
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ClassDocumentationException (pcErrorString AS CHARACTER,
                                                    pcFileName AS CHARACTER):
        SUPER (pcErrorString).

        ASSIGN THIS-OBJECT:FileName = pcFileName .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the ClassDocumentationException class
        Notes:
        @param pcErrorString The error message associated with this Exception object
        @param piMessageNumber The error message number associated with this Exception object
        @param pcFileName The value for the FileName property
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ClassDocumentationException (pcErrorString AS CHARACTER,
                                                    piMessageNumber AS INTEGER,
                                                    pcFileName AS CHARACTER):
        SUPER (pcErrorString, piMessageNumber).

        ASSIGN THIS-OBJECT:FileName = pcFileName .

    END CONSTRUCTOR.

END CLASS.
