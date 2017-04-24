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
    File        : CompileException
    Purpose     : Exception thrown from the CompileHelper:CompileFile method
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Apr 23 22:41:20 CEST 2013
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Exceptions.* FROM PROPATH .
USING Progress.Lang.*             FROM PROPATH .

CLASS Consultingwerk.Exceptions.CompileException
    INHERITS Exception
    IMPLEMENTS IErrorTitle
    {&SERIALIZABLE}:

    /**
     * Purpose: Gets and sets the FileName that was compiled
     * Notes:
     */
    DEFINE PUBLIC PROPERTY FileName AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Returns the Title to be used in the Error Dialog or Message Box
     * Notes:
     */
    DEFINE PUBLIC PROPERTY Title AS CHARACTER NO-UNDO INITIAL "Error during compilation"{&TRAN}
    GET.
    SET.

    /**
     * Purpose: Constructor of the Exception class
     * Notes:
     * @param poInnerException The reference to the original error
     * @param pcErrorString The error message associated with this Exception object
     */
    CONSTRUCTOR PUBLIC CompileException (poInnerException AS Progress.Lang.Error,
                                         pcErrorString AS CHARACTER):
        SUPER (poInnerException,
               pcErrorString).

    END CONSTRUCTOR.


    /**
     * Purpose: Constructor of the Exception class
     * Notes:
     * @param poInnerException The reference to the original error
     * @param pcErrorString The error message associated with this Exception object
     * @param piMessageNumber The error message number associated with this Exception object
     */
    CONSTRUCTOR PUBLIC CompileException (poInnerException AS Progress.Lang.Error,
                                         pcErrorString AS CHARACTER,
                                         piMessageNumber AS INTEGER):
        SUPER (poInnerException,
               pcErrorString,
               piMessageNumber).

    END CONSTRUCTOR.


    /**
     * Purpose: Constructor of the Exception class
     * Notes:
     * @param poInnerException The reference to the original error
     */
    CONSTRUCTOR PUBLIC CompileException (poInnerException AS Progress.Lang.Error):
        SUPER (poInnerException).

    END CONSTRUCTOR.


    /**
     * Purpose: Constructor of the Exception class
     * Notes:
     * @param pcErrorString The error message associated with this Exception object
     */
    CONSTRUCTOR PUBLIC CompileException (pcErrorString AS CHARACTER):
        SUPER (pcErrorString).

    END CONSTRUCTOR.


    /**
     * Purpose: Constructor of the Exception class
     * Notes:
     * @param pcErrorString The error message associated with this Exception object
     * @param piMessageNumber The error message number associated with this Exception object
     */
    CONSTRUCTOR PUBLIC CompileException (pcErrorString AS CHARACTER,
                                         piMessageNumber AS INTEGER):
        SUPER (pcErrorString,
               piMessageNumber).

    END CONSTRUCTOR.


    /**
     * Purpose: Constructor of the Exception class
     * Notes:
     */
    CONSTRUCTOR PUBLIC CompileException ():
        SUPER ().

    END CONSTRUCTOR.

END CLASS.
