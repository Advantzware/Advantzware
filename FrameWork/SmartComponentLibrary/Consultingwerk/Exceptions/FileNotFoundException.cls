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
    File        : FileNotFoundException
    Purpose     : Exception raised for files not found
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Oct 16 21:00:20 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Exceptions.* FROM PROPATH .
USING Progress.Lang.*             FROM PROPATH .

CLASS Consultingwerk.Exceptions.FileNotFoundException
    INHERITS Exception:

    /**
     * Purpose: Returns the File Name associated with this error
     * Notes:
     */
    DEFINE PUBLIC PROPERTY FileName AS CHARACTER NO-UNDO
    GET.
    PROTECTED SET.

    /**
     * Purpose: Constructor of the FileNotFoundException class
     * Notes:
     * @param poInnerException The reference to the original error
     * @param pcFileName The file name associated with this exception
     * @param pcErrorString The error message associated with this Exception object
     */
    CONSTRUCTOR PUBLIC FileNotFoundException (poInnerException AS Progress.Lang.Error,
                                              pcFileName AS CHARACTER,
                                              pcErrorString AS CHARACTER):
        SUPER (poInnerException, pcErrorString).

        ASSIGN THIS-OBJECT:Filename = pcFileName .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor of the FileNotFoundException class
     * Notes:
     * @param poInnerException The reference to the original error
     * @param pcFileName The file name associated with this exception
     * @param pcErrorString The error message associated with this Exception object
     * @param piMessageNumber The error message number associated with this Exception object
     */
    CONSTRUCTOR PUBLIC FileNotFoundException (poInnerException AS Progress.Lang.Error,
                                              pcFileName AS CHARACTER,
                                              pcErrorString AS CHARACTER,
                                              piMessageNumber AS INTEGER):
        SUPER (poInnerException, pcErrorString, piMessageNumber).

        ASSIGN THIS-OBJECT:Filename = pcFileName .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor of the FileNotFoundException class
     * Notes:
     * @param poInnerException The reference to the original error
     * @param pcFileName The file name associated with this exception
     */
    CONSTRUCTOR PUBLIC FileNotFoundException (poInnerException AS Progress.Lang.Error,
                                              pcFileName AS CHARACTER):
        SUPER (poInnerException).

        ASSIGN THIS-OBJECT:Filename = pcFileName .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor of the FileNotFoundException class
     * Notes:
     * @param pcErrorString The error message associated with this Exception object
     * @param pcFileName The file name associated with this exception
     */
    CONSTRUCTOR PUBLIC FileNotFoundException (pcFileName AS CHARACTER,
                                              pcErrorString AS CHARACTER):
        SUPER (pcErrorString).

        ASSIGN THIS-OBJECT:Filename = pcFileName .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor of the FileNotFoundException class
     * Notes:
     * @param pcErrorString The error message associated with this Exception object
     * @param pcFileName The file name associated with this exception
     * @param piMessageNumber The error message number associated with this Exception object
     */
    CONSTRUCTOR PUBLIC FileNotFoundException (pcFileName AS CHARACTER,
                                              pcErrorString AS CHARACTER,
                                              piMessageNumber AS INTEGER):
        SUPER (pcErrorString, piMessageNumber).

        ASSIGN THIS-OBJECT:Filename = pcFileName .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor of the FileNotFoundException class
     * Notes:
     * @param pcFileName The file name associated with this exception
     */
    CONSTRUCTOR PUBLIC FileNotFoundException (pcFileName AS CHARACTER):
        SUPER (SUBSTITUTE ("The file ~"&1~" cannot be found.":U, pcFileName), 0).

        ASSIGN THIS-OBJECT:Filename = pcFileName .

    END CONSTRUCTOR.

END CLASS.
