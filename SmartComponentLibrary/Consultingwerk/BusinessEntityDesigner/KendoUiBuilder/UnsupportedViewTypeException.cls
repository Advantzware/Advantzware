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
    File        : UnsupportedViewType
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Oct 03 07:42:47 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Exceptions.*                            FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.KendoUiBuilder.* FROM PROPATH .
USING Progress.Lang.*                                        FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.KendoUiBuilder.UnsupportedViewTypeException
    INHERITS Exception:

    /**
     * Purpose: Gets and sets the type of the view
     * Notes:
     */
    DEFINE PUBLIC PROPERTY ViewType AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Constructor of the UnsupportedViewTypeException class
     * Notes:
     * @param poInnerException The reference to the original error
     * @param pcErrorString The error message associated with this Exception object
     * @param pcViewType The value for the ViewType property
     */
    CONSTRUCTOR PUBLIC UnsupportedViewTypeException (poInnerException AS Progress.Lang.Error,
                                                     pcErrorString AS CHARACTER,
                                                     pcViewType AS CHARACTER):

        SUPER (poInnerException, pcErrorString).

        ASSIGN THIS-OBJECT:ViewType = pcViewType .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor of the UnsupportedViewTypeException class
     * Notes:
     * @param poInnerException The reference to the original error
     * @param pcErrorString The error message associated with this Exception object
     * @param piMessageNumber The error message number associated with this Exception object
     * @param pcViewType The value for the ViewType property
     */
    CONSTRUCTOR PUBLIC UnsupportedViewTypeException (poInnerException AS Progress.Lang.Error,
                                                     pcErrorString AS CHARACTER,
                                                     piMessageNumber AS INTEGER,
                                                     pcViewType AS CHARACTER):

        SUPER (poInnerException, pcErrorString, piMessageNumber).

        ASSIGN THIS-OBJECT:ViewType = pcViewType .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor of the UnsupportedViewTypeException class
     * Notes:
     * @param poInnerException The reference to the original error
     * @param pcViewType The value for the ViewType property
     */
    CONSTRUCTOR PUBLIC UnsupportedViewTypeException (poInnerException AS Progress.Lang.Error,
                                                     pcViewType AS CHARACTER):
        SUPER (poInnerException).

        ASSIGN THIS-OBJECT:ViewType = pcViewType .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor of the UnsupportedViewTypeException class
     * Notes:
     * @param pcErrorString The error message associated with this Exception object
     * @param pcViewType The value for the ViewType property
     */
    CONSTRUCTOR PUBLIC UnsupportedViewTypeException (pcErrorString AS CHARACTER,
                                                     pcViewType AS CHARACTER):
        SUPER (pcErrorString).

        ASSIGN THIS-OBJECT:ViewType = pcViewType .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor of the UnsupportedViewTypeException class
     * Notes:
     * @param pcErrorString The error message associated with this Exception object
     * @param piMessageNumber The error message number associated with this Exception object
     * @param pcViewType The value for the ViewType property
     */
    CONSTRUCTOR PUBLIC UnsupportedViewTypeException (pcErrorString AS CHARACTER,
                                                     piMessageNumber AS INTEGER,
                                                     pcViewType AS CHARACTER):

        SUPER (INPUT pcErrorString, INPUT piMessageNumber).

        ASSIGN THIS-OBJECT:ViewType = pcViewType .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor of the UnsupportedViewTypeException class
     * Notes:
     */
    CONSTRUCTOR PUBLIC UnsupportedViewTypeException ():
        SUPER ().

    END CONSTRUCTOR.

END CLASS.
