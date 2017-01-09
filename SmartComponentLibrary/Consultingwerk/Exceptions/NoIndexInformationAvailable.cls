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
    File        : NoIndexInformationAvailable
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Dec 03 12:02:59 CET 2013
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Exceptions.* FROM PROPATH .
USING Progress.Lang.*             FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.Exceptions.NoIndexInformationAvailable
    INHERITS Exception:

    /**
     * Purpose: Constructor of the NoIndexInformationAvailable class
     * Notes:
     * @param pcBufferName The name of the buffer associated wit the Exception
     */
    CONSTRUCTOR PUBLIC NoIndexInformationAvailable (pcBufferName AS CHARACTER):

        SUPER (SUBSTITUTE ("No index-information available for buffer: &1."{&TRAN}, pcBufferName), 0) .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor of the NoIndexInformationAvailable class
     * Notes:
     * @param pcBufferName The name of the buffer associated wit the Exception
     * @param pcErrorString The error message associated with this Exception object
     * @param piMessageNumber The error message number associated with this Exception object
     */
    CONSTRUCTOR PUBLIC NoIndexInformationAvailable (pcBufferName AS CHARACTER,
                                                    pcErrorString AS CHARACTER,
                                                    piMessageNumber AS INTEGER):

        SUPER (SUBSTITUTE ("No index-information available for buffer: &1."{&TRAN}, pcBufferName), 0) .

        THIS-OBJECT:AddMessage (pcErrorString, piMessageNumber) .

    END CONSTRUCTOR.

END CLASS.
