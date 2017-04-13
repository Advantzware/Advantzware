/**********************************************************************
 * Copyright (C) 2006-2012 by Consultingwerk Ltd. ("CW") -            *
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
    File        : DatasetException
    Purpose     : Exception raised by the DatasetHelper:ThrowDatasetErrors
                  method with the ERROR-STRING's of the ProDataset records
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Oct 21 17:23:51 CEST 2014
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Exceptions.* FROM PROPATH .
USING Progress.Lang.*             FROM PROPATH .

CLASS Consultingwerk.Exceptions.DatasetException
    INHERITS Exception
    {&SERIALIZABLE}:

    /**
     * Purpose: Constructor of the DatasetException class
     * Notes:
     * @param pcErrorString The error message associated with this Exception object
     * @param piMessageNumber The error message number associated with this Exception object
     */
    CONSTRUCTOR PUBLIC DatasetException (pcErrorString AS CHARACTER,
                                         piMessageNumber AS INTEGER):

        SUPER (INPUT pcErrorString, INPUT piMessageNumber).

    END CONSTRUCTOR.

    /**
     * Purpose: Returns the buffer name associated with the given message
     *          number
     * Notes:
     * @param piMessageNum The number of the message to return the buffer name from
     * @return The buffer name or ? when no field is associated with the message
     */
    METHOD PUBLIC CHARACTER GetBufferName (piMessageNum AS INTEGER):

        DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cBuffer  AS CHARACTER NO-UNDO .

        ASSIGN cMessage = THIS-OBJECT:GetMessage (piMessageNum) .

        IF NUM-ENTRIES (cMessage, CHR (4)) > 2 THEN
            ASSIGN cBuffer = ENTRY (3, cMessage, CHR (4)) .

        IF cBuffer > "":U THEN
            RETURN cBuffer .
        ELSE
            RETURN ? .

    END METHOD.

    /**
     * Purpose: Returns the field name associated with the given message
     *          number
     * Notes:
     * @param piMessageNum The number of the message to return the field name from
     * @return The field name or ? when no field is associated with the message
     */
    METHOD PUBLIC CHARACTER GetFieldName (piMessageNum AS INTEGER):

        DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cField   AS CHARACTER NO-UNDO .

        ASSIGN cMessage = THIS-OBJECT:GetMessage (piMessageNum) .

        IF NUM-ENTRIES (cMessage, CHR (4)) > 1 THEN
            ASSIGN cField = ENTRY (2, cMessage, CHR (4)) .

        IF cField > "":U THEN
            RETURN cField .
        ELSE
            RETURN ? .

    END METHOD.

    /**
     * Purpose: Returns the message text associated with the given message
     *          number
     * Notes:   The message text excludes the field and buffer name
     * @param piMessageNum The number of the message to return the message text from
     * @return The message text associated with the message
     */
    METHOD PUBLIC CHARACTER GetMessageText (piMessageNum AS INTEGER):

        DEFINE VARIABLE cMessage     AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cMessageText AS CHARACTER NO-UNDO .

        ASSIGN cMessage     = THIS-OBJECT:GetMessage (piMessageNum)
               cMessageText = ENTRY (1, cMessage, CHR (4)) .

        IF cMessageText > "":U THEN
            RETURN cMessageText .
        ELSE
            RETURN ? .

    END METHOD.

END CLASS.
