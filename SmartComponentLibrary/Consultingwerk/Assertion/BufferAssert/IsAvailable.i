&IF 1=0 &THEN
/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : IsAvailable.i
    Purpose     : Include File base Assertion for IsAvailable

    Syntax      : {Consultingwerk/Assertion/BufferAssert/IsAvailable.i hBuffer}
                  {Consultingwerk/Assertion/BufferAssert/IsAvailable.i "BUFFER Customer:HANDLE"}

    Description : Reduces runtime overhead of using the IsAvailable assertion

    Author(s)   :
    Created     : Thu Jun 25 21:33:03 CEST 2015
    Notes       : SCL-875
  ----------------------------------------------------------------------*/
&ENDIF

    DO:
        /* Mike Fechner, Consultingwerk Ltd. 23.06.2015
           As this is fairly frequently called, don't call into ValidHandle */
        IF NOT VALID-HANDLE ({1}) THEN
            UNDO, THROW NEW Consultingwerk.Exceptions.InvalidHandleException ("BUFFER":U) .

        IF {1}:TYPE <> "BUFFER":U THEN
            UNDO, THROW NEW Consultingwerk.Exceptions.InvalidTypeException ("BUFFER":U, {1}:TYPE) .

        IF NOT {1}:AVAILABLE THEN
            UNDO, THROW NEW Consultingwerk.Assertion.AssertException (SUBSTITUTE ("No record is available in buffer &1."{&TRAN},
                                                                                  {1}:NAME),
                                                                      0) .
    END.