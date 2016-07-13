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
        {Consultingwerk/Assertion/HandleAssert/WidgetType.i "{1}" Consultingwerk.WidgetTypeEnum:Buffer} .
    
        IF NOT {1}:AVAILABLE THEN 
            UNDO, THROW NEW Consultingwerk.Assertion.AssertException (SUBSTITUTE ("No record is available in buffer &1."{&TRAN},
                                                                                  {1}:NAME),
                                                                      0) .  
    END.