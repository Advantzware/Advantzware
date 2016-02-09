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
    File        : ValidHandle.i
    Purpose     : Include File base Assertion for ValidHandle
            
    Syntax      : {Consultingwerk/Assertion/HandleAssert/ValidHandle.i hWidget """hWidget"":U"}
                  {Consultingwerk/Assertion/HandleAssert/ValidHandle.i hWidget}

    Description : Reduces runtime overhead of using the ValidHandle assertion 

    Author(s)   : 
    Created     : Thu Jun 25 21:33:03 CEST 2015
    Notes       : SCL-875
  ----------------------------------------------------------------------*/
&ENDIF


    IF NOT VALID-HANDLE ({1}) THEN 
        UNDO, THROW NEW Consultingwerk.Exceptions.InvalidHandleException ({2}) .