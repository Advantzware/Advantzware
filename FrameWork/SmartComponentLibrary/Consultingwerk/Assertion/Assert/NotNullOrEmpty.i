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
    File        : NotNullOrEmpty.i
    Purpose     : Include File base Assertion for NotNullOrEmpty
            
    Syntax      : {Consultingwerk/Assertion/Assert/NotNullOrEmpty.i pcValue}
                  {Consultingwerk/Assertion/Assert/NotNullOrEmpty.i pcValue """Description"":U"}

    Description : Reduces runtime overhead of using the IsAvailable assertion 

    Author(s)   : 
    Created     : Thu Jun 25 21:33:03 CEST 2015
    Notes       : SCL-875
  ----------------------------------------------------------------------*/
&ENDIF

    DO:
        IF {1} > "":U THEN . 
        ELSE  
            UNDO, THROW NEW Consultingwerk.Exceptions.InvalidValueException (STRING({1}) &IF "{2}" NE "" &THEN , {2} &ENDIF) .
    END.