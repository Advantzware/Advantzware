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
    File        : validate-default-message.i
    Purpose     : Handling of defualt message for the Validate class

    Syntax      : Parameters:
                  {1} MessagePropertyName (ValidationDefaultMessage class)
                  {2} MessageId (in the VALMSG message group)
                  {3} Substitution values for the message, comma delimited
                        first entry:        field value
                        second entry:       reference value (e.g. EQ) or first value for a range
                        third entry:        field label
                        additional entries: further substitute values, e.g. fourth entry may be high-limit of in-range

    Description :

    Author(s)   :
    Created     : Sat Sep 03 13:44:35 CEST 2016
    Notes       : SCL-1425
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

            IF pcErrorMessage = "":U OR pcErrorMessage = ? THEN DO:
                &IF DEFINED (SmartFramework) NE 0 &THEN
                ASSIGN pcErrorMessage = MessageFormatter:GetMessage ("VALMSG":U, {2}, {3}) .
                &ELSE
                ASSIGN pcErrorMessage = SUBSTITUTE (ValidationDefaultMessages:{1}, {3}) .
                &ENDIF
            END.