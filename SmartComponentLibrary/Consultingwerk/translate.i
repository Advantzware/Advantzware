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
    File        : translate.i
    Purpose     : Provides String Translation based on an original string

    Syntax      : Static String: {Consultingwerk/translate.i &scope="''" &string="'Question':U"}
                  Variable:      {Consultingwerk/translate.i &scope=cScope &string=cString}
                  No scope:      {Consultingwerk/translate.i &string="'Question':U"}

                  By key:        {Consultingwerk/translate.i &scope="''" &key="'xyz':U"}

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Jan 11 00:33:45 CET 2013
  ----------------------------------------------------------------------*/

&IF DEFINED (string) NE 0 &THEN
(IF NOT VALID-OBJECT ({Consultingwerk/get-service.i Consultingwerk.SmartFramework.ITranslationProvider}) THEN 
    {&string}
ELSE 
    {Consultingwerk/get-service.i Consultingwerk.SmartFramework.ITranslationProvider}:GetTranslationByString (
    &IF DEFINED (scope) NE 0 &THEN {&scope} &ELSE "":U &ENDIF , 
    {&string} ))    
&ELSE
(IF NOT VALID-OBJECT ({Consultingwerk/get-service.i Consultingwerk.SmartFramework.ITranslationProvider}) THEN 
    ?
ELSE 
    {Consultingwerk/get-service.i Consultingwerk.SmartFramework.ITranslationProvider}:GetTranslationByKey (
    &IF DEFINED (scope) NE 0 &THEN {&scope} &ELSE "":U &ENDIF , 
    {&key} ))    
&ENDIF    