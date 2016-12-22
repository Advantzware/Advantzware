&IF 1=0 &THEN
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
    File        : debug.i
    Purpose     : Turns on or of compile time option for including 
                  debug only code

    Syntax      : Include file

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Feb 12 17:3509:47 CET 2011
    Notes       : 
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
&ENDIF   

&IF 0=1 &THEN
&GLOBAL-DEFINE DEBUG
&GLOBAL-DEFINE DebugCustomTypeDescriptor
&GLOBAL-DEFINE DebugPropertyAttributes System.ComponentModel.DefaultValueAttribute
&GLOBAL-DEFINE DebugLogFile "../debug.log"
&ENDIF