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
    File        : _idestartup.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Sat Feb 06 00:00:26 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&IF DEFINED(execProgram) EQ 0 &THEN
&SCOPED-DEFINE execProgram mainmenu.
&ENDIF

{methods/defines/globdefs.i &NEW="NEW GLOBAL"}
{methods/defines/hndldefs.i &NEW="NEW"}

DEFINE NEW SHARED VARIABLE quit_login AS LOGICAL NO-UNDO.
DEFINE VARIABLE m_id AS CHAR NO-UNDO.
DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEF NEW GLOBAL SHARED VAR g-sharpshooter AS LOG NO-UNDO.  /* no, it's yes only from sharpsh.p */

/* ***************************  Main Block  *************************** */

g-sharpshooter = NO.

ASSIGN
  ldummy = SESSION:SET-WAIT-STATE("GENERAL")
  g_version = "2.1A-8.2A".

