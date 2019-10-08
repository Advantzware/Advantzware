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
    File        : test-insert-embedfinalize-include.p
    Purpose     : 

    Syntax      :

    Description : This file is part of the WinKit MTK

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jan 18 07:04:29 CET 2012
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

/* ***************************  Main Block  *************************** */

RUN tools/winkit/insert-embedfinalize-include.p
    ("C:\Work\WinKit\Trunk\ABL\src\test\win-or-dialog\c-win2.w") .