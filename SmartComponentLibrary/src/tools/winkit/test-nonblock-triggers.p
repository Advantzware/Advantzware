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
    File        : test-nonblock-triggers.p
    Purpose     : 

    Syntax      :

    Description : This file is part of the WinKit MTK

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jun 29 22:49:02 CEST 2011
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

DEFINE VARIABLE oFiles AS "System.String[]" NO-UNDO .
DEFINE VARIABLE oFiles1 AS "System.String[]" NO-UNDO .
DEFINE VARIABLE oFiles2 AS "System.String[]" NO-UNDO .

/* ***************************  Main Block  *************************** */

/*oFiles1 = System.IO.Directory:GetFiles ("c:\work\WinKit\Trunk\ABL",                                                             */
/*                                        "*.p",                                                                                  */
/*                                        System.IO.SearchOption:AllDirectories) .                                                */
/*                                                                                                                                */
/*oFiles2 = System.IO.Directory:GetFiles ("c:\work\WinKit\Trunk\ABL",                                                             */
/*                                        "*.w",                                                                                  */
/*                                        System.IO.SearchOption:AllDirectories) .                                                */
/*                                                                                                                                */
/*oFiles = CAST (System.Array:CreateInstance (Progress.Util.TypeHelper:GetType("System.String"), oFiles1:Length + oFiles2:Length),*/
/*               "System.String[]") .                                                                                             */
/*                                                                                                                                */
/*oFiles1:CopyTo(oFiles, 0) .                                                                                                     */
/*oFiles2:CopyTo(oFiles, oFiles1:Length) .                                                                                        */

oFiles = {Consultingwerk/new-array.i System.String 2} .

oFiles:SetValue (BOX ("C:\Work\WinKit\Trunk\ABL\src\test\win-or-dialog\c-win.w"), 0) .
oFiles:SetValue (BOX ("C:\Work\WinKit\Trunk\ABL\src\test\win-or-dialog\dialog-f.w"), 1) . 

RUN tools/winkit/find-nonblock-triggers.p (oFiles, "c:\temp\non-block-trigger.xml",
                                                   "c:\temp\non-block-trigger.log") . 

