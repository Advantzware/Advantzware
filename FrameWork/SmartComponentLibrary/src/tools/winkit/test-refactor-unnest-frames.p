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
    File        : test-refactor-unnest-frames.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Feb 07 21:26:02 CET 2012
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

/* ***************************  Main Block  *************************** */

RUN src/tools/winkit/refactor-unnest-frames.p 
        ("C:\Work\WinKit\Trunk\ABL\src\test\nested-frames\nested-frames-test.w") .