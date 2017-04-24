/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : _copylob.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Thu May 22 15:52:22 CEST 2014
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE INPUT  PARAMETER pcFileName AS CHARACTER NO-UNDO.

/* ***************************  Main Block  *************************** */

DEFINE SHARED BUFFER {1} FOR DICTDB2.{1} .

COPY-LOB FROM {1}.{2} TO FILE pcFileName . 
