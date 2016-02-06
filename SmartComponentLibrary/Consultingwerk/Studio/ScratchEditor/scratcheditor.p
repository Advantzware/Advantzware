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
    File        : scratcheditor.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Oct 25 09:32:29 CEST 2012
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{adecomm/oeideservice.i}

/* ***************************  Main Block  *************************** */

FILE-INFO:FILE-NAME = "Consultingwerk/Studio/ScratchEditor/empty.p":U .

IF FILE-INFO:FULL-PATHNAME > "":U THEN . 
ELSE DO:
    MESSAGE "Unable to find scratch editor template empty.p":U
        VIEW-AS ALERT-BOX ERROR .
    
    RETURN . 
END.

openEditor  
     (?,
      FILE-INFO:FULL-PATHNAME,
      "UNTITLED":U,
      ?) .
      