/**********************************************************************
 * Copyright (C) 2006-2012 by Consultingwerk Ltd. ("CW") -            *
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
    File        : ttTriggerEvent
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Aug 28 16:20:09 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE {&ACCESS} TEMP-TABLE ttTriggerEvent NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE ttTriggerEventBefore &ENDIF
    FIELD TriggerEvent     AS CHARACTER 
    FIELD TriggerWidget    AS CHARACTER 
    FIELD TriggerBrowse    AS CHARACTER 
    FIELD TriggerCodeGuid  AS CHARACTER  
    INDEX TriggerEvent TriggerEvent TriggerWidget TriggerBrowse
    INDEX TriggerCodeGuid TriggerCodeGuid  .
 