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
    File        : proSIvalidateEntityName.p
    Purpose     : 

    Syntax      :

    Description : Validates a Business Entity Name

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Apr 22 09:07:52 CEST 2010
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

DEFINE INPUT  PARAMETER pcEntityName AS CHARACTER NO-UNDO .
DEFINE OUTPUT PARAMETER plValid      AS LOGICAL   NO-UNDO .

{ Consultingwerk/products.i }

/* Mike Fechner, Consultingwerk Ltd. 08.05.2013
   Support for custom include files to the proSI... procedures. 
   This allows adding SHARED variable definitions that may be 
   required to execute legacy database triggers */
&IF "{&ProSIcustomIncludeDirectory}":U NE "":U &THEN
{ {&ProSIcustomIncludeDirectory}/proSIvalidateEntityNameCustom.i }
&ENDIF

/* ***************************  Main Block  *************************** */

&IF DEFINED (ExcludeProceduralOERA) EQ 0 &THEN 
IF NUM-ENTRIES (pcEntityName, ".":U) > 1 THEN 
&ENDIF
    ASSIGN plValid = Consultingwerk.OERA.ServiceInterface:ValidateBusinessEntityName (pcEntityName) .
&IF DEFINED (ExcludeProceduralOERA) EQ 0 &THEN 
ELSE DO:
    { {&OERASI}/launchserviceinterface.i }
    
    /* Currently not validating procedural business Entities */
    ASSIGN plValid = DYNAMIC-FUNCTION ("validateBusinessEntityName":U IN gshServiceInterface,
                                       pcEntityName) . 
END.
&ENDIF
    