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
    File        : proSIfetchDataset.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu May 14 08:01:51 CEST 2009
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW .

DEFINE INPUT  PARAMETER pcEntityName AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER DATASET-HANDLE phDataset . 
DEFINE INPUT-OUTPUT PARAMETER DATASET-HANDLE phContextDataset .

DEFINE VARIABLE lContextDatasetAssigned AS LOGICAL NO-UNDO INIT FALSE .

{ Consultingwerk/products.i }

/* Mike Fechner, Consultingwerk Ltd. 08.05.2013
   Support for custom include files to the proSI... procedures. 
   This allows adding SHARED variable definitions that may be 
   required to execute legacy database triggers */
&IF "{&ProSIcustomIncludeDirectory}":U NE "":U &THEN
{ {&ProSIcustomIncludeDirectory}/proSIfetchDatasetCustom.i }
&ENDIF

/* ***************************  Main Block  *************************** */

IF VALID-HANDLE (phContextDataset) THEN 
    ASSIGN Consultingwerk.Framework.Session.SessionManager:ContextDataset = phContextDataset 
           lContextDatasetAssigned                                        = TRUE .

&IF DEFINED (ExcludeProceduralOERA) EQ 0 &THEN 
{ {&OERASI}/launchserviceinterface.i }
&ENDIF

IF NOT pcEntityName > "":U THEN 
    UNDO, THROW NEW Progress.Lang.AppError ("Unable to read schema with no EntityName set.":U, 0) .

&IF DEFINED (ExcludeProceduralOERA) EQ 0 &THEN 
IF NUM-ENTRIES (pcEntityName, ".":U) > 1 THEN DO ON ERROR UNDO, THROW:
&ENDIF
    Consultingwerk.OERA.ServiceInterface:FetchDataset (pcEntityName,
                                                       OUTPUT DATASET-HANDLE phDataset) .
    
    DELETE OBJECT phDataset NO-ERROR . 
    
    CATCH err AS Progress.Lang.Error :
        UNDO, THROW err .     	
    END CATCH.
&IF DEFINED (ExcludeProceduralOERA) EQ 0 &THEN 
END.
ELSE 
        RUN fetchDataset IN gshServiceInterface (pcEntityName, 
                                                 OUTPUT phDataset) .
&ENDIF

FINALLY:
    IF VALID-HANDLE (phContextDataset) THEN 
        DELETE OBJECT phContextDataset NO-ERROR . 

    /* Mike Fechner, Consultingwerk Ltd. 23.10.2011
       Only reset the ContextDataset when it was set by this instance
       (avoid issues with call nesting) */
    IF lContextDatasetAssigned AND SESSION:REMOTE THEN 
        Consultingwerk.Framework.Session.SessionManager:ContextDataset = ?  .
END FINALLY.
