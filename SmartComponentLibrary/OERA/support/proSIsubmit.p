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
    File        : proSIsubmit.p
    Purpose     : Service Interface procedure for saveChanges Business 
                  Entity method

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Apr 04 17:02:33 CEST 2009
    Notes       : pcContext is currently not used by the serivce interface
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW .

DEFINE INPUT        PARAMETER pcEntityName               AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET-HANDLE phIODataSet.
DEFINE INPUT-OUTPUT PARAMETER pcContext                  AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER plcParameter               AS LONGCHAR  NO-UNDO .
DEFINE INPUT-OUTPUT PARAMETER DATASET-HANDLE phContextDataset .

DEFINE VARIABLE lContextDatasetAssigned AS LOGICAL NO-UNDO INIT FALSE .

DEFINE VARIABLE oParameter AS Consultingwerk.ISerializable NO-UNDO . 

{ Consultingwerk/products.i }

/* Mike Fechner, Consultingwerk Ltd. 08.05.2013
   Support for custom include files to the proSI... procedures. 
   This allows adding SHARED variable definitions that may be 
   required to execute legacy database triggers */
&IF "{&ProSIcustomIncludeDirectory}":U NE "":U &THEN
{ {&ProSIcustomIncludeDirectory}/proSIsubmitCustom.i }
&ENDIF

/* ***************************  Main Block  *************************** */

&IF DEFINED (ExcludeProceduralOERA) EQ 0 &THEN 
{ {&OERASI}/launchserviceinterface.i }
&ENDIF

IF VALID-HANDLE (phContextDataset) THEN 
    ASSIGN Consultingwerk.Framework.Session.SessionManager:ContextDataset = phContextDataset 
           lContextDatasetAssigned                                        = TRUE .

&IF DEFINED (ExcludeProceduralOERA) EQ 0 &THEN 
IF NUM-ENTRIES (pcEntityName, ".":U) > 1 THEN 
&ENDIF    
    DO ON ERROR UNDO, THROW:
        
    IF plcParameter > "":U THEN 
        ASSIGN oParameter = Consultingwerk.Serializable:DeserializeInstance (plcParameter) . 
    ELSE 
        ASSIGN oParameter = ? . 

    Consultingwerk.OERA.ServiceInterface:SaveChanges (pcEntityName, 
                                                      INPUT-OUTPUT DATASET-HANDLE phIODataSet BY-REFERENCE,
                                                      oParameter) .
    
    IF VALID-OBJECT (oParameter) THEN 
        ASSIGN plcParameter = oParameter:Serialize () . 
    
    { {&OERASI}/sicatch.i }
    
    FINALLY:
        IF VALID-HANDLE (phIODataSet) THEN 
            DELETE OBJECT phIODataSet .        		
    END FINALLY.
END.                                                      
&IF DEFINED (ExcludeProceduralOERA) EQ 0 &THEN 
ELSE DO:

    RUN saveChanges IN gshServiceInterface (pcEntityName,
                                            INPUT-OUTPUT phIODataSet) NO-ERROR. 

END.
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
