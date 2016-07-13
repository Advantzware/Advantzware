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
    File        : proSIretrieve.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Apr 04 15:40:40 CEST 2009
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

USING Consultingwerk.OERA.* FROM PROPATH . 

DEFINE INPUT         PARAMETER pcEntity       AS CHARACTER NO-UNDO EXTENT.
DEFINE INPUT         PARAMETER pcTables       AS CHARACTER NO-UNDO EXTENT.
DEFINE INPUT         PARAMETER pcQueries      AS CHARACTER NO-UNDO EXTENT.
DEFINE INPUT         PARAMETER pcJoins        AS CHARACTER NO-UNDO EXTENT.
DEFINE INPUT         PARAMETER pcPositions    AS CHARACTER NO-UNDO EXTENT.
DEFINE INPUT         PARAMETER pcRequests     AS CHARACTER NO-UNDO EXTENT.
DEFINE INPUT         PARAMETER pcBatchContext AS CHARACTER NO-UNDO.
DEFINE INPUT         PARAMETER plFillBatch    AS LOGICAL   NO-UNDO.
DEFINE INPUT         PARAMETER piStopAfter    AS INTEGER   NO-UNDO.
DEFINE INPUT-OUTPUT  PARAMETER pcNumRecords   AS CHARACTER NO-UNDO EXTENT.
DEFINE INPUT-OUTPUT  PARAMETER plcParameter   AS LONGCHAR  NO-UNDO EXTENT. 
DEFINE INPUT         PARAMETER plcNamedQuery  AS LONGCHAR  NO-UNDO EXTENT. 

DEFINE OUTPUT PARAMETER DATASET-HANDLE phOutputDataSet1 .  
DEFINE OUTPUT PARAMETER DATASET-HANDLE phOutputDataSet2 .  
DEFINE OUTPUT PARAMETER DATASET-HANDLE phOutputDataSet3 .  
DEFINE OUTPUT PARAMETER DATASET-HANDLE phOutputDataSet4 .
DEFINE OUTPUT PARAMETER DATASET-HANDLE phOutputDataSet5 .  
DEFINE OUTPUT PARAMETER DATASET-HANDLE phOutputDataSet6 .  
DEFINE OUTPUT PARAMETER DATASET-HANDLE phOutputDataSet7 .  
DEFINE OUTPUT PARAMETER DATASET-HANDLE phOutputDataSet8 .  
DEFINE OUTPUT PARAMETER DATASET-HANDLE phOutputDataSet9 .  
DEFINE OUTPUT PARAMETER DATASET-HANDLE phOutputDataSet10 .  
 
DEFINE INPUT-OUTPUT  PARAMETER pcContext      AS CHARACTER NO-UNDO EXTENT.
DEFINE OUTPUT        PARAMETER pcPrevContext  AS CHARACTER NO-UNDO EXTENT.
DEFINE OUTPUT        PARAMETER pcNextContext  AS CHARACTER NO-UNDO EXTENT.

DEFINE INPUT-OUTPUT PARAMETER DATASET-HANDLE phContextDataset .

DEFINE VARIABLE hEntity     AS HANDLE    NO-UNDO.
DEFINE VARIABLE iEntity     AS INTEGER   NO-UNDO.
DEFINE VARIABLE hDataset    AS HANDLE    NO-UNDO.
DEFINE VARIABLE lNew        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cContext    AS CHARACTER NO-UNDO. 
DEFINE VARIABLE iNumRecords AS INTEGER   NO-UNDO.
DEFINE VARIABLE iBuffer     AS INTEGER   NO-UNDO.

DEFINE VARIABLE oFetchDataRequest AS Consultingwerk.OERA.FetchDataRequest NO-UNDO . 

{ Consultingwerk/products.i }
&IF DEFINED (ExcludeProceduralOERA) EQ 0 &THEN 
{ {&OERASI}/launchserviceinterface.i }
&ENDIF

DEFINE VARIABLE lContextDatasetAssigned AS LOGICAL NO-UNDO INIT FALSE .

/* Mike Fechner, Consultingwerk Ltd. 08.05.2013
   Support for custom include files to the proSI... procedures. 
   This allows adding SHARED variable definitions that may be 
   required to execute legacy database triggers */
&IF "{&ProSIcustomIncludeDirectory}":U NE "":U &THEN
{ {&ProSIcustomIncludeDirectory}/proSIretrieveCustom.i }
&ENDIF

/* ***************************  Main Block  *************************** */

IF VALID-HANDLE (phContextDataset) THEN 
    ASSIGN Consultingwerk.Framework.Session.SessionManager:ContextDataset = phContextDataset 
           lContextDatasetAssigned                                        = TRUE .

/* dim output from input */
EXTENT(pcPrevContext) = EXTENT(pcEntity).
EXTENT(pcNextContext) = EXTENT(pcEntity).
 
DO iEntity = 1 TO EXTENT(pcEntity) ON ERROR UNDO, THROW:
    /* Mike Fechner, Consultingwerk Ltd. 07.03.2010
       Reset Error-Status */
    ERROR-STATUS:ERROR = FALSE . 
    
    /* input batchcontext only applies to first entity */
    IF iEntity = 1 THEN
      cContext = pcBatchContext.
    ELSE 
      cContext = "":U. 
     
     /* This service only batches first table */
    iNumRecords = INTEGER (ENTRY (1,pcNumRecords[iEntity])).
    
    /* find not supported in service ... read all  */
    IF pcRequests[iEntity] BEGINS "WHERE ":U THEN 
      iNumRecords = 0.
    
    IF (pcTables[iEntity] <> "":U OR plcNamedQuery[iEntity] > "":U OR plcParameter[iEntity] > "":U) 
    /* Mike Fechner, Consultingwerk Ltd. 31.03.2009
       Bug 1733: DEFS request */
      AND ENTRY(1, pcRequests[iEntity], CHR(1)) <> "DEFS":U
    
    THEN DO ON ERROR UNDO, THROW:
&IF DEFINED (ExcludeProceduralOERA) EQ 0 &THEN 
        IF NUM-ENTRIES (pcEntity[iEntity], ".":U) > 1 THEN 
&ENDIF            
        DO ON ERROR UNDO, THROW:
            ASSIGN hDataset = ? . 

            oFetchDataRequest = NEW Consultingwerk.OERA.FetchDataRequest (pcTables[iEntity],
                                                                          pcQueries[iEntity],
                                                                          pcRequests[iEntity],
                                                                          iNumRecords,
                                                                          ENTRY (1, cContext, CHR(3))) .        
        
            IF NUM-ENTRIES (cContext, CHR(3)) > 1 THEN 
                ASSIGN oFetchDataRequest:CustomContext = ENTRY (2, cContext, CHR(3)) .
        
            IF piStopAfter >= 0 THEN 
                oFetchDataRequest:StopAfter = piStopAfter . 

            IF plcParameter[iEntity] > "":U THEN 
                oFetchDataRequest:CustomParameter = Consultingwerk.Serializable:DeserializeInstance (plcParameter[iEntity]) .
            
            IF plcNamedQuery[iEntity] > "":U THEN 
                oFetchDataRequest:NamedQuery = CAST (Consultingwerk.Serializable:DeserializeInstance (plcNamedQuery[iEntity]),
                                                     INamedQueryParameter) .
        
            /* Mike Fechner, Consultingwerk Ltd. 30.09.2015
               SCL-645: This is the only place where we consider it is o.k. to call into the 
                        ServiceInterface:FetchData method with a BY-REFERENCE dataset. This is
                        because this procedure is only supposed to be called by the ServiceAdapter
                        and the ServiceAdapter does RUN this procedure with on BY-REFERENCE for the 
                        Dataset parameter */
            Consultingwerk.OERA.ServiceInterface:FetchData (pcEntity[iEntity],
                                                            oFetchDataRequest,
                                                            OUTPUT DATASET-HANDLE hDataset BY-REFERENCE) .
                                                            
            ASSIGN cContext               = oFetchDataRequest:Context
                   pcNextContext[iEntity] = oFetchDataRequest:NextContext
                   pcPrevContext[iEntity] = oFetchDataRequest:PrevContext .             
            
            IF VALID-OBJECT (oFetchDataRequest:CustomParameter) THEN 
                plcParameter[iEntity] = oFetchDataRequest:CustomParameter:Serialize () .

            { {&OERASI}/sicatch.i }
            
            FINALLY:
                IF VALID-OBJECT (oFetchDataRequest) THEN 
                    DELETE OBJECT oFetchDataRequest .                                                 
            END FINALLY.
      END.                                                                
&IF DEFINED (ExcludeProceduralOERA) EQ 0 &THEN 
      ELSE DO:
        RUN fetchData IN gshServiceInterface (pcEntity[iEntity],
                                              pcTables[iEntity],
                                              pcQueries[iEntity],
                                              pcRequests[iEntity],
                                              iNumRecords,
                                              INPUT-OUTPUT cContext,
                                              OUTPUT hDataset) NO-ERROR.
        
        /* This service only returns context for first table 
           ADM2 will handle fewer entries than tables */
        IF iEntity = 1 THEN
           pcNextContext = cContext.
      END.                                              
&ENDIF 
    END.   
    ELSE DO:
&IF DEFINED (ExcludeProceduralOERA) EQ 0 &THEN 
        IF NUM-ENTRIES (pcEntity[iEntity], ".":U) > 1 THEN DO:
&ENDIF
            ASSIGN hDataset = ? . 
            
            Consultingwerk.OERA.ServiceInterface:FetchDataset (pcEntity[iEntity],
                                                               OUTPUT DATASET-HANDLE hDataset) .
      END.
&IF DEFINED (ExcludeProceduralOERA) EQ 0 &THEN 
      ELSE 
        RUN fetchDataset IN gshServiceInterface (pcEntity[iEntity], 
                                                 OUTPUT hDataset) NO-ERROR.
    END. 
&ENDIF
        
&IF DEFINED (IgnoreErrorStatusErrorInServiceInterfaceProcedure) EQ 0 &THEN
    /* Mike Fechner, Consultingwerk Ltd. 12.02.2015
       SCL-577 : Optionally do not check for ERROR-STATUS:ERROR here,
                 see products.i */
    IF ERROR-STATUS:ERROR THEN RETURN ERROR IF RETURN-VALUE > "":U THEN RETURN-VALUE ELSE ERROR-STATUS:GET-MESSAGE(1).
&ENDIF    
               
  CASE iEntity:
    WHEN 1 THEN
      phOutputDataset1  = hDataset.
    WHEN 2 THEN
      phOutputDataset2  = hDataset.
    WHEN 3 THEN
      phOutputDataset3  = hDataset.
    WHEN 4 THEN
      phOutputDataset4  = hDataset.
    WHEN 5 THEN
      phOutputDataset5  = hDataset.
    WHEN 6 THEN
      phOutputDataset6  = hDataset.
    WHEN 7 THEN
      phOutputDataset7  = hDataset.
    WHEN 8 THEN
      phOutputDataset8  = hDataset.
    WHEN 9 THEN
      phOutputDataset9  = hDataset.
    WHEN 10 THEN
      phOutputDataset10 = hDataset.
  END CASE .

  FINALLY:
      IF NUM-ENTRIES (pcEntity[iEntity], ".":U) > 1 THEN
          DELETE OBJECT hDataset NO-ERROR .
  END FINALLY.  

END.    /* do iEntity = 1 */  

/* Mike Fechner, Consultingwerk Ltd. 10.10.2014
   Without this CATCH and THROW the error will end up in the 
   AppServer logfile and not make it to the client */
CATCH err AS Progress.Lang.Error :
    UNDO, THROW err . 	
END CATCH.

FINALLY:
    IF VALID-HANDLE (phOutputDataset1) THEN 
        DELETE OBJECT phOutputDataset1 NO-ERROR .    		

    IF VALID-HANDLE (phContextDataset) THEN 
        DELETE OBJECT phContextDataset NO-ERROR . 

    /* Mike Fechner, Consultingwerk Ltd. 23.10.2011
       Only reset the ContextDataset when it was set by this instance
       (avoid issues with call nesting) */
    IF lContextDatasetAssigned AND SESSION:REMOTE THEN 
        Consultingwerk.Framework.Session.SessionManager:ContextDataset = ?  .
END FINALLY.          
                  