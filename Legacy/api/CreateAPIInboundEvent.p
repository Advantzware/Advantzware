/*------------------------------------------------------------------------
    File        : api\CreateAPIInboundEvent.p
    Purpose     : Creates APIInboundEvent Table Log

    Syntax      :

    Description : Creates APIInboundEvent Table Log

    Author(s)   : Vishnu Vellanki
    Created     : Tue Jun 21 07:33:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcRoute            AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER iplcRequestData     AS LONGCHAR  NO-UNDO.
DEFINE INPUT  PARAMETER iplcReponseData     AS LONGCHAR  NO-UNDO.
DEFINE INPUT  PARAMETER iplSuccess          AS LOGICAL   NO-UNDO.
DEFINE INPUT  PARAMETER ipcMessage          AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcDateTime         AS DATETIME  NO-UNDO.
DEFINE INPUT  PARAMETER ipcRequestedBy      AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcRecordSource     AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcNotes            AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcPayloadID        AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opriAPIInboundEvent AS CHARACTER NO-UNDO.

CREATE APIInboundEvent.
ASSIGN
    APIInboundEvent.apiRoute        = ipcRoute
    APIInboundEvent.requestData     = iplcRequestData
    APIInboundEvent.responseData    = iplcReponseData
    APIInboundEvent.success         = iplSuccess
    APIInboundEvent.errorMessage    = ipcMessage
    APIInboundEvent.requestDateTime = ipcDateTime
    APIInboundEvent.requestedBy     = ipcRequestedBy
    APIInboundEvent.recordSource    = ipcRecordSource
    APIInboundEvent.notes           = ipcNotes
    APIInboundEvent.externalID      = ipcPayloadID
    opriAPIInboundEvent             = STRING(ROWID(APIInboundEvent))
    .
