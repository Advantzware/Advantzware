
using System;
using System.Data;
using System.Configuration;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;
using System.Web.Caching;
using ASINET;
using ASIDataNS;
using Progress.Open4GL.Proxy;

/// <summary>
/// Summary description for browsinvoice
/// </summary>
[System.ComponentModel.DataObject]
public class bol : AppServerConnect.AppServer
{
    public bol()
    {
        //
        // TODO: Add constructor logic here
        //
    }


    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsRelBolDataSet SelectRelBOL(string prmUser, string prmAction, string prmComp, Int32 prmSeq, Int32 prmRelease, string prmTag, string prmTrailor, Int32 prmCases, string prmIno, string prmIName, Int32 prmOrdNo, Int32 prmQty, string prmLoc, string prmLocBin, string prmCustNo, Int32 prmQtyCase, Int32 prmCasesUnit, Int32 prmPartial, string prmJobNo, Int32 prmJobNo2, Int32 prmLine, string prmWarned, string prmPoNo, Int32 prmRelQty, Int32 prmScanQty, string prmFilePath)
    {
        string cError = "";
        string prmMailTo = "";
        string prmSubject = "";
        string prmBody = "";

        dsRelBolDataSet dsRelBol = new dsRelBolDataSet();
        dsRelBol = null;
        AppServerConnect();
        aoObject.relbol(prmUser, prmAction, prmComp, prmSeq, prmRelease, prmTag, prmTrailor, prmCases, prmIno, prmIName, prmOrdNo, prmQty, prmLoc, prmLocBin, prmCustNo, prmQtyCase, prmCasesUnit, prmPartial, prmJobNo, prmJobNo2, prmLine, prmWarned, prmPoNo, prmRelQty, prmScanQty,prmFilePath, ref dsRelBol, out cError, out prmMailTo, out prmSubject, out prmBody);
        AppServerDisconnect();

        func.SendMail(prmMailTo, prmSubject, prmBody);
        
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsRelBol;
     
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public bool CheckRelBOL(string prmUser, string prmAction, string prmComp, Int32 prmSeq, Int32 prmRelease, string prmTag, string prmTrailor, Int32 prmCases, string prmIno, string prmIName, Int32 prmOrdNo, Int32 prmQty, string prmLoc, string prmLocBin, string prmCustNo, Int32 prmQtyCase, Int32 prmCasesUnit, Int32 prmPartial, string prmJobNo, Int32 prmJobNo2, Int32 prmLine, string prmWarned, string prmPoNo, Int32 prmRelQty, Int32 prmScanQty, string prmFilePath)
    {
        string cError = "";
        string prmMailTo = "";
        string prmSubject = "";
        string prmBody = "";
        dsRelBolDataSet dsRelBol = new dsRelBolDataSet();       
        AppServerConnect();
        aoObject.relbol(prmUser, prmAction, prmComp, prmSeq, prmRelease, prmTag, prmTrailor, prmCases, prmIno, prmIName, prmOrdNo, prmQty, prmLoc, prmLocBin, prmCustNo, prmQtyCase, prmCasesUnit, prmPartial, prmJobNo, prmJobNo2, prmLine, prmWarned, prmPoNo, prmRelQty, prmScanQty,prmFilePath, ref dsRelBol, out cError, out prmMailTo, out prmSubject, out prmBody);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            return false;
        }
        else
        {
            return true;
        }   
    }    

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsBolInfoDataSet SeBolInfo(string prmUser, string prmAction, string prmCustomer, Int32 prmBol)
    {
        string cError = "";

        dsBolInfoDataSet dsBolInfo = new dsBolInfoDataSet();
        dsBolInfo = null;
        AppServerConnect();       
        aoObject.Bolinfo(prmUser, prmAction, prmCustomer, prmBol, out cError, ref dsBolInfo);
        AppServerDisconnect();
        
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsBolInfo;
     
    }
     
}