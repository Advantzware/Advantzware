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
/// Summary description for custitem
/// </summary>
[System.ComponentModel.DataObject]
public class adjustment : AppServerConnect.AppServer
{
    public adjustment()
    {
        //
        // TODO: Add constructor logic here
        //
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsFgAdjustmentDataSet SelectAdjustment(string prmUser, string prmActFgAdj, Int32 prmSeq, string prmTag, DateTime prmRecDate, string prmPo, string prmitemno, string prmjob)
    {

        dsFgAdjustmentDataSet dsFgAdjustment = new dsFgAdjustmentDataSet();
        dsFgAdjustment = null;
        AppServerConnect();
        aoObject.FgAdjustment(prmUser, prmActFgAdj, prmSeq, prmTag, prmRecDate, prmPo, prmitemno,prmjob, ref dsFgAdjustment);
        AppServerDisconnect();

        return dsFgAdjustment;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsFgViewAdjustmentDataSet ViewAdjustment(string prmUser, string prmFgViewAct, Int32 prmSeq, DateTime prmAdjustmentDate, string prmitemno, string prmitemname, string prmjob1, Int32 prmjob2, string prmwhse, string prmbin, string prmtag, string prmcustomer, Int32 prmunits, Int32 prmqtyunit, Int32 prmpartial, Int32 prmtotalqty, Decimal prmcost, string prmcreatedby, string prmlastupdated)
    {
        string cError = "";
        dsFgViewAdjustmentDataSet dsFgViewAdjustment = new dsFgViewAdjustmentDataSet();
        dsFgViewAdjustment = null;
        AppServerConnect();
        aoObject.FgViewAdjustment(prmUser, prmFgViewAct, prmSeq, prmAdjustmentDate, prmitemno, prmitemname, prmjob1, prmjob2, prmwhse, prmbin, prmtag, prmcustomer, prmunits, prmqtyunit, prmpartial, prmtotalqty, prmcost, prmcreatedby, prmlastupdated,out cError, ref dsFgViewAdjustment);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsFgViewAdjustment;
    }
}
   
  