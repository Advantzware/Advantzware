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
/// Summary description for Totals
/// </summary>
[System.ComponentModel.DataObject]
public class Totals:AppServerConnect.AppServer
{
	public Totals()
	{
		//
		// TODO: Add constructor logic here
		//
	}


    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsItemTotalDataSet Selecttotals(string prmUser, string prmAction, string prmOrderNum, string prmItemNum)
    {

        dsItemTotalDataSet dsItemTotal = new dsItemTotalDataSet();
        dsItemTotal = null;
        AppServerConnect();
        aoObject.ItemTotals(prmUser, prmAction, prmOrderNum,prmItemNum, ref dsItemTotal);
        AppServerDisconnect();

        return dsItemTotal;
    }

    //[System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Update, true)]
    //public dsItemTotalDataSet UpdateTotals(Total tData)
    //{
    //    dsItemTotalDataSet dsItemTotalChange = new dsItemTotalDataSet();
    //    dsItemTotalDataSet dsItemTotal = new dsItemTotalDataSet();
    //    dsItemTotal = null;
    //    dsItemTotal = (dsItemTotalDataSet)HttpContext.Current.Cache.Get("Totals");
    //    //dsItemTotal.ttItemTotal[0].cust_no = tData.cust_no;
    //    //dsItemTotal.ttItemTotal[0].part_no = tData.part_no;
    //    //dsItemTotal.ttItemTotal[0].prgm_ver = tData.prgm_ver;
    //    //dsItemTotal.ttItemTotal[0].menu_item = tData.menu_item;
    //    //dsItemTotal.ttItemTotal[0].popup = tData.popup;
    //    //dsItemTotal.ttItemTotal[0].run_persistent = tData.run_persistent;
    //    //dsItemTotal.ttItemTotal[0].track_usage = tData.track_usage;
    //    //dsItemTotal.ttItemTotal[0].can_run = tData.can_run;
    //    //dsItemTotal.ttItemTotal[0].can_create = tData.can_create;
    //    //dsItemTotal.ttItemTotal[0].can_update = tData.can_update;
    //    //dsItemTotal.ttItemTotal[0].can_delete = tData.can_delete;
    //    //dsItemTotal.ttItemTotal[0].mfgroup = tData.mfgroup;
    //    dsItemTotalChange = (dsItemTotalDataSet)dsItemTotal.GetChanges();
    //    AppServerConnect();
    //    aoObject.ItemTotals("", "", "", "Update", "", ref dsItemTotal);
    //    AppServerDisconnect();

    //    return dsItemTotal;
    //}
}
public class Total
{
    private string _cust_no;
    private string _part_no;


    public string cust_no
    {
        get{return _cust_no;}
        set { _cust_no = value; }
    }
    public string part_no
    {
        get { return _part_no; }
        set { _part_no = value; }
    }
}