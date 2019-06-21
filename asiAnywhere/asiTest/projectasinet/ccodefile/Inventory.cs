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
/// Summary description for Inventory
/// </summary>
[System.ComponentModel.DataObject]
public class Inventory:AppServerConnect.AppServer
{
	public Inventory()
	{
		//
		// TODO: Add constructor logic here
		//
	}

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsInventoryDataSet SelectInventory(string prmUser, string prmAction, string prmOrderNum,string prmItemNum,string prmName,string prmVendor1,string prmVendItem1,string prmVendor2,string prmVendItem2,string prmOrdPolicy,string prmStocked,string prmPurchased,string prmIsSet,string prmalloc,string prmordlevel,string prmordmin,string prmordmax,string prmpuruom,string prmleaddays,string prmbegdate,string prmbegbal)
    {

        dsInventoryDataSet dsInventory = new dsInventoryDataSet();
        dsInventory = null;
        AppServerConnect();
        aoObject.Inventory(prmUser, prmAction, prmOrderNum,prmItemNum,prmName,prmVendor1,prmVendItem1,prmVendor2,prmVendItem2,prmOrdPolicy,prmStocked,prmPurchased,prmIsSet,prmalloc,prmordlevel,prmordmin,prmordmax,prmpuruom,prmleaddays,prmbegdate,prmbegbal, ref dsInventory);
        AppServerDisconnect();

        return dsInventory;
    }


    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Update, true)]
    public dsInventoryDataSet UpdateInventory(string prmUser, string prmAction, string prmOrderNum, string prmItemNum, string prmName, string prmVendor1, string prmVendItem1, string prmVendor2, string prmVendItem2, string prmOrdPolicy, string prmStocked, string prmPurchased, string prmIsSet, string prmalloc, string prmordlevel, string prmordmin, string prmordmax, string prmpuruom, string prmleaddays, string prmbegdate, string prmbegbal)
    {

        dsInventoryDataSet dsInventory = new dsInventoryDataSet();
        dsInventory = null;
        AppServerConnect();
        aoObject.Inventory(prmUser, prmAction, prmOrderNum, prmItemNum, prmName, prmVendor1, prmVendItem1, prmVendor2, prmVendItem2, prmOrdPolicy, prmStocked, prmPurchased, prmIsSet, prmalloc, prmordlevel, prmordmin, prmordmax, prmpuruom, prmleaddays, prmbegdate, prmbegbal, ref dsInventory);
        AppServerDisconnect();

        return dsInventory;
    }

}
