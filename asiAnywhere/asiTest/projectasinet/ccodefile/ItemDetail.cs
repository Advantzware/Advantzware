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
/// Summary description for ItemDetail
/// </summary>
[System.ComponentModel.DataObject]
public class ItemDetail : AppServerConnect.AppServer
{
    public ItemDetail()
    {
        //
        // TODO: Add constructor logic here
        //
    }

    

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsItemDetailDataSet SelectItemDetail(string prmUser, string prmItem)
    {

        dsItemDetailDataSet dsItemDetail = new dsItemDetailDataSet();
        dsItemDetail = null;
        AppServerConnect();
        aoObject.ItemDetail(prmUser, prmItem, ref dsItemDetail);
        AppServerDisconnect();
        // HttpContext.Current.Cache.Insert("ItemData", dsItemDetail, null, DateTime.Now.AddMinutes(10), System.Web.Caching.Cache.NoSlidingExpiration);
        return dsItemDetail;
    }
    
}


