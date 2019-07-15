using System;
using System.Data;
using System.Configuration;
using System.Collections;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;

/// <summary>
/// Summary description for Class1
/// </summary>
public partial class reorder_Report : System.Web.UI.Page
{
    protected void Page_Load(object sender, System.EventArgs e)
    {
        //Response.Write(Session["cust_reorder_list_rep1"]);
        if (Session["cust_reorder_list_rep1"] != null)
        {
            string open = Convert.ToString(Session["cust_reorder_list_rep1"]);
            Response.Redirect(open);
        }
    }
}
