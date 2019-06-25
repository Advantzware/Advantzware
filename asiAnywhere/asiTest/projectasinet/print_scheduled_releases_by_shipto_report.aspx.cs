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
public partial class print_scheduled_releases_by_shipto_report: System.Web.UI.Page
{
    protected void Page_Load(object sender, System.EventArgs e)
    {
        if (Session["scheduled_releases_rep"] != null)
        {
            string open = Convert.ToString(Session["scheduled_releases_rep"]);
            Response.Redirect(open);
        }
    }
}
