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
public partial class print_ticket_list: System.Web.UI.Page
{
    protected void Page_Load(object sender, System.EventArgs e)
    {
        if (Session["ticket_report_list"] != null)
        {
            string open = Convert.ToString(Session["ticket_report_list"]);
            Response.Redirect(open);
        }
    }
}
