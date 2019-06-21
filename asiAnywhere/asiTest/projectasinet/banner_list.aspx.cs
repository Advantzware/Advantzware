#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
using System.Data.SqlClient;
#endregion

/// <summary>
/// Summary description for Class1
/// </summary>
public partial class banner_list : System.Web.UI.Page
{
    protected void Page_Load(object sender, System.EventArgs e)
    {
        
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "banner_list.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            //lblComp.Text = PrmComp;

            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }
        if (!Page.IsPostBack)
        {
            lblUser.Text = UserLogin.UserName;

        }

        GridView1.SelectedIndex = Convert.ToInt32(Session["banner_list_index"]);
        try
        {
            if (Session["banner_list_index"] == null)
            {
                GridView1.SelectedIndex = 0;
                Session["banner_url_add"] = GridView1.SelectedRow.Cells[1].Text;

            }
        }
        catch
        {
            return;
        }

    }


    protected void hlnkLogOut_Click(object sender, EventArgs e)
    {

        string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
        if (sLoginURL == "")
        {
            Response.Write("<script language=javascript>alert('" + "Login page isn’t set" + "!');</script>");
            return;
        }

        Page.Session.Clear();
        Response.Redirect(sLoginURL);
    }
    protected void GridView1_SelectedIndexChanged(object sender, EventArgs e)
    {
        Session["banner_list_index"] = GridView1.SelectedIndex;
        Session["banner_url_add"] = GridView1.SelectedRow.Cells[1].Text;


    }
    protected void lnk_bannerview_click(object sender, EventArgs e)
    {
        Response.Redirect("banner_view.aspx");
    }


}
