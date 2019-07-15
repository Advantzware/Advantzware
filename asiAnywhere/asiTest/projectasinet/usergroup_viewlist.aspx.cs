
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
#endregion

public partial class Cusergroup_viewlist : System.Web.UI.Page
{
    private string userid = "";
    protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);

        if (!func.CheckUserPermissions("[dbo].[usergroup]", "SA"))
        {
            Response.Write("<p>" + "You don't have permissions to access this table" + "<a href=\"login.aspx\">&nbsp;" + "Back to login page" + "</a></p>");
            Response.End();
        }

        lblMessage.Text = "";
       

        string sCulture = ConfigurationManager.AppSettings["LCID"];
        if (!String.IsNullOrEmpty(sCulture))
        {
            int nCulture = int.Parse(sCulture);
            System.Threading.Thread.CurrentThread.CurrentCulture = new System.Globalization.CultureInfo(nCulture, false);
        }

        if (!Page.IsPostBack)
        {

            if (Session["User"] != null)
            {
                UserClass UserLogin = (UserClass)Session["User"];
                lblUser.Text = UserLogin.UserName;

                hlnkChangePwd.Visible = (UserLogin.UserName != "Guest");

            }



            if (func.CheckUserPermissions("[dbo].[usergroup]", "s"))
            {
            }

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
    protected void lnk_listgroup_Click(object sender, EventArgs e)
    {
        Response.Redirect("usergroup_list.aspx");
    }
    protected void lnk_viewgroup_Click(object sender, EventArgs e)
    {
        Response.Redirect("usergroup_viewlist.aspx");
    }
    protected void lnk_groupuser_Click(object sender, EventArgs e)
    {
        Response.Redirect("group_user.aspx");
    }

    protected void FormView1_DataBound(object sender, EventArgs e)
    {
        if (FormView1.CurrentMode == FormViewMode.Edit)
        {
            TextBox code = (TextBox)FormView1.FindControl("groupTextBox");
            code.Focus();
        }
        if (FormView1.CurrentMode == FormViewMode.Insert)
        {
            Label id = (Label)FormView1.FindControl("groupIDLabel1");
            int value = Convert.ToInt32(userid.ToString());
            id.Text = Convert.ToString(value + 1);
         
            TextBox code = (TextBox)FormView1.FindControl("groupTextBox");
            code.Focus();
        }

    }
    protected void AddButton_Click(object sender, EventArgs e)
    {
        Label id = (Label)FormView1.FindControl("groupID2Label");
        userid = id.Text;
    }
}
