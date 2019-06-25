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

public partial class creditstatusinv : System.Web.UI.Page
{
    protected void Page_Load(object sender, EventArgs e)
    {
        if (Session["view_order_entry_pages"] == null)
        {
            LinkButton3.Visible = false;
        }
        if (Session["view_order_entry_pages"] != null)
        {
            LinkButton2.Visible = false;
        }

        if (Session["view_order_entry_pages_with_estimate"] != null)
        {
            LinkButton2.Visible = false;
            LinkButton3.Visible = false;
        }
        if (Session["view_order_entry_pages_with_estimate"] == null)
        {
            LinkButton4.Visible = false;
        }

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;

        lbl_page.Text = "Credit Status";
        if (!Page.IsPostBack)
        {
            if (Session["User"] != null)
            {

                UserClass.CheckLogin(Page);
                lblUser.Text = UserLogin.UserName;

                string vUserId = UserLogin.UserName;
                string vPage = "creditstatusinv.aspx";
                string aUsers = null;
                string PrmComp = null;
                bool vCanCreate = false;
                bool vCanRun = false;
                bool vCanUpdate = false;
                bool vCanDelete = false;

                func1 f1 = new func1();
                //Response.Write(Page);
                f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);
                //lblComp.Text = PrmComp;
                if (vCanRun == false)
                {
                    Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                    Response.Write("<script>window.location.href = 'login.aspx';</script>");

                }

                string vUserIdviewinvoice = UserLogin.UserName;
                string vPageviewinvoice = "viewinvoice.aspx";
                string aUsersviewinvoice = null;
                string PrmCompviewinvoice = null;
                bool vCanCreateviewinvoice = false;
                bool vCanRunviewinvoice = false;
                bool vCanUpdateviewinvoice = false;
                bool vCanDeleteviewinvoice = false;

                func1 f1viewinvoice = new func1();
                //Response.Write(Page);
                f1viewinvoice.CheckProgramPermissions(vPageviewinvoice, vUserIdviewinvoice, ref  vCanCreateviewinvoice, ref  vCanRunviewinvoice, ref  vCanUpdateviewinvoice, ref  vCanDeleteviewinvoice, ref  PrmCompviewinvoice, ref  aUsersviewinvoice);

                lblComp.Text = PrmCompviewinvoice;
                //Response.Write(PrmCompviewinvoice);
                if (vCanRunviewinvoice == true)
                {
                    lnkviewinvoice.Visible = true;

                }

                if (vCanRunviewinvoice == false)
                {
                    lnkviewinvoice.Visible = false;

                }

                string vUserIdbrowsinvoice = UserLogin.UserName;
                string vPagebrowsinvoice = "browsinvoice.aspx";
                string aUsersbrowsinvoice = null;
                string PrmCompbrowsinvoice = null;
                bool vCanCreatebrowsinvoice = false;
                bool vCanRunbrowsinvoice = false;
                bool vCanUpdatebrowsinvoice = false;
                bool vCanDeletebrowsinvoice = false;

                func1 f1browsinvoice = new func1();
                //Response.Write(Page);
                f1browsinvoice.CheckProgramPermissions(vPagebrowsinvoice, vUserIdbrowsinvoice, ref  vCanCreatebrowsinvoice, ref  vCanRunbrowsinvoice, ref  vCanUpdatebrowsinvoice, ref  vCanDeletebrowsinvoice, ref  PrmCompbrowsinvoice, ref  aUsersbrowsinvoice);

                lblComp.Text = PrmCompbrowsinvoice;
                //Response.Write(vCanRun);
                if (vCanRunbrowsinvoice == true)
                {
                    lnkbrowsinvoice.Visible = true;

                }

                if (vCanRunbrowsinvoice == false)
                {
                    lnkbrowsinvoice.Visible = false;

                }
                string vUserIdbol = UserLogin.UserName;
                string vPagebol = "bol.aspx";
                string aUsersbol = null;
                string PrmCompbol = null;
                bool vCanCreatebol = false;
                bool vCanRunbol = false;
                bool vCanUpdatebol = false;
                bool vCanDeletebol = false;

                func1 f1bol = new func1();
                //Response.Write(Page);
                f1bol.CheckProgramPermissions(vPagebol, vUserIdbol, ref  vCanCreatebol, ref  vCanRunbol, ref  vCanUpdatebol, ref  vCanDeletebol, ref  PrmCompbol, ref  aUsersbol);

                lblComp.Text = PrmCompbol;
                //Response.Write(vCanRun);
                if (vCanRunbol == true)
                {
                    lnkbol.Visible = true;

                }

                if (vCanRunbol == false)
                {
                    lnkbol.Visible = false;

                }


            }
        }
    }
    protected void lnkbrowsinvoice_Click(object sender, EventArgs e)
    {
        Response.Redirect("browsinvoice.aspx");
    }

    protected void lnkviewinvoice_Click(object sender, EventArgs e)
    {
        Response.Redirect("viewinvoice.aspx");
    }
    protected void lnkbol_Click(object sender, EventArgs e)
    {
        Response.Redirect("bol.aspx");
    }
    protected void LinkButton1_Click(object sender, EventArgs e)
    {
        Response.Redirect("menu.aspx");
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
        if (Request.Cookies["showmenu"] != null)
        {
            Response.Cookies["showmenu"].Expires = DateTime.Now.AddDays(-1);
        }
        Response.Redirect(sLoginURL);
    }
    protected void LinkButton2_Click(object sender, EventArgs e)
    {
        Response.Redirect("order_entry.aspx");
    }
    protected void LinkButton_Click(object sender, EventArgs e)
    {
        Response.Redirect("order_inquiry.aspx");
    }
    protected void LinkButton3_Click(object sender, EventArgs e)
    {
        Response.Redirect("order_estimate.aspx");
    }
}
