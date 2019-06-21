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

public partial class viewinvoice : System.Web.UI.Page
{
    protected void lnkbrowsinvoice_Click(object sender, EventArgs e)
    {
        Response.Redirect("browsinvoice.aspx");
    }

    protected void lnkcreditstatus_Click(object sender, EventArgs e)
    {
        Response.Redirect("creditstatusinv.aspx");
    }

    protected void lnkbol_Click(object sender, EventArgs e)
    {
        Response.Redirect("bol.aspx");
    }

    protected void GridView1_SelectedIndexChanged(object sender, EventArgs e)
    {

    }

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

        lbl_page.Text = "View Invoice";
        if (!Page.IsPostBack)
        {
            if (Session["User"] != null)
            {
                UserClass UserLogin = (UserClass)Session["User"];
                ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;

                lblUser.Text = UserLogin.UserName;

                string vUserId = UserLogin.UserName;
                string vPage = "viewinvoice.aspx";
                string aUsers = null;
                string PrmComp = null;
                bool vCanCreate = false;
                bool vCanRun = false;
                bool vCanUpdate = false;
                bool vCanDelete = false;

                func1 f1 = new func1();
                //Response.Write(Page);
                f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);
                if (vCanRun == false)
                {
                    Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                    Response.Write("<script>window.location.href = 'login.aspx';</script>");

                }

                string vUserIdcreditstatusinv = UserLogin.UserName;
                string vPagecreditstatusinv = "creditstatusinv.aspx";
                string aUserscreditstatusinv = null;
                string PrmCompcreditstatusinv = null;
                bool vCanCreatecreditstatusinv = false;
                bool vCanRuncreditstatusinv = false;
                bool vCanUpdatecreditstatusinv = false;
                bool vCanDeletecreditstatusinv = false;

                func1 f1creditstatusinv = new func1();
                //Response.Write(Page);
                f1creditstatusinv.CheckProgramPermissions(vPagecreditstatusinv, vUserIdcreditstatusinv, ref  vCanCreatecreditstatusinv, ref  vCanRuncreditstatusinv, ref  vCanUpdatecreditstatusinv, ref  vCanDeletecreditstatusinv, ref  PrmCompcreditstatusinv, ref  aUserscreditstatusinv);

                lblComp.Text = PrmCompcreditstatusinv;
                //Response.Write(PrmCompcreditstatusinv);
                if (vCanRuncreditstatusinv == true)
                {
                    lnkcreditstatus.Visible = true;

                }

                if (vCanRuncreditstatusinv == false)
                {
                    lnkcreditstatus.Visible = false;

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

        String expression = "LINE";
        SortDirection direction;
        direction = SortDirection.Ascending;
        GridView1.Sort(expression, direction);
        warning.Visible = false;
    }


    protected void PrintInvoice_Click(object sender, EventArgs e)
    {
        string swhere1 = Request.Form["selectradio"];
        if (swhere1 != null)
        {
            //Response.Write("hello");
            Session["prmAction"] = "PrintInvoice";
            Session["vRowid"] = swhere1;
            if (!Request.Browser.Browser.Contains("Safari"))
                Response.Write("<script>window.open('ViewInvPrint.aspx'); target='_blank'</script>");
            else
                Response.Redirect("ViewInvPrint.aspx");
        }
        else
        {
            Response.Write("<script>alert('There has not been an invoice created for this order yet.')</script>");
        }

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
            Response.Write("alert('" + "Login page isn’t set" + "!');");
            return;
        }

        Page.Session.Clear();
        if (Request.Cookies["showmenu"] != null)
        {
            Response.Cookies["showmenu"].Expires = DateTime.Now.AddDays(-1);
        }
        Response.Redirect(sLoginURL);
    }
    protected void lnkviewinvoice_Click(object sender, EventArgs e)
    {
        Response.Redirect("viewInvoice.aspx");
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
