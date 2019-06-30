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

public partial class browsinvoiceaspx : System.Web.UI.Page
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
        lblUser.Text = UserLogin.UserName;
        if (Session["User"] != null)
        {


            string vUserId = UserLogin.UserName;
            string vPage = "browsinvoice.aspx";
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
            if (aUsers == "external")
            {
                customerlabel.Visible = false;
                txt_cust.Visible = false;
                CustomerLook.Visible = false;
            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page')</script>");
                Response.Write("<script>window.location.href='login.aspx'</script>");
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
            //Response.Write(vCanRun);
            if (vCanRuncreditstatusinv == true)
            {
                lnkcreditstatus.Visible = true;

            }

            if (vCanRuncreditstatusinv == false)
            {
                lnkcreditstatus.Visible = false;

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

        lbl_page.Text = "Brows Invoice";
        GridView1.SelectedIndex = Convert.ToInt32(Session["index3"]);

        Session["index4"] = Session["line"];
        try
        {
            if (Session["index3"] == null)
            {
                GridView1.SelectedIndex = Convert.ToInt32(Session["line"]) - 1;
                Session["vBol"] = GridView1.SelectedRow.Cells[4].Text;
                Session["vCust"] = GridView1.SelectedRow.Cells[5].Text;
                Session["browsinvoice_list_invoice"] = GridView1.SelectedRow.Cells[1].Text;
                Session["index4"] = null;
            }
            else
            {
                GridView1.SelectedIndex = Convert.ToInt32(Session["index3"]);
            }
        }
        catch
        {
            //return;
        }

        if (CheckBox1.Checked == true)
        {
            openinvoice.Value = "YES";
        }
        else
        {
            openinvoice.Value = "NO";
        }
        if (CheckBox2.Checked == true)
        {
            paidinvoice.Value = "YES";
        }
        else
        {
            paidinvoice.Value = "NO";
        }
        if (!Page.IsPostBack)
        {
            CheckBox1.Checked = true;
            CheckBox2.Checked = true;
            String expression = "i-no";
            SortDirection direction;
            direction = SortDirection.Descending;
            GridView1.Sort(expression, direction);
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

    protected void btnSearch_Click(object sender, EventArgs e)
    {
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Search";
        ObjectDataSource1.SelectParameters["prmInvoice"].DefaultValue = txt_inv.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCustomer"].DefaultValue = txt_cust.Text.Trim();
        ObjectDataSource1.SelectParameters["prmItem"].DefaultValue = txt_item.Text.Trim();
        ObjectDataSource1.SelectParameters["prmAccount"].DefaultValue = "";
        ObjectDataSource1.SelectParameters["prmPart"].DefaultValue = txt_part.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCustPo"].DefaultValue = txt_po.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBOL"].DefaultValue = txt_bol.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEstimate"].DefaultValue = txt_est.Text.Trim();
        ObjectDataSource1.SelectParameters["prmDate"].DefaultValue = txt_date.Text.Trim();
        ObjectDataSource1.SelectParameters["prmOpen"].DefaultValue = openinvoice.Value.Trim();
        ObjectDataSource1.SelectParameters["prmPaid"].DefaultValue = paidinvoice.Value.Trim();
        //Response.Write(openinvoice.Value);
        //Response.Write(paidinvoice.Value);
    }





    protected void lnkviewinvoice_Click(object sender, EventArgs e)
    {
        Response.Redirect("viewinvoice.aspx");
    }

    protected void lnkbrowsinvoice_Click(object sender, EventArgs e)
    {
        Session["brwsinvoice"] = Session["brwsinvoice"];
        Response.Redirect("browsinvoice.aspx");
    }


    protected void btn_reset_Click(object sender, EventArgs e)
    {
        string str = "";

        txt_inv.Text = str.ToString();
        txt_cust.Text = str.ToString();
        txt_item.Text = str.ToString();
        //txt_acct.Text = str.ToString();
        txt_part.Text = str.ToString();
        txt_po.Text = str.ToString();
        txt_bol.Text = str.ToString();
        txt_est.Text = str.ToString();
        txt_date.Text = str.ToString();

        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "select";
        ObjectDataSource1.SelectParameters["prmInvoice"].DefaultValue = txt_inv.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCustomer"].DefaultValue = txt_cust.Text.Trim();
        ObjectDataSource1.SelectParameters["prmItem"].DefaultValue = txt_item.Text.Trim();
        ObjectDataSource1.SelectParameters["prmAccount"].DefaultValue = "";
        ObjectDataSource1.SelectParameters["prmPart"].DefaultValue = txt_part.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCustPo"].DefaultValue = txt_po.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBOL"].DefaultValue = txt_bol.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEstimate"].DefaultValue = txt_est.Text.Trim();
        ObjectDataSource1.SelectParameters["prmDate"].DefaultValue = txt_date.Text.Trim();
        ObjectDataSource1.SelectParameters["prmOpen"].DefaultValue = CheckBox1.Text.Trim();
        ObjectDataSource1.SelectParameters["prmPaid"].DefaultValue = CheckBox2.Text.Trim();

    }
    protected void lnkcreditstatus_Click(object sender, EventArgs e)
    {
        Session["brwsinvoice"] = Session["brwsinvoice"];
        Session["vCust"] = Session["vCust"];
        Response.Redirect("creditstatusinv.aspx");
    }
    protected void lnkbol_Click(object sender, EventArgs e)
    {

        Session["brwsinvoice"] = Session["brwsinvoice"];
        Session["vBol"] = Session["vBol"];
        Response.Write(Session["vBol"]);
        Response.Redirect("bol.aspx");

    }
    protected void GridView1_SelectedIndexChanged(object sender, EventArgs e)
    {
        Session["vBol"] = GridView1.SelectedRow.Cells[4].Text;
        Session["vCust"] = GridView1.SelectedRow.Cells[5].Text;
        Session["browsinvoice_list_invoice"] = GridView1.SelectedRow.Cells[1].Text;
        //Response.Write(Session["vBol"]); Response.Write(Session["vCust"]); Response.Write(Session["brwsinvoice"]);
        int index = GridView1.SelectedIndex;
        Session["index3"] = index;
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
