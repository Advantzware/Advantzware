
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

public partial class list_cust_inv : System.Web.UI.Page
{


    protected void Page_Load(object sender, System.EventArgs e)
    {
        
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Select";
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "item_cust_inv.aspx";
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
                //txt_customer.Visible = false;
                //CustomerLook.Visible = false;
            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
            lblComp.Text = PrmComp;
        }


        if (!Page.IsPostBack)
        {

            if (Session["User"] != null)
            {
                //UserClass UserLogin = (UserClass)Session["User"];
                lblUser.Text = UserLogin.UserName;

            }



        }
        try
        {
            Session["Rowuser"] = UserLogin.UserName;
            TextBox ddl_display = (TextBox)FormView2.FindControl("aLineLabel");
            //ddl_display.Text = Convert.ToString(Session["gridsize"]);
            Session["size"] = Convert.ToInt32(ddl_display.Text);
            GridView1.PageSize = Convert.ToInt32(Session["size"]);

            GridView1.SelectedIndex = Convert.ToInt32(Session["itemcustinv_index"]);
            if (Session["itemcustinv_index"] == null)
            {
                GridView1.SelectedIndex = 0;
                Session["itemcustinv_no"] = ((Label)GridView1.SelectedRow.FindControl("Label3")).Text;
            }
            txt_vencode.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
            txt_venfgitem.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
            //txt_apvencode.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
            txt_custpart.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
            
        }
        catch { }


    }


    protected void hlnkLogOut_Click(object sender, EventArgs e)
    {

        string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
        if (sLoginURL == "")
        {
            Response.Write("<script language=javascript>alert('" + "Login page isn�t set" + "!');</script>");
            return;
        }

        Page.Session.Clear();
        Response.Redirect(sLoginURL);
    }



    protected void btnShowAll_Click(object sender, System.EventArgs e)
    {

        string ss = "";
        txt_vencode.Text = ss.ToString();
        txt_venfgitem.Text = ss.ToString();
        //txt_apvencode.Text = ss.ToString();
        txt_custpart.Text = ss.ToString();
        

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Select";
        ObjectDataSource1.SelectParameters["prmCustNum"].DefaultValue = txt_vencode.Text.Trim();
        ObjectDataSource1.SelectParameters["prmFgItmNum"].DefaultValue = txt_venfgitem.Text.Trim();
        //ObjectDataSource1.SelectParameters["prmVendCode"].DefaultValue = txt_apvencode.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCustPartNum"].DefaultValue = txt_custpart.Text.Trim();

        Session["itemcustinv_vencode"] = txt_vencode.Text.Trim();
        Session["itemcustinv_fgitem"] = txt_venfgitem.Text.Trim();
        //Session["itemcustinv_ap"] = txt_apvencode.Text.Trim();
        Session["itemcustinv_custpart"] = txt_custpart.Text.Trim();
        
    }



    protected void btnSearch_Click(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Search";
        ObjectDataSource1.SelectParameters["prmCustNum"].DefaultValue = txt_vencode.Text.Trim();
        ObjectDataSource1.SelectParameters["prmFgItmNum"].DefaultValue = txt_venfgitem.Text.Trim();
        //ObjectDataSource1.SelectParameters["prmVendCode"].DefaultValue = txt_apvencode.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCustPartNum"].DefaultValue = txt_custpart.Text.Trim();

        Session["itemcustinv_vencode"] = txt_vencode.Text.Trim();
        Session["itemcustinv_fgitem"] = txt_venfgitem.Text.Trim();
        //Session["itemcustinv_ap"] = txt_apvencode.Text.Trim();
        Session["itemcustinv_custpart"] = txt_custpart.Text.Trim();

    }



    protected void hlkBackToMenu_Click(object sender, EventArgs e)
    {
        string sMenuURL = ConfigurationManager.AppSettings["MenuFile"];
        if (sMenuURL == String.Empty)
        {
            Response.Write("<script language=javascript>alert('Menu page isn't set');</script>");
            return;
        }


        Response.Redirect(sMenuURL);
    }



    protected void ddl_display_TextChanged(object sender, EventArgs e)
    {
        TextBox ddl_display = (TextBox)FormView2.FindControl("aLineLabel");
        Session["gridsize"] = ddl_display.Text;
        //ddl_display.Text = Convert.ToString(Session["gridsize"]);
        ObjectDataSource2.SelectParameters["vLine"].DefaultValue = Convert.ToString(Session["gridsize"]);


    }
    protected void GridView1_SelectedIndexChanged(object sender, EventArgs e)
    {
        Session["itemcustinv_index"] = GridView1.SelectedIndex;
        Session["itemcustinv_no"]  = ((Label)GridView1.SelectedRow.FindControl("Label3")).Text;
       
    }

    protected void lnk_list_click(object sender, EventArgs e)
    {
        Response.Redirect("item_cust_inv.aspx");
    }

    protected void lnk_view_click(object sender, EventArgs e)
    {
        Response.Redirect("viewitem_cust_inv.aspx");
    }



}
