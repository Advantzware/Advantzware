
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
#endregion

public partial class cust_list_sold : System.Web.UI.Page
{

    private bool bSort = true;

    protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "cust_list_sold.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            labelcompany.Text = PrmComp;

            if (aUsers == "external")
            {


            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }


        if (!Page.IsPostBack)
        {

            if (Session["User"] != null)
            {
                //    UserClass UserLogin = (UserClass)Session["User"]; 
                lblUser.Text = UserLogin.UserName;

            }



            if (func.CheckUserPermissions("[dbo].[cust]", "s"))
            {
            }

        } //  ! Page.IsPostBack




        GridView1.SelectedIndex = Convert.ToInt32(Session["customer_list_sold_index"]);

        try
        {
            if (Session["customer_list_sold_index"] == null)
            {
                GridView1.SelectedIndex = 0;

                Session["customer_list_soldto"] = GridView1.SelectedRow.Cells[1].Text;
            }
        }
        catch
        {
            return;
        }

        try
        {
            TextBox ddl_display = (TextBox)FormView1.FindControl("aLineLabel");
            //ddl_display.Text = Convert.ToString(Session["gridsize"]);
            Session["size"] = Convert.ToInt32(ddl_display.Text);
            GridView1.PageSize = Convert.ToInt32(Session["size"]);
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

    protected void Back_tomenu_Click(object sender, EventArgs e)
    {
        Response.Redirect("menu.aspx");
    }



    protected void ddl_display_TextChanged(object sender, EventArgs e)
    {
        try
        {
            TextBox ddl_display = (TextBox)FormView1.FindControl("aLineLabel");
            Session["gridsize"] = ddl_display.Text;
            ObjectDataSource2.SelectParameters["vLine"].DefaultValue = Convert.ToString(Session["gridsize"]);
        }
        catch
        {
            return;
        }
    }


    protected void GridView1_SelectedIndex(object sender, EventArgs e)
    {
        Session["customer_list_sold_index"] = GridView1.SelectedIndex;

        Session["customer_list_soldto"] = GridView1.SelectedRow.Cells[1].Text;

    }

    protected void btnSearch_Click(object sender, EventArgs e)
    {
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Search";
        ObjectDataSource1.SelectParameters["prmsoldid"].DefaultValue = Sold_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmsoldname"].DefaultValue = Name_TextBox.Text.Trim();

        //Session["customer1_list_cust"] = null;
    }
    protected void btnShowAll_Click(object sender, EventArgs e)
    {
        Response.Redirect("cust_list_sold.aspx");
    }
    protected void lnk_viewcustomers_Click(object sender, EventArgs e)
    {
        Response.Redirect("cust_viewlist.aspx");
    }
    protected void lnk_listship_Click(object sender, EventArgs e)
    {
        Response.Redirect("cust_list_ship.aspx");
    }
    protected void lnk_viewship_Click(object sender, EventArgs e)
    {
        Response.Redirect("cust_view_ship.aspx");
    }
    protected void lnk_Listcustomers_Click(object sender, EventArgs e)
    {
        Response.Redirect("customer_list.aspx");
    }
    protected void lnk_viewsold_Click(object sender, EventArgs e)
    {
        Response.Redirect("cust_view_sold.aspx");
    }
    protected void lnk_listsold_Click(object sender, EventArgs e)
    {
        Response.Redirect("cust_list_sold.aspx");
    }

}
