
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
#endregion

public partial class cust_plant : System.Web.UI.Page
{

    private bool bSort = true;

    protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmActPlant"].DefaultValue = "SelCustPlant";


        try
        {
            GridView1.SelectedIndex = Convert.ToInt32(Session["index_cust_plant_list"]);
            if (Session["index_cust_plant_list"] == null)
            {
                GridView1.SelectedIndex = 0;
                
                Session["view_cust_plant_loc"] = GridView1.SelectedRow.Cells[2].Text;
                Session["view_cust_plant_cust"] = GridView1.SelectedRow.Cells[1].Text;
                Session["view_cust_plant_cust_reckey"] = ((Label)GridView1.SelectedRow.FindControl("reckeyLabel")).Text;
            }
        }
        catch { }

        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "cust_plant_list.aspx";
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
            Session["Customers_Company"] = labelcompany.Text;
            if (aUsers == "external")
            {


            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }
        try
        {
            Session["Rowuser"] = UserLogin.UserName;
            TextBox ddl_display = (TextBox)FormView1.FindControl("aLineLabel");
            //ddl_display.Text = Convert.ToString(Session["gridsize"]);
            Session["size"] = Convert.ToInt32(ddl_display.Text);
            GridView1.PageSize = Convert.ToInt32(Session["size"]);
        }
        catch { }


        if (!Page.IsPostBack)
        {
            lblUser.Text = UserLogin.UserName;
        }
        
        txt_cust.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_ship.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_plantno.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        
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
        TextBox ddl_display = (TextBox)FormView1.FindControl("aLineLabel");
        Session["gridsize"] = ddl_display.Text;
        //ddl_display.Text = Convert.ToString(Session["gridsize"]);
        ObjectDataSource2.SelectParameters["vLine"].DefaultValue = Convert.ToString(Session["gridsize"]);

    }
    protected void btnSearch_Click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmActPlant"].DefaultValue = "SearchCustPlant";
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmCust"].DefaultValue = txt_cust.Text.Trim();
        ObjectDataSource1.SelectParameters["prmShipid"].DefaultValue = txt_ship.Text.Trim();
        ObjectDataSource1.SelectParameters["prmPlantid"].DefaultValue = txt_plantno.Text.Trim();
    }
    protected void btn_reset_Click(object sender, EventArgs e)
    {
        Response.Redirect("cust_plant_list.aspx");
    }

    protected void GridView1_SelectedIndexChanged(object sender, EventArgs e)
    {
        //GridView1.SelectedIndex = 0;
        Session["index_cust_plant_list"] = GridView1.SelectedIndex;

        Session["view_cust_plant_loc"] = GridView1.SelectedRow.Cells[2].Text;
        Session["view_cust_plant_cust"] = GridView1.SelectedRow.Cells[1].Text;
        Session["view_cust_plant_cust_reckey"] = ((Label)GridView1.SelectedRow.FindControl("reckeyLabel")).Text;
       
        }
    protected void list_cust_click(object sender, EventArgs e)
    {
        Response.Redirect("cust_plant_list.aspx");
    }
    protected void view_cust_click(object sender, EventArgs e)
    {
        Response.Redirect("view_cust_plant.aspx");
    }
}
