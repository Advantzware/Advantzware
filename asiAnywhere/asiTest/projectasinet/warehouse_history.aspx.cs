
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

public partial class warehouse_history : System.Web.UI.Page
{
    

    protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        //ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Select";
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "warehouse_history.aspx";
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

            GridView1.SelectedIndex = Convert.ToInt32(Session["warehouse_history_index"]);
            if (Session["warehouse_history_index"] == null)
            {
                GridView1.SelectedIndex = 0;
                Session["warehousehist_seq_txt_view"] = GridView1.SelectedRow.Cells[1].Text;
                Session["warehousehist_seq_txt_view_reckey"] = ((Label)GridView1.SelectedRow.FindControl("reckeyLabel")).Text; 
            }
            txt_seq.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
            txt_transdate.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
            txt_custpo.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
            txt_custpart.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
            txt_fgitem.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')"); 
        }
        catch { }       
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



    protected void btnShowAll_Click(object sender, System.EventArgs e)
    {
        
        string ss = "";
        txt_seq.Text = ss.ToString();
        txt_transdate.Text = ss.ToString();
        txt_custpo.Text = ss.ToString();
        txt_custpart.Text = ss.ToString();
        txt_fgitem.Text = ss.ToString();

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Select";
        ObjectDataSource1.SelectParameters["prmSeqNo"].DefaultValue = txt_seq.Text.Trim();
        ObjectDataSource1.SelectParameters["prmTransDate"].DefaultValue = txt_transdate.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCustomersPoNo"].DefaultValue = txt_custpo.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCustPart"].DefaultValue = txt_custpart.Text.Trim();
        ObjectDataSource1.SelectParameters["prmFgItem"].DefaultValue = txt_fgitem.Text.Trim();

        Session["warehouse_hist_action_txt"] = "Select";
        Session["warehouse_hist_seq_txt"] = txt_seq.Text.Trim();
        Session["warehouse_hist_transdate_txt"] = txt_transdate.Text.Trim();
        Session["warehouse_hist_custpo_txt"] = txt_custpo.Text.Trim();
        Session["warehouse_hist_custpart_txt"] = txt_custpart.Text.Trim();
        Session["warehouse_hist_fgitem_txt"] = txt_fgitem.Text.Trim();
    }
   


    protected void btnSearch_Click(object sender, System.EventArgs e)
    {
       
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Search";
        ObjectDataSource1.SelectParameters["prmSeqNo"].DefaultValue = txt_seq.Text.Trim();
        ObjectDataSource1.SelectParameters["prmTransDate"].DefaultValue = txt_transdate.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCustomersPoNo"].DefaultValue = txt_custpo.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCustPart"].DefaultValue = txt_custpart.Text.Trim();
        ObjectDataSource1.SelectParameters["prmFgItem"].DefaultValue = txt_fgitem.Text.Trim();

        Session["warehouse_hist_action_txt"] = "Select";
        Session["warehouse_hist_seq_txt"] = txt_seq.Text.Trim();
        Session["warehouse_hist_transdate_txt"] = txt_transdate.Text.Trim();
        Session["warehouse_hist_custpo_txt"] = txt_custpo.Text.Trim();
        Session["warehouse_hist_custpart_txt"] = txt_custpart.Text.Trim();
        Session["warehouse_hist_fgitem_txt"] = txt_fgitem.Text.Trim();        
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
        Session["warehouse_history_index"] = GridView1.SelectedIndex;
        Session["warehousehist_seq_txt_view"] = GridView1.SelectedRow.Cells[1].Text;        
        Session["warehousehist_seq_txt_view_reckey"] = ((Label)GridView1.SelectedRow.FindControl("reckeyLabel")).Text; 
    }

    protected void lnk_browse_hist_click(object sender, EventArgs e)
    {
        Response.Redirect("warehouse_history.aspx");
    }

    protected void lnk_view_hist_click(object sender, EventArgs e)
    {
        Response.Redirect("view_warehouse_history.aspx");
    }
   


}
