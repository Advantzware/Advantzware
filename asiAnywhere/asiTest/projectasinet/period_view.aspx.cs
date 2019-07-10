
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
#endregion

public partial class period_view : System.Web.UI.Page
{
    

    protected void Page_Load(object sender, System.EventArgs e)
    {
        
        
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "company_list.aspx";
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
                //UserClass UserLogin = (UserClass)Session["User"];
                lblUser.Text = UserLogin.UserName;
                Session["customer_user_id"] = UserLogin.UserName;

            }
        }
        
    }

    protected void FormView1_DataBound(object sender, EventArgs e)
    {
        if (FormView1.CurrentMode == FormViewMode.Insert)
        {
            TextBox year = (TextBox)FormView1.FindControl("peryrTextBox");
            year.Focus();

                      
        }
        if (FormView1.CurrentMode == FormViewMode.Edit)
        {
            try
            {
                TextBox status = (TextBox)FormView1.FindControl("perpstatTextBox");
                RadioButtonList rd1 = (RadioButtonList)FormView1.FindControl("RadioButtonList1");
                if (status.Text == "yes")
                    rd1.SelectedIndex = 0;
                if (status.Text == "no")
                    rd1.SelectedIndex = 1;
            }
            catch { }
            TextBox date = (TextBox)FormView1.FindControl("perpstTextBox");
            date.Focus();
    
        }
        if (FormView1.CurrentMode == FormViewMode.ReadOnly)
        {
            try
            {
                Label reckey = (Label)FormView1.FindControl("preckeyLabel");
                Label vyear = (Label)FormView1.FindControl("peryrLabel");
                Session["period_list_reckey_name"] = reckey.Text;
                Session["period_list_reckey_year"] = vyear.Text.Trim();

            }
            catch { }
            try
            {
                Label status = (Label)FormView1.FindControl("perpstatLabel");
                RadioButtonList rd1 = (RadioButtonList)FormView1.FindControl("RadioButtonList1");
                if (status.Text == "yes")
                    rd1.SelectedIndex = 0;
                if (status.Text == "no")
                    rd1.SelectedIndex = 1;
            }
            catch { }
        }
    }
    protected void Back_tomenu_Click(object sender, EventArgs e)
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
        Response.Redirect(sLoginURL);
    }


    
    
    protected void lnk_Listcompany_Click(object sender, EventArgs e)
    {
        Response.Redirect("company_list.aspx");
    }
    protected void lnk_viewcompany_Click(object sender, EventArgs e)
    {
        Response.Redirect("company_view.aspx");
    }

    protected void addButton_Click(object sender, EventArgs e)
    {

        TextBox peryr = (TextBox)FormView1.FindControl("peryrTextBox");
        TextBox perpnum = (TextBox)FormView1.FindControl("perpnumTextBox");
        TextBox perpst = (TextBox)FormView1.FindControl("perpstTextBox");
        TextBox perpend = (TextBox)FormView1.FindControl("perpendTextBox");
        RadioButtonList status = (RadioButtonList)FormView1.FindControl("RadioButtonList1");

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        try
        {
            Session["period_list_reckey_year"] = peryr.Text.Trim();
            ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "AddNewRec";
            ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource1.SelectParameters["prmyr"].DefaultValue = peryr.Text.Trim();
            ObjectDataSource1.SelectParameters["prmpnum"].DefaultValue = perpnum.Text.Trim();
            ObjectDataSource1.SelectParameters["prmpst"].DefaultValue = Convert.ToString(HiddenField1.Value);
            ObjectDataSource1.SelectParameters["prmpend"].DefaultValue = perpend.Text.Trim();
            ObjectDataSource1.SelectParameters["prmpstat"].DefaultValue = status.SelectedValue;

            FormView1.ChangeMode(FormViewMode.ReadOnly);


        }
        catch { }
      
    }
    protected void UpdateButton_Click(object sender, EventArgs e)
    {

        TextBox peryr = (TextBox)FormView1.FindControl("peryrTextBox");
        TextBox perpnum = (TextBox)FormView1.FindControl("perpnumTextBox");
        TextBox perpst = (TextBox)FormView1.FindControl("perpstTextBox");
        TextBox perpend = (TextBox)FormView1.FindControl("perpendTextBox");
        RadioButtonList status = (RadioButtonList)FormView1.FindControl("RadioButtonList1");
        
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        try
        {

            ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "UpdateRec";
            ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource1.SelectParameters["prmyr"].DefaultValue = peryr.Text.Trim();
            ObjectDataSource1.SelectParameters["prmpnum"].DefaultValue = perpnum.Text.Trim();
            ObjectDataSource1.SelectParameters["prmpst"].DefaultValue = perpst.Text.Trim();
            ObjectDataSource1.SelectParameters["prmpend"].DefaultValue = perpend.Text.Trim();
            ObjectDataSource1.SelectParameters["prmpstat"].DefaultValue = status.SelectedValue;
            
            FormView1.ChangeMode(FormViewMode.ReadOnly);
                 
            
        }
        catch { }
            
           
       


    }


    protected void Delete_Click(object sender, EventArgs e)
    {

        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Delete";
        Session["customer1_list_index"] = null;
        FormView1.ChangeMode(FormViewMode.ReadOnly);
              
    }
    protected void lnk_listperiod_Click(object sender, EventArgs e)
    {
        Response.Redirect("period_list.aspx");
    }
    protected void lnk_viewperiod_Click(object sender, EventArgs e)
    {
        Response.Redirect("period_view.aspx");
    }
    
    protected void img_btn_add_click(object sender, EventArgs e)
    {
        FormView1.ChangeMode(FormViewMode.Insert);
    }



}
